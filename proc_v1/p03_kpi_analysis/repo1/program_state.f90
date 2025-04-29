module program_state
    ! Module for debugging purposes - has no dependencies and 
    ! can be USEd by any source file.
      logical :: get_icap_report_values_called=.false.
      logical :: cm_planning_reserve_margin_allocated=.false.
      integer :: BCP_arraysize=0 ! BTU_COST_POINTR array size
      integer :: get_var_call_count=0
      integer :: annual_energy_products_call_count=0
      character (len=128) :: get_var_called_from=""
      character (len=128) :: get_primary_mover_index_called_from=""
      character (len=128) :: gud_called_from="" ! Get_user_day
      character (len=128) :: cx_called_from="" ! Cx_DailOperAnPS2
      character (len=128) :: cxac_called_from="" ! cx_annual_contract
      character (len=128) :: cur_called_from="" ! cl_unique_report_str
      

      ! call count for capacity_planning_object routine
      integer :: cc_capacity_planning_object=0 
      integer :: cc_ANNUAL_MARKET_OPTION_COST=0
      


end module program_state

! Program_state and debugtrace should be in different modules.
module debugtrace
use filemod
implicit none
    ! Returns this as the file handle when 
    ! debug tracing is off and a trace file is requested.
    ! Attempts to read/write to this file then invoke
    ! runtime errors, making it easy to find mistakes
    ! in tracing code when direct file writes are used to
    ! write multiple items in one call.
    ! Must be checked when clients are writing directly 
    ! to the trace-assigned handle
    integer, parameter :: BAD_TRACE_HANDLE=-773440
    character (len=2) :: linefeed=char(13)//char(10)
    logical :: m_debug_tracing_on=.true.
    logical :: m_all_tracing_on=.false.
    
      ! Requestors
      ! Increase array size when adding tracers
      integer, parameter :: rq_array_size=84
      logical, save :: requestor_ids(rq_array_size)
      logical, save :: rq_ids_initialized=.false.
      ! Tracing requestor IDs so that individual files can be 
      ! turned on and off.
      integer, parameter :: rq_el_makeovl=1 ! El_makeovl
      integer, parameter :: rq_cl_units_read_ext=2 ! cl_units_read
      integer, parameter :: rq_ct_objt=3 ! ct_objt (covers too many routines)
      integer, parameter :: rq_contract_read=4 ! contracts_read
      integer, parameter :: rq_elur=5 ! El_units_read (el_objt_102_g)
      integer, parameter :: rq_cls_efor=6 ! rtn_cl_screen_efor in cl_screen_data_ext
      integer, parameter :: rq_edr=7 ! envir_data_read in en_objt (en_data_read)
      integer, parameter :: rq_cacm=8 ! cl_units_read_ext (calc_annual_cap_maint)
      integer, parameter :: rq_to21_aep=9 ! Annual_energy_products routine in transobj2 (transobj2_1)
      integer, parameter :: rq_vpvn=10 !  Variable_Units_object in up_objt (vector_pointr_vn)
      integer, parameter :: rq_up_102_h=11 !get_el_mw_from_pointer in up_objt (up_objt_102_H)
      integer, parameter :: rq_uo_gvc=12 ! get_var_calls, up_objt
      integer, parameter :: rq_uo_gvs=13 ! get_var_sequence, up_objt
      integer, parameter :: rq_uo_102_J=14 ! up_objt_102_J
      integer, parameter :: rq_mads=15 ! MIDAS_ANN_DECOMP_SIMULATION in msgandec
      integer, parameter :: rq_msgnorm=16 ! Msgnorm.txt, MIDAS_NORMAL_SIMULATION
      integer, parameter :: rq_pc_pt=17 ! procost_trace in pro_cost
      integer, parameter :: rq_mrsd=18 ! msgmmrev, manage_rps_state_demand
      integer, parameter :: rq_mm=19 ! midasmod
      integer, parameter :: rq_co_cp=20 ! cap_objt, capacity_planning_trace
      integer, parameter :: rq_co_ao=21 ! cap_objt, auto optim
      integer, parameter :: rq_co_pd=22 ! cap_objt, PLANNING_DECISIONS
      integer, parameter :: rq_amoc=23 ! cap_objt, ANNUAL_MARKET_OPTION_COST
      integer, parameter :: rq_co_mg=24 ! cap_objt, mrx_green
      integer, parameter :: rq_ra=25 ! resource_available (cn_objt)
      integer, parameter :: rq_ftas=26  ! annual_screening (cap_objt)
      integer, parameter :: rq_wh_mlnmap=27
      integer, parameter :: rq_tf_tlp2h=28
      integer, parameter :: rq_who_cmnl=29
      integer, parameter :: rq_pclr=30 ! Period_cl_report subroutine
      integer, parameter :: rq_huidn=31 ! Hesi unit id num function
      integer, parameter :: rq_sca=32
      integer, parameter :: rq_dispat=33
      integer, parameter :: rq_gmdcu=34 ! GET_MONTHLY_DISP_COST_BY_UNIT
      integer, parameter :: rq_hr_calc=35 ! HR_CALC routine
      
      ! ENRG trace - managed from energy_trace module
      integer, parameter :: rq_enrg_trace=36 ! System energy trace (module)
      integer, parameter :: rq_offline=37
      integer, parameter :: rq_pm=38
      integer, parameter :: rq_pmidb=39
      integer, parameter :: rq_elm=40
      integer, parameter :: rq_wcr=41
      integer, parameter :: rq_pos=42
      integer, parameter :: rq_loads=43
      integer, parameter :: rq_haor=43 ! handle_annual_output_report routine.
      integer, parameter :: rq_tgr=44
      integer, parameter :: rq_fto=45 ! OUTPUT routine in msgoutpt
      integer, parameter :: rq_mga=46
      integer, parameter :: rq_rra=47 ! return_risk_adj_s_cap_cost routine
      integer, parameter :: rq_gcrk=48 ! get_cm_retirement_kw routine in cl_units_read_ext
      integer, parameter :: rq_uscc=49 ! update screen cap costs
      integer, parameter :: rq_rstd=50 ! return_s_trans_group_id
      integer, parameter :: rq_gtgp=51 ! get_trans_group_position
      integer, parameter :: rq_atl=52 ! annual_transaction_loads
      integer, parameter :: rq_sip=53 ! snszmds
      integer, parameter :: rq_cxat=54 ! cx_annual_contract
      integer, parameter :: rq_cxdo=55 !CX_DailyOperAnPS2
      integer, parameter :: rq_gud=56 ! get_user_day
      integer, parameter :: rq_rif=57 ! read_icap_file
      integer, parameter :: rq_sic=58
      integer, parameter :: rq_ghov=59 ! get_hour_day_option_value
      integer, parameter :: rq_so=60 ! init_screening_object
      integer, parameter :: rq_epc=61 ! el_pool_costs_and_unit_reports
      integer, parameter :: rq_mxa=62 ! MX_ANNUAL report file
      integer, parameter :: rq_gon=63 ! get_option_names routine
      integer, parameter :: rq_mr=64 ! mix_ratios subroutine
      integer, parameter :: rq_ass=65 ! annual_storage_sort
      integer, parameter :: rq_camp=66 ! cx_alter_mx_chrono_prices
      integer, parameter :: rq_rud=67 ! read_user_day
      integer, parameter :: rq_udm=68
      integer, parameter :: rq_mcfu=69
      integer, parameter :: rq_cxp3=70 ! CX_DailyOperAnPS3
      integer, parameter :: rq_esh=71 ! existing_storage_n_hybrid
      integer, parameter :: rq_aum=72 ! cx_trans_annual_user_market
      integer, parameter :: rq_bth=73 ! DR_BOOTH subroutine
      integer, parameter :: rq_cmme=74 ! cal_mar_monthly_energy
      integer, parameter :: rq_gac=75 ! get_average_cost
      integer, parameter :: rq_aca=76  ! asset_class_analysis
      integer, parameter :: rq_ycf=77 ! write_years_cmd_file
      integer, parameter :: rq_clm=78 ! cl_maint file
      integer, parameter :: rq_cru=79 ! cla_return_unitnm
      integer, parameter :: rq_un=80 ! unit name trace
      integer, parameter :: rq_crs=81 ! cl_unique_rpt_str
      integer, parameter :: rq_ms=82 ! maint_scheduler
      integer, parameter :: rq_icv=83 ! income_convergence routine
      integer, parameter :: rq_clax=84 ! return_cl_asset_class_expenses


contains

! If this module is used and there is a need to write more than one 
! value, put the write in an IF block and check this function.
! Then you can write directly to the file, like this:
! if (get_debug_tracing_on()) then
!     write([num/handle],*) value1, value2, ...
! end if
function get_debug_tracing_on()
    logical :: get_debug_tracing_on
    get_debug_tracing_on=m_debug_tracing_on
end function get_debug_tracing_on
subroutine set_alltrace_on()
    m_all_tracing_on=.true.
end subroutine set_alltrace_on

subroutine set_debug_tracing_on(dt_value)
    logical :: dt_value
    
    m_debug_tracing_on=dt_value
end subroutine set_debug_tracing_on
function is_requestor_on(requestor_id)
logical :: is_requestor_on

integer :: requestor_id

    if(.not. rq_ids_initialized) then
        rq_ids_initialized=.true.
        requestor_ids=.true.
        ! Turn individual tracefiles on and off here.
        requestor_ids(rq_el_makeovl)=.false. ! El_makeovl
        requestor_ids(rq_cl_units_read_ext)=.false.
        requestor_ids(rq_ct_objt)=.false.
        requestor_ids(rq_contract_read)=.false. ! contracts_read
        requestor_ids(rq_elur)=.false.
        requestor_ids(rq_cls_efor)=.false.
        requestor_ids(rq_edr)=.false.
        requestor_ids(rq_cacm)=.true.
        requestor_ids(rq_to21_aep)=.false. ! Annual energy products
        requestor_ids(rq_vpvn)=.false.
        requestor_ids(rq_up_102_h)=.false.
        requestor_ids(rq_uo_gvc)=.false.
        requestor_ids(rq_uo_gvs)=.false.
        requestor_ids(rq_uo_102_J)=.false.
        requestor_ids(rq_mads)=.false.
        requestor_ids(rq_msgnorm)=.false.
        requestor_ids(rq_pc_pt)=.false.
        requestor_ids(rq_mrsd)=.false.
        requestor_ids(rq_mm)=.false.
        requestor_ids(rq_co_cp)=.true. 
        requestor_ids(rq_co_ao)=.false. 
        requestor_ids(rq_co_pd)=.false. 
        requestor_ids(rq_amoc)=.true.
        requestor_ids(rq_co_mg)=.false.
        requestor_ids(rq_ra)=.false.
        requestor_ids(rq_ftas)=.false. ! Annual_screening
        requestor_ids(rq_wh_mlnmap)=.false.
        requestor_ids(rq_tf_tlp2h)=.false.
        requestor_ids(rq_who_cmnl)=.true.
        requestor_ids(rq_pclr)=.true.
        requestor_ids(rq_huidn)=.false.
        requestor_ids(rq_sca)=.false.
        requestor_ids(rq_dispat)=.false.
        requestor_ids(rq_gmdcu)=.false.
        requestor_ids(rq_hr_calc)=.false.
        requestor_ids(rq_enrg_trace)=.false. ! System energy trace (module)
        requestor_ids(rq_offline)=.false.
        requestor_ids(rq_pm)=.true.
        requestor_ids(rq_pmidb)=.true.
        requestor_ids(rq_elm)=.false.
        requestor_ids(rq_wcr)=.false.
        requestor_ids(rq_pos)=.false. ! POS array 
        requestor_ids(rq_loads)=.false. ! Loads tracing
        requestor_ids(rq_haor)=.false.
        requestor_ids(rq_tgr)=.true. ! Transact group reserve
        requestor_ids(rq_fto)=.true. ! OUTPUT routine in msgoutpt
        requestor_ids(rq_mga)=.true. ! annual_mx_green
        requestor_ids(rq_rra)=.false. ! return_risk_adj_s_cap_cost routine
        requestor_ids(rq_gcrk)=.true. ! get_cm_retirement_kw routine in cl_units_read_ext
        requestor_ids(rq_uscc)=.false. ! update screen cap costs
        requestor_ids(rq_rstd)=.false. ! return_s_trans_group_id
        requestor_ids(rq_gtgp)=.true. ! get_trans_group_position
        requestor_ids(rq_atl)=.true. ! annual_transaction_loads
        requestor_ids(rq_sip)=.true. ! snszmds
        requestor_ids(rq_cxat)=.true. ! cx_annual_contract
        requestor_ids(rq_cxdo)=.true. !CX_DailyOperAnPS2
        requestor_ids(rq_gud)=.true. ! get_user_day
        requestor_ids(rq_rif)=.true. ! read_icap_file
        requestor_ids(rq_sic)=.true. ! SortIncrPos
        requestor_ids(rq_ghov)=.false. !get_hour_day_option_value - too much data
        requestor_ids(rq_so)=.true. ! init_screening_object
        requestor_ids(rq_epc)=.false. ! el_pool_costs_and_unit_reports
        requestor_ids(rq_mxa)=.true. ! MX_ANNUAL report file
        requestor_ids(rq_gon)=.false. ! get_option_names routine
        requestor_ids(rq_mr)=.true. ! mix_ratios subroutine
        requestor_ids(rq_ass)=.true. ! Annual_storage_sort
        requestor_ids(rq_camp)=.true. ! cx_alter_mx_chrono_prices
        requestor_ids(rq_rud)=.true.
        requestor_ids(rq_udm)=.true.
        requestor_ids(rq_mcfu)=.true.
        requestor_ids(rq_cxp3)=.true. ! CX_DailyOperAnPS3
        requestor_ids(rq_esh)=.true. ! existing_storage_n_hybrid
        requestor_ids(rq_aum)=.true. ! cx_trans_annual_user_market
        requestor_ids(rq_bth)=.true. ! DR_BOOTH subroutine
        requestor_ids(rq_cmme)=.true. ! cal_mar_monthly_energy
        requestor_ids(rq_gac)=.true. ! get_average_cost
        requestor_ids(rq_aca)=.true.  ! asset_class_analysis
        requestor_ids(rq_clm)=.true. ! cl_maint file
        requestor_ids(rq_cru)=.true. ! cla_return_unitnm
        requestor_ids(rq_ycf)=.true. ! write_years_cmd_file
        requestor_ids(rq_un)=.true. ! unit name trace
        requestor_ids(rq_crs)=.true. ! cl_unique_rpt_str
        requestor_ids(rq_ms)=.true. ! maint_scheduler
        requestor_ids(rq_icv)=.true. ! income_convergence routine
        requestor_ids(rq_clax)=.true. ! return_cl_asset_class_expenses



    end if
    
    if(m_all_tracing_on) then
        is_requestor_on=.true.
    else
        is_requestor_on=requestor_ids(requestor_id)
    end if

end function

function open_trace(filename, requestor_id)
    ! open_new_next_file assigns file handle and 
    ! performs needed file-open checking.
    integer :: requestor_id
    character (len=*), intent(in) :: filename
    integer :: open_trace
    open_trace=BAD_TRACE_HANDLE
    
    if(m_debug_tracing_on .or. m_all_tracing_on) then
        if(is_requestor_on(requestor_id)) then
            open_trace=open_new_text_file(filename)
        end if
    else
        open_trace=BAD_TRACE_HANDLE
    end if

end function open_trace
subroutine write_trace_message(handle, message)
    integer, intent(in) :: handle
    character (len=*), intent(in) :: message
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) trim(message)
        call flush(handle)
    end if
end subroutine write_trace_message

subroutine write_trace_string(handle, caption, v)
    integer, intent(in) :: handle
    character (len=*), intent(in) :: caption
    character (len=*), intent(in) :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", trim(v), " "
        call flush(handle)
    end if
    
end subroutine write_trace_string
subroutine write_trace_strings(handle, caption, v)
    integer, intent(in) :: handle
    character (len=*), intent(in) :: caption
    character (len=*), intent(in) :: v(:)
    integer :: ub, lb, I
    
    lb=lbound(v,1)
    ub=ubound(v,1)

    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", (V(I),I=lb,ub), " "
        call flush(handle)
    end if
    
end subroutine write_trace_strings

subroutine write_trace_int2(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    integer (kind=2) :: v

    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_int2
subroutine write_trace_int4(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    integer (kind=4) :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_int4
subroutine write_trace_int8(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    integer (kind=8) :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_int8
subroutine write_trace_int(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    integer  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        call write_trace_int2(handle, trim(caption), int(v,2))
    end if

end subroutine write_trace_int


subroutine write_trace_bool(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    logical  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if

end subroutine write_trace_bool
subroutine write_trace_bool4(handle, caption,v)
     integer :: handle
    character (len=*), intent(in) :: caption
    logical(kind=4)  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if

end subroutine write_trace_bool4
subroutine write_trace_bool1(handle, caption,v)
     integer :: handle
    character (len=*), intent(in) :: caption
    logical(kind=1)  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if

end subroutine write_trace_bool1
subroutine write_trace_bool2(handle, caption,v)
     integer :: handle
    character (len=*), intent(in) :: caption
    logical(kind=2)  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if

end subroutine write_trace_bool2
subroutine write_trace_real(handle, caption,v)
     integer :: handle
    character (len=*), intent(in) :: caption
    real  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if

end subroutine write_trace_real
subroutine write_trace_real4(handle, caption,v)
     integer :: handle
    character (len=*), intent(in) :: caption
    real (kind=4)  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if

end subroutine write_trace_real4
subroutine write_trace_real8(handle, caption,v)
     integer :: handle
    character (len=*), intent(in) :: caption
    real (kind=8)  :: v
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if

end subroutine write_trace_real8
subroutine write_trace_real4s(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    real(kind=4), intent(in) :: v(:)
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_real4s
subroutine write_trace_real4s_vert(handle, caption, v)
    ! Write one element at a time, space-separated,
    ! with regular linefeeds.
    use program_state

    integer :: handle
    character (len=*), intent(in) :: caption
    real(kind=4), intent(in) :: v(:)
    integer :: ub, index
    ub=ubound(v,1)
    if (handle/=BAD_TRACE_HANDLE) then
        write(handle, *) caption, " "
        do index=1, ub
            write(handle,*) v(index), " "
        end do
    end if
    
    
end subroutine write_trace_real4s_vert
subroutine write_trace_reals_vert(handle, caption, v)
    ! Write one element at a time, space-separated,
    ! with regular linefeeds.
    use program_state

    integer :: handle
    character (len=*), intent(in) :: caption
    real, intent(in) :: v(:)
    integer :: ub, index
    
    ub=ubound(v,1)
    ! Limit number of results
    if(ub>500) then
        ub=500
    end if
    
    if (handle/=BAD_TRACE_HANDLE) then
        write(handle, *) caption, " "
        do index=1, ub
            write(handle,*) v(index), " "
        end do
    end if
    
    
end subroutine write_trace_reals_vert
subroutine write_trace_real8s(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    real(kind=8), intent(in) :: v(:)
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_real8s
subroutine write_trace_reals(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    real, intent(in) :: v(:)
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_reals
subroutine write_trace_reals2(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    real, intent(in) :: v(2)
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_reals2
subroutine write_trace_int2s(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    integer(kind=2), intent(in) :: v(:)
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_int2s

subroutine write_trace_int4s(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    integer(kind=4), intent(in) :: v(:)
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_int4s

subroutine write_trace_bool1s(handle, caption, v)
    integer :: handle
    character (len=*), intent(in) :: caption
    logical (kind=1), intent(in) :: v(:)
    if(handle /= BAD_TRACE_HANDLE) then
        write(handle,*) " ",  trim(caption)," ", v, " "
        call flush(handle)
    end if
end subroutine write_trace_bool1s


end module debugtrace

module energy_trace
! This module allows tracing of energy (ENRG) across the application.
use debugtrace
implicit none
    integer, save :: file_trace_enrg=0
contains

    subroutine write_energy_trace_message(routine, tag, message)
        character (len=*), intent(in) :: routine, tag, message
        integer :: fte ! for debugger
        call open_energy_trace()
        fte=file_trace_enrg
        
        call write_trace_message(file_trace_enrg, trim(routine) // " " // trim(tag))
    end subroutine write_energy_trace_message


    subroutine open_energy_trace
        integer :: fte ! For debugger
        if(file_trace_enrg==0) then
            file_trace_enrg=open_trace("enrg.trace", rq_enrg_trace)
            fte=file_trace_enrg
            call write_trace_message(file_trace_enrg, "Tracing of all system changes to energy.")
        end if
        
        
        
    end subroutine open_energy_trace
    
    subroutine write_energy_trace(routine, tag, v)
        integer :: fte ! for debugger
        character (len=*), intent (in) :: routine, tag
        real :: v
        call open_energy_trace()
        fte=file_trace_enrg
                
        if(file_trace_enrg/=BAD_TRACE_HANDLE) then
            write(file_trace_enrg, *) trim(routine), " ", trim(tag), " ", v
        end if

    end subroutine write_energy_trace
    subroutine write_mw_pc_trace_rk4s(tag, v)
        real(kind=4), intent(in) :: v(:)
        character (len=*), intent(in) :: tag
        call open_energy_trace()
        call write_trace_real4s(file_trace_enrg, tag, v)
        
    end subroutine write_mw_pc_trace_rk4s
    subroutine write_mw_pc_trace(tag, v)
       real, intent(in) :: v
       character (len=*) :: tag
       call open_energy_trace()
       call write_trace_real(file_trace_enrg, trim(tag) // " (MW_PC)", v)
    end subroutine write_mw_pc_trace
    subroutine write_mw_pc_traces(tag, v)
       real, intent(in) :: v(:)
       character (len=*) :: tag
       call open_energy_trace()
       call write_trace_reals(file_trace_enrg, trim(tag) // " (MW_PC)", v)  
    end subroutine write_mw_pc_traces
    subroutine write_daily_energy_trace(tag, v)
       real (kind=4), intent(in) :: v
       character (len=*) :: tag
       
       ! Will stop writing at n number of write

        call write_trace_real(file_trace_enrg, trim(tag) , v)
    end subroutine write_daily_energy_trace
    
end module energy_trace
module offline_trace
    use debugtrace
    implicit none
    integer, save :: file_trace_offl=0
    contains
    subroutine open_offline_trace()
        if(file_trace_offl==0) then
            file_trace_offl=open_trace("offline.trace", rq_offline)
        end if
    end subroutine open_offline_trace
    subroutine write_offline_trace(tag, v)
        character (len=*), intent(in) :: tag
        integer (kind=2), intent(in) :: v
        integer :: fto=0
        fto=file_trace_offl
        call write_trace_int2(file_trace_offl, trim(tag) , v)
        
    end subroutine write_offline_trace
    
    subroutine write_offline_trace_r4(tag, v)
        character (len=*) :: tag
        character (len=25) :: dest
        real (kind=4) :: v
        integer :: fto=0
        fto=file_trace_offl
        dest=" "
        write(dest, *) v
        
        call write_trace_string(file_trace_offl, trim(tag), dest)
        
    end subroutine write_offline_trace_r4
    
end module offline_trace

module pos_trace
use debugtrace
implicit none
    integer, save :: file_trace_pos=0
    integer, save :: numwrites=0
    
    ! Traces setting of POS array across multiple routines.
contains
    subroutine m_open_pos_trace()
        if(file_trace_pos==0) then
            file_trace_pos=open_trace("pos.trace", rq_pos)
        end if
    end subroutine m_open_pos_trace
    subroutine write_trace_pos(caption, v)
        character (len=*), intent(in) :: caption
        integer (kind=2), intent(in) :: v
        
        call m_open_pos_trace()
        numwrites=numwrites+1
        if(numwrites<500) then
            call write_trace_int2(file_trace_pos, caption, v)
        end if
        
    end subroutine write_trace_pos
    
end module pos_trace
module load_trace
    use debugtrace
    integer :: file_trace_loads=0
contains
    subroutine open_load_trace() ! Called from every method; opens once
        if(file_trace_loads==0) then
            file_trace_loads=open_trace("loads.trace", rq_loads)
        end if
    end subroutine open_load_trace
    subroutine write_daily_load_argument_trace_r4(tag, day, v)
        character (len=*), intent(in) :: tag
        real (kind=4), intent(in) :: v(24)
        integer (kind=2), intent(in) :: day
        integer :: I
        call open_load_trace()
        if(file_trace_loads/=BAD_TRACE_HANDLE) then
            write(file_trace_loads, *) "Day: ", day, &
                "Loads: ", (V(I),I=1,24)

        end if
        
    end subroutine write_daily_load_argument_trace_r4

    subroutine write_daily_load_argument_trace_i4(tag, day, v)
        character (len=*), intent(in) :: tag
        integer (kind=4), intent(in) :: v(24)
        integer (kind=2), intent(in) :: day
        integer :: I
        call open_load_trace()
        if(file_trace_loads/=BAD_TRACE_HANDLE) then
            write(file_trace_loads, *) "Day: ", day, &
                "Loads: ", (V(I),I=1,24)

        end if
        
    end subroutine write_daily_load_argument_trace_i4


end module load_trace

module sic_trace
    use debugtrace
    integer :: file_trace_sic=0
contains
    subroutine open_sic_trace
        if(file_trace_sic==0) then
            file_trace_sic=open_trace("sort_incr_pos.trace", rq_sic)
        end if
        
    end subroutine open_sic_trace
    
    subroutine write_sic_trace(tag, iSup)
        character(len=*), intent(in) :: tag
        integer (kind=2) :: iSup
        if(file_trace_sic==0) then
            call open_sic_trace()
        end if
        call write_trace_int2(file_trace_sic, tag, iSup)
        
    end subroutine write_sic_trace
    
end module sic_trace
module unitnm_trace
    use debugtrace
    implicit none
    integer :: file_trace_un=0
    contains
    subroutine open_unitnm_trace
        implicit none
        if(file_trace_un==0) then
            file_trace_un=open_trace("unitnm.trace", rq_un)
        end if
        
    end subroutine open_unitnm_trace
    subroutine write_unitnm_trace(tag, i, unit_name)
        implicit none
        character (len=*), intent(in) :: tag
        integer (kind=2), intent(in) :: i ! Index
        character (len=*), intent(in) :: unit_name
        
        call open_unitnm_trace()
        if(file_trace_un/=BAD_TRACE_HANDLE) then    
            write(file_trace_un, *) trim(tag), &
                " index ", i, " Name ", trim(unit_name), " "

        end if
        
    end subroutine write_unitnm_trace
    
    
end module unitnm_trace





