module capacity_arrays
use abb_capmarketrptdata
use miscmod
implicit none

! cg_count was moved from icap_io (cn_objt originally) to 
! here, because it's what's being used to iterate over the
! cm_planning_reserve_margin array and is twice the size (8) of 
! the original value used to dimension it (0:upper_trans_group) (4)
integer (kind=2) :: CG_COUNT=0

! *** WARNING ***
! max_prod_types might be up to five larger for tf_objt2.for.
! See DAILY_OPTION_OBJECT function for possibly-affected code. 
! (That function 
! is not currently called from anywhere, however.)
! Issue with max_prod_types:
! max_prod_types allocations:
!  19 in clreport; 18 in MARGNOBJ; 24 in tf_objt2
! .

! max_prod_types (clr) went to 23 from 19. 19 was
! the last increment added for DG, so 
! 20-23 are OW, HB, H2, CS
! 1=COAL; 2=GAS ; 3=OIL; 4=NUCLEAR; 5=WATER; 6=OTHER 
    logical :: report_compatibility_mode=.true.
    integer (kind=2), parameter :: NUMPMS=24
    integer (kind=2), parameter :: max_prod_types_clr=19 !clreport
        !integer (kind=2), parameter :: max_prod_types_clr=24
    integer (kind=2), parameter :: old_max_prod_types_clr=19
    ! integer, parameter :: MAX_PROD_TYPES_mo=19 ! margnobl
    integer (kind=2), parameter :: max_prod_types_tfo=24 ! tf_objt
    integer (kind=2), parameter :: max_prod_types_pd=18 ! Picadisp
    integer (kind=2), parameter :: MAX_FUEL_TYPES_pd=6 ! Picadisp
    integer (kind=2), parameter :: max_fuel_types_clr=6
    integer (kind=2) :: NUM_FUEL_CATEGORIES=11
    
    
	
	 ! 080722. ADDED PM'S
	 ! 030124, added PMs, jtr
    real (kind=4) :: RPS_THERMAL_DB(2,400,NumPMs,0:12)
    real (kind=4), allocatable :: SYSTEM_PROD_BY_TG_BY_MWH(:,:,:)
    real, allocatable :: LOAD_BY_TG_BY_MWH(:,:)
    real (kind=4), allocatable :: QUALIFYING_GEN_DB(:,:,:)
    real (kind=4), allocatable :: DEV_TG_CAP(:,:,:)
    real (kind=4), allocatable ::   DEV_NEW_TG_CAP(:,:,:)
    real (kind=4), allocatable ::  SAVED_DEV_NEW_TG_CAP(:,:,:)
    real (kind=4), allocatable :: RPS_HYDRO_DB(:,:,:,:)
    real (kind=4), allocatable ::  TG_CapacitySupplyObligation(:,:)
    real (kind=4), allocatable :: GRX_QUAL_GEN_DB(:,:,:), &
         SAVE_GRX_QUAL_GEN_DB(:,:,:)

    real (kind=4) :: TG_ResourceActualPreference
    real (kind=4), allocatable :: TG_EffectiveCapacity(:,:)
    integer (kind=2) :: TRANS_PROD_NO=0
    integer :: TRANS_PROD_REC
    ! ACCUMULATE BY TRANSACTION GROUP AND PROD TYPE
    real, allocatable :: ANNUAL_LOAD_BY_TG_BY_MWH(:,:)

    real, allocatable ::  ANNUAL_PROD_BY_TG_BY_MW(:,:), & 
        PROD_BY_TG_BY_MWH(:,:), ANNUAL_PROD_BY_TG_BY_MWH(:,:), &
        PROD_BY_TG_BY_MW(:,:),  MW_BY_TG_BY_FUEL(:,:), &
        ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
        ANNUAL_MW_BY_TG_BY_FUEL(:,:), MWH_BY_TG_BY_FUEL(:,:)
    real (kind=4) :: WH_MONTH_ENERGY
    integer (kind=2) :: T_I,T_J
    logical (kind=1) :: TRANSACT_PROD_REPORT_ACTIVE=.false.
    
    ! ACCUMULATE BY TRANSACTION GROUP AND FUEL TYPE
    real, allocatable :: ANNUAL_MWH_BY_TG_BY_FUEL(:,:) 
contains
subroutine allocate_rps_hydro_db()
    integer :: allocstat
	if(allocated(RPS_HYDRO_DB)) Then
		deallocate(RPS_HYDRO_DB)
	endif
         ! Todone:  Move rps_hydro_db to capacity_arrays module.
    ALLOCATE(RPS_HYDRO_DB(2,400,NUMPMS,0:12), stat=allocstat)
    call check_alloc("capacity_arrays:0002", "RPS_HYDRO_DB", allocstat)
    
end subroutine allocate_rps_hydro_db
subroutine  allocate_grx_db_arrays(active_rps)
integer (kind=2), intent(in) :: active_rps
integer :: ios
	if(allocated(QUALIFYING_GEN_DB)) then
		deallocate(QUALIFYING_GEN_DB)
	endif
	if(allocated(GRX_QUAL_GEN_DB)) then
		deallocate(GRX_QUAL_GEN_DB)
	endif
	if(allocated(SAVE_GRX_QUAL_GEN_DB)) then
		deallocate(SAVE_GRX_QUAL_GEN_DB)
	endif
	
    allocate(QUALIFYING_GEN_DB(ACTIVE_RPS,0:NUMPMS,2), stat=ios)
    call check_alloc("capacity_arrays:0005", "QUALIFYING_GEN_DB", ios)
	allocate(GRX_QUAL_GEN_DB(ACTIVE_RPS,0:NUMPMS,2), stat=ios)
    call check_alloc("capacity_arrays:0004", "grx_qual_gen_db", ios)

	allocate(SAVE_GRX_QUAL_GEN_DB(ACTIVE_RPS,0:NUMPMS,2), stat=ios)
    call check_alloc("capacity_arrays:0006", "SAVE_GRX_QUAL_GEN_DB", ios)
	
end subroutine allocate_grx_db_arrays

subroutine allocate_dev_tg_cap(nr_trans_classes, STUDY_PERIOD)
integer, intent(in) :: nr_trans_classes, STUDY_PERIOD
integer :: allocstat

     if (allocated(DEV_TG_CAP)) then
        deallocate(DEV_TG_CAP)
     endif
     if(study_period==0) then
        call end_program("capacity_arrays:0003 - study_period is zero.")
     end if
     
     ALLOCATE(DEV_TG_CAP(0:16,0:MAX(1,nr_trans_classes), &
       STUDY_PERIOD))
       
     if(allocated(DEV_NEW_TG_CAP)) then
        deallocate(DEV_NEW_TG_CAP)
     endif
     ALLOCATE (DEV_NEW_TG_CAP(0:21,0:MAX(1,nr_trans_classes), &
                                                  STUDY_PERIOD))
     if (allocated(SAVED_DEV_NEW_TG_CAP)) then
        deallocate(SAVED_DEV_NEW_TG_CAP)
     endif
     allocate(SAVED_DEV_NEW_TG_CAP(0:21,0:MAX(1, &
       nr_trans_classes), STUDY_PERIOD), stat=allocstat) 
     call check_alloc("capacity_arrays:0001", "saved_dev_new_tg_cap", &
        allocstat)

end subroutine allocate_dev_tg_cap
end module capacity_arrays
module rangechecker
use end_routine
use string
implicit none
contains
    subroutine check_bounds_i4_4s(array, key, array_name, idx1, idx2, idx3, idx4)
        
        integer :: index
        integer (kind=4), intent (in) :: array(:,:,:,:)
        integer (kind=2), intent(in) :: idx1, idx2, idx3, idx4
        integer :: lb1, lb2, lb3, lb4
        integer :: ub1, ub2, ub3, ub4
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)
        lb3=lbound(array,3)
        ub3=ubound(array,3)
        lb4=lbound(array,4)
        ub4=ubound(array,4)
        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.
        else if (lb3>idx3 .or. ub3<idx3) then
            attempt_will_fail=.true.
        else if (lb4>idx4 .or. ub4<idx4) then
            attempt_will_fail=.true.
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array."
                call end_program(er_message)
        end if

        
    end subroutine check_bounds_i4_4s
    subroutine check_bounds_r4_4s(array, key, array_name, idx1, idx2, idx3, idx4)
        use string
        implicit none

        
        real (kind=4), intent (in) :: array(:,:,:,:)
        integer (kind=2), intent(in) :: idx1, idx2, idx3, idx4
        integer :: lb1, lb2, lb3, lb4
        integer :: ub1, ub2, ub3, ub4
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        logical :: is_rps_hydro_db
        character (len=50) :: an_upper
        
        an_upper=trim(ucase(array_name))
        
        is_rps_hydro_db= (trim(an_upper) ==  "RPS_HYDRO_DB")
        

        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)
        lb3=lbound(array,3)
        ub3=ubound(array,3)
        lb4=lbound(array,4)
        ub4=ubound(array,4)
        if(idx4==0 .and. is_rps_hydro_db) then
            lb4=0 ! Debugstop
        end if
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.
        else if (lb3>idx3 .or. ub3<idx3) then
            attempt_will_fail=.true.
        else if (lb4>idx4 .or. ub4<idx4) then
            attempt_will_fail=.true.
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of " // &
                trim(array_name) // "[" // &
                trim(itos(int(ub1))) // "," // &
                trim(itos(int(ub2))) // "," // &
                trim(itos(int(ub3))) // "," // &
                trim(itos(int(ub4))) // "]" // &
                 " with [" // &
                trim(itos(int(idx1))) // "," // trim(itos(int(idx2))) // "," // &
                trim(itos(int(idx3))) // "," // trim(itos(int(idx4))) // "]"

                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r4_4s

    subroutine check_bounds_r4_3s(array, key, array_name, idx1, idx2, idx3)

        integer :: index

        real (kind=4), intent (in) :: array(:,:,:)
        integer (kind=2), intent(in) :: idx1, idx2, idx3
        integer :: lb1, lb2, lb3
        integer :: ub1, ub2, ub3
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)
        lb3=lbound(array,3)
        ub3=ubound(array,3)

        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.
        else if (lb3>idx3 .or. ub3<idx3) then
            attempt_will_fail=.true.
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array."
                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r4_3s
    subroutine check_bounds_r_2s(array, key, array_name, idx1, idx2)
        integer, parameter :: num_bounds=2
        integer :: index

        real, intent (in) :: array(:,:)
        integer (kind=2), intent(in) :: idx1, idx2
        integer :: lb1, lb2
        integer :: ub1, ub2
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)


        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.

        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array."
                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r_2s
    subroutine check_bounds_r_3s(array, key, array_name, idx1, idx2, &
        idx3, extra_data)

        integer :: index
        real, intent (in) :: array(:,:,:)
        integer (kind=2), intent(in) :: idx1, idx2, idx3
        integer :: lb1, lb2, lb3
        integer :: ub1, ub2, ub3
        character (len=*), intent(in) :: key, array_name
        character (len=*), optional, intent(in) :: extra_data
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)
        lb3=lbound(array,3)
        ub3=ubound(array,3)

        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.
        else if (lb3>idx3 .or. ub3<idx3) then
            attempt_will_fail=.true.
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array."
                if (present(extra_data)) then
                    er_message=trim(er_message) // " Extra data: " // &
                    trim(extra_data) // "."
                end if
                
                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r_3s
    subroutine check_bounds_r_4s(array, key, array_name, idx1, idx2, &
        idx3, idx4, extra_data)
        
        integer :: timeshere=0
        integer :: index
        real, intent(inout) :: array(:,:,:,:)
        integer (kind=2) :: idx1, idx2, idx3, idx4
        integer :: lb1, lb2, lb3, lb4
        integer :: ub1, ub2, ub3, ub4
        character (len=*):: key, array_name
        character (len=*) :: extra_data
        logical :: attempt_will_fail
        
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)
        lb3=lbound(array,3)
        ub3=ubound(array,3)
        lb4=lbound(array,4)
        ub4=ubound(array,4)

        timeshere=timeshere+1
        if(timeshere>143) then 
            timeshere=timeshere ! Debugstop
        end if
        if(ub1>45000) then
           er_message= "rangechecker:0001 - Stack corrupt. " // &
            "Timeshere=" // trim(itos(timeshere)) // " (called for " // &
            trim(key) // ")."
            if(len_trim(extra_data)>0) then
                er_message=trim(er_message) // &
                "Extra data: " // trim(extra_data)
            end if
            
            call end_program(er_message)
            
        end if
        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.
        else if (lb3>idx3 .or. ub3<idx3) then
            attempt_will_fail=.true.
        else if (lb4>idx4) then
            attempt_will_fail=.true.
        else if (ub4<idx4) then
            attempt_will_fail=.true.
        end if
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array. " // &
                "Timeshere=" // trim(itos(int(timeshere))) // "."

                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r_4s
    subroutine check_bounds_r4_1s(array, key, array_name, idx1)
        integer, parameter :: num_bounds=1
        integer :: index
        real (kind=4), intent (in) :: array(:)
        integer (kind=2), intent(in) :: idx1
        integer :: lb1
        integer :: ub1
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of " // &
                trim(array_name) // "[" // &
                trim(itos(int(ub1))) //  "]" // &
                 " with [" // &
                trim(itos(int(idx1))) // "]"

                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r4_1s
    
 subroutine check_bounds_r_1s(array, key, array_name, idx1)
        integer, parameter :: num_bounds=1
        integer :: index
        real, intent (in) :: array(:)
        integer (kind=2), intent(in) :: idx1
        integer :: lb1
        integer :: ub1
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of " // &
                trim(array_name) // "[" // &
                trim(itos(int(ub1))) //  "]" // &
                 " with [" // &
                trim(itos(int(idx1))) // "]"

                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r_1s
  subroutine check_bounds_r4_2s(array, key, array_name, idx1, idx2)
        integer, parameter :: num_bounds=2
        integer :: index
        real (kind=4), allocatable, intent (in) :: array(:,:)
        integer (kind=2), intent(in) :: idx1, idx2
        integer :: lb1, lb2
        integer :: ub1, ub2
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)


        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.

        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array."
                call end_program(er_message)
        end if

        
    end subroutine check_bounds_r4_2s
   subroutine check_bounds_i4_1s(array, key, array_name, idx1, zerobased)
        
        integer :: index
        integer (kind=2), intent (in) :: array(:)
        integer (kind=2), intent(in) :: idx1
        integer :: lb1
        integer :: ub1
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        logical, optional :: zerobased
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        if(present(zerobased)) then
            lb1=0
        end if
        ub1=ubound(array,1)
        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array."
                call end_program(er_message)
        end if

        
    end subroutine check_bounds_i4_1s
    subroutine check_bounds_i4_2s(array, key, array_name, idx1, idx2)
        
        integer :: index
        integer (kind=4), intent (in) :: array(:,:)
        integer (kind=2), intent(in) :: idx1, idx2
        integer :: lb1, lb2
        integer :: ub1, ub2
        character (len=*), intent(in) :: key, array_name
        logical :: attempt_will_fail
        
        attempt_will_fail=.false.
        lb1=lbound(array,1)
        ub1=ubound(array,1)
        lb2=lbound(array,2)
        ub2=ubound(array,2)
        
        
        if (lb1>idx1 .or. ub1<idx1) then
            attempt_will_fail=.true.
        else if(lb2>idx2 .or. ub2<idx2) then
            attempt_will_fail=.true.
        
        end if
        
        
        if(attempt_will_fail) then
               er_message=trim(key) // " - An attempt is being made to " // &
               "make an out-of-bounds request of the " // &
                trim(array_name) // " array."
                call end_program(er_message)
        end if

        
    end subroutine check_bounds_i4_2s
 
end module rangechecker
