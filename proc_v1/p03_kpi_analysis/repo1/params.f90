module params
implicit none

! *** WARNING ***
! max_prod_types might be up to five larger for tf_objt2.for.
! See DAILY_OPTION_OBJECT function for possibly-affected code. (That function 
! is not currently called from anywhere, however.)

! max_prod_types went to 23 from 19. 19 was
! the last increment added for DG, so 
! 20-23 are OW, HB, H2, CS

integer, parameter :: newgen_capsize=10

integer, parameter :: max_technology_counters=16

character(len=512) :: working_folder=""
 
contains
subroutine params_init()
character(len=512) :: wf
    call curdir(" ", working_folder)
    ! For dbg -- module variable working_folder not
    ! visible in debugger.
    wf=working_folder 
    working_folder=trim(working_folder)

end subroutine params_init

end module params