module fuelused_decs
use miscmod
use logging
implicit none
INTEGER (kind=2), allocatable :: FUEL_PRICE_POINTR(:)
real, allocatable :: NEW_FUEL_PRICE(:,:,:)
real :: P_COST_1=0.

integer(kind=2) :: fuel_pointer
contains
subroutine check_fuel_price_pointer()
integer :: index, ub
real :: pc

	pc=P_COST_1 !P_COST_1 cannot be seen in debugger.
	
	 ub=ubound(FUEL_PRICE_POINTR,1)
	 IF(pc < 0.) THEN
		index=ABS(INT(pc,2))
		if(index>ub) THEN
			call end_program("fuelused_decs:00001 " // &
			"Index " // trim(itos(int(index))) // " exceeds " // &
			"bounds (" //trim(itos(int(ub))) // ").")
		endif
	endif


end subroutine check_fuel_price_pointer
subroutine GET_A_CAP_MARKET_PRICE(R_PRIMARY_FUEL_ID, &
      RATE, START_MO,iYR)
implicit none
integer (kind=2), intent(in) :: iYR, START_MO
real, intent(in) :: R_PRIMARY_FUEL_ID
real (kind=4), intent(inout) :: RATE
integer, save :: timesthru=0
character (len=1024) :: outstr
integer :: index, ub
real :: pc
        pc=p_cost_1

        timesthru=timesthru+1


         ! TODO: What is this, and why is
         ! an index stored in a REAL field?
         P_COST_1 = R_PRIMARY_FUEL_ID
         pc=P_COST_1
         if(P_COST_1== -3.48990000E+004) then
            call end_program("fuelused_decs:0003 - P_Cost_1 " // &
            "value invalid.")
            
         end if
         
! First time through should have these values here:
! p_cost_1 : P_COST_1 = -1.00100000E+004
! r_primary_fuel_id : R_PRIMARY_FUEL_ID = -1.00100000E+004

         ub=ubound(FUEL_PRICE_POINTR,1)
         if(ub>11000) then
            ub=ub ! Debugstop
         endif
         
         call check_fuel_price_pointer()

         IF(P_COST_1 < 0.) THEN
            index=ABS(INT(P_COST_1,2))

            fuel_pointer=FUEL_PRICE_POINTR(index)
            IF(fuel_pointer <= 0) THEN
               WRITE(4,*) 'No Capacity Price vector',P_COST_1
               WRITE(4,*) 'was found in the Capacity Markets file.'
               WRITE(4,*) 'for Capacity Limited unit '
               WRITE(4,*) '*** line 1011 FUELUSED.FOR ***'
               CALL SEE_WARNING_MESSAGES()
            ENDIF
!
            P_COST_1 = NEW_FUEL_PRICE(fuel_pointer,START_MO,iYR)
            pc=P_COST_1
        else
            RATE=RATE ! debugstop
            pc=p_cost_1
         ENDIF
 
 
         RATE =  P_COST_1
      RETURN
end subroutine GET_A_CAP_MARKET_PRICE
end module fuelused_decs
