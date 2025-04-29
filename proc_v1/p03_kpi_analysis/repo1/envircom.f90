
     module envircom
     implicit none

!
!      ENVIRONMENTAL VARIABLES
!
!     common: ENVIR, ENVIRC, ENVIR_OUTPUT

	CHARACTER (len=1) :: EMIS_STRAT(5)

        real :: NET_ANN_EMIS(5)
	real :: EMIS_EXPENSE(5)
	real :: EMIS_REVENUE(5)

	real :: epa_eas
	real :: EMIS_DISPATCH_ADDER(5)
	real :: CT_SO2_ANNUAL
	real :: EMIS_CAP(5)
	real :: EPA_SETASIDE_RATE
	real :: EPA_SETASIDE_REVENUE
	real :: EPA_SETASIDE_CREDITS
	real :: PURCH_CRED_PRICE(5)
	real :: SELL_CRED_PRICE(5)
	real :: MIN_BANK(5),MAX_BANK(5)
	real :: ANNUAL_EMIS(5)
	real :: CUM_ANN_EMIS(5)

	real :: EMIS_SELL(5)
	real :: EMIS_PURCH(5)
	real :: INC_EMIS_EXPENSE
	real :: EL_SO2_ANNUAL
	real :: INC_EMIS_REVENUE
	real :: INC_NET_EMIS_EXPENSE

     end module envircom

