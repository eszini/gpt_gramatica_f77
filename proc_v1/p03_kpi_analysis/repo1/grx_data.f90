module grx_data
    implicit none
    logical (kind=4) :: grx_converged
    real (kind=4) :: current_co2_dispatch_cost=0.
    integer (kind=4) :: grx_iterations=1
    integer (kind=2) :: retired_options_pntr=0
    real (kind=4) :: next_current_co2_dispatch_cost=0.
    real (kind=4) :: CO2_CAP_EXPENDITURES=0.
    logical (kind=4) :: is_co2_price_mrkt_capped=.false.
    real (kind=4) :: co2_cap_increase_from_market=0

end module grx_data