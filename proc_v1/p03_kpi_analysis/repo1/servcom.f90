module servcom
implicit none
    integer (kind=2), parameter :: max_service_groups=6
    integer (kind=2), parameter :: max_service_items=5


    real :: transmission_charges
	real :: wheeling_charges
    real :: stand_by_trans_charges
	real :: dispatching_charges
    real :: other_service_charges
    real :: service_revenues
    real :: service_expenses
    real :: service_adj_clause_expenses
    real :: service_base_rate_expenses
    real :: service_base_revenue_offset
    real :: service_adj_clause_offset
    real :: service_group_costs(0:max_service_items,max_service_groups)
end module servcom