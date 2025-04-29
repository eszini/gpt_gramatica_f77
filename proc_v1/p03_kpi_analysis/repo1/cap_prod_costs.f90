      module cap_prod_costs
      implicit none
      
!     Capacity-limited production costs
      
        real (kind=8) :: purchase_energy=0.
        
        real :: owned_nuclear_fuel_expense(4)
        real :: leased_nuclear_fuel_expense(4)
        real :: total_nuclear_fuel_expense
        real :: fossil_fuel_costs(4)
        real :: fixed_om_costs
        real :: purchased_power_costs(4)
        real :: purchase_costs
        real :: reg_variable_fossil_costs
        real :: mmbtu_federal_tax
        real :: mmbtu_federal_tax_in_adj_clause
      end module cap_prod_costs

