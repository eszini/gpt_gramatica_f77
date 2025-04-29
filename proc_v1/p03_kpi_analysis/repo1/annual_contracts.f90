module annual_contracts
use prod_arrays_dimensions
implicit none
    real :: annual_contract_capacity(max_contracts)
    real :: annual_contract_energy(max_contracts)
    real :: annual_contract_fixed_cost(max_contracts)
    real :: annual_contract_so2(max_contracts)
    real :: annual_contract_variable_cost(max_contracts)
end module annual_contracts