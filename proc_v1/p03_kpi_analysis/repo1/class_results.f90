module class_fin_results
implicit none
    type t_class_fin_results
        real :: INC_SALES_REVENUES,INC_FUEL_EXPENSE
        real :: INC_VAROM_EXPENSE,INC_FIXEDOM_EXPENSE
        real :: INC_PURCHASE_EXPENSE,INC_NUCLEAR_FUEL_EXPENSE
        real :: INC_DSM_EXPENSE,INC_DSM_REBATE
        real :: INC_NON_UTIL_COSTS
        real :: X_PRICE_1,X_PRICE_2,X_PRICE_3
        real :: TAXINCOME
        real :: INC_ADJUSTMENT_CLAUSE_REVENUES
        real :: INC_BASE_RATES_REVENUES
        real :: INC_OTHER_REVENUES
        real :: INC_BTL_REVENUES
    end type t_class_fin_results
    
    ! SAVE required by Lahey, and should be implicit per the Fortran standard.
    type(t_class_fin_results), save :: ns_class_fin_results ! Namespace
    
end module class_fin_results
! module extracted from common block class_assets_results in acrslcom.mon.
module class_assets_results
    implicit none
    real :: interest
    real :: ltd_ps_retirements
    real :: retained_earnings
    real :: net_income
    real :: preferred_dividends
    real :: long_term_debt_issued
    real :: net_of_tax_exexp
    real :: preferred_stock_issued
    real :: investment_income
    real :: afudc
    real :: btl_income_taxes
    real :: deferred_taxes_cr
    real :: deferred_taxes_dr
    real :: itc_used
    real :: itc_amortization
    real :: state_taxes
    real :: federal_taxes
    real :: local_taxes
    real :: environmental_tax
    real :: total_expenses_b4_taxes
    real :: state_tax_on_capital
    real :: federal_tax_on_capital
    real :: class_property_taxes
    real :: book_depreciation
    real :: amortization_expense
    real :: ciac_amortization
    real :: srp_ratio
    real :: preferred_issued_this_year
    real(kind=4) :: btl_amortization
    real(kind=4) :: btl_def_taxes_cr
    real(kind=4):: interest_amortization
    real(kind=4):: mark_to_market
    real(kind=4):: payroll_taxes
    real(kind=4):: fe_noncashpension
    real(kind=4):: fe_noncashearningsnoncorp
    real(kind=4):: btl_deferred_taxes_dr
end module class_assets_results

module class_assets_results_2
    implicit none
    type t_ns_class_assets_results_2
    real :: secondary_sales_revenues
    real :: atl_income_taxes
    real :: ouc_tax_payments
    end type t_ns_class_assets_results_2
    type (t_ns_class_assets_results_2), save :: &
        ns_class_assets_results_2
end module class_assets_results_2
