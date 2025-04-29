module fin_results
implicit none
    type t_fin_results
        real :: INC_SALES_REVENUES,INC_FUEL_EXPENSE, &
            INC_VAROM_EXPENSE,INC_FIXEDOM_EXPENSE, &
            INC_PURCHASE_EXPENSE,INC_NUCLEAR_FUEL_EXPENSE, &
            INC_DSM_EXPENSE,INC_DSM_REBATE, &
            INC_NON_UTIL_COSTS, &
            X_PRICE_1,X_PRICE_2,X_PRICE_3, &
            TAXINCOME, &
            INC_ADJUSTMENT_CLAUSE_REVENUES, &
            INC_BASE_RATES_REVENUES, &
            INC_OTHER_REVENUES, &
            INC_BTL_REVENUES
   end type t_fin_results
   type(t_fin_results), save :: ns_fin_results
   
end module fin_results