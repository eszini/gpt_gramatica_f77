module tva_ai_financial
implicit none
    type t_ns_tva_ai_financial
    real :: AI_GPV=0,AI_CASH=0,AI_DEPRECIATION=0,AI_AFUDC1=0
    real :: AI_EXPENSE=0
    real :: AI_TAX_DEPRECIATION=0
    real :: AI_AMT_PREFERENCES=0
    real :: AI_DEFERRED_TAX=0
    end type t_ns_tva_ai_financial
    
    ! SAVE required by LF95, but is believed to be redundant.
    type(t_ns_tva_ai_financial), save :: ns_tva_ai_financial
end module tva_ai_financial