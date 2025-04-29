module class_tva_ai_financial
implicit none
    type t_ns_class_tva_ai_financial
        real :: ai_gpv=0
        real :: ai_cash=0,ai_depreciation=0
        real :: ai_afudc1=0
        real :: ai_expense=0
        real :: ai_tax_depreciation=0
        real :: ai_amt_preferences=0
        real :: ai_deferred_tax=0
   end type t_ns_class_tva_ai_financial
   
   ! SAVE required by Lahey, and should be implicit
   type(t_ns_class_tva_ai_financial), save :: &
        ns_class_tva_ai_financial ! Namespace
end module