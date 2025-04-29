module class_ctaxes
implicit none
    type t_ns_class_ctaxes
        real :: FDBKINCO,FDBOKTAX
        real ::  FDBOKOFFSETINC,FDBOKFORWARDLOSS,FDBOKNOLTAKEN
        real ::  FED_INCOME_TAX_DEDUCTIONS,TXABAN,ITCUSED
        real ::  FDTAXADJ,FDTAXPAID,STBOKTAX,STBOKINCO
        real ::  STBOKOFFSETINC,STBOKFORWARDLOSS,STBOKNOLTAKEN
        real ::  STATE_INCOME_TAX_DEDUCTIONS,OTHSTTXADJ,STTAXADJ
        real ::  ITCAVAIL,ITCBOOK,TXNORM
        real ::  FEDERAL_TXDEFP
        real ::  ADJTXNORM
    end type t_ns_class_ctaxes
    
    type (t_ns_class_ctaxes) :: ns_class_ctaxes ! Namespace
    
end module class_ctaxes