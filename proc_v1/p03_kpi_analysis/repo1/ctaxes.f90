module ctaxes
implicit none
    type t_ns_ctaxes
    real :: fdbkinco,fdboktax
    real :: fdbokoffsetinc,fdbokforwardloss,fdboknoltaken
    real :: fdmtxd,txaban,itcused,fdtaxadj,fdtaxpaid,stboktax,stbokinco
    real :: stbokoffsetinc,stbokforwardloss,stboknoltaken
    real :: stmtxd,othsttxadj,sttaxadj,itcavail,itcbook,txnorm
    real :: federal_txdefp,adjtxnorm
    end type t_ns_ctaxes
    
    ! SAVE required by Lahey, and should be implicit per the Fortran standard.
    type(t_ns_ctaxes), save :: ns_ctaxes !ctaxes namespace
    
end module ctaxes