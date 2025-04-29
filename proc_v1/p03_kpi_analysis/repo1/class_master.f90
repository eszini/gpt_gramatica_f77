
module class_master
    type t_ns_class_master
    real :: afdc2,amtexp,adjafc,adjdft 
    real :: assets,bdpafc,bkdppt,cashex,ce,cep,common_dividends
    real :: common_stock_issued,common_stock_bought
    real :: ddcum,depacc,depamt,depexp,depsl,exexp
    real :: ffexp,fpafc1,fpce,gopinc,itcdfp
    real :: liabil,debt_file_ltd_issued
    real :: debt_file_preferred_issued
    real :: mnexp,nfafc1,nfafc2
    real :: nfce,nfes,nfexp,ni,nopinc,opexp
    real :: opexpt,opextx,ppexp
    real :: psiss
    real :: rb,rbdtax,rearnp,retire,stdiss
    real :: tafdcb,tafexp,titcpy
    real :: txoth,txprop,txss,wcchan,sl_tax_dep,tpcapinsrt,ttxprefdep
    real :: adj_rate_revenue
    real :: add_clause_exp,base_rate_exp,not_collected_exp
    real :: inc_other1_expense,inc_other2_expense
    real :: inc_other3_expense,nf_owned,nf_leased,nf_expense_ratebase
    real :: leased_nfce
    
    ! ratebase additions
    real :: rb_pcapinrst,rb_txexp,rb_txprefdep,rb_amrte,rb_itcpy,rb_itc
    real :: rb_ddamrte,def_tax_ratebase,regulated_tax_dep
    real :: regulated_book_dep,rb_afdc1
    real :: property_tax_gpv,property_tax_npv
    real :: btl_state_taxes,btl_federal_taxes
    real :: btl_state_exexp_tax_benefit
    real :: btl_federal_exexp_tax_benefit
    
    ! taxes common blocks all variables are real
    real::  fdbkinco,fdboktax
    real :: fdbokoffsetinc,fdbokforwardloss,fdboknoltaken
    real :: fed_income_tax_deductions,txaban,itcused
    real :: fdtaxadj,fdtaxpaid,stboktax,stbokinco
    real :: stbokoffsetinc,stbokforwardloss,stboknoltaken
    real :: state_income_tax_deductions,othsttxadj,sttaxadj
    real :: itcavail,itcbook,txnorm
    real :: federal_txdefp
    end type t_ns_class_master
    
    type (t_ns_class_master) :: ns_class_master

end module class_master
