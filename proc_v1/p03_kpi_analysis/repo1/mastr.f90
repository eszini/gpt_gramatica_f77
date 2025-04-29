module mastr
use prod_arrays_dimensions
implicit none
    type t_ns_mastr
        real :: ni
        real :: afdc1=0,afdc2=0,amtexp
        real :: annint(0:max_financial_simulation_years-1)
        real :: annualized_ps_div(0:max_financial_simulation_years-1)
        real :: annualized_ltd_interest &
            (0:max_financial_simulation_years-1)
        real :: bdpafc=0,bkdppt=0,cashex=0,ce=0,cep=0,csdiv=0,csiss
        real :: ddcum=0,depacc=0,depamt=0,depexp=0,depsl=0,exexp
        real :: ffexp=0,fpafc1=0,fpce=0,gopinc=0,itc=0,itcamt=0,itcdfp
        real :: liabil=0, &
            ltdint(0:max_financial_simulation_years-1)=0,ltdiss
        real :: ltdret(0:max_financial_simulation_years-1)=0,mnexp=0, &
            nfafc1=0
        real :: nfafc2
        real :: nfce=0,nfes=0,nfexp=0,nopinc=0,opexp
        real :: opexpt=0,opextx=0,ppexp=0
        real :: psdiv(0:max_financial_simulation_years-1)
        real :: psiss=0,psred(0:max_financial_simulation_years-1)
        real :: rb=0,rbdtax=0,rearnp=0,retire=0,stdiss
        real :: tafdcb=0,tafexp=0,titcpy=0,txdefp=0,txoprv
        real :: txoth=0,txprop=0,txss=0,wcchan=0,sl_tax_dep=0
        real :: tpcapinsrt=0,ttxprefdep
        real :: rate_revenue(0:max_financial_simulation_years-1)
        real :: adj_rate_revenue
        real :: add_clause_exp=0,base_rate_exp=0,not_collected_exp
        real :: inc_other1_expense=0,inc_other2_expense
        real :: inc_other3_expense=0,nf_owned=0,nf_leased=0
        real :: nf_expense_ratebase
        real :: leased_nfce=0,nuc_fuel_tax_expense=0,owned_nf_tax_dep
        real :: nf_cash_expense=0,nf_non_cash_expense
        real :: leasor_nf_tax_dep=0,leasor_fuel_depreciation

        ! ratebase additions
        real :: rb_pcapinrst=0,rb_txexp=0,rb_txprefdep=0,rb_amrte=0
        real :: rb_itcpy=0
        real :: rb_itc
        real :: rb_ddamrte=0,def_tax_ratebase=0,regulated_tax_dep
        real :: regulated_book_dep=0,rb_afdc1
        real :: property_tax_gpv=0,property_tax_npv
        real :: btl_state_exexp_tax_benefit
        real :: btl_federal_exexp_tax_benefit
        real :: net_of_tax_exexp
        real :: ea_def_tax_ratebase
        real :: ea_def_tax
    end type t_ns_mastr

! SAVE required by Lahey, and should be implicit per the Fortran standard.
type(t_ns_mastr), save :: ns_mastr

end module mastr