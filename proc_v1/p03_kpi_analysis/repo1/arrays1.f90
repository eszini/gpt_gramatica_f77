module arrays1


implicit none
    type tns_arrays1
      real :: abands(2),afdc1c(2),afdc2c(2),afdcdp(2),bkdepc(2), &
        bkvcs(2),capitl(2),cepcum(2),comequ(2),cscum(2),csso(2), &
        cwip(2), &
        ddamtc(2),depcum(2),gpv(2),itcdfc(2),linec(2),ltdcum(2), &
        nafc1c(2),nafc2c(2),nfip(2),nfis(2),nnfv(2),npv(2),&
        oasset(2), &
        plant(2),pscum(2),rbcwip(2),rbdd(2),rbnf(2),rbnfip(2), &
        rearnc(2),sales(2),stdcum(2),stdint(2),txdefc(2),wccum(2), &
        oprev(2),stdrte(2),investments(2),  & !40
!  added 8/5/91 for ease of wpsco 
        nuc_decom_fund_bal(2), &
!  added tax items for amt 
        cumtxnorm(2), &
!  added revenue items for price calculations 
        base_revenue(2),other_revenue(2),adj_clause_revenue(2), &
        othltl(2),cum_def_revenues(2),lease_balance(2), &
& !  added values for ratebase calculation 
        rbcwip_afdc_meth2(2),npv_ratebase(2), &
        cum_def_tax_ratebase(2),cum_itc_def_ratebase(2), &
        rb_reg_dd(2), &
& !  added 10/28/92 for kepco &
        deferred_expense_balance(2) ! arrays1_common_block_items = 2*54
    end type tns_arrays1
    type(tns_arrays1) :: ns_arrays1
    
end module arrays1