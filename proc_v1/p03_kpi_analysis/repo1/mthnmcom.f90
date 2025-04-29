!     ******************************************************************
!     Mthnmcom.mon
!        !opyright(c) DrG Solutions 2011
!
!        !reated: 7/2/2011 6:29:42 PM
!     Author : Mark S Gerber
!     Last change: MSG 12/14/2011 6:08:39 PM
!     ******************************************************************

module mthnmcom
      implicit none

!
! MONTHLY VARIABLE NAMES COPYRIGHT (C) 1999 M.S. GERBER & ASSOCIATES, INC.
!
      integer(kind=2),parameter:: BAL_SHEET_VARS=90  ! 90 Next Fix monthly output if changing BAL_SHEET_VARS
      integer(kind=2),parameter:: INCOME_VARS=500    ! 323 Next available income item
      integer(kind=2),parameter:: CASH_VARS=250      ! 217 Next available cash item
      integer(kind=2),parameter:: CPL_TAX_VARS=45    ! 43 Next avaiable CPL item
      integer(kind=2),parameter:: TAX_VARS=50        ! 23 Next avaiable Tax item
!
! added 12/1/99 to keep Skip Seekamp in older version dated 11/19/99
!
      integer(kind=2),parameter:: ipl_operating_revenues=249
      integer(kind=2),parameter:: mty_depn_n_amortn=250
      integer(kind=2),parameter:: monthly_tot_opn_rvnues=30
      integer(kind=2),parameter:: monthly_govt_revenues=18
      integer(kind=2),parameter:: monthy_taxes_nonincome=251
      integer(kind=2),parameter:: ipl_elec_plan_fuel_costs=258
      integer(kind=2),parameter:: ipl_elect_pln_purch_costs=259
      integer(kind=2),parameter:: ipl_misc_regltd_revnu=260
!
! INCOME STATEMENT VARIABLES
!
! IF THE EXPENSE OR REVENUE IS IN THE EXPENSE FILE PUT THE NAME IN
!


      
      integer(kind=2),parameter:: monthly_secondary_sales=3
      integer(kind=2),parameter:: monthly_other_revenue=4
      integer(kind=2),parameter:: monthly_relationship_revenues=10
      integer(kind=2),parameter:: monthly_bulk_power=15
      integer(kind=2),parameter:: monthly_capacity_sales=17
      integer(kind=2),parameter:: mthy_incm_rsv_mgn_cap_sales=237
      integer(kind=2),parameter:: mty_xpns_rsv_mgn_cap_pchs=238
      integer(kind=2),parameter:: Monthly_Fossil_Fuel=41
      integer(kind=2),parameter:: monthly_purchased_power=42
      integer(kind=2),parameter:: monthly_variable_oandm=43
      integer(kind=2),parameter:: monthly_fixed_oandm=44
      integer(kind=2),parameter:: monthly_other_oandm=45
      integer(kind=2),parameter:: monthly_purchased_gas=46
      integer(kind=2),parameter:: Monthly_Other=47
      integer(kind=2),parameter:: monthly_ownd_nuc_fl_expense=48
      integer(kind=2),parameter:: Mthy_Leasd_Nucl_Fuel_Expns=239
      integer(kind=2),parameter:: monthly_nuclr_fuel_tax_expns=240
      integer(kind=2),parameter:: monthly_gas_storage_amort=226
      integer(kind=2),parameter:: monthly_fuel_inventory_amort=227
      integer(kind=2),parameter:: mnth_mtl_and_supplys_amort=228
      integer(kind=2),parameter:: monthly_vacation_pay=77
      integer(kind=2),parameter:: MonthlyPensionExpense=78
      integer(kind=2),parameter:: MonthlySTORMExpense=79
      integer(kind=2),parameter:: Monthly_Nuclear_Fuel_Expense=49
      integer(kind=2),parameter:: MonthlyDSMExpense=50
      integer(kind=2),parameter:: MonthlyDSMRebate=51
      integer(kind=2),parameter:: MonthlyServiceTransactions=53
      integer(kind=2),parameter:: MonthlyEmissionCredits=54
      integer(kind=2),parameter:: MonthlyDOEDecommissioning=55
      integer(kind=2),parameter:: MonthlyDOEDisposal=56
      integer(kind=2),parameter:: MonthlyTransmissionOperation=59
      integer(kind=2),parameter:: monthly_xmsn_maintnc=60
      integer(kind=2),parameter:: MonthlyDistributionOperation=61
      integer(kind=2),parameter:: MonthlyDistributionMaintenance=62
      integer(kind=2),parameter:: MonthlyCustomerAccounts=63
      integer(kind=2),parameter:: MonthlyCustomerServices=64
      integer(kind=2),parameter:: MonthlySalesExpense=65
      integer(kind=2),parameter:: MonthlyAGOperations=66
      integer(kind=2),parameter:: MonthlyAGMaintenance=67
      integer(kind=2),parameter:: MonthlyDeferredFuelExpense=159
      integer(kind=2),parameter:: Monthly_Amortization=68
      integer(kind=2),parameter:: Mthly_dfrd_rev_Amortization=69

      integer(kind=2),parameter:: MonthlyATLLeasePayment=208
      integer(kind=2),parameter:: Monthly_Lease_Interest_Expense=75
      integer(kind=2),parameter:: Mthly_Lease_Amort_xpns=70
      integer(kind=2),parameter:: Monthly_Lease_BTL_Amort_Expense=71
      integer(kind=2),parameter:: Monthly_Goodwill_Amort=181
      integer(kind=2),parameter:: Monthly_Regulatory_Assets_Amort=182
      integer(kind=2),parameter:: Monthly_FASB_109_Amort=183
      integer(kind=2),parameter:: Monthly_FASB_133_Amort=184
      integer(kind=2),parameter:: Monthly_FASB_143_Amort=234
      integer(kind=2),parameter:: Monthly_CIAC_Amort=211
      integer(kind=2),parameter:: Monthly_ATL_Gain_Amort=203
      integer(kind=2),parameter:: Monthly_BTL_Gain_Amort=204
      integer(kind=2),parameter:: monthly_other_atl_amort=205
      integer(kind=2),parameter:: monthly_other_btl_amort=206
      integer(kind=2),parameter:: mthly_debit_fl_Pchs_pwr_amort=277
      integer(kind=2),parameter:: mthy_common_stock_issue_amort=200
      integer(kind=2),parameter:: Monthly_Other_Debit_Amort=185
      integer(kind=2),parameter:: Monthly_Book_Depreciation=72    
      integer(kind=2),parameter:: Monthly_Retirement_Expense=76
      integer(kind=2),parameter:: Monthly_Property_Taxes=81
      integer(kind=2),parameter:: MonthlyExpFilePropertyTaxes=273
      integer(kind=2),parameter:: mth_modl_calcd_prop_taxes=274
      integer(kind=2),parameter:: monthly_oth_taxes=82
      integer(kind=2),parameter:: MonthlyExpFileOtherTaxes=275
      integer(kind=2),parameter:: Monthly_ITC_Amortization=83
      integer(kind=2),parameter:: Monthly_Operating_Revenue_Tax=84
      integer(kind=2),parameter:: mthy_exp_file_opng_revnue_tax=276
      integer(kind=2),parameter:: mthy_state_atl_incm_tax_pd=85
      integer(kind=2),parameter:: Monthly_State_Tax_on_Capital=86
      integer(kind=2),parameter:: mthy_fed_atl_income_tax_pd=87
      integer(kind=2),parameter:: Monthly_Federal_Tax_on_Capital=88
      integer(kind=2),parameter:: Monthly_Income_Tax_Deferrals_Dr=89
      integer(kind=2),parameter:: Monthly_Income_Tax_Deferrals_Cr=90
      integer(kind=2),parameter:: mth_othr_incm_tax_dfrls_cr=242
      integer(kind=2),parameter:: mthy_ltd_ps_incm_tax_dfrls_cr=243
      integer(kind=2),parameter:: mthy_nf_incm_tax_dfrls_cr=241
      integer(kind=2),parameter:: mthly_pretax_expenses=31
      integer(kind=2),parameter:: monthly_total_revenues=30
      integer(kind=2),parameter:: btl_monthly_deferred_revenues=9
      integer(kind=2),parameter:: BTL_Monthly_Total_Other_Income=109
      integer(kind=2),parameter:: BTL_monthly_other_income=5
      integer(kind=2),parameter:: BTL_Monthly_NoTax_Other_Income=16
      integer(kind=2),parameter:: BTL_Monthly_Interest_Income=101
      integer(kind=2),parameter:: Monthly_Investment_Earnings=157
      integer(kind=2),parameter:: mthly_invstmt_earngs_rcvbl=271
      integer(kind=2),parameter:: mthy_nclr_dcmssng_fd_rngs=231
      integer(kind=2),parameter:: mthy_net_tax_nclr_dcm_fd_rngs=232
      integer(kind=2),parameter:: mthy_rtrmt_med_fd_rngs=233
      integer(kind=2),parameter:: Monthly_Dividend_70_Earnings=209
      integer(kind=2),parameter:: mty_rglr_dvd_income=246
      integer(kind=2),parameter:: mty_mdl_lt_nvst_income=247
      integer(kind=2),parameter:: mty_totl_lti_income=102
      integer(kind=2),parameter:: BTL_Monthly_STInvestmet_Income=103
      integer(kind=2),parameter:: monthly_notes_receivable_income=244
      integer(kind=2),parameter:: mt_dbt_file_linvst_income=245
      integer(kind=2),parameter:: BTL_Monthly_Expenses=58
      integer(kind=2),parameter:: BTL_Monthly_Income_Taxes=96
      integer(kind=2),parameter:: BTL_Mthly_Income_Tax_Deferrals=97
      integer(kind=2),parameter:: BTL_Monthly_Amortization=73
      integer(kind=2),parameter:: Monthly_AFUDC_Total=106     
      integer(kind=2),parameter:: Monthly_AFUDC_Equity=107   
      integer(kind=2),parameter:: Monthly_AFUDC_Borrowed=108   
      integer(kind=2),parameter:: Monthly_LTD_Total_Interest=116
      integer(kind=2),parameter:: MonthlySTDInterest=117
      integer(kind=2),parameter:: Monthly_LTD_Cash_Interest=118
      integer(kind=2),parameter:: mty_intst_on_nts_pyble=161
      integer(kind=2),parameter:: Monthly_LTD_Amort_Interest=119
      integer(kind=2),parameter:: Monthly_Unused_Fed_Tax_Credits=131
      integer(kind=2),parameter:: Monthly_Subsidiary_Income=132
      integer(kind=2),parameter:: Monthly_Extraordinary_Items=133
      integer(kind=2),parameter:: Monthly_CommonDividends=121
      integer(kind=2),parameter:: Monthly_Total_PS_Dividends=122
      integer(kind=2),parameter:: Monthly_Regular_PS_Dividends=123
      integer(kind=2),parameter:: Monthly_MIPS_Dividends=124
      integer(kind=2),parameter:: Monthly_Total_Taxes_Expense=150
      integer(kind=2),parameter:: Monthly_Op_Income=151
      integer(kind=2),parameter:: Monthly_INCOME_BEFORE_INTEREST=152
      integer(kind=2),parameter:: Monthly_INCOME_AFTERINTEREST=153
      integer(kind=2),parameter:: Monthly_NetIncome=154
      integer(kind=2),parameter:: Monthly_Earnings_2_Common=155
      integer(kind=2),parameter:: Monthly_RETAINED_EARNINGS=156
      integer(kind=2),parameter:: Monthly_Shares_Outstanding=160
      integer(kind=2),parameter:: mty_impa_net_incm_cvrg=160
      integer(kind=2),parameter:: Monthly_Earnings_per_Share=224
      integer(kind=2),parameter:: Quarterly_Earnings_perShare=225
      integer(kind=2),parameter:: mty_tot_fed_incm_tax_pd=162
      integer(kind=2),parameter:: mty_tot_st_incm_tax_pd=163
      integer(kind=2),parameter:: mt_st_btl_incm_tax_pd=164
      integer(kind=2),parameter:: mt_fed_btl_incm_tax_pd=165
      integer(kind=2),parameter:: mt_fed_df_tax_dr_nols_amt=166
      integer(kind=2),parameter:: mty_st_def_tax_dr_nols=167
      integer(kind=2),parameter:: Monthly_LTD_Booked_Interest=168
      integer(kind=2),parameter:: Monthly_Booked_PS_Dividends=169
      integer(kind=2),parameter:: Monthly_Issue_Expense_Amorts=170
      integer(kind=2),parameter:: Monthly_PS_Amort_Dividends=180
      integer(kind=2),parameter:: mt_dt_file_intrst_amrt=186
      integer(kind=2),parameter:: mt_dbt_fl_chg_intrst_exp_amrt=263
      integer(kind=2),parameter:: mt_chg_dbt_fl_issu_exp_amrt=264
      integer(kind=2),parameter:: mty_dbt_fl_intrst_amrt=187
      integer(kind=2),parameter:: mty_dbt_fl_intrst_amrt_cr=278
      integer(kind=2),parameter:: Monthly_LTD_Premium_Amort=229
      integer(kind=2),parameter:: Monthly_PS_Premium_Amort=230
      integer(kind=2),parameter:: PhysicalRevenueVariable=188
      integer(kind=2),parameter:: PhysicalRevenueFixed=189
      integer(kind=2),parameter:: PhysicalExpenseVariable=190
      integer(kind=2),parameter:: PhysicalExpenseFixed=191
      integer(kind=2),parameter:: FinancialRevenueVariable=192
      integer(kind=2),parameter:: FinancialRevenueFixed=193
      integer(kind=2),parameter:: FinancialExpenseVariable=194
      integer(kind=2),parameter:: FinancialExpenseFixed=195
      integer(kind=2),parameter:: TotalDerivativeRevenues=196
      integer(kind=2),parameter:: TotalDerivativeExpenses=197
      integer(kind=2),parameter:: NetDerivativeMargin=198
      integer(kind=2),parameter:: PhysicalFuelDerivRevVar=212
      integer(kind=2),parameter:: PhysicalFuelDerivRevFix=213
      integer(kind=2),parameter:: PhysicalFuelDerivExpVar=214
      integer(kind=2),parameter:: PhysicalFuelDerivExpFix=215
      integer(kind=2),parameter:: FinancialFuelDerivRevVar=216
      integer(kind=2),parameter:: FinancialFuelDerivRevFix=217
      integer(kind=2),parameter:: FinancialFuelDerivExpVar=218
      integer(kind=2),parameter:: FinancialFuelDerivExpFix=219
      integer(kind=2),parameter:: TotalFuelDerivativeRevenues=220
      integer(kind=2),parameter:: TotalFuelDerivativeExpenses=221
      integer(kind=2),parameter:: NetFuelDerivativeMargin=222
      integer(kind=2),parameter:: tot_pwr_fl_deriv_mgn=223
      integer(kind=2),parameter:: ICAP_Revs_mth=199 ! ICAP REVENUES
      integer(kind=2),parameter:: Monthly_EBITAD=201
      integer(kind=2),parameter:: MonthlyGasWholesaleRevenues=210
      integer(kind=2),parameter:: Monthly_EBIT=202
      integer(kind=2),parameter:: IPL_Monthy_OandM_Expenses=252
      integer(kind=2),parameter:: IPL_Total_Operating_Expenses=253
      integer(kind=2),parameter:: IPL_Operating_Income=254
      integer(kind=2),parameter:: mty_wvpa_acrd_mbr_rvnues=248
      integer(kind=2),parameter:: WVPA_NonMember_Power_Sales=257   
      integer(kind=2),parameter:: wvpa_prop_txs_n_pwr_cst=272
      integer(kind=2),parameter:: WVPA_pwr_cost_4_non_mbrs=255
      integer(kind=2),parameter:: wvpa_pwr_cost_4_mbrs=256
      integer(kind=2),parameter:: wvpa_non_mbr_svcs_cost=261
      integer(kind=2),parameter:: WVPA_Member_Cost_of_Services=262 
      integer(kind=2),parameter:: WVPA_Member_Rates_withSDI=265
      integer(kind=2),parameter:: WVPA_Member_Rates_withoutSDI=266
      integer(kind=2),parameter:: WVPA_Member_EnergySales=267
      integer(kind=2),parameter:: WVPA_SDI_EnergySales=268
      integer(kind=2),parameter:: WVPA_MemberMargin=269
      integer(kind=2),parameter:: WVPA_Non_MemberMargin=270
      integer(kind=2),parameter:: IMPA_Depreciation_Add_Back=279

        ! start of FE stuff activated
        ! revenues 280-340
      integer(kind=2),parameter:: MonthlyOthRegRevenues=280
      integer(kind=2),parameter:: MonthlyOthATLRevenues=281
      integer(kind=2),parameter:: MonthlyPCABTLRevenues=282
      integer(kind=2),parameter:: MonthlyMarkToMarket=321
      integer(kind=2),parameter:: MonthlyPayrollTaxes=322
      integer(kind=2),parameter:: MonthlyExecBenefits=406
      integer(kind=2),parameter:: MonthlyIncentiveComp=407
      integer(kind=2),parameter:: MonthlyEarningNonCompany=325
      integer(kind=2),parameter:: BTLMonthlyDeferralTaxDr=326
      integer(kind=2),parameter:: BTLMonthlyDeferralTaxCr=327
        ! expenses 350-380
      integer(kind=2),parameter:: MonthlyOtherATLExpenses=350
      integer(kind=2),parameter:: MonthlyAssessedOPEB=307

!
! FE Section
!
     
!
! Monthly Reveneue
!
      integer(kind=2),parameter:: MonthlyCatawbaRevenues=6
      integer(kind=2),parameter:: MonthlyCatawbaExpenses=57

!***********************************************************************
!
! CASH STATEMENT VARIABLES
!
!***********************************************************************
!
! CASH REVENUES
!
      integer(kind=2),parameter:: Cash_Base_Rates=1               
      integer(kind=2),parameter:: Cash_Adjustment_Clause=2       
      integer(kind=2),parameter:: CashSecondarySales=3
      integer(kind=2),parameter:: Cash_Other_Revenue=4          
      integer(kind=2),parameter:: CashBTLRevenues=5
      integer(kind=2),parameter:: cash_catawba_revenues=6
      integer(kind=2),parameter:: Cash_Gas_Revenues=7      
      integer(kind=2),parameter:: CashUnbilledRevenues=8
      integer(kind=2),parameter:: CashDeferredRevenues=9
      integer(kind=2),parameter:: Cash_Relationship_Revenues=10   
      integer(kind=2),parameter:: Cash_Residential=11
      integer(kind=2),parameter:: Cash_Commercial=12
      integer(kind=2),parameter:: Cash_Industrial=13
      integer(kind=2),parameter:: Cash_Lighting=14
      integer(kind=2),parameter:: CashBulkPower=15
      integer(kind=2),parameter:: CashNetofTaxBTLRevenues=16
      integer(kind=2),parameter:: CashCapacitySales=17
      integer(kind=2),parameter:: Cash_Government=18             
      integer(kind=2),parameter:: CashPGAAdjustment=19
      integer(kind=2),parameter:: TotalCashDerivativeRevenues=20
      integer(kind=2),parameter:: tot_cash_fuel_deriv_revenues=22
      integer(kind=2),parameter:: CashICAPRevenues=21
      integer(kind=2),parameter:: Cash_Prior_Years_Method_Adj=26
      integer(kind=2),parameter:: Cash_Prior_level_Method_Adj=27
      integer(kind=2),parameter:: Cash_Operating_Method_Adj=28   
      integer(kind=2),parameter:: CashTotalBaseRevenues=29
      integer(kind=2),parameter:: Cash_TOTAL_OPERATING_RECEIPTS=30
      integer(kind=2),parameter:: CashGasWholesaleRevenues=177
      integer(kind=2),parameter:: Cash_Utility_Sales=178
      integer(kind=2),parameter:: Cash_Competitive_Sales=214
      integer(kind=2),parameter:: CashPhysicalRevenueVariable=161
      integer(kind=2),parameter:: CashPhysicalRevenueFixed=162
      integer(kind=2),parameter:: cash_financl_rev_variable=163
      integer(kind=2),parameter:: CashFinancialRevenueFixed=164
      integer(kind=2),parameter:: cash_phys_fuel_deriv_rev_var=178
      integer(kind=2),parameter:: CashPhysicalFuelDerivRevFix=179
      integer(kind=2),parameter:: cash_fin_fuel_deriv_rev_var=180
      integer(kind=2),parameter:: CashFinancialFuelDerivRevFix=181
      integer(kind=2),parameter:: csh_income_rsv_mgn_cap_sales=190
      integer(kind=2),parameter:: csh_expns_rsv_mgn_cap_purch=191
       !
       ! CASH EXPENSES
       !

      integer(kind=2),parameter:: Cash_TOTAL_EXPENSE_PAYMENTS=31
      integer(kind=2),parameter:: Cash_BTL_Lease_Cash=40        
      integer(kind=2),parameter:: Cash_ATL_Lease_Cash=172
      integer(kind=2),parameter:: Cash_Fossil_Fuel=41          
      integer(kind=2),parameter:: Cash_Purchased_Power=42     
      integer(kind=2),parameter:: Cash_Variable_OandM=43     
      integer(kind=2),parameter:: Cash_Fixed_OandM=44       
      integer(kind=2),parameter:: Cash_Other_OandM=45      
      integer(kind=2),parameter:: CashPurchasedGas=46
      integer(kind=2),parameter:: Cash_Other=47           
      integer(kind=2),parameter:: Cash_Leased_Nuclear_Fuel=49         
      integer(kind=2),parameter:: CashDSMExpense=50
      integer(kind=2),parameter:: CashDSMRebate=51
      integer(kind=2),parameter:: CashLeaseExpense=52
      integer(kind=2),parameter:: CashServiceTransactions=53
      integer(kind=2),parameter:: CashEmissionCredits=54
      integer(kind=2),parameter:: CashDOEDecommissioning=55
      integer(kind=2),parameter:: CashDOEDisposal=56
      integer(kind=2),parameter:: cash_catawba_expenses=57
      integer(kind=2),parameter:: CashBTLExpenses=58
      integer(kind=2),parameter:: CashTransmissionOperation=59
      integer(kind=2),parameter:: CashTransmissionMaintenance=60
      integer(kind=2),parameter:: CashDistributionOperation=61
      integer(kind=2),parameter:: CashDistributionMaintenance=62
      integer(kind=2),parameter:: CashCustomerAccounts=63
      integer(kind=2),parameter:: CashCustomerServices=64
      integer(kind=2),parameter:: CashSalesExpense=65
      integer(kind=2),parameter:: CashAGOperations=66
      integer(kind=2),parameter:: Cash_AG_Maintenance=67              
      integer(kind=2),parameter:: Cash_Post_Retirement_Payments=68
      integer(kind=2),parameter:: csh_2_post_retiremt_paymentss=207
      integer(kind=2),parameter:: Cash_Retiree_Medical_Payments=175
      integer(kind=2),parameter:: CashLeaseInterestPayments=69
      integer(kind=2),parameter:: csh_btl_lease_intrst_pymts=170
      integer(kind=2),parameter:: csh_unfundd_retrmt_pymts=151
      integer(kind=2),parameter:: Cash_Storm_Payments=152
      integer(kind=2),parameter:: Cash_Vacation_Payments=153
      integer(kind=2),parameter:: cash_investmt_divnd_earngs=154
      integer(kind=2),parameter:: cash_tot_invest_intrst_earnings=197
      integer(kind=2),parameter:: cash_invest_interest_ernings=155
      integer(kind=2),parameter:: Cash_From_Trust_Fund_Earnings=208
      integer(kind=2),parameter:: Cash_Interest_on_Notes_Payable=156
      integer(kind=2),parameter:: Cash_Paid_on_Accounts_Payable=157   
      integer(kind=2),parameter:: cash_rcd_accts_rcvbl=158
      integer(kind=2),parameter:: Price_of_Issued_Shares=159
      integer(kind=2),parameter:: TotalCashDerivativeExpenses=165
      integer(kind=2),parameter:: CashPhysicalExpenseVariable=166
      integer(kind=2),parameter:: CashPhysicalExpenseFixed=167
      integer(kind=2),parameter:: CashFinancialExpenseVariable=168
      integer(kind=2),parameter:: CashFinancialExpenseFixed=169
      integer(kind=2),parameter:: CashPhysicalFuelDerivExpVar=182
      integer(kind=2),parameter:: CashPhysicalFuelDerivExpFix=183
      integer(kind=2),parameter:: CashFinancialFuelDerivExpVar=184
      integer(kind=2),parameter:: CashFinancialFuelDerivExpFix=185
      integer(kind=2),parameter:: tot_cash_fuel_deriv_expenses=186
      integer(kind=2),parameter:: WVPA_Cash_Member_Cost_of_Power=198 
      integer(kind=2),parameter:: wvpa_csh_prop_txs_in_pwr_cost=211
      integer(kind=2),parameter:: wvpa_cash_nonmbr_pwr_cost=199
      integer(kind=2),parameter:: wvpa_cash_fm_nonmbr_pwr_sales=200
      integer(kind=2),parameter:: wvpa_cash_mbr_svc_cost=205
      integer(kind=2),parameter:: wvpa_cash_nonmbr_svc_cost=206
      integer(kind=2),parameter:: wvpa_cash_2_accrd_mbr_revnue=209
      integer(kind=2),parameter:: cash_2_cash=210
      integer(kind=2),parameter:: cash_atl_other_rev=218
      integer(kind=2),parameter:: CashExpFileDividendRev=220
      integer(kind=2),parameter:: CashExpFileInterestRev=221
      integer(kind=2),parameter:: CashATLExpenses=222
      integer(kind=2),parameter:: CashEarningsNonCorporate=223 ! Ann 95
      integer(kind=2),parameter:: CashExecBenefits=224 !Ann 96
      integer(kind=2),parameter:: CashIncentiveCompReserve=225 !Ann 97
      integer(kind=2),parameter:: CashAssessedOPEB=226 ! Ann 98

       !
       ! OTHER CASH ITEMS
       !
      integer(kind=2),parameter:: cash_ps_dividends=80
      integer(kind=2),parameter:: cash_ltd_interest=81
      integer(kind=2),parameter:: cash_std_interest=82
      integer(kind=2),parameter:: cash_common_dividends=83
      integer(kind=2),parameter:: cash_investment_affiliates=70
      integer(kind=2),parameter:: cash_loads_2_affiliates=71
      integer(kind=2),parameter:: cash_ps_issued=72
      integer(kind=2),parameter:: cash_ltd_issued=74
      integer(kind=2),parameter:: cash_ps_retirements=73
      integer(kind=2),parameter:: cash_ltd_retirements=75
      integer(kind=2),parameter:: cash_ltd_ps_issue_expense=76
      integer(kind=2),parameter:: cash_std_issued=77
      integer(kind=2),parameter:: cash_common_stock_buyback=78
      integer(kind=2),parameter:: cash_common_stock_issued=79
      integer(kind=2),parameter:: cash_lease_receipts=84
      integer(kind=2),parameter:: cash_change_ltinvestments=149
      integer(kind=2),parameter:: cash_change_debt_investments=176
      integer(kind=2),parameter:: cash_common_issue_expense=160
       !
       ! TAX ITEMS
       !
      integer(kind=2),parameter:: cash_operating_revenue_tax=90
      integer(kind=2),parameter:: cash_exp_file_oprn_rev_tax=216
      integer(kind=2),parameter:: cash_other_taxes=91
      integer(kind=2),parameter:: cash_exp_file_othr_taxes=215
      integer(kind=2),parameter:: cash_property_taxes=92
      integer(kind=2),parameter:: cash_exp_file_prop_taxes=212
      integer(kind=2),parameter:: cash_mdl_calcd_prop_taxes=213
      integer(kind=2),parameter:: cash_st_income_taxes_pd=93
      integer(kind=2),parameter:: cash_state_on_capital=94
      integer(kind=2),parameter:: cash_fed_income_taxs_pd=95
      integer(kind=2),parameter:: cash_fed_tax_on_capital=96
      integer(kind=2),parameter:: CashPayrollTaxes=97
       !
       ! CUSTOMER DEPOSITS, CIAC, NOTES
       !
      integer(kind=2),parameter:: cash_customer_deposits=119
      integer(kind=2),parameter:: cash_ciac=118
      integer(kind=2),parameter:: cash_from_assets_sale=116
      integer(kind=2),parameter:: cash_fm_repaid_issued_notes=143
      integer(kind=2),parameter:: cash_4_new_notes_issued=173 ! need for cash
      integer(kind=2),parameter:: csh_4_redeeming_notes_owed=144
      integer(kind=2),parameter:: cash_fm_notes_issd_by_othrs=174 ! source of cash
       !
       ! INTEREST AND SUBSIDIARY ITEMS
       !

      integer(kind=2),parameter:: csh_st_invstmt_incm=101
      integer(kind=2),parameter:: cash_subsidiary_dividends=102
       !
       ! CAPITIAL ITEMS
       !
      integer(kind=2),parameter:: noncash_pension_in_capx=195
      integer(kind=2),parameter:: cash_net_plant_capx=196
      integer(kind=2),parameter:: cash_plant_construction=120
      integer(kind=2),parameter:: cash_ai_investment=125
      integer(kind=2),parameter:: cash_nclr_fl_fabrication=121
      integer(kind=2),parameter:: class_net_invest_slvg_othr=122
      integer(kind=2),parameter:: cash_net_investments=138
      integer(kind=2),parameter:: cash_net_salvage=139
      integer(kind=2),parameter:: cash_net_other=140
      integer(kind=2),parameter:: cash_equity_to_subsidiaries=123
      integer(kind=2),parameter:: cash_capital_leases=124
      integer(kind=2),parameter:: cash_acquisition_cost=192
      integer(kind=2),parameter:: cash_nuc_decommissioning_fund=126
      integer(kind=2),parameter:: cash_working_capital=127
      integer(kind=2),parameter:: cash_fuel_inventory_chg=142
      integer(kind=2),parameter:: class_matrls_inventry_addition=187
      integer(kind=2),parameter:: cash_gas_strg_inventry_addtn=188
      integer(kind=2),parameter:: cash_additions_to_inventories=189
      integer(kind=2),parameter:: cash_deferred_expenses=128
      integer(kind=2),parameter:: cash_aro_payments=202
      integer(kind=2),parameter:: cash_aro_trust_payments=203
      integer(kind=2),parameter:: cash_tot_aro_payments=204
       !
       ! TAX ADJUSTMENTS BETWEEN AFILIATES
       !
      integer(kind=2),parameter:: cash_parent_taxes_pd=132
      integer(kind=2),parameter:: cash_subsidiary_tax_pmts=133
      integer(kind=2),parameter:: cash_consolidate_taxes_pd=134
      integer(kind=2),parameter:: cash_subsdry_fed_taxes_pd=193
      integer(kind=2),parameter:: cash_subsdry_st_taxes_pd=194
      integer(kind=2),parameter:: cash_subsdry_st_taxes_pd_by_pt=201
      integer(kind=2),parameter:: cash_opening_balance=135
      integer(kind=2),parameter:: cash_closing_balance=136
      integer(kind=2),parameter:: cash_period_change=137
      integer(kind=2),parameter:: common_shares_issued=150
       !
       ! CASH STATEMENT TOTALS
       !
      integer(kind=2),parameter:: net_cash_receipts_or_pmts=110
      integer(kind=2),parameter:: cash_capital_svc_pmts=111
      integer(kind=2),parameter:: cash_total_tax_payments=112
      integer(kind=2),parameter:: cash_total_construction=113
      integer(kind=2),parameter:: total_capital_requirements=114
      integer(kind=2),parameter:: funds_change_b4_financing=115
      integer(kind=2),parameter:: external_financing_pfmd=117
      integer(kind=2),parameter:: total_tax_and_other=129
      integer(kind=2),parameter:: total_fm_other_sources=130
      integer(kind=2),parameter:: funds_fm_operations=131

       !***********************************************************************
       !
       ! BALANCE SHEET ITEMS
       !
       !***********************************************************************
       !
      integer(kind=2),parameter:: monthly_gross_plant_in_svc=1
      integer(kind=2),parameter:: monthly_afudc_capitalized=69
      integer(kind=2),parameter:: monthly_plant_capitalized=70
      integer(kind=2),parameter:: monthly_retirements=18
      integer(kind=2),parameter:: mtly_cwip=2
      integer(kind=2),parameter:: monthly_total_utility_plant=28
      integer(kind=2),parameter:: monthly_accum_depreciation=3
      integer(kind=2),parameter:: monthly_net_nuclear_fuel=4
      integer(kind=2),parameter:: monthly_net_utility_plant=29
      integer(kind=2),parameter:: monthly_subsidiary_investment=7
      integer(kind=2),parameter:: monthly_other_investments=8
      integer(kind=2),parameter:: monthly_aro_net_asset_value=80
      integer(kind=2),parameter:: monthly_notes_receivable=6
      integer(kind=2),parameter:: monthly_capitalized_leases=9
      integer(kind=2),parameter:: monthly_nclr_dcms_trust_bal=10
      integer(kind=2),parameter:: monthly_post_retire_med_fnd=11
      integer(kind=2),parameter:: monthly_net_dfrd_debits=12
      integer(kind=2),parameter:: monthly_net_goodwill=72
      integer(kind=2),parameter:: monthly_net_regulatory_assets=73
      integer(kind=2),parameter:: mty_net_fasb_109=74
      integer(kind=2),parameter:: mty_net_fasb_133=75
      integer(kind=2),parameter:: mty_net_fasb_87=83
      integer(kind=2),parameter:: monthly_unamort_interest_bal=76
      integer(kind=2),parameter:: mthly_unamortized_issue_exp_bal=77
      integer(kind=2),parameter:: mty_net_oth_debits=71
      integer(kind=2),parameter:: monthly_deferred_revenues=13
      integer(kind=2),parameter:: mthly_deferred_income_taxes_dr=14
      integer(kind=2),parameter:: monthly_lti=15
      integer(kind=2),parameter:: monthly_cash_lti=23
      integer(kind=2),parameter:: monthly_debt_lti=24
      integer(kind=2),parameter:: monthly_short_lti=16
      integer(kind=2),parameter:: monthly_accounts_receivable_bal=5
      integer(kind=2),parameter:: monthly_current_assets=17
      integer(kind=2),parameter:: monthly_deferred_fuel_bal=19
      integer(kind=2),parameter:: mthly_deferred_purchse_powr_bal=22
      integer(kind=2),parameter:: monthly_fuel_inventory_bal=20
      integer(kind=2),parameter:: monthly_gas_in_storage_bal=78
      integer(kind=2),parameter:: monthly_matrls_supplies_bal=79
      integer(kind=2),parameter:: monthly_acc_dep_adjs=21
      integer(kind=2),parameter:: monthly_total_assets=30
      integer(kind=2),parameter:: monthly_common_stock=31
      integer(kind=2),parameter:: mthly_coi_retained_earnings_bal=61
      integer(kind=2),parameter:: monthly_retained_earnings_bal=32
      integer(kind=2),parameter:: Monthly_TOTAL_COMMON_EQUITY=58
      integer(kind=2),parameter:: monthly_preferred_stock=33
      integer(kind=2),parameter:: monthly_long_term_debt=34
      integer(kind=2),parameter:: monthly_ltd_current_amount=86
      integer(kind=2),parameter:: monthly_total_capital=59
      integer(kind=2),parameter:: mty_ltd_bal_wo_ct_amt=84
      integer(kind=2),parameter:: mty_ttl_cptl_wo_ct_ltd=85
      integer(kind=2),parameter:: monthly_other_long_term_liabs=36
      integer(kind=2),parameter:: monthly_aro_liability_value=81
      integer(kind=2),parameter:: monthly_oci_value=82
      integer(kind=2),parameter:: mthly_nucl_decommiss_liability=37
      integer(kind=2),parameter:: monthly_short_term_debt_bal=38
      integer(kind=2),parameter:: monthly_customer_deposits=39
      integer(kind=2),parameter:: monthly_notes_payable=49
      integer(kind=2),parameter:: mtly_post_retre_med_payable=43
      integer(kind=2),parameter:: monthly_accrued_pension=45
      integer(kind=2),parameter:: mty_accrued_vaca_pay=46
      integer(kind=2),parameter:: monthly_accounts_payable_bal=35
      integer(kind=2),parameter:: Monthly_Strom_Reserve=48
      integer(kind=2),parameter:: MonthlyExecBenefitsReserve=62
      integer(kind=2),parameter:: MonthlyIncentiveCompReserve=63
       ! deferred credit items
      integer(kind=2),parameter:: monthly_deferred_gains=47
      integer(kind=2),parameter:: monthly_cica=40
      integer(kind=2),parameter:: mty_dfrd_income_taxes=41
      integer(kind=2),parameter:: mty_dfrd_itc_credt=42
      integer(kind=2),parameter:: monthly_gain_on_reaqd_debt_bal=87
      integer(kind=2),parameter:: mty_regty_liablties_bal=88
      integer(kind=2),parameter:: monthly_other_deferred_credits=89
      integer(kind=2),parameter:: mty_tot_dfrd_creds=90
      integer(kind=2),parameter:: monthly_liabilities_nec=44
      integer(kind=2),parameter:: monthly_total_liabilities=60
       !
       ! NAMES FOR FIRST YEAR PAYABLES AND RECEIVABLES
       !
      integer(kind=2),parameter:: PAYMENT_VARS=20
      integer(kind=2),parameter:: payment_std_interest=1
      integer(kind=2),parameter:: payment_operating_revenue_tax=2
      integer(kind=2),parameter:: payment_property_taxes=3
      integer(kind=2),parameter:: payment_state_income_taxes_paid=4
      integer(kind=2),parameter:: payment_federal_inc_taxes_paid=5
      integer(kind=2),parameter:: payment_state_tax_on_capital=6
      integer(kind=2),parameter:: payment_federal_tax_on_capital=7
      integer(kind=2),parameter:: payment_other_taxes=8
      integer(kind=2),parameter:: payment_catawba_revenues=9
      integer(kind=2),parameter:: payment_catawba_expenses=10
      integer(kind=2),parameter:: payment_expenses_payable=11
      integer(kind=2),parameter:: payment_revenues_receivable=12
      integer(kind=2),parameter:: wvpa_nonmbr_revnues_rcbl=13
      integer(kind=2),parameter:: wvpa_nonmember_cost_of_power=14
      integer(kind=2),parameter:: wvpa_member_cost_of_power=15
      integer(kind=2),parameter:: wvpa_fuel_expense=16
      integer(kind=2),parameter:: wvpa_nonfuel_expense=17
      integer(kind=2),parameter:: wvpa_fixed_expense=18
       !
       ! FUTURE ASSET ITEMS
       !
      integer(kind=2),parameter:: MAX_FA_VARIALBES=11
      integer(kind=2),parameter:: MAX_NF_VARIABLES=11
      integer(kind=2),parameter:: book_construction_expenditures=1
      integer(kind=2),parameter:: cash_construction_expenditures=2
      integer(kind=2),parameter:: ai_book_investment=3
      integer(kind=2),parameter:: ai_cash_investment=4
      integer(kind=2),parameter:: afudc_on_cash=5
      integer(kind=2),parameter:: afudc_on_plant=6
      integer(kind=2),parameter:: afudc_borrowed_on_cash=7
      integer(kind=2),parameter:: plant_2_service=8
      integer(kind=2),parameter:: fuel_2_service=8
      integer(kind=2),parameter:: book_dep_on_plant_afudc=9
      integer(kind=2),parameter:: fa_cwip_balance=10
      integer(kind=2),parameter:: nfip_balance=10
      integer(kind=2),parameter:: fa_gpv_balance=11
       !
       ! GENERAL TAX ITEMS
       !
      integer(kind=2),parameter:: monthly_temporary_atl_tax_dif=1
      integer(kind=2),parameter:: monthly_temporary_btl_tax_dif=2
      integer(kind=2),parameter:: monthly_permanent_atl_tax_dif=3
      integer(kind=2),parameter:: monthly_permanent_btl_tax_dif=4
      integer(kind=2),parameter:: mthly_atl_federal_income_taxes=5
      integer(kind=2),parameter:: monthly_atl_state_income_taxes=6
      integer(kind=2),parameter:: mthly_btl_federal_income_taxes=7
      integer(kind=2),parameter:: monthly_btl_state_income_taxes=8
      integer(kind=2),parameter:: monthly_foreign_tax_credit=9
      integer(kind=2),parameter:: monthly_possessions_tax_credit=10
      integer(kind=2),parameter:: monthly_section_29_tax_credits=11
      integer(kind=2),parameter:: monthly_investment_tax_credit=12
      integer(kind=2),parameter:: monthly_other_credits=13
      integer(kind=2),parameter:: monthly_atl_taxable_income=14
      integer(kind=2),parameter:: monthly_btl_taxable_income=15
      integer(kind=2),parameter:: monthly_federal_taxable_income=16
      integer(kind=2),parameter:: monthly_state_taxable_income=17
      integer(kind=2),parameter:: monthly_ltd_tax_deduction=19
      integer(kind=2),parameter:: monthly_atl_debit_tax_expense=20
      integer(kind=2),parameter:: monthly_btl_debit_tax_expense=21
      integer(kind=2),parameter:: mty_ltd_ps_tax_issue_expense=22
       !
       ! CP&L TAX ITEMS
       !
      integer(kind=2),parameter:: income_b4_income_taxes=1
      integer(kind=2),parameter:: tax_afudc_equity=2
      integer(kind=2),parameter:: tax_afudc_borrowed=3
      integer(kind=2),parameter:: tax_interest_expenses=4
      integer(kind=2),parameter:: tax_interest_adjustment=5
      integer(kind=2),parameter:: tax_interest_2_assoc_comp=6
      integer(kind=2),parameter:: tax_ttl_income_b4_taxes=7
      integer(kind=2),parameter:: tax_book_depreciation=8
      integer(kind=2),parameter:: tax_amortization=9
      integer(kind=2),parameter:: tax_interest_amortization=10
      integer(kind=2),parameter:: tax_total_afudc=12
      integer(kind=2),parameter:: tax_book_nclr_fuel=13
      integer(kind=2),parameter:: tax_taken_4_book_totl=14
      integer(kind=2),parameter:: tax_tax_depreciation=16
      integer(kind=2),parameter:: tax_construction_expenses=17
      integer(kind=2),parameter:: tax_capitalized_interest=18
      integer(kind=2),parameter:: tax_net_salvage=19
      integer(kind=2),parameter:: tax_taken_for_tax_total=20
      integer(kind=2),parameter:: perm_tax_differences=21
      integer(kind=2),parameter:: temp_tax_differences=22
      integer(kind=2),parameter:: taxable_income_b4_state_taxes=23
      integer(kind=2),parameter:: tax_state_income_tax=24
      integer(kind=2),parameter:: tax_capital_gains_income=25
      integer(kind=2),parameter:: tax_nols_used=26
      integer(kind=2),parameter:: tax_m1_current_deductions=27
      integer(kind=2),parameter:: current_fedl_taxable_income=28
      integer(kind=2),parameter:: tax_capl_gains_tax_rate=29
      integer(kind=2),parameter:: tax_fed_tax_rate=30
      integer(kind=2),parameter:: tax_fed_tax_b4_creds=31
      integer(kind=2),parameter:: tax_foreign_tax_cred=32
      integer(kind=2),parameter:: tax_possessions_tax_cred=33
      integer(kind=2),parameter:: tax_sec_29_creds_used=34
      integer(kind=2),parameter:: tax_invesment_tax_cred=35
      integer(kind=2),parameter:: tax_sect_43_creds_used=36
      integer(kind=2),parameter:: tax_amt_creds_used=37
      integer(kind=2),parameter:: tax_totl_creds_used=38
      integer(kind=2),parameter:: tax_adjustments_2_taxes=39
      integer(kind=2),parameter:: tax_fed_tax_aftr_creds=40
      integer(kind=2),parameter:: tax_fed_amt_tax=41
      integer(kind=2),parameter:: tax_currnt_fed_income_tax=42
       !
       ! CP&L JURISDICTIONAL VALUES
       !
      integer(kind=2),parameter:: CPL_JURISDICTIONS=10
      integer(kind=2),parameter:: nc_retail=1
      integer(kind=2),parameter:: sc_retail=2
      integer(kind=2),parameter:: Wholesale=3
      integer(kind=2),parameter:: cpl_other=4
      integer(kind=2),parameter:: NCEMPA=5
      integer(kind=2),parameter:: NCEMC=6
      integer(kind=2),parameter:: sc_fuel_clause=7
      integer(kind=2),parameter:: nc_fuel_clause=8
      integer(kind=2),parameter:: CPL_FUEL_VARS=20
      integer(kind=2),parameter:: cpl_mwh=1
      integer(kind=2),parameter:: cpl_dmd=2
      integer(kind=2),parameter:: cpl_customers=3
      integer(kind=2),parameter:: cpl_fuel_cost=4
      integer(kind=2),parameter:: cpl_base_fuel_factor=5
      integer(kind=2),parameter:: cpl_emf_factor=6
      integer(kind=2),parameter:: cpl_avg_fuel_cost=7
      integer(kind=2),parameter:: cpl_jurisdictional_fuel_cost=8
      integer(kind=2),parameter:: cpl_jurisdictional_rev=9
      integer(kind=2),parameter:: cpl_startbal=10
      integer(kind=2),parameter:: cpl_deferral_recovery=11
      integer(kind=2),parameter:: cpl_ending_bal=12
      integer(kind=2),parameter:: cpl_cumulative_fuel_cost=13
      integer(kind=2),parameter:: cpl_cum_enrg_sales=14
      integer(kind=2),parameter:: cpl_cum_juris_sales=15
      integer(kind=2),parameter:: cpl_jurisdiction_sales=16
      integer(kind=2),parameter:: cpl_emf_revenues=17
      integer(kind=2),parameter:: cpl_emf_startbal=18
      integer(kind=2),parameter:: cpl_emf_endbal=19

end module mthnmcom
!
!
