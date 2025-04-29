module ac_equiv
!     last change: msg 1/14/2013 5:11:42 pm
!     integer (kind=2), parameter, save :: acq_variable_number = 950 
!next unused 901 range from 0 to acq_variable_number-1
!     variable number 476-485 and 495 are used in the routine 
! ebitda_report
!     variable number 488-494 are used in the routine ebitda_report 
!for hoosier variables

!     variable numbers 502-510, 561 and 562 are for ameren revenue and
! expense transfers.
!     variable numbers 511-560, 581-625, 644-650 are used in the first
! energy p&l report
!     variable numbers 565-577 are used 578-580 are reserved for the
! sec ratios report
!     variable number 757 is used for the coverage ratio calculated 
!ratio & for wvpa tier ratio
!     variable number 758 is used for the property taxes in power 
!cost for wvpa 8/1/04
!     variable numbers 773, 775, 776 are used by gre in the 
!routine gre_dsc_report
!     variable numbers 774 is rate_base_deferred_credits
!     variable number(s) 779 is used in the ebitda report for free 
!cash flow
!     variable numbers 780-785 are used in the rating_agency_ratios 
! subroutine
!     variable numbers 786-799  are avail for new hardwired expenses
!     variable numbers 800-900  are used for fe rev&exp name expansion
!     variable numbers 901-904 are used for deferred tax items.
!     variable numbers 905-908 fe execbenefits and incentive comp non
! & cash

implicit none
! output variables from msgmmsb6

! variables declared in .h but never brought into this file
real (kind=4) :: acq_rate_base_def_itc
real (kind=4) :: ACQ_ELIMINATION_CASH_ADJUSTMENT
real (kind=4) :: ACQ_CAPITIALIZED_INTEREST
real (kind=4) :: acq_nf_non_cash_expense
real (kind=4) :: ACQ_OWNED_NF_BURN
real (kind=4) :: ACQ_CLASS_NUCLEAR_FUEL_EXPENSE
real (kind=4) :: ACQ_DSM_EXPENSE
real (kind=4) :: ACQ_FIXED_EXPENSE
real (kind=4) :: acq_pension_expense_non_cash
real (kind=4) :: acq_storm_expense_non_cash
real (kind=4) :: ACQ_SERVICE_TRANSACTION_EXPENSE
real (kind=4) :: ACQ_TOTAL_INCOME_TAXES
real (kind=4) :: ACQ_EMISSION_CREDIT_EXPENSE
real (kind=4) :: acq_dsm_rebate
real (kind=4) :: acq_nuc_fuel_owned_burn
real (kind=4) :: acq_nuc_fuel_leased_burn
real (kind=4) :: acq_rate_base_nuc_fuel
real (kind=4) :: acq_rate_base_cwip,rate_base_def_itc
real (kind=4) :: acq_rate_base_nuc_decom
real (kind=4) :: nf_non_cash_expense
real (kind=4) :: acq_nuc_fuel_total_burn
real (kind=4) :: acq_nf_deferred_tax_basis
real (kind=4) :: acq_doe_nuc_fuel_fee
real (kind=4) :: acq_nuc_decommissioning_cost
real (kind=4) :: acq_nf_cash_expense
real (kind=4) :: acq_atl_state_taxes
real (kind=4) :: acq_nf_atl_amortization
real (kind=4) :: acq_ltd_interest_and_amortization
real (kind=4) :: acq_operating_revenue_tax
real (kind=4) :: acq_other_and_environmental_taxes
real (kind=4) :: acq_total_expense_and_taxes
real (kind=4) :: acq_atl_federal_taxes
real (kind=4) :: acq_tax_credits
real (kind=4) :: acq_std_interest
real (kind=4) :: acq_operating_income
real (kind=4) :: acq_afudc_equity
real (kind=4) :: acq_income_b4_interest
real (kind=4) :: acq_interest_on_long_term_debt
real (kind=4) :: acq_income_after_interest
real (kind=4) :: acq_afudc_borrowed
real (kind=4) :: acq_stock_book_value
real (kind=4) :: acq_net_profit_or_loss
real (kind=4) :: acq_earnings_available_to_common
real (kind=4) :: acq_taxable_income
real (kind=4) :: acq_average_shares
real (kind=4) :: acq_dividend_payout
real (kind=4) :: acq_earnings_per_share
real :: acq_average_equity
real (kind=4) :: acq_return_on_equity
real (kind=4) :: acq_fed_taxable_income_after_nols
real (kind=4) :: acq_federal_taxable_income
real (kind=4) :: acq_state_taxable_income
real (kind=4) :: acq_state_taxable_income_after_nols
real (kind=4) :: acq_rate_base_deferred_debits
real (kind=4) :: acq_return_on_ratebase
real (kind=4) :: acq_tax_deductible_expenses_book
real (kind=4) :: acq_taxable_income_b4_deductions
real (kind=4) :: acq_rate_base_def_taxes
real (kind=4) :: acq_rate_base_plant_in_service
real (kind=4) :: acq_asset_class_rate_base
real (kind=4) :: acq_net_nf_value
real (kind=4) :: acq_cumulative_depreciation
real (kind=4) :: acq_gross_plant_value_plus_cwip
real (kind=4) :: acq_cwip
real (kind=4) :: acq_gross_plant_value
real (kind=4) :: acq_rate_base_capitialized_leases
real (kind=4) :: acq_rate_base_asset_nec
real (kind=4) :: acq_rate_base_ciac
real (kind=4) :: acq_vacation_pay_non_cash
real (kind=4) :: acq_catawba_deferred_debits
real (kind=4) :: acq_catawba_deferred_taxes_cr
real (kind=4) :: acq_catawba_other_net_revenues
real (kind=4) :: acq_catawba_expenses
real (kind=4) :: acq_catawba_revenues
real (kind=4) :: acq_construction_tax_expense
real (kind=4) :: acq_nf_sl_tax_depreciation
real (kind=4) :: acq_sl_tax_depreciation
real (kind=4) :: acq_change_in_cash
real (kind=4) :: acq_external_financing_performed
real (kind=4) :: acq_change_in_funds_b4_financing
real (kind=4) :: acq_preferred_stock_retirements
real (kind=4) :: acq_change_in_working_capitial
real (kind=4) :: acq_leased_nf_burn
real (kind=4) :: acq_construction_and_net_investment
real (kind=4) :: acq_nf_cash
real (kind=4) :: acq_fa_cash
real (kind=4) :: acq_funds_from_operations
real (kind=4) :: acq_customer_deposits
real (kind=4) :: acq_taxes_paid_by_parent
real (kind=4) :: acq_income_taxes_consolidated
real (kind=4) :: acq_catawba_defer_taxes_bal_cr
real (kind=4) :: acq_tax_payments_2_parent_from_sub
real (kind=4) :: acq_dividend_from_subsidiary
real (kind=4) :: acq_income_before_capital_service
real (kind=4) :: acq_short_term_debt_issued
real (kind=4) :: acq_total_liabilities
real (kind=4) :: acq_liabilities_nec
real (kind=4) :: acq_catawba_total_cap_payments
real (kind=4) :: acq_def_tax_cr_bal
real (kind=4) :: acq_ciac_bal
real (kind=4) :: acq_customer_deposits_balance
real (kind=4) :: acq_def_itc_bal
real (kind=4) :: acq_total_capitial_bal
real (kind=4) :: acq_long_term_debt_bal
real (kind=4) :: acq_short_term_debt_bal
real (kind=4) :: acq_common_equity_bal
real (kind=4) :: acq_total_assets
real (kind=4) :: acq_common_stock_bal
real (kind=4) :: acq_catawba_level_cap_payments
real (kind=4) :: acq_catawba_capacity_payments
real (kind=4) :: acq_decommisioning_fund_bal
real (kind=4) :: acq_dd_balance_net
real (kind=4) :: acq_deferred_revenues_cum
real (kind=4) :: acq_deferred_taxes_dr_bal
real (kind=4) :: acq_utility_plant_net
real (kind=4) :: acq_assets_other
real (kind=4) :: acq_subsidiary_investment
real (kind=4) :: acq_leases_capitalized
real (kind=4) :: acq_cash_balance
real (kind=4) :: acq_nf_entering_service
real (kind=4) :: acq_intra_company_nf_burn
     
       real :: acq_long_term_debt_retirements


      ! variables declared in msgmmsb6 and used when this was an 
      ! include file.
      ! they should have been declared in a module like this one.
      real (kind=4) :: acq_adjustment_clause_revenues
      real (kind=4) :: acq_revenues_other
      real (kind=4) :: acq_reference_base_revenues
      real (kind=4) :: acq_revenues_operating
      real (kind=4) :: acq_fuel_expense
      real (kind=4) :: acq_purchase_power_expense
      real (kind=4) :: acq_variable_expense
      real (kind=4) :: acq_retained_earnings_bal
      real (kind=4) :: acq_preferred_stock_bal
      real (kind=4) :: acq_catawba_class_btl_revenues
      real (kind=4) :: acq_asset_nec


      integer (kind=2), parameter :: acq_variable_number = 950
      real (kind=4), save :: acq_variable(0:acq_variable_number-1)

 real (kind=4), save :: acq_subsidary_variables(0:acq_variable_number-1)
      real (kind=4), save :: acq_execbenefitsnoncash
      real (kind=4), save :: acq_incentivecompensationnoncash
      real (kind=4), save :: acq_class_execbenefits_balance 
      real (kind=4), save :: acq_class_incentivecomp_balance
      real (kind=4), save :: acq_execbenefitscash
      real (kind=4), save :: acq_incentivecompensationcash
      real (kind=4), save :: acq_assessedpensionopeb
      real (kind=4), save :: acq_incderivcapacitysales
      real (kind=4), save :: acq_incderivcapacitypurchases
      
      
!       gre dsc variables
      
      real (kind=4), save :: acq_gre_falkirk_interest ! 760
      real (kind=4), save :: acq_gre_total_interest_expense ! 761
      real (kind=4), save :: acq_gre_falkirk_depreciation ! 762
      real (kind=4), save :: acq_gre_total_depreciation ! 763
      real (kind=4), save :: acq_gre_other_principal_adjs ! 764
      real (kind=4), save :: acq_gre_principal_manual_adjs ! 765
      real (kind=4), save :: acq_gre_total_principal_payments ! 766
      real (kind=4), save :: acq_gre_interest_payments_manual_adjs ! 767
      real (kind=4), save :: acq_gre_total_interest_payments ! 768
      real (kind=4), save :: acq_gre_net_margin ! 769
      real (kind=4), save :: acq_gre_total cost of service ! 770
      real (kind=4), save :: acq_gre_other_income ! 771
      real (kind=4), save :: acq_gre_other_revenue ! 772
      real (kind=4), save :: acq_gre_dsc ! 773
      real (kind=4), save :: acq_gre_debt_retirements ! 777
      real (kind=4), save :: acq_gre_lease_payments_in_tier ! 778
      
      real (kind=4), save :: acq_class_fasb87_balance ! 729
      real (kind=4), save :: acq_class_oci_balance ! 730
      real (kind=4), save :: acq_total_equity_with_oci ! 731
      real (kind=4), save :: acq_total_capitial_with_oci ! 732
      real (kind=4), save :: acq_capx_pension_adjustment ! 734
      real (kind=4), save :: acq_current_ltd_retirements ! 740
      real (kind=4), save :: acq_ltd_balance_wo_current_ltd ! 741
      real (kind=4), save :: acq_total_capitial_bal_wo_current_ltd ! 742
      real (kind=4), save :: acq_wvpa_member_rates_with_sdi !744
      real (kind=4), save :: acq_wvpa_member_rates_without_sdi ! 745
      real (kind=4), save :: acq_wvpa_member_energy_sales ! 751
      real (kind=4), save :: acq_wvpa_sdi_energy_sales ! 752
      real (kind=4), save :: acq_wvpa_member_margin ! 753
      real (kind=4), save :: acq_wvpa_non_member_margin ! 754
      real (kind=4), save :: acq_wvpa_debt_interest_coverage ! 756
      real (kind=4), save ::acq_wvpa_property_taxes_in_power_costs ! 758
      real (kind=4), save :: acq_nf_tax_depreciation
      real (kind=4), save :: acq_nf_tax_expense
      real (kind=4), save :: acq_nf_sl_def_tax_dep
      real (kind=4), save :: acq_nf_deferred_taxes_cr ! 733
      real (kind=4), save :: acq_gain_on_reaquired_debt_bal_eoy ! 710
      real (kind=4), save::acq_regulatory_deferred_credits_bal_eoy ! 711
      real (kind=4), save :: acq_other_deferred_credits_bal_eoy ! 712
      real (kind=4), save :: acq_total_deferred_credits_bal ! 713
      real (kind=4), save :: acq_deferred_credit_amortization ! 777
      real (kind=4), save :: acq_capacity_purchases_to_level_rm ! 725
      real (kind=4), save :: acq_capacity_sales_to_level_rm ! 726
      real (kind=4), save :: acq_inc_capacity_sales_to_level_rm ! 727
      real (kind=4),save :: acq_inc_capacity_purchases_to_level_rm ! 728
      real (kind=4), save :: acq_other_payments_to_trust_funds ! 755
      real (kind=4), save :: acq_ecities_transfer_benefit ! 683 wtb
      real (kind=4),save :: acq_ecities_wholesale_prod_cost ! 684 c-prod
      real (kind=4), save :: acq_ecities_var_prod_cost ! 685 c-supp
      real (kind=4), save :: acq_ecities_new_fixed_cost ! 686 c-cap
      real (kind=4), save :: acq_ecities_market_energy_sales ! 687 r
      real (kind=4), save :: acq_ecities_transmission_fees ! 688 c-fees
      real (kind=4), save :: acq_pension_tax_deduction_nonbook ! 689
      real (kind=4), save :: acq_storm_tax_deduction_nonbook ! 690
      real (kind=4), save :: acq_vacation_tax_deduction_nonbook ! 691
      real (kind=4), save :: acq_pension_tax_deduction_book ! 714
      real (kind=4), save :: acq_storm_tax_deduction_book ! 715
      real (kind=4), save :: acq_vacation_tax_deduction_book ! 716
      real (kind=4), save :: acq_other_om_tax_expense ! 724
      real (kind=4), save :: acq_atl_lease_interest
      real (kind=4), save :: acq_btl_lease_interest
      real (kind=4), save :: acq_tax_lease_expense
      real (kind=4), save :: acq_lease_principal_payments
      real (kind=4), save :: acq_deferred_taxes_rollup_cr
      real (kind=4), save :: acq_ltd_ps_deferred_tax_cr
      real (kind=4), save :: acq_fasb143_aro_net_asset_bal ! 636
      real (kind=4), save :: acq_fasb143_aro_liablility_bal ! 637
      real (kind=4), save :: acq_fasb143_aro_interest_accreation !638
      real (kind=4), save :: acq_fasb143_aro_cash_payments ! 746
      real (kind=4), save :: acq_fasb143_aro_trust_cash_payments ! 747
      real (kind=4), save :: acq_fasb143_total_aro_cash_payments ! 748
      real (kind=4), save :: acq_notes_payable_interest ! 639
      real (kind=4), save :: acq_change_invest_income_recivable ! 640
      real (kind=4), save :: acq_asset_minus_liabs ! 641
      real (kind=4), save :: acq_annual_change_asset_minus_liabs !642
      real (kind=4), save :: acq_income_tax_payable_4_affiliates ! 643
      real (kind=4), save :: acq_output_class_id
      real (kind=4), save :: acq_long_term_debt_mgt_retirements
      real (kind=4), save :: acq_class_gas_adj_clause_revenue
      real (kind=4), save :: acq_std_interest_rate_used
      real(kind=4), save :: acq_unamort_debt_interest_bal
      real(kind=4), save :: acq_unamort_debit_interest_bal
      real(kind=4), save :: acq_interest_amort_from_debit_file
      real(kind=4), save :: acq_icap_revenues
      real(kind=4), save :: acq_total_cash_expenses
      real(kind=4), save :: acq_non_base_revenues
      real(kind=4), save :: acq_ebit_per_ave_share
      real(kind=4), save :: acq_gross_cash_flow_per_ave_share
      real(kind=4), save :: acq_cbidt
      real(kind=4), save :: acq_cbidt_per_ave_share
      real(kind=4), save :: acq_gross_cash_flow
      real (kind=4), save :: acq_gross_cash_2_debt,acq_total_debt
      real(kind=4), save :: acq_ebitda_2_debt
      real(kind=4), save :: acq_ebitda
      real(kind=4), save :: acq_other_expense_1
      real(kind=4), save :: acq_other_expense_2
      real(kind=4), save :: acq_other_expense_3
      real(kind=4), save :: acq_purchased_gas
      real (kind=4), save :: acq_fed_deferred_taxes_from_nol
      real (kind=4), save :: acq_state_deferred_taxes_from_nol
      real(kind=4), save :: acq_def_taxes_from_fed_amt_credits
      real(kind=4), save :: acq_def_tax_addend_from_tax_file
      real (kind=4), save :: acq_transact_market_purchases
         
      real (kind=4) , save :: acq_transact_market_revenues
      real (kind=4), save :: acq_other_purchase_power_expense
      real (kind=4), save :: acq_afiliate_purchase_power_expense
      real (kind=4), save :: acq_net_goodwill
      real (kind=4), save :: acq_goodwill_of_new_sub_investment
      real (kind=4), save :: acq_total_acquision_cost
      real (kind=4), save :: acq_common_shares_issue_this_period
      real (kind=4), save :: acq_fe_purchase_power_total ! 650
      real (kind=4), save :: acq_fe_nuc_non_fuel_exp ! 651
      real (kind=4), save :: acq_fe_nuc_non_fuel_exp_wo_deposal ! 659
      real (kind=4), save :: acq_fe_fossil_non_fuel_exp ! 652
      real (kind=4), save :: acq_fe_inc_total_prod_cost ! 653
      real (kind=4), save :: acq_fe_exp_purchase_power_expense ! 654
      
      real (kind=4), save :: acq_prior_cause_of_itc_amort
      real (kind=4), save :: acq_income_tax_timing_adjs
      real (kind=4), save :: acq_nf_non_cash_wo_decommissioning

       real(kind=4), save :: acq_nf_expense_wo_decom_disposal
       real(kind=4), save :: acq_non_income_taxes_accrual_adj
       real(kind=4), save :: acq_change_interest_divs_accrued
       real(kind=4), save :: acq_state_taxes_accrual_adj
       real(kind=4), save :: acq_federal_taxes_accrual_adj
       real(kind=4), save :: acq_funds_consolidating_adjustment
       real(kind=4), save :: acq_intra_taxes_payable_output
         
       real (kind=4), save :: acq_notes_payable_balance
         
       real(kind=4), save :: acq_notes_receivable_balance
       real(kind=4), save :: acq_cash_change_in_notes_pay_rec
       real(kind=4), save :: acq_net_of_tax_nucl_fund_return
       real(kind=4), save :: acq_std_interest_cash_payment
       real(kind=4), save :: acq_change_in_accounts_receivable
       real(kind=4), save :: acq_investment_salvage_other
       real(kind=4), save :: acq_taxable_investment_income
       real (kind=4), save :: acq_trans_nuke_fuel_bal
         
       real (kind=4), save :: acq_change_in_accounts_payable
         
       real(kind=4), save :: acq_exp_change_in_accounts_payable
       real(kind=4), save :: acq_wvpa_change_in_accounts_payable
       real(kind=4), save :: acq_production_change_in_payables
       real (kind=4), save :: acq_prop_tax_value_b4_exclusion
         
       real(kind=4), save :: acq_prop_tax_addendum
       real(kind=4), save :: acq_prop_tax_value_exclusion
       real(kind=4), save :: acq_prop_tax_from_prior_level
       real(kind=4), save :: acq_prop_tax_rate
       real(kind=4), save :: acq_property_value
       real(kind=4), save :: acq_property_taxes_based_on_value
       real(kind=4), save :: acq_cash_2_lt_investments
       real (kind=4), save :: acq_annualized_ps_dividends
         
       real(kind=4), save :: acq_annualized_ltd_interest
       real(kind=4), save :: acq_embed_cost_capital_w_std
       real(kind=4), save :: acq_embed_cost_capital_wo_std
       real(kind=4), save :: acq_net_tax_embed_cost_captl_w_std
       real(kind=4), save :: acq_net_tax_embed_cost_captl_wo_std
       real(kind=4), save :: acq_annualized_std_interest
       real(kind=4), save :: acq_annualized_equity_cost
       real(kind=4), save :: acq_lease_receipts
       real(kind=4), save :: acq_other_lt_liability_bal
       real(kind=4), save :: acq_class_leased_nfce
       real(kind=4), save :: acq_nuclear_fuel_lease_payments
       real(kind=4), save :: acq_new_sub_investment
       real(kind=4), save :: acq_class_regulatory_assets
       real(kind=4), save :: acq_fasb_109
       real(kind=4), save :: acq_fasb_133
       real(kind=4), save :: acq_class_unamortized_interest_bal
       real(kind=4), save :: acq_class_unamortized_issue_exp_balance
       real(kind=4), save :: acq_other_net_deferrals
       real(kind=4), save :: acq_other_dd_amort
       real(kind=4), save :: acq_goodwill_amort
       real(kind=4), save :: acq_class_regulatory_assets_amort
       real(kind=4), save :: acq_fasb_109_amort
       real(kind=4), save :: acq_fasb_133_amort
       real (kind=4), save :: acq_year_end_shares

       real(kind=4), save :: acq_boy_shares
       real(kind=4), save :: acq_retirement_medical_payments
       real(kind=4), save :: acq_retirement_medical_fund
       real(kind=4), save :: acq_retirement_med_payable
       real(kind=4), save :: acq_decom_fund_liability
       real(kind=4), save :: acq_lt_investments
       real(kind=4), save :: acq_stock_market_price
       real(kind=4), save :: acq_average_stock_price
       real(kind=4), save :: acq_gas_revenues
       real(kind=4), save :: acq_capacity_sales_revenue
       real(kind=4), save :: acq_catawba_tax_deductible_expense
       real(kind=4), save :: acq_other_taxable_income
       real(kind=4), save :: acq_catawba_inc_statement_expenses
       real(kind=4), save :: acq_federal_nol_generated
       real(kind=4), save :: acq_federal_nols_used
       real(kind=4), save :: acq_state_nol_generated
       real(kind=4), save :: acq_state_nols_used
       real(kind=4), save :: acq_transmission_operation
       real(kind=4), save :: acq_transmission_maintenance
       real(kind=4), save :: acq_distribution_operation
       real(kind=4), save :: acq_distribution_maintenance
       real(kind=4), save :: acq_customer_accounts
       real(kind=4), save :: acq_customer_services
       real(kind=4), save :: acq_sales_expense
       real(kind=4), save :: acq_ag_operations
       real(kind=4), save :: acq_ag_maintenance
       real(kind=4), save :: acq_unbilled_revenues
       real(kind=4), save :: acq_atl_deferred_revenues
       real(kind=4), save :: acq_relationship_revenues
       real(kind=4), save :: acq_residential_revenues
       real(kind=4), save :: acq_captured_opt_method_revenues
       real(kind=4), save :: acq_commercial_revenues
       real(kind=4), save :: acq_industrial_revenues
       real(kind=4), save :: acq_government_sales_revenue
       real(kind=4), save :: acq_lighting_revenues
       real(kind=4), save :: acq_bulk_power_revenues
       real(kind=4), save :: acq_non_cash_atl_revenues
       real(kind=4), save :: acq_total_base_rates_revenues
       real(kind=4), save :: acq_operating_method_adjustment
       real(kind=4), save :: acq_operating_method_rollup
       real(kind=4), save :: acq_gav_other_opt_revenues
       real(kind=4), save :: acq_competitive_sales_revenue
       real(kind=4), save :: acq_utility_sales_revenue
       real(kind=4), save :: acq_fe_competitive_unit_sales
       real(kind=4), save :: acq_fe_intra_company_utility_sales
       real(kind=4), save :: acq_wvpa_member_accrued_revenues
       real(kind=4), save :: acq_regulated_revenues_11
       real(kind=4), save :: acq_regulated_revenues_12
       real(kind=4), save :: acq_regulated_revenues_13
       real(kind=4), save :: acq_regulated_revenues_14
       real(kind=4), save :: acq_regulated_revenues_15
       real(kind=4), save :: acq_non_reg_revenues_10
       real(kind=4), save :: acq_non_reg_revenues_11
       real(kind=4), save :: acq_non_reg_revenues_12
       real(kind=4), save :: acq_non_reg_revenues_13
       real(kind=4), save :: acq_non_reg_revenues_14
       real(kind=4), save :: acq_non_reg_revenues_15
       real(kind=4), save :: acq_wvpa_non_member_cost_of_power
       real(kind=4), save :: acq_wvpa_member_cost_of_power
       real(kind=4), save :: acq_wvpa_member_cost_of_services
       real(kind=4), save :: acq_wvpa_non_member_cost_of_services

       real, save :: acq_total_derivative_revenue
       real, save :: acq_total_derivative_expense
       real, save :: acq_phys_derivatives_var_revenue
       real, save :: acq_phys_derivatives_fix_revenue
       real, save :: acq_fin_derivatives_var_revenue
       real, save :: acq_fin_derivatives_fix_revenue
       real, save :: acq_phys_derivatives_var_expense
       real, save :: acq_phys_derivatives_fix_expense
       real, save :: acq_fin_derivatives_var_expense
       real, save :: acq_fin_derivatives_fix_expense
       real, save :: acq_phy_deriv_revenue_energy
       real, save :: acq_phy_deriv_expense_energy
       real, save :: acq_fin_deriv_revenue_energy
       real, save :: acq_fin_deriv_expense_energy
       real, save :: acq_phy_fuel_deriv_var_revenue
       real, save :: acq_phy_fuel_deriv_fix_revenue
       real, save :: acq_phy_fuel_deriv_var_expense
       real, save :: acq_phy_fuel_deriv_fix_expense
       real, save :: acq_fin_fuel_deriv_var_revenue
       real, save :: acq_fin_fuel_deriv_fix_revenue
       real, save :: acq_fin_fuel_deriv_var_expense
       real, save :: acq_fin_fuel_deriv_fix_expense
       real, save :: acq_net_derivatives_income
       real, save :: acq_phy_fuel_deriv_revenue_energy
       real, save :: acq_phy_fuel_deriv_expense_energy
       real, save :: acq_fin_fuel_deriv_revenue_energy
       real, save :: acq_fin_fuel_deriv_expense_energy
       real, save :: acq_total_fuel_deriv_revenues
       real, save :: acq_total_fuel_deriv_expenses
       real, save :: acq_net_fuel_deriv_margin
       real, save :: acq_net_total_pow_fuel_deriv_margin
       real, save :: acq_total_energy_of_revenue_deriv
       real, save :: acq_total_energy_of_expense_deriv
       real (kind=4), save :: acq_taxes_non_income
real(kind=4), save :: acq_atl_amortization,ltd_interest_and_amortization
       
       real (kind=4), save :: acq_total_def_taxes_cr
 real (kind=4), save :: acq_nf_capitialized_interest,nf_atl_amortization
       real (kind=4), save :: acq_nf_afudc_on_cash
       real (kind=4), save :: acq_total_capital_service
       real (kind=4), save :: acq_fed_taxes_b4_credits_adjts
         
       real (kind=4) , save :: acq_fed_credits_used
       real (kind=4) , save :: acq_fed_tax_credit_passed_up
       real (kind=4) , save :: acq_adj_2_federal_taxes
       real (kind=4) , save :: acq_m1_fed_additions
       real (kind=4) , save :: acq_m1_fed_deductions
       real (kind=4) , save :: acq_sec_29_credits_passed_up
       real (kind=4) , save :: acq_sec_42_credits_passed_up
       real (kind=4) , save :: acq_sec_29_credits_used
       real (kind=4) , save :: acq_sec_42_credits_used
       real (kind=4) , save :: acq_unused_federal_tax_credits
       real (kind=4) , save :: acq_unused_consold_tax_credits
         real (kind=4), save :: acq_state_taxes_b4_credits_adjts
         
       real (kind=4) , save :: acq_state_credits_used
       real (kind=4) , save :: acq_state_tax_credit_passed_up
       real (kind=4) , save :: acq_adj_2_state_taxes
       real (kind=4) , save :: acq_m1_state_additions
       real (kind=4) , save :: acq_m1_state_deductions
       real (kind=4) , save :: acq_other_taxes
       real (kind=4) , save :: acq_btl_amortization_in_expenses
       real (kind=4) , save :: acq_amortization_b4_cap_service
       real (kind=4) , save :: acq_atl_lease_amortization
       real (kind=4) , save :: acq_btl_lease_amortization
       real (kind=4) , save :: acq_atl_lease_amort_expense
       real (kind=4) , save :: acq_amt_minimum_tax
       real (kind=4) , save :: acq_amt_credits_used
       real (kind=4) , save :: acq_amt_credits_created
       real (kind=4) , save :: acq_salvage_transactions
       real (kind=4) , save :: acq_other_income_net_of_tax
       real (kind=4) , save :: acq_ps_prem_issue_exp_amort
       real (kind=4) , save :: acq_ps_dividend_plus_amort
       real (kind=4) , save :: acq_variable_om_amort
       real (kind=4) , save :: acq_other_om_amort
       real (kind=4) , save :: acq_variable_expense_plus_amort
       real (kind=4) , save :: acq_other_om_expense_plus_amort
       real (kind=4) , save :: acq_pur_power_expense_plus_amort
       real (kind=4) , save :: acq_fuel_expense_plus_amort
       real (kind=4) , save :: acq_purchase_gas_expense_plus_amort
       real (kind=4) , save :: acq_preferred_stock_in_cap
       real (kind=4) , save :: acq_long_term_debt_in_cap
       real (kind=4) , save :: acq_common_equity_bal_in_cap
       real (kind=4) , save :: acq_short_term_debt_in_cap
       real (kind=4) , save :: acq_funds_4_construction
       real (kind=4) , save :: acq_ending_nf_rate_base
       real (kind=4) , save :: acq_ending_cwip_rate_base
       real (kind=4) , save :: acq_consolid_sec_29_credits_used
       real (kind=4) , save :: acq_lost_sec_29_credits
       real (kind=4) , save :: acq_ltd_ps_issue_expenses
       real (kind=4) , save :: acq_cash_timing_adjustments
       real (kind=4) , save :: acq_annual_cash_flow_timing_adjustments
       real (kind=4) , save :: acq_ltd_interest_cash_payments
       real (kind=4) , save :: acq_ps_dividend_cash_payments
       real (kind=4) , save :: acq_common_stock_cash_dividends
       real (kind=4) , save :: acq_class_accounts_payable
       real (kind=4) , save :: acq_class_accounts_receivable
       real (kind=4) , save :: acq_class_fuel_inventory
       real (kind=4) , save :: acq_class_deferred_fuel_balance
       real (kind=4) , save :: acq_deferred_fuel_expense
       real (kind=4) , save :: acq_purchase_power_amort
       real (kind=4) , save :: acq_deferred_purchased_gas
       real (kind=4) , save :: acq_deferred_pga_expense
       real (kind=4) , save :: acq_gas_in_storage
       real (kind=4) , save :: acq_materials_n_supplies
       real (kind=4) , save :: acq_cash_adds_2_inventories
       real (kind=4) , save :: acq_cash_additions_2_fuel_inventory
       real (kind=4) , save :: acq_cash_adds_2_matrials_inventory
       real (kind=4) , save :: acq_cash_adds_2_gas_inventory
       real (kind=4) , save :: acq_expensing_fuel_inventory
       real (kind=4) , save :: acq_expensing_matrials_inventory
       real (kind=4) , save :: acq_expensing_gas_inventory
       real (kind=4) , save :: acq_class_deferred_purchase_power
       real (kind=4) , save :: acq_class_pension_liability
       real (kind=4) , save :: acq_class_deferred_gain_from_sales
       real (kind=4) , save :: acq_class_storm_reserve_balance
       real (kind=4) , save :: acq_class_vacation_pay_balance
       real (kind=4) , save :: acq_class_rb_deferred_tax_dr
       real (kind=4) , save :: acq_class_rb_deferred_revenues
       real (kind=4) , save :: acq_class_rb_pension_liability
       real (kind=4) , save :: acq_class_rb_deferred_asset_gain
       real (kind=4) , save :: acq_class_rb_storm_reserve
       real (kind=4) , save :: acq_class_rb_accrued_vacation_pay
       real (kind=4) , save :: acq_rate_base_deferred_credits
     real (kind=4) , save :: acq_vacation_pay_cash,vacation_pay_non_cash
      real (kind=4), save :: acq_pension_expense_cash
      real (kind=4), save :: pension_expense_non_cash
   real (kind=4) , save :: acq_storm_expense_cash,storm_expense_non_cash
       real (kind=4) , save :: acq_fuel_exp_transfr_4_power_sold
       real (kind=4) , save :: acq_secondary_sales_rev_transfr
       real (kind=4) , save :: acq_other_secondary_enrgy_sales_rev
       real (kind=4) , save :: acq_realloc_purchase_power_exp
       real (kind=4) , save :: acq_realloc_secondary_sales_rev
       real (kind=4) , save :: acq_wholesale_fuel_expense
       real (kind=4) , save :: acq_wholesale_vom_expense
       real (kind=4) , save :: acq_fuel_expense_after_transfr
       real (kind=4) , save :: acq_variable_expense_after_transfr
       real (kind=4) , save :: acq_ameren_var_om_amounts
       real (kind=4) , save :: acq_var_om_transfr_amounts
       real (kind=4) , save :: acq_var_market_purchases
       real (kind=4), save :: acq_fixed_market_purchases

        
        
        
        ! fe monthly midas expenses
        !
         real (kind=4), save :: atl_opt_expenses ! 1
         equivalence (acq_variable(788),atl_opt_expenses)
        
        
        ! for next available number see top of file
        
        
        ! gre special reports
        
      equivalence (acq_variable(760),acq_gre_falkirk_interest) ! 760
    equivalence (acq_variable(761),acq_gre_total_interest_expense) ! 761
     equivalence (acq_variable(762),acq_gre_falkirk_depreciation) ! 762
        equivalence (acq_variable(763),acq_gre_total_depreciation) ! 763
      equivalence (acq_variable(764),acq_gre_other_principal_adjs) ! 764
     equivalence (acq_variable(765),acq_gre_principal_manual_adjs) ! 765
        equivalence (acq_variable(766),acq_gre_total_principal_payments)
    equivalence(acq_variable(767),acq_gre_interest_payments_manual_adjs)
         equivalence (acq_variable(768),acq_gre_total_interest_payments)
         equivalence (acq_variable(769),acq_gre_net_margin) ! 769
    equivalence (acq_variable(770),acq_gre_total cost of service) ! 770
         equivalence (acq_variable(771),acq_gre_other_income) ! 771
         equivalence (acq_variable(772),acq_gre_other_revenue) ! 772
         equivalence (acq_variable(773),acq_gre_dsc) ! 773
         equivalence (acq_variable(777),acq_gre_debt_retirements) ! 777
    equivalence (acq_variable(778),acq_gre_lease_payments_in_tier) ! 778
        
equivalence (acq_variable(710),acq_gain_on_reaquired_debt_bal_eoy) ! 710
 equivalence (acq_variable(711),acq_regulatory_deferred_credits_bal_eoy)
      equivalence (acq_variable(712),acq_other_deferred_credits_bal_eoy)
         equivalence (acq_variable(713),acq_total_deferred_credits_bal)
        equivalence (acq_variable(777),acq_deferred_credit_amortization)
         equivalence (acq_variable(729),acq_class_fasb87_balance) ! 729
         equivalence (acq_variable(730),acq_class_oci_balance) ! 730
         equivalence (acq_variable(731),acq_total_equity_with_oci) ! 731
       equivalence (acq_variable(732),acq_total_capitial_with_oci) ! 732
      equivalence (acq_variable(740),acq_current_ltd_retirements) ! 740
   equivalence (acq_variable(741),acq_ltd_balance_wo_current_ltd)  ! 741
   equivalence (acq_variable(742),acq_total_capitial_bal_wo_current_ltd)
       equivalence (acq_variable(755),acq_other_payments_to_trust_funds)
         
equivalence (acq_variable(683),acq_ecities_transfer_benefit) ! 683
equivalence (acq_variable(684),acq_ecities_wholesale_prod_cost) ! 684
equivalence (acq_variable(685),acq_ecities_var_prod_cost) ! 685
equivalence (acq_variable(686),acq_ecities_new_fixed_cost) ! 686
         
equivalence (acq_variable(687),acq_ecities_market_energy_sales) ! 687 r
equivalence (acq_variable(688),acq_ecities_transmission_fees)!688 c-fees
         
equivalence (acq_variable(689),acq_pension_tax_deduction_nonbook) ! 689
         
equivalence (acq_variable(690),acq_storm_tax_deduction_nonbook) ! 690
equivalence (acq_variable(691),acq_vacation_tax_deduction_nonbook) ! 691
equivalence (acq_variable(714),acq_pension_tax_deduction_book) ! 714
equivalence (acq_variable(715),acq_storm_tax_deduction_book) ! 715
equivalence (acq_variable(716),acq_vacation_tax_deduction_book) ! 716
              
         equivalence (acq_variable(650),acq_fe_purchase_power_total)
         
        equivalence (acq_variable(651),acq_fe_nuc_non_fuel_exp) ! 651
        equivalence (acq_variable(652),acq_fe_fossil_non_fuel_exp) ! 652
        equivalence (acq_variable(653),acq_fe_inc_total_prod_cost) ! 653
equivalence (acq_variable(654),acq_fe_exp_purchase_power_expense) ! 654
     equivalence (acq_variable(527),acq_fe_competitive_unit_sales) ! 527
      equivalence (acq_variable(526),acq_fe_intra_company_utility_sales)
equivalence (acq_variable(659),acq_fe_nuc_non_fuel_exp_wo_deposal) ! 659
         
         equivalence (acq_variable(655),acq_fed_deferred_taxes_from_nol)
         
       equivalence (acq_variable(656),acq_state_deferred_taxes_from_nol)
      equivalence (acq_variable(657),acq_def_taxes_from_fed_amt_credits)
        equivalence (acq_variable(658),acq_def_tax_addend_from_tax_file)
        equivalence (acq_variable(476),acq_total_cash_expenses)
        equivalence (acq_variable(485),acq_non_base_revenues)
        equivalence (acq_variable(29),acq_deferred_taxes_rollup_cr)
        equivalence (acq_variable(681),acq_ltd_ps_deferred_tax_cr)
         
         
       equivalence (acq_variable(502),acq_fuel_exp_transfr_4_power_sold)
         
        equivalence (acq_variable(503),acq_secondary_sales_rev_transfr)
     equivalence (acq_variable(504),acq_other_secondary_enrgy_sales_rev)
        equivalence (acq_variable(505),acq_realloc_purchase_power_exp)
        equivalence (acq_variable(506),acq_realloc_secondary_sales_rev)
        equivalence (acq_variable(507),acq_wholesale_fuel_expense)
        equivalence (acq_variable(508),acq_wholesale_vom_expense)
        equivalence (acq_variable(509),acq_fuel_expense_after_transfr)
      equivalence (acq_variable(510),acq_variable_expense_after_transfr)
        equivalence (acq_variable(561),acq_ameren_var_om_amounts)
        equivalence (acq_variable(562),acq_var_om_transfr_amounts)
         
        equivalence (acq_variable(449),acq_total_derivative_revenue)
        
        equivalence (acq_variable(450),acq_total_derivative_expense)
        equivalence (acq_variable(451),acq_phys_derivatives_var_revenue)
        equivalence (acq_variable(452),acq_phys_derivatives_fix_revenue)
        equivalence (acq_variable(453),acq_fin_derivatives_var_revenue)
        equivalence (acq_variable(454),acq_fin_derivatives_fix_revenue)
        equivalence (acq_variable(455),acq_phys_derivatives_var_expense)
        equivalence (acq_variable(456),acq_phys_derivatives_fix_expense)
        equivalence (acq_variable(457),acq_fin_derivatives_var_expense)
        equivalence (acq_variable(458),acq_fin_derivatives_fix_expense)
        equivalence (acq_variable(646),acq_phy_deriv_revenue_energy)
        equivalence (acq_variable(647),acq_phy_deriv_expense_energy)
        equivalence (acq_variable(648),acq_fin_deriv_revenue_energy)
        equivalence (acq_variable(649),acq_fin_deriv_expense_energy)
        equivalence (acq_variable(692),acq_phy_fuel_deriv_var_revenue)
        equivalence (acq_variable(693),acq_phy_fuel_deriv_fix_revenue)
        equivalence (acq_variable(694),acq_phy_fuel_deriv_var_expense)
        equivalence (acq_variable(695),acq_phy_fuel_deriv_fix_expense)
        equivalence (acq_variable(696),acq_fin_fuel_deriv_var_revenue)
        equivalence (acq_variable(697),acq_fin_fuel_deriv_fix_revenue)
        equivalence (acq_variable(698),acq_fin_fuel_deriv_var_expense)
        equivalence (acq_variable(699),acq_fin_fuel_deriv_fix_expense)
        equivalence (acq_variable(448),acq_net_derivatives_income)
       equivalence (acq_variable(700),acq_phy_fuel_deriv_revenue_energy)
       equivalence (acq_variable(701),acq_phy_fuel_deriv_expense_energy)
       equivalence (acq_variable(702),acq_fin_fuel_deriv_revenue_energy)
       equivalence (acq_variable(703),acq_fin_fuel_deriv_expense_energy)
        equivalence (acq_variable(704),acq_total_fuel_deriv_revenues)
        equivalence (acq_variable(705),acq_total_fuel_deriv_expenses)
        equivalence (acq_variable(706),acq_net_fuel_deriv_margin)
     equivalence (acq_variable(707),acq_net_total_pow_fuel_deriv_margin)
       equivalence (acq_variable(708),acq_total_energy_of_revenue_deriv)
       equivalence (acq_variable(709),acq_total_energy_of_expense_deriv)
     equivalence (acq_variable(486),acq_goodwill_of_new_sub_investment)
        equivalence (acq_variable(487),acq_total_acquision_cost)
     equivalence (acq_variable(636),acq_fasb143_aro_net_asset_bal) ! 636
    equivalence (acq_variable(637),acq_fasb143_aro_liablility_bal) ! 637
equivalence (acq_variable(638),acq_fasb143_aro_interest_accreation) !638
        equivalence (acq_variable(746),acq_fasb143_aro_cash_payments)
     equivalence (acq_variable(747),acq_fasb143_aro_trust_cash_payments)
     equivalence (acq_variable(748),acq_fasb143_total_aro_cash_payments)
        equivalence (acq_variable(639),acq_notes_payable_interest) ! 639
equivalence (acq_variable(640),acq_change_invest_income_recivable) ! 640
        equivalence (acq_variable(641),acq_asset_minus_liabs) ! 641
equivalence (acq_variable(642),acq_annual_change_asset_minus_liabs) !642
equivalence (acq_variable(643),acq_income_tax_payable_4_affiliates)! 643
        equivalence (acq_variable(677),acq_atl_lease_interest)
        equivalence (acq_variable(678),acq_btl_lease_interest)
        equivalence (acq_variable(679),acq_tax_lease_expense)
        equivalence (acq_variable(680),acq_lease_principal_payments)

         equivalence (acq_variable(0),acq_total_base_rates_revenues)
         
         
        equivalence (acq_variable(329),acq_residential_revenues)
        equivalence (acq_variable(496),acq_captured_opt_method_revenues)
        equivalence (acq_variable(330),acq_commercial_revenues)
        equivalence (acq_variable(331),acq_industrial_revenues)
        equivalence (acq_variable(332),acq_lighting_revenues)
        equivalence (acq_variable(382),acq_government_sales_revenue)
        equivalence (acq_variable(660),acq_wvpa_member_accrued_revenues)
        equivalence (acq_variable(661),acq_regulated_revenues_11)
        equivalence (acq_variable(662),acq_regulated_revenues_12)
        equivalence (acq_variable(663),acq_regulated_revenues_13)
        equivalence (acq_variable(664),acq_regulated_revenues_14)
        equivalence (acq_variable(665),acq_regulated_revenues_15)
        equivalence (acq_variable(671),acq_non_reg_revenues_10)
        equivalence (acq_variable(672),acq_non_reg_revenues_11)
        equivalence (acq_variable(673),acq_non_reg_revenues_12)
        equivalence (acq_variable(674),acq_non_reg_revenues_13)
        equivalence (acq_variable(675),acq_non_reg_revenues_14)
        equivalence (acq_variable(676),acq_non_reg_revenues_15)
        equivalence (acq_variable(1),acq_adjustment_clause_revenues)
        equivalence (acq_variable(474),acq_class_gas_adj_clause_revenue)

        equivalence (acq_variable(3),acq_revenues_other)
        equivalence (acq_variable(252),acq_reference_base_revenues)
        equivalence (acq_variable(379),acq_operating_method_rollup)
        equivalence (acq_variable(253),acq_operating_method_adjustment)
        equivalence (acq_variable(4),acq_revenues_operating)
        equivalence (acq_variable(307),acq_gas_revenues)
        equivalence (acq_variable(380),acq_capacity_sales_revenue)
        equivalence (acq_variable(326),acq_unbilled_revenues)
        equivalence (acq_variable(327),acq_atl_deferred_revenues)
        equivalence (acq_variable(328),acq_relationship_revenues)
        equivalence (acq_variable(333),acq_bulk_power_revenues)
        equivalence (acq_variable(334),acq_non_cash_atl_revenues)
        equivalence (acq_variable(381),acq_gav_other_opt_revenues)
        equivalence (acq_variable(563),acq_competitive_sales_revenue)
        equivalence (acq_variable(564),acq_utility_sales_revenue)
     equivalence (acq_variable(744),acq_wvpa_member_rates_with_sdi) !744
 equivalence (acq_variable(745),acq_wvpa_member_rates_without_sdi) ! 745
     equivalence (acq_variable(751),acq_wvpa_member_energy_sales) ! 751
        equivalence (acq_variable(752),acq_wvpa_sdi_energy_sales) ! 752
        equivalence (acq_variable(753),acq_wvpa_member_margin) ! 753
        equivalence (acq_variable(754),acq_wvpa_non_member_margin) ! 754
   equivalence (acq_variable(756),acq_wvpa_debt_interest_coverage) ! 755
  equivalence (acq_variable(758),acq_wvpa_property_taxes_in_power_costs)
         
        
        ! cash expenses
        
        equivalence (acq_variable(5),acq_fuel_expense)
        equivalence (acq_variable(6),acq_purchase_power_expense)
        equivalence (acq_variable(470),acq_transact_market_purchases)
        equivalence (acq_variable(471),acq_transact_market_revenues)
        equivalence (acq_variable(472),acq_other_purchase_power_expense)
     equivalence (acq_variable(666),acq_afiliate_purchase_power_expense)
       equivalence (acq_variable(735),acq_wvpa_non_member_cost_of_power)
        equivalence (acq_variable(736),acq_wvpa_member_cost_of_power)
        equivalence (acq_variable(749),acq_wvpa_member_cost_of_services)
    equivalence (acq_variable(750),acq_wvpa_non_member_cost_of_services)
        equivalence (acq_variable(7),acq_variable_expense)
        equivalence (acq_variable(362),acq_variable_om_amort)
        equivalence (acq_variable(439),acq_other_om_amort)
        equivalence (acq_variable(460),acq_purchase_power_amort)
        equivalence (acq_variable(440),acq_other_om_expense_plus_amort)
        equivalence (acq_variable(461),acq_pur_power_expense_plus_amort)
        equivalence (acq_variable(722),acq_fuel_expense_plus_amort)
     equivalence (acq_variable(723),acq_purchase_gas_expense_plus_amort)
        equivalence (acq_variable(724),acq_other_om_tax_expense)
      equivalence (acq_variable(725),acq_capacity_purchases_to_level_rm)
        equivalence (acq_variable(726),acq_capacity_sales_to_level_rm)
      equivalence (acq_variable(727),acq_inc_capacity_sales_to_level_rm)
  equivalence (acq_variable(728),acq_inc_capacity_purchases_to_level_rm)
        equivalence (acq_variable(421),acq_vacation_pay_non_cash)
        equivalence (acq_variable(422),acq_pension_expense_non_cash)
        equivalence (acq_variable(423),acq_storm_expense_non_cash)
        equivalence (acq_variable(363),acq_variable_expense_plus_amort)
        equivalence (acq_variable(8),acq_fixed_expense)
        equivalence (acq_variable(9),acq_service_transaction_expense)
        equivalence (acq_variable(10),acq_other_expense_1)
    equivalence (acq_variable(11),acq_other_expense_2,ACQ_purchased_gas)
        equivalence (acq_variable(12),acq_other_expense_3)
        equivalence (acq_variable(335),acq_transmission_operation)
        equivalence (acq_variable(336),acq_transmission_maintenance)
        equivalence (acq_variable(337),acq_distribution_operation)
        equivalence (acq_variable(338),acq_distribution_maintenance)
        equivalence (acq_variable(321),acq_customer_accounts)
        equivalence (acq_variable(322),acq_customer_services)
        equivalence (acq_variable(323),acq_sales_expense)
        equivalence (acq_variable(324),acq_ag_operations)
        equivalence (acq_variable(325),acq_ag_maintenance)

        equivalence (acq_variable(13),acq_dsm_expense)
        equivalence (acq_variable(14),acq_dsm_rebate)
        equivalence (acq_variable(15),acq_emission_credit_expense)
        equivalence (acq_variable(16),acq_class_nuclear_fuel_expense)
        equivalence (acq_variable(219),acq_owned_nf_burn)
        equivalence (acq_variable(229),acq_nuc_fuel_owned_burn)
        equivalence (acq_variable(230),acq_nuc_fuel_leased_burn)
        equivalence (acq_variable(234),acq_nuc_fuel_total_burn)
        equivalence (acq_variable(231),acq_nf_non_cash_expense)
        equivalence (acq_variable(232),acq_doe_nuc_fuel_fee)
        equivalence (acq_variable(233),acq_nuc_decommissioning_cost)
      equivalence (acq_variable(437),acq_nf_non_cash_wo_decommissioning)
        equivalence (acq_variable(438),acq_nf_expense_wo_decom_disposal)
        equivalence (acq_variable(235),acq_net_of_tax_nucl_fund_return)
        equivalence (acq_variable(227),acq_nf_tax_expense)
        equivalence (acq_variable(236),acq_nf_sl_def_tax_dep)
        equivalence (acq_variable(733),acq_nf_deferred_taxes_cr)
       equivalence (acq_variable(734),acq_capx_pension_adjustment) ! 734
        equivalence (acq_variable(237),acq_nf_cash_expense)
        equivalence (acq_variable(238),acq_nf_deferred_tax_basis)
        equivalence (acq_variable(296),acq_nf_capitialized_interest)
        equivalence (acq_variable(297),acq_nf_atl_amortization)
        equivalence (acq_variable(298),acq_nf_afudc_on_cash)
        equivalence (acq_variable(290),acq_retirement_medical_payments)
        equivalence (acq_variable(291),acq_atl_amortization)
        equivalence (acq_variable(352),acq_atl_lease_amortization)
        equivalence (acq_variable(354),acq_atl_lease_amort_expense)
        equivalence (acq_variable(393),acq_deferred_fuel_expense)
        equivalence (acq_variable(473),acq_deferred_pga_expense)

       equivalence (acq_variable(293),acq_ltd_interest_and_amortization)
        equivalence (acq_variable(295),acq_total_def_taxes_cr)
        equivalence (acq_variable(497),acq_other_dd_amort)
        equivalence (acq_variable(498),acq_goodwill_amort)
       equivalence (acq_variable(499),acq_class_regulatory_assets_amort)
        equivalence (acq_variable(500),acq_fasb_109_amort)
        equivalence (acq_variable(501),acq_fasb_133_amort)

        ! tax items

        equivalence (acq_variable(21),acq_operating_revenue_tax)
       equivalence (acq_variable(22),acq_other_and_environmental_taxes)
        equivalence (acq_variable(218),acq_other_taxes)
        equivalence (acq_variable(24),acq_atl_state_taxes)
        equivalence (acq_variable(26),acq_atl_federal_taxes)
        equivalence (acq_variable(50),acq_tax_credits)
        equivalence (acq_variable(30),acq_total_expense_and_taxes)
        equivalence (acq_variable(31),acq_operating_income)
        equivalence (acq_variable(427),acq_taxable_investment_income)
        equivalence (acq_variable(357),acq_other_income_net_of_tax)
        equivalence (acq_variable(350),acq_btl_amortization_in_expenses)
        equivalence (acq_variable(37),acq_afudc_equity)
        equivalence (acq_variable(38),acq_income_b4_interest)
        equivalence (acq_variable(39),acq_interest_on_long_term_debt)
        equivalence (acq_variable(40),acq_std_interest)
        equivalence (acq_variable(405),acq_std_interest_cash_payment)
        equivalence (acq_variable(41),acq_afudc_borrowed)
        equivalence (acq_variable(42),acq_income_after_interest)
        equivalence (acq_variable(45),acq_net_profit_or_loss)
        equivalence (acq_variable(360),acq_ps_prem_issue_exp_amort)
        equivalence (acq_variable(361),acq_ps_dividend_plus_amort)
        equivalence (acq_variable(47),acq_earnings_available_to_common)
        equivalence (acq_variable(51),acq_stock_book_value)
        equivalence (acq_variable(52),acq_stock_market_price)
        equivalence (acq_variable(53),acq_average_shares)
        equivalence (acq_variable(299),acq_year_end_shares)
        equivalence (acq_variable(409),acq_boy_shares)
     equivalence (acq_variable(626),acq_common_shares_issue_this_period)
        equivalence (acq_variable(55),acq_earnings_per_share)
        equivalence (acq_variable(57),acq_dividend_payout)
        equivalence (acq_variable(58),acq_return_on_equity)
        equivalence (acq_variable(59),acq_average_equity)

        ! tax stuff

        equivalence (acq_variable(128),acq_taxable_income)
        equivalence (acq_variable(129),acq_capitialized_interest)
        equivalence (acq_variable(131),acq_state_taxable_income)
        equivalence (acq_variable(308),acq_state_nol_generated)
        equivalence (acq_variable(309),acq_state_nols_used)
     equivalence (acq_variable(132),acq_state_taxable_income_after_nols)
        equivalence (acq_variable(134),acq_adj_2_state_taxes)
        equivalence (acq_variable(345),acq_state_taxes_b4_credits_adjts)
        equivalence (acq_variable(346),acq_state_credits_used)
        equivalence (acq_variable(347),acq_state_tax_credit_passed_up)
        equivalence (acq_variable(348),acq_m1_state_additions)
        equivalence (acq_variable(349),acq_m1_state_deductions)
        equivalence (acq_variable(135),acq_federal_taxable_income)
        equivalence (acq_variable(310),acq_federal_nol_generated)
        equivalence (acq_variable(311),acq_federal_nols_used)
       equivalence (acq_variable(136),acq_fed_taxable_income_after_nols)
        equivalence (acq_variable(138),acq_adj_2_federal_taxes)
        equivalence (acq_variable(340),acq_fed_taxes_b4_credits_adjts)
        equivalence (acq_variable(341),acq_fed_credits_used)
        equivalence (acq_variable(342),acq_fed_tax_credit_passed_up)
        equivalence (acq_variable(371),acq_sec_29_credits_passed_up)
        equivalence (acq_variable(372),acq_sec_42_credits_passed_up)
        equivalence (acq_variable(373),acq_sec_29_credits_used)
        equivalence (acq_variable(374),acq_sec_42_credits_used)
        equivalence (acq_variable(375),acq_unused_federal_tax_credits)
        equivalence (acq_variable(376),acq_unused_consold_tax_credits)
        equivalence (acq_variable(378),acq_lost_sec_29_credits)
        equivalence (acq_variable(343),acq_m1_fed_additions)
        equivalence (acq_variable(344),acq_m1_fed_deductions)
        equivalence (acq_variable(355),acq_amt_minimum_tax)
        equivalence (acq_variable(400),acq_amt_credits_used)
        equivalence (acq_variable(401),acq_amt_credits_created)
        equivalence (acq_variable(140),acq_total_income_taxes)
        equivalence (acq_variable(142),acq_elimination_cash_adjustment)
        equivalence (acq_variable(209),acq_taxable_income_b4_deductions)
        equivalence (acq_variable(210),acq_tax_deductible_expenses_book)

        ! ratebase items

        equivalence (acq_variable(61),acq_return_on_ratebase)
        equivalence (acq_variable(62),acq_asset_class_rate_base)
        equivalence (acq_variable(143),acq_rate_base_plant_in_service)
        equivalence (acq_variable(144),acq_rate_base_def_taxes)
        equivalence (acq_variable(146),acq_rate_base_deferred_debits)
        equivalence (acq_variable(147),acq_rate_base_nuc_fuel)
        equivalence (acq_variable(148),acq_rate_base_cwip)
        equivalence (acq_variable(149),acq_rate_base_def_itc)
        equivalence (acq_variable(150),acq_rate_base_nuc_decom)
        equivalence (acq_variable(151),acq_rate_base_ciac)
        equivalence (acq_variable(152),acq_rate_base_asset_nec)
       equivalence (acq_variable(153),acq_rate_base_capitialized_leases)
        equivalence (acq_variable(369),acq_ending_nf_rate_base)
        equivalence (acq_variable(370),acq_ending_cwip_rate_base)
        equivalence (acq_variable(415),acq_class_rb_deferred_tax_dr)
        equivalence (acq_variable(774),acq_rate_base_deferred_credits)
        equivalence (acq_variable(416),acq_class_rb_deferred_revenues)
        equivalence (acq_variable(417),acq_class_rb_pension_liability)
        equivalence (acq_variable(418),acq_class_rb_deferred_asset_gain)
        equivalence (acq_variable(419),acq_class_rb_storm_reserve)
        
       equivalence (acq_variable(420),acq_class_rb_accrued_vacation_pay)
        ! balance sheet assets (63-100)
        
        equivalence (acq_variable(63),acq_gross_plant_value)
        equivalence (acq_variable(64),acq_cwip)
        equivalence (acq_variable(65),acq_gross_plant_value_plus_cwip)
        equivalence (acq_variable(66),acq_cumulative_depreciation)
        equivalence (acq_variable(67),acq_net_nf_value)
        equivalence (acq_variable(68),acq_utility_plant_net)
        equivalence (acq_variable(69),acq_assets_other)
        equivalence (acq_variable(91),acq_subsidiary_investment)
        equivalence (acq_variable(92),acq_leases_capitalized)
        equivalence (acq_variable(402),acq_notes_receivable_balance)
        equivalence (acq_variable(404),acq_cash_change_in_notes_pay_rec)
        equivalence (acq_variable(70),acq_decommisioning_fund_bal)
        equivalence (acq_variable(71),acq_dd_balance_net)
        equivalence (acq_variable(72),acq_deferred_revenues_cum)
        equivalence (acq_variable(73),acq_deferred_taxes_dr_bal)
        equivalence (acq_variable(74),acq_cash_balance)
       equivalence (acq_variable(410),acq_class_deferred_purchase_power)
       equivalence (acq_variable(407),acq_change_in_accounts_receivable)
        equivalence (acq_variable(390),acq_class_accounts_receivable)
        equivalence (acq_variable(391),acq_class_fuel_inventory)
      equivalence (acq_variable(339),acq_class_unamortized_interest_bal)
 equivalence (acq_variable(743),acq_class_unamortized_issue_exp_balance)
        equivalence (acq_variable(462),acq_net_goodwill)
        equivalence (acq_variable(463),acq_class_regulatory_assets)
        equivalence (acq_variable(464),acq_fasb_109)
        equivalence (acq_variable(465),acq_fasb_133)
        equivalence (acq_variable(466),acq_other_net_deferrals)
        equivalence (acq_variable(467),acq_deferred_purchased_gas)
        equivalence (acq_variable(468),acq_gas_in_storage)
        equivalence (acq_variable(469),acq_materials_n_supplies)
     equivalence (acq_variable(394),acq_cash_additions_2_fuel_inventory)
        equivalence (acq_variable(395),acq_expensing_fuel_inventory)
      equivalence (acq_variable(717),acq_cash_adds_2_matrials_inventory)
        equivalence (acq_variable(718),acq_expensing_matrials_inventory)
        equivalence (acq_variable(719),acq_cash_adds_2_gas_inventory)
        equivalence (acq_variable(720),acq_expensing_gas_inventory)
        equivalence (acq_variable(721),acq_cash_adds_2_inventories)
        equivalence (acq_variable(392),acq_class_deferred_fuel_balance)
        equivalence (acq_variable(75),acq_asset_nec)
        equivalence (acq_variable(76),acq_total_assets)
        equivalence (acq_variable(77),acq_common_stock_bal)
        equivalence (acq_variable(78),acq_retained_earnings_bal)
        equivalence (acq_variable(79),acq_common_equity_bal)
        equivalence (acq_variable(80),acq_preferred_stock_bal)
        equivalence (acq_variable(81),acq_long_term_debt_bal)
        equivalence (acq_variable(82),acq_total_capitial_bal)
        equivalence (acq_variable(403),acq_notes_payable_balance)
        equivalence (acq_variable(83),acq_other_lt_liability_bal)
        equivalence (acq_variable(358),acq_class_leased_nfce)
        equivalence (acq_variable(359),acq_nuclear_fuel_lease_payments)
        equivalence (acq_variable(84),acq_short_term_debt_bal)
        equivalence (acq_variable(85),acq_customer_deposits_balance)
        equivalence (acq_variable(86),acq_ciac_bal)
        equivalence (acq_variable(411),acq_class_pension_liability)
      equivalence (acq_variable(412),acq_class_deferred_gain_from_sales)
        equivalence (acq_variable(413),acq_class_storm_reserve_balance)
        equivalence (acq_variable(414),acq_class_vacation_pay_balance)
        equivalence (acq_variable(905),acq_class_execbenefits_balance)
        equivalence (acq_variable(906),acq_class_incentivecomp_balance)
        equivalence (acq_variable(87),acq_def_tax_cr_bal)
        equivalence (acq_variable(88),acq_def_itc_bal)
        equivalence (acq_variable(475),acq_prior_cause_of_itc_amort)
        equivalence (acq_variable(389),acq_class_accounts_payable)
        equivalence (acq_variable(406),acq_change_in_accounts_payable)
      equivalence (acq_variable(737),acq_exp_change_in_accounts_payable)
     equivalence (acq_variable(738),acq_wvpa_change_in_accounts_payable)
       equivalence (acq_variable(739),acq_production_change_in_payables)
        equivalence (acq_variable(89),acq_liabilities_nec)
        equivalence (acq_variable(90),acq_total_liabilities)
        equivalence (acq_variable(803),acq_assessedpensionopeb)
        equivalence (acq_variable(806),acq_execbenefitsnoncash)
        equivalence (acq_variable(807),acq_incentivecompensationnoncash)
        equivalence (acq_variable(907),acq_execbenefitscash)
        equivalence (acq_variable(908),acq_incentivecompensationcash)
        equivalence (acq_variable(104),acq_short_term_debt_issued)
       equivalence (acq_variable(105),acq_income_before_capital_service)
        equivalence (acq_variable(107),acq_dividend_from_subsidiary)
      equivalence (acq_variable(108),acq_tax_payments_2_parent_from_sub)
        equivalence (acq_variable(214),acq_income_taxes_consolidated)
        equivalence (acq_variable(215),acq_taxes_paid_by_parent)
        equivalence (acq_variable(125),acq_customer_deposits)
        equivalence (acq_variable(109),acq_funds_from_operations)
        equivalence (acq_variable(424),acq_vacation_pay_cash)
        equivalence (acq_variable(425),acq_pension_expense_cash)
        equivalence (acq_variable(426),acq_storm_expense_cash)
        equivalence (acq_variable(110),acq_fa_cash)
        equivalence (acq_variable(111),acq_nf_cash)
        equivalence (acq_variable(399),acq_new_sub_investment)
        equivalence (acq_variable(112),acq_investment_salvage_other)
     equivalence (acq_variable(115),acq_construction_and_net_investment)
        equivalence (acq_variable(116),acq_leased_nf_burn)
        equivalence (acq_variable(118),acq_change_in_working_capitial)
        equivalence (acq_variable(119),acq_long_term_debt_retirements)
        equivalence (acq_variable(154),acq_preferred_stock_retirements)
        equivalence (acq_variable(121),acq_change_in_funds_b4_financing)
        equivalence (acq_variable(122),acq_lease_receipts)
        equivalence (acq_variable(123),acq_external_financing_performed)
        equivalence (acq_variable(385),acq_ltd_ps_issue_expenses)
        equivalence (acq_variable(759),acq_cash_timing_adjustments)
 equivalence (acq_variable(386),acq_annual_cash_flow_timing_adjustments)
        equivalence (acq_variable(408),acq_income_tax_timing_adjs)
        equivalence (acq_variable(387),acq_ltd_interest_cash_payments)
        equivalence (acq_variable(388),acq_ps_dividend_cash_payments)
        equivalence (acq_variable(398),acq_common_stock_cash_dividends)
        equivalence (acq_variable(124),acq_change_in_cash)
        equivalence (acq_variable(220),acq_sl_tax_depreciation)
        equivalence (acq_variable(221),acq_annualized_ps_dividends)
        equivalence (acq_variable(222),acq_annualized_ltd_interest)
        equivalence (acq_variable(223),acq_embed_cost_capital_w_std)
        equivalence (acq_variable(224),acq_embed_cost_capital_wo_std)
      equivalence (acq_variable(396),acq_net_tax_embed_cost_captl_w_std)
     equivalence (acq_variable(397),acq_net_tax_embed_cost_captl_wo_std)
        equivalence (acq_variable(225),acq_annualized_std_interest)
        equivalence (acq_variable(226),acq_annualized_equity_cost)
        equivalence (acq_variable(228),acq_construction_tax_expense)
       ! duke catawba cp&l shares the catawba revenues and 
       ! expenses variables
       
        equivalence (acq_variable(239),acq_catawba_revenues)
      equivalence (acq_variable(240),acq_catawba_inc_statement_expenses)
        equivalence (acq_variable(306),acq_catawba_expenses)
        equivalence (acq_variable(241),acq_catawba_other_net_revenues)
        equivalence (acq_variable(242),acq_catawba_deferred_taxes_cr)
        equivalence (acq_variable(243),acq_catawba_deferred_debits)
        equivalence (acq_variable(244),acq_catawba_defer_taxes_bal_cr)
        equivalence (acq_variable(245),acq_catawba_total_cap_payments)
        equivalence (acq_variable(246),acq_catawba_class_btl_revenues)
        equivalence (acq_variable(247),acq_catawba_level_cap_payments)
        equivalence (acq_variable(248),acq_catawba_capacity_payments)
      equivalence (acq_variable(304),acq_catawba_tax_deductible_expense)
        equivalence (acq_variable(249),acq_nf_tax_depreciation)
        equivalence (acq_variable(250),acq_nf_entering_service)
        equivalence (acq_variable(251),acq_intra_company_nf_burn)
        equivalence (acq_variable(286),acq_retirement_medical_fund)
        equivalence (acq_variable(287),acq_retirement_med_payable)
        equivalence (acq_variable(288),acq_decom_fund_liability)
        equivalence (acq_variable(289),acq_lt_investments)
        equivalence (acq_variable(301),acq_total_capital_service)
        equivalence (acq_variable(303),acq_average_stock_price)
        equivalence (acq_variable(305),acq_other_taxable_income)
        equivalence (acq_variable(315),acq_taxes_non_income)
        equivalence (acq_variable(351),acq_amortization_b4_cap_service)
        equivalence (acq_variable(353),acq_btl_lease_amortization)
        equivalence (acq_variable(356),acq_salvage_transactions)

        ! ratios

        equivalence (acq_variable(364),acq_preferred_stock_in_cap)
        equivalence (acq_variable(365),acq_long_term_debt_in_cap)
        equivalence (acq_variable(366),acq_common_equity_bal_in_cap)
        equivalence (acq_variable(367),acq_short_term_debt_in_cap)
        equivalence (acq_variable(368),acq_funds_4_construction) ! 368
        equivalence (acq_variable(428),acq_trans_nuke_fuel_bal)

        ! property tax
        equivalence (acq_variable(429),acq_prop_tax_value_b4_exclusion)
        equivalence (acq_variable(430),acq_prop_tax_addendum)
        equivalence (acq_variable(431),acq_prop_tax_value_exclusion)
        equivalence (acq_variable(432),acq_prop_tax_from_prior_level)
        equivalence (acq_variable(433),acq_prop_tax_rate)
        equivalence (acq_variable(434),acq_property_value)
       equivalence (acq_variable(435),acq_property_taxes_based_on_value)
        equivalence (acq_variable(436),acq_cash_2_lt_investments)
        equivalence (acq_variable(441),acq_output_class_id)
        equivalence (acq_variable(442),acq_change_interest_divs_accrued)
        equivalence (acq_variable(443),acq_state_taxes_accrual_adj)
        equivalence (acq_variable(444),acq_federal_taxes_accrual_adj)
        equivalence (acq_variable(445),acq_non_income_taxes_accrual_adj)
      equivalence (acq_variable(446),acq_funds_consolidating_adjustment)
        equivalence (acq_variable(447),acq_intra_taxes_payable_output)
        equivalence (acq_variable(627),acq_std_interest_rate_used)
        equivalence (acq_variable(628),acq_unamort_debt_interest_bal)
        equivalence (acq_variable(629),acq_unamort_debit_interest_bal)
      equivalence (acq_variable(630),acq_interest_amort_from_debit_file)
        equivalence (acq_variable(631),acq_icap_revenues)
        
        ! ratios
        equivalence (acq_variable(632),acq_gross_cash_flow)
        equivalence (acq_variable(633),acq_gross_cash_2_debt)
        equivalence (acq_variable(634),acq_total_debt)
        equivalence (acq_variable(635),acq_ebitda_2_debt)
        equivalence (acq_variable(667),acq_ebit_per_ave_share)
        ! earnings bf interest, taxes, depreciation
        equivalence (acq_variable(479),acq_ebitda) 
       equivalence (acq_variable(668),acq_gross_cash_flow_per_ave_share)
        equivalence (acq_variable(669),acq_cbidt)
        equivalence (acq_variable(670),acq_cbidt_per_ave_share)
         equivalence (acq_variable(947),acq_incderivcapacitysales )
         
         equivalence (acq_variable(948),acq_incderivcapacitypurchases )
        
        ! (variable(),), ! 381 std_interest_rate_used
        
        ! sbu variables
        
        ! sbu variables that are in the output data set
        
       real (kind=4), save :: acq_sbu_variables(0:acq_variable_number-1)
        ! save sbu_variables
         real (kind=4), save :: acq_sbu_deferred_taxes_cr
         
         
       real(kind=4), save :: acq_sbu_def_tax_cr_balance
       real(kind=4), save :: acq_sbu_def_itc_balance
       real(kind=4), save :: acq_sbu_class_nuclear_fuel_expense
       real(kind=4), save :: acq_sbu_owned_nf_burn
       real(kind=4), save :: acq_sbu_nuc_fuel_owned_burn
       real(kind=4), save :: acq_sbu_nuc_fuel_leased_burn
       real(kind=4), save :: acq_sbu_nuc_fuel_total_burn
       real(kind=4), save :: acq_sbu_nf_non_cash_expense
       real(kind=4), save :: acq_sbu_doe_nuc_fuel_fee
       real(kind=4), save :: acq_sbu_nuc_decommissioning_cost
       real(kind=4), save :: acq_sbu_net_of_tax_nucl_fund_return
       real(kind=4), save :: acq_sbu_nf_tax_expense
       real(kind=4), save :: acq_sbu_nf_sl_def_tax_dep
       real(kind=4), save :: acq_sbu_nf_cash_expense
       real(kind=4), save :: acq_sbu_nf_deferred_tax_basis
       real(kind=4), save :: acq_sbu_fa_cash
       real(kind=4), save :: acq_sbu_nf_cash
       real(kind=4), save :: acq_sbu_capitialized_lease_addition
       real(kind=4), save :: acq_sbu_other_revenues
       real(kind=4), save :: acq_sbu_retirement_medical_fund
       real(kind=4), save :: acq_sbu_retirement_med_payable
       real(kind=4), save :: acq_sbu_decom_fund_liability
       real(kind=4), save :: acq_sbu_lt_investments
       real(kind=4), save :: acq_sbu_retirement_medical_payments
       real(kind=4), save :: acq_sbu_btl_def_taxes_cr
       real(kind=4), save :: acq_sbu_btl_def_taxes_dr
       real(kind=4), save :: acq_sbu_unamortized_interest_bal
       real(kind=4), save :: acq_sbu_unamortized_issue_expense_bal
       real(kind=4), save :: acq_sbu_ending_nf_rate_base
       real(kind=4), save :: acq_sbu_ending_cwip_rate_base
       real(kind=4), save :: acq_sbu_unamort_debt_interest_bal
       real(kind=4), save :: acq_sbu_unamort_debit_interest_bal
       equivalence (acq_sbu_variables(3), acq_sbu_other_revenues)
         
 equivalence (acq_sbu_variables(16), acq_sbu_class_nuclear_fuel_expense)
        equivalence (acq_sbu_variables(682), acq_sbu_deferred_taxes_cr)
        equivalence (acq_sbu_variables(87), acq_sbu_def_tax_cr_balance)
        equivalence (acq_sbu_variables(88), acq_sbu_def_itc_balance)
        equivalence (acq_sbu_variables(110), acq_sbu_fa_cash)
        equivalence (acq_sbu_variables(111), acq_sbu_nf_cash)
        equivalence (acq_sbu_variables(114), &
            acq_sbu_capitialized_lease_addition)
        equivalence (acq_sbu_variables(219), acq_sbu_owned_nf_burn)
       equivalence (acq_sbu_variables(229), acq_sbu_nuc_fuel_owned_burn)
      equivalence (acq_sbu_variables(230), acq_sbu_nuc_fuel_leased_burn)
       equivalence (acq_sbu_variables(234), acq_sbu_nuc_fuel_total_burn)
       equivalence (acq_sbu_variables(231), acq_sbu_nf_non_cash_expense)
        equivalence (acq_sbu_variables(232), acq_sbu_doe_nuc_fuel_fee)
  equivalence (acq_sbu_variables(233), acq_sbu_nuc_decommissioning_cost)
        equivalence (acq_sbu_variables(235), &
            acq_sbu_net_of_tax_nucl_fund_return)
        equivalence (acq_sbu_variables(227), acq_sbu_nf_tax_expense)
        equivalence (acq_sbu_variables(236), acq_sbu_nf_sl_def_tax_dep)
        equivalence (acq_sbu_variables(237), acq_sbu_nf_cash_expense)
     equivalence (acq_sbu_variables(238), acq_sbu_nf_deferred_tax_basis)
   equivalence (acq_sbu_variables(286), acq_sbu_retirement_medical_fund)
    equivalence (acq_sbu_variables(287), acq_sbu_retirement_med_payable)
      equivalence (acq_sbu_variables(288), acq_sbu_decom_fund_liability)
        equivalence (acq_sbu_variables(289), acq_sbu_lt_investments)
        equivalence (acq_sbu_variables(290), &
            acq_sbu_retirement_medical_payments)
        equivalence (acq_sbu_variables(902), acq_sbu_btl_def_taxes_cr)
        equivalence (acq_sbu_variables(903), acq_sbu_btl_def_taxes_dr)
  equivalence (acq_sbu_variables(339), acq_sbu_unamortized_interest_bal)
        equivalence (acq_sbu_variables(743), &
            acq_sbu_unamortized_issue_expense_bal)
       equivalence (acq_sbu_variables(369), acq_sbu_ending_nf_rate_base)
     equivalence (acq_sbu_variables(370), acq_sbu_ending_cwip_rate_base)
 equivalence (acq_sbu_variables(628), acq_sbu_unamort_debt_interest_bal)
equivalence (acq_sbu_variables(629), acq_sbu_unamort_debit_interest_bal)

end module ac_equiv