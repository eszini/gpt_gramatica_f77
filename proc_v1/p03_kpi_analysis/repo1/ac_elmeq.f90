module ac_elmeq
    implicit none



    real:: acl_adjustment_clause_revenues
    ! to members.

    real:: acl_base_rates_revenues
    real (kind=4) :: acl_variable(0:500)
    real:: acl_secondary_sales_revenues
    real:: acl_revenues_other
    real:: acl_reference_base_revenues
    real:: acl_operating_method_adjustment
    real:: acl_revenues_operating
    ! cash expenses
    real:: acl_fuel_expense
    real:: acl_purchase_power_expense
    real:: acl_variable_expense
    real:: acl_fixed_expense
    real:: acl_service_transaction_expense
    real:: acl_other_expense_1
    real:: acl_other_expense_2
    real:: acl_other_expense_3
    real:: acl_atl_lease_payments
    real:: acl_dsm_expense
    real:: acl_dsm_rebate
    real:: acl_emission_credit_expense
    real:: acl_class_nuclear_fuel_expense
    real:: acl_owned_nf_burn
    real:: acl_book_depreciation
    real:: acl_amortization_expense
    real:: acl_ciac_amortization
    real:: acl_total_expenses_b4_taxes

    ! tax items

    real:: acl_operating_revenue_tax
    real:: acl_other_and_environmental_taxes
    real:: acl_environmental_tax
    real:: acl_class_property_taxes
    real:: acl_other_taxes
    real:: acl_local_taxes
    real:: acl_atl_state_taxes
    real:: acl_state_tax_on_capital
    real:: acl_atl_federal_taxes
    real:: acl_federal_tax_on_capital
    real:: acl_deferred_taxes_dr
    real:: acl_deferred_taxes_cr
    real:: acl_tax_credits
    real:: acl_total_expense_and_taxes
    real:: acl_operating_income
    real:: acl_deferred_revenues
    real:: acl_class_btl_revenues
    real:: acl_investment_income
    real:: acl_class_btl_expenses
    real:: acl_btl_income_taxes
    real:: acl_afudc_equity
    real:: acl_income_b4_interest
    real:: acl_interest_on_long_term_debt
    real:: acl_std_interest
    real:: acl_afudc_borrowed
    real:: acl_income_after_interest
    real:: acl_subsidiary_income
    real:: acl_net_of_tax_exexp
    real:: acl_net_profit_or_loss
    real:: acl_preferred_dividends
    real:: acl_earnings_available_to_common
    real:: acl_common_dividends
    real:: acl_retained_earnings
    real:: acl_stock_book_value
    real:: acl_stock_market_price
    real:: acl_average_shares
    real:: acl_market_2_book
    real:: acl_earnings_per_share
    real:: acl_dividend_per_share
    real:: acl_dividend_payout
    real:: acl_return_on_equity
    real:: acl_average_equity

    ! tax stuff

    real:: acl_taxable_income
    real:: acl_capitialized_interest
    real:: acl_income_tax_depreciation
    real:: acl_state_taxable_income
    real:: acl_state_taxable_income_after_nols
    real:: acl_state_income_tax_deductions
    real:: acl_adj_2_state_taxes
    real:: acl_state_taxes
    real:: acl_federal_taxable_income
    real:: acl_fed_taxable_income_after_nols
    real:: acl_fed_income_tax_deductions
    real:: acl_adj_2_federal_taxes
    real:: acl_federal_taxes
    real:: acl_amtincome
    real:: acl_total_income_taxes
    real:: acl_state_tax_rate
    real:: acl_federal_income_tax_rate
    real:: acl_tax_payments_2_parent_from_sub
    real:: acl_taxable_income_b4_deductions
    real:: acl_tax_deductible_expenses_book

    ! ratebase items

    real:: acl_return_on_ratebase
    real:: acl_asset_class_rate_base
    real:: acl_rate_base_plant_in_service
    real:: acl_rate_base_def_taxes
    real:: acl_addendum_2_rate_base
    real:: acl_rate_base_deferred_debits
    real:: acl_rate_base_nuc_fuel
    real:: acl_rate_base_cwip
    real:: acl_rate_base_def_itc
    real:: acl_rate_base_nuc_decom
    real:: acl_rate_base_ciac
    real:: acl_rate_base_asset_nec
    real:: acl_rate_base_capitialized_leases
    ! balance sheet assets (63-100
    real:: acl_gross_plant_value
    real:: acl_cwip
    real:: acl_gross_plant_value_plus_cwip
    real:: acl_cumulative_depreciation
    real:: acl_net_nf_value
    real:: acl_utility_plant_net
    real:: acl_assets_other
    real:: acl_subsidiary_investment
    real:: acl_leases_capitialzied
    real:: acl_decommisioning_fund_bal
    real:: acl_dd_balance_net
    real:: acl_deferred_revenues_cum
    real:: acl_deferred_taxes_dr_bal
    real:: acl_cash_balance
    real:: acl_asset_nec
    real:: acl_total_assets
    real:: acl_common_stock_bal
    real:: acl_retained_earnings_bal
    real:: acl_common_equity_bal
    real:: acl_preferred_stock_bal
    real:: acl_long_term_debt_bal
    real:: acl_total_capitial_bal
    real:: acl_other_lt_liability_bal
    real:: acl_short_term_debt_bal
    real:: acl_customer_deposits_balance
    real:: acl_ciac_bal
    real:: acl_def_tax_cr_bal
    real:: acl_def_itc_bal
    real:: acl_liabilities_nec
    real:: acl_total_liabilities
    real:: acl_common_stock_issued
    real:: acl_long_term_debt_issued
    real:: acl_preferred_stock_issued
    real:: acl_short_term_debt_issued
    real:: acl_income_before_capital_service
    real:: acl_cash_from_sale_of_assets
    real:: acl_dividend_from_subsidiary
    real:: acl_income_taxes_consolidated
    real:: acl_taxes_paid_by_parent
    real:: acl_ciac_cash
    real:: acl_customer_deposits
    real:: acl_funds_from_operations
    real:: acl_fa_cash
    real:: acl_nf_cash
    real:: acl_new_subsidiary_investment
    real:: acl_investment_salvage_other
    real:: acl_capitialized_lease_additions
    real:: acl_construction_and_net_investment
    real:: acl_leased_nf_burn
    real:: acl_nucl_decom_fund
    real:: acl_change_in_working_capitial
    real:: acl_long_term_debt_retirements
    real:: acl_preferred_stock_retirements
    real:: acl_common_stock_bought
    real:: acl_deferred_expense_cash
    real:: acl_change_in_funds_b4_financing
    real:: acl_lease_receipts
    real:: acl_external_financing_performed
    real:: acl_change_in_cash
    real:: acl_elimination_cash_adjustment
    real (kind=4) :: acl_atl_amortization,acl_btl_amortization
    real (kind=4) :: acl_ltd_interest_and_amortization, &
        acl_btl_def_taxes
    real (kind=4) :: acl_total_def_taxes_cr
    real (kind=4) :: acl_net_derivatives_income

    equivalence (acl_variable(0),acl_base_rates_revenues)

    equivalence(acl_variable(1), acl_adjustment_clause_revenues)

    equivalence(acl_variable(2),acl_secondary_sales_revenues)
    equivalence(acl_variable(3),acl_revenues_other)


    equivalence (acl_variable(252),acl_reference_base_revenues)

    equivalence(acl_variable(253), acl_operating_method_adjustment)
    equivalence(acl_variable(4),acl_revenues_operating)
    ! h expenses
    equivalence(acl_variable(5),acl_fuel_expense)
    equivalence(acl_variable(6),acl_purchase_power_expense)
    equivalence(acl_variable(7),acl_variable_expense)
    equivalence(acl_variable(8),acl_fixed_expense)
    equivalence(acl_variable(9),acl_service_transaction_expense)
    equivalence(acl_variable(10),acl_other_expense_1)
    equivalence(acl_variable(11),acl_other_expense_2)
    equivalence(acl_variable(12),acl_other_expense_3)
    equivalence(acl_variable(60),acl_atl_lease_payments)
    equivalence(acl_variable(13),acl_dsm_expense)
    equivalence(acl_variable(14),acl_dsm_rebate)
    equivalence(acl_variable(15),acl_emission_credit_expense)
    equivalence(acl_variable(16),acl_class_nuclear_fuel_expense)
    equivalence(acl_variable(116),acl_leased_nf_burn)
    equivalence(acl_variable(219),acl_owned_nf_burn)
    equivalence(acl_variable(17),acl_book_depreciation)
    equivalence(acl_variable(18),acl_amortization_expense)
    equivalence(acl_variable(19),acl_ciac_amortization)
    equivalence(acl_variable(20),acl_total_expenses_b4_taxes)

    ! items
    equivalence(acl_variable(21),acl_operating_revenue_tax)
    equivalence(acl_variable(22),acl_other_and_environmental_taxes)
    equivalence(acl_variable(216),acl_environmental_tax)
    equivalence(acl_variable(217),acl_class_property_taxes)
    equivalence(acl_variable(218),acl_other_taxes)
    equivalence(acl_variable(23),acl_local_taxes)
    equivalence(acl_variable(24),acl_atl_state_taxes)
    equivalence(acl_variable(25),acl_state_tax_on_capital)
    equivalence (acl_variable(26),acl_atl_federal_taxes)
    equivalence(acl_variable(27),acl_federal_tax_on_capital)
    equivalence (acl_variable(28),acl_deferred_taxes_dr)
    equivalence (acl_variable(29),acl_deferred_taxes_cr)
    equivalence (acl_variable(50),acl_tax_credits)
    equivalence(acl_variable(30),acl_total_expense_and_taxes)
    equivalence (acl_variable(31),acl_operating_income)
    equivalence (acl_variable(32),acl_deferred_revenues)
    equivalence (acl_variable(33),acl_class_btl_revenues)
    equivalence (acl_variable(34),acl_investment_income)
    equivalence (acl_variable(35),acl_class_btl_expenses)
    equivalence (acl_variable(36),acl_btl_income_taxes)
    equivalence (acl_variable(37),acl_afudc_equity)
    equivalence (acl_variable(38),acl_income_b4_interest)
    equivalence (acl_variable(39),acl_interest_on_long_term_debt)
    equivalence (acl_variable(40),acl_std_interest)
    equivalence (acl_variable(41),acl_afudc_borrowed)
    equivalence (acl_variable(42),acl_income_after_interest)
    equivalence (acl_variable(43),acl_subsidiary_income)
    equivalence (acl_variable(44),acl_net_of_tax_exexp)
    equivalence (acl_variable(45),acl_net_profit_or_loss)
    equivalence (acl_variable(46),acl_preferred_dividends)
    equivalence (acl_variable(47),acl_earnings_available_to_common)
    equivalence (acl_variable(48),acl_common_dividends)
    equivalence (acl_variable(49),acl_retained_earnings)
    equivalence (acl_variable(51),acl_stock_book_value)
    equivalence (acl_variable(52),acl_stock_market_price)
    equivalence (acl_variable(53),acl_average_shares)
    equivalence (acl_variable(54),acl_market_2_book)
    equivalence (acl_variable(55),acl_earnings_per_share)
    equivalence (acl_variable(56),acl_dividend_per_share)
    equivalence (acl_variable(57),acl_dividend_payout)
    equivalence (acl_variable(58),acl_return_on_equity)
    equivalence (acl_variable(59),acl_average_equity)

    ! stuff (really -- stuff?)

    equivalence (acl_variable(128),acl_taxable_income)
    equivalence (acl_variable(129),acl_capitialized_interest)
    equivalence (acl_variable(130),acl_income_tax_depreciation)
    equivalence (acl_variable(131),acl_state_taxable_income)
    equivalence (acl_variable(132),acl_state_taxable_income_after_nols)
    equivalence (acl_variable(133),acl_state_income_tax_deductions)
    equivalence (acl_variable(134),acl_adj_2_state_taxes)
    equivalence (acl_variable(212),acl_state_taxes)
    equivalence (acl_variable(135),acl_federal_taxable_income)
    equivalence (acl_variable(136), acl_fed_taxable_income_after_nols)
    equivalence (acl_variable(137),acl_fed_income_tax_deductions)
    equivalence (acl_variable(138),acl_adj_2_federal_taxes)
    equivalence (acl_variable(211),acl_federal_taxes)
    equivalence (acl_variable(139),acl_amtincome)
    equivalence (acl_variable(140),acl_total_income_taxes)
    equivalence (acl_variable(141),acl_state_tax_rate)
    equivalence (acl_variable(213),acl_federal_income_tax_rate)
    equivalence (acl_variable(209),acl_taxable_income_b4_deductions)
    equivalence (acl_variable(210),acl_tax_deductible_expenses_book)
    equivalence (acl_variable(61),acl_return_on_ratebase)
    equivalence (acl_variable(62),acl_asset_class_rate_base)
    equivalence (acl_variable(143),acl_rate_base_plant_in_service)
    equivalence (acl_variable(144),acl_rate_base_def_taxes)
    equivalence (acl_variable(145),acl_addendum_2_rate_base)
    equivalence (acl_variable(146),acl_rate_base_deferred_debits)
    equivalence (acl_variable(147),acl_rate_base_nuc_fuel)
    equivalence (acl_variable(148),acl_rate_base_cwip)
    equivalence (acl_variable(149),acl_rate_base_def_itc)
    equivalence (acl_variable(150),acl_rate_base_nuc_decom)
    equivalence (acl_variable(151),acl_rate_base_ciac)
    equivalence (acl_variable(152),acl_rate_base_asset_nec)
    equivalence (acl_variable(153), acl_rate_base_capitialized_leases)
    ! balance sheet assets (63-100)
    equivalence (acl_variable(63),acl_gross_plant_value)
    equivalence (acl_variable(64),acl_cwip)
    equivalence (acl_variable(65),acl_gross_plant_value_plus_cwip)
    equivalence (acl_variable(66),acl_cumulative_depreciation)
    equivalence (acl_variable(67),acl_net_nf_value)
    equivalence (acl_variable(68),acl_utility_plant_net)
    equivalence (acl_variable(69),acl_assets_other)
    equivalence (acl_variable(91),acl_subsidiary_investment)
    equivalence (acl_variable(92),acl_leases_capitialzied)
    equivalence (acl_variable(70),acl_decommisioning_fund_bal)
    equivalence (acl_variable(71),acl_dd_balance_net)
    equivalence (acl_variable(72),acl_deferred_revenues_cum)
    equivalence (acl_variable(73),acl_deferred_taxes_dr_bal)
    equivalence (acl_variable(74),acl_cash_balance)
    equivalence (acl_variable(75),acl_asset_nec)
    equivalence (acl_variable(76),acl_total_assets)
    equivalence (acl_variable(77),acl_common_stock_bal)
    equivalence (acl_variable(78),acl_retained_earnings_bal)
    equivalence (acl_variable(79),acl_common_equity_bal)
    equivalence (acl_variable(80),acl_preferred_stock_bal)
    equivalence (acl_variable(81),acl_long_term_debt_bal)
    equivalence (acl_variable(82),acl_total_capitial_bal)
    equivalence (acl_variable(83),acl_other_lt_liability_bal)
    equivalence (acl_variable(84),acl_short_term_debt_bal)
    equivalence (acl_variable(85),acl_customer_deposits_balance)
    equivalence (acl_variable(86),acl_ciac_bal)
    equivalence (acl_variable(87),acl_def_tax_cr_bal)
    equivalence (acl_variable(88),acl_def_itc_bal)
    equivalence (acl_variable(89),acl_liabilities_nec)
    equivalence (acl_variable(90),acl_total_liabilities)
    equivalence (acl_variable(101),acl_common_stock_issued)
    equivalence (acl_variable(102),acl_long_term_debt_issued)
    equivalence (acl_variable(103),acl_preferred_stock_issued)
    equivalence (acl_variable(104),acl_short_term_debt_issued)
    equivalence (acl_variable(105), acl_income_before_capital_service)
    equivalence (acl_variable(106),acl_cash_from_sale_of_assets)
    equivalence (acl_variable(107),acl_dividend_from_subsidiary)
    equivalence (acl_variable(108), acl_tax_payments_2_parent_from_sub)
    equivalence (acl_variable(214),acl_income_taxes_consolidated)
    equivalence (acl_variable(215),acl_taxes_paid_by_parent)
    equivalence (acl_variable(113),acl_ciac_cash)
    equivalence (acl_variable(125),acl_customer_deposits)
    equivalence (acl_variable(109),acl_funds_from_operations)
    equivalence (acl_variable(142),acl_elimination_cash_adjustment)
    equivalence (acl_variable(110),acl_fa_cash)
    equivalence (acl_variable(111),acl_nf_cash)
    equivalence (acl_variable(126),acl_new_subsidiary_investment)
    equivalence (acl_variable(112),acl_investment_salvage_other)
    equivalence (acl_variable(114),acl_capitialized_lease_additions)
    equivalence (acl_variable(115), acl_construction_and_net_investment)
    equivalence (acl_variable(117),acl_nucl_decom_fund)
    equivalence (acl_variable(118),acl_change_in_working_capitial)
    equivalence (acl_variable(119),acl_long_term_debt_retirements)
    equivalence (acl_variable(154),acl_preferred_stock_retirements)
    equivalence (acl_variable(120),acl_common_stock_bought)
    equivalence (acl_variable(127),acl_deferred_expense_cash)
    equivalence (acl_variable(121),acl_change_in_funds_b4_financing)
    equivalence (acl_variable(122),acl_lease_receipts)
    equivalence (acl_variable(123),acl_external_financing_performed)
    equivalence (acl_variable(124),acl_change_in_cash)
    equivalence (acl_variable(291),acl_atl_amortization)
    equivalence (acl_variable(292),acl_btl_amortization)
    equivalence (acl_variable(293), acl_ltd_interest_and_amortization)
    equivalence (acl_variable(294),acl_btl_def_taxes)
    equivalence (acl_variable(295),acl_total_def_taxes_cr)
    equivalence (acl_variable(448),acl_net_derivatives_income)
end module ac_elmeq
