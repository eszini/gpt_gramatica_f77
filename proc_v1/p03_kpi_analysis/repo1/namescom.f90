module namescom
!     ******************************************************************
!     Namescom.mon
!     Copyright(c) DrG Solutions 2011
!
!     Created: 7/2/2011 6:23:20 PM
!     Author : Mark S Gerber
!     Last change: MSG 11/2/2011 12:01:16 PM
!     ******************************************************************

!
! DRILLING REPORT NAMES
!

      character(len=1),parameter:: BALANCE_SHEET_ITEM='B'
      character(len=1),parameter:: INCOME_REPORT_ITEM='I'
      character(len=1),parameter:: CASH_REPORT_ITEM='C'
      character(len=1),parameter:: TAX_REPORT_ITEM='T'
      character(len=1),parameter:: BALANCE_SHEET_ANNUAL_ITEM='A'

!
! NOTE IF THE EXPENSE FOR REVENUE IS IN THE EXPENSE FILE PUT THE NAME
! HERE
!
      integer(kind=2),parameter:: ODEC_NA3_ID=401 ! ,BTL=2,ATL=1,Reg=0
      integer(kind=2),parameter:: LAST_INCOME_LINE=80 !30)
      integer(kind=4),parameter:: EXP_OFFSET_LINE=30
!
! REVENUES TYPES
!
      integer(kind=2),parameter:: BaseRates=1
      
      integer(kind=2),parameter:: SecondarySales=3
      integer(kind=2),parameter:: OtherRevenue=4
      integer(kind=2),parameter:: BTLRevenues=5
      integer(kind=2),parameter:: CatawbaRevenues=6
      integer(kind=2),parameter:: GasRevenues=7
      integer(kind=2),parameter:: UnbilledRevenues=8
      integer(kind=2),parameter:: DeferredRevenues=9
      integer(kind=2),parameter:: RelationshipRevenues=10
      integer(kind=2),parameter:: Customer_Revenues=10
      integer(kind=2),parameter:: Residential=11
      integer(kind=2),parameter:: Commercial=12
      integer(kind=2),parameter:: Industrial=13
      integer(kind=2),parameter:: Lighting=14
      integer(kind=2),parameter:: BulkPower=15
      integer(kind=2),parameter:: NetofTaxBTLRevenues=16
      integer(kind=2),parameter:: CapacitySales=17
      integer(kind=2),parameter:: Government=18
      integer(kind=2),parameter:: PGAAdjustment=19
      integer(kind=2),parameter:: CompetitiveSales=20
      integer(kind=2),parameter:: UtilitySales=21
      integer(kind=2),parameter:: Prior_Years_Method_Adjustment=26
      integer(kind=2),parameter:: Prior_level_Method_Adjustment=27
      integer(kind=2),parameter:: Operating_MethodAdjustment=28
      integer(kind=2),parameter:: Total_Base_Revenues=29
      integer(kind=2),parameter:: TOTAL_OPERATING_REVENUES=30
      integer(kind=2),parameter:: GasWholesaleRevenues=31
      integer(kind=2),parameter:: DerivativePhysicalRevVariable=32
      integer(kind=2),parameter:: DerivativePhysicalRevFixed=33
      integer(kind=2),parameter:: DerivativeFinancialRevVariable=34
      integer(kind=2),parameter:: DerivativeFinancialRevFixed=35
      integer(kind=2),parameter:: PayrollTaxes=65
      integer(kind=2),parameter:: MarkToMarket=66
      integer(kind=2),parameter:: AFUDCEquity=67
      integer(kind=2),parameter:: AFUDCBorrowed=68
      integer(kind=2),parameter:: PCABTLRevenues=69
!
! reused variable for FE - All variables were changed to eliminate
!                          in-between blanks Not used in any file
!
      integer(kind=2),parameter:: Associated_Company_Interest=7
      integer(kind=2),parameter:: PJM_MISO_Retail_Transmission=10
      integer(kind=2),parameter:: PJM_MISO_FES_Transmission=19
      integer(kind=2),parameter:: Associated_Company=31
      integer(kind=2),parameter:: PJM_MISO_Revenues=36
!
! new FE variables  - All variables were changed to eliminate 
!                     in-between blanks Not used in any file
!
      integer(kind=2),parameter:: Forfeited_Discounts_Rents=37
      integer(kind=2),parameter:: Miscellaneous_Service_Revenues=38
      integer(kind=2),parameter:: Retail_Generation=39
      integer(kind=2),parameter:: Wholesale_Generation=40
      integer(kind=2),parameter:: Energy_Efficiency=41
      integer(kind=2),parameter:: Net_Interco_Sale_Leaseback=42
      integer(kind=2),parameter:: Transitions=43
      integer(kind=2),parameter:: SLB_Wholesale_Associated_Co=44
      integer(kind=2),parameter:: PJM_Contra=45
      integer(kind=2),parameter:: Interest_Dividend_Income=46    
!
! EXPENSE TYPES
!
      integer(kind=2),parameter:: LAST_EXPENSE_ITEM=100
      integer(kind=2),parameter:: Total_ExpensesB4_Taxes=1
      integer(kind=2),parameter:: BTLDecommissioningLiability=8
      integer(kind=2),parameter:: Exp_Collection_in_Adj_Clause=9
      integer(kind=2),parameter:: BTLLeaseCash=10
      integer(kind=2),parameter:: FossilFuel=11
      integer(kind=2),parameter:: PurchasedPower=12
      integer(kind=2),parameter:: VariableOandM=13
      integer(kind=2),parameter:: FixedOandM=14
      integer(kind=2),parameter:: OtherOandM=15
      integer(kind=2),parameter:: PurchasedGas=16
      integer(kind=2),parameter:: Purchased_Capacity_to_Level_RM=16
      integer(kind=2),parameter:: Other=17
      integer(kind=2),parameter:: OwnedNuclearFuel=18
      integer(kind=2),parameter:: LeasedNuclearFuel=19
      integer(kind=2),parameter:: TotalNuclearFuel=19
      integer(kind=2),parameter:: DSMExpense=20
      integer(kind=2),parameter:: DSMRebate=21
      integer(kind=2),parameter:: ATLBookLeaseExpense=22
      integer(kind=2),parameter:: ServiceTransactions=23
      integer(kind=2),parameter:: EmissionCredits=24
      integer(kind=2),parameter:: DOEDecommissioning=25
      integer(kind=2),parameter:: DOEDisposal=26
      integer(kind=2),parameter:: CatawbaExpenses=27
      integer(kind=2),parameter:: BTLExpenses=28
      integer(kind=2),parameter:: TransmissionOperation=29
      integer(kind=2),parameter:: TransmissionMaintenance=30
      integer(kind=2),parameter:: DistributionOperation=31
      integer(kind=2),parameter:: DistributionMaintenance=32
      integer(kind=2),parameter:: CustomerAccounts=33
      integer(kind=2),parameter:: CustomerServices=34
      integer(kind=2),parameter:: SalesExpense=35
      integer(kind=2),parameter:: AGOperations=36
      integer(kind=2),parameter:: AGMaintenance=37
      integer(kind=2),parameter:: Amortization=38
      integer(kind=2),parameter:: DeferredRevenueAmortization=39
      integer(kind=2),parameter:: ATLLeaseAmortization=40
      integer(kind=2),parameter:: BTLLeaseAmortization=41
      integer(kind=2),parameter:: BookDepreciation=42
      integer(kind=2),parameter:: BTLAmortization=43
      integer(kind=2),parameter:: BTLLeaseInterest=44
      integer(kind=2),parameter:: ATLLeaseInterest=45
      integer(kind=2),parameter:: DeferredFuel=46
      integer(kind=2),parameter:: VacationPay=47
      integer(kind=2),parameter:: PensionExpense=48
      integer(kind=2),parameter:: UnfundedPensionExpense=56
      integer(kind=2),parameter:: STORMExpense=49
      integer(kind=2),parameter:: STDInterest=50
      integer(kind=2),parameter:: LTDInterest=51
      integer(kind=2),parameter:: PreferredDividends=69
      integer(kind=2),parameter:: DerivativePhysicalExpVariable=52
      integer(kind=2),parameter:: DerivativePhysicalExpFixed=53
      integer(kind=2),parameter:: DerivativeFinancialExpVariable=54
      integer(kind=2),parameter:: DerivativeFinancialExpFixed=55
      integer(kind=2),parameter:: WVPAMemberPurchases=60
      integer(kind=2),parameter:: WVPANonMemberPurchases=57
      integer(kind=2),parameter:: WVPAMemberServices=58
      integer(kind=2),parameter:: WVPANonMemberServices=59
      integer(kind=2),parameter:: CashDerativePhysicalExpVar=83
      integer(kind=2),parameter:: CashDerativePhysicalExpFixed=84
      integer(kind=2),parameter:: CashDerativeFinancialExpVar=85
      integer(kind=2),parameter:: CashDerativeFinancialExpFixed=86
      integer(kind=2),parameter:: CashWVPAMemberPurchases=87
      integer(kind=2),parameter:: CashWVPANonMemberPurchases=88
      integer(kind=2),parameter:: CashWVPAMemberServices=89
      integer(kind=2),parameter:: CashWVPANonMemberServices=90
!
! TAX NAMES
!
      integer(kind=2),parameter:: Itemistaxed=1
      integer(kind=2),parameter:: Itemisnottaxed=2
      integer(kind=2),parameter:: Both=1
      integer(kind=2),parameter:: Federal=2
      integer(kind=2),parameter:: State=3
      integer(kind=2),parameter:: LAST_TAX_NOL_ITEM=6
      integer(kind=2),parameter:: Federal_NOLs=35
      integer(kind=2),parameter:: AMT_NOLs=36
      integer(kind=2),parameter:: General_Carry_Tax_Credits=37
      integer(kind=2),parameter:: Sec_42_Carry_Tax_Credits=38
      integer(kind=2),parameter:: State_NOLs=39
      integer(kind=2),parameter:: State_Credits_Carried_Forward=40
      integer(kind=2),parameter:: LAST_TAX_ITEM=50
      integer(kind=2),parameter:: OtherTaxes=1
      integer(kind=2),parameter:: PropertyTaxes=2
      integer(kind=2),parameter:: Revenue_Taxes=3
      integer(kind=2),parameter:: Income_Tax_Credits=4
      integer(kind=2),parameter:: Income_Tax_Adjustments=5
      integer(kind=2),parameter:: M1_Additions=6
      integer(kind=2),parameter:: M1_Deductions=7
      integer(kind=2),parameter:: Deferred_TaxesCr=8
      integer(kind=2),parameter:: Deferred_TaxesDr=9
      integer(kind=2),parameter:: ITCAmortization=10
      integer(kind=2),parameter:: BTL_Tax_Deductions=11
      integer(kind=2),parameter:: Tax_Depreciation=12
      integer(kind=2),parameter:: AMT_ACE_Tax_Depreciation=13
      integer(kind=2),parameter:: AMT_Depreciation_Preference=14
      integer(kind=2),parameter:: Sec_29_Credits=15
      integer(kind=2),parameter:: Sec42_Credits=16
      integer(kind=2),parameter:: Federal_Income_Tax=17
      integer(kind=2),parameter:: ITC_AmortizationRate=18
      integer(kind=2),parameter:: Local_TaxRate=19
      integer(kind=2),parameter:: Operating_Revenue_Tax_Rate=20
      integer(kind=2),parameter:: Property_TaxRate=21
      integer(kind=2),parameter:: Other_Taxes_of_Revenues_Rate=22
      integer(kind=2),parameter:: Other_Taxes_of_Expenses_Rate=23
      integer(kind=2),parameter:: Federal_Capitial_Tax_Rate=24
      integer(kind=2),parameter:: Local_Capitial_Tax_Rate=25
      integer(kind=2),parameter:: Taxable_Income_Deductions_Rate=26
      integer(kind=2),parameter:: AMTIncome_Addendum=32
      integer(kind=2),parameter:: Addendum_Federal_Capitial_Tax=33
      integer(kind=2),parameter:: Addendum_Local_Capitial_Tax=34
      integer(kind=2),parameter:: Federal_Capitial_Tax_Deduction=40
      integer(kind=2),parameter:: Local_Capitial_Tax_Deduction=41
      integer(kind=2),parameter:: Temporary_ATL_Tax_Differences=42
      integer(kind=2),parameter:: Temporary_BTL_Tax_Differences=43
      integer(kind=2),parameter:: Permanent_ATL_Tax_Differences=44
      integer(kind=2),parameter:: Permanent_BTL_Tax_Differences=45
      integer(kind=2),parameter:: BTL_Deferred_TaxesCr=46
      integer(kind=2),parameter:: BTL_Deferred_TaxesDr=47
!
! PAYMENT NAMES FOR ORLANDO THESE ARE RESTRICTED FUND PAYMENTS
!
      integer(kind=2),parameter:: LAST_PAYMENT_ITEM=10
      integer(kind=2),parameter:: Decommissioning_Fund=1
      integer(kind=2),parameter:: Decommissioning_Liability_Chan=2
      integer(kind=2),parameter:: Post_Retirement_Medical=3
      integer(kind=2),parameter:: Retiree_MedicalPayments=4
      integer(kind=2),parameter:: OUC_Bond_Sinking_Fund=2
      integer(kind=2),parameter:: OUC_Long_Term_Investment=3
      integer(kind=2),parameter:: OUC_Debt_Service_Reserve=4
      integer(kind=2),parameter:: OUC_Customer_Deposit_Fund=5
      integer(kind=2),parameter:: OUC_Construction_Trust_Fund=6
      integer(kind=2),parameter:: OUC_Renewal_Replacement_Fund=7
!
! INVESTMENT NAMES FOR ORLANDO THESE ARE STABILIZATION/OTHER FUND DEPOSITS
!
      integer(kind=2),parameter:: LAST_INVESTMENT_ITEM=10
      integer(kind=2),parameter:: Dividend_Subsidiary=1
      integer(kind=2),parameter:: Net_Income_Subsidiary=2
      integer(kind=2),parameter:: Investment_in_Subsidiary=3
      integer(kind=2),parameter:: Other_Investments=4
      integer(kind=2),parameter:: Capitialized_LeaseAdditions=5
      integer(kind=2),parameter:: Cash_for_Assets=6
      integer(kind=2),parameter:: Change_in_LTInvestments=7
      integer(kind=2),parameter:: OUC_Liability_Reduction_Fund=1
      integer(kind=2),parameter:: OUC_Self_Insurance_Fund=2
      integer(kind=2),parameter:: OUC_Base_Stabilization_Fund=3
      integer(kind=2),parameter:: OUC_Fuel_Stabilization_Fund=4
      integer(kind=2),parameter:: OUC_Water_Stabilization_Fund=5
      integer(kind=2),parameter:: OUC_Customer_Retention_Fund=6
      integer(kind=2),parameter:: OUC_Other=7
      integer(kind=2),parameter:: OUC_Withdraw_Liab_Reduct_Fd=8
!
! RATE BASE NAMES
!
      integer(kind=2),parameter:: LAST_RATE_BASE_ITEM=5
      integer(kind=2),parameter:: Assets_NEC_Adjustment=1
      integer(kind=2),parameter:: Rate_Base_Adjustment=2
      integer(kind=2),parameter:: Deferred_TaxAdjustment=3
      integer(kind=2),parameter:: Plant_in_Service_Adjustment=4
!
! CD/CIAC NAMES                             
!
      integer(kind=2),parameter:: LAST_CD_CIAC_ITEM=5
      integer(kind=2),parameter:: CD_Balance_as_PC_of_Revenues=1
      integer(kind=2),parameter:: CD_Addendum=2
      integer(kind=2),parameter:: CIAC_Cash_Contributions=3
      integer(kind=2),parameter:: CIAC_AmortizationRate=4
      integer(kind=2),parameter:: CIAC_Addendum_to_Amortization=5
!
! Working Capital NAMES
!
      integer(kind=2),parameter:: LAST_WORKING_CAPITAL_ITEM=15
      integer(kind=2),parameter:: Assets_NEC_Addendum=1
      integer(kind=2),parameter:: Liabilities_NEC_Addendum=2
      integer(kind=2),parameter:: Fuel_Inventory_Change=3
      integer(kind=2),parameter:: Materials_Supplies=4
      integer(kind=2),parameter:: GasStorage=5
      integer(kind=2),parameter:: Rate_Receivable_Accounts=6
      integer(kind=2),parameter:: Addendum_Receivable_Accounts=7
      integer(kind=2),parameter:: Rate_Payable_Accounts=8
      integer(kind=2),parameter:: Addendum_Payable_Accounts=9
      integer(kind=2),parameter:: Rate_Assets_NEC=10
      integer(kind=2),parameter:: Rate_Liabilities_NEC=11
      integer(kind=2),parameter:: Fuel_Inventory_Change_Out=12
      integer(kind=2),parameter:: Materials_Supplies_Out=13
      integer(kind=2),parameter:: Gas_Storage_Out=14
!
! Earnings on Funds NAMES
!
      integer(kind=2),parameter:: LAST_EARNINGS_ITEM=15
      integer(kind=2),parameter:: Earnings_Decommissioning_Fund=1
      integer(kind=2),parameter:: Earnings_Post_Retirement_Fund=2
      integer(kind=2),parameter:: Earnings_ST_Investments=3
      integer(kind=2),parameter:: Earnings_LT_Investments=4
      integer(kind=2),parameter:: STDInterestRate=5
      integer(kind=2),parameter:: Decommissioning_Rate=6
      integer(kind=2),parameter:: Post_Retirement_Rate=7
      integer(kind=2),parameter:: STI_Rate=8
      integer(kind=2),parameter:: LTI_Rate=9
      integer(kind=2),parameter:: Customer_Deposits_Rate=10
      integer(kind=2),parameter:: OCI_Post_Retirement_Rate=11
      integer(kind=2),parameter:: OCI_Decommissioning_Rate=12

!
! Sale/Removal NAMES
!
      integer(kind=2),parameter:: LAST_SALE_REMOVAL_ITEM=10
      integer(kind=2),parameter:: Net_Removal=1
      integer(kind=2),parameter:: Cash_Received=2
      integer(kind=2),parameter:: Gross_Book_Value=3
      integer(kind=2),parameter:: CumulativeDepreciation=4
      integer(kind=2),parameter:: Sale_Deferred_Taxes_Adjustment=5
      integer(kind=2),parameter:: Sale_ATL_Amortization=6
      integer(kind=2),parameter:: SaleBTLAmortization=7
      integer(kind=2),parameter:: Cost_of_Removal=8
      integer(kind=2),parameter:: SalvageValue=9
      integer(kind=2),parameter:: Addedumto_Book_Gain_Loss=10
!
! Shareholder Value NAMES
!
      integer(kind=2),parameter:: LAST_SHAREHOLDER_VALUE_ITEM=4
      integer(kind=2),parameter:: Equity_Risk_Adjustment=1
      integer(kind=2),parameter:: Economic_Assets_Adjustment=2
      integer(kind=2),parameter:: Operating_Profits_Adjustment=3
      integer(kind=2),parameter:: Capitial_Recovery_EXCLUDED_OPAT=4
!
! FASB Names
!
      integer(kind=2),parameter:: LAST_FASB_ITEM=15
      integer(kind=2),parameter:: NucDecom_DiscountRate=1
      integer(kind=2),parameter:: NucDecom_Liability_Addendum=2
      integer(kind=2),parameter:: OCI_Adjustment=3
      integer(kind=2),parameter:: FASB87_Intangible_Asset_Adj=4
      integer(kind=2),parameter:: FABS87_Pension_Liab_Adj=5
      integer(kind=2),parameter:: OCI_Deferred_Tax_Adj=6
      integer(kind=2),parameter:: ARO_Discount_Rate=1
      integer(kind=2),parameter:: ARO_Accretion_Amount=2
      integer(kind=2),parameter:: ARO_Cash_Payment=7
      integer(kind=2),parameter:: ARO_Cash_to_Bal_Sheet_Trust=8
!
! CapX Names
!
      integer(kind=2),parameter:: LAST_CAPX_ITEM=6
      integer(kind=2),parameter:: Non_Cash_Pension=1
!
! CASH NAMES
!
      integer(kind=2),parameter:: LAST_CASH_ITEM=5
      integer(kind=2),parameter:: Revenue_Receipts=1
      integer(kind=2),parameter:: Expense_Payments=2
      integer(kind=2),parameter:: CashReceipts=3
      integer(kind=2),parameter:: CashPayments=4
!
! Transfers
!
      integer(kind=2),parameter:: LAST_TRANSFER_ITEM=64
      integer(kind=2),parameter:: Gross_PlantValue=1
      integer(kind=2),parameter:: Cumulated_Plant_Depreciation=2
      integer(kind=2),parameter:: Balance_Deferred_Taxes=3
      integer(kind=2),parameter:: Deferred_DebitAdjustment=4
      integer(kind=2),parameter:: ATL_Debit_Amortization=5
      integer(kind=2),parameter:: BTL_Debt_Amortization=6
      integer(kind=2),parameter:: Annual_Deferred_Taxes=7
      integer(kind=2),parameter:: Deferred_ITCs_Balance=8
      integer(kind=2),parameter:: Net_Nuclear_Fuel=9
      integer(kind=2),parameter:: RetainedEarnings=10
      integer(kind=2),parameter:: Extra_OrdinaryExpense=11
      integer(kind=2),parameter:: LT_Liabilities_Balance=12
      integer(kind=2),parameter:: Balance_Deferred_Taxes_Dr=13
      integer(kind=2),parameter:: CWIPBalance=14
      integer(kind=2),parameter:: Paidin_Capital=15
      integer(kind=2),parameter:: ARO_Net_Assets=16
      integer(kind=2),parameter:: ARO_Liabilities=17
      integer(kind=2),parameter:: Other_Deferred_Credits=18
      integer(kind=2),parameter:: Regulatory_Deferred_Credits=19
      integer(kind=2),parameter:: Reaquired_Debt_Gain=20
!
! Cash to xxx
!
      integer(kind=2),parameter:: LAST_CASH_TO_ITEMS=10
!
! Actuals
!
      integer(kind=2),parameter:: LAST_ACTUAL_ITEM=15
      integer(kind=2),parameter:: LAST_ACTUAL_TAX_ITEM=15
      integer(kind=2),parameter:: Actual_Deferred_TaxesCr=1
      integer(kind=2),parameter:: Actual_Fed_Income_Taxes=2
      integer(kind=2),parameter:: Actual_State_Income_Taxes=3
      integer(kind=2),parameter:: Actual_Deferred_TaxesDr=4
      integer(kind=2),parameter:: Actual_ATL_Fed_Income_Taxes=5
      integer(kind=2),parameter:: Actual_ATL_State_Income_Taxes=6
      integer(kind=2),parameter:: Actual_BTL_Fed_Income_Taxes=7
      integer(kind=2),parameter:: Actual_BTL_State_Income_Taxes=8
!
! OTHER ADDENDUM ITEMS
!
      integer(kind=2),parameter:: LAST_OTHER_ITEM=10
      integer(kind=2),parameter:: SRP_Target_Sales=1
      integer(kind=2),parameter:: STDMinimum_Balance=2
      integer(kind=2),parameter:: Cash_Minimum_Balance=3
      integer(kind=2),parameter:: OUC_Fuel_Inventory=1
      integer(kind=2),parameter:: OUC_Prepayments=2
      integer(kind=2),parameter:: OUC_Materials_Inventory=3
      integer(kind=2),parameter:: GRE_DSC_Principal_Pay_Adj=6
      integer(kind=2),parameter:: GRE_DSC_Interest_Pay_Adj=7
!
! INVESTMENT ACCOUNTS USED IN DEBT FILE
!
      integer(kind=2),parameter:: NUM_INVESTMENT_ACCOUNTS=15
      integer(kind=2),parameter:: NUM_OF_NOTE_ACCOUNTS=15
      integer(kind=2),parameter:: Dividend_70=1
      integer(kind=2),parameter:: Dividend=2
      integer(kind=2),parameter:: Interest_Payments=2
      integer(kind=2),parameter:: Interest_Earnings=3
      integer(kind=2),parameter:: Additions=4
      integer(kind=2),parameter:: Reductions=5
      integer(kind=2),parameter:: Cash_Interest_Payments=6
      integer(kind=2),parameter:: Cash_Interest_Earnings=7
      integer(kind=2),parameter:: Cash_Dividend_Earnings=8
      integer(kind=2),parameter:: Intra_Company_Interest_Earnings=9
      integer(kind=2),parameter:: Minibond_Interest_Payment=6
      integer(kind=2),parameter:: Minibond_Principal_Payment=7
      integer(kind=2),parameter:: Minibond_Interest_Deposit=8
      integer(kind=2),parameter:: Minibond_Principal_Deposit=9
      integer(kind=2),parameter:: Annualized_Interest=10
      integer(kind=2),parameter:: Minibond_Annualized_Interest=10
      integer(kind=2),parameter:: BAAN_Additions=1
      integer(kind=2),parameter:: BAAN_Reductions=2
      integer(kind=2),parameter:: BAAN_Interest_Payments=3
      integer(kind=2),parameter:: BAAN_Annualized_Interest=4

end module namescom

