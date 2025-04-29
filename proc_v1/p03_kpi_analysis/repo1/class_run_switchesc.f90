!
module CLASS_RUN_SWITCHESC
    implicit none
    character(len=1) :: DIVIDEND_PAYMENT_METHOD
    character(len=1) :: AFUDC_RETURN_POLICY
    character(len=1) :: RETURN_ON_RATEBASE_SOURCE
    character(len=1) :: OPERATING_METHOD
    character(len=1) :: PRICE_SOURCE_FOR_PRICE_DRIVER
    character(len=1) :: EQUITY_DEFINITION
    character(len=1) :: RATEBASE_VALUATION
    save :: DIVIDEND_PAYMENT_METHOD, AFUDC_RETURN_POLICY, &
           RETURN_ON_RATEBASE_SOURCE, OPERATING_METHOD, &
           PRICE_SOURCE_FOR_PRICE_DRIVER, EQUITY_DEFINITION, &
           RATEBASE_VALUATION
end module CLASS_RUN_SWITCHESC
!
!
