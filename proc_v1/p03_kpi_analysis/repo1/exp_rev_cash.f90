module exp_rev_cash
! Created module to enable sharing of data previously shared using
! ENTRY statements
implicit none
	real (kind=4), allocatable :: ALLOC_EXPEN(:),ALLOC_CASH_EXPEN(:)
	real (kind=4), allocatable :: READ_EXPEN(:)
	real (kind=4), allocatable :: EXPENSES(:),ASSET_CLASS_LIST(:)
	real (kind=4), allocatable :: ASSET_ALLOCATION_LIST(:)
	real (kind=4), allocatable :: EXPENSES_MONTHLY(:,:,:,:,:,:)
	real (kind=4), allocatable :: REVENUES_MONTHLY(:,:,:,:,:,:)
	real (kind=4), allocatable :: CASH_MONTHLY(:,:,:,:)
	real (kind=4), allocatable :: CASH_REV_EXP_MONTHLY(:,:,:,:)
	real (kind=4), allocatable :: OUTPUT_VALUE(:)
	real (kind=4), allocatable :: BUDGET_EXPENSE(:,:,:)
	real (kind=4), allocatable :: ACCOUNT_PAYABLE_VALUES(:)

end module exp_rev_cash
