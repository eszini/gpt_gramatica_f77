module service_defs
! Module contains variables used by servicac - moved here and 
! renamed so they can be shared between servicac and ENTRY
! routines moved to actual routines in service_decs.
implicit none
	integer (kind=2) ::MAX_SERVICE_CLASS_ID_NUM
	integer (kind=2) :: ASSET_CLASS_sd
	INTEGER (kind=2) ::  MO_sd
	real (kind=4), allocatable :: ST_ANN_CLASS_EXPENSE(:,:,:)
	real (kind=4), allocatable :: ST_BTL_LEASE_PAYMENT(:,:)
	real (kind=4), allocatable :: ST_ANN_CLASS_ADJ_CLAUSE(:,:)
end module service_defs
