module dsm
! Imported dsm common block from dsmcom.mon.
use csvdat
implicit none

    real :: LM_CUSTOMERS(AVAIL_DATA_YEARS,MAX_DSM_DEVICES) 
    real :: NEW_LM_CUSTOMERS(AVAIL_DATA_YEARS,MAX_DSM_DEVICES)
    real :: DSM_DEVICE_ENERGY(MAX_DSM_DEVICES)
    real :: DSM_DEVICE_PEAK(AVAIL_DATA_YEARS,MAX_DSM_DEVICES)
    
end module dsm
