! module retslcom ported from RETSLCOM.MON F77 include file, 
! RETAIL_INFORMATION common block
module retslcom
implicit none
      REAL :: DELTA_CLASS_SALES_FROM_DSM(7)
      REAL :: RETAIL_SALES_BY_CLASS(7)
      REAL :: DELTA_PURCHASES_FROM_DSM(7)
      REAL :: DELTA_SYSTEM_SALES_FROM_DSM(7)
      REAL :: MONTHLY_CLASS_SALES_FROM_DSM(0:12,7)
end module retslcom