module transobj2_static
! Module makes several large arrays static
! instead of allocating on the stack.
! ONLY FOR USE BY TRANSOBJ2
      real (kind=4), save :: RPS_TRANS_DB(1:3,1:400,1:21,0:12)=0.
      real (kind=4), save :: GRX_RPS_TRANS_DB(1:3,1:400,1:21,0:12)=0.
      real (kind=4), save :: LOCAL_STORAGE_PATTERN(8760)=0.
      real (kind=4), save :: LOCAL_INDEP_PATTERN(8760)=0.
      integer :: dim1ub, dim2ub, dim3ub, dim4ub
end module transobj2_static
