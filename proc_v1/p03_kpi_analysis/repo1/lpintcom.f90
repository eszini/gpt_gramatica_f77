module lpintcom
      implicit none
      character(len=20) :: Fmtxaixaxi
      real(kind=4) :: LargeReal,SmallReal
      parameter(Fmtxaixaxi='(1x,a,i4,1x,a,1x,i4)',LargeReal=1e30)
      parameter(SmallReal=1e-6) ! allow for cumulative round-off errors near 0

    ! variables assumed to be known by the LPdriver
      integer(kind=2) :: AS,OF0,OF1,AIU,PrtDetail ! 0=> .out file generated, 1=>.dbg generated, 2=> more
      common /LPInt/ AS,OF0,OF1,AIU,PrtDetail
end module lpintcom

