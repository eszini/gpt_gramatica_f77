!
!
      module mcbtn

      implicit none

      INTEGER(KIND=2), PARAMETER :: LimGroups  =   60
      INTEGER(KIND=2), PARAMETER :: LimBlocks  = 1200
      INTEGER(KIND=2), PARAMETER :: LimBTBlks  =    9
      INTEGER(KIND=2), PARAMETER :: SupDepth   =    8
      INTEGER(KIND=2), PARAMETER :: LimNodes   =  255
      INTEGER(KIND=2), PARAMETER :: LimCentiSD =  400

      INTEGER(KIND=2) :: VectSizeElem, nAugmLDC, nEvalLDC, PrtDetail
      INTEGER(KIND=2) :: kLoaded(0:SupDepth, 0:LimGroups)
      INTEGER(KIND=2) :: nLoadedBlocks(0:LimGroups)

      INTEGER(KIND=4) :: VectSizeByte

      REAL(KIND=4) :: BlockC(LimBlocks)
      REAL(KIND=4) :: BlockP(LimBlocks)
      REAL(KIND=4) :: BlockQ(LimBlocks)
      REAL(KIND=4) :: BlockIncrCost(LimBlocks)
      REAL(KIND=4) :: CCNormOrdinate(-LimCentiSD:LimCentiSD)
      REAL(KIND=4) :: NodeProb(0:LimNodes)
      REAL(KIND=4) :: PrevProb(0:LimNodes)
      REAL(KIND=4) :: NodeCOut(0:LimNodes)
      REAL(KIND=4) :: PrevCOut(0:LimNodes)
      REAL(KIND=4) :: COutMean(0:LimGroups)
      REAL(KIND=4) :: COutVari(0:LimGroups)
      REAL(KIND=4) :: NDMean, StdDev, HundSD, MeanM3s, MeanP3s

      LOGICAL(KIND=1) :: UsingMoments
      end module mcbtn
!
!
