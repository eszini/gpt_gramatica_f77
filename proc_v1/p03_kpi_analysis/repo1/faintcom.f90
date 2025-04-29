MODULE faintcom
!######################################################################
! Module created to replace FAintCom.mon
! 2024 - 05-14
! Pablo Bilbao
!######################################################################
  ! INTEGER, PARAMETER :: LimFacetSize = 2147483647 no se usa
  INTEGER (KIND=2), PARAMETER :: Zer2 = 0, One2 = 1, Two2 = 2, Ten2 = 10, i232 = 32
  INTEGER (KIND=2), PARAMETER :: LimEqPoints = 1000, DebugUnit = 255, DataUnit = 8
  INTEGER (KIND=2), PARAMETER :: InvalidIndex = 0, InvalidFacet = 0, LimCumulants = 4
  INTEGER (KIND=2), PARAMETER :: LimiHP = 5, LimPoints = 1000, LimUnknowns = 1000
  INTEGER (KIND=2), PARAMETER :: LimAuxEquations = 128, SupB2LogSize = 31
  INTEGER (KIND=2), PARAMETER :: BoundNB0 = 0, BoundNB1 = 1, BasicKey = 2, BasicNKy = 3
  LOGICAL (KIND=1), PARAMETER :: FalseByte = .FALSE., True_Byte = .TRUE.
  REAL (KIND=4), PARAMETER :: LargeReal = 1.0e16, SmallReal = 1.0e-4, NoiseFactor = 1.001
  
  REAL (KIND=4) :: OptCost, SmallCost, SmallArea, LargeArea, BaseLoad, Duration
  REAL, DIMENSION(0:LimEqPoints) :: EqLoad, EqLDn0
  INTEGER (KIND=4) :: I4_nVar, I4_2nVar, I4_nAux, I4_4nAux
  INTEGER (KIND=2) :: ErrorsTrapped, AS, nIter, nVar, nAux, nKappa, &
  nPoints, nAbnormalModes, nDisp, PrtDetail, PrtUni
  LOGICAL (KIND=1) :: BloomsPath
END MODULE faintcom