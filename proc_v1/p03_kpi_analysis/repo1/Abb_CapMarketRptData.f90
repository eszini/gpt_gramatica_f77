!******************************************************************
! ABBCapacityMarketRept.F95
! Copyright(c) DrG Solutions 2013
!
! Created: 9/15/2015 11:10:15 AM
! Author : Mark S Gerber
! Last change: msg 9/14/2016 12:03:32 PM

!******************************************************************

! ABB Report Capacity Market Report, MSGCPMKT.RPT.
! combines variables from reports TB, RX, UZ,MX,


 MODULE ABB_CapMarketRptData
    INTEGER (KIND=2), PARAMETER :: MAX_CAPACITY_MARKETS=255
    CHARACTER (LEN=35) CAPACITY_AREA_NAME(MAX_CAPACITY_MARKETS)
    REAL (KIND=4) :: UZVars(13,MAX_CAPACITY_MARKETS,22)
    REAL (KIND=4) :: TBVars(13,0:MAX_CAPACITY_MARKETS,23:25)
    REAL (KIND=4) :: MXVars(13,MAX_CAPACITY_MARKETS,26:26)
    REAL (KIND=4) :: RXVars(13,0:MAX_CAPACITY_MARKETS,27:28)
    REAL (KIND=4) :: NEWVars(13,MAX_CAPACITY_MARKETS,29:40)
 END MODULE
