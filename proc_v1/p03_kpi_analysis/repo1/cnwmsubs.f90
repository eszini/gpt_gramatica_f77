!     ******************************************************************
!     CNWMSUBS.F90
!     Copyright(c)  2000
!
!     Created: 1/4/2005 7:24:42 PM
!     Author : MARK S GERBER
!     Last change: MSG 7/3/2012 7:28:29 PM
!     ******************************************************************

      subroutine ShowStatus(AllExtant,Prefix,Suffix)
	  use filemod
      logical(kind=4) :: IsExtant,AllExtant
      character(len=3) :: Prefix
      character(len=*) :: Suffix
      character(len=255) :: FileSpec
      integer(kind=4) :: FileSize

      IsExtant=.false. 
      FileSpec=Prefix//trim(Suffix)//'.dat'
      FileSize=get_file_size(FileSpec)
      if((len_trim(Suffix)>0).and.(Suffix(1:4)/='NONE')) &
        AllExtant=AllExtant.and.IsExtant
      write(9004,'(2L2,i12,1x,a)') IsExtant,AllExtant, &
        FileSize,trim(FileSpec)
      end
! -----
      subroutine AppendI2w5(u,i) ! adapted from AppendInt5()
      integer(kind=4) :: u
      integer(kind=2) :: i
!
      if(    i<10) then
        write(u,'( i1,",")',advance='no') i
      elseif(i<100) then
        write(u,'( i2,",")',advance='no') i
      elseif(i<1000) then
        write(u,'( i3,",")',advance='no') i
      elseif(i<10000) then
        write(u,'( i4,",")',advance='no') i
! i<=32767 by assumption [else these format codes may be extended]
      else 
        write(u,'( i5,",")',advance='no') i
      end if
    ! ampersand usage under F77L3 requires CarriageControl=fortran
      return
      end ! subroutine AppendI2w5
! -----
      subroutine AppendF8p2(u,r,DLB) !suitable only for limited-range r
      integer(kind=4) :: u
      logical(kind=1) :: DLB ! DeleteLeadingBlanks
      real(kind=4) :: r
      character(len=8) :: s8
!
      if(DLB) then
        write(s8,'(f8.2)') r
        do while(s8(1:1)==' ')
          s8=s8(2:8)
        end do
        if(s8(1:1)=='*') then ! use more width to preclude overflow
          call AppendFloat9(u,r)
        else
          write(u,'(    a,",")',advance='no') trim(s8)
        end if
      else
        write(u,'( f8.2,",")',advance='no') r
      end if
      end
! -----
	  
      function AsteriskIf(j) ! useful in crude bar-graphs
	  character(len=1) :: AsteriskIf
      integer(kind=2) :: j
      if(j>0) then
        AsteriskIf='*'
      else
        AsteriskIf='|'
      end if
      end function AsteriskIf
! -----
      subroutine DeleteUS(s,n) ! delete underscores; adapted from MakeUpper()
      integer(kind=4) :: m,n,i
      character(len=*) :: s ! len(s) is undependable
!
      m=n ! avoid attempt to change n, which may be a constant
      do i=n,1,-1 ! delete ith char if previously '_'
        if(s(i:i)=='_') then
          if(i>=m) then
            m=m-1
            s=s(1:m)
          else
            s=s(1:i-1)//s(i+1:m)
          end if
        end if
      end do
      end
! -----
      subroutine DulyNote(s) ! allows removal of obsolescent calls to WriteStrAt
      character(len=*) :: s
!
!      call WriteStrAt(1,1,s)
      write(9004,'(1x,a)') trim(s)
!      write(*     ,'(1x,a)') trim(s)
      end
! -----
      function FuncCNWAMtx(j,i)
	  real(kind=4) :: FuncCNWAMtx
      integer(kind=4) :: j,i
      real(kind=4) :: r4
!
      call ReturnCNWAMtx(j,i,r4)
      FuncCNWAMtx=r4
      end function FuncCNWAMtx
! -----
      function GQ(x,y) ! Guarded Quotient of presumed positive args
	  real(kind=4) :: GQ
      real(kind=4) :: x,y
!
      if(y<=0.0) then
!        write(9004,'(a,2g15.7)') ' ERROR:  attempt to divide',x,y
        GQ=0.0
      else
        GQ=x/y
      end if
      end function GQ
! -----
      function IdxInI4(i4Array,n,ThisItem)
	  integer(kind=2) :: IdxInI4
      integer(kind=2) :: n,i
      integer(kind=4) :: ThisItem,i4Array(n)
!
      do i=1,n
!        write(4,'(4i6,a)') i,n,i4Array(i),ThisItem,' II'
        if(i4Array(i)==ThisItem) exit
      end do
!      if(i>n) call ps(1,'specified index not found on list (IdxInI4)')
    ! caller must decide how to handle the case of index not found
      IdxInI4=i
      end function IdxInI4
! -----
      function MilliFrac(x,y) ! assumes 0<=x<=y
	  integer(kind=2) :: MilliFrac
      real(kind=4) :: x,y
      if((y<0.001).or.(y<x)) then
        MilliFrac=0
      elseif(x>=y) then
        Millifrac=1000
      else
        MilliFrac=nint(1000.0*x/y)
      end if
      end function MilliFrac
! -----
      function zbiSum(iVect,iSup)
	  integer(kind=4) :: zbiSum
      integer(kind=4) :: iSup,vSum,i
      integer(kind=2) :: iVect(0:iSup)
      vSum=0
      do i=0,iSup
        vSum=vSum+iVect(i)
      end do
      zbiSum=vSum
      end function zbiSum

      module CNW_main
      integer(kind=4) :: nLink, &
        EPntID  (    :), &
        SNodID  (    :), &
        DNodID  (    :), &
        LinkNdID(  :,:)
      integer(kind=2) :: iYr,iYrPrev=99,nCCSegs,iFocus, & 
        LimFocusIter, &
        nSNod,nExpU,nDNod,nRout,nBasn,nCont,iSNod,iDNod,RepInYr=0, &
        LinkID  (    :), &
        SNodOfLk(    :), &
        DNodOfLk(    :), &
        RnOfLink(    :), &
        BasnID  (    :), &
        BnOfSNod(    :), &
        PnOfDNod(    :), &
        SegsUsd (    :)
      integer(kind=1) :: StOfDNod(:)
      logical(kind=1) :: Optimal,Focussed ! Feasible was inaccurate description
      real(kind=4) :: TestFactor, &
        SNodQtyLB (  :), &
        SNodQtyUB (  :), &
        SNodQty   (  :), &
        DNodQty   (  :), &
        fRetrofLk (  :), &
        fDNdQtyLk (  :), &
        SO2PileUB (  :), &
        SO2BlndUB (  :), &
        SNodPri (    :), &
        MineFunc(:,:,:), &
        LinkChg (    :), &
        RoutUB  (    :), &
        BasnQtyLB (  :), &
        BasnQtyUB (  :), &
        SnHeatCon (  :), &
        HeatConLB (  :), &
        HeatConUB (  :), &
        HeatCoGLB (  :), &
        HeatCoLUB (  :), &
        HeatConAv (:,:), & ! (-1:1,*) prior, init,  current years' heat-content by node
        HeatConID (:,:), & !  (0:1,*) increase and decrease fraction allowed by node
        UnitCap (    :), &
        SO2Content(  :), &
        HeatRateAv(  :), &
        ScrCapCost(  :), &
        ScrCapFCRt(  :), &
        ScrVOMCost(  :), &
        ScrFOMCost(  :)
      character(len=27) :: &
        BasnName(    :)
      allocatable &
        EPntID    , &
        SNodID    , &
        DNodID    , &
        LinkID    , &
        LinkNdID  , &
        SNodOfLk  , &
        DNodOfLk  , &
        RnOfLink  
      allocatable &
        BasnID    , &
        BnOfSNod  , &
        PnOfDNod  , &
        SegsUsd   , &
        StOfDNod  , &
        SNodQtyLB , &
        SNodQtyUB , &
        SNodQty   , &
        DNodQty   , &
        fRetrofLk , &
        fDNdQtyLk , &
        SO2PileUB , &
        SO2BlndUB , &
        SNodPri   , &
        MineFunc  , &
        LinkChg   , &
        RoutUB    , &
        BasnQtyLB , &
        BasnQtyUB , &
        SnHeatCon , &
        HeatConLB , &
        HeatConUB , &
        HeatCoGLB , &
        HeatCoLUB , &
        HeatConAv , &
        HeatConID , &
        UnitCap   , &
        SO2Content, &
        HeatRateAv, &
        ScrCapCost, &
        ScrCapFCRt, &
        ScrVOMCost, &
        ScrFOMCost, &
        BasnName
      end module
! -----
      recursive subroutine GregsCNWRoutine(BaseYr,RunYr,RefreshNodes) ! owner of interface arrays

      use COAL_MODEL_INPUT_TO_CNW
      use CoalModelOutputsFromCNW
      use ArrayAllocationInterface
      use CNW_main
      character(len=255) :: StudyName,OneLine
      CHARACTER(len=*) :: R_StudyName
      logical RefreshNodes
      logical(kind=1) :: LimitTypesAt2,FocusScrubbing, &
        B0=.false.,B1=.true.,NeedOutFiles
      integer(kind=2) :: nDNUB,BaseYr,RunYr,nDnPrev,SO2_MARKETS_YR_BASED_VALUES
      integer(kind=4) :: DbgU=9004
      real(kind=4) :: MaxContFr,TestFParam
      save
!
      nSO2Limits=SO2_MARKETS_YR_BASED_VALUES(RunYr)
      iYr=RunYr
      call UpdateBasinData() ! after 20110831, bounds may vary with iYr
    ! 20110421 following uses iYr only for Plant_Annual_Demand_By_Year
      if(RefreshNodes) call FillOutCNWStructure() ! reads base-year values for fUnsc & SO2UB
    ! count MSG's invocations for reporting to DbgU
      if(iYr>iYrPrev) then
        RepInYr=1
      else
        RepInYr=RepInYr+1
      end if
      iFocus=1
      write(OneLine,'(a,3i3)') 'Beginning Model-Year,iter,iFocus', &
        iYr,RepInYr,iFocus
      call DulyNote(OneLine)
      if((iYr>iYrPrev).or.(iYr>1)) call UpdateAccums() ! nDnPrev<nDNod is possible
      write(DbgU,'(/3i3,i5,a/(10i5))') iYr,RepInYr,iFocus,nDNod, &
        ' org fUnscNode(ppK):', &
        (nint(1e3*fUnscNode(0,iDNod)),iDNod=1,nDNod)
!
      call UpdateCNWSupplyData()
      call UpdateCNWDemandData() ! revises HeatConAav,fUnscNode,SO2PileUB,SO2BlndUB for new units
      call FillOutCNWTransLinks()
!
      call DefineCNWSO2Options() ! sets PrtDetail,fUnscNode(2,.)
      call ReadCNWContData()
      call AdjustCNWContracts(MaxContFr) ! and WARN of infeasible conditions removed
!
      write(DbgU,'(/3i3,i5,a/(10i5))') iYr,RepInYr,iFocus,nDNod, &
        ' tgt fUnscNode(ppK):', &
        (nint(1e3*fUnscNode(2,iDNod)),iDNod=1,nDNod)
      call MinCNWCosts()

      do iFocus=1,LimFocusIter ! GAT hopes that 5 iterations suffice to FocusScrubbing
        call SolveCNWLpUsingPcx()
        call ReportCNWSolutionDetails(BaseYr,FocusScrubbing)

        if(.not.Optimal) then
          call ps(1,'WARNING:  solution reported is not optimal')

        end if

        if(Focussed.or.(.not.FocusScrubbing)) exit
      end do

      call CheckCNWSolution()
      write(DbgU,'(/3i3,i5,a/(10i5))') iYr,RepInYr,iFocus,nDNod, &
        ' opt fUnscNode(ppK):', &
        (nint(1e3*fUnscNode(1,iDNod)),iDNod=1,nDNod)
      nDnPrev=nDNod
      iYrPrev=iYr
!
!      call DumpCNWOutputsModule(nBasn,nSNod,nDNod,DbgU,DNodQty)
      call DulyNote(' ') ! visually separate model-years
      return ! recursive subroutine GregsCNWRoutine
! -----
      entry UpdateAccums
    ! following was extracted from tail of ReadCNWLinkData to use proper DNodQty
    ! as the HeatConAv(0,iDNod), which is the basis for HeatConID incr/decr specs
      if(iYr>iYrPrev) &
      HeatConAv(-1,1:nDnPrev)=HeatConAv(1,1:nDnPrev) ! consolidate confirmed optimal values
      HeatConAv(0, :)=-1.0
      HeatConAv(0,1:nDnPrev)=HeatConAv(-1,1:nDnPrev) ! update the
    ! prior-year heat content to match that obtained for previous iYr solution;
    ! use HeatConAv(0,) and HeatConID to determine LB & UB for each aggr plant
!

      fUnscNode(0, :)=-1.0

      return
! -----
      entry InitCNWStructure(R_StudyName,NeedOutFiles,TestFParam) ! ,BaseYr)
    ! assign module values to locals for use by entry's:
      nBasn=nBasin
      nSNod=nCoalSupply
      nDNod=nMaxExistingCoalDemand ! nCoalDemand before 20110411
      nExpU=nMaxGenericCoalDemand ! allocate room sufficient for expansion
      nRout=nTransportLinksExistingUnits+nGenericTransLinks ! nTransportLinks prior to 20110411
      nCont=nCoalContracts
      StudyName=R_StudyName
      if(NeedOutFiles) call OpenCNWOutFiles(StudyName)
      write(DbgU,'(/a,6i6)') ' sizes passed from caller of ICS: ', &
        nBasn,nSNod,nDNod,nRout,nCont,nExpU
      if((nBasn==0).or.(nSNod==0).or.(nDNod==0)) call ps(1,'size is 0')
!      nDNUB=nDNod*MaxExpansionFactor
!
    ! assign defaults for unspecified (obsolete or debugging) arguments
      StudyName='study'
      MaxContFr=1.0 ! default 'allow contract to comprise entire CT-supply or Plnt-demand'
!     MaxContFr=0.35 ! default; 0.4 caused infeasibility in early testing

      write(DbgU,'(a,f12.3)') ' TestF passed from to ICS: ',TestFParam

      TestFactor=0.5 ! after 20110826
      write(DbgU,'(a,f12.3)') ' SO2_cost multiplier used: ',TestFactor
      LimFocusIter=max(mod(1000.0*TestFParam,10.0),1.0) 
      write(DbgU,'(a,i3   )') ' ScrubberFocus iter limit: ',LimFocusIter
      LimitTypesAt2=B0
      FocusScrubbing=(TestFactor<1.0)

    ! allocate arrays for output results
      call InitCNWOutputsModule(nBasn,nSNod,nDNod+nExpU)
!
      call AllocateCNWBasnArrays()
      call GetCNWNodeCount()
      call AllocateCNWNodeArrays(nDNod+nExpU)
      write(DbgU,'(/a,7i6)') ' sizes used to allocate arrays  : ', &
        nBasn,nSNod,nDNod,nRout,0,nCont,nDNod+nExpU
      nDnPrev=nDNod
      return ! entry InitCNWStructure
! -----
      entry FillOutCNWStructure ! using current-year active nodes
    ! after allocating, restore caller's values (back off intial overestimate)
    ! 20110411 MSG requested to use less-than-allocated sizes for assignment loops
      nRout=nTransportLinks ! perhaps less than allocated earlier, varies by iYr
      nCont=nCoalContracts ! affects no allocation, but varies by iYr
      nDNod=nCoalDemand
      write(DbgU,'(/a,6i6)') ' sizes before reading node data : ', &
        nBasn,nSNod,nDNod,nRout,0,nCont
      call ReadCNWSupplyNodeData()
      call ReadCNWDemandNodeData()
      return
! -----
      entry FillOutCNWTransLinks
      nRout=nTransportLinks ! perhaps less than allocated earlier, varies by iYr
      nCont=nCoalContracts ! affects no allocation, but varies by iYr
      call CountCNWRouteLinks()
      write(DbgU,'(/a,6i7)') ' sizes before reading link data : ', &
        nBasn,nSNod,nDNod,nRout,0,nCont
      call ReAllocateCNWLinkArrays() ! nLink may vary by iYr
      write(DbgU,'(/a,6i7)') ' sizes used to fill-out arrays  : ', &
        nBasn,nSNod,nDNod,nRout,nLink,nCont
      call AllocateArray(RoutUB,nRout)
      RoutUB=-1.0
      call ReadCNWLinkData(LimitTypesAt2)
      return
! -----
      entry AllocateCNWBasnArrays
      call AllocateArray(BasnQtyUB,nBasn)
      call AllocateArray(BasnQtyLB,nBasn)
      call AllocateArray(BasnID,nBasn)
      call AllocateArray(BasnName,nBasn)

    ! assign default of -1, implying N.A.
      BasnID=-1
      BasnQtyLB=-1.0
      BasnQtyUB=-1.0
      BasnName=' '
      return
! -----
      entry AllocateCNWNodeArrays(nDNUB)

      call AllocateArray(SNodQty  ,nSNod)
      call AllocateArray(SNodPri  ,nSNod)
      call AllocateArray(SNodQtyLB,nSNod) ! min quantity at supply
      call AllocateArray(SNodQtyUB,nSNod) ! max quantity at supply
      call AllocateArray(BnOfSNod ,nSNod)
      call AllocateArray(SegsUsd  ,nSNod)
      call AllocateArray(SNodID   ,nSNod)
      call AllocateArray(SnHeatCon,nSNod)
      call AllocateArray(SO2Content,nSNod)
      call AllocateArray(SNodName  ,nSNod)
      ! segment x(EoS fract cap),MinedCost,DeltaX
      call AllocateArray(MineFunc,1,int(nCCSegs,4),0,2,1,int(nSNod,4)) 
!
      call AllocateArray(DNodQty ,nDNUB) ! quantity demand or supply
      !        init, optimal, & target fraction of Demand not usin
      call AllocateArray(fUnscNode, 0,2,1,int(nDNUB,4)) 

      call AllocateArray(SO2PileUB,nDNUB)
      call AllocateArray(SO2BlndUB,nDNUB)
      call AllocateArray(HeatConLB ,nDNUB)
      call AllocateArray(HeatConUB ,nDNUB)
      call AllocateArray(HeatCoGLB ,nDNUB)
      call AllocateArray(HeatCoLUB ,nDNUB)
      ! prior, init, & current years' Avg Heat Content
      call AllocateArray(HeatConAv,-1,1,1,int(nDNUB,4))
      ! annual fractional decrease or increase allowed 
      call AllocateArray(HeatConID,0,1,1,int(nDNUB,4)) 
      call AllocateArray(HeatRateAv,nDNUB)
      call AllocateArray(UnitCap,nDNUB)
      call AllocateArray(ScrCapCost,nDNUB)
      call AllocateArray(ScrCapFCRt,nDNUB)
      call AllocateArray(ScrVOMCost,nDNUB)
      call AllocateArray(ScrFOMCost,nDNUB)
      call AllocateArray(PnOfDNod,nDNUB)
      call AllocateArray(StOfDNod,int(nDNUB,4))
      call AllocateArray(EPntID  ,nDNUB)
      call AllocateArray(DNodID  ,nDNUB)
      call AllocateArray(DNodName,nDNUB)
    ! assign default of -1, implying N.A.
      BnOfSNod=-1
      SegsUsd =-1
      PnOfDNod=-1

      EPntID=-1
      SNodID=-1
      DNodID=-1
      SNodQtyLB=-1.0
      SNodQtyUB=-1.0
!
      fUnscNode=-1.0
!      SO2RateDN=-1.0
!      SO2RateIc=-1.0
      SO2PileUB=-1.0
      SO2BlndUB=-1.0
      SNodQty=-0.1
      DNodQty=-0.1 ! cannot preclude usage of -1 as a pointer
      SNodPri=-1.0
      MineFunc=-1.0
      SnHeatCon=-1.0
      HeatConLB=-1.0
      HeatConUB=-1.0
      HeatCoGLB=-1.0
      HeatCoLUB=-1.0
      HeatConID=-1.0
      HeatConAv=-1.0
      UnitCap=-1.0
      ScrCapCost=-1.0
      ScrCapFCRt=-1.0
      ScrVOMCost=-1.0
      ScrFOMCost=-1.0
      SO2Content=-1.0
      HeatRateAv=-1.0
      SNodName=' '
      DNodName=' '
      return ! entry AllocateCNWNodeArrays
! -----
      entry ReAllocateCNWLinkArrays
      call AllocateArray(LinkChg,nLink) ! A-to-B commodity charge at 100%
      call AllocateArray(LinkID,nLink)
      call AllocateArray(LinkNdID,0,1,1,nLink) ! A-to-B IDs
      call AllocateArray(SNodOfLk,nLink) ! link Supply's serial number (contiguous)
      call AllocateArray(DNodOfLk,nLink)
      call AllocateArray(RnOfLink,nLink)
      call AllocateArray(SDRnOfLk,0,2,1,nLink)
      call AllocateArray(fRetrofLk,nLink)
      call AllocateArray(fDNdQtyLk,nLink)

    ! assign default of -1, implying N.A.
      LinkChg=-1.0
      LinkID=-1
      LinkNdID=-1
      SNodOfLk=-1
      DNodOfLk=-1
      RnOfLink=-1
      SDRnOfLk=-1
      fRetrofLk=-1.0
      fDNdQtyLk=-1.0
      return
!
      end ! subroutine GregsCNWRoutine
! -----
      subroutine InitCNWOutputsModule(nBasn,nSNod,nDNUB)
      use CoalModelOutputsFromCNW
      use ArrayAllocationInterface
      use miscmod
      integer(kind=2) :: nBasn,nSNod,nDNUB
!
      call AllocateArray(DmndAvgFobCst,nDNUB)
      call AllocateArray(DmndAvgTrnCst,nDNUB)
      call AllocateArray(DmndAvgEmsCst,nDNUB)
      call AllocateArray(MineQAMTcost,1,int(nSNod,4),0,3) ! 0 is Quantity; 1-3 are Marginal,Avg,Total costs
      call AllocateArray(BasnDmndQty,nBasn,nDNUB)
      call AllocateArray(StoD_QyAfMfAdMdTrEmCst, &
        0,6,0,int(nSNod,4),1,int(nDNUB,4))
      !  for the 1st index, 0 is Quantity/kT; 1-5 are respectively
      !  Marginal FOB,Avg FOB,Marginal Dvd,Avg Dvd,Trans,Emiss costs in $/T,
      !  for the 2nd index, 0 is for the average across nSNod suppliers
      DmndAvgFobCst=0.0
      DmndAvgTrnCst=0.0
      DmndAvgEmsCst=0.0
      MineQAMTcost=0.0
      BasnDmndQty=0.0
      StoD_QyAfMfAdMdTrEmCst=0.0 ! needed when some nodes are not active
      end
! -----
!define ignore_plant_SO2UB
! assume that units can be aggregated by plant
!note:  LimNSeg herein limits # of mine-cost curve segments to be used;
!reduce LimNSeg to at least 1 to reduce size of constraint matrix;
!define RUB_mult 1e3 disabled 20110428 per GT and GH request
! MaxUperP limits the # of units per plant to (that-1), nPlnt to 32k/(that)
!define OobWidth 0.01 adequate if dataset were feasible, at least 10 if not:
!define OobWidth 0.001 0.01 inadequate
!define OobWidth 0.10
      recursive subroutine CoalNtwkMain(BaseYr, &
        LimitTypesAt2,MaxContFr,StudyName)
      use COAL_MODEL_INPUT_TO_Cnw
      use CoalModelOutputsFromCnw
      use ArrayAllocationInterface
      use CNW_main
      use miscmod
	  use flusher
    ! declarations for interface variables
      integer(kind=2) :: BaseYr
      logical(kind=1) :: LimitTypesAt2
      real(kind=4) :: MaxContFr
      character(len=255) :: StudyName
!
    ! declarations for internal variables and arrays
      character(len=511) :: OneLine
      character(len=255) :: DirPath=(' ')
      character(len=255) :: FileName
      character(len=50) :: CoTypeName,ThisNdName
      character(len=28) :: ReqDesc(:)
      character(len=27) :: VarDesc(:)

      character(len=9) :: BaseFileName
      character(len=4) :: bVa4,R4toA4,A4Equiv(:),OutSuffix=('.out')
      character(len=2) :: ThisPoSt
      character(len=1) :: cDummy,FirstCh,AComma=(','),AHyphen=('-'), & !  ABlank/' '/,
        AlarmCh,QuestionIf,AsteriskIf,RelChar(0:2)='=<>'
      character(len=*) :: RETsuffix
      logical(kind=1) :: B0=.false.,B1=.true., &
        nTypesLtd,UnitsDiffer,ErrHCtLB,ErrHCtUB,DNodVoid, &
        DecrAbs,DecrOrd,Negat, & !  HeaderAbsent,InTolerance,OrigBounds,BdyViolated,
        OnTgtCol,OnTgtRow,Destructive,Negligible,UniformDecrOK, & !  +  ,MinimizingObj/.true./ SMofInterest
        StUBUsed(0:51),FocusScrubbing
      integer(kind=1) :: iStUBSup, &
        ReVec(:),LkEndUsd(:) ! ,LinkFrTo(:,:),PossFrTo(:,:),ContFrTo(:,:)
    ! note that nVpLk,nVpDN,nVpRt,nRpLk,nRpRt,nRpDN,nContMax,nContLoB,nPlnt are local vars unknown to caller
      integer(kind=2) :: i,j,k,m,n,p,q,iBeg,iEnd,cYr,jSeg,nNzSegs,nWd,nICA, &
        nSNdU,nDNdU,mDNod,iRout,nVpDN,nVpRt,nRpLk,nRpRt,nRpDN,SumSUsd, &
        mRank1st,mRank2nd,iSNrank1,iSNrank2,iDNrank1,iDNrank2,nRoutVoid, &
        iPolState,nStUBUsd,iPolStGrp,nGpUBUsd,nInGroup(0:2), &
        BasinID,FuelTID, &
        iBasn,nBasnUB,iPlnt,nPlnt,piDNod,piSNod,nScrFuzzy,nScrInsLP, & !  UofPt,
        nPlntMax=32766, & !  was 32767/100
        nInB2LogQty(0:15), &
        PrtDetail,iSc=11,PriID,SecID,i2_0=0, &
        nCTGeqSThr,jBin,QuintBinCount(0:5),DecimBinCount(0:10), &
        PctBinCount(0:100),MineMSeg(:), & 
        IndexIn,IdxInI4,MilliFrac 
      integer(kind=4) :: i4,j4,EvPlntID,pEPlntID,nNzQty,nVpLk, &
        iLink,nLkUsed,iLkUsed, &
        ZeroIfExtant,ErrOrEnd, &
        TxtU=9002,DbgU=9004, &
        BnvU=9005,MnvU=9006,MdcU=9007,AdcU=9008,CtvU=9009, &
        OutU,OrgCentiSec,EndCentiSec,ExecET, &
        nContMax,nContLoB,nLkUsdAt(:), &
        mReq,iReq,nVar,nNdVar,jVar,iCol,jCol, &
        iRqkTn,iRqfBt,iRqWSC,iRqSO2, &
        iUbV,nUbV,jUbVec(:), &
        iLbV,nLbV,jLbVec(:), & ! okay to pass allocatables even if never allocated
        !  use the PCX default to MINimize its objective
        MinObj=1,WrtDetail=5, & !  nLZ,
        zbColOfNze(:),zbRowOfNze(:),zbColHold(:),zbRowHold(:), &
        ConsMtSize,ConsMtSLim,iNze, &
        OneBasedCol,OneBasedRow,zbCol,zbRow, &
        PCX_ret_code,PCX_main_sp,zbiSum
      integer(kind=8) :: EvUnitID
      real(kind=8) :: OFV,SO2Penalty,ContraCost,ContraValu,DotProd8,nCTObjBias
      real(kind=4) :: d4,f4Prev,f4,h4,n4,o4,p4,q4,r4,t4,MBpKT,DpGWh,DpMWC, & !  u4,
        QtySum,CstSum,HCtSum,CapSum,GWhSum,ScrCst,SubSum,UuQSum,UuQFSm,  & !  SO2Sum,
        SumDemandGBt,MaxSupplykT,MinDemandkT,FullSignif, &
        fUnscrOrg,fUnscrInf,QtyWtdSlack,RetroWt,PlntQty, &

        SupDmndQty, & !  WtAtHCLB,WtAtHCUB, ! SupContQty,
        SNodACst,SNodMCst,DNodACst,DNodMCst,WtdCost(0:6), & !  SimAvgPri,SupSourMC,
        MeanQty,EstMeanQty,VnceQty,StDvQty,ContraP,ContraQ, &
        MeanLog,EstMeanLog,VnceLog,StDvLog,LogQty,SO2EmsPri, &

        ThisQtySeg(20), &
        ThisPriSeg(20), &
        StGroupUB(0:2), &
        ContQty(:,:),ContCst(:,:),fPlntUnit(:),StoDkT(:),kTpMB(:), &
        ShCst(:),EmCst(:,:),InfDvdCst(:),SupDvdCst(:), &
        UbValu(:),LbValu(:), & !  okay to pass even if never allocated
        cVOrg(:),cVect(:), &
        bVect(:),SpVec(:),xVect(:),ConsMtxNze(:),ConsEHold(:), &
        MineMCst(:),MineACst(:),MineCost(:),TransCst(:),TransQty(:), &
        EmissCst(:),RoutQty(:),StatsQty(:), &
        LnkQtySn(:),LnkQtyDn(:),BasinQty(:), &

        fUnscrMin=0.05, & !  .01 KillenStation; .02 Kyger Creek,Keystone (PA),Wateree; .03 Roxboro; 0.04 Cliffside
        MinePri,MineQUB,MnNdFunc(20,0:2), & !  SupplyMult,
        FuncCNWAMtx,ACMvalue,RCMvalue,GQ ! ,ioReal ! +  ,xVsm(:),AMat(:,:) ! SMofInterest
      allocatable VarDesc,ReqDesc,A4Equiv, &
        ReVec,LkEndUsd,nLkUsdAt, & !  LinkFrTo,PossFrTo,ContFrTo,
        jUbVec,UbValu, &
        jLbVec,LbValu,ConsMtxNze,ConsEHold, &
        zbColOfNze,zbRowOfNze,zbColHold,zbRowHold, &
        MineMSeg, & !  AvailSN,DnOfPlnt,
        cVOrg,cVect,bVect,SpVec,xVect, &
        ContQty,ContCst,fPlntUnit,StoDkT,kTpMB, &
        ShCst,EmCst,InfDvdCst,SupDvdCst, &
        MineMCst,MineACst,MineCost,TransCst,TransQty, &
        EmissCst,RoutQty,StatsQty, &
        LnkQtySn,LnkQtyDn,BasinQty


        
      save iStUBSup,i4,EvPlntID,nTypesLtd,OutU,OneLine, &
        OrgCentiSec,EndCentiSec,ExecET,StUBUsed, &
        iReq,mReq,jVar,nVar,nNdVar, &
        iUbV,nUbV,iLbV,nLbV,cYr, &
        PrtDetail,WrtDetail, &
        nSNdU,nDNdU,nNzSegs,iRout,iLink,nRoutVoid,nLkUsed,SumSUsd, &
        nVpLk,nVpDN,nVpRt,nRpLk,nRpRt,nRpDN, &
        nContMax,nContLoB, &
        iPlnt,nPlnt,nStUBUsd,nGpUBUsd,nInGroup,nBasnUB, &
        MineMSeg, & !  AvailSN,DnOfPlnt,
        VarDesc,ReqDesc,A4Equiv,ThisNdName, &
        ReVec,LkEndUsd,nLkUsdAt, & !  LinkFrTo,PossFrTo,ContFrTo,
        cVOrg,cVect,bVect,SpVec,xVect, &
        jUbVec,UbValu, &
        jLbVec,LbValu,ConsMtxNze, &
        zbColOfNze,zbRowOfNze, &
        ConsMtSize,ConsMtSLim, &
        ThisQtySeg, &
        ThisPriSeg, &
        SumDemandGBt,MaxSupplykT, &
        ContQty,ContCst,fPlntUnit,StoDkT,kTpMB,MBpkT, &
        ShCst,EmCst,InfDvdCst,SupDvdCst,FullSignif, &
        MineMCst,MineACst,MineCost,TransCst,TransQty, &
        EmissCst,RoutQty,StatsQty, &
        LnkQtySn,LnkQtyDn,BasinQty, &
        OFV,ContraCost,ContraValu,nCTObjBias

      return ! recursive subroutine CoalNtwkMain (declarations only)
! -----
      entry GetCNWNodeCount
      nCCSegs=20 ! save in module CNW_main, the array size used
      nBasnUB=nBasn
      write(DbgU,'(2i5,a)') nSNod,nDNod,' nodes to be allocated'
      if(nDNod<=0) call ps(1,'No active Demand-nodes were counted')
      return
! -----
      entry UpdateBasinData
    ! load local arrays with values from the module(s)
      BasnID   (1:nBasn)=Basin_ID    (1:nBasn)
      BasnQtyLB(1:nBasn)=Basin_QtyLB (1:nBasn)
      BasnQtyUB(1:nBasn)=Basin_QtyUB (1:nBasn)
      BasnName (1:nBasn)=Basin_Abbrev(1:nBasn)

      do iBasn=1,nBasn

        write(DbgU,'(i4,i5,2f12.2,1x,a)') iBasn,BasnID(iBasn), &
          BasnQtyLB(iBasn),BasnQtyUB(iBasn),trim(BasnName(iBasn))
      end do
      return
! -----
      entry AssignCostCurve(MinePri,MineQUB,MnNdFunc) ! ,SupplyMult)
      k=nNzSegs
      if(ThisQtySeg(nNzSegs)>999998.0) nNzSegs=nNzSegs-1 ! cancel MSG's kludge
      if(nNzSegs<=0) return
      MineQUB=MaxVal(ThisQtySeg(1:nNzSegs)) ! highest rate of production allowed
      MinePri=MaxVal(ThisPriSeg(1:nNzSegs)) ! highest price (at the highest rate of prod.)

      f4Prev=0.0
      jSeg=0
      do while(jSeg<nNzSegs)
        jSeg=jSeg+1
        f4=ThisQtySeg(jSeg)/MineQUB ! fraction of max
        if(jSeg>1) then
          DecrAbs=(f4<=f4Prev) ! equality here => zero UB for segment capacity
          DecrOrd=(ThisPriSeg(jSeg)<MnNdFunc(jSeg-1,1))
          if(DecrAbs) write(DbgU,'(a/(10f9.1))') &
            ' DATA ERROR:  non-increasing Cost Curve abscissas:', &
            (ThisQtySeg(j),j=1,nNzSegs)
          if(DecrOrd) write(DbgU,'(a,2i3/(10f9.3))') &
            ' DATA ERROR:  decreasing Cost Curve ordinates:',jSeg,k, &
            (ThisPriSeg(j),j=1,nNzSegs)
          if(DecrAbs.or.DecrOrd) then
          ! retain costs through the prior point, but with full capacity
            jSeg=jSeg-1
            nNzSegs=jSeg
            f4Prev=0.0
            if(nNzSegs>1) f4Prev=MnNdFunc(nNzSegs-1,0)
          end if
        end if
        if(jSeg==nNzSegs) f4=1.0
        MnNdFunc(jSeg,0)=f4 ! fraction of max
        MnNdFunc(jSeg,1)=ThisPriSeg(jSeg) ! $/T
        MnNdFunc(jSeg,2)=f4-f4Prev ! fractional change across segment in (,0,)
        f4Prev=f4
      end do ! jSeg
      return ! entry AssignCostCurve
! -----
      entry ReadCNWSupplyNodeData

      SumSUsd=0
      do iSNod=1,nSNod
        j=Supply_Basin_ID(iSNod)
        k=IndexIn(BasnID,nBasn,j)
        if(k>nBasn) call ps(1,'Supply-node basin is invalid') ! inactive/disabled
        SNodID(iSNod)=Supply_Fuel_Type_ID(iSNod)+j*10000 ! merge CoalType with BasnID in j
        BnOfSNod(iSNod)=k ! contiguous SN on [1,nBasn]
        SNodQtyLB (iSNod)=Supply_Min_Annual_Production(iSNod)
        SNodQtyUB (iSNod)=Supply_Max_Annual_Production(iSNod) ! revised in AssignCostCurve
        SnHeatCon (iSNod)=Supply_Heat_Content(iSNod)
        SO2Content(iSNod)=Supply_SO2_Rate    (iSNod)
        CoTypeName=Supply_Description        (iSNod)

        SNodName(iSNod)=CoTypeName
        nNzSegs=Supply_Curve_Points(iSNod) ! ,nCCSegs)
        ThisQtySeg(1:nNzSegs)=Supply_Production(1:nNzSegs,iSNod)
        ThisPriSeg(1:nNzSegs)=Supply_Cost      (1:nNzSegs,iSNod)
        call AssignCostCurve(SNodPri(iSNod),SNodQtyUB(iSNod), &
          MineFunc(1,0,iSNod)) ! ,TestFactor)
        SumSUsd=SumSUsd+nNzSegs
        SegsUsd(iSNod)=nNzSegs
        if(nNzSegs>0) nSNdU=nSNdU+1
        write(DbgU,'(2i5,i8,1x,a15,5f12.2,i3,i5,20(f6.2,f6.0,f5.2))') &
          iSNod,BnOfSNod(iSNod),SNodID(iSNod),CoTypeName, &
          SNodQtyLB(iSNod),SNodQtyUB(iSNod),SNodPri(iSNod), &
          SnHeatCon(iSNod), &
          SO2Content(iSNod),nNzSegs,SumSUsd, &
          ((MineFunc(jSeg,j,iSNod),j=0,2),jSeg=1,nNzSegs)
!        write(DbgU,'(a,20i6)') ' CCx',(nint(MineFunc(jSeg,0,iSNod)
!    +    *10000.0),jSeg=1,nNzSegs) ! show the spacing of CC points
      end do ! read Supply-node data
      iStUBSup=nSO2Limits ! save in a more precisely named var
      write(DbgU,'(a,i3/(20a3))') ' Sma:',iStUBSup, &
        (SO2MarketCodes(j),j=0,iStUBSup)
      write(DbgU,'(a,i3/(1x,10i11))') ' SO2UB/kT:',iStUBSup, &
        (nint(SO2EmissionLimits(j)),j=0,iStUBSup)
      write(DbgU,'(a,i6/(20i3))') ' #SgU:',nSNod,(SegsUsd (j),j=1,nSNod)
      write(DbgU,'(a,i6/(20i4))') ' BoN1:',nSNod,(BnOfSNod(j),j=1,nSNod)
      return ! entry ReadCNWSupplyNodeData
! -----
      entry GetiPlnt
      iPlnt=IdxInI4(EPntID,nPlnt,EvPlntID) ! contiguous SN on [1,nPlnt]
      if(iPlnt>nPlnt) then ! log the instance of an unprecedented ID
        nPlnt=nPlnt+1 ! nPlnt<=nDNod
        if(nPlnt>nPlntMax) call ps(1,'# of plants exceeds limit')
        EPntID(nPlnt)=EvPlntID
        write(DbgU,'(2i9,a)') nPlnt,EvPlntID,' new GetiPlnt'
      end if
      PnOfDNod(iDNod)=iPlnt ! useful in allocating contracts
      return
! -----
      entry ReadCNWDemandNodeData

      p4=0.01 ! convert from percentage to fraction

      nPlnt=0

      StOfDNod=-1
      i4=20000000000/100 ! 31-bit unsigned integers impose limit
      do iDNod=1,nDNod
        DNodName   (iDNod)=Plant_Name             (iDNod)
        DNodQty    (iDNod)=Plant_Annual_Demand_By_Year(iYr,iDNod) ! iYr after 20110421
        UnitCap    (iDNod)=UnitCapacity           (iDNod)
        HeatConLB  (iDNod)=Plant_Heat_Lower_Bound (iDNod) ! *0.125 ! KLUDGE
        HeatConUB  (iDNod)=Plant_Heat_Upper_Bound (iDNod) ! *1.500 ! KLUDGE
        fUnscNode(0,iDNod)=Plant_Fraction_Unscrubbed(iDNod)

        SO2PileUB(  iDNod)=SO2MaxInAnyCoal(iDNod)
        SO2BlndUB(  iDNod)=SO2MaxInBlend(iDNod)
        HeatConID(0,iDNod)=Plant_Percent_Increase (iDNod)
        HeatConID(1,iDNod)=Plant_Percent_Decrease (iDNod)
        HeatConAv(0,iDNod)=Plant_Heat_Content_Base_Year(iDNod)
        HeatRateAv( iDNod)=AverageHeatrate        (iDNod)
        ScrCapCost( iDNod)=SOxControlCapitalCost(iDNod) ! ($/kW)
        ScrCapFCRt( iDNod)=SOxControlCarryingCharge(iDNod) ! (fraction/year)
        ScrVOMCost( iDNod)=SOxControlVarCost(iDNod) ! ($/MWh)
        ScrFOMCost( iDNod)=SOxControlFixedCost(iDNod) ! ($/kW/yr)
        EvPlntID=EV_Plant_ID(iDNod)

        EvUnitID=EV_Unit_ID(iDNod)
        ThisNdName=DNodName(iDNod) ! only for display below
        ThisPoSt=StateLocation(iDNod)
!
      ! circumvent problems of base-year HeatContent out-of-bounds
        h4=HeatConAv(0,iDNod)
        q4=HeatConLB(iDNod)
        r4=HeatConUB(iDNod)
        if(q4>h4) then
          write(DbgU,'(i9,2f9.1,a)') EvPlntID,h4,q4, &
            ' Plant''s base-year HeatContent<LB; DATA ERROR (covered)'
          HeatConLB(iDNod)=h4
        end if
        if(r4<h4) then
          write(DbgU,'(i9,2f9.1,a)') EvPlntID,h4,r4, &
            ' Plant''s base-year HeatContent>UB; DATA ERROR'
          HeatConUB(iDNod)=h4
        end if
      ! zeroes for % Incr/Decr => impose defaults per MSG
        if((HeatConID(0,iDNod)==0.0).and. &
           (HeatConID(1,iDNod)==0.0)) then
          HeatConID(0,iDNod)=100.0 ! KLUDGE
          HeatConID(1,iDNod)= 10.0 ! KLUDGE
        end if
        do j=0,1
          HeatConID(j,iDNod)=1.0+HeatConID(j,iDNod)*p4*float(1-j*2) ! convert % to multiplier
        end do
        h4=HeatConAv(0,iDNod)
        HeatCoGLB(iDNod)=amax1(HeatConLB(iDNod),h4*HeatConID(1,iDNod))
        HeatCoLUB(iDNod)=amin1(HeatConUB(iDNod),h4*HeatConID(0,iDNod))
        if((0>EvPlntID).or.(EvPlntID>i4)) call ps(1, &
          ' EvPlntID coding would cause overflow of 31-bit DNodID')
        DNodID(iDNod)=EvPlntID*100+mod(EvUnitID,int(100,8)) ! hope for uniqueness here
        call GetiPlnt()

        do iPolState=0,iStUBSup
          if(SO2MarketCodes(iPolState)==ThisPoSt) exit
        end do
        if(.not.(iPolState>iStUBSup)) then
          StOfDNod(iDNod)=iPolState ! index in SO2MarketCodes active
        end if
        write(DbgU, &
          '(2i5,i3,i11,1x,a15,f12.0,5f8.0,3f6.3,L2,2f7.2)') iDNod, &
          PnOfDNod(iDNod), & 
          StOfDNod(iDNod), &
          DNodID(iDNod), &
          ThisNdName, &
          DNodQty(iDNod), &
          UnitCap(iDNod), &
          HeatConLB(iDNod), &
          HeatConUB(iDNod), &
          HeatConAv(0,iDNod), &
          HeatRateAv(iDNod),HeatConID(0:1,iDNod), &
          fUnscNode(0,iDNod), &
          OptUnscrubbedFrac(iDNod), &
          SO2PileUB(iDNod), &
          SO2BlndUB(iDNod)
        if(fUnscNode(0,iDNod)>1.0) then
          write(DbgU,'(1x,e9.3,a)') fUnscNode(0,iDNod), &
            '=fraction unscrubbed>1; DATA ERROR'
          fUnscNode(0,iDNod)=1.0
        end if
      end do ! read Demand-node data
     write(DbgU,'(a,i6/(20i4))') ' BoN2:',nSNod,(BnOfSNod(j),j=1,nSNod)
!
      write(DbgU,'(1x,10i5)') (BasnID(iBasn),iBasn=1,nBasn)
      write(DbgU,'(1x,10i11)') (SNodID(iSNod),iSNod=1,nSNod)
      write(DbgU,'(1x,10i11)') (DNodID(iDNod),iDNod=1,nDNod)
      return ! entry ReadCNWDemandNodeData
! -----
      entry CountCNWRouteLinks
      write(DbgU,'(i6,a)') nRout,' route-records active in file'
      nLink=0
      nContMax=0
     write(DbgU,'(a,i6/(20i4))') ' BoN3:',nSNod,(BnOfSNod(j),j=1,nSNod)
      do iRout=1,nRout
        iBasn=Basin_HI_ID(iRout) ! these arrays are from COAL_TRANSPORTATION_LINKS_INPUT

        EvPlntID=Plant_EV_ID(iRout)
        iBasn=IndexIn(BasnID,nBasn,iBasn)
        iPlnt=IdxInI4(EPntID,nPlnt,EvPlntID)
        if((iBasn>nBasn).or.(iPlnt>nPlnt)) then
          write(DbgU,'(4i6,i9,a)') iRout,nLink,iBasn,iPlnt,EvPlntID, &
            ' inactive/invalid basin or plant on route'
          cycle
        end if
      ! count the CoalTypes available from this route's Supply basin
        j=0
        do iSNod=1,nSNod
          if((SegsUsd(iSNod)<=0).or.(BnOfSNod(iSNod)/=iBasn)) cycle
          do iDNod=1,nDNod
            if(PnOfDNod(iDNod)==iPlnt) j=j+1
          end do
        end do
        nContMax=nContMax+j ! augment count of max CoalTypeToUnit contracts
        nLink=nLink+j ! allow any CoalType at iBasn to reach iPlnt's units
        write(DbgU,'(4i6,i3,a)') iRout,nLink,iBasn,iPlnt,j, &
          ' route,links,B,P,nCT@B'
      end do
     write(DbgU,'(a,i6/(20i4))') ' BoN4:',nSNod,(BnOfSNod(j),j=1,nSNod)
      write(DbgU,'(i6,i7,a)') &
        nRout,nLink,' routes & links to be allocated'
      write(DbgU,'(i6,a)') nContMax,' max CoalTypeToUnit contracts'
      if(nLink==0) call ps(1,'Zero links; problem is undefined')

      return ! entry CountCNWRouteLinks
! -----
      entry UpdateCNWSupplyData
!      MineFunc(1:nCCSegs,0,1:nSNod)=Supply_Production_By_Year
!     +        (1:nCCSegs,  1:nSNod,iYr)
!      MineFunc(1:nCCSegs,1,1:nSNod)=Supply_Cost_By_Year
!     +        (1:nCCSegs,  1:nSNod,iYr)
      SumSUsd=0
      nSNdU=0
      do iSNod=1,nSNod
        nNzSegs=Supply_Curve_Points(iSNod)
!        ThisQtySeg(1:nNzSegs)=MineFunc(1:nNzSegs,0,iSNod)
!        ThisPriSeg(1:nNzSegs)=MineFunc(1:nNzSegs,1,iSNod)
        ThisQtySeg(1:nNzSegs)=Supply_Production_By_Year &
                  (1:nNzSegs,iSNod,iYr)
        ThisPriSeg(1:nNzSegs)=Supply_Cost_By_Year &
                  (1:nNzSegs,iSNod,iYr)
        call AssignCostCurve(SNodPri(iSNod),SNodQtyUB(iSNod), &
          MineFunc(1,0,iSNod)) ! ,TestFactor)
        SumSUsd=SumSUsd+nNzSegs
        SegsUsd(iSNod)=nNzSegs ! in case Cost Curve was truncated
        if(nNzSegs>0) nSNdU=nSNdU+1
!        write(DbgU,'(i5,2f12.2,i3,i5)') iSNod,
!     +    SNodQtyUB(iSNod),SNodPri(iSNod),nNzSegs,SumSUsd
      end do
      write(DbgU,'(a)') ' after UpdateSupplyData'
      call RefreshCNWSizes()
      call ReportCNWSizes()
      return ! entry UpdateCNWSupplyData
! -----
      entry UpdateCNWDemandData
      DNodQty(1:nDNod)=Plant_Annual_Demand_By_Year(iYr,1:nDNod)
      p4=0.01
      t4=0.0
      do iDNod=1,nDNod

        if(DNodQty(iDNod)<=0.0) cycle
        t4=t4+DNodQty(iDNod)
        EvPlntID=EV_Plant_ID(iDNod)
        fUnscNode(0,iDNod)=Plant_Fraction_Unscrubbed(iDNod)
        call GetiPlnt() ! allow for nPlnt increase over prior pruned value
        if(fUnscNode(0,iDNod)<0.0) then ! unit may have just come on-line

          SO2PileUB(  iDNod)=SO2MaxInAnyCoal(iDNod)
          SO2BlndUB(  iDNod)=SO2MaxInBlend(iDNod)
          HeatConAv(0,iDNod)=Plant_Heat_Content_Base_Year(iDNod)
          write(DbgU,'(2i5,2a)') iDNod,nint(1e3*fUnscNode(0,iDNod)), &
            ' first year? ',DNodName(iDNod)
         if(fUnscNode(0,iDNod)<0.0) call ps(1,'fUnscNode<0 ERROR UpDD')
        end if
      end do
      call DulyNote('after UpdateDemandData')
      write(DbgU,'(/i3,a,e12.5)') iYr,' Model-Year Demand/GBtu: ', &
        t4*0.001
      write(DbgU,'(10i11)') (nint(DNodQty(iDNod)*0.001),iDNod=1,nDNod)
      if(t4<1.0) call ps(1,'(total BTU requirement<1)=>logic error')
      return ! entry UpdateCNWDemandData
! -----
      entry ReadCNWLinkData(LimitTypesAt2)
      iLink=0
      RoutUB(1:nRout)=Annual_Quantity_Limit(1:nRout) ! *RUB_mult ! kT KLUDGE
      write(DbgU,'(/i6,a)') nRout,' RoutUB:'
      write(DbgU,'(1x,10i8)') (nint(RoutUB(k)),k=1,nRout)
      write(DbgU,'(/i6,a)') nSNod,' BnOfSNod:'
      write(DbgU,'(1x,10i6)') (BnOfSNod(k),k=1,nSNod)
      write(DbgU,'(/i6,a)') nDNod,' PnOfDNod(initial):'
      write(DbgU,'(1x,10i6)') (PnOfDNod(k),k=1,nDNod)
!      write(DbgU,'(/i6,a)') nRout,' Plant_EV_ID(iRout):'
!      write(DbgU,'(5i11)') (Plant_EV_ID(k),k=1,nRout)
      write(DbgU,'(/i6,a)') nPlnt,' EPntID(iPlnt):'
      write(DbgU,'(5i11)') (EPntID(k),k=1,nPlnt)
      do iRout=1,nRout
        k=Coal_Link_ID          (iRout)
        i=Basin_HI_ID           (iRout)
        EvPlntID=Plant_EV_ID    (iRout)
        t4=Transport_Price      (iRout)
!       f4=Annual_Quantity_Limit(iRout) ! currently unused here
        iBasn=IndexIn(BasnID,nBasn,i)
        iPlnt=IdxInI4(EPntID,nPlnt,EvPlntID)
        if((iBasn>nBasn).or.(iPlnt>nPlnt)) then
          write(DbgU,'(4i6,i9,a)') iRout,iLink,iBasn,iPlnt,EvPlntID, &
            ' inactive/invalid basin or plant on route'
          cycle
        end if
        m=0
        do iSNod=1,nSNod
          if((SegsUsd(iSNod)<=0).or.(BnOfSNod(iSNod)/=iBasn)) cycle
          r4=SO2Content(iSNod)
          m=m+1 ! counts Supply nodes serving iPlnt
          j=0
          do iDNod=1,nDNod
!            write(DbgU,'(a,3i6,1x,a)')
!     +        ' link',iLink,iSNod,iDNod,trim(DNodName(iDNod))
            if(PnOfDNod(iDNod)/=iPlnt) cycle
            if(SO2PileUB(iDNod)<r4) cycle ! SO2Content of iSNod is not acceptable
          !    SO2PileUB(iDNod) is not needed below here;
          ! above lowers nLink, but it cannot be determined earlier
            j=j+1 ! counts parallel links from iSNod to all units in iPlnt
            iLink=iLink+1

            RnOfLink(iLink)=iRout ! used to distinguish parallel links
            LinkID(iLink)=k ! invariant with iSNod
            LinkNdID(0,iLink)=SNodID(iSNod) ! OK if equal to some DNodID(iDNod)
            LinkNdID(1,iLink)=DNodID(iDNod)
            LinkChg(iLink)=t4
            SNodOfLk(iLink)=iSNod
            DNodOfLk(iLink)=iDNod

          end do ! iDNod
        end do ! iSNod
        if(m==0) write(DbgU,'(i6,a,i9,a,i5,a)') iRout,' route to Plant', &
          EvPlntID,' has no active supply at Basin',i,'; DATA ERROR'
      end do ! iRout-loop counting iLink
      MaxIndexSDRnOfLk=iLink
      if(nLink>iLink) nLink=iLink ! reduce range of later loops
      call DulyNote('after reading Link-Route data')
      call AllocateArray(LkEndUsd,nLink)
      call AllocateArray(nLkUsdAt,nLink)
      LkEndUsd=0 ! default implying 'not useful'

      write(DbgU,'(i7,a)') nLink,' links to active units'

    ! prune Supply nodes untouched by active links

      t4=0.0
      iSNod=nSNod+1
      do while(iSNod>1)
        iSNod=iSNod-1
        j=0
        do iLink=1,nLink
          if(SNodOfLk(iLink)==iSNod) then
          if(DNodOfLk(iLink)>0) then
            LkEndUsd(iLink)=1 ! implying 'Supply-end is active'
            j=j+1
          end if
          end if
        end do
        if(j==0) then

          write(DbgU,'(i5,2a)') iSNod,' Supply (without link): ', &
            trim(SNodName(iSNod))
          SumSUsd=SumSUsd-SegsUsd(iSNod)
          nSNdU=nSNdU-1 ! iSNod is nut useful
          SegsUsd(iSNod)=0
        else
          t4=t4+SNodQtyUB(iSNod)
        end if
      end do ! pruning nSNod (Supply)
      write(DbgU,'(a/(20i3  ))')' #SegsUsed:',(SegsUsd(j),j=1,nSNod)
      write(DbgU,'(i5,e12.5,a)') nSNod,t4,' Supply_limit on Qty/kT'

      write(DbgU,'(i6,a)') nSNod,' Supply-nodes after pruning:'

      call AllocateArray(kTpMB,nSNod)
!
      MaxSupplykT=0.0
      write(DbgU,'(a)') &
        ' SNod  SNodID Basin Type iBn SNMaxkTn SumMaxkT MegaB/kT', &
        ' ---- ------- ----- ---- --- -------- -------- --------'
      t4=0.0
      do iSNod=1,nSNod
        if(SegsUsd(iSNod)<=0) cycle
        i4=SNodID(iSNod)
        iBasn=BnOfSNod(iSNod)
        r4=SNodQtyUB(iSNod) ! kT
        MaxSupplykT=MaxSupplykT+r4
        kTpMB(iSNod)=1000.0/(SnHeatCon(iSNod)*2000.0) 
        t4=t4+1.0/kTpMB(iSNod)
        write(DbgU,'(i5,i8,i6,i5,i4,3f9.0)') iSNod, &
          i4,i4/10000,mod(i4,10000),iBasn,r4,MaxSupplykT, &
          GQ(1.0,kTpMB(iSNod))
      end do
      MBpkT=t4/float(nSNod) ! simple unweighted mean
     write(DbgU,'(i5,a,f9.1)') nSNod,'-node average MegaBtu/kT: ',MBpkT
!
!
    ! prune Demand nodes untouched by active links or with no Demand
      write(DbgU,'(/i7,a/(20i5))') nLink,' pre-pruning DNodOfLk:', &
        (DNodOfLk(i4),i4=1,nLink)
      nDNdU=0
      do iDNod=nDNod,1,-1 ! reverse order was useful when decrementing nDNod
        iPlnt=PnOfDNod(iDNod)
        if(IPlnt>0) then
          EvPlntID=EPntID(iPlnt)
        else
          EvPlntID=-1
        end if
        if(UnitCap(iDNod)<=0.0) DNodQty(iDNod)=0.0 ! per MSG request re retired units
        q4=DNodQty(iDNod)
        if(q4<=0.0) then ! DNodOfLk retains value
          iPlnt=0
        else
          j=0
          do iLink=1,nLink
            if(DNodOfLk(iLink)==iDNod) then
            ! set LkEndUsd=2 iff previously 1
!             LkEndUsd(iLink)=min(LkEndUsd(iLink),int(1,1))*2 ! 2=> 'both ends active'
              if(LkEndUsd(iLink)>=1) LkEndUsd(iLink)=2 ! should be faster than above
              j=1 ! sufficient, faster than j=j+1
            end if
          end do
        end if
        if((iPlnt<=0).or.(j==0)) then ! iDNod may be for a future plant

          PnOfDNod(iDNod)=0
         write(DbgU,'(i6,i5,i3,i11,i9,f7.1,f11.0,2a)') iDNod,iPlnt,j, &
            DNodID(iDNod),EvPlntID,UnitCap(iDNod),q4, &
            ' No Supply and/or no Demand at DNode: ', &
            trim(DNodName(iDNod))

        else
          nDNdU=nDNdU+1
        end if
        if(mod(int(iDNod),300)==0) call DulyNote('another 300 DNod pruned')
      end do ! iDNod pruning nDNod (Demand)
      do iLink=1,nLink
        if(LkEndUsd(iLink)/=2) DNodOfLk(iLink)=0
      end do
      if(nDNod<100) &
        write(DbgU,'(i7,a/(20i5))') nLink,' post-pruning DNodOfLk:', &
        (DNodOfLk(i4),i4=1,nLink)
      write(DbgU,'(i6,a)') nDNdU,' Demand-nodes used after pruning'
      write(DbgU,'(/i6,a)') nDNod,' PnOfDNod(post-pruning):'
      write(DbgU,'(1x,10i6)') (PnOfDNod(k),k=1,nDNod)
      call DulyNote('after pruning Nodes')
!      write(DbgU,'(/i7,a/(20i3))') nLink,' post-pruning LkEndUsd():',
!    +  (LkEndUsd(i4),i4=1,nLink) ! all were 2 using full dataset
!
    ! note that pruning above is now effected not by reducing nSNod or nDNod,
    ! but by marking usability of links via LkEndUsd array, and by PnOfDNod
!
      SumDemandGBt=0.0
      write(DbgU,'(a)') &
        ' iDNod     DNodID     Plant Un NdReqGBtu  SumReqGBtu', &
        ' ----- ---------- --------- -- --------- -----------'
      do iDNod=1,nDNod
        if(PnOfDNod(iDNod)<=0) cycle
        i4=DNodID(iDNod)
        r4=DNodQty(iDNod)*0.001
        SumDemandGBt=SumDemandGBt+r4
        write(DbgU,'(i6,i11,i10,i3,f10.0,f12.0,a)')iDNod,i4,i4/100, &
          mod(i4,100),r4,SumDemandGBt
      end do
      write(DbgU,'(i6,e12.5,a)') &
	    nDNod,SumDemandGBt/(0.001*MBpkT), &
        ' estimated Unit Demand Qty/kT at mean MegaBtu/kT (all active)'
      nTypesLtd=LimitTypesAt2 ! make accessible to other entry points
      nLkUsed=0
      do iLink=1,nLink
        if(LkEndUsd(iLink)==2) nLkUsed=nLkUsed+1
      end do

      call ReAllocLocalsPostPruning() ! using nSNod,nDNod,nPlnt,nLink
      return ! entry ReadCNWLinkData
! -----
      entry DefineCNWSO2Options

      PrtDetail=1 ! for some debugging details

      if(iYr>3) PrtDetail=0
      do iDNod=1,nDNod ! set target fUnscNode for Demand Nodes
        f4=fUnscNode(0,iDNod)
        if(PnOfDNod(iDNod)==0) then ! iDNod is inactive or unreachable during iYr

          fUnscNode(0,iDNod)=-1.0
          fUnscNode(2,iDNod)=-1.0
          OptUnscrubbedFrac(iDNod)=B0
        else ! caller used prior-year results to modify OptUnscrubbedFrac
          if(OptUnscrubbedFrac(iDNod)) then ! retain f4 as the nominal target

            fUnscNode(2,iDNod)=amin1(f4,fUnscrMin)
          else ! unit had immutable fUnscNode; leave no room for LP to reduce it
            fUnscNode(2,iDNod)=f4
          end if
        end if
        write(DbgU,'(i5,L2,2i5,a)') iDNod,OptUnscrubbedFrac(iDNod), &
          (nint(1e3*fUnscNode(j,iDNod)),j=0,2,2),' fUnsc org & tgt'
        if(fUnscNode(0,iDNod)<fUnscNode(2,iDNod)) call ps(1, &
          ' fUnsc target exceeds the original')
      end do
      do iLink=1,nLink
        if(LkEndUsd(iLink)/=2) cycle ! should preclude units with 0 Demand
        iSNod=SNodOfLk(iLink)
        iDNod=DNodOfLk(iLink)

          QtySum=DNodQty(iDNod) ! MBtu requirement
          CapSum=UnitCap(iDNod) ! MW
          GWhSum=QtySum/HeatRateAv(iDNod) ! (MBtu)/(Btu/kWh) => (GWh)
          DpGWh= ScrVOMCost(iDNod)/0.001 ! ($/MWh)/(G/M) => ($/GWh)
          DpMWC=(ScrFOMCost(iDNod)+ &
           ScrCapCost(iDNod)*ScrCapFCRt(iDNod))*1000.0 
          ScrCst=GWhSum*DpGWh+CapSum*DpMWC ! ($)
          if((fUnscNode(0,iDNod)<0.0).or. &
         (PrtDetail>1))write(DbgU,'(2i6,i7,i3,5f7.0,3i4,f14.0,1x,a)') &
            iDNod,iSNod,iLink,LkEndUsd(iLink), &
            HeatConLB(iDNod),HeatConUB(iDNod),HeatConAv(0,iDNod), &
            HeatCoGLB(iDNod),HeatCoLUB(iDNod), &
            nint(HeatConID(0,iDNod)*100.)-100, & !  display these 3 as %
            nint(HeatConID(1,iDNod)*100.)-100,PnOfDNod(iDNod),QtySum, &
            DNodName(iDNod)(1:35)
          if(fUnscNode(0,iDNod)<0.0) call ps(1, &
            'fUnscNode<0 ERROR in DSO')
          iPolState=StOfDNod(iDNod)
          if(iPolState>=0) then
            SO2EmsPri=SO2CreditPrice(iPolState)
          elseif(iStUBSup>=0) then ! NA price and limit are in effect for iYr
            SO2EmsPri=SO2CreditPrice(0)
          else
            SO2EmsPri=0.0
          end if
!       end do ! iDNod
        StoDkT(iLink)=DNodQty(iDNod)*kTpMB(iSNod) ! (MBtu)*(kT/MBtu) => (kT)
      ! ($/MBtu) emissions cost, separated from ShCst 20110107
        EmCst(0,iLink)=SO2EmsPri*SO2Content(iSNod)*fUnscNode(0,iDNod) ! ($=(LbSO2MBtu) => ($/MBtu)
        EmCst(1,iLink)=SO2EmsPri*SO2Content(iSNod)*fUnscNode(2,iDNod) & !  ($=(LbSO2MBtu) => ($/MBtu)
          +GQ(ScrCst,QtySum) ! ($)/(MBtu) assuming scrubbers are 0/1 decision
        ShCst(iLink)=LinkChg(iLink)*1000.0*kTpMB(iSNod) ! ($=(nk)*(kT/MBtu) => ($/MBtu)
      ! above is cost of shipping only, excluding emissions and coal FOB
      end do ! iLink
!
    ! this section now should be moved above to just after pruning
      i4=nLkUsed
      nLkUsed=0
      do iLink=1,nLink
        if(LkEndUsd(iLink)==2) nLkUsed=nLkUsed+1
        nLkUsdAt(iLink)=nLkUsed
      end do
      if(nLkUsed/=i4) call ps(1,'error_1 counting links used')
!
      t4=0.0
      StUBUsed=B0
      do iPlnt=1,nPlnt
        PlntQty=0.0
        do iDNod=1,nDNod ! assign fraction of MBtu demand to fPlntUnit
          if(PnOfDNod(iDNod)/=iPlnt) cycle
          iPolState=StOfDNod(iDNod)
          if(iPolState/=-1) then
            StUBUsed(iPolState)=B1
            StUBUsed(0)=B1 ! use of any state UB => use of NA's UB
          end if
          PlntQty=PlntQty+DNodQty(iDNod)
        end do
        if(PlntQty>0.0) then
          t4=t4+PlntQty
          do iDNod=1,nDNod
            if(PnOfDNod(iDNod)/=iPlnt) cycle
            fPlntUnit(iDNod)=DNodQty(iDNod)/PlntQty

          end do
        end if
      end do ! iPlnt
!
      write(DbgU,'(/i5,a,e13.5)') nDNod,' DNode_total Demand/MBtu:',t4
      write(DbgU,'(/i5,a/(40L2))') nDNod,' OptUnscrFrac:', &
        OptUnscrubbedFrac(1:nDNod)
      nStUBUsd=0 ! count includes NA, excludes pruned states
      nInGroup=0 ! count members in each group of states
      do iPolState=0,iStUBSup
        if(StUBUsed(iPolState).eqv.B1) then
          iPolStGrp=min(StateGroupAssignment(iPolState),2)
          nInGroup(iPolStGrp)=nInGroup(iPolStGrp)+1 ! StGroupUB is computed later
          nStUBUsd=nStUBUsd+1
        end if
      end do
      nGpUBUsd=0 ! count non-void groups with index>0
      do iPolStGrp=1,2
        if(nInGroup(iPolStGrp)>0) nGpUBUsd=nGpUBUsd+1
      end do
      write(DbgU,'(a,20i3)') ' GpUBUsed:',nInGroup(0:2)
      write(DbgU,'(a,i3/(30L2))') ' StUBUsed:',nStUBUsd, &
        (StUBUsed(j),j=0,iStUBSup)
      return ! entry DefineCNWSO2Options
! -----
      entry ReadCNWContData
      do i=1,nCont ! max(nCont,1)

        BasinID =Contract_Basin_ID(i) 
        FuelTID =Contract_Mine_ID (i) 
        EvPlntID=Contract_Plant_ID(i)
        ContraQ=Contract_Annual_Quantity(iYr,i)
        ContraP=Contract_Annual_Price   (iYr,i)

!
        do k=1,nSNod

          if(SNodID(k)==FuelTID+BasinID*10000) exit
        end do

        if(k>nSNod) then
          write(OneLine,'(2i5,a)') BasinID,FuelTID, &
            ' Basin & Type in Contracts-file unmatched in Supply-file'
          call DulyNote(OneLine)
          cycle
        end if
!
        iPlnt=IdxInI4(EPntID,nPlnt,EvPlntID) ! not final before DefineCNWSO2Options

        if(iPlnt>nPlnt) then
          write(OneLine,'(i11,a)') EvPlntID, &
            ' Plant ID in Contracts-file is not active in Demand-file'
          call DulyNote(OneLine)
          cycle
        end if
!

          ContQty(k,iPlnt)=ContQty(k,iPlnt)+ContraQ ! (kT)
          ContCst(k,iPlnt)=ContCst(k,iPlnt)+ContraQ*ContraP*1000.0 ! (kT)*($=(nk) => ($)

        write(DbgU,'(3i5,a,f7.1)') i,k,iPlnt,' ContQty',ContraQ
      end do ! reading lines of Contract data
      write(DbgU,'(i5,a)') nCont,' Contracts active'
      call DulyNote('after reading Contract data')
      return ! entry ReadCNWContData
! -----
      entry AdjustCNWContracts(MaxContFr)
      return ! entry AdjustCNWContData
! -----
      entry OpenCNWOutFiles(StudyName)
      FileName='rpt'//trim(StudyName)//'.rpt' 
      ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
!      enable next 2 lines when DbgU/9004/ is used above
      call OpenFmFile(DbgU,2,2,ZeroIfExtant,B0,DirPath,FileName)
      if(ZeroIfExtant/=0) goto 902
!
      write(FileName,'(4a)') 'CnwUsage', & !  output:  fraction of MineQty alloc to Units
        AHyphen,trim(StudyName),OutSuffix
      ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
      call OpenFmFile(TxtU,2,2,ZeroIfExtant,B0,DirPath,FileName)
      if(ZeroIfExtant/=0) goto 902
!
      write(FileName,'(4a)') 'CnwBasnV', & !  output:  Volume/kT from Basins to Units
        AHyphen,trim(StudyName),OutSuffix
      ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
      call OpenFmFile(BnvU,2,2,ZeroIfExtant,B0,DirPath,FileName)
      if(ZeroIfExtant/=0) goto 902
!
      write(FileName,'(4a)') 'CnwMineV', & !  output:  Volume/kT from Mines to Units
        AHyphen,trim(StudyName),OutSuffix
      ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
      call OpenFmFile(MnvU,2,2,ZeroIfExtant,B0,DirPath,FileName)
      if(ZeroIfExtant/=0) goto 902
!
      write(FileName,'(4a)') 'CnwMCDvd', & !  output:  Marginal Cost at Units, Delivered
        AHyphen,trim(StudyName),OutSuffix
      ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
      call OpenFmFile(MdcU,2,2,ZeroIfExtant,B0,DirPath,FileName)
      if(ZeroIfExtant/=0) goto 902
!
      write(FileName,'(4a)') 'CnwACDvd', & !  output:  Average Cost at Units, Delivered
        AHyphen,trim(StudyName),OutSuffix
      ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
      call OpenFmFile(AdcU,2,2,ZeroIfExtant,B0,DirPath,FileName)
      if(ZeroIfExtant/=0) goto 902
!
      write(FileName,'(4a)') 'CnwContV', & !  output:  contract Volume/kT shipped from Mines to Units
        AHyphen,trim(StudyName),OutSuffix
      ZeroIfExtant=-1 ! OpenFmFile caller will handle absence
      call OpenFmFile(CtvU,2,2,ZeroIfExtant,B0,DirPath,FileName)
      if(ZeroIfExtant/=0) goto 902
      return
  902 call ps(1,'Error opening file in replace-mode:  '// &
        trim(FileName))
      return ! entry OpenCNWOutFiles
! -----
      entry ReportET(RETsuffix)
      call timer(EndCentiSec)
      ExecET=EndCentiSec-OrgCentiSec
      write(DbgU,'(f8.2,2a,2i11)') float(ExecET)*0.01, &
        ' seconds elapsed ',trim(RETsuffix) ! ,OrgCentiSec,EndCentiSec
      call flush_unit(int(DbgU,2))
      return

      entry ReAllocLocalsPostPruning

      call AllocateArray(StoDkT,nLink)
      call AllocateArray(ShCst ,nLink)
      call AllocateArray(EmCst,0,1,1,nLink)

      call AllocateArray(fPlntUnit,nDNod)

      call AllocateArray(ContQty,nSNod,nPlnt)
      call AllocateArray(ContCst,nSNod,nPlnt)
!
!Obs  LinkFrTo=0
!Obs  PlntQty=0.0
      StoDkT=0.0
      ShCst=0.0
      EmCst=0.0
!Obs  InfDvdCst=1e12
!Obs  SupDvdCst=0.0
      fPlntUnit=0.0
!Obs  DnOfPlnt=0
      ContQty=0.0
      ContCst=0.0
      return ! entry ReAllocLocalsPostPruning
! -----
      entry ReAllocateCNWOptimizingArrays
      call AllocateArray(cVect,nVar)
      call AllocateArray(xVect,nVar)
      call AllocateArray(A4Equiv,nVar)
      call AllocateArray(VarDesc,nVar)
!
      call AllocateArray(bVect,mReq)
      call AllocateArray(SpVec,mReq)
      call AllocateArray(ReVec,mReq)
      call AllocateArray(ReqDesc,mReq)
!
      if(.not.allocated(ConsMtxNze)) &
        ConsMtSLim=1024 ! initial limit on size of ConsMtxNze,zbColOfNze,zbRowOfNze
      call AllocateArray(ConsMtxNze,ConsMtSLim)
      call AllocateArray(zbColOfNze,ConsMtSLim)
      call AllocateArray(zbRowOfNze,ConsMtSLim)
!
      call AllocateArray(jLbVec,nLbV)
      call AllocateArray(LbValu,nLbV)
!

      call AllocateArray(jUbVec,nUbV)
      call AllocateArray(UbValu,nUbV)
!
      call AllocateArray(MineMSeg,nSNod)
      call AllocateArray(MineMCst,nSNod)
      call AllocateArray(MineACst,nSNod)
      call AllocateArray(MineCost,nSNod)
      call AllocateArray(TransCst,nSNod)
      call AllocateArray(EmissCst,nSNod)
      call AllocateArray(TransQty,nSNod)
      call AllocateArray(LnkQtySn,nSNod)
!
      call AllocateArray(LnkQtyDn,nDNod)
      call AllocateArray(BasinQty,nBasn)
      call AllocateArray(RoutQty,0,int(nRout,4)) ! 0-valued subscript covers for unexpected voids

      cVect=0.0 ! default
      bVect=0.0 ! default
      SpVec=0.0 ! default
      return ! entry ReAllocateCNWOptimizingArrays
! -----
      entry CloseCNWOutFiles
      do OutU=BnvU,CtvU
        close(OutU)
      end do
      close(TxtU)
!      close(DbgU)
      return
! -----
      entry RefreshCNWSizes
    ! these variables outline the structure of vectors xVect,cVect,bVect,ReVec
    ! n?pPt => # of ? per plant
      nRpDN=4 ! ‰kT,‰fBtu,Wtd‰fBtu,‰SO2
      nVpDN=2 ! ‰kT,SO2emiss (latter excluded from UB after 20110825)
      nVpLk=2 ! fBtu org,min fUnsc
      nVpRt=2 ! kTn shipped via nominal and overage segments
      nRpRt=1 ! ‰kT on iRout
      nRpLk=0
    ! interpret nBasnUB as 'number of basins upper-bounded'

      nNdVar=nSNdU+SumSUsd+nDNdU*nVpDN
      nLbV  =nSNdU+nDNdU+nContMax*2 +nBasnUB     
      nUbV  =nNdVar-nDNdU+nContMax*2+nRout*nVpRt &
        +nStUBUsd & !  UB on fBtu is implicit in the group constraint (sum=1)
        +nGpUBUsd &
        +nBasnUB*2 ! *2 throughout after 20120215 for soft-limit
      nVar  =nNdVar+nLkUsed*nVpLk+nRout*nVpRt &
        +nStUBUsd*2 & !  final vars aggregate SO2 by state ...
        +nGpUBUsd*2 & !  ...and by state-group ...
        +nBasnUB*2 ! ... and limit BasnQty; put at end to retain structure of nVar;
      mReq=nSNdU*2+nRout*nRpRt+ &
        nDNdU*nRpDN+nStUBUsd+nGpUBUsd & ! -nPlnt1SN-nPlnt2SN; nGpUBUsd rows aggregate x<SO2_UB
        +nBasnUB
      nContLoB=nContMax ! default until revised lower
      return ! entry RefreshCNWSizes
! -----
      entry ReportCNWSizes
      write(DbgU,'(2a/5i5,7i7,i9,i5/)') &
        ' SNod SNdU DNod DNdU Plnt', &
   '  Contr  Links  LkUsd  ARows  ACols nLoBnd nUpBnd AMtxSize SUsd', &
        nSNod,nSNdU,nDNod,nDNdU,nPlnt, &
        nContLoB,nLink,nLkUsed,mReq,nVar,nLbV,nUbV,ConsMtSize,SumSUsd ! ,nNdVar
      return
! -----
! above in ($/kT) as specified by GAT 20120625 '$1000/Ton'
      entry MinCNWCosts
      write(DbgU,'(a)') ' before allocating LP arrays'
      FullSignif=0.025 ! TestFactor ! below which fBtu is considered negligible wrt count
      ConsMtSize=0 ! count of elements used in above arrays
      call RefreshCNWSizes() ! these may have been modified below in prior iYr
      call ReportCNWSizes()
!Obs  call ReleaseCNWOptimizingArrays() ! void if cVect etc. are sufficient
      call ReAllocateCNWOptimizingArrays() ! sizes of the LP arrays can vary
      call timer(OrgCentiSec)
!
      iReq=0
      jVar=0
      iLbV=0
      iUbV=0
      ReVec=99 ! default to catch logic errors
    ! explete the arrays on the LP interface:  cVect,bVect,ReVec,ConsMtxNze
!
    ! each Supply node has 1 variable (bilaterally-bounded) for
    ! the aggregate across segmens, and nNzSegs upper-bounded variables
      do iSNod=1,nSNod
        nNzSegs=SegsUsd(iSNod)
        if(nNzSegs<=0) cycle
        call CNWIncrVar()
        write(VarDesc(jVar),'(a,i7.7)') 'kTon_mined_frm_Sply_', &
          SNodID(iSNod)
        call CNWIncrLB()
        LbValu(iLbV)=SNodQtyLB(iSNod)
      ! without the following, PCX infers that UbValu is zero when LbValu is specified above
        call CNWIncrUB()
        r4=SNodQtyUB(iSNod)
        UbValu(iUbV)=r4 ! *MineFunc(nNzSegs,0,iSNod) ! MF>1 possibly
!        write(DbgU,'(3i7,2a)') jVar,iLbV,iUbV,' ~bV ',VarDesc(jVar)
!NOT:   cVect(jVar)=SNodPri(iSNod)*1000.0 ! in ($/T)
!        write(DbgU,'(3i4,f9.3,2(1x,a))') iSNod,jVar,nSNod,cVect(jVar),
!     +    VarDesc(jVar),SNodName(iSNod)
!
        do j=1,nNzSegs
          call CNWIncrVar() ! incremented jVar is needed by CNWIncrUB()
          write(VarDesc(jVar),'(a,i2.2,a,i7.7)') 'Seg_',j, &
            '_kTon_mined_@_',SNodID(iSNod)
          call CNWIncrUB()
!          write(DbgU,'(3i7,1x,a)') jVar,iLbV,iUbV,VarDesc(jVar)
          UbValu(iUbV)=MineFunc(j,2,iSNod)*r4 ! segment-width in kT
          cVect(jVar) =MineFunc(j,1,iSNod)*1000.0 ! ($=(nk) => ($/kT)

        end do ! j
      end do ! iSNod
      if(jVar/=nNdVar-nDNdU*nVpDN) call ps(1, &
        'error assigning vars for Supply nodes')
!
    ! each Demand Node has nVpDN variables
      MinDemandkT=0.0
      do iDNod=1,nDNod
        if(PnOfDNod(iDNod)<=0) cycle
        r4=DNodQty(iDNod)/0.001 ! (MegaBtu demanded)/(M/k) => (kBtu)
!        if(r4<=0.0) cycle

        call CNWIncrVar() ! SumkT
        write(VarDesc(jVar),'(a,i4.4)') 'Sum_kTon_Rcvd_at_DNode_',iDNod
        call CNWIncrLB()
        call CNWIncrUB()
      ! bounds are implied by HeatConLB & HeatConUB (now HeatCoGLB & HeatCoLUB)
        LbValu(iLbV)=(r4/HeatCoLUB(iDNod))/2000.0 ! ((kBtu)/(Btu/Lb))/(Lb/T) => (kT)
        UbValu(iUbV)=(r4/HeatCoGLB(iDNod))/2000.0 ! kT
        MinDemandkT=MinDemandkT+LbValu(iLbV)
!
        t4=0.0
        i=0
        j=0
        k=0
!        piSNod=-1
!        piDNod=-1
        do iLink=1,nLink ! warn of INFEASIBLE link constraints at iDNod
          if(LkEndUsd(iLink)/=2) cycle
!          iLkUsed=nLkUsdAt(iLink)
          iSNod=SNodOfLk(iLink)
          if(DNodOfLk(iLink)==iDNod) then
!            iRout=RnOfLink(iLink)
            t4=t4+RoutUB(RnOfLink(iLink)) ! sum UB of routes to iDNod
            k=k+1 ! count of distinct links to iDNod
!           iCol=nNdVar+(iLkUsed-1)*nVpLk+1 ! +nContLoB
            q4=StoDkT(iLink) ! link kTn received at iDNod from iSNod
            ErrHCtLB=(LbValu(iLbV)>q4)
            ErrHCtUB=(UbValu(iUbV)<q4)
            if(ErrHCtLB) i=i+1 ! count of LB_infeasible links to iDNod
            if(ErrHCtUB) j=j+1 ! count of UB_infeasible links to iDNod

          end if
!          piSNod=iSNod
!          piDNod=DNodOfLk(iLink)
        end do ! iLink
        if(t4<LbValu(iLbV)) then
          write(DbgU,'(2(1x,g10.3,a,i9,a))') LbValu(iLbV), &
            '=Minimum kT required by Plant',EPntID(iDNod), &
            ' exceeds',t4,'=‰(route_UB) => DATA ERROR'
!Obs      LbValu(iLbV)=t4*0.125 ! KLUDGE to achieve feasibility ! not needed after 20110831
        end if
        if((k>0).and.((i==k).or.(j==k))) then
        ! all links are infeasible to one extreme,
        ! so the aggregate with any set of weights is also
          write(DbgU,'(3i5,i9,3f8.0,e10.3,a/a/1x)') iDNod,i,j, &
            EPntID(iDNod), &
            HeatConLB(iSNod), & !  e.g. use iSNod last seen above
            HeatCoGLB(iDNod), &
            HeatCoLUB(iDNod),r4,' all Supply HCs are out-of-bounds;', &
            ' DATA ERROR; HC bound(s) will be relaxed by a factor of 4'
          if(ErrHCtLB) then
            LbValu(iLbV)=LbValu(iLbV)*0.25
            HeatCoLUB(iDNod)=HeatCoLUB(iDNod)*4.0
          end if
          if(ErrHCtUB) then
            UbValu(iUbV)=UbValu(iUbV)*4.0
            HeatCoGLB(iDNod)=HeatCoGLB(iDNod)*0.25
          end if
          ErrHCtLB=(LbValu(iLbV)>q4)
          ErrHCtUB=(UbValu(iUbV)<q4)
          if(ErrHCtLB.or.ErrHCtUB) call ps(1,'ERROR is incorrigible')
        end if
!        write(DbgU,'(3i7,1x,a,2f9.1,i5)') jVar,iLbV,iUbV,VarDesc(jVar)
!     +    ,LbValu(iLbV),UbValu(iUbV),iDNod
        if(nVpDN<2) cycle
! 2
        call CNWIncrVar() ! SO2 emitted by iDNod (after scrubbing)
        write(VarDesc(jVar),'(a,i4.4)') 'SO2_emissions/kT_DNode_',iDNod
      ! emissions in above var are N.A. to BlndUB, which is imposed in iRqWSC

      end do ! iDNod
     write(DbgU,'(2e12.5,a)') MaxSupplykT,MinDemandkT,' MaxS,MinD (kT)'
      if(MaxSupplykT<=MinDemandkT) call ps(1, &
        'ERROR:  Demand exceeds Supply')
      if(jVar/=nNdVar) call ps(1, &
        'error assigning vars for Demand plants')
!
    1 format(a,i3.3,"_",i1,a,i4.4)
!
!Obs  ContFrTo=0
    ! each of nContLoB generates bounds on 1 fBtu variable
    ! by here, jVar=nNdVar
      nContLoB=0
!ifdef ContByPlant
      EvPlntID=-1
      iSNod=-1
      do iLink=1,nLink
        if(LkEndUsd(iLink)/=2) cycle
        jCol=jVar+1
        jVar=jVar+nVpLk ! to match loop below
!Obs    iPlnt=DNodOfLk(iLink)
        iDNod=DNodOfLk(iLink)
        iPlnt=PnOfDNod(iDNod)
        if(iPlnt<=0) cycle ! after 20110311, unreachable plants have 0 Demand
        piSNod=iSNod
        iSNod=SNodOfLk(iLink)
        pEPlntID=EvPlntID
        EvPlntID=LinkNdID(1,iLink)/100
        if((iSNod==piSNod).and.(EvPlntID==pEPlntID)) cycle
        r4=ContQty(iSNod,iPlnt)*fPlntUnit(iDNod) ! (kT)
        if(r4==0.0) cycle
       write(DbgU,'(i7,2i5,i7,f9.2,a)')iLink,iSNod,iDnod,EvPlntID,r4, &
          ' CQty@DNode'
        nContLoB=nContLoB+1 ! count only contracts used this year

        q4=r4/StoDkT(iLink) ! assume that contract-writer knew of fUnscNode
        do j=0,1 ! q4 bound can be met with or without scrubbers
          jVar=jCol+j
          call CNWIncrLB()
          LbValu(iLbV)=q4 ! (kT)/(kT) => (1) fraction contract/(total demand at iDnod)
        ! without the following, PCX infers that UbValu is zero when LbValu is specified above
          call CNWIncrUB()
          UbValu(iUbV)=1.0
          if(LbValu(iLbV)>1.0) then
           write(VarDesc(jVar),1)'Lnk_fBtu_frm_',iSNod,j,'_DNod',iDNod ! redundant to loop below, useful for debugging
            write(DbgU,'(4i7,1x,a,2i4,2f9.1,f6.3,a/a)') iLink,jVar, &
            iLbV,iUbV,VarDesc(jVar),nContLoB,iSNod,r4,StoDkt(iLink), &
              LbValu(iLbV),' ContQty/DNod_LB', &
              ' excessive-contract ERROR; reducing Link_fBtu LB to 1'
            LbValu(iLbV)=1.0 ! note that 1.0 may cause problems in PCX
          end if
        end do
      end do ! iLink
!
    ! expedite execution by reducing size of the LP
      write(DbgU,'(2i7,a)') nContMax,nContLoB,' contracts extant'
      k=(nContMax-nContLoB)*2
      nLbV=nLbV-k
      nUbV=nUbV-k
      jVar=nNdVar ! regress to pre-iLink-loop value; above loop did not IncrVar
!endif ContByPlant
!
    ! each link has nVpLk variables (fraction of iDnod-demanded Btu shipped), org & min fUnsc
      do iLink=1,nLink
        if(LkEndUsd(iLink)/=2) cycle
        iSNod=SNodOfLk(iLink)
        iDNod=DNodOfLk(iLink)

        do j=0,1 ! org,min fUnsc
          call CNWIncrVar() ! fBtu
          cVect(jVar)=(ShCst(iLink)+EmCst(j,iLink))*DNodQty(iDNod) ! (($/MBtu)*(MBtu) => ($)

         write(VarDesc(jVar),1)'Lnk_fBtu_frm_',iSNod,j,'_DNod',iDNod

        end do
      end do ! iLink

    ! each Route has nVpRt UB variables (kTn shipped within & above RoutUB)
      nRoutVoid=0
      do iRout=1,nRout
        k=0
        do iLink=1,nLink ! allow n>1 links to use iRout
          if(LkEndUsd(iLink)/=2) cycle
          if(RnOfLink(iLink)/=iRout) cycle
          k=k+1
          exit ! 1 suffices to imply not void
        end do ! iLink
        if(k==0) then ! iRout had no usable links
          nRoutVoid=nRoutVoid+1
          cycle
        end if
!
        call CNWIncrVar()
        cVect(jVar)=0.0 ! ShCst & EmCst affect usage of jVar
        call CNWIncrUB()
        UbValu(iUbV)=RoutUB(iRout)
        write(VarDesc(jVar),'(a,i4.4)') 'kT within UB via Rout_',iRout
!
        call CNWIncrVar()
        cVect(jVar)=20e3 ! $20/Ton surcharge for overuse of route per GAT
        call CNWIncrUB()
        UbValu(iUbV)=RoutUB(iRout)*999.0
        write(VarDesc(jVar),'(a,i4.4)') 'kT above  UB via Rout_',iRout
      end do
      write(DbgU,'(i6,a)') nRoutVoid,' transport-routes void'
      nVar=nVar-nRoutVoid*nVpRt
      nUbV=nUbV-nRoutVoid*nVpRt
!
    ! next nStUBUsd*2 variables are statewide-aggregate SO2 emissions,
    ! upper-bounded and, aft 20120626, split into infra- and supra-nominal_UB
!Dbg  write(DbgU,'(3i7,a)') iStUBSup,jVar,nVar,' bef SO2 vars'
      StGroupUB=0.0
      do iPolState=0,iStUBSup
        if(StUBUsed(iPolState).eqv.B0) cycle
        call CNWIncrVar() ! costs were imposed on the link-variables above
        write(VarDesc(jVar),'(2a)') 'SO2_emissn<UB/kTn_market_', &
          SO2MarketCodes(iPolState)
!Not    call CNWIncrLB() ! Garrick Hoops 20110730 denied need for LB on state emissions
        call CNWIncrUB()
        q4=SO2EmissionLimits(iPolState) ! (kT)
        iPolStGrp=StateGroupAssignment(iPolState)
        if(iPolStGrp>0) then
        ! any state in group may exceed its nominal limit by its 'Volatility'
          StGroupUB(iPolStGrp)=StGroupUB(iPolStGrp)+q4 ! group limit is sum of nominals
          f4=StateCapVolatility(iPolState) ! normalized fraction on [0,1]

          q4=q4*(1.0+f4)
        else
          f4=0.0

        end if
        if(q4<=0.0) q4=9e7 ! KLUDGE to cover MSG problem

        UbValu(iUbV)=q4 ! 999999.0 ! KLUDGE

        call CNWIncrVar() ! impose penalty but no UB on supra-nominal_UB
        cVect(jVar)=1e9 ! $/kT SO2 cost on supra-nominal_UB 20120626
        write(VarDesc(jVar),'(2a)') 'SO2_emissn>UB/kTn_market_', &
          SO2MarketCodes(iPolState)
      end do ! iPolState
!
    ! next nGpUBUsd*2 variables are infra- and supra-nominal_UB
      do iPolStGrp=1,2
        if(nInGroup(iPolStGrp)==0) cycle
        call CNWIncrVar()
!       cVect(iReq)=0.0 ! for infra-nominal
        call CNWIncrUB()
        UbValu(iUbV)=StGroupUB(iPolStGrp)
        write(VarDesc(jVar),'(a,i1)') 'SO2_emissn<UB/kTn_StGroup_', &
          iPolStGrp
!Dbg    write(DbgU,'(3i7,f11.2,1x,a)') jVar,iLbV,iUbV,
!Dbg +    UbValu(iUbV),VarDesc(jVar)
!
        call CNWIncrVar() ! impose penalty but no UB for supra-nominal_UB
        cVect(jVar)=1e9 ! $/kT SO2 cost on supra-nominal_UB 20120626
        write(VarDesc(jVar),'(a,i1)') 'SO2_emissn>UB/kTn_StGroup_', &
          iPolStGrp
!Dbg    write(DbgU,'(3i7,12x,a)') jVar,iLbV,iUbV,VarDesc(jVar)
      end do
!Dbg  write(DbgU,'(3i7,a)') iStUBSup,jVar,nVar,' aft SO2 vars'
!
    ! limit the sum of Supply nodes' kT from basins
      do iBasn=1,nBasn
!Obs    if(BasnQtyUB(iBasn)>=999998.0) cycle ! ignore both bounds on this basin
!
        call CNWIncrVar() ! for the quantity below the nominal_UB
        write(VarDesc(jVar),'(a,i3.3)') 'SNod_sum_kT<UB_fr_Basin_', &
          iBasn
        call CNWIncrLB()
        call CNWIncrUB()
        LbValu(iLbV)=BasnQtyLB(iBasn)
        UbValu(iUbV)=BasnQtyUB(iBasn)
        write(DbgU,'(3i7,2i11,1x,a)') jVar,iLbV,iUbV, &
          nint(LbValu(iLbV)),nint(UbValu(iUbV)),VarDesc(jVar)
        if(LbValu(iLbV)>UbValu(iUbV)) call ps(1,'bounds disorder')
!
        call CNWIncrVar() ! for the quantity above the nominal_UB
        cVect(jVar)=1e6 ! large surcharge to penalize violation of UB
        write(VarDesc(jVar),'(a,i3.3)') 'SNod_sum_kT>UB_fr_Basin_', &
          iBasn
        call CNWIncrUB() ! LB is implicitly 0
        UbValu(iUbV)=BasnQtyUB(iBasn) ! allow 100% overproduction
      end do
!
      write(DbgU,'(2i7,a)') jVar,nVar,' aft all vars'
      call DulyNote('after defining LP variables')
      call ReportCNWSizes() ! after possible changes above
      if(jVar/=nVar) call ps(1,'error assigning vars for links etc.')
      if(iLbV/=nLbV) nLbV=iLbV ! call ps(1,'error counting LB-vars')
      if(iUbV/=nUbV) nUbV=iUbV ! call ps(1,'error counting UB-vars')
!
!Dbg! KLUDGE (except zeroes) failed to regain feasibility 20110731
!Dbg  nLbV=nSNdU ! 0 ! nSNdU+nDNdU
!Dbg  nUbV=nSNdU+SumSUsd ! 0 ! nNdVar
!
!
    ! each Supply-node generates 2 constraints (row in ConsMtx)
      jCol=0
      do iSNod=1,nSNod
        nNzSegs=SegsUsd(iSNod)
        if(nNzSegs<=0) cycle
        call CNWIncrReq() ! net sum across segments of kT I/O at iSNod
        ReVec(iReq)=0 ! for EQL
        write(ReqDesc(iReq),'(a,i7.7)') ' SegmentSum_kTon_SNd_', &
          SNodID(iSNod)
        jCol=jCol+1
        iCol=jCol ! save for use below
        call AssignCNWAMtx(jCol,iReq,-1.0) ! sum in kT supplied by iSNod
        do jSeg=1,nNzSegs
          jCol=jCol+1
          call AssignCNWAMtx(jCol,iReq,1.0) ! segment kT mined at iSNod
        end do
!
        call CNWIncrReq() ! sum of kT supplied at iSNod equals kT shipped via links
        ReVec(iReq)=0 ! for EQL
        write(ReqDesc(iReq),'(a,i7.7)') ' Sum/Link_kTon_SNode_', &
          SNodID(iSNod)
        call AssignCNWAMtx(iCol,iReq,-1.0) ! kT sum supplied by iSNod
        do iLink=1,nLink
          if(LkEndUsd(iLink)/=2) cycle
          if(SNodOfLk(iLink)/=iSNod) cycle
          iLkUsed=nLkUsdAt(iLink)

          iCol=nNdVar+(iLkUsed-1)*nVpLk+1 ! +nContLoB
          call AssignCNWAMtx(iCol  ,iReq,StoDkT(iLink)) ! kT shipped from iSNod via iLink, org fUnscNode
          call AssignCNWAMtx(iCol+1,iReq,StoDkT(iLink)) ! kT shipped from iSNod via iLink, tgt fUnscNode
        end do
      end do ! iSNod

      if((jCol/=nNdVar-nDNdU*nVpDN).or.(iReq/=nSNdU*2)) then
        call ps(1,'error forming constraints for Supply nodes')
      endif

      write(DbgU,'(2i7,a)') iReq,mReq,' constraints'
      call DulyNote('after forming Supply constraints')
!
    ! each Demand Node generates nRpDN constraints (row in ConsMtx)
      jCol=nNdVar-nDNdU*nVpDN-(nVpDN-1)
      do iDNod=1,nDNod
        if(PnOfDNod(iDNod)<=0) cycle

        jCol=jCol+nVpDN

        do i=1,nRpDN
          call CNWIncrReq()
          ReVec(iReq)=0 ! for EQL
          if(i==1) then ! x(jCol) kTn is lower- and upper-bounded by Btu requirements
          
        write(ReqDesc(iReq),'(a,i3.3)') ' Sum/Link_kTon_Rcvd_DNode', &
              iDNod
            call AssignCNWAMtx(jCol,iReq,-1.0) ! expect x(jCol) to be positive
            iRqkTn=iReq
          elseif(i==2) then
          ! sum across links the fBtu received at iDNod
            bVect(iReq)=1.0 ! NOT DNodQty(iDNod) ! MegaBtu demanded
        write(ReqDesc(iReq),'(a,i3.3)') ' Sum/Link_fBtu_Rcvd_DNode', &
              iDNod
            iRqfBt=iReq
          elseif(i==3) then
          ! accumulate the fBtu-weighted average rate of SO2/MegaBtu
            ReVec(iReq)=1 ! for LEQ
            bVect(iReq)=SO2BlndUB(iDNod) ! NOT =1000.0) 
        write(ReqDesc(iReq),'(a,i3.3)') ' Avg/Link_SO2inBlend_DNod', &
              iDNod
            iRqWSC=iReq ! mnemnic WeightedSulfurContent in blend of coals used
          elseif(i==4) then
          ! sum across links the kTn of SO2 emitted by iDNod

         write(ReqDesc(iReq),'(a,i3.3)') ' Sum/Link_SO2kT_Emis_DNod', &
              iDNod
            call AssignCNWAMtx(jCol+1,iReq,-1.0) ! expect x(jCol) to be positive
            iRqSO2=iReq
          end if

        end do ! i=1,nRpDN
!
!Obs    k=0
!        QtySum=0.0
        do iLink=1,nLink ! allow n>1 links to supply iDNod
          if(LkEndUsd(iLink)/=2) cycle
          if(DNodOfLk(iLink)/=iDNod) cycle
          iLkUsed=nLkUsdAt(iLink)
!Obs      k=k+1
          iSNod=SNodOfLk(iLink)
          iCol=nNdVar+(iLkUsed-1)*nVpLk+1 ! +nContLoB
          f4=SO2Content(iSNod) ! (LbSO2/MBtu) ! same units as SO2BlndUB above
          q4=StoDkT(iLink)
          r4=q4*f4*SnHeatCon(iSNod)/(1.0e6) 
          do j=0,1
            call AssignCNWAMtx(iCol+j,iRqkTn,q4   ) ! link kTn received at iDNod
            call AssignCNWAMtx(iCol+j,iRqfBt,1.0  ) ! link fBtu received at iDNod
            call AssignCNWAMtx(iCol+j,iRqWSC,f4) ! link SO2Content*fBtu at iDNod
            call AssignCNWAMtx(iCol+j,iRqSO2,r4 & !  link SO2/kT received at iDNod
              *fUnscNode(j*2,iDNod)) ! original for j=0
          end do

        end do

      end do ! iDNod
      if((jCol+nVpDN-1/=nNdVar).or.(iReq/=nSNdU*2+nDNdU*nRpDN)) &
        call ps(1,'error forming constraints for Demand Nodes')
      write(DbgU,'(2i7,a)') iReq,mReq,' constraints'
      call DulyNote('after forming Demand constraints')
!
      t4=0.0
      nRoutVoid=0
      do iRout=1,nRout
        call CNWIncrReq()
        k=0
        do iLink=1,nLink ! allow n>1 links to use iRout
          if(LkEndUsd(iLink)/=2) cycle
!          iPlnt=DNodOfLk(iLink)
!         if(iPlnt<=0) cycle ! after 20110311, unreachable plants have 0 Demand
          if(RnOfLink(iLink)/=iRout) cycle
          iLkUsed=nLkUsdAt(iLink)
          iCol=nNdVar+(iLkUsed-1)*nVpLk+1
          do j=0,1
            call AssignCNWAMtx(iCol+j,iReq,StoDkT(iLink)) ! link kTn shipped via iRout
          end do
          k=k+1
        end do ! iLink
        if(k==0) then ! iRout had no usable links
          nRoutVoid=nRoutVoid+1
          iReq=iReq-1
          cycle
        end if

        ReVec(iReq)=0 ! for EQL
        bVect(iReq)=0.0
        iCol=nNdVar+nLkUsed*nVpLk+(iRout-nRoutVoid)*nVpRt
        call AssignCNWAMtx(iCol-1,iReq,-1.0) ! kTn shipped via iRout in bounds
        call AssignCNWAMtx(iCol  ,iReq,-1.0) ! kTn shipped via iRout above UB
        t4=t4+RoutUB(iRout)
      write(ReqDesc(iReq),'(a,i4.4)') ' Sum/Link_kTon_Rcvd_Rout',iRout

      end do ! iRout
      write(DbgU,'(i6,a)') nRoutVoid,' transport-routes void'
      write(DbgU,'(i6,e12.5,a)') nRout-nRoutVoid,t4, &
        ' total route_limit on Qty/kT'
      mReq=mReq-nRoutVoid*nRpRt ! nVar was reduced by nRoutVoid*nVpRt above
      if((jCol+nVpDN-1/=nNdVar).or.(iReq/=nSNdU*2+nDNdU*nRpDN &
        +(nRout-nRoutVoid)*nRpRt)) &
        call ps(1,'error forming constraints for links')
      write(DbgU,'(2i7,a)') iReq,mReq,' constraints'
      call DulyNote('after forming Link-Route constraints')
!
    ! nStUBUsd+nGpUBUsd constraints limit aggregate SO2 emissions
      iCol=nVar-nBasnUB*2-nGpUBUsd*2-nStUBUsd*2
      do iPolState=0,iStUBSup
        if(StUBUsed(iPolState).eqv.B0) cycle
        call CNWIncrReq()
        ReVec(iReq)=0 ! for EQL

        write(ReqDesc(iReq),'(2a)') ' SO2_emissions/kTn_market_', &
          SO2MarketCodes(iPolState)
!Dbg    write(DbgU,'(/2i7,a)') iReq,iPolStGrp,ReqDesc(iReq)
        iCol=iCol+1
        call AssignCNWAMtx(iCol,iReq,-1.0) ! iCol has this state's SO2<UB
        iCol=iCol+1
        call AssignCNWAMtx(iCol,iReq,-1.0) ! iCol has this state's SO2>UB
        f4=0.0
        do iLink=1,nLink
          if(LkEndUsd(iLink)/=2) cycle
          iDNod=DNodOfLk(iLink)

          if(iPolState>0) then
!            iDNod=DnOfPlnt(iPlnt)
            if(StOfDNod(iDNod)/=iPolState) cycle
        ! else all units contribute to SO2 for NA at iPolState==0
          end if
          iSNod=SNodOfLk(iLink)
          iLkUsed=nLkUsdAt(iLink)
          jCol=nNdVar+(iLkUsed-1)*nVpLk+1 ! +nContLoB ! index of fBtu shipped via iLink

          r4=(SO2Content(iSNod)*SnHeatCon(iSNod)/(1.0e6))*StoDkT(iLink)
          do j=0,1
            p4=fUnscNode(j*2,iDNod) ! original for j=0
            f4=f4+r4*p4 ! sum (kTSO2) across nLink coefficients
            call AssignCNWAMtx(jCol+j,iReq,r4*p4)

          end do
        end do ! iLink
        if(iPolState==0) then
            write(DbgU,'(i7,f9.6,a)') &
             nLink,f4/((1.0e6)*float(nLink)),&
             ' mean AMtxReSO2 (GT/link)', &
             nLink,f4/1.0e6,' sum AMtxReSO2/GT'
        endif
      end do ! iPolState
    ! impose the State-Group limits
      iCol=nVar-nBasnUB*2-nGpUBUsd*2
      do iPolStGrp=1,2
        if(nInGroup(iPolStGrp)==0) cycle
        call CNWIncrReq()
!Obs    ReVec(iReq)=1 ! for LEQ
        ReVec(iReq)=0 ! for EQL
!Obs    bVect(iReq)=StGroupUB(iPolStGrp)
        bVect(iReq)=0.0
        write(ReqDesc(iReq),'(a,i1)') ' SO2_emissions/kTn_StateGrp', &
          iPolStGrp
!Dbg    write(DbgU,'(/2i7,a)') iReq,iPolStGrp,ReqDesc(iReq)
        iCol=iCol+1
        call AssignCNWAMtx(iCol,iReq,-1.0) ! iCol has group's SO2<UB
        iCol=iCol+1
        call AssignCNWAMtx(iCol,iReq,-1.0) ! iCol has group's SO2>UB
        jCol=nVar-nBasnUB*2-nStUBUsd*2
        do iPolState=0,iStUBSup
          if(StUBUsed(iPolState).eqv.B0) cycle
          jCol=jCol+1
          if(iPolStGrp/=StateGroupAssignment(iPolState)) cycle
          call AssignCNWAMtx(jCol,iReq,1.0) ! jCol has this state's SO2<UB
          jCol=jCol+1
          call AssignCNWAMtx(jCol,iReq,1.0) ! jCol has this state's SO2>UB

        end do
      end do ! iPolStGrp
!
      iCol=nVar-nBasnUB*2

      do iBasn=1,nBasn

        iCol=iCol+1
        jCol=1
        call CNWIncrReq()
        ReVec(iReq)=0 ! for EQL
        bVect(iReq)=0.0
        write(ReqDesc(iReq),'(a,i3.3)') ' SNod_sum_kTon_frm_Basin_', &
          iBasn
        call AssignCNWAMtx(iCol,iReq,-1.0)
        iCol=iCol+1 ! for excess above nominal_UB; added 20120215
        call AssignCNWAMtx(iCol,iReq,-1.0)
!Dbg    write(DbgU,'(/2i7,a)') iReq,iCol,ReqDesc(iReq)
        do iSNod=1,nSNod
          if(SegsUsd(iSNod)<=0) cycle
          if(BnOfSNod(iSNod)==iBasn) then

            call AssignCNWAMtx(jCol,iReq,1.0) ! iSNod kT supplied from basin

          end if
          jCol=jCol+1+SegsUsd(iSNod)
        end do
      end do

      if(iReq/=mReq) call ps(1,'error forming SO2_limit constraints')
!
!
      call DulyNote('after filling Constraint Matrix')
      call ReportCNWSizes()


      call ReportET('prior to sorting of LP AMtx')
    ! sort to facilitate search within ReturnCNWAMtx and for order within PCX
      call SortByAscendingKey( &
        ConsMtSize,zbRowOfNze,zbColOfNze,ConsMtxNze)
      call DulyNote('after sorting Constraint Matrix')
      call ReportET('thru the sorting of LP AMtx')

      if                   (nVar< 100)  then

        write(DbgU,'(13x,a)') 'ConsMtx'
        write(DbgU,'(a,100i4.3/(12x,100i4.3))') &
          ' iRq Re bVec',(jVar,jVar=1,nVar)

        do iReq=1,mReq ! display entire constraint matrix
          do jVar=1,nVar
          ! needed to preclude 'initiating I/O during I/O' error
            A4Equiv(jVar)=R4toA4(FuncCNWAMtx(jVar,iReq)) ! very slow
          end do
          bVa4=R4toA4(bVect(iReq))
          write(DbgU,'(i4,i3,a5,100a/(12x,100a))') iReq,ReVec(iReq), &
            bVa4,(A4Equiv(jVar),jVar=1,nVar),ReqDesc(iReq)
          if(iReq==nSNod*2) then
              write(DbgU,'(1x)')
          endif
        end do ! iReq
        write(DbgU,'(1x)')
!
        do jVar=1,nVar
        ! needed to preclude 'initiating I/O during I/O' error
          A4Equiv(jVar)=R4toA4(cVect(jVar))
        end do
        write(DbgU,'(a,100a4/(12x,100a4))') '        cVec', &
          (A4Equiv(jVar),jVar=1,nVar)
!        write(DbgU,'(1x)')
!
        iUbV=1
        do jVar=1,nVar
          if(jUbVec(iUbV)==jVar) then
            A4Equiv(jVar)=R4toA4(UbValu(iUbV))
            if(iUbV<nUbV) iUbV=iUbV+1
          else
            A4Equiv(jVar)='   _'
          end if
        end do
        write(DbgU,'(a,100a4/(12x,100a4))') '       UbVal', &
          (A4Equiv(jVar),jVar=1,nVar)
!
        iLbV=1
        do jVar=1,nVar
          if(jLbVec(iLbV)==jVar) then
            A4Equiv(jVar)=R4toA4(LbValu(iLbV))
            if(iLbV<nLbV) iLbV=iLbV+1
          else
            A4Equiv(jVar)='   _'
          end if
        end do
        write(DbgU,'(a,100a4/(12x,100a4))') '       LbVal', &
          (A4Equiv(jVar),jVar=1,nVar)
        write(DbgU,'(1x)')
!
        if(nVar<=100) then
          do jVar=nVar,2,-1
            write(DbgU,'(15x,100a)') ('|   ',j=2,jVar),VarDesc(jVar)
          end do
            write(DbgU,'(15x,100a)')                   VarDesc(1)
        end if
!
!
!  end of debugging stuff appropriate only for small problems

      elseif(nVar<1000) then

        write(DbgU,'(/(a))') '   jVar  cVector(j)', & !    cVector(j)',
                             ' ------ -----------'  ! ------------'
        do jVar=1,nVar

          write(DbgU,'(i7,f12.0,      1x,a)') jVar, & !  cVect(jVar),
            cVect(jVar),VarDesc(jVar)
        end do
        write(DbgU,'(1x)')

        write(DbgU,'(a)') '   iReq Re  bVector(i)' !   bVector(i)'
        write(DbgU,'(a)') ' ------ -- -----------' ! ------------'
        do iReq=1,mReq
          write(DbgU,'(i7,i3,f12.2,      a)') iReq,ReVec(iReq), &
            bVect(iReq),ReqDesc(iReq)
        end do
        write(DbgU,'(1x)')

      end if

      call timer(OrgCentiSec)
      return ! entry MinCNWCosts
! -----
      entry CheckCNWVectors
      write(DbgU,'(/i6,a/(20i3  ))') nSNod,' #SegsData:', &
        (Supply_Curve_Points(j),j=1,nSNod)
      write(DbgU,'(2i7,f12.2,2a)') (iLbV,jLbVec(iLbV),LbValu(iLbV), &
        ' LoB ',VarDesc(jLbVec(iLbV)),iLbV=1,nLbV)
      do iLbV=1,nLbV
        if((0.0>LbValu(iLbV)).or.(LbValu(iLbV)>1e32)) call ps(1, &
          ' LbValu out of bounds')
      end do
!
      write(DbgU,'(2i7,f12.2,2a)') (iUbV,jUbVec(iUbV),UbValu(iUbV), &
        ' UpB ',VarDesc(jUbVec(iUbV)),iUbV=1,nUbV)
      do iUbV=1,nUbV
        if((0.0>UbValu(iUbV)).or.(UbValu(iUbV)>1e32)) call ps(1, &
          ' UbValu out of bounds')
      end do
!
!      write(DbgU,'(i7,f12.2,a)') (jVar,cVect(jVar),
!     +  ' cVect',jVar=1,nVar)
      write(DbgU,'(i7,i2,f12.2,a)') (iReq,ReVec(iReq),bVect(iReq), &
        ' r&b',iReq=1,mReq)
      do iReq=1,mReq
        if((0>ReVec(iReq)).or.(ReVec(iReq)>2)) call ps(1, &
          ' ReVec out of bounds')
      end do
!
      if(ConsMtSize<2000) write(DbgU,'(i10,2i7,f12.2,a)') (iNze, &
        zbColOfNze(iNze),zbRowOfNze(iNze),ConsMtxNze(iNze), &
        ' c,r,vNze',iNze=1,ConsMtSize)
      jCol=zbColOfNze(1)
      do iNze=1,ConsMtSize

        iCol=jCol
        jCol=zbColOfNze(iNze)
        if(jCol>iCol+1) then
        ! ConsMtxNze was sorted such that jVar varies slower than iReq
          write(DbgU,'(i10,3i7,f12.2,a)') iNze,zbRowOfNze(iNze),iCol, &
            jCol,ConsMtxNze(iNze),' iNZ,r,c0,c1,vNZ beyond the void'
          call ps(1,'column var has no NZ entries in ConsMtx')
        end if
      end do
      call flush_unit(int(DbgU,2))
      return ! entry CheckCNWVectors
! -----
!define call_AppendF8p2(u,r,b) write(u,'( f8.2,",")',advance='no') r
      entry SolveCNWLpUsingPcx
      if(((nVar<2000).or.(nDNod<20)).and.(iFocus==1)) then
        call CheckCNWVectors()
      else
        do iLbV=1,nLbV
          if((0.0>LbValu(iLbV)).or.(LbValu(iLbV)>1e32)) then
        write(DbgU,'(2i7,f12.2,2a)') iLbV,jLbVec(iLbV),LbValu(iLbV), &
              ' LoB ',VarDesc(jLbVec(iLbV))
            call ps(1,' LbValu out of bounds')
          end if
        end do
        do iUbV=1,nUbV
          if((0.0>UbValu(iUbV)).or.(UbValu(iUbV)>1e32)) then
        write(DbgU,'(2i7,f12.2,2a)') iUbV,jUbVec(iUbV),UbValu(iUbV), &
              ' UpB ',VarDesc(jUbVec(iUbV))
            call ps(1,' UbValu out of bounds')
          end if
        end do
      ! check for bounds disorder
        iLbV=1
        iUbV=1
        do jVar=1,nVar
          if(jLbVec(iLbV)/=jVar) cycle
          do while(iUbV<=nUbV) ! each jVar with LB must also have UB
            if(jUbVec(iUbV)==jLbVec(iLbV)) exit
            iUbV=iUbV+1
          end do
          if(iUbV>nUbV) then
            write(DbgU,'(4i7,a)') jVar,iLbV,iUbV,nUbV,' jV,iL,iU,nU'
            if(nUbV>0) call ps(1,'no UB found for LB')
          end if
          if(LbValu(iLbV)>UbValu(iUbV)) write(DbgU,'(3i7,2e12.5,2a)') &
            jVar,iLbV,iUbV,LbValu(iLbV),UbValu(iUbV),' LB>UB error ', &
            VarDesc(jVar)
          if(iLbV<nLbV) iLbV=iLbV+1
        end do
      end if
      call timer(OrgCentiSec)
      write(BaseFileName,'(a6,i2.2,a1)') 'PCX_yr',iYr,char(0) ! C-language string-terminator
      xVect=-0.5
      write(DbgU,'(a)') ' next statement calls PCX_main '
      call DulyNote(' ') ! C-language routines begin writing without CrLf
      call flush_unit(int(DbgU,2))
      PCX_ret_code=PCX_main_sp(MinObj,WrtDetail,BaseFileName, &
        mReq,nVar,nUbV,nLbV,ConsMtSize, &
        ReVec,jUbVec,jLbVec, &
        zbColOfNze,zbRowOfNze, &
        bVect,cVect,xVect,SpVec,UbValu,LbValu,ConsMtxNze)
!
      call ReportET('during PCX optimization')

      Optimal=(PCX_ret_code==0) ! 20090908 PCX returns Solution->Status

      return ! entry SolveCNWLpUsingPcx
! -----
      entry CheckCNWSolution

      call DulyNote('Checking heat-content re bounds')

      write(DbgU,'(2a)') &
        ' iDNod PYrHeat HtCoGLB   CYrHeat   HtCoLUB', &
     ' GLB/PY CYH/PY LUB/PY', & !  fUnPt0 fUnPt1 fUnPt2 mResid #~Biny',
        ' ----- ------- -------   -------   -------', &
        ' ------ ------ ------'  !  ------ ------ ------ ------ ------'
      do iDNod=1,nDNod
        if(PnOfDNod(iDNod)<=0) cycle
        h4=HeatConAv(0,iDNod) ! prior-year
        r4=HeatConAv(1,iDNod) ! current-year optimum
        p4=HeatCoGLB(iDNod)
        q4=HeatCoLUB(iDNod)

        write(DbgU,'(i6,2f8.1,2(a2,f8.1),3f7.3,1x,a)') iDNod, & 
          h4,p4,QuestionIf(0.999*p4>r4), & !  0.999 and 1.001 reduce false alarms
          r4,QuestionIf(r4>q4*1.001),q4, &
          p4/h4,r4/h4,q4/h4, &
          DNodName(iDNod)
!        if(r4<5000.0) call ps(1,'ERROR: de minimal HeatConAv')
      ! restore fUnscNode in cases of node not used }
        if(fUnscNode(1,iDNod)<0.0) then
           fUnscNode(1,iDNod)=fUnscNode(0,iDNod)
        write(DbgU,'(i6,a)') iDNod,' iDNod unused, org fUnsc restored'

        end if
      end do
!
    ! display shadow prices
      write(DbgU,'(/a)') ' Constraint RHS & Shadow Price'
      write(DbgU,'(i7,i2,2f18.6,a)') (iReq,ReVec(iReq),bVect(iReq), &
        SpVec(iReq),ReqDesc(iReq),iReq=1,mReq)

    ! check lower-bounded vars
      call DulyNote('Checking LB-vars')
      do iLbV=1,nLbV
        jVar=jLbVec(iLbV)

        r4=xVect(jVar)
        f4=LbValu(iLbV)
        if(f4==0.0) cycle ! this LbValue is not of interest
        if(f4<0.01) then
          f4=1.0 ! preclude attempt to divide by 0
        else
          f4=r4/f4 ! should be >=1
        end if
        if(iLbV<=nSNod) then
          jCol=iLbV ! each node is lower-bounded
        elseif(iLbV<=nSNod+nDNdU) then
          jCol=iLbV-nSNod ! report the index on [1,nDNdU]
        else
          jCol=iLbV-(nSNod+nDNdU) ! report the index on [1,nContLoB]
        end if
        if((PrtDetail>1).or.(f4<0.999)) &
          write(DbgU,'(3i6,2f11.3,f9.3,a2,2a)') iLbV,jVar,jCol, &
        r4,LbValu(iLbV),f4,QuestionIf(f4<0.999),' xj,LB,Xj/LB for ', &
          trim(VarDesc(jVar))
      end do
      write(DbgU,'(1x)')
!
    ! check upper-bounded vars
      call DulyNote('Checking UB-vars')
      do iUbV=1,nUbV
        jVar=jUbVec(iUbV)

        r4=xVect(jVar)
        f4=UbValu(iUbV)
        if(f4>0.5*1e12) cycle ! this UbValu was a dummy to keep PCX happy
        if(f4<0.01) then
          f4=r4 ! as if UbValu were 1.0
        else
          f4=r4/f4 ! should be <=1
        end if
        if(jVar<=nNdVar-nDNdU*nVpDN) then

          k=0
          do jCol=1,nSNod
            k=k+1+SegsUsd(jCol)
            if(k>=jVar) exit
          end do ! with jCol=iSNod
        elseif(jVar<=nNdVar) then
          jCol=jVar-(nNdVar-nDNdU*nVpDN) ! for iDNod vars
        elseif(jVar<=nNdVar+nContLoB) then
          jCol=jVar-nNdVar ! for Cont UB-vars
        else
          jCol=0 ! flag the final vars for aggregate SO2
        end if
        if((PrtDetail>1).or.(f4>1.001)) &
          write(DbgU,'(3i6,2f11.3,f9.5,a2,2a)') iUbV,jVar,jCol, &
        r4,UbValu(iUbV),f4,QuestionIf(f4>1.001),' xj,UB,Xj/UB for ', &
          trim(VarDesc(jVar))
      end do
      write(DbgU,'(1x)')
!
      call DulyNote('Checking link inequalities')
      iReq=nSNod*2
      jCol=nNdVar-nVpLk
      do iLink=1,nLink
        if(LkEndUsd(iLink)/=2) cycle
        if(mReq>=100) exit
        iDNod=DNodOfLk(iLink)
!       if(iPlnt<=0) cycle ! after 20110311, unreachable plants have 0 Demand
        jCol=jCol+nVpLk
        do j=1,nRpLk
          iReq=iReq+1
          if(ReVec(iReq)/=1) cycle
          r4=bVect(iReq)
          QtySum=0.0
          do i=1,nVpLk
            jVar=jCol+i
            f4=FuncCNWAMtx(jVar,iReq)
            if(f4==0.0) cycle
            QtySum=QtySum+f4*xVect(jVar)
           write(DbgU,'(i7,3f12.4,1x,a)') jVar,QtySum,f4,xVect(jVar), &
              VarDesc(jVar)
          end do
          if(r4<=0.0) then
            f4=1.0
          else
            f4=QtySum/r4
          end if
          write(DbgU,'(i6,f12.4,f6.3,f8.5,a2,i2,2a)') iReq,QtySum, &
            r4,f4,QuestionIf(f4>1.001),ReVec(iReq), &
            ' constraint',ReqDesc(iReq)
        end do ! j=1,nRpLk
!Obs    write(DbgU,'(1x)')
      end do ! iLink
!
!
      if(mReq>99) return
      call DulyNote('Checking equalities')
      do iReq=nSNod*2+nLink*nRpLk+1,mReq
        if(ReVec(iReq)/=0) cycle
        r4=bVect(iReq)
        QtySum=0.0
        jCol=nNdVar-nVpLk
        do iLink=1,nLink
          if(LkEndUsd(iLink)/=2) cycle
          iDNod=DNodOfLk(iLink)

          jCol=jCol+nVpLk

            do i=1,nVpLk
              jVar=jCol+i
              f4=FuncCNWAMtx(jVar,iReq)
              if(f4==0.0) cycle
              QtySum=QtySum+f4*xVect(jVar)
           write(DbgU,'(i7,3f12.4,1x,a)') jVar,QtySum,f4,xVect(jVar), &
                VarDesc(jVar)
            end do
        end do ! iLink
        if(r4<=0.0) then
          f4=1.0
        else
          f4=QtySum/r4
        end if
        write(DbgU,'(i6,f12.4,f6.3,f8.5,a2,i2,2a)') iReq,QtySum, &
          r4,f4,QuestionIf(abs(f4-1.0)>0.01),ReVec(iReq), &
          ' constraint',ReqDesc(iReq)
        write(DbgU,'(1x)')
      end do
!
      iCol=nVar-nBasnUB*2 ! *2 after 20120215
!Obs  if(iBnUB>0) then
      do iBasn=1,nBasn
        r4=BasnQtyUB(iBasn)
!Obs    if(r4>=999998.0) cycle
        iCol=iCol+1
        QtySum=xVect(iCol)
        iCol=iCol+1 ! skip column of excess after 20120215
        if(r4<=0.0) then
          f4=QtySum
        else
          f4=QtySum/r4
        end if
        write(DbgU,'(i6,2f9.1,f8.5,a2,2a)') mReq-1,QtySum, &
          r4,f4,QuestionIf(f4>1.01),' constraint LEQ', & !  ReqDesc(mReq-1) while unique
          ReqDesc(mReq-(nBasn-iBasn)) ! was -(nVar-iCol)) before 20120215
        write(DbgU,'(1x)')
      end do
!Obs  end if
!
      call flush_unit(int(DbgU,2))
      return ! entry CheckCNWSolution
! -----
      entry WriteCommonTitles
      write(OutU,'(a)',advance='no') ' '
      call AppendStr(OutU,'Endpoint')
      call AppendStr(OutU,'Year')
      call AppendStr(OutU,'Unit Name')
      call AppendStr(OutU,'EV Plant ID')
      call AppendStr(OutU,'EV Unit ID')
      return
! -----
      entry WriteCsvPrefix(SecID) ! was (PriID,SecID)
      write(OutU,'(1x)',advance='no') ! expect advance=no to follow
      call AppendI2w5(OutU,iSc)
      call AppendI2w5(OutU,cYr)
      call AppendStr (OutU,ThisNdName)
!Obs  call AppendI2w5(OutU,PriID)
      write(OutU,'( i11,a1)',advance='no') i4,','
      call AppendI2w5(OutU,SecID)
      return
! -----
      entry ExpleteFinal4Col
      do j=1,4
        call AppendF8p2(OutU,0.0,B1)
      end do
      return
! -----
      entry ReportCNWSolutionDetails(BaseYr,FocusScrubbing)
      cYr=BaseYr+iYr
      call timer(OrgCentiSec)
      write(OneLine,'(a,3i3)') 'Results for Model-Year,iter,iFocus', &
        iYr,RepInYr,iFocus
      write(TxtU,'(//1x,a/)') trim(OneLine)
      call DulyNote(OneLine)
!include "TypesLtd.fp1"
!
      t4=0.0
      do jVar=1,nSNdU+SumSUsd
        if(cVect(jVar)==0.0) t4=t4+xVect(jVar) ! kT mined from Supply
      end do
      write(DbgU,'(/e12.5,a)') t4,' LP_optimal total Supply Qty/kT'
!
      OFV=0.0d0
      SO2Penalty=0.0d0
      ContraCost=0.0d0
      ContraValu=0.0d0
      nCTObjBias=0.0d0
      if(PrtDetail>1) write(DbgU,'(/2a/2a)') &
        '   jVar      c(jVar)      x(jVar) kx(jVar) ObjctFunc', &
        '  SO2_cost', &
        ' ------ ------------ ------------ -------- ---------', &
        ' ---------'
      iCol=nLkUsed*nVpLk
      do jVar=1,nVar
        r4=cVect(jVar)*xVect(jVar)
        OFV=OFV+r4
        jCol=jVar-nNdVar
        if((0<jCol).and.(jCol<=iCol)) then
        ! xVect(jVar) is a link quantity in fBtu from iSNod to iDNod
!         iLink=jVar-nNdVar ! valid iff nVpLk=1
          iLkUsed=(jVar-1-nNdVar)/nVpLk+1
          do iLink=iLkUsed,nLink ! search until matching count is found
            if(nLkUsdAt(iLink)==iLkUsed) exit
          end do
          if(iLink>nLink) call ps(1,'error_2 countine links used')
!          iSNod=SNodOfLk(iLink)
          iDNod=DNodOfLk(iLink)

          SO2Penalty=SO2Penalty+xVect(jVar) & !  OFV increment due to SO2 = xVect*...
            *(cVect(jVar)-LinkChg(iLink)*1000.0/StoDkT(iLink)) ! ... SO2 cost for iLink
          if(mod(jCol,nVpLk)==0) then
            q4=xVect(jVar-1)+xVect(jVar)
            if(nint(1000.0*(fUnscNode(0,iDNod) &
                           -fUnscNode(2,iDNod)))<15) then
              RetroWt=1.0 ! org fUnscNode is already at (or within 1% of) the target
            else
              RetroWt=GQ(xVect(jVar),q4) ! fractional usage of retrofit scrubbers
            end if
            fRetrofLk(iLink)=RetroWt
            fDNdQtyLk(iLink)=q4 ! fraction of Btu at iDNod supplied by iLink
            if(PrtDetail>1) then
              k=nint(1e3*q4)
             if(k>0) write(DbgU,'(2i5,a,i6,i5)') k,nint(1e3*RetroWt), &
                ' RW at Link,DNode',iLink,iDNod
              if(jCol<iCol) then
                if((VarDesc(jVar+1)(25:27)/=VarDesc(jVar)(25:27))) &
                  write(DbgU,'(1x)') ! but not all links to iDNod are contiguous
              end if
            end if
          end if
        end if
        if(PrtDetail>1) write(DbgU,'(i7,2e13.6,f9.3,2e10.3,1x,a)') &
          jVar,cVect(jVar),xVect(jVar),xVect(jVar)*1000.0, &
          OFV,SO2Penalty, & !  nCTObjBias,
          VarDesc(jVar)
        if(mod(jVar,20000)==0) then
          write(ThisNdName,'(i6)') jVar
          call DulyNote(trim(ThisNdName)//' variables reported')
        end if
      end do ! jVar
!
    ! assign fBtu-weighted average RetroWt by DNod
      write(DbgU,'(/a/a)') ' ~Bin Inst DNod RfWt Prev Curr Targ', &
                           ' ---- ---- ---- ---- ---- ---- ----'
      nScrFuzzy=0
      nScrInsLP=0
      do iDNod=1,nDNod
        if(PnOfDNod(iDNod)<=0) cycle
        iCol=0
       if(nint(1000.0*(fUnscNode(0,iDNod)-fUnscNode(2,iDNod)))<15) then
          RetroWt=1.0 ! org fUnscNode is already at (or within 1% of) the target
        else ! assign RetroWt to the quantity-weighted average fRetrofLk
          QtySum=0.0
          CstSum=0.0
          do iLink=1,nLink
            if((LkEndUsd(iLink)/=2).or.(DNodOfLk(iLink)/=iDNod)) cycle
            if(iCol==0) iCol=iLink
            jCol=iLink
            f4=fDNdQtyLk(iLink)
            QtySum=QtySum+f4 ! should sum to 1
            CstSum=CstSum+f4*fRetrofLk(iLink)
          end do
          RetroWt=GQ(CstSum,QtySum)
          if(iCol>0) then ! save weighted average RetroWt
            do iLink=iCol,jCol ! limit range of do, to expedite
             if((LkEndUsd(iLink)/=2).or.(DNodOfLk(iLink)/=iDNod)) cycle
              fRetrofLk(iLink)=RetroWt
            end do
          end if
        end if
        fUnscNode(1,iDNod)=(1.0-RetroWt)* &
        fUnscNode(0,iDNod)+     RetroWt* &
        fUnscNode(2,iDNod)
        k=nint(100.0*RetroWt)
        if(abs(k-50)<49) nScrFuzzy=nScrFuzzy+1 ! count non-binary decisions for units
        if(OptUnscrubbedFrac(iDNod).and. &
          (fUnscNode(1,iDNod)>=0.0).and. &
          (fUnscNode(1,iDNod)<0.1)) nScrInsLP=nScrInsLP+1
        write(DbgU,'(7i5,L2,1x,a)') nScrFuzzy,nScrInsLP,iDNod, &
          nint(1e3*RetroWt), &
          (nint(1e3*fUnscNode(j,iDNod)),j=0,2), &
          OptUnscrubbedFrac(iDNod),DNodName(iDNod)
      end do ! iDNod
      write(OneLine,'(i4,i5,a,i3)') nScrFuzzy,nScrInsLP, &
        ' DNodes unfocussed, retrofit by LP in Year',iYr
      call DulyNote(OneLine)
!
      Focussed=B1
     if(FocusScrubbing.and.(nScrFuzzy>0).and.(iFocus<LimFocusIter))then
      ! perturb costs in LP to make scrubbing decision more binary at each iDNod
        call DulyNote( &
          'using perturbed costs in LP to improve focus of scrubbers')
        Focussed=B0
        jVar=nNdVar
        iDNod=-1
        do iLink=1,nLink
          if(LkEndUsd(iLink)/=2) cycle
!          piDNod=iDNod
          iDNod=DNodOfLk(iLink)
!          if(iPlnt<=0) cycle
          jVar=jVar+nVpLk
          k=nint(100.0*fRetrofLk(iLink))
          if(abs(k-50)<49) then ! scrubber retrofit decision was not binary

            if(k<50) then ! encourage usage of SO2 credits
              cVect(jVar-1)=cVect(jVar-1)*TestFactor ! decrease
              cVect(jVar  )=cVect(jVar  )/TestFactor ! increase
            else         ! discourage usage of SO2 credits
              cVect(jVar-1)=cVect(jVar-1)/TestFactor ! increase
              cVect(jVar  )=cVect(jVar  )*TestFactor ! decrease
            end if
          end if
        end do
        return
      end if
!
    ! get total production, total cost, and marginal cost at each Mine
      write(TxtU, &
       '(" SNod Sg  jVar SNodQty/kT NdQ/Lim CFOB/M$ AvgCFOB MrgCFOB")')
      write(TxtU, &
       '(" ---- -- ----- ---------- ------- ------- ------- -------")')
      MineMSeg=nCCSegs
      MineACst=0.0
!      MineMCst=0.0
      jCol=1
      do iSNod=1,nSNod
        nNzSegs=SegsUsd(iSNod)
        if(nNzSegs<=0) cycle
        QtySum=0.0
        CstSum=0.0
!Obs    jVar=1+(iSNod-1)*(1+nCCSegs) ! for kT from Supply
        jVar=jCol
        f4=xVect(jVar)
        q4=f4
        SNodQty(iSNod)=q4
        do jSeg=1,nNzSegs ! nCCSegs
          f4Prev=f4
          jVar=jVar+1
          f4=xVect(jVar)
          if(q4<0.1) exit
          if((f4<0.1).and.(QtySum>=0.999*q4)) exit ! with jVar beyond last segment used
          QtySum=QtySum+f4 ! without QtySum test above, narrow segs => exit too early
          CstSum=CstSum+f4*MineFunc(jSeg,1,iSNod) ! (kT)*($/T) => (k$)
        end do
        if(jSeg<nNzSegs) f4=f4Prev ! loop exited with f4 nearly zero
        jSeg=max0(jSeg-1,1) ! index of the marginal segment
        write(DbgU,'(3i6,2f11.3,f6.3,a)') iSNod,jSeg,jVar,f4,q4, &
          GQ(f4,q4),' iSN,MargSeg'
        MineMCst(iSNod)=MineFunc(jSeg,1,iSNod) ! ($/T)
        MineACst(iSNod)=MineMCst(iSNod) ! ($/T) default needed in case Qty=0
        MineCost(iSNod)=CstSum*1000.0 ! ($)
        MineMSeg(iSNod)=jSeg
        if(q4<0.1) MineMSeg(iSNod)=0 ! visual emphasis only
        if(SNodQty(iSNod)>0.1) &
         MineACst(iSNod)= &
         MineCost(iSNod)/(SNodQty(iSNod)*1000.0) 
       write(TxtU,'(i5,i3,i6,f11.2,f8.5,3f8.2,1x,a)')iSNod,jSeg,jVar, &
         SNodQty(iSNod), &
         SNodQty(iSNod)/SNodQtyUB(iSNod),CstSum/1000.0, & ! note use of 1000.0 above
         MineACst(iSNod),MineMCst(iSNod),trim(SNodName(iSNod))
         jCol=jCol+1+nNzSegs
      end do ! iSNod
      MineQAMTcost(1:nSNod,0)=SNodQty (1:nSNod)
      MineQAMTcost(1:nSNod,1)=MineACst(1:nSNod)
      MineQAMTcost(1:nSNod,2)=MineMCst(1:nSNod)
      MineQAMTcost(1:nSNod,3)=MineCost(1:nSNod)
!
! beginning of optional output
!
      if((PrtDetail>0).and.(nVar<=2000)) then
!
        write(DbgU,'(/(e14.6,a))') &
          OFV,       ' objective-function value (OFV)', &
          ContraValu," contracts' value in OFV", &
          nCTObjBias,' nCT bias in OFV', &
        SO2Penalty,' SO2 penalties in OFV',OFV-ContraValu-nCTObjBias- &
          SO2Penalty,' cost of non-contract coal+transport', &
          ContraCost,' cost of     contract coal+transport'
        ! note:  unclear whether ContraCost excludes SO2 penalties
      end if ! PrtDetail>0
!
!
! end of optional, beginning of mandatory output
!
      write(TxtU,'(/a)') ' Fractional Qty Allocated to Mines, in mills'
      write(TxtU,'(1x)',advance='no') ! expect advance=no to follow
      do iSNod=1,nSNod
        if(SegsUsd(iSNod)>0) write(TxtU,'( 9999i3)',advance='no') &
          iSNod/1000
      end do
      write(TxtU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(TxtU,'(1x)',advance='no') ! expect advance=no to follow
      do iSNod=1,nSNod
        if(SegsUsd(iSNod)>0) write(TxtU,'( 9999i3)',advance='no') &
          mod(int(iSNod),1000)
      end do
      write(TxtU,'( 1x,6i2,2a)',advance='no') (j,j=0,5), & ! each of 6 bins has width 10%
        ' #InNzBins DNode DNodeQty/kT AvCFOB MgCFOB AvCDvd MgCDvd', &
        ' AvTrnC AvEmsC'
      write(TxtU,'(1x)',advance='yes') ! writes CrLf after blank
!
      write(TxtU,'(1x)',advance='no') ! expect advance=no to follow
!      write(TxtU,'( 9999a3)',advance='no') (' --',iSNod=1,nSNdU)
      do iSNod=1,nSNod
        if(SegsUsd(iSNod)>0) write(TxtU,'( a3)',advance='no') ' --'
      end do
      write(TxtU,'( 1x,13a)',advance='no') (' -',j=0,5), &
        ' --------- ----- -----------',(' ------',j=1,6)
      write(TxtU,'(1x)',advance='yes') ! writes CrLf after blank
!
!
      do OutU=BnvU,CtvU ! presumes these unit numbers are contiguous
        if(iYr>1) exit ! header line is invariant with iYr
        call WriteCommonTitles()
      ! write file-specific column-titles
        if(OutU==BnvU) then
!          write(DbgU,'(a)') ' Quantity(kTon) from Basins to Units'
          do iBasn=1,nBasn
!           call AppendI2w5(OutU,BasnID(iBasn)) ! basin names are N.A.
            call AppendStr(OutU,BasnName(iBasn))
          end do
        else
!          write(DbgU,'(a)') ' Quantity or Cost from Mines to Units'
          do iSNod=1,nSNod
            if(SegsUsd(iSNod)>0) call AppendStr(OutU,SNodName(iSNod))
          end do
        end if
        if    (OutU==MdcU) then
          call AppendStr(OutU,'WtdMrgCstDvd')
        elseif(OutU==AdcU) then
          call AppendStr(OutU,'WtdAvgCstFOB')
          call AppendStr(OutU,'WtdMrgCstFOB')
          call AppendStr(OutU,'WtdAvgCstDvd')
          call AppendStr(OutU,'WtdMrgCstDvd')
        end if
        write(OutU,'(1x)',advance='yes') ! writes CrLf after blank

      end do ! OutU
!
      LnkQtySn=0.0
      LnkQtyDn=0.0
      DecimBinCount=0
      nInB2LogQty=0
      PctBinCount=0
      RoutQty=0.0
      SO2ResultsEmissions=0.0
      SDRnOfLk=0
      SDRnOfLk(0,1:nLink)=SNodOfLk(1:nLink) ! =iSNod

      nNzQty=0
      MeanQty=0.0
      MeanLog=0.0
      VnceQty=0.0
      VnceLog=0.0
      EstMeanQty=0.0
      EstMeanLog=0.0
      SupDmndQty=0.0
      do iDNod=1,nDNod


        if(PnOfDNod(iDNod)<=0) cycle
        iPolState=StOfDNod(iDNod)
        BasinQty=0.0
        TransQty=0.0
        TransCst=0.0
        EmissCst=0.0
        HeatConAv(1,iDNod)=0.0 ! average for iYr
      ! write common prefix for each record
        i4=DNodID(iDNod)
        j=mod(i4,100) ! EvUnitID
        i4=i4/100 ! EvPlntID
        ThisNdName=DNodName(iDNod)
        do OutU=BnvU,CtvU
          call WriteCsvPrefix(j) ! was i,j
        end do
!
      ! fill columns of basin-aggregate quantity
        r4=0.0 ! only for debugging
        do iLink=1,nLink
          if(LkEndUsd(iLink)/=2) cycle
          if(DNodOfLk(iLink)/=iDNod) cycle ! many iLink connect iDNod & iSNod
          iLkUsed=nLkUsdAt(iLink)
          SDRnOfLk(1,iLink)=iDNod ! NOT iPlnt as above
          iRout=RnOfLink(iLink)
          iSNod=SNodOfLk(iLink)
          iBasn=BnOfSNod(iSNod)
          if((1>iBasn).or.(iBasn>nBasn)) call ps(1,'invalid iBasn')
          q4=1.0/(1000.0*kTpMB(iSNod))


          jCol=nNdVar+(iLkUsed-1)*nVpLk+1 ! 1st of nVpLk is fBtu

          do j=0,1 ! org,min fUnsc

            r4=xVect(jCol+j)*StoDkT(iLink) ! *fPlntUnit(iDNod) ! kT allocation of iPlnt to iDNod
            t4=EmCst(j,iLink)*q4*r4 ! ($=(MBT) =>(($/Tc) ignoring (r4)
            if(r4>=0.1) SDRnOfLk(2,iLink)=RnOfLink(iLink)
            RoutQty(iRout)=RoutQty(iRout)+r4
            LnkQtySn(iSNod)=LnkQtySn(iSNod)+r4 ! (kT)
            LnkQtyDn(iDNod)=LnkQtyDn(iDNod)+r4 ! (kT)
            BasinQty(iBasn)=BasinQty(iBasn)+r4

            TransQty(iSNod)=TransQty(iSNod)+amax1(0.0,r4) ! (kT)
            TransCst(iSNod)=TransCst(iSNod)+LinkChg(iLink)*r4 ! ($/T) ignoring (r4)
            EmissCst(iSNod)=EmissCst(iSNod)+t4              ! (($/Tc) ignoring (r4)
            if(iPolState>=0) then
              SO2ResultsEmissions(1,iPolState)= &
              SO2ResultsEmissions(1,iPolState)+t4 ! ($/Tc)*(kT)=>(k$) including (r4)
            elseif(iStUBSup>=0) then ! report only additional costs NEC
              SO2ResultsEmissions(1,0)=SO2ResultsEmissions(1,0)+t4
            end if

          end do ! j

        end do ! iLink
!
        do iBasn=1,nBasn
          call AppendF8p2(BnvU,BasinQty(iBasn),B1)
        end do
        BasnDmndQty(1:nBasn,iDNod)=BasinQty(1:nBasn)
!
      ! fill columns of type-specific quantity, A&M cost FOB, A&M cost Dvd
        WtdCost=0.0 ! WtdCost(0:6)=0.0
        do iSNod=1,nSNod
          if(SegsUsd(iSNod)<=0) cycle
          QtySum=TransQty(iSNod) ! (kT)
          TransCst(iSNod)=GQ(TransCst(iSNod),QtySum) ! (kT)-weighted average ...
          EmissCst(iSNod)=GQ(EmissCst(iSNod),QtySum) ! ... ($/Tc) across links

          SNodMCst=MineMCst(iSNod)

            SNodACst=MineACst(iSNod) ! average cost at the Mine

            DNodACst=SNodACst+TransCst(iSNod) ! +EmissCst(iSNod) omitted per MSG ...
            DNodMCst=SNodMCst+TransCst(iSNod) ! +EmissCst(iSNod) ... request 20110330

          call AppendF8p2(MnvU,QtySum,B1)
          call AppendF8p2(MdcU,DNodMCst,B1) ! marginal cost Dvd ($/T) varies with iDNod

          call AppendF8p2(AdcU,DNodACst,B1) ! average cost Dvd ($/T) varies with iDNod

          WtdCost(0)=WtdCost(0)+QtySum          ! sum of kT across iSNod for iDNod
          WtdCost(1)=WtdCost(1)+QtySum*SNodACst ! wtd average  cost FOB ($/T)
          WtdCost(2)=WtdCost(2)+QtySum*SNodMCst ! wtd marginal cost FOB ($/T)
          WtdCost(3)=WtdCost(3)+QtySum*DNodACst ! wtd average  cost Dvd ($/T)
          WtdCost(4)=WtdCost(4)+QtySum*DNodMCst ! wtd marginal cost Dvd ($/T)
          WtdCost(5)=WtdCost(5)+QtySum*TransCst(iSNod) ! wtd Trans Cost ($/T)
          WtdCost(6)=WtdCost(6)+QtySum*EmissCst(iSNod) ! wtd Emiss Cost ($/Tc)
          HeatConAv(1,iDNod)=HeatConAv(1,iDNod)+QtySum*SnHeatCon(iSNod) ! =HCLB

          StoD_QyAfMfAdMdTrEmCst(0,iSNod,iDNod)=QtySum
          StoD_QyAfMfAdMdTrEmCst(1,iSNod,iDNod)=SNodACst
          StoD_QyAfMfAdMdTrEmCst(2,iSNod,iDNod)=SNodMCst
          StoD_QyAfMfAdMdTrEmCst(3,iSNod,iDNod)=DNodACst
          StoD_QyAfMfAdMdTrEmCst(4,iSNod,iDNod)=DNodMCst
        ! accumulate statistics for display only
          r4=TransQty(iSNod)
          if(r4<1.0) cycle ! 0.1) cycle
          LogQty=alog(r4)
          MeanQty=MeanQty+ r4
          MeanLog=MeanLog+ LogQty
          if(MeanQty==0.0) then
            EstMeanQty=r4
            EstMeanLog=LogQty
          else
            VnceQty=VnceQty+(r4-EstMeanQty)**2
            VnceLog=VnceLog+(LogQty-EstMeanLog)**2
          end if
          i4=nint(r4)
          do j=0,15
            if(i4==0) exit ! with j>B2LogQty
            i4=i4/2
          end do
          j=j-1
          nInB2LogQty(j)=nInB2LogQty(j)+1
          nNzQty=nNzQty+1 ! count ignores TransQty<1; at most nSNod*nDNod
        end do ! iSNod
        StoD_QyAfMfAdMdTrEmCst(5,1:nSNod,iDNod)=TransCst ! ($/T)
        StoD_QyAfMfAdMdTrEmCst(6,1:nSNod,iDNod)=EmissCst ! ($/Tc)
      ! the upper-left body of the AdcU-file now contains ACDvd
        QtySum=WtdCost(0) ! kT sum across iSNod
        nCTGeqSThr=0
        iSNrank1=0
        iSNrank2=0
        mRank1st=0
        mRank2nd=0
        QuintBinCount=0 ! QuintBinCount(0:5)=0 ! imagine a crude histogram
        write(TxtU,'(1x)',advance='no') ! expect advance=no to follow

        do iSNod=1,nSNod
          if(SegsUsd(iSNod)<=0) cycle

          k=MilliFrac(TransQty(iSNod),LnkQtyDn(iDNod))
          write(TxtU,'( i3)',advance='no') k ! fractional usage/mills at iSNod
          if(k<=0) cycle
          jBin=min(k/100,5) ! (jBin==5) => (k>=500) => (TQ(iSNod)>=LQ(iDNod)/2)

          if(k>=FullSignif*1000.0) then
            nCTGeqSThr=nCTGeqSThr+1 ! count significant suppliers
            if    (mRank1st<k) then
              mRank2nd=mRank1st
              mRank1st=k
              iSNrank2=iSNrank1
              iSNrank1=iSNod
            elseif(mRank2nd<k) then
              mRank2nd=k
              iSNrank2=iSNod
            end if
        if(PrtDetail>1)write(DbgU,'(5i5,2i4,a)') iDNod,k,nCTGeqSThr, &
              iSNrank1,iSNrank2,mRank1st,mRank2nd,' i&v of rank 1&2'
          end if
          QuintBinCount(jBin)=QuintBinCount(jBin)+1
          if(k>1000) call ps(1,'MilliFrac>1000')
          jBin=k/10
          PctBinCount(jBin)=PctBinCount(jBin)+1

        end do ! iSNod

        write(TxtU,'( a,6i2,a,i1,a5,i6,f12.2)',advance='no') ';', &
          (QuintBinCount(j),j=0,5),':',nCTGeqSThr, &
          '       @',iDNod,QtySum
        if(QtySum<0.1) then
          HeatConAv(1,iDNod)=HeatConAv(0,iDNod) ! report prior-year value
        else
          HeatConAv(1,iDNod)=HeatConAv(1,iDNod)/QtySum
        end if
        if(nCTGeqSThr>10) nCTGeqSThr=10
        DecimBinCount(nCTGeqSThr)=DecimBinCount(nCTGeqSThr)+1
        if(SupDmndQty<QtySum) SupDmndQty=QtySum ! of interest for scaling HpÓ & HmÓ
!
        r4=0.0
        StoD_QyAfMfAdMdTrEmCst(0,0,iDNod)=QtySum
        do j=1,6 ! append columns of quantity-weighted-average prices
          if(QtySum>0.1) r4=WtdCost(j)/QtySum
          if(j==4) call AppendF8p2(MdcU,r4,B1)
          if(j<=4) call AppendF8p2(AdcU,r4,B1)
          write(TxtU,'( f7.2)',advance='no') r4
          StoD_QyAfMfAdMdTrEmCst(j,0,iDNod)=r4 ! ($/Tc) averaged across nSNod
        end do
        write(TxtU,'(1x)',advance='yes') ! writes CrLf after blank
        if(iDNod<=99) write(DbgU,'(i6,2f9.1,e9.2,2a)') iDNod, &
          HeatConAv(1,iDNod),QtySum,r4,' EmCst($/Tc) at DNod ', &
          DNodName(iDNod)
!
        do OutU=BnvU,CtvU
          write(OutU,'(1x)',advance='yes') ! writes CrLf after blank
        end do
        if(mod(iDNod,int(100,2))==0) then
          write(ThisNdName,'(i5)') iDNod
          call DulyNote(trim(ThisNdName)//' Demand nodes reported')
        end if
      end do ! iDNod
!
      jVar=nVar-nBasnUB*2-nGpUBUsd*2-nStUBUsd*2
      iReq=mReq-nBasnUB-nGpUBUsd-nStUBUsd
      do iPolState=0,iStUBSup
        if(StUBUsed(iPolState).eqv.B0) cycle
        iReq=iReq+1
        jVar=jVar+1
        SO2ResultsEmissions(0,iPolState)=xVect(jVar) & !  kT of SO2 for coal burned
          +xVect(jVar+1) ! supra-nominal_UB (added 20120626)
        SO2ResultsEmissions(2,iPolState)=SpVec(iReq) ! shadow price from LP

        write(DbgU,'( i3,2i7,f9.3,f12.0)',advance='no') &
          iPolState,jVar,iReq,SO2ResultsEmissions(0:1,iPolState) ! (1,) are cost/k$ accumulated above
        if((-1e4<SpVec(iReq)).and.(SpVec(iReq)<1e5)) then
          write(DbgU,'( f12.6)',advance='no') SpVec(iReq)
        else
          write(DbgU,'( e12.5)',advance='no') SpVec(iReq)
        end if
        write(DbgU,'(2a)') ' kTSO2,k$cost,ShadowPrice',ReqDesc(iReq)
        jVar=jVar+1 ! after 20120626, account for supra-nominal_UB used above
      end do
      SO2ResultsShadPrGrp(1:2)=0.0
      do iPolStGrp=1,2
        if(nInGroup(iPolStGrp)==0) cycle
        iReq=iReq+1
        SO2ResultsShadPrGrp(iPolStGrp)=SpVec(iReq) ! shadow price from LP
      end do
!
      call ReportET('during Demand-node reporting')
!      call flush(RptU)
    ! finalize mean and s.d. of TransQty
      r4=float(nNzQty)
      MeanQty=MeanQty/r4
      MeanLog=MeanLog/r4
      VnceQty=VnceQty/r4-(MeanLog-EstMeanQty)**2
      VnceLog=VnceLog/r4-(MeanLog-EstMeanLog)**2
      StDvQty=0.0
      StDvLog=0.0
      if(VnceQty>0.0) StDvQty=VnceQty**0.5
      if(VnceLog>0.0) StDvLog=VnceLog**0.5
      MeanLog=MeanLog/log(2.0)
      
      StDvLog=StDvLog/log(2.0)
!
    ! write summary lines
    ! write common prefix for each record
      ThisNdName='column total'
      i4=0
      do OutU=MnvU,AdcU ! write common fields
        if(OutU==MdcU) cycle
  700   call WriteCsvPrefix(i2_0)
        do iSNod=1,nSNod
          if(SegsUsd(iSNod)<=0) cycle
          if(OutU==MnvU) then
            if(ThisNdName(1:1)=='c') then
              r4=SNodQty (iSNod) ! 1st pass
            else
              r4=LnkQtySn(iSNod) ! 2nd pass
            end if
            call AppendF8p2(MnvU,r4,B1)
          else
            call AppendF8p2(AdcU,MineACst(iSNod),B1)
          end if
        end do
        if(OutU==AdcU) call ExpleteFinal4Col()
        write(OutU,'(1x)',advance='yes') ! writes CrLf after blank
        if(ThisNdName(1:1)=='c') then ! write checksum to MnvU
          ThisNdName='sum to paths'
          goto 700
        end if
        ThisNdName='Unwtd Avg Cost FOB'
      end do
!
      OutU=MnvU
      ThisNdName='Unwtd Capacity Factor (ppM)'
      call WriteCsvPrefix(i2_0)
!      do iSNod=1,nSNod
!        if(SegsUsd(iSNod)>0)
!     +    call AppendF8p2(OutU,SNodQty(iSNod)/SNodQtyUB(iSNod),B1)
!      end do
!NOT  call ExpleteFinal4Col()
      write(OutU,'( 9999(i7,","))',advance='no') (nint(1e6* &
        SNodQty(iSNod)/SNodQtyUB(iSNod)),iSNod=1,nSNod)
      write(OutU,'(1x)',advance='yes') ! writes CrLf after blank ! this is the final output to MnvU for iYr
!
      ThisNdName='Unwtd Mrg Cost FOB'
      do OutU=MdcU,AdcU
        call WriteCsvPrefix(i2_0)
        do iSNod=1,nSNod
          if(SegsUsd(iSNod)>0) call AppendF8p2(OutU,MineMCst(iSNod),B1)
        end do
        if(OutU==MdcU) then
          call AppendF8p2(OutU,0.0,B1)
        else
          call ExpleteFinal4Col()
        end if
        write(OutU,'(1x)',advance='yes') ! writes CrLf after blank
        call flush_unit(int(OutU,2))
      end do
      call DulyNote('end of writing OUT-files')
!
      write(TxtU,'(/a,f6.3,a)') &
        ' Number of Demand Units in #CT-used Bins @', &
        FullSignif,'=Significance_Threshold'
      write(TxtU,'(11i5,a)')    (j,j=0,10),' SupDemandQty'
      write(TxtU,'(12a)') (' ----',j=0,10),' ------------'
      write(TxtU,'(11i5,f13.2/)') DecimBinCount,SupDmndQty
!
      write(TxtU,'(/a)') ' Number of Demand Units in Base2LogQ Bins'
!      write(TxtU,'(16i4,a)')   (j,j=0,15),' nNzDemandQty'
!      write(TxtU,'(17a)') (' ---',j=0,15),' ------------'
!      write(TxtU,'(16i4,i13/)') nInB2LogQty,nNzQty
      do i=0,15
        k=min0(nInB2LogQty(i)/5,998) ! 20101223 added min0 for case of INFEASIBLE
        write(TxtU,'(i3,i5,1x,999a1)') i,nInB2LogQty(i), &
          (AsteriskIf(j),j=0,k)
      end do
      write(TxtU,'(a2,i6)') ' ‰',nNzQty

  write(TxtU,'( 3f9.2,a)') MeanLog,StDvLog,exp((MeanLog-2.0*StDvLog) &
        *log(2.0)),' mean & s.d. of B2Log(Qty/kT), Q@(z=-2)'

      write(TxtU,'(/a)') ' Histogram of MilltiFractions>0 of Demand'
      nNzQty=0
      do i=0,100
        k=PctBinCount(i)
        nNzQty=nNzQty+k ! sum should match that written 11 lines above
        k=k/7 ! for a peak around 500

        if(k>998) then ! cover pathological cases
          write(DbgU,'(3i9,a)') i,PctBinCount(i),k,' huge PctBinCount'
          k=998
        end if
        write(TxtU,'(2i4,1x,999a1)') i,PctBinCount(i), &
          (AsteriskIf(j),j=0,k)
      end do
      write(TxtU,'(a2,i6)') ' ‰',nNzQty
!
      write(DbgU,'(a/(10f9.2))') ' MineACst:',(MineACst(j),j=1,nSNod)
      write(DbgU,'(a/(10f9.2))') ' MineMCst:',(MineMCst(j),j=1,nSNod)
     write(DbgU,'(a/(10f9.0))') ' Mine_Qty/kT:',(SNodQty (j),j=1,nSNod)
  write(DbgU,'(a/(1x,10i8))')' MineUtil (ppM):',(nint(1e6*SNodQty(j) &
        /SNodQtyUB(j)),j=1,nSNod)
      write(DbgU,'(a/(20i3  ))')' #SegsUsed:',(SegsUsd(j),j=1,nSNod)
      write(DbgU,'(a/(20i3  ))')' MineMSeg:',(MineMSeg(j),j=1,nSNod)
      write(DbgU,'(a/(20i3  ))')' #SegsOrig-MineMSeg (=>slack):', &
        (SegsUsd(j)-MineMSeg(j),j=1,nSNod)
      write(DbgU,'(a/(1x,10i8))')' RoutUtil (ppM):', &
        (nint(1e6*RoutQty(j)/RoutUB(j)),j=1,nRout)
      t4=0.0
      do j=1,nRout ! show the worst offenders
        t4=t4+RoutQty(j)
!Dbg    if(RoutQty(j)>0.1) write(DbgU,'(2i5,a,2f9.0)')
!Dbg +    j,nint(1e3*GQ(RoutQty(j),RoutUB(j))),
!Dbg +    '=mills RoutQty/RoutUB ',RoutQty(j),RoutUB(j) ! /RUB_mult
      end do
      write(DbgU,'(i6,e12.5,a)') nRout,t4,' route_total Qty/kT'
      do iLink=1,nLink ! check for problem MSG noted
        iRout=SDRnOfLk(2,iLink)
        if((0>iRout).or.(iRout>nRout)) write(DbgU,'(i6,i2,2i5,a)') &
          iLink,LkEndUsd(iLink),RnOfLink(iLink),iRout, &
          ' invalid Route index SDRnOfLk(2,.)'
      end do
!
      write(DbgU,'(a)') ' iDNod iSNod   TransCost   EmissCost'
      write(DbgU,'(2i6,2f12.3)') ((iDNod,iSNod, &
         StoD_QyAfMfAdMdTrEmCst(5:6,iSNod,iDNod),iSNod=1,min(20,int(nSNod))), &
        iDNod=1,min(20,int(nDNod))) ! 5=>TransCst, 6=>EmissCst
      call ReportET('during all formatted reporting')
      return ! entry ReportCNWSolutionDetails
! -----
      entry CNWIncrReq
!      if(iReq>0) write(DbgU,'(2i6,a)') iReq,mReq,ReqDesc(iReq)
      iReq=iReq+1 ! this ReqDesc is yet undefined
      if(iReq>mReq) call ps(1,'allocation error:  insufficient mReq')
      return
! -----
      entry CNWIncrVar
!      if(jVar>0) write(DbgU,'(2i7,1x,a)') jVar,nVar,VarDesc(jVar)
      jVar=jVar+1 ! this VarDesc is yet undefined
      if(jVar>nVar) call ps(1,'allocation error:  insufficient nVar')
      return
! -----
      entry CNWIncrLB
      iLbV=iLbV+1
!      write(DbgU,'(3i7,1x,a)') jVar,iUbV,nUbV,VarDesc(jVar)
      if(iLbV>nLbV) call ps(1,'allocation error:  insufficient nLbV')
      jLbVec(iLbV)=jVar
      return
! -----
      entry CNWIncrUB
      iUbV=iUbV+1
!      write(DbgU,'(3i7,1x,a)') jVar,iUbV,nUbV,VarDesc(jVar)
      if(iUbV>nUbV) call ps(1,'allocation error:  insufficient nUbV')
      jUbVec(iUbV)=jVar
      return
! -----
      entry AssignCNWAMtx(OneBasedCol,OneBasedRow,ACMvalue)
      if(ConsMtSize==ConsMtSLim) then ! move vectors' contents into larger space
        write(DbgU,'(i9,a)') ConsMtSLim,' < # of non-zeroes in ConsMtx'
        call AllocateArray(zbColHold,ConsMtSLim) ! temporary swap-space
        call AllocateArray(zbRowHold,ConsMtSLim)
        call AllocateArray(ConsEHold,ConsMtSLim)
      ! save copies of full vectors in ~Hold
        zbColHold=zbColOfNze
        zbRowHold=zbRowOfNze
        ConsEHold=ConsMtxNze
!Obs    deallocate(zbColOfNze,zbRowOfNze,ConsMtxNze)
        ConsMtSLim=ConsMtSLim*2 ! double vectors' size-limit
        call AllocateArray(zbColOfNze,ConsMtSLim)
        call AllocateArray(zbRowOfNze,ConsMtSLim)
        call AllocateArray(ConsMtxNze,ConsMtSLim)
      ! restore saved values from ~Hold vectors
        zbColOfNze(1:ConsMtSize)=zbColHold ! (1:ConsMtSize)
        zbRowOfNze(1:ConsMtSize)=zbRowHold ! (1:ConsMtSize)
        ConsMtxNze(1:ConsMtSize)=ConsEHold ! (1:ConsMtSize)
!Obs    deallocate(zbColHold,zbRowHold,ConsEHold) ! free swap-space
      end if
      ConsMtSize=ConsMtSize+1

      zbColOfNze(ConsMtSize)=OneBasedCol-1 ! note that subscripts here ...
      zbRowOfNze(ConsMtSize)=OneBasedRow-1
!      ConsMtxOfs(ConsMtSize)=OneBasedOfs
      ConsMtxNze(ConsMtSize)=ACMvalue      ! ... are kept as 1-based
      return ! entry AssignCNWAMtx
! -----
      entry ReturnCNWAMtx(OneBasedCol,OneBasedRow,RCMvalue)
    
      RCMvalue=0.0 ! default commonly valid

    ! after 20070919 search separate row-major arrays for zbCol and zbRow
      zbCol=OneBasedCol-1
      zbRow=OneBasedRow-1
      OnTgtCol=B0
      OnTgtRow=B0
      do iNze=1,ConsMtSize
        if(zbColOfNze(iNze)==zbCol) then ! Nze for this column are contiguous
          OnTgtCol=B1
          if(zbRowOfNze(iNze)==zbRow) then ! (iNze) matching zbCol & zbRow
            OnTgtRow=B1
            exit
          end if
        else
          if(OnTgtCol) exit ! early, since loop has passed all possible matches
        end if
      end do
      if(OnTgtRow) RCMvalue=ConsMtxNze(iNze) ! else retain 0.0
    ! else no match implies ConsMtx(Col,Row) is void
      return ! entry ReturnCNWAMtx
! -----
      end ! recursive subroutine CoalNetworkMain
! -----
