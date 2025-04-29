!     ******************************************************************
!     facalg.for
!     Copyright(c)  2000
!
!     Created: 8/23/2009 5:09:34 PM
!     Author : MARK S GERBER
!     Last change: MSG 8/23/2009 5:17:58 PM
!     ******************************************************************




      subroutine HaltWith(Prefix,Suffix,Reason)
      use end_routine, only: end_program
        use Faintcom ! merely for ErrorsTrapped,PrtUni,PrtDetail
        integer (kind=2) :: Prefix,Suffix
        character (len=*) :: Reason
        character (len=20) :: ErrFmt
        parameter(ErrFmt='(1x,a,i3,1x,a,1x,i3)')


        write(PrtUni,ErrFmt)'error:  ',Prefix,Reason,Suffix ! screen corruption OK
        if(PrtDetail>0) then
          ErrorsTrapped=1 ! until an error-coding system is devised & implemented

        end if
        if((Prefix/=0).or.(Suffix/=0)) then
          call DeallocFAInt ! vars allocated in StorePlantData
          call DeallocAuxes
        end if

        call end_program("Error was trapped in Facet Algorithm")
      end


      subroutine CkAllocSt(Abbrev,iDealloc)
        use Faintcom ! merely for ErrorsTrapped,AS,PrtUni
        integer (kind=4) :: iDealloc
        character (len=*) :: Abbrev
        character (len=3) :: Prefix(0:1)
        data Prefix/' ',' de'/ !parameter would pad with trailing blanks

        if(AS/=0) then
          ErrorsTrapped=ior(ErrorsTrapped,int(1,2)) ! the caller may want to abort
          write(PrtUni,'(1x,a)') &   !  screen corruption okay here
            'Failure in'//Prefix(iDealloc)//'allocating '//Abbrev// &  
            ' array'
          ! 0,0 => no call to DeallocFAInt
          call HaltWith(0_2,0_2,'invalid allocation') 
        end if
      end


      subroutine ChkReLim(n,Limit,VarType)
        integer (kind=2) :: n,Limit
        character (len=*) :: VarType

        if(n>Limit) call HaltWith(n,Limit, &
          'exceeds in StorePlantData the '//VarType//'-limit of')
      end


      subroutine Sort(iSup,iPos,a,Increasing)

        logical (kind=4) :: Increasing
        logical (kind=1) :: Decreasing
        integer (kind=2) :: iSup,i,j,k,p,q,Gap,iPos(*)
        real (kind=4) :: a(*)

        Decreasing=.not.Increasing
        Gap=iSup/2
        do while(Gap>0)
          do i=Gap,iSup
            j=i-Gap
            do while(j>0)
              k=j+Gap
              p=iPos(j)
              q=iPos(k)
              if((Increasing.and.(a(p)<=a(q))).or. &
                 (Decreasing.and.(a(p)>=a(q)))) then
                j=0 ! break the while loop (assign j=-1 for 0-based arrays)
              else ! interchange the indexing array values
                iPos(k)=p
                iPos(j)=q
              end if
              j=j-Gap
            end do
          end do
          Gap=Gap/2
        end do
      end ! subroutine Sort


      subroutine wnaa(s) ! write s to StdOut without advancing record
        use Faintcom ! merely for PrtUni
        character (len=*) :: s

        write(PrtUni,'(a\)') s
      end


      subroutine GetOneDigit(i,i0,i1) ! useful only during debugging
        ! i0,i1 are lower and upper bounds on i, respectively
        integer (kind=2) :: i,i0,i1,i2 
        character (len=1) :: GetCh,AnyChar

        do ! while(.true.)
          call Tone2(1200,600,33) ! prompt for input with a 2-tone sequence
          AnyChar=GetCh(i2) ! i2 is a dummy argument
          i=ichar(AnyChar)-ichar('0')
          if((i0<=i).and.(i<=i1)) exit
        end do
      end


      subroutine FindOptCostByFacets( &
        LAST_POINT,LPROB,LODDUR,UNIT,BLKNO, &
        NUNITS,NBLOK2,NUM_ARB_CONSTRAINTS,NUM_CUMULANTS, &
        LOWER_BOUND_CAP_FACTOR, &
        UPPER_BOUND_CAP_FACTOR,MWBLOK, &
        eavail_fo,MAINTENANCE_RATE,ENERGY)
		use Faintcom
        integer (kind=2) :: LAST_POINT,NUNITS,NBLOK2, &
          UNIT(*),BLKNO(*),NUM_ARB_CONSTRAINTS,NUM_CUMULANTS
        real (kind=4) :: LPROB(*),LODDUR(*), &
          LOWER_BOUND_CAP_FACTOR(*),MWBLOK(*),eavail_fo(*), &
          UPPER_BOUND_CAP_FACTOR(*),MAINTENANCE_RATE(*),ENERGY(2,*), &
          UnservedQ,xDynStg
        integer (kind=2) :: CallCount,CallsSkipped ! useful only during testing
        data CallCount/0/,CallsSkipped/0/

        character (len=11) :: clk

        if(CallCount==0) then

          PrtDetail=0 ! for normal operation of the model by a customer

          if(PrtDetail>0) then

            PrtUni=DebugUnit
            open(PrtUni,file='c:FacetAlg.out', &
                carriagecontrol='FORTRAN CARRIAGE CONTROL')
            if(PrtDetail>1) write(PrtUni,'(1x,a,i1)') &
              'Level of print detail=',PrtDetail
          else
            PrtUni=6 ! StdOut handle is always available on unit 6
          end if
        end if
        CallCount=CallCount+1
        if(CallCount<=CallsSkipped) return
        ErrorsTrapped=0 ! => 'no diagnostic generated'

        call StoreLoadCurve(LAST_POINT,LPROB,LODDUR)
        if(ErrorsTrapped==0) call StorePlantData( & ! calls AllocFAInt & AllocAuxes
          UNIT,BLKNO,NBLOK2,NUM_ARB_CONSTRAINTS,NUM_CUMULANTS, &
          LOWER_BOUND_CAP_FACTOR, &
          UPPER_BOUND_CAP_FACTOR,MWBLOK, &
          eavail_fo,MAINTENANCE_RATE,NUNITS)
        OptCost=0.0



        if(ErrorsTrapped==0) call MinimizeCost(NBLOK2,UNIT, &
          BLKNO,MWBLOK,MAINTENANCE_RATE,ENERGY,UnservedQ,xDynStg)
        if(PrtDetail>0) write(PrtUni, &
          '(1x,f9.3,a,f9.3,a,f12.3,a,i3,a,i1,a,i1,a,i5)') &
          UnservedQ,'=UQ',xDynStg,'=xDynStg',OptCost,'=cost after', &
          nIter,' iterations @ nK=',nKappa,', nM=',nAbnormalModes, &
          ' @Pd',CallCount

        if(PrtDetail>1) then
          call wnaa(' allowing the ')
          if(BloomsPath) then
            call wnaa &
              ('first bound ordinary variable with properly-signed')
          else
            call wnaa &
              ('bound ordinary variable with the largest-magnitude')
          end if
          write(PrtUni,'(a//)')' dual to enter'
        end if

        call DeallocFAInt ! vars allocated in StorePlantData
        call DeallocAuxes

        return
      end ! subroutine FindOptCostByFacets


      logical (kind=1) function Nearly0(Arg,VarType)
        use Faintcom
        real (kind=4) :: Arg
        integer (kind=4) :: VarType

        if(VarType==0) then
          Nearly0=abs(Arg)<SmallReal
        else
          Nearly0=abs(Arg)<SmallArea
        end if
      end


      real (kind=4) function IntegralPower(x,n) ! returns the integral power of a real base
        real (kind=4) :: x
        integer (kind=2) :: n


        if(n==0) then
          IntegralPower=1.0
        elseif(x==0.0) then
          IntegralPower=x
        else ! for non-zero n and x
          IntegralPower=x**n
        end if
      end


      real (kind=4) function InnerProduct(x,y,n)
        real (kind=4) :: s,x(*),y(*)
        integer (kind=2) :: n,i

        s=0.0 ! consider use of real*8
        do i=1,n
          s=s+x(i)*y(i)
        end do
        InnerProduct=s
      end


      real (kind=4) function yInterpolate(xMid,x1,x2,y1,y2)
        real (kind=4) :: xMid,x1,x2,y1,y2


        yInterpolate=y1+(y2-y1)*(xMid-x1)/(x2-x1)        ! 3+1+1+1 ops
      end


      real (kind=4) function GaussianDen(zGD)
!       returns the density of the standard Normal distribution
!       parameter constants
        real (kind=4) :: &
          MaxReal, &
          MinReal, &
          SqrtMinReal, &
          MaxLn, &
          HalfMaxLn
        parameter( &
          MaxReal=9.0e37, & !  a bit conservative for real*4, but sufficient
          MinReal=1.1e-38, &
          SqrtMinReal=1.04e-19, &
          MaxLn=87.39, & !  alog(MaxReal)
          HalfMaxLn=43.695)
        real (kind=4) :: zGD,t


        t=-0.5*zGD*zGD ! perhaps more accurate than exponentiation
        if(t<-HalfMaxLn) then ! prevent divide overflows by restricting GaussianDen
          GaussianDen=SqrtMinReal
        else
          GaussianDen=0.39894228*exp(t)
        end if
      end ! GaussianDen


      real (kind=4) function CCNFmd(zCCN)

        real (kind=4) :: zCCN,t

        t=1./(1.+0.33267*zCCN)
        CCNFmd=t*(0.4361836+t*(-.1201676+t*0.9372980))
      end


      real (kind=4) function NormalOrd(x,Mu,Sigma)
        real (kind=4) :: x,z,Mu,Sigma,GaussianDen,CCNFmd

        z=(x-Mu)/Sigma
        if(z<0.0) then
          NormalOrd=1.0-GaussianDen(z)*CCNFmd(-z)
        else
          NormalOrd=    GaussianDen(z)*CCNFmd( z)
        end if
      end


      recursive subroutine FacetAlgorithm ! subroutine ends after all entry-points end
      use end_routine, only: end_program, er_message
        use faintcom
        use enrglimit

        integer (kind=2) :: i,j,k

        character (len=8) :: Fmt1a,Fmt71,Fmt03
        character (len=58) :: Unplaceable
        integer (kind=2) :: LimAbnModes
        parameter(Fmt1a='(1x,a)  ',Fmt71='(f7.1\) ',Fmt03='(f10.3\)', &
          Unplaceable= &
          'var unplaced; its aux constraints have excessive BasicNKy', &
          LimAbnModes=0) ! 0 to inhibit extraction of abnormal modes from load curve
        real (kind=8)  :: TailUQ
        real (kind=4) :: & 
          InfRHS,xLoadPoint,xLoadPtInf,MeanDxLoad,MeanLoad,SDLoad, &
          ChgCapac,CapConv, &
          EqLDur(0:LimEqPoints), & !  eq-load ordinates with 0 outages included
          EqLCumul(LimCumulants,0:LimAbnModes), &
          CtMoment(LimCumulants),NCMoment(LimCumulants), &
          Ld0Cumul(LimCumulants,0:LimAbnModes), &
          DistrWeight(0:LimAbnModes), &
          BinCoeff(0:LimCumulants,0:LimCumulants)
        integer (kind=4) :: I4_4LCmBnM,I4_4nEqPtsp1,I4_4nKappa,I4_nKappa,I4_SupiHP
        integer (kind=2) :: nEqPoints ! integer*1 suffices for magnitude of those below:
        integer (kind=2) :: jOrdEnt,jKey,jBind,jPrBk,jBlkPair, &
          kLvg,kLp1,kInf,kBind,Base2LogSize,nActiveFacets, &
          LimitCondit,iOVE,nGamma,SupiHP,nStgEqn
        logical (kind=1) :: Optimal,OrdVarEntering,InitSort,BasisStrucObsolete, &
          UsingCumul,FixingOvershoots,PriorBlockLoaded

        common/OCost/ TailUQ, & !  SplitCrit,
          InfRHS,xLoadPoint,xLoadPtInf,MeanDxLoad,MeanLoad,SDLoad, &
          ChgCapac,CapConv,EqLDur,EqLCumul, &
          CtMoment,NCMoment,Ld0Cumul,DistrWeight,BinCoeff, &
          I4_4LCmBnM,I4_4nEqPtsp1,I4_4nKappa,I4_nKappa,I4_SupiHP, &
          nEqPoints,jOrdEnt,jKey,jBind,jPrBk,jBlkPair, &
          kLvg,kLp1,kInf,kBind,Base2LogSize,nActiveFacets, &
          LimitCondit,iOVE,nGamma,SupiHP,nStgEqn, &
          Optimal,OrdVarEntering,InitSort,BasisStrucObsolete, &
          UsingCumul,FixingOvershoots,PriorBlockLoaded


!       items assumed to be known by the caller of MinimizeCost
        integer (kind=2) :: &
          iOfFacAlgs(:),  & !  (j) is the caller's index i for local var index j
          jOfCallers(:),  & !  (i) is the local var index j for caller's index i
          VarOfDispRank(:), & !(i) is the local var index j in dispatch position i
          AuxRow(:),      & !  (j) is the index of first constraint covering var j
          UnitOwning(:),  & !  (j) is block j's unit index
          BlockPos(:),    & !  (j) is block j's position within a plant unit, on [0,2]
          BlockOfSameUnitAs(:,:) !(j,i)=var in same unit as j, but with BlockPos=i
        logical (kind=1) :: &
          AuxCycled(:),  & !  (j) => var has previously entered basis from BoundNBA
          AuxiCovers(:), & !  (j) => j in any constr.
          AuxCovers(:,:) ! (i,j) => j in constr. i
        real (kind=4) :: &
          Capacity(:),  & !  plant capacity in energy or quantity units/time
          EquivAvail(:), & !  equivalent availability = 1-(equivalent forced-outage rate)
          AuxRHS(:),    & !  right-hand-side of aux equation, units relative to AColumn
          GLB(:),       & !  lower bounds on variable quantity
          LUB(:),       & !  upper bounds on variable quantity
          x(:),         & !  unknown quantities of interest
          cOrg(:),      & !  per-unit cost of each quantity, used to report total cost
          cMod(:),      & !  (j) is dispatch criterion for var j; lower=>dispatch earlier
          AColumn(:,:)  ! (i,j) is var j's coeff in constraint i
        ! InputRate(:)  ! (j) is the average heat-rate for var j; (j)*x(j)=input(j)

        allocatable :: &
          iOfFacAlgs,jOfCallers,VarOfDispRank,AuxRow,UnitOwning, &
          BlockPos,BlockOfSameUnitAs,AuxCycled,AuxiCovers,AuxCovers, &
          Capacity,EquivAvail,AuxRHS,GLB,LUB,x,cOrg,cMod,AColumn ! ,InputRate

        save &
          iOfFacAlgs,jOfCallers,VarOfDispRank,AuxRow,UnitOwning, &
          BlockPos,BlockOfSameUnitAs,AuxCycled,AuxiCovers,AuxCovers, &
          Capacity,EquivAvail,AuxRHS,GLB,LUB,x,cOrg,cMod,AColumn ! ,InputRate


        integer (kind=2) :: &
          PrevState (:), &
          VarState  (:), & ! (j)=> state (among those valid States) of var j
          BNKinRow  (:), & ! (i) is the BasicNKy var associated with DOrgRow(i)
          PrevBDIRow(:), & ! (j) indexes the old DInvRow location of BasicNKy vars
          BasisDIRow(:), & ! (j) indexes the     DInvRow location of BasicNKy vars
          PrevBKyVar(:), & ! (j) indexes the old KeyVar  related  to BasicNKy vars
          BasisKyVar(:), & ! (j) indexes the     KeyVar  related  to BasicNKy vars
          FctOwning (:), & ! (j) indexes the facet-margin set covering var j
          KeyVar    (:), & ! (k) indexes column in A (index in Psik) that is 'key'
          VarInFctOrder(:), & !(i) is the local var index j in order of incr. facets &
          VFOFirstInFct(:) !(k) is the VarInFctOrder index of first var in facet k
        real (kind=4) :: AuxDualMult(:),AuxColumn(:),d(:),Phi(:),DualVar(:), &
          DColumn(:,:), & !  with less-rapidly-changing index => column
          DOrgRow(:,:), & !  with less-rapidly-changing index => row
          DInvRow(:,:), &
          VarCumul(:,:) ! (.,j) is vector for var j; (.,0) is vector for EqLDn0
                        ! (.,-1) is vector for pair of blocks within same unit
        logical (kind=1) :: BkOwns(:),BindSetOwns(:),TestSetOwns(:),WouldBeZero(:)

        allocatable :: &
          PrevState,VarState,BNKinRow,PrevBDIRow,BasisDIRow, &
          PrevBKyVar,BasisKyVar,FctOwning,KeyVar, &
          VarInFctOrder,VFOFirstInFct, &
          AuxDualMult,AuxColumn,d,Phi,DualVar, &
          DColumn,DOrgRow,DInvRow,VarCumul, &
          BkOwns,BindSetOwns,TestSetOwns,WouldBeZero

        save &
          PrevState,VarState,BNKinRow,PrevBDIRow,BasisDIRow, &
          PrevBKyVar,BasisKyVar,FctOwning,KeyVar, &
          VarInFctOrder,VFOFirstInFct, &
          AuxDualMult,AuxColumn,d,Phi,DualVar, &
          DColumn,DOrgRow,DInvRow,VarCumul, &
          BkOwns,BindSetOwns,TestSetOwns,WouldBeZero

        allocatable :: &
          ArrayOld, & !  used only locally in InvertMatrix
          AuxCoeff,CFLB,CFUB,BlockDispCost,BlockHeatRate, & !  used only in StorePlantData
          jMarginal,GrayOrder,HoldOrder ! used in FindMostRestrictive

        save ChrState,LimitingIneq,jMarginal,GrayOrder,SupiGr, &
          Hermite,Factorial

!       variables used only by CopyEqLoad:
        real (kind=4) :: HeldCumul(LimCumulants,0:LimAbnModes), &
          HeldLDur(0:LimEqPoints)
        save HeldCumul,HeldLDur

!       variables used only by ReviseBasisVectors and those it calls:
        integer (kind=2) :: jLvg,jLp1,jNewlyNKy,FinalState
        logical (kind=1) :: FacetOwnsBNK
        save jLvg,jLp1,jNewlyNKy,FinalState,FacetOwnsBNK

!       variables used only by entry-points below, in order of use:
        logical (kind=1) :: EcoReversed,TempLog1
        integer (kind=2) :: NBLOK2,NUM_ARB_CONSTRAINTS,NUM_CUMULANTS,NUNITS, &
          UNIT(*),BLKNO(*)
        real (kind=4) :: &
          LOWER_BOUND_CAP_FACTOR(*), &
          UPPER_BOUND_CAP_FACTOR(*),MWBLOK(*), &
          eavail_fo(*), MAINTENANCE_RATE(*)
        integer (kind=2) :: i1,i2,i3,iMinJ,iUnit,j0,j1,j2,j3,jRet,jDis, &
          iAux,jAux,iAuxOrg, &
          nVarInAux,nVarUB,Multiplicity,nDSERed,nCFLBVar
        integer (kind=4) :: I4_nVUB,I4_BRem
        character (len=1) :: ConstraintUnits ! Energy, Heat, Fuel, Pollutant emissions
        character (len=9) :: VarString
        character (len=35) :: Fmt8f
        parameter(VarString='variables', &
          Fmt8f='(1x,i3,i4,i2,4f7.4,f9.3,2f9.3,f9.1)')
        real (kind=4) :: CostUQ,CapacUQ,RHS,SupRHS,SupCost,SysCap_loc,Capaci, &
          BlkCoeff,SlackCoeff, &
          SupcModBP0,  & !  largest modified cost assigned to any var with BlockPos=0
          AuxCoeff(:), & !  (j) = coefficient of var j in the current aux constraint
          CFLB    (:), & !  (j) = lower bounds on capacity factor for var j
          CFUB    (:), & !  (j) = upper bounds on capacity factor for var j
          BlockDispCost(:,:),BlockHeatRate(:,:)
        integer (kind=2) :: jSR,iSR
        integer (kind=2) :: jD,jM,iCE  ! ,jAux,iAux
        integer (kind=4) :: I4_2nRem,I4_4nRem
        real (kind=4) :: CELB(*),CEUB(*)
        integer (kind=2) :: LAST_POINT,iPoint
        logical (kind=1) :: NonzeroPeak,NoniX,IncrY
        real (kind=4) :: LPROB(*),LODDUR(*)
        integer (kind=2) :: nVrAux ! possibly larger than nVar, which may not yet be known
        integer (kind=2) :: nVr ! possibly larger than nVar, which may not yet be known
        real (kind=4) :: xOrg,xWidth,xDomain(0:1),xGrid(0:1),yGrid(0:1)
        integer (kind=2) :: iBegPt,iEndPt
        logical (kind=1) :: NegativeArea
        integer (kind=2) :: jFTA,kFTA
        character (len=11) :: Truncation
        real (kind=8)  :: MinArea
        real (kind=4) :: StdDev,Density,c1,c2,z,xLod,yPrb,ySum,GaussianDen,CCNFmd, &
          GammaNorm(2),Factorial(4),Hermite(0:LimiHP),HPcoef(0:LimiHP)
        equivalence (z,Hermite(1)),(c1,GammaNorm(1)),(c2,GammaNorm(2))
        data Hermite(0)/1./
        data Factorial/1.,2.,6.,24./ ! the value of n-factorial
        integer (kind=2) :: km1,m,nIdent,Sterling(LimCumulants,LimCumulants)
        real (kind=4) :: CapOutage,PrOutage,nmFactor
        logical (kind=1) :: MonotCLM
        real (kind=4) :: LoadStepSize,LoadProbed,AvgOrdinate,PriorAvgOrd
        real (kind=8) :: OrigUQ
        real (kind=4) :: yDecrement,yDecNormal,LocalMean,ResidSlope, &
          SumWeights,ModeYDecr(LimPoints),NormalOrd
        integer (kind=2) :: iMode,iGLC,iPtGLC,iBegMode,iEndMode,ModeTrimmed, &
          ModeXBeg(LimPoints),ModeXEnd(LimPoints),ModeOrder(LimPoints)
        logical (kind=1) :: OneTruncated
        real (kind=4) :: x1,x2,y1,y2,DeltaX,DeltaY,DistrMean,TermsOfOrderK, &
          Slope,SloPr,Slop2,Sl2Pr,IntegralPower
        integer (kind=2) :: jMode,ModelOrder,iPtInf,iPtSup,iBgMode,iGMC,kGMC, &
          iPtGMC,ModeXBg(*),ModeXEd(*),ModeOrd(*)
        logical (kind=1) :: Bracketing
        integer (kind=2) :: iPt,iBM,ModXBeg(*),ModXEnd(*),ModOrder(*)
        integer (kind=2) :: jMN,n
        real (kind=4) :: SigMN
        integer (kind=2) :: Order,jm1
        character (len=*) :: Fmt
        real (kind=4) :: fVal
        integer (kind=2) :: nLeadingBlanks,iVal,jWNAI ! iVal is assumed non-negative
        logical (kind=1) :: Nearly0
        integer (kind=2) :: iBlk,iMult,PrevIter 
        real (kind=4) :: InnerProduct,CallersQ,ENERGY(2,*), &
          UnservedQ,xDynStg,xAccum
        integer (kind=2) :: jPrev,PrevKey,ThisKey,nDifferent,RowChanged
        logical (kind=1) :: InvObsolete
        integer (kind=2) :: iPass
        logical (kind=1) :: Errant
        real (kind=4) :: sCP
        logical (kind=1) :: PreInv,FullInv
        character (len=12) :: MethodGJ
        character (len=10) :: MethodSR
        character (len=22) :: PostInv
        parameter(MethodGJ='Gauss-Jordan', &
                  MethodSR='Single-Row', &
                  PostInv =' post-inversion using ')
        integer (kind=2) :: kMI,nMI
        real (kind=4) :: kDotProd,RowPrimary(nMI,nMI),NewCol(nMI)
        integer (kind=2) :: jASM,nASM
        real (kind=4) :: Scalar,RowVect(*),v(*)
        integer (kind=2) :: jSC
        real (kind=4) :: AColumnSC(nAux,*),AccumColumnSC(*)
        integer (kind=2) :: jCU,kCU,UnplacedBNK
        integer (kind=2) :: jPt,jBFS,nStgVar,iLastRetVoDR,kFirm
        logical (kind=1) :: Monotonic
        real (kind=4) :: bAccum,xAHold,xjHold,DisCapac,ChgQ,DisQ,SumStgRHS, &
          Efficiency,InfEfficiency
        real (kind=8) :: ThisUQ,ChgUQ
        integer (kind=2) :: jMKV
        real (kind=4) :: xAcMKV
        integer (kind=2) :: iLastRetVar,iLastRetVarInit,iLRVInf,iDisp,kFrm
        logical (kind=1) :: InPriorFct,SameStgEqn,jDisFits
        real (kind=4) :: xAccumCAB,xAccumInit,xKeyPrev,xDisPrev,xAggNoSplit, &
          xAggPrev,xKeyDecr, &
          xDischgRem,xDischgInit,xDischgUsed,CDischgUsed,PairsQ
        real (kind=8) :: PrevUQ,FirmUQ
        integer (kind=2) :: kDel
        logical (kind=1) :: UnityRetCF
        integer (kind=2) :: jCSV
        real (kind=4) :: xAcCSV
        integer (kind=2) :: jMB,kPrBk
        integer (kind=2) :: jSI
        real (kind=4) :: xAccumSI
        integer (kind=2) :: jLA ! ,kLA
        real (kind=4) :: xAccumLA
        character (len=19) :: Fmtf8p2,Fmtf8p5
        parameter(Fmtf8p2='(1x,i3,f8.2,6e11.4)',Fmtf8p5='(1x,i3,f8.5,6e11.4)')
        logical (kind=1) :: InclThisFct,PreTestSet
        integer (kind=2) :: PrBkPos,jCPB,kCPB,kFct
        real (kind=4) :: EscalationFactor,FactorToKthPower
        logical (kind=1) :: FromHold0
        logical (kind=1) :: FromHold
        integer (kind=2) :: jLUB,jFirst0
        real (kind=4) :: xZero,FracStep, EDExcl(0:LimEqPoints) ! eq-load's duration excluding outages of Capac
        integer (kind=2) :: jB,jC,jGLB
        character (len=14) :: Fmt6e
        character (len=16) :: Fmtf68
        character (len=28) :: Fmtf10
        parameter( Fmt6e ='(1x,i3,6e11.4)', &
          Fmtf68='(1x,f6.3,f8.3,a)', &
          Fmtf10='(1x,i3,i3,i2,f10.3,a,3f11.4)')
        real (kind=4) :: Capac,EFOR_loc,EqAv,xShifted,yAtxShifted,yInterpolate, &
          EDIncl(0:LimEqPoints) ! eq-load's duration including outages of Capac
        real (kind=4) :: yOrg(0:*),yNow(0:*)
        real (kind=4) :: UnsvdQ
        integer (kind=2) :: k2
        logical (kind=1) :: FirstItem
        character (len=47) :: FmtNoPhi1,FmtNoPhi2
        parameter( FmtNoPhi1='(1x,2i3,a,f8.2 ,f12.5,f9.3,f19.2,f9.3,f10.6)', &
          FmtNoPhi2='(1x,2i3,a,i5,i3,f12.5,f9.3,f19.2,f9.3,f10.6)')
        real (kind=4) :: AvgDurat(LimUnknowns),PrevAvg,Avg2ndP,PrevCap,Cap2ndP
        character (len=4) :: ChrVS(LimUnknowns),ChrState(0:3)
        character (len=2) :: Warning
        data ChrState/' nB0',' nB1',' Key',' BnK'/
        integer (kind=2) :: nRows,PivotRow
        integer (kind=4) :: I4_nBinRow,I4_n
        real (kind=4) :: HeldElement,PivotElement,Factor,ArrayOrg(nRows,nRows), &
          ArrayInv(nRows,nRows),ArrayOld(:,:),HeldRow(LimUnknowns)
        logical (kind=1) :: SameSubset
        integer (kind=2) :: jCIS,NBaState
        logical (kind=1) :: Singular
        integer (kind=2) :: jKeyOE,kOE,kOfs
        integer (kind=2) :: jNKyAAR,jNBaAAR,kAAR
        logical (kind=1) :: Aligned
        integer (kind=2) :: kFctFR
        logical (kind=1) :: AlignedFR
        integer (kind=2) :: kFctSS,jAlt,jNBa,jNKy
        logical (kind=1) :: AlignedSS
        integer (kind=2) :: kFctNKAI,jNKyToBe,kNKyToBe
        logical (kind=1) :: AnyConstr
        character (len=36) :: NoBNK
        parameter(NoBNK='facet contains no basic non-key var')
        integer (kind=2) :: jSkip,jBnk
        integer (kind=2) :: kIWBnk,jIWB
        logical (kind=1) :: Partition
        integer (kind=2) :: k0,k1
        integer (kind=2) :: jUnionKey
        integer (kind=2) :: jOrg,jCompl,PrStjOrg
        character (len=46) :: NoCompl
        parameter( NoCompl='var has no aux complement in state unequal to')
        real (kind=4) :: Alpha
        character :: InfChr ! useful only if PrtDetail>0
        character (len=10) :: StrRHS ! internal file useful only if PrtDetail>0
      ! index number of the limiting inequality, according to Bloom's paper
        integer (kind=2) :: LimitingIneq(0:2,0:1)
        data LimitingIneq/18,19,0,24,25,27/
        logical (kind=1) :: OrdVarFinalFct,FctVoidOfDj
        integer (kind=2) :: jLoadedLast,kPrior0,kPrior1,iMSb, &
        iMarginal,nMarginal, &
          jMarginal(:)
        integer (kind=4) :: HalfSize,GrayOrder(:),HoldOrder(:), & !  temporary storage
          SupiGr, & !  may grow as large as LimFacetSize
          iGray,fGray, &
          PrevBitPattern, &
          ThisBitPattern, &
          DiffBitPattern, &
          InclBitPattern
!         FullBitPattern ! large enough for 32 vars
!       real*4 PrevCapac,CapIncr,AccumArea,AccumXInSet,AccumDj
        real (kind=8) :: PrevCapac,AccumArea,AccumXInSet,AccumDj
        real (kind=4) :: CapIncr
        integer (kind=2) :: kLFS,nVarsInFct,nNB1sInFct,nVarsIFLim,BindSetLim, &
          BindSetVar(6)
        integer (kind=2) :: jPrevOE ! used to break pathological cycling
        real (kind=4) :: OptDual


      entry AllocFAInt(nVr)
!       integer (kind=2) :: nVr ! possibly larger than nVar, which may not yet be known

        allocate(iOfFacAlgs(nVr),stat=AS)
        call CkAllocSt('iOfC',0)
        allocate(jOfCallers(nVr),stat=AS)
        call CkAllocSt('jOfC',0)
        allocate(x         (nVr),stat=AS)
        call CkAllocSt('xQty',0)
        allocate(cOrg      (nVr),stat=AS)
        call CkAllocSt('RawC',0)
        allocate(cMod      (nVr),stat=AS)
        call CkAllocSt('AlgC',0)
        allocate(GLB       (nVr),stat=AS)
        call CkAllocSt('GtLB',0)
        allocate(LUB       (nVr),stat=AS)
        call CkAllocSt('LtUB',0)
        allocate(Capacity  (nVr),stat=AS)
        call CkAllocSt('Capy',0)
        allocate(EquivAvail(nVr),stat=AS)
        call CkAllocSt('EAvl',0)
        allocate(UnitOwning(nVr),stat=AS)
        call CkAllocSt('Unit',0)
        allocate(BlockPos  (nVr),stat=AS)
        call CkAllocSt('BPos',0)
        allocate(AuxRow    (nVr),stat=AS)
        call CkAllocSt('ARow',0)
        allocate(AuxCycled (nVr),stat=AS)
        call CkAllocSt('AxCy',0)
        allocate(AuxiCovers(nVr),stat=AS)
        call CkAllocSt('AiCv',0)
        allocate(BlockOfSameUnitAs(nVr,2),stat=AS)
        call CkAllocSt('BoSU',0)
        allocate(VarOfDispRank(nVr),stat=AS)
        call CkAllocSt('VoDR',0)
!       allocate(InputRate (nVr),stat=AS)
!       call CkAllocSt('IRat',0)
      return ! entry AllocFAInt


      entry AllocAuxes(nVrAux)
!       integer*2 nVrAux ! possibly larger than nVar, which may not yet be known

        allocate(AuxRHS    (nAux),stat=AS)
        call CkAllocSt('ARHS',0)
        allocate(AuxCovers (nAux,nVrAux),stat=AS)
        call CkAllocSt('AxCv',0)
        allocate(AColumn   (nAux,nVrAux),stat=AS) ! data varies 1st across rows
        call CkAllocSt('ACol',0)
      return ! entry AllocAuxes


      entry DeallocFAInt
        deallocate(iOfFacAlgs,stat=AS)
        call CkAllocSt('iOfC',1)
        deallocate(jOfCallers,stat=AS)
        call CkAllocSt('jOfC',1)
        deallocate(x,stat=AS)
        call CkAllocSt('xQty',1)
        deallocate(cOrg,stat=AS)
        call CkAllocSt('RawC',1)
        deallocate(cMod,stat=AS)
        call CkAllocSt('AlgC',1)
        deallocate(GLB,stat=AS)
        call CkAllocSt('GtLB',1)
        deallocate(LUB,stat=AS)
        call CkAllocSt('LtUB',1)
        deallocate(Capacity,stat=AS)
        call CkAllocSt('Capy',1)
        deallocate(EquivAvail,stat=AS)
        call CkAllocSt('EAvl',1)
        deallocate(UnitOwning,stat=AS)
        call CkAllocSt('Unit',1)
        deallocate(BlockPos,stat=AS)
        call CkAllocSt('BPos',1)
        deallocate(AuxRow,stat=AS)
        call CkAllocSt('ARow',1)
        deallocate(AuxCycled,stat=AS)
        call CkAllocSt('AxCy',1)
        deallocate(AuxiCovers,stat=AS)
        call CkAllocSt('AiCv',1)
        deallocate(BlockOfSameUnitAs,stat=AS)
        call CkAllocSt('BoSU',1)
!       deallocate(VarOfDispRank,stat=AS) ! moved to ObtainBasicFeasibleSolution
!       call CkAllocSt('VoDR',1)
!       deallocate(InputRate,stat=AS)
!       call CkAllocSt('IRat',1)
      return ! entry DeallocFAInt


      entry DeallocAuxes
        if(nAux>0) then
        deallocate(AuxRHS,stat=AS)
        call CkAllocSt('ARHS',1)
        deallocate(AuxCovers,stat=AS)
        call CkAllocSt('AxCv',1)
        deallocate(AColumn,stat=AS)
        call CkAllocSt('ACol',1)
        end if
      return ! entry DeallocAuxes


      entry AllocOCost
        allocate(PrevState(nVar),stat=AS)
        call CkAllocSt('PvSt',0)
        allocate(VarState(nVar),stat=AS)
        call CkAllocSt('VSta',0)
        allocate(PrevBDIRow(nVar),stat=AS)
        call CkAllocSt('PBDI',0)
        allocate(BasisDIRow(nVar),stat=AS)
        call CkAllocSt('BDIR',0)
        allocate(PrevBKyVar(nVar),stat=AS)
        call CkAllocSt('PBKV',0)
        allocate(BasisKyVar(nVar),stat=AS)
        call CkAllocSt('BKyV',0)
        allocate(FctOwning(nVar),stat=AS)
        call CkAllocSt('FOwn',0)
        allocate(KeyVar(nVar),stat=AS)
        call CkAllocSt('KeyV',0)
        allocate(d(nVar),stat=AS)
        call CkAllocSt('d   ',0)
        allocate(Phi(nVar),stat=AS)
        call CkAllocSt('Phi ',0)
        allocate(DualVar(nVar),stat=AS)
        call CkAllocSt('DVar',0)
        allocate(BkOwns(nVar),stat=AS)
        call CkAllocSt('BkOw',0)
        allocate(BindSetOwns(nVar),stat=AS)
        call CkAllocSt('BSOw',0)
        allocate(TestSetOwns(nVar),stat=AS)
        call CkAllocSt('TSOw',0)
        allocate(WouldBeZero(nVar),stat=AS)
        call CkAllocSt('WBeZ',0)
        allocate(VarInFctOrder(nVar),stat=AS)
        call CkAllocSt('ViFO',0)
        allocate(VFOFirstInFct(0:nVar),stat=AS) ! 0 allows room for InvalidFacet
        call CkAllocSt('VFiF',0)
        if(nAux>0) then
          allocate(BNKinRow(nAux),stat=AS)
          call CkAllocSt('BNKR',0)
          allocate(AuxDualMult(nAux),stat=AS)
          call CkAllocSt('AxDM',0)
          allocate(AuxColumn(nAux),stat=AS)
          call CkAllocSt('ACol',0)
          allocate(DColumn(nAux,nAux),stat=AS)
          call CkAllocSt('DCol',0)
          allocate(DOrgRow(nAux,nAux),stat=AS)
          call CkAllocSt('DORw',0)
          allocate(DInvRow(nAux,nAux),stat=AS)
          call CkAllocSt('DIRw',0)
        end if
        if(UsingCumul) then
          allocate(VarCumul(nKappa,-1:nVar),stat=AS)
          call CkAllocSt('Cumu',0)
        end if
      return ! entry AllocOCost


      entry DeallocOCost
        deallocate(PrevState,stat=AS)
        call CkAllocSt('PvSt',1)
        deallocate(VarState,stat=AS)
        call CkAllocSt('VSta',1)
        deallocate(PrevBDIRow,stat=AS)
        call CkAllocSt('PBDI',1)
        deallocate(BasisDIRow,stat=AS)
        call CkAllocSt('BDIR',1)
        deallocate(PrevBKyVar,stat=AS)
        call CkAllocSt('PBKV',1)
        deallocate(BasisKyVar,stat=AS)
        call CkAllocSt('BKyV',1)
        deallocate(FctOwning,stat=AS)
        call CkAllocSt('FOwn',1)
        deallocate(KeyVar,stat=AS)
        call CkAllocSt('KeyV',1)
        deallocate(d,stat=AS)
        call CkAllocSt('d   ',1)
        deallocate(Phi,stat=AS)
        call CkAllocSt('Phi ',1)
        deallocate(DualVar,stat=AS)
        call CkAllocSt('DVar',1)
        deallocate(BkOwns,stat=AS)
        call CkAllocSt('BkOw',1)
        deallocate(BindSetOwns,stat=AS)
        call CkAllocSt('BSOw',1)
        deallocate(TestSetOwns,stat=AS)
        call CkAllocSt('TSOw',1)
        deallocate(WouldBeZero,stat=AS)
        call CkAllocSt('WBeZ',1)
        deallocate(VarInFctOrder,stat=AS)
        call CkAllocSt('ViFO',1)
        deallocate(VFOFirstInFct,stat=AS)
        call CkAllocSt('VFiF',1)
        if(nAux>0) then
          deallocate(BNKinRow,stat=AS)
          call CkAllocSt('BNKR',1)
          deallocate(AuxDualMult,stat=AS)
          call CkAllocSt('AxDM',1)
          deallocate(AuxColumn,stat=AS)
          call CkAllocSt('ACol',1)
          deallocate(DColumn,stat=AS)
          call CkAllocSt('DCol',1)
          deallocate(DOrgRow,stat=AS)
          call CkAllocSt('DORw',1)
          deallocate(DInvRow,stat=AS)
          call CkAllocSt('DIRw',1)
        end if
        if(UsingCumul) then
          deallocate(VarCumul,stat=AS)
          call CkAllocSt('Cumu',1)
        end if
      return ! entry DeallocOCost


      entry StoreLoadCurve(LAST_POINT,LPROB,LODDUR)


        nPoints=LAST_POINT

        NonzeroPeak=LPROB(nPoints)>SmallReal
        if(nPoints==0) call HaltWith(0_2,1_2, &
          'Load duration curve needs #points>=')
        if((nPoints>LimPoints).or.(NonzeroPeak.and. &
           (nPoints==LimPoints))) call HaltWith &
          (nPoints,LimPoints,'Load duration curve needs #points<=')

        do iPoint=1,nPoints

          EqLoad(iPoint)=LODDUR(iPoint)
          EqLDn0(iPoint)=LPROB (iPoint) ! area under curve has same units as EqLoad

          if(iPoint>1) then ! correct local arrays for non-monotonic input curve
            NoniX=EqLoad(iPoint)<=EqLoad(iPoint-1)
            IncrY=EqLDn0(iPoint)> EqLDn0(iPoint-1)
            if(NoniX.or.IncrY) then ! adjust one or both to conform to a CDF
              if(PrtDetail>0) write( PrtUni,'(1x,i3,f14.6,f10.7/1x,i3,f14.6,f10.7\)' ) &
			  iPoint-1,EqLoad(iPoint-1),EqLDn0(iPoint-1), iPoint  ,EqLoad(iPoint  ),EqLDn0(iPoint  )
              if(IncrY) then
                if(PrtDetail>0) write(PrtUni,Fmt1a) &
                  '^ incr load ordinates'
                EqLDn0(iPoint)=EqLDn0(iPoint-1)
              end if
              if(NoniX) then
                if(PrtDetail>0) write(PrtUni,Fmt1a) &
                  '       ^ non-incr load abscissas'
                EqLoad(iPoint)=EqLoad(iPoint-1)*1.00001
              end if
            end if
          end if
          if(PrtDetail>1) write(PrtUni,'(1x,i3,f14.6,f10.7,1x,50a1)') &
            iPoint,EqLoad(iPoint),EqLDn0(iPoint),('*',j=1, &
            nint(50.*EqLDn0(iPoint)))
        end do

        if(NonzeroPeak) then
          if(PrtDetail>0) write(PrtUni,'(1x,i3,f14.6,f10.7,a)')nPoints, &
            EqLoad(nPoints),EqLDn0(nPoints), &
            ' peak extended by 0.001% to zero probability'
          nPoints=nPoints+1
          EqLoad(nPoints)=EqLoad(nPoints-1)*1.00001
          EqLDn0(nPoints)=0.0
        end if
        EqLoad(0)=-EqLoad(nPoints) ! extend the base duration to negative loads
        BaseLoad=EqLoad(1)
        Duration=EqLDn0(1) ! unity within Midas, as EqLDn0 has load probabilities
        EqLDn0(0)=Duration
      return ! entry StoreLoadCurve


      entry CompressElement(jD,jM,CELB,CEUB) ! delete array elements indexed jD


        iCE=iOfFacAlgs(jD) ! save i index of M>1 before overwriting iOfFacAlgs(jD)

        jOfCallers(iCE)=jM ! j index of var containing merger of jM and jD
        I4_2nRem=int(2*(nVar-jD),4)
        I4_4nRem=2*I4_2nRem
        if(PrtDetail>2) write(PrtUni,'(1x,7i3,7f10.4)')jM,jD, &
          BlockOfSameUnitAs(jD,1), &
          BlockOfSameUnitAs(jD,2), &
          iOfFacAlgs(jD), &
          UnitOwning(jD), &
          BlockPos  (jD), &
          Capacity  (jD), &
          EquivAvail(jD), &
          cOrg      (jD), &
          cMod      (jD), &
          x         (jD)


        do iCE=jD+1,jD+nVar
          iOfFacAlgs(iCE-1)=iOfFacAlgs(iCE)
          UnitOwning(iCE-1)=UnitOwning(iCE)
          BlockPos  (iCE-1)=BlockPos  (iCE)
          Capacity  (iCE-1)=Capacity  (iCE)
          EquivAvail(iCE-1)=EquivAvail(iCE)
          CELB      (iCE-1)=CELB      (iCE)
          CEUB      (iCE-1)=CEUB      (iCE)
   
          cOrg      (iCE-1)=cOrg      (iCE)
          cMod      (iCE-1)=cMod      (iCE)
          x         (iCE-1)=x         (iCE)
          BlockOfSameUnitAs(iCE-1,1)=BlockOfSameUnitAs(iCE,1)
          BlockOfSameUnitAs(iCE-1,2)=BlockOfSameUnitAs(iCE,2)
        end do
        do iCE=1,nDisp ! decrement by 1 those values>jD in arrays that return j
          if(jOfCallers   (iCE)>jD) jOfCallers   (iCE)=jOfCallers(iCE)-1
        end do
        do iCE=1,nVar
          if(VarOfDispRank(iCE)>jD) VarOfDispRank(iCE)=VarOfDispRank(iCE &
            )-1
          if(BlockOfSameUnitAs(iCE,1)>jD) &
             BlockOfSameUnitAs(iCE,1)=BlockOfSameUnitAs(iCE,1)-1
          if(BlockOfSameUnitAs(iCE,2)>jD) &
             BlockOfSameUnitAs(iCE,2)=BlockOfSameUnitAs(iCE,2)-1
        end do

      return ! entry CompressElement


      entry ShowRank


        write(PrtUni,'(1x,26i3)')(jOfCallers   (jSR),jSR=1,nDisp)
        write(PrtUni,'(1x,26i3)')(iOfFacAlgs   (jSR),jSR=1,nVar)

        do jSR=1,nVar-1
          iSR=iOfFacAlgs(jSR)
          if(iSR>0) then
            if(jOfCallers(iSR)/=jSR) write(PrtUni,'(1x,3i4,a)') &
              jSR,iSR,jOfCallers(iSR),' j array corruption'
          end if
        end do

      return


      entry StorePlantData( &
        UNIT,BLKNO,NBLOK2,NUM_ARB_CONSTRAINTS,NUM_CUMULANTS, &
        LOWER_BOUND_CAP_FACTOR, &
        UPPER_BOUND_CAP_FACTOR,MWBLOK, &
        eavail_fo,MAINTENANCE_RATE,NUNITS)




        nCFLBVar=0
        call GET_NUM_DYN_STORAGE(nStgEqn) ! from ENRGLIMT. 9/29/95. GAT.

        do i=1,NBLOK2 ! count those with positive lower bounds on capacity factor
          if((0.0<LOWER_BOUND_CAP_FACTOR(i)).and. &
             (    LOWER_BOUND_CAP_FACTOR(i)<eavail_fo(i)).and. &
             (    LOWER_BOUND_CAP_FACTOR(i)<UPPER_BOUND_CAP_FACTOR(i))) &
          then
            if(BLKNO(i)/=1) call HaltWith(BLKNO(i),i, &
              'invalid block-position for CF-lower-bounded var')
            nCFLBVar=nCFLBVar+1
          end if
        end do


        if(PrtDetail>2) write(PrtUni,'(1x,4i6)')nStgEqn,nBlok2,nCFLBVar, &
          NUNITS
        nVar=NBLOK2  & !  to be reduced by the count of zero-capacity blocks
          +nStgEqn*2 & !  each storage equation must involve exactly 2 vars
          +nCFLBVar  ! count of vars having a non-zero lower-bound on CF
        nAux=NUM_ARB_CONSTRAINTS+nStgEqn

        nKappa=4
        if(nKappa>LimCumulants) nKappa=LimCumulants
        if(nKappa==1) nKappa=2 ! variance must be non-zero
        UsingCumul=nKappa>0
        BloomsPath=.false. !.true. ! follow an iteration path to match Bloom's example
        call ChkReLim(nVar,LimUnknowns,VarString)
        call ChkReLim(nAux,LimAuxEquations,'aux-equation')
        nVarUB=min(nVar+nAux+int(1,2),LimUnknowns) ! limit the memory allocated
        I4_nVUB=nVarUB ! of only local usefulness

        ! most of the allocatable arrays are needed before nVar is finally known
        call AllocFAInt(nVarUB) ! include AuxCycled & AuxiCovers here for convenience
        ! allocate locals:
        allocate(AuxCoeff(nVarUB),stat=AS)
        call CkAllocSt('AuxC',0)
        allocate(CFLB    (nVarUB),stat=AS)
        call CkAllocSt('CFLB',0)
        allocate(CFUB    (nVarUB),stat=AS)
        call CkAllocSt('CFUB',0)
        allocate(BlockDispCost(NUNITS,2),stat=AS)
        call CkAllocSt('BkDC',0)
        allocate(BlockHeatRate(NUNITS,2),stat=AS)
        call CkAllocSt('BkHR',0)


        call initw1(iOfFacAlgs,nVarUB,InvalidIndex)
        call initw1(jOfCallers,nVarUB,InvalidIndex) ! default => capacity=0
        call initw1(AuxRow    ,nVarUB,InvalidIndex)
        call initb1(AuxCycled ,nVarUB,FalseByte)
        call initd1(x,nVarUB,0.0) ! default implying var is not an aux slack

        if(PrtDetail>2) write(PrtUni,Fmt1a)' jV  Un B Min_CF Max_CF '// &
          'Avlbty MntRat  RawCost BlokSize Capacity SystCapy'
        SysCap_loc=0.0
        SupCost=0.0

        if(nStgEqn>0) call GET_DYN_STORAGE_VARIABLES &
          (UnitOwning,BlockPos,CFLB,CFUB,cOrg,Capacity, &
          EquivAvail,SupCost,SysCap_loc,PrtUni,Fmt8f,PrtDetail) ! from ENRGLIMT

        call GET_BLOCK_DISP_COST(UNIT,BLKNO,BlockDispCost,NBLOK2)


      ! compress sets of identical return/discharge pairs
        nDSERed=0 ! number of redundant dynamic-storage equations
        Multiplicity=0 ! preclude comparison with a prior block
        j1=1    ! compressed index of return var at Multiplicity of 1
        j2=j1+1 ! compressed index of dischg var at Multiplicity of 1
        i=0
        do while(i<nStgEqn-nDSERed) ! assume identical vars' indices are separated by 2
          i=i+1
          jDis=i*2    ! index of dischg var in a yet-uncompressed pair
          jRet=jDis-1 ! index of return var
          if(Multiplicity==0) then
            Multiplicity=-1
          elseif( & ! assume that CFLB, CFUB, UnitOwning, & BlockPos track these:
            (EquivAvail(jRet)==EquivAvail(j1)).and. &
            (EquivAvail(jDis)==EquivAvail(j2)).and. &
            (Capacity  (jRet)==Capacity  (j1)).and. &
            (Capacity  (jDis)==Capacity  (j2)).and. &
            (cOrg      (jRet)==cOrg      (j1)).and. &
            (cOrg      (jDis)==cOrg      (j2))) then
            Multiplicity=Multiplicity-1
            if(i==nStgEqn-nDSERed) Multiplicity=-Multiplicity ! end of dynamic storage vars
          else
            Multiplicity=-Multiplicity ! positive => end of run of identical vars
          end if
          if(Multiplicity>1) then ! end of identical blocks => compress
            j3=j1+2 ! index of first array position to be overwritten
            BlockPos(j1)=-Multiplicity
            BlockPos(j2)=-Multiplicity
            Capacity(j1)=real(Multiplicity)*Capacity(j1)
            Capacity(j2)=real(Multiplicity)*Capacity(j2)

            I4_BRem=int(2*((nStgEqn-nDSERed)-(i-1))-1,4) ! #-1 of elements to be moved
            if(PrtDetail>2) write(PrtUni,'(1x,6i3,2f11.3,a)') &
              I4_BRem,nStgEqn,nDSERed,jDis,j1,j3, &
              Capacity(j1),Capacity(j3),' bef cm'

            if(I4_BRem>=0) then ! left-shift data for storage vars following
              j0=j1+Multiplicity*2

              do i1=0,I4_BRem
                UnitOwning(j3+i1)=UnitOwning(j0+i1)
                BlockPos  (j3+i1)=BlockPos  (j0+i1)
                CFLB      (j3+i1)=CFLB      (j0+i1)
                CFUB      (j3+i1)=CFUB      (j0+i1)
                EquivAvail(j3+i1)=EquivAvail(j0+i1)
                Capacity  (j3+i1)=Capacity  (j0+i1)
                cOrg      (j3+i1)=cOrg      (j0+i1)
              end do
              if(PrtDetail>2) write(PrtUni,'(1x,6i3,2f11.3,a)') &
                I4_BRem,nStgEqn,nDSERed,jDis,j1,j3, &
                Capacity(j1),Capacity(j3),' aft cm'
            end if
            nDSERed=nDSERed+Multiplicity-1
            i      =i      -Multiplicity+1
          end if
          if(PrtDetail>2) write(PrtUni,'(1x,6i3,f11.3,a)') &
            i,nStgEqn,nDSERed,jDis,j1,Multiplicity,Capacity(jDis), &
            ' DSERed'
          if(Multiplicity>0) then
            Multiplicity=-1 ! index the beginning of a new run
            j1=j1+2 ! compressed index of return var at Multiplicity of 1
            j2=j1+1 ! compressed index of dischg var at Multiplicity of 1
          end if
        end do
        nStgEqn=nStgEqn-nDSERed
        nAux   =nAux   -nDSERed
        I4_nAux=nAux
        I4_4nAux=nAux*4
        if(nAux>0) then
          call AllocAuxes(nVarUB) ! allocation must use the final value of nAux

          call initb1(AuxCovers,int(nAux*nVarUB,2),FalseByte)
        end if

        j=nStgEqn*2
        do i=1,NBLOK2 ! move caller's capacity-limited data into local arrays
          iUnit=UNIT(i)
          Capaci=MWBLOK(i)*(1.-MAINTENANCE_RATE(iUnit)) ! derate capacity by MR
          j=j+1 ! assume Capaci>0 until parameters are displayed
          iOfFacAlgs(j)=i
          jOfCallers(i)=j
          CFLB(j)=LOWER_BOUND_CAP_FACTOR(i)
          CFUB(j)=UPPER_BOUND_CAP_FACTOR(i)
          Capacity(j)=Capaci
          UnitOwning(j)=iUnit
          BlockPos(j)=BLKNO(i)
          cOrg(j)=BlockDispCost(iUnit,max(BlockPos(j),int(1,2))) ! negative is allowed

          EquivAvail(j)=eavail_fo(iUnit) ! use availability to limit capacity factor:
          if(CFUB(j)>EquivAvail(j)) CFUB(j)=EquivAvail(j)
        ! The reduction of EA to the lesser of the pair is required for convolution
        ! of outages to properly account for quantity unserved for whatever reason;
        ! if CFUB (through the program-wide variable LUB) were to limit x more than
        ! EquivAvail had, the cumulants (based on EquivAvail) would not adequately
        ! convey the change in unserved quantity after convolution.
          if(SupCost<cOrg(j)) SupCost=cOrg(j)
          SysCap_loc=SysCap_loc+Capaci
          if(PrtDetail>2) write(PrtUni,Fmt8f)j,iUnit, &
            BlockPos(j),CFLB(j),CFUB(j),EquivAvail(j), &
            MAINTENANCE_RATE(iUnit),cOrg(j),MWBLOK(i),Capaci,SysCap_loc
          if(Capaci<SmallReal*MWBLOK(i)) then ! ignore vars with capacity on outage

            jOfCallers(i)=InvalidIndex ! since no results can be reported
            j=j-1
          end if
        end do
        nVar=j
        nDisp=NBLOK2 ! size of jOfCallers, for use within CompressElement

      ! nCFLBVar above allows room in arrays for splitting of block 1 into 2;
      ! convert single-block vars with CFLB>0 into two vars with the 1st a must-run
        i=nVar
        do j=1,nVar
          if((0.0<CFLB(j)).and.(CFLB(j)<EquivAvail(j)).and. &
            (CFLB(j)<CFUB(j)).and.(BlockPos(j)==1)) then
            i=i+1 ! create new var for the residual of block cap less must-run portion
            nDisp=nDisp+1
            Capaci=Capacity(j)
            Capacity(j)=Capaci*CFLB(j)/EquivAvail(j) ! must-run portion
            Capacity(i)=Capaci-Capacity(j) ! residual
            EquivAvail(i)=EquivAvail(j)
            CFUB(i)=amin1(EquivAvail(j),CFUB(j)) ! limit CFUB to availability
            CFUB(i)=Capaci*(CFUB(i)-CFLB(j))/Capacity(i)
            CFUB(j)=EquivAvail(j) ! force x(j) to exactly EquivAvail(j)*Capacity(j)
            CFLB(j)=CFUB(j) ! (GLB==LUB) => need to augment x(j) in MinimizeCost
            CFLB(i)=0.0
            iOfFacAlgs(i)=nDisp ! not iOfFacAlgs(j)
            jOfCallers(iOfFacAlgs(i))=i
            BlockPos(j)=0 ! var j is the must-run portion
            BlockPos(i)=2 ! var i is the residual; prior extant block 2 is disallowed
            cOrg(i)=cOrg(j)

            UnitOwning(i)=UnitOwning(j)
          end if
        end do
        nVar=i


        call initw1(BlockOfSameUnitAs,int(nVarUB*2,2),InvalidIndex)
        SupcModBP0=-LargeReal ! 0 should suffice
        do j=1,nVar ! none of the vars added beyond current nVar has BlockPos==2
          VarOfDispRank(j)=j ! before constrained merit-order sorting
          if((j <= nStgEqn*Two2).and.(mod(j,Two2) == One2)) & !  j is the return var of the pair;
            cOrg(j)=-cOrg(j+1) ! dispatch storage-return vars in reverse-econ order
          cMod(j)=cOrg(j)
          if(BlockPos(j)==0) then ! 'must-run' block
            cMod(j)=cMod(j)*0.001 ! dispatch early, even if cOrg(j) is large
            if(SupcModBP0<cMod(j)) SupcModBP0=cMod(j)
          elseif(BlockPos(j)==2) then ! search for BlockPos==0 or 1 in the same unit

            i=UnitOwning(j)
            j1=1
            do while((j1<nVar).and.((UnitOwning(j1)/=i).or. &
              (BlockPos(j1)==2)))
              j1=j1+1
            end do
            BlockOfSameUnitAs(j ,1)=j1
            BlockOfSameUnitAs(j1,2)=j
          end if
        end do

!       using prime coefficients to make non-identical matches unlikely,
!       perturb cMod in attempt to make identical blocks contiguous after Sort:
        do j=(nStgEqn*2)+1,nVar
          if((BlockPos(j)==1).and. &
            (BlockOfSameUnitAs(j,2)==InvalidIndex)) cMod(j)=cOrg(j) &
              -0.07100*EquivAvail(j) & !  distinguish blocks by availability
              -0.00037*Capacity(j)   ! load larger blocks before smaller
        ! enforce dispatch of BlockPos==0 before others which might have cOrg=0
          if(cMod(j)<=0.0) call HaltWith(UnitOwning(j),BlockPos(j), &
            'unit has suspect data; check cost for block')
          if((BlockPos(j)>0).and.(cMod(j)<SupcModBP0)) &
            cMod(j)=SupcModBP0*2.0
        end do

        call Sort(nVar,VarOfDispRank,cMod,.true.) ! before addition of aux slacks or UQ server
        if(PrtDetail>2) call ShowRank
        EcoReversed=.false.
        i=1
        do while(i<=nVar) ! merge economically reversed blocks within a single unit
          j2=VarOfDispRank(i)
          if((PrtDetail>2).and.(j2>(nStgEqn*2))) &
            write(PrtUni,Fmt8f)j2,UnitOwning(j2),BlockPos(j2), &
            CFLB(j2),CFUB(j2),EquivAvail(j2), &
            MAINTENANCE_RATE(UnitOwning(j2)),cMod(j2),Capacity(j2)
          if(BlockPos(j2)==2) then
            j1=BlockOfSameUnitAs(j2,1)
            if(cMod(j1)>=cMod(j2)) then
            ! this unit's two blocks are economically equal or reversed, so merge
            ! them into a single block with capacity-averaged cost, since they must
            ! logically be dispatched as adjacent blocks; note that if j1 is a
            ! 'must-run' block, then the above cMod comparison will fail (hence,
            ! incoming BlockPos(j1) is not 0); that is, the use of cMod instead
            ! of cOrg to order the dispatch has already eliminated 'must-run'
            ! blocks from consideration here
              EcoReversed=.true.
              if(PrtDetail>2) write(PrtUni,'(1x,3i3,2f9.3,a)') &
                nVar,j1,j2,cMod(j1),cMod(j2),' econ-rev pair'
              if(j1>j2) call HaltWith(j1,j2, &
                'block 1 listed after block 2: ') ! unlikely
              Capaci=Capacity(j1)+Capacity(j2)
              IF(CAPACI < .0001) THEN
               CAPACI = CAPACI
              ENDIF
              cOrg(j1)=(Capacity(j1)*cOrg(j1)+ &
                        Capacity(j2)*cOrg(j2))/Capaci ! for costing
              cMod(j1)=(Capacity(j1)*cMod(j1)+ &
                        Capacity(j2)*cMod(j2))/Capaci ! for sorting
              CFLB(j1)=(Capacity(j1)*CFLB(j1)+ &
                        Capacity(j2)*CFLB(j2))/Capaci
              CFUB(j1)=(Capacity(j1)*CFUB(j1)+ &
                        Capacity(j2)*CFUB(j2))/Capaci
              Capacity(j1)=Capaci ! can't assume CFLB & CFUB are identical for both blocks
              BlockPos(j1)=3 ! indicating the merger of blocks 1 & 2
              BlockOfSameUnitAs(j1,2)=InvalidIndex ! since j1 is single-block now
              call CompressElement(j2,j1,CFLB,CFUB) ! assume j1<j2

              call CopyLinIVec1(VarOfDispRank(i+1),VarOfDispRank(i), &
                nVar-i)
              i=i-1 ! examine the new VarOfDispRank(i) at next iteration
              nVar=nVar-1 ! reduce the count of vars
              if(PrtDetail>2) write(PrtUni,Fmt8f)j1,UnitOwning(j1), &
                BlockPos(j1),CFLB(j1),CFUB(j1),EquivAvail(j1), &
                MAINTENANCE_RATE(UnitOwning(j1)),cMod(j1),Capacity(j1)
            end if
          end if
          i=i+1
        end do
        if(PrtDetail>2) call ShowRank
        if(EcoReversed) then ! some economically reversed pairs were merged
          do j=1,nVar
            VarOfDispRank(j)=j ! before resorting required by change of costs above
          end do
          call Sort(nVar,VarOfDispRank,cMod,.true.) ! after merging reversed blocks
          if(PrtDetail>2) call ShowRank
        end if

        Multiplicity=0
        if(PrtDetail>2) j0=VarOfDispRank(1)
        i=1
        do while(i<=nVar) ! combine multiple identical single-block units
          j=VarOfDispRank(i)
        ! if((PrtDetail>2).and.(i>1))write(PrtUni,'(1x,8i3,2f10.3,5l2)')
          if (PrtDetail>2)           write(PrtUni,'(1x,8i3,2f10.3,5l2)') &
            nVar,i,j,BlockPos(j),BlockOfSameUnitAs(j,2), &
            UnitOwning(j),iOfFacAlgs(j),Multiplicity,Capacity(j), &
            cMod(j),cMod(j)==cMod(j0), &
            EquivAvail(j)==EquivAvail(j0), &
            Capacity  (j)==Capacity  (j0), &
            CFLB(j)==CFLB(j0), &
            CFUB(j)==CFUB(j0)
          if((j>nStgEqn*2).and. & ! discharge & return vars are aggregated above
             (BlockPos(j)==1).and. &
             (BlockOfSameUnitAs(j,2)==InvalidIndex)) then
          ! no second block exists for this unit; compression is possible;
          ! allowing compression for BlockPos=2 would complicate deconvolution;
            if(CFUB(j)<EquivAvail(j)) then ! derate capacity (e.g. for shadow plant)
              Capacity(j)=Capacity(j)*CFUB(j)/EquivAvail(j)
              if(PrtDetail>2) write(PrtUni,'(1x,i4,f10.3,2f9.6,a)') &
                j,Capacity(j),CFUB(j),EquivAvail(j),' derated by CF/EA'
              CFUB(j)=EquivAvail(j)
            end if ! currently allow for shadow only on single-block plants
            if(Multiplicity==0) then ! preclude comparison with a prior block
              Multiplicity=-1
            elseif((cMod(j)==cMod(j0)).and. &
              (EquivAvail(j)==EquivAvail(j0)).and. &
              (CFLB(j)==CFLB(j0)).and.(Capacity(j)==Capacity(j0)).and. &
              (CFUB(j)==CFUB(j0))) then
              Multiplicity=Multiplicity-1
              if(i==nVar) Multiplicity=-Multiplicity ! end of vars
            else
              Multiplicity=-Multiplicity ! positive => end of run of identical vars
            end if
          else
            Multiplicity=-Multiplicity ! positive => end of run of identical vars
          end if
          if(Multiplicity>1) then ! end of identical blocks => compress
            i1=i-Multiplicity ! identical vars have consecutive rank i, not j
          ! revise VarOfDispRank such that within this group of identical blocks,
          ! the block with the lowest j has the lowest rank, since CompressElement
          ! requires j1<j2 below; perturbing cMod failed when Sort reordered too many
            do i2=i1,i-1
              iMinJ=i2
              do i3=i2+1,i-1 ! compare all higher-indexed vars within this M-group
                if(VarOfDispRank(iMinJ)>VarOfDispRank(i3)) iMinJ=i3 ! i of least j
              end do
              j2=VarOfDispRank(i2) ! swap j vars (harmless if i2=iMinJ)
              VarOfDispRank(i2)=VarOfDispRank(iMinJ)
              VarOfDispRank(iMinJ)=j2
              if(PrtDetail>3) write(PrtUni,'(1x,6i4)')nVar,i1,i,j2, &
                iMinJ,VarOfDispRank(i2)
            end do
            j1=VarOfDispRank(i1) ! index of var at Multiplicity of 1
            if(PrtDetail>2) write(PrtUni,'(1x,7i3,a)')nVar,i,j,i1,j1, &
              UnitOwning(j1),Multiplicity,' bef CE'
            BlockPos(j1)=-Multiplicity
            Capacity(j1)=real(Multiplicity)*Capacity(j1)
            do while(i1+1<i) ! skip array positions at Multiplicity of 2,3,...
              i1=i1+1 ! sequence index of var with next higher Multiplicity index
              j2=VarOfDispRank(i1)
              call CompressElement(j2,j1,CFLB,CFUB) ! requires j1<j2
            end do

            call CopyLinIVec1(VarOfDispRank(i), &
                              VarOfDispRank(i-Multiplicity+1),nVar-i+1)
            nVar=nVar-Multiplicity+1
            if(PrtDetail>2) write(PrtUni,'(1x,7i3,a)')nVar,i,j,i1,j1, &
              UnitOwning(j1),Multiplicity,' aft CE'

            i=i-Multiplicity+1 ! account for the (Multiplicity-1) vars compressed
          end if
          if(Multiplicity>0) Multiplicity=-1 ! index the beginning of a new run
          j=VarOfDispRank(i)
          if((BlockPos(j)==1).and. &
            (BlockOfSameUnitAs(j,2)==InvalidIndex).and. &
            (Multiplicity==0)) Multiplicity=-1 ! allow var j to begin a new group
          j0=j
          i=i+1
        end do

        nDisp=nVar ! limit the merit-order loop in ObtainBasicFeasibleSolution

        SmallCost=SmallReal*SupCost ! make thresholds relate to the data
        SmallArea=SmallReal*SysCap_loc ! *Duration
        LargeArea=LargeReal*SysCap_loc ! *Duration

        iAuxOrg=0 ! nAux was reduced above
        do iAux=1,nAux ! copy from Midas program vars into AuxCoeff,AuxRHS
          if(iAux<=nStgEqn) then ! note implicit assumption that nVarInAux=2
            j2=iAux*2 ! index of the discharge var in the pair
            Multiplicity=-BlockPos(j2)
            if(Multiplicity<=0) Multiplicity=1
          end if

          call initd1(AuxCoeff,nVarUB,0.0)

          if(iAux<=nStgEqn) then ! use the pre-compression index into caller's arrays
            iAuxOrg=iAuxOrg+Multiplicity ! assume AuxCoeff and AuxRHS track Capacity & cOrg
            call GET_DYN_STORAGE_AUX_CONSTRAINT(iAux,iAuxOrg,nVarInAux, &
              AuxCoeff,AuxRHS)

          end if

          if(iAux<=nStgEqn) then
            AuxRHS(iAux)=real(Multiplicity)*AuxRHS(iAux)

          end if

          do j=1,nVarUB ! nVar is not finally determined until the end of iAux loop
            if(abs(AuxCoeff(j))>SmallReal) then
              AuxCovers(iAux,j)=.true.
              if(AuxRow(j)==InvalidIndex) AuxRow(j)=iAux

              if(PrtDetail>2) write(PrtUni,'(1x,2i3,f12.3,f9.3)')iAux,j, &
                AuxCoeff(j),x(j)
            end if
          end do
          if(AuxRHS(iAux)<0.0) call &
            HaltWith(iAux,iAux,'aux constraint RHS may not be negative')
          do j=1,nVarUB
            AColumn(iAux,j)=AuxCoeff(j) ! storage locations are not adjacent here
          end do
        end do ! loop over iAux


        CostUQ=SupCost*1.1 ! justifying the position of x(nVar) as last-loaded
        CapacUQ=SysCap_loc ! *2.0 ! sufficient
        nVar=nVar+1 ! for the artificial var to cover the Unserved Quantity
        call ChkReLim(nVar,LimUnknowns,VarString)
        I4_nVar=nVar ! word var for passing to Spindrift routines
        I4_2nVar=nVar*2
        UnitOwning(nVar)=InvalidIndex ! =nVar is possibly not unique
        BlockPos  (nVar)=1
        iOfFacAlgs(nVar)=InvalidIndex
        EquivAvail(nVar)=1.0
        CFLB(nVar)=0.0
        CFUB(nVar)=1.0
        cOrg(nVar)=CostUQ
        cMod(nVar)=CostUQ
        Capacity(nVar)=CapacUQ

        if(PrtDetail>2) write(PrtUni,Fmt1a) &
          ' jV iC Un BP    x     Cost Capacity Q_LowerB Q_UpperB'
        do j=1,nVar ! translate CF limits into quantity limits
          AuxiCovers(j)=AuxRow(j)/=InvalidIndex
          if(x(j)<0.0) then ! var j is an aux slack
            iAux=AuxRow(j) ! unique for aux slacks

            SupRHS=AuxRHS(iAux) ! /Duration ! adjust non-zero RHS for Duration
            if(SupRHS<SmallReal) then
              do jAux=1,nVar ! scan the negative coefficients
                if((jAux/=j).and.AuxCovers(iAux,jAux)) then
                  RHS=-AColumn(iAux,jAux)*Capacity(jAux)
                  if(SupRHS<RHS) SupRHS=RHS ! save the largest magnitude
                end if
              end do
            end if
            Capacity(j)=SupRHS/AColumn(iAux,j) ! AColumn(iAux,j)>0
            if(PrtDetail>2) write(PrtUni,'(1x,2i3,3f7.1,a)') &
              j,iAux,AuxRHS(iAux),SupRHS,Capacity(j),' for aux slack'
            cOrg(j)=0.0
            cMod(j)=0.0
!           usage of slack vars does not affect cost; even a small
!           positive cMod(j) here gives incentive to merge vars of the same aux
!           constraint into a single facet, allowing one var to 'split' the
!           other and thus allowing their capacity factors to be equal, even
!           under a strictly decreasing demand curve; this violates the intent
!           of those aux constraints which require CF2<=CF1, for example; this
!           issue is not resolved by initializing OptDual to SmallCost instead
!           of 0, nor by inputting a positive AuxRHS, nor by putting a positive
!           lower bound on the aux slacks (CFLB below); I believe it is a fault
!           inherent in the facet approach (AGT editorial on 1/30/95);

!           2/8/95 circumvention requires (currently manual) insertion of a
!           dummy variable in the data-file, having small (e.g. 0.001) capacity
!           and 0 cost, and using it to split the CF constraint into two; although
!           the 0 cost does not match the cost of either real var, the dummy
!           var does 'split' either var's resultant facet, hence serves to
!           separate the vars having CF1 and CF2 into distinct facets; the
!           energy absorbed by this dummy var could later be assigned to its
!           nearest neighbor at its operating cost, but the technique works even
!           for vanishingly small dummies (e.g. 1e-8), yet only when the costs
!           of vars 1 & 2 are non-decreasing; it fails when an economic reversal
!           is required to satisfy CF1>CF2

!           8/27/96 The above problem of keeping CF2<=CF1 in any single unit is
!           now considered academic, since we now merge blocks 1 & 2 in cases
!           for which they have equal or decreasing cost
            CFLB(j)=0.0 ! not read from file
            CFUB(j)=1.0 ! not read from file
            EquivAvail(j)=1.0
          end if ! expletion of array elements for aux-slack vars
          GLB(j)=CFLB(j)*Capacity(j) ! *Duration
          LUB(j)=CFUB(j)*Capacity(j) ! *Duration
          if(PrtDetail>2) write(PrtUni,'(1x,4i3,f5.1,f9.3,3f9.2)') &
            j,iOfFacAlgs(j),UnitOwning(j),BlockPos(j),x(j),cMod(j), &
            Capacity(j),GLB(j),LUB(j)
        end do

!       release locally allocated memory:
        deallocate(AuxCoeff,stat=AS)
        call CkAllocSt('AuxC',1)
        deallocate(CFLB,stat=AS)
        call CkAllocSt('CFLB',1)
        deallocate(CFUB,stat=AS)
        call CkAllocSt('CFUB',1)
        deallocate(BlockDispCost,stat=AS)
        call CkAllocSt('BkDC',1)
        deallocate(BlockHeatRate,stat=AS)
        call CkAllocSt('BkHR',1)
      return ! entry StorePlantData


      entry MinimizeCost(NBLOK2,UNIT,BLKNO,MWBLOK,MAINTENANCE_RATE, &
        ENERGY,UnservedQ,xDynStg)


        call AllocOCost
        call ObtainBasicFeasibleSolution

        do while(ErrorsTrapped==0)
          if(ErrorsTrapped==0) call InvertAuxBasis
          if(ErrorsTrapped==0) then
            if(PrtDetail>1) call ShowVectors
            if(PrtDetail>0) call ShowCostAtIter(UnservedQ)
            PrevIter=nIter
            do 
              if(ErrorsTrapped==0) call ChooseDescentDirection ! decides Optimal
              if(.not.Optimal) then
                if(ErrorsTrapped==0) call FigureDescentExtent
                if(ErrorsTrapped==0) call ReviseBasisVectors ! increments nIter
              end if
              if (Optimal.or.(nIter>PrevIter)) exit
            end do
          end if
          if(Optimal.or.(ErrorsTrapped/=0)) exit
        end do
        call DeallocOCost
        if(ErrorsTrapped/=0) return

        OptCost=InnerProduct(cOrg,x,nVar-1_2) ! x(nVar) is an artificial var
        xAccum=0.0
        xDynStg=0.0
        if(PrtDetail>1) write(PrtUni,Fmt1a) 'iBk Un Bk jV BP   ' &
          //'Capacity       x    ENERGY(B,U) in MinimizeCost'
        iBlk=0
        do i=1,nStgEqn ! i and j are in the system of compressed indices
          j=i*2 ! index of the discharge unit in the pair
          CallersQ=x(j)
          xAccum=xAccum+CallersQ*(1.0-Capacity(j-1)/Capacity(j)) ! rationale is lost
          xDynStg=xDynStg+CallersQ
          Multiplicity=-BlockPos(j)
          if(Multiplicity<=0) Multiplicity=1 ! 0 implies a bug in logic
          CallersQ=x(j)/real(Multiplicity)
          Capacity(j)=Capacity(j)/real(Multiplicity) ! useful only if PrtDetail>1
          do iMult=1,Multiplicity ! allocate x(j) across Multiplicity units
            iBlk=iBlk+1 ! iBlk is in the system of uncompressed indices

            call DYN_STORAGE_RESULTS(iBlk,CallersQ) ! report only generation to caller

            if(PrtDetail>1) write(PrtUni,'(1x,i3,i6,i3,3x,4f11.3,a)') &
              iBlk,Multiplicity,iBlk*2,Capacity(j),x(j),CallersQ, &
              MeanLoad-xAccum,'=UQ'
          end do
        end do
        do iBlk=1,NBLOK2
          j=jOfCallers(iBlk)
          if(j==InvalidIndex) then ! effective block-capacity was (nearly) 0
            CallersQ=0.0 ! perhaps redundant to caller's actions
            if(PrtDetail>1) write(PrtUni,'(1x,4i3,f14.3,11x,f11.3)') &
              iBlk,UNIT(iBlk),BLKNO(iBlk),j,MWBLOK(iBlk),CallersQ
          else
            if(BlockPos(j)<0) then ! var j was the merger of n identical blocks
              CallersQ=x(j)/real(-BlockPos(j)) ! allocate x(j) equitably among blocks
            else
              if(BlockPos(j)<=2) then
                if((BlockPos(j)==0).and.(GLB(j)==LUB(j))) then ! j emulated a must-run;
                ! augment x & Capacity by those of the must-run's residual block i
                  i=BlockOfSameUnitAs(j,2)
                  if(i<1) call HaltWith(j,iBlk,'invalid BoSUA')
                  x(j)=x(j)+x(i)
                  Capacity(j)=Capacity(j)+Capacity(i)
                  BlockPos(j)=1 ! restore original
                end if
                CallersQ=x(j)
              else ! var j was the merger of 2 economically reversed blocks
                CallersQ=x(j)*(MWBLOK(iBlk)* &
                  (1.-MAINTENANCE_RATE(UNIT(iBlk))))/Capacity(j)
                ! allocate the total in x(j) re effective capacity
              end if
            end if
            xAccum=xAccum+CallersQ
            if((PrtDetail>1).or.((PrtDetail>0).and.(iBlk==NBLOK2))) &
              write(PrtUni,'(1x,5i3,4f11.3,a)')iBlk,UNIT(iBlk), &
              BLKNO(iBlk),j,BlockPos(j),Capacity(j),x(j),CallersQ, &
              MeanLoad-xAccum,'=UQ'
          end if ! cf. MeanLoad-xAccum reported above with ShowCostAtIter's UnservedQ
          ENERGY(max(BLKNO(iBlk),int(1,2)),UNIT(iBlk))=CallersQ
        end do
      return ! entry MinimizeCost

!       local routines which need not be known by FindOptimalCost's caller


      entry wnai(nLeadingBlanks,iVal) ! write iVal to StdOut without advancing record


        do jWNAI=1,nLeadingBlanks
          call wnaa(' ')
        end do
        if    (iVal<= 9) then
          write(PrtUni,'(i1\)')iVal
        elseif(iVal<=99) then
          write(PrtUni,'(i2\)')iVal
        else
          write(PrtUni,'(i3\)')iVal
        end if
      return


      entry wnaf(Fmt,fVal) ! write fVal to StdOut without advancing record


        write(PrtUni,Fmt)fVal
      return


      entry FillPascalTriangle(Order) ! Order<=LimCumulants
!       routine to generate the binomial coefficients:  combinatorial j,k
!       BinCoeff(j,k) is the combinatorial of j objects taken k at a time, k<=j
!       integer*2 Order,jm1

        jm1=0
        do j=0,Order
          BinCoeff(j,0)=1.0
          BinCoeff(j,j)=1.0
          do k=1,jm1 ! use the Pascal-triangle identity
            BinCoeff(j,k)=BinCoeff(jm1,k-1)+BinCoeff(jm1,k)
          end do
          jm1=j
        end do
      return


      entry MapNCMomentsToCumulants(jMN)
!       transform the lowest-order moments into distribution cumulants

!       The characteristic function is described in Theory of Probability
!       by Bernard Harris.  If the random variable is denoted by x, its
!       density function by f(x), an arbitrary function of x by g(x),
!       the characteristic function of g(x) by P(t),
!       and the square-root of minus one by i, then P(t) is defined
!       by the expected value (over the distribution of x) of EXP(i*t*g(x)).
!       Let the natural logarithm of P(t) be denoted by C(t), the nth
!       derivative at s of C(t) with respect to t by (t,s,D**n)oC(t), and the
!       nth-order cumulant by K(n,x).  Then K(n,x) is defined by K(n,x) =
!       (i**-n)*(t,0,D**n)oC(t).  These cumulants are simply the numerator
!       coefficients in the Maclaurin series expansion of C(t) in powers of the
!       complex variable (i*t).  Hence they characterize C(t) whenever the series
!       converges.
!
!       The utility of the characteristic function is clear in the commonly
!       encountered problem, called convolution, of describing the distribution of
!       the sum (or difference) of two random variables.  Any weighted
!       sum of random variables can be cast as a repeated summation of two
!       signed variables, one being the accumulator and the other the
!       increment.  Let z be the sum variable, and x and y be the signed
!       summand variables.  By the definition of the characteristic function
!       above, if g(z) = z = x+y then the resultant P(t) is the expected value
!       of EXP(i*t*(x+y)).  Since the exponential of the sum is the product
!       of the individual exponential terms, and since expectation may be
!       considered a linear operator, the resultant P(t) is the product of
!       the individual characteristic functions for x and y.
!       This provides the opening advantage:  the result has been stated as
!       an arithmetic manipulation of two (hypothetically) known functions.
!       Performing the multiplication using logarithms, we note that the
!       logarithm C(t) of the sum variable's characteristic function P(t)
!       is simply the sum of the logarithms of the summands' characteristic
!       functions.  Each of the three functions' logarithms may be expressed
!       as a Maclaurin series expansion whose coefficients are given by the
!       appropriate set of cumulants.  The logarithms of the characteristic
!       functions of the summands may then by added term-by-term in the
!       Maclaurin series.  The concluding advantage is clear:
!       the nth-order cumulant of z is simply the sum of the nth-order
!       cumulants of x and y.  Thus, the distribution of the sum variable z
!       is implicitly characterized by a set of cumulants, each of which is
!       a simple sum of the summands' like-order cumulants.  In practice, the
!       shape and numerical value of the characteristic function P(t) and its
!       logarithmic equivalent C(t) are of no interest here; only the cumulants
!       are required to characterize the density and cumulative distribution
!       functions, using the Edgeworth expansion routine.
!
!       It remains to relate the cumulants of a variable's distribution to a set
!       of measurable quantities, i.e., to the variable's statistics.  Let the
!       expected value of x**n (i.e., the nth-order non-central moment of x)
!       be denoted by M(n,x).  Next, consider the identity which results from
!       expressing the characteristic function as a Maclaurin series:
!       (t,0,D**n)oP(t) = (i**n)*(the expected value of ((g(x))**n)).  Thus, for
!       g(x) = x we have an expression similar in form to that for cumulants
!       given above:  M(n,x) = (i**-n)*(t,0,D**n)oP(t).  These non-central moments
!       are the numerator coefficients in the Maclaurin series expansion of P(t) in
!       powers of the complex variable (i*t).  Just as the set of K(n,x) serves to
!       characterize the C(t) function, these M(n,x) characterize the P(t) function.
!
!       Next, we inductively develop the relationship between K(n,x) and M(n,x).
!       Using the rule for differentiating the logarithm of a function, we
!       have that (t,s,D**1)oC(t) = ((t,s,D**1)oP(t))/P(t).  Clearing the
!       denominator yields (t,s,D**1)oP(t) = (P(t))*(t,s,D**1)oC(t).
!       Differentiating a second time yields (t,s,D**2)oP(t) = (P(t))*(t,s,D**2)
!       oC(t) + ((t,s,D**1)oP(t))*(t,s,D**1)oC(t).  A third time yields
!       (t,s,D**3)oP(t) = (P(t))*(t,s,D**3)oC(t) + 2*((t,s,D**1)oP(t))*(t,s,D**2)
!       oC(t) + ((t,s,D**2)oP(t))*(t,s,D**1)oC(t).   Using notation without
!       the understood 't' and 's' parameters makes the pattern more evident;
!       the above three equations are briefly restated for clarity.
!       (D**1)oP = P*(D**1)oC.
!       (D**2)oP = P*(D**2)oC +   ((D**1)oP)*(D**1)oC.
!       (D**3)oP = P*(D**3)oC + 2*((D**1)oP)*(D**2)oC + ((D**2)oP)*(D**1)oC.
!       Using the notation that B(n,j) is the combinatorial (or binomial
!       coefficient) of n items taken j at a time, and that S(i,j,k,a) is
!       the sum of items a indexed by j over the range from i through k
!       inclusive, we generalize the above expressions to the (n+1)th-order
!       derivative.  (D**n+1)oP = S(0,j,n,B(n,j)*((D**n-j)oP)*(D**j+1)oC).
!       Note that in both the general and the specific cases the orders of
!       the derivatives on the right-hand side sum to the order on the left.
!       Solving for the last term in the summation yields
!       ((D**n-n)oP)*(D**n+1)oC = (D**n+1)oP - S(0,j,n-1,B(n,j)*((D**n-j)oP)*
!       (D**j+1)oC.  Since (D**0)oP = P, dividing through by P yields
!       (D**n+1)oC = ((D**n+1)oP - S(0,j,n-1,B(n,j)*((D**n-j)oP)*(D**j+1)oC)/P.
!       Next we divide through by (i**(n+1)) and evaluate at s = 0, understanding
!       that P(0) is unity.  This yields the formula for the (n+1)th cumulant:
!       K(n+1,x) = M(n+1,x) - S(0,j,n-1,B(n,j)*M(n-j,x)*K(j+1,x)).  Thus a given
!       order of cumulant is just that same order non-central moment less a
!       sum of terms in lower-order non-central moments and cumulants.  While
!       notationally messy, the above formula is easily programmed in a few
!       concise statements.  Its development was largely dependent on the
!       mathematical insight provided by ODOE Senior Analyst Tom Wilson.



        do k=1,nKappa
          VarCumul(k,jMN)=NCMoment(k)
          if(k>1) then
            n=k-1
            do i=0,n-1
              VarCumul(k,jMN)=VarCumul(k,jMN)- &
                BinCoeff(n,i)*VarCumul(i+1,jMN)*NCMoment(n-i)
            end do
          end if
        end do

        if(PrtDetail>2) then
          if(VarCumul(2,jMN)<=0.0) &
                                call HaltWith(0_2,jMN,'variance at var')
          SigMN=sqrt(VarCumul(2,jMN))
          write(PrtUni,'(1x,a,i3,2f11.4,2e13.6\)')'Kap',jMN, & !  \ works only @nKappa=4
            VarCumul(1,jMN),SigMN,(VarCumul(k,jMN),k=3,nKappa)
          if(nKappa>2) write(PrtUni,'(1x,a,2f9.3)')'Gam', &
           (VarCumul(k,jMN)/SigMN**k,k=3,nKappa) ! these are raw, not normalized Gammas
        end if
      return ! entry MapNCMomentsToCumulants


      entry SaveEndPoints(iPt,iBM,ModXBeg,ModXEnd,ModOrder)
!       var names necessarily differ from caller's


        nAbnormalModes=nAbnormalModes+1
        ModXBeg(nAbnormalModes)=iBM
        ModXEnd(nAbnormalModes)=iPt
        ModOrder(nAbnormalModes)=nAbnormalModes ! before sorting
        if(PrtDetail>2) write(PrtUni,'(1x,i2,2i3)') &
          iBM,iPt,nAbnormalModes
        iBM=iPt
      return


      entry GetModeCumulants(jMode,ModelOrder,iPtInf,iPtSup,iBgMode, &
        ModeXBg,ModeXEd,ModeOrd,DistrMean,Bracketing)
!       var names necessarily differ from caller's


        DistrMean=0.0
        do iPtGMC=iPtInf,iPtSup ! extract the mean of abscissas
          x2=EqLoad(iPtGMC)
          y2=EqLDur(iPtGMC)
          if(iPtGMC>iPtInf) DistrMean=DistrMean+(x2+x1)*(y1-y2) ! see rationale below
          x1=x2
          y1=y2
        end do
        DistrMean=0.5*DistrMean !/Duration
        if(PrtDetail>1) write(PrtUni,'(1x,i2,3i3,f14.6,f9.6,a)') jMode, &
          iBgMode,iPtInf,iPtSup,DistrMean,DistrWeight(jMode),' GMC'
      ! CtMoment=0.0 ! call cinitd(CtMoment,I4_nKappa,0.0)
        call initd1(CtMoment,nKappa,0.0)
        do iPtGMC=iPtInf,iPtSup ! obtain central moments
          x2=EqLoad(iPtGMC)-DistrMean
          y2=EqLDur(iPtGMC) !/Duration; ! DeltaY is a discrete probability on [0,1]
          if(iPtGMC>iPtInf) then
          ! if(y1<y2) y2=y1 ! adjust y2
            DeltaY=y1-y2 ! non-negative
          ! if(x2<=x1) x2=x1*1.0001 ! adjust x2
            if(Bracketing) then
              DeltaX=x2-x1 ! positive
              Slope=DeltaY/DeltaX
            ! if(iPtGMC>iPtInf+1) Slop2=(Slope-SloPr)/DeltaX else Slop2=0.0
              if(iPtGMC>iPtInf+1) then ! ignore noise below the 1% level
                if(abs(Slope-SloPr)>0.01*SloPr) &
                  Slop2=(Slope-SloPr)/DeltaX
              ! else retain the prior value of Slop2
              else
                Slop2=0.0
              end if
              if((Sl2Pr<0.0).and.(Slop2>0.0)) call SaveEndPoints( &
                iPtGMC-One2,iBgMode,ModeXBg,ModeXEd,ModeOrd)
              if(PrtDetail>2) write(PrtUni, &
                '(1x,i2,f10.3,f9.6,f10.6,1x,e9.2\)') &
                iPtGMC,x2,y2,Slope,Slop2
            else
              if(PrtDetail>3) write(PrtUni,'(1x,i2,f10.3,f9.6\)') &
                iPtGMC,x2,y2
            end if
!           do k=2,nKappa+1 CtMoment[k-1]=CtMoment[k-1]+Slope*(x2^k-x1^k)/real(k)
!           do k=1,nKappa   CtMoment[k  ]=CtMoment[k  ]+Slope*(x2^k+1-x1^k+1)/real(k+1)
!           The above formulation, involving differences of powers of small
!           numbers, is prone to numerical error; the error may be avoided
!           by taking advantage of the identity
!           a^n - b^n = (a-b) * (a^(n-1) + b*a^(n-2) + ... + a*b^(n-2) + b^(n-1)),
!           the leading factor of which is cancelled by the denominator of Slope:
            do kGMC=1,ModelOrder
              TermsOfOrderK=0.0
              do iGMC=0,kGMC

                TermsOfOrderK=TermsOfOrderK+ &
                  IntegralPower(x1,iGMC)* &
                  IntegralPower(x2,(kGMC-iGMC))
              end do
              CtMoment(kGMC)=CtMoment(kGMC)+DeltaY*TermsOfOrderK/ &
                real(kGMC+1)
              if(PrtDetail>3) write(PrtUni,'(1x,e9.2\)') &
                DeltaY*TermsOfOrderK/real(kGMC+1)
            end do
            if((PrtDetail>3).or.((PrtDetail>2).and.Bracketing)) &
              write(PrtUni,Fmt1a)
          else
            if(PrtDetail>3) write(PrtUni,'(1x,i2,f10.3,f9.6)') &
              iPtGMC,x2,y2
          end if
          x1=x2
          y1=y2
          if(Bracketing) then
            SloPr=Slope
            Sl2Pr=Slop2
          end if
        end do
        if(.not.Bracketing) then
        ! WHAT SHOULD THIS BE?  AGT replies:  line below [cf. p.28 of Migrating to Fortran 90]
        ! Ld0Cumul(:,jMode)=0.0 ! call cinitd(Ld0Cumul(1,jMode),I4_nKappa,0.0)
          call initd1(Ld0Cumul(1,jMode),nKappa,0.0)
          Ld0Cumul(1,jMode)=DistrMean
          Ld0Cumul(2,jMode)=CtMoment(2)
          if(ModelOrder>2) then ! define the higher-order cumulants
            Ld0Cumul(3,jMode)=CtMoment(3)
            Ld0Cumul(4,jMode)=CtMoment(4)-3.0*CtMoment(2)*CtMoment(2)
          end if
        end if
        if(PrtDetail>1) then ! display weight and cumulants for this mode
          write(PrtUni,'(1x,i2,f9.6,2f14.6\)')jMode,DistrWeight(jMode), &
            DistrMean,CtMoment(2)
          write(PrtUni,'(1x,e13.6\)') &
            (Ld0Cumul(kGMC,jMode),kGMC=3,ModelOrder)
          write(PrtUni,Fmt1a)'mode stats'
        end if
      return ! entry GetModeCumulants


      entry GetLoadCumulants


!       extract abnormal modes from EqLDn0
        iBegMode=1
        call GetModeCumulants(Zer2,Two2,One2,nPoints,iBegMode,ModeXBeg, &
          ModeXEnd,ModeOrder,MeanLoad,True_Byte) ! use a 2nd-order (Normal) model
        SDLoad=sqrt(CtMoment(2))
        if(nPoints>iBegMode) call SaveEndPoints(nPoints,iBegMode, &
          ModeXBeg,ModeXEnd,ModeOrder)
        do iGLC=1,nAbnormalModes ! weight modes before ranking
          do ModeTrimmed=0,1
            iBegMode=ModeXBeg(iGLC)
            iEndMode=ModeXEnd(iGLC)
            x1=EqLoad(iBegMode)
            x2=EqLoad(iEndMode)
!           rank modes by their excess of y decrease beyond Normal, as if each
!           mode acted alone in reducing the weight of the Normal residual;
!           note that the global mean and sigma above roughly describe the
!           current distribution except in the vicinity of a near-impulsive
!           density mode, and hence are used to splice at the endpoints; let
!           the mode's raw decrease in y be designated yDm,
!           the mode's Normal decr. in y be designated yDn, [Normal weight<1]
!           the mode's excess decr. in y be designated yDe,
!           the mode's residual decr.  y be designated yDr,
!           the mode's raw increase in z be designated zDm, and
!           the mode's Normal density at endpoints be fz1 and fz2; then
!           yDn=zDm*(fz1+fz2)/2 [mean density times change in std. Normal deviates]
!           yDn=Fz1-Fz2         [change in standard Normal cum. function on z1,z2]
!           yDr=yDm-yDe         [residual=raw-excess deducted]
!           yDr=(1-yDe)*yDn     [residual=(original wt.-wt. reduction)*(unity yDn)]
!              =(1-yDm+yDr)*yDn
!              =(1-yDm)*yDn/(1-yDn)
!           yDe=yDm-[(1-yDm)*yDn/(1-yDn)]
!              =[yDm*(1-yDn)-(1-yDm)*yDn]/(1-yDn)
!              =(yDm-yDn)/(1-yDn)
            yDecrement=EqLDn0(iBegMode)-EqLDn0(iEndMode) ! raw decrease
            yDecNormal=NormalOrd(x1,MeanLoad,SDLoad)- &
                       NormalOrd(x2,MeanLoad,SDLoad) ! as if Normal weight=1
            ResidSlope=yDecNormal/(x2-x1)
            OneTruncated=.true. ! merely to gain entry
            do while((ModeTrimmed==0).and.OneTruncated)
!           remove those points from the modal domain's beginning or end whose
!           forward or reverse slope, respectively, is exceeded by the slope of
!           the residual Normal distribution; this is necessary to keep the net
!           difference slope positive (for valid cumulative probability functions)
              OneTruncated=.false.
              do while((iBegMode+1<iEndMode).and.(EqLDur(iBegMode+1)> &
                EqLDur(iBegMode)-ResidSlope*(EqLoad(iBegMode+1)- &
                EqLoad(iBegMode))))
              ! truncate initial near-normal area
                OneTruncated=.true.
                if(PrtDetail>2) &
                  write(PrtUni,'(1x,i2,2i3,2f10.3,3f9.6,a)') &
                  iBegMode,iEndMode,iGLC,x1,x2,ResidSlope, &
                  yDecrement,yDecNormal,' beg'
                iBegMode=iBegMode+1
                x1=EqLoad(iBegMode)
                yDecrement=EqLDn0(iBegMode)-EqLDn0(iEndMode) ! raw decrease
                yDecNormal=NormalOrd(x1,MeanLoad,SDLoad)- &
                           NormalOrd(x2,MeanLoad,SDLoad) ! as if Normal weight=1
                ResidSlope=yDecNormal/(x2-x1)
              end do
              do while((iEndMode-1>iBegMode).and.(EqLDur(iEndMode-1)< &
                EqLDur(iEndMode)+ResidSlope*(EqLoad(iEndMode)- &
                EqLoad(iEndMode-1))))
              ! truncate final near-normal area
                OneTruncated=.true.
                if(PrtDetail>2) &
                  write(PrtUni,'(1x,i2,2i3,2f10.3,3f9.6,a)') &
                  iBegMode,iEndMode,iGLC,x1,x2,ResidSlope, &
                  yDecrement,yDecNormal,' end'
                iEndMode=iEndMode-1
                x2=EqLoad(iEndMode)
                yDecrement=EqLDn0(iBegMode)-EqLDn0(iEndMode) ! raw decrease
                yDecNormal=NormalOrd(x1,MeanLoad,SDLoad)- &
                           NormalOrd(x2,MeanLoad,SDLoad) ! as if Normal weight=1
                ResidSlope=yDecNormal/(x2-x1)
              end do
              ModeXBeg(iGLC)=iBegMode
              ModeXEnd(iGLC)=iEndMode
            end do
          end do
          ModeYDecr(iGLC)=(yDecrement-yDecNormal)/(1.0-yDecNormal) ! excess
          if(PrtDetail>2) write(PrtUni,'(1x,i2,2i3,2f10.3,4f9.6)') &
            iBegMode,iEndMode,iGLC,x1,x2,ResidSlope, &
            yDecrement,yDecNormal,ModeYDecr(iGLC)
        end do
        call Sort(nAbnormalModes,ModeOrder,ModeYDecr,.false.)
        if(nAbnormalModes>LimAbnModes) nAbnormalModes=LimAbnModes
        SumWeights=0.0
        iMode=0
        do while(iMode<nAbnormalModes) ! extract modes from EqLDn0
          iMode=iMode+1
          if(ModeYDecr(ModeOrder(iMode))<0.02) then ! ModeYDecr must be positive
            nAbnormalModes=iMode-1
            exit
          end if
          j=ModeOrder(iMode)
          iBegMode=ModeXBeg(j)
          iEndMode=ModeXEnd(j)
          yDecrement=ModeYDecr(j)
          y1=EqLDn0(iBegMode)
          x1=EqLoad(iBegMode)
          x2=EqLoad(iEndMode)
          DistrWeight(iMode)=yDecrement
          SumWeights=SumWeights+yDecrement
        ! estimate Normal slope as constant across this mode's domain
          yDecNormal=NormalOrd(x1,MeanLoad,SDLoad)- &
                     NormalOrd(x2,MeanLoad,SDLoad) ! as if Normal weight=1
          ResidSlope=yDecNormal*(1.0-yDecrement)/(x2-x1) ! EqLDur is on [0,1]
          if(PrtDetail>2) &
            write(PrtUni,'(1x,i2,3i3,f10.3,4f9.6,f11.8,a)') &
            iMode,j,iBegMode,iEndMode,(x2-x1), &
            NormalOrd(x1,MeanLoad,SDLoad), &
            NormalOrd(x2,MeanLoad,SDLoad), &
            yDecNormal,yDecrement,ResidSlope,' ResidSl'
          do iPtGLC=iBegMode,nPoints ! modify EqLDn0
            if(iPtGLC<=iEndMode) then
              y2=EqLDn0(iPtGLC) ! residual is linear over this mode's domain
              if(PrtDetail>2) write(PrtUni,'(1x,i2,f10.3,f9.6\)') &
                iPtGLC,EqLoad(iPtGLC),y2
              EqLDn0(iPtGLC)=y1-ResidSlope*(EqLoad(iPtGLC)-x1)
              EqLDur(iPtGLC)=1.0+(y2-EqLDn0(iPtGLC))/yDecrement ! normalize range
              if(PrtDetail>2) write(PrtUni,'(2f9.6)') &
                EqLDn0(iPtGLC),EqLDur(iPtGLC)
            else
              EqLDn0(iPtGLC)=EqLDn0(iPtGLC)+yDecrement
            end if
          ! other EqLDur values are not examined below, need not be assigned
          end do
          call GetModeCumulants(iMode,Two2,iBegMode,iEndMode,iBegMode, &
            ModeXBeg,ModeXEnd,ModeOrder,LocalMean,FalseByte) ! use a 2nd-order (Normal) model
        end do
        yDecrement=1.0-SumWeights ! range of residual EqLDn0
        DistrWeight(0)=yDecrement
        if(nAbnormalModes>0) then
          do iPtGLC=1,nPoints ! normalize range of EqLDur
            EqLDur(iPtGLC)=EqLDn0(iPtGLC)/yDecrement
            if(PrtDetail>2) write(PrtUni,'(1x,i2,f12.4,2f9.6)') &
              iPtGLC,EqLoad(iPtGLC),EqLDn0(iPtGLC),EqLDur(iPtGLC)
          end do
          if(PrtDetail>1) write(PrtUni,'(1x,5f9.6,a)')yDecrement, &
            EqLDn0(1),EqLDn0(nPoints), &
            EqLDur(1),EqLDur(nPoints),' after normalization'
        end if
        call GetModeCumulants(Zer2,nKappa,One2,nPoints,iBegMode, &
          ModeXBeg,ModeXEnd,ModeOrder,LocalMean,FalseByte) ! use an nKappa-order model for the residual
      return ! entry GetLoadCumulants


      entry ChkLoadModel(MonotCLM) ! test if model's ordinates decline monotonically


        LoadProbed=BaseLoad
        LoadStepSize=0.01*(EqLoad(nPoints)-LoadProbed)
        call FindTailArea(LoadProbed)
        OrigUQ=TailUQ
        PriorAvgOrd=1.0
        MonotCLM=.true. ! default retained if all test-ordinates meet criteria
        do j=1,100
          LoadProbed=LoadProbed+LoadStepSize
          call FindTailArea(LoadProbed)
          AvgOrdinate=(OrigUQ-TailUQ)/LoadStepSize

          if( (AvgOrdinate<-0.01).or. &
             ((AvgOrdinate> 0.01).and. &
              (AvgOrdinate>1.005*PriorAvgOrd))) then
            if(PrtDetail>1) write(PrtUni,'(1x,i3,2f10.6,a,i1)') &
              j,PriorAvgOrd,AvgOrdinate, &
              ' load non-monotonic at nKappa=',nKappa
            MonotCLM=.false. ! criteria above depend upon loop's step-size
          end if
          OrigUQ=TailUQ
          PriorAvgOrd=AvgOrdinate
        end do
      return ! entry ChkLoadModel


      entry GetPlantCumulants


        do j=1,nVar
          PrOutage=1.0-EquivAvail(j)
          if(PrOutage==0.0) then ! all cumulants are zero
!           call cinitd(VarCumul(1,j),I4_nKappa,0.0) DOES I4_nKappa DECREASE AS J INCRESE
!              SO THAT YOU DON'T GO BEYOND THE SECOND DIMENSION?  AGT replies 'no,
!              but cinitd works as if the array were 1-dimensional; leftmost index
!              varies most rapidly in Fortran'
            call initd1(VarCumul(1,j),nKappa,0.0)
          else ! care for interdependency of two-block units is taken later
            if(BlockPos(j)<0) then ! infer that multiple identical blocks were merged
            ! Harris, Theory of Probability, Addison-Wesley 1966, p. 111, gives
            ! the (non-central) moment-generating function for a binomial
            ! distribution with parameters n,p as G(t)=(1-p+p*e^(i*t))^n, and
            ! the kth-order moment as (i^-k)(d^k)G(t)/(dt^k) evaluated at t=0;
            ! simplify notation by letting z=e^(i*t), b=1-p+p*z, and (Dkof)x=
            ! (d^k)x(t)/(dt^k), whence dz/dt=i*z, db/dt=i*p*z, and at t=0,
            ! z(0)=e^0=1 and b(0)=1; the first 4 derivatives of G are thus
            ! (D1of)G=(D1of)(b^n)=n*b^(n-1)*i*p*z=i*[n*b^(n-1)*p*z]
            ! (D2of)G=(D1of)(D1of)G
            !          =i*[(n-1)*n*b^(n-2)*i*p^2*z^2
            !          +         n*b^(n-1)*i*p  *z]
            !          =(i^2)*[(n-1)*n*b^(n-2)*p^2*z^2
            !          +             n*b^(n-1)*p  *z]
            ! (D3of)G=(D1of)(D2of)G
            !          =(i^2)*[(n-2)*(n-1)*n*b^(n-3)*i*p^3*z^3
            !          +           2*(n-1)*n*b^(n-2)*i*p^2*z^2
            !          +             (n-1)*n*b^(n-2)*i*p^2*z^2
            !          +                   n*b^(n-1)*i*p  *z]
            !          =(i^3)*[(n-2)*(n-1)*n*b^(n-3)*p^3*z^3
            !          +           3*(n-1)*n*b^(n-2)*p^2*z^2
            !          +                   n*b^(n-1)*p  *z]
            ! (D4of)G=(D1of)(D3of)G
            !          =(i^3)*[(n-3)*(n-2)*(n-1)*n*b^(n-4)*i*p^4*z^4
            !          +           3*(n-2)*(n-1)*n*b^(n-3)*i*p^3*z^3
            !          +           3*(n-2)*(n-1)*n*b^(n-3)*i*p^3*z^3
            !          +               2*3*(n-1)*n*b^(n-3)*i*p^2*z^2
            !          +                   (n-1)*n*b^(n-2)*i*p^2*z^2
            !          +                         n*b^(n-1)*i*p  *z]
            !          =(i^4)*[(n-3)*(n-2)*(n-1)*n*b^(n-4)*p^4*z^4
            !          +           6*(n-2)*(n-1)*n*b^(n-3)*p^3*z^3
            !          +                 7*(n-1)*n*b^(n-2)*p^2*z^2
            !          +                         n*b^(n-1)*p  *z]
            ! dividing the above by i^k and evaluating at t=0 yields moments:
            ! NCMoment(1)=(i^-1)(D1of)G, at t=0
            !            =n*b^(n-1)*p*z, at t=0
            !            =n*p;
            ! NCMoment(2)=(i^-2)*(D2of)G, at t=0
            !            =(n-1)*n*b^(n-2)*p^2*z^2
            !            +      n*b^(n-1)*p  *z, at t=0
            !            =(n-1)*n*p^2+n*p;
            ! NCMoment(3)=(i^-3)(D3of)G, at t=0
            !            =(n-2)*(n-1)*n*b^(n-3)*p^3*z^3
            !            +    3*(n-1)*n*b^(n-2)*p^2*z^2
            !            +            n*b^(n-1)*p  *z, at t=0
            !            =(n-2)*(n-1)*n*p^3+3*(n-1)*n*p^2+n*p
            ! NCMoment(4)=(i^-4)(D4of)G, at t=0
            !            =(n-3)*(n-2)*(n-1)*n*b^(n-4)*p^4*z^4
            !            +    6*(n-2)*(n-1)*n*b^(n-3)*p^3*z^3
            !            +          7*(n-1)*n*b^(n-2)*p^2*z^2
            !            +                  n*b^(n-1)*p  *z, at t=0
            !            =(n-3)*(n-2)*(n-1)*n*p^4
            !            +    6*(n-2)*(n-1)*n*p^3
            !            +          7*(n-1)*n*p^2+n*p
            ! Before proceeding, we note that Harris (op. cit., page 101) gives
            ! NCMoment(k)=(Sum over m from 1 through k) of (Sterling number of the
            ! second kind (m,k))*(mth factorial moment), and on page 104 gives
            ! mth factorial moment of binomial distribution with parameters n,p
            ! as (p^m)*factorial(n)/factorial(n-m); denoting such Sterling numbers
            ! as vmk, pp. 25-26 (op. cit.) gives v1k=vkk=1, and page 43 (op. cit.)
            ! gives vm(k+1)=v(m-1)k+m*vmk, with vmk=0 for m>k or m<1, whence
            ! v11=1
            ! v12=v22=1
            ! v13=v33=1, v23=v12+2*v22=1+2=3
            ! v14=v44=1, v24=v13+2*v23=1+2*3=7, v34=v23+3*v33=3+3*1=6
            ! NCMoment(1)=v11*(p^1)*n=p*n
            ! NCMoment(2)=v12*(p^1)*n+v22*(p^2)*n*(n-1)=p*n+(p^2)*n*(n-1)
            ! NCMoment(3)=v13*(p^1)*n+v23*(p^2)*n*(n-1)+v33*(p^3)*n*(n-1)*(n-2)
            !            =p*n+3*(p^2)*n*(n-1)+(p^3)*n*(n-1)*(n-2)
            ! NCMoment(4)=v14*(p^1)*n+v24*(p^2)*n*(n-1)+v34*(p^3)*n*(n-1)*(n-2)
            !            +v44*(p^4)*n*(n-1)*(n-2)*(n-3)
            !            =p*n+7*(p^2)*n*(n-1)+6*(p^3)*n*(n-1)*(n-2)
            !            +(p^4)*n*(n-1)*(n-2)*(n-3)
            ! Since these results agree with those derived from the primitive
            ! moment-generating function above, we proceed with confidence using
            ! the formula involving Sterling numbers:
              nIdent=-BlockPos(j)
              NCMoment(1)=PrOutage*Capacity(j) ! ÷ n*p in the above notation
              CapOutage=Capacity(j)/real(nIdent)
              Sterling(1,1)=1
              do k=2,nKappa
                Sterling(1,k)=1
                Sterling(k,k)=1
                NCMoment(k)=PrOutage*real(nIdent) ! =Sterling(1,k)*(PrOutage**1)*nIdent
                km1=k-1
                nmFactor=real(nIdent*(nIdent-1))
                do m=2,k
                  if(m<k) Sterling(m,k)= &
                          Sterling(m-1,km1)+m*Sterling(m,km1)
                  NCMoment(k)=NCMoment(k)+ &
                    real(Sterling(m,k))*nmFactor*(PrOutage**m)
                  if(PrtDetail>2) write(PrtUni,'(1x,5i5,2f12.3,2f12.6)') &
                    j,nIdent,k,m,Sterling(m,k),nmFactor,Capacity(j), &
                    PrOutage,NCMoment(k)
                  nmFactor=nmFactor*real(nIdent-m)
                end do
                NCMoment(k)=NCMoment(k)*(CapOutage**k) ! with dimension of order k
              end do
            else ! form the non-central moments of capacity on outage
              CapOutage=Capacity(j)
              NCMoment(1)=PrOutage
              km1=1
              do k=1,nKappa
                NCMoment(k)=NCMoment(km1)*CapOutage
                km1=k
              end do
            end if
            if(PrtDetail>3) write(PrtUni,'(1x,2i3,f7.2,6e11.4)') &
              j,BlockPos(j),CapOutage,(NCMoment(k),k=1,nKappa)
            call MapNCMomentsToCumulants(j)
          end if

        end do
      return ! entry GetPlantCumulants


      entry FindTailArea(xLod) ! ,yPrb) if ordinates are of interest

!     This routine uses an Edgeworth series to represent the values of a
!     cumulative probability distribution.  The Edgeworth series is an
!     expansion in terms of the standard Normal distribution and its
!     derivatives.  The use of such a series was discussed in a paper by
!     Tennessee Valley Authority (TVA) authors Babb, Bayless, Jenkins, and
!     Stremel, entitled "Production Costing Using the Cumulant Method of
!     Representing the Equivalent Load Curve", and published in the 1979
!     Journal of the IEEE Power Engineering Society.  The TVA authors note
!     the analogy between the normalized (to unity-duration) equivalent-load
!     duration curve and the complement of a cumulative
!     probability-distribution function.  Instead of the fourth-order
!     Gram-Charlier series used in the IEEE paper, this routine uses the
!     sixth-order Edgeworth series given in paragraph 26.2 of the Handbook
!     of Mathematical Functions, by the National Bureau of Standards (NBS),
!     Department of Commerce.  In practice, the higher-order terms are used
!     only as required; the fourth-order Edgeworth series is identical to
!     the fourth-order Gram-Charlier.
!
!     Let Q(z) designate the distribution's single-tail probability
!     that standardized deviates exceed z,
!     f(z) the standard Normal density at z standard deviates,
!     N(n,z) its nth derivative at z,
!     N(-1,z) the cumulative standard Normal distribution,
!     and Gn the nth-order gamma coefficient of the distribution.
!     Then the sixth-order Edgeworth series for suitably smooth distributions
!     gives the cumulative function in a form analogous to a Maclaurin series:
!     1-Q(z) = N(-1,z)
!      - [N(2,z)*G1/6]
!      + [N(3,z)*G2=G1*G172]
!      - [N(4,z)*G3=G1*G2144 + N(8,z)*G1*G1*G1/1296]
!      + [N(5,z)*G4=(G2*G21152 + G1*G3/720)
!      +  N(9,z)*G1*G1*G2=G1*G1*G1*G131104].
!     As the order increases from 2 (for the strictly Normal) to 6, one group of
!     terms (enclosed within square brackets) is added for each increment in order.
!     Now let R(n,z) be the ratio of N(n,z) to f(z).  Then by complementing both
!     sides of the equation above and factoring out f(z), we obtain
!     Q(z) = 1-N(-1,z) + f(z)*(
!     + [R(2,z)*G1/6]
!     - [R(3,z)*G2=G1*G172]
!     + [R(4,z)*G3=G1*G2144 + R(8,z)*G1*G1*G1/1296]
!     - [R(5,z)*G4=(G2*G21152 + G1*G3/720)
!     +  R(9,z)*G1*G1*G2=G1*G1*G1*G131104]).
!     We view Q(z) as the required load-probability at the specified
!     abscissa z, given the parameters Gn of the distribution.
!
!     The coefficients of the derivative terms (either N(n,z) or R(n,z)) in
!     the series involve products of the standardized gamma coefficients
!     of the distribution.  Let Kn designate the nth-order cumulant, and S
!     the standard deviation of the distribution.  (For a definition of
!     cumulants, see the discussion under MapNCMomentsToCumulants.)  The
!     NBS reference defines gamma for m>0 as Gm = Kn/(S**n), where m = n-2.
!     A strictly Normal distribution, of which all Kn are zero for n>2, has
!     all its gamma coefficients at zero.  The values of gamma thus serve to
!     express the degree to which an arbitrary distribution deviates from a
!     Normal distribution.  In the Edgeworth series, each gamma term of
!     order n appears with n-factorial in the denominator.  To simplify
!     the computations, the denominator is incorporated in the gamma terms
!     as they are derived, in a short loop covering the cumulants used.  Equation
!     26.2.31 of the NBS reference factors the nth-order derivative N(n,z) into
!     the product of the f(z), the nth-order Hermite polynomial, and (-1)**n.
!     We denote by H(n,z) the nth-order Hermite polynomial in z.
!     Since the even powers of minus one are all unity and the odds are all
!     negative unity, the negative signs in the complementary distribution
!     function above are fortuitously cancelled by replacing all R(n,z) by
!     their equivalents, H(n,z)*(-1)**n.
!
!     From Advanced Engineering Mathematics by Erwin Kreyszig, we note the
!     recursion relation among Hermite polynomials:  H(n+1,z) = z*H(n,z)-n*
!     H(n-1,z), where H(0,z) is identically zero and H(1,z) is z.
!     Use of this relation allows rapid evaluation of the higher-order
!     Hermite polynomials at a given value of z by computing only the
!     difference between two products.  By contrast, evaluating the 5th-order
!     polynomial (z**5-10*z**3+15*z) using Horner's rule as (((z**2)-10)*
!     (z**2)+15)*z would take four multiplications and two additions.
!
!     The routine is called with a single abscissa at which the ordinate
!     on the load-duration curve is required.  The abscissa is converted
!     to a standard Normal deviate, and the ordinate is obtained by building
!     up the values of the required Hermite polynomials by recursion, and
!     augmenting the strictly Normal model, 1.-N(-1,z) above, by the sum of
!     these gamma-weighted polynomial terms.
!     The value of the 'strictly Normal model' is obtained
!     by simply calling the CCNFmd function when the deviate z is
!     non-negative; for negative deviates, the complementary cumulative
!     Normal distribution function is the complemented function of
!     the positive deviate of the same magnitude, or 1-CCNFmd(-z).
!     In either case, the value obtained, like the values
!     of the polynomial terms, is expressed as a factor of the Normal
!     density itself.  That density is obtained by a call to the GaussianDen
!     function, but is limited to an arbitrarily small value to preclude
!     later problems with numeric overflow.
!     Thus, all accumulated ordinate terms are multiplied
!     by the value of the density function at the current abscissa.
!
!     In power-system models, the ordinate obtained above is often only
!     of passing interest compared to the area under the duration curve
!     from the load point to infinity.
!     Before the polynomial-indexing loop, the area
!     from the current deviate z to infinity is initialized to the
!     area under a strictly Normal duration curve; this is obtained using
!     the reference's formula 26.2.44 as the density at z less z times the
!     (strictly Normal) ordinate at z.  The density is factored out,
!     leaving 1.-z*CCNFmd(z).
!
!     Since this routine is called to compute tail area, an additional
!     statement is executed within the loop that accumulates the ordinates.
!     This branch accounts for the area (from the current deviate z as the
!     lower limit) under the currently-indexed Hermite polynomial function.
!     Recalling that H(n,z) is just (-1)**n times the nth derivative
!     of the Normal density, and that the integral of an nth derivative
!     yields the (n-1)th derivative, we use only previously computed terms.
!     Each pass through the accumulation loop augments the area by the
!     next-lower-order Hermite polynomial multiplied by the
!     current order's gamma-related coefficient.
!     At the conclusion of the polynomial-indexing loop, the terms
!     accumulated for the integral are multiplied
!     by their common density factor, and scaled by sigma;
!     all the integration had been performed in dimensionless units.
!
!     Note that the integration technique used is analytic, not numeric
!     as was the trapezoidal technique used by the TVA authors.  This
!     should provide a measure of smoothing of the pointwise errors inherent
!     in the Edgeworth expansion.  Being essentially a weighted sum of finite
!     polynomials multiplying the Normal density, the expansion does
!     not have the freedom to follow arbitrarily abrupt transitions in
!     the cumulative distribution function.  For example, whereas a
!     density function composed of a single impulse of unity weight
!     might be modelled suitably by a miniscule-variance expansion,
!     a density function composed of two or more complementary impulses
!     leads to rather large variance and higher-order cumulants, causing
!     large relative errors near the transitions, ringing, overshoot, and
!     an overall smearing of the desired step-transitions in the expanded
!     cumulative function.  The Edgeworth expansion converges rapidly
!     (i.e., using few terms for tolerable accuracy) only for unimodal,
!     reasonably 'smooth' density functions.  Since the equivalent
!     load-duration curves being modelled herewith all contain a large
!     component (native or net loads) of smoothly distributed values,
!     the pointwise errors in the expansion are not expected to be
!     significant or troublesome.

!     routines called:  GaussianDen, CCNFmd
!     y(x) is the Edgeworth expansion of a complementary cumulative
!     probability density function; currently allows use of cumulants
!     EqLCumul up to the sixth order; source is paragraph 26.2.47 of NBS'
!     Handbook of Mathematical Functions.
!     nGamma is the highest order of GammaNorm (dimensionless cumulant).

!       integer*2 jFTA,kFTA
!       character*11 Truncation
!       real*8 MinArea
!       real*4 StdDev,Density,c1,c2,z,xLod,yPrb,ySum,GaussianDen,CCNFmd,
!    +    GammaNorm(2),Factorial(4),Hermite(0:LimiHP),HPcoef(0:LimiHP)
!       equivalence (z,Hermite(1)),(c1,GammaNorm(1)),(c2,GammaNorm(2))
!       data Hermite(0)/1./
!       data Factorial/1.,2.,6.,24./ ! the value of n-factorial

        StdDev=sqrt(EqLCumul(2,0))
!       StdDev=dsqrt(dble(EqLCumul(2,0))) ! need EqLCumul to be real*8 for improvement
!       transform the higher-order cumulants into non-dimensional GammaNorm
!       z=0.5/EqLCumul(2,0)
        do kFTA=3,nKappa
!         GammaNorm(kFTA-2)=EqLCumul(kFTA,0)/StdDev**kFTA
!         above form corresponds to standard definitions of gamma, but we
!         can expedite the computations by incorporating the i-factorials
!         that appear in the denominators of the HPcoef's below; note that
!         GammaNorm(kFTA) incorporates factorial(kFTA+2) in its denominator
!         z=z/(real(kFTA)*StdDev) ! perhaps slower than the one-line assignment below
!         GammaNorm(kFTA-2)=EqLCumul(kFTA,0)*z
          GammaNorm(kFTA-2)=EqLCumul(kFTA,0)/ &
            (Factorial(kFTA)*StdDev**kFTA)
        end do
!       relate the coefficients HPcoef of the Hermite polynomials to
!       the GammaNorm parameters of the distribution; for jFTA<2, HPcoef(jFTA) are
!       implicitly zero and need not be covered in the jFTA-loops below.
        if(nGamma>=1) then
        ! HPcoef=0.0 ! call cinitd(HPcoef,1+I4_SupiHP,0.0)
          call initd0(HPcoef,int(1+SupiHP,2),0.0)
!         HPcoef(2)=g1/6.
          HPcoef(2)=c1
!                6=3!
          if(nGamma>=2) then
!           HPcoef(3)=g2/24.
            HPcoef(3)=c2
!                 24=4!
!           HPcoef(5)=g1*g1/72.
            HPcoef(5)=c1*c1/Factorial(2)
!                 72=2*6*6=2!*3!*3!
          end if
        end if
        z=(xLod-EqLCumul(1,0))/StdDev
        Density=GaussianDen(z)
!       initialize y to the complementary cumulative Normal function at z,
!       as a multiple of the density of the Normal function at z
        if(z<0.0) then
          yPrb=1./Density-CCNFmd(-z)
        else
          yPrb=           CCNFmd( z)
        end if
        TailUQ=1.-z*yPrb
        do jFTA=2,SupiHP
        ! generate the Hermite polynomial values by recursion; note that
        ! here Hermite(jFTA) is the Hermite polynomial of order jFTA:
        ! Hermite(0)=1, Hermite(1)=z, Hermite(2)=z**2-1, Hermite(3)=z**3-3*z...
        ! Hermite(jFTA+1)=z*Hermite(jFTA)-real(jFTA  )*Hermite(jFTA-1)
          Hermite(jFTA)=z*Hermite(jFTA-1)-real(jFTA-1)*Hermite(jFTA-2) ! needed for yPrb
        ! if(jFTA<SupiHP) Hermite(jFTA)=z*Hermite(jFTA-1)-real(jFTA-1)*Hermite(jFTA-2) ! suffices for TailUQ
        ! augment y by the contribution of each higher-order term in
        ! the Edgeworth expansion (factored by the density function):
          if(PrtDetail>2) yPrb=yPrb+HPcoef(jFTA)*Hermite(jFTA) ! ordinate is of interest
        ! return also the integral under the y(x) curve from x1 to x2;
        ! each jFTA term of y above is the product of a coefficient
        ! HPcoef and a (jFTA-1)th-order Hermite polynomial; using the fact
        ! that the jFTAth-order Hermite polynomial times the density equals
        ! (-1)**jFTA times the jFTAth derivative of the Normal density, one
        ! can proceed to integrate y term-by-term:  each HPcoef in
        ! the ordinate's y is mapped into the negative of its one-
        ! lower-order HPcoef in the integration's TailUQ.
        ! by NBS formula 26.2.44, the integral from a to infinity of
        ! CCNFmd*density is (density-z*CCNFmd*density), evaluated at a;
        ! factoring out density yields density(za)*(1-za*CCNFmd(za))
          TailUQ=TailUQ+HPcoef(jFTA)*Hermite(jFTA-1)
        end do
!       y returns the curve ordinate at the abscissa x
        ySum=yPrb*Density*DistrWeight(0) ! only if ordinate is of interest
!       add dimension to area, due to dx = sigma*dz
        TailUQ=TailUQ*Density*StdDev
        if(PrtDetail>2) Truncation=' '
        if(FixingOvershoots) then
          if(TailUQ<0.0) then ! check for underestimation near undershoot (yPrb<0)
            if(PrtDetail>2) write(Truncation,'(a,f10.8)')'~',-TailUQ
            TailUQ=0.0
          else ! check for underestimation of TailUQ due to overshoot (yPrb>1 @ x<xLod)
            MinArea=EqLCumul(1,0)-xLod ! tail area from 0 to infinity equals the mean
            if(TailUQ<MinArea) then ! area is no less than if prior yPrb were always 1
              if(PrtDetail>2) write(Truncation,'(a,f10.7)') &
                '-',MinArea-TailUQ
              TailUQ=MinArea
            end if
          end if
        end if
        TailUQ=TailUQ*DistrWeight(0)
        if(PrtDetail>2) write(PrtUni,'(1x,a,2f11.4,f10.3,f10.6\)') &
          'FTA',EqLCumul(1,0),StdDev,xLod,z
        do jFTA=1,nAbnormalModes ! use a Normal approximation for each component
        ! if(PrtDetail>2) write(PrtUni,'(/1x,i3,2e14.6,a)')jFTA,EqLCumul(1,jFTA),EqLCumul(2,jFTA),' abnormal'
        ! if(EqLCumul(2,jFTA)==0.0) call HaltWith(0,0,' zero divisor')
          StdDev=sqrt(EqLCumul(2,jFTA))
          z=(xLod-EqLCumul(1,jFTA))/StdDev
          Density=GaussianDen(z)
          if(z<0.0) then
            yPrb=1./Density-CCNFmd(-z)
          else
            yPrb=           CCNFmd( z)
          end if
          ySum=ySum+yPrb*Density*DistrWeight(jFTA) ! only if ordinate is of interest
          TailUQ=TailUQ+(1.-z*yPrb)*Density*StdDev*DistrWeight(jFTA)
        end do
        if(PrtDetail>2) write(PrtUni,'(f11.8,f13.6,a)') &
          ySum,TailUQ,Truncation
      return ! entry FindTailArea


      entry FindAreaBetween(xOrg,xWidth) ! allows for variable xStep
!       real*4 xOrg,xWidth,xDomain(0:1),xGrid(0:1),yGrid(0:1)
!       integer*2 iBegPt,iEndPt
!       logical*1 NegativeArea

        if(xWidth<0.0) then ! enforce xDomain(0)<xDomain(1) ordering
          NegativeArea=.true.
          xDomain(1)=xOrg
          xDomain(0)=xDomain(1)+xWidth
        else
          NegativeArea=.false.
          xDomain(0)=xOrg
          xDomain(1)=xDomain(0)+xWidth
        end if
        if(xDomain(1)>EqLoad(nEqPoints)) xDomain(1)=EqLoad(nEqPoints) ! enforce domain limits
        if(xDomain(0)<EqLoad(        0)) xDomain(0)=EqLoad(        0)
        TailUQ=0.0
        iEndPt=1
        do while((iEndPt< nEqPoints).and.(xDomain(0)>EqLoad(iEndPt)))
          iEndPt=iEndPt+1
        end do
        iBegPt=iEndPt-1
        do while((iEndPt<=nEqPoints).and.(xDomain(0)<xDomain(1)))
          xGrid(0)=EqLoad(iBegPt)
          xGrid(1)=EqLoad(iEndPt)
          yGrid(0)=EqLDur(iBegPt)
          yGrid(1)=EqLDur(iEndPt)
          if(xDomain(0)>xGrid(0)) then
            yGrid(0)=yGrid(0)+ &
           (yGrid(1)-yGrid(0))*(xDomain(0)-xGrid(0))/(xGrid(1)-xGrid(0))
            xGrid(0)=xDomain(0)
          end if
          if(xDomain(1)<xGrid(1)) then
            yGrid(1)=yGrid(0)+ &
           (yGrid(1)-yGrid(0))*(xDomain(1)-xGrid(0))/(xGrid(1)-xGrid(0))
            xGrid(1)=xDomain(1)
          end if
          TailUQ=TailUQ+(yGrid(1)+yGrid(0))*(xGrid(1)-xGrid(0)) ! twice trapezoidal area
          xDomain(0)=xGrid(1)
!         iBegPt=iEndPt ! probably slower than the statement below
          iBegPt=iBegPt+1
          iEndPt=iEndPt+1
        end do
        if(NegativeArea) then
          TailUQ=-0.5*TailUQ
        else
          TailUQ= 0.5*TailUQ
        end if
      return ! entry FindAreaBetween


      entry InvertMatrix(ArrayOrg,ArrayInv,nRows)
!     Inverts matrix ArrayOrg into matrix ArrayInv by Gauss-Jordan
!     pivoting, elimination, and triangular back-substitution.  The
!     order of both ArrayOrg and ArrayInv (both assumed square) is nRows.
!     The logic below assumes the second subscript indexes rows.
!       integer*2 nRows,PivotRow
!       integer*4 I4_nBinRow,I4_n
!       real*4 HeldElement,PivotElement,Factor,ArrayOrg(nRows,nRows),
!    +    ArrayInv(nRows,nRows),ArrayOld(:,:),HeldRow(LimUnknowns)

        allocate(ArrayOld(nRows,nRows),stat=AS)
        call CkAllocSt('AOld',0)

        I4_nBinRow=4*nRows ! for 4-byte reals
        I4_n=nRows
        do i=1,nRows
        ! call cmove(ArrayOrg(1,i),ArrayOld(1,i),I4_nBinRow) ! preserve the Org data from corruption
        ! ArrayOld(:,i)=ArrayOrg(:,i)
          call CopyLinRVec1(ArrayOrg(1,i),ArrayOld(1,i),nRows)
        ! adjoin the identity matrix in ArrayInv
!         ArrayInv(1,i:nRows) = 0.0 ! IS THIS CORRECT?  AGT replies 'no; see below'
!         ArrayInv(:,i)=0.0 ! alternatively, put above the do-i loop:  ArrayInv=0.0
!         call cinitd(ArrayInv(1,i),I4_n,0.0) !THIS WRAPS AS I INCREASES; AGT replies
!         'no it does not; memory advances as the leftmost index increases first'
          call initd1(ArrayInv(1,i),nRows,0.0)
          ArrayInv(i,i)=1.0
        end do
!       beginning at the upper left, reduce ArrayOld to upper-triangular
        do k=1,nRows
          PivotRow=k
          PivotElement=abs(ArrayOld(k,k))
          do i=k+1,nRows ! search rows i>k for maximum pivotal element
            if(PivotElement<abs(ArrayOld(k,i))) then
              PivotRow=i
              PivotElement=abs(ArrayOld(k,i))
            end if
          end do
          if(PivotRow>k) then ! swap rows k,PivotRow in ArrayOld and ArrayInv
          ! HeldRow             =ArrayOld(:,PivotRow) ! call cmove(ArrayOld(1,PivotRow),HeldRow             ,I4_nBinRow)
          ! ArrayOld(:,PivotRow)=ArrayOld(:,       k) ! call cmove(ArrayOld(1,       k),ArrayOld(1,PivotRow),I4_nBinRow)
          ! ArrayOld(:,       k)=HeldRow              ! call cmove(HeldRow             ,ArrayOld(1,       k),I4_nBinRow)
          ! HeldRow             =ArrayInv(:,PivotRow) ! call cmove(ArrayInv(1,PivotRow),HeldRow             ,I4_nBinRow)
          ! ArrayInv(:,PivotRow)=ArrayInv(:,       k) ! call cmove(ArrayInv(1,       k),ArrayInv(1,PivotRow),I4_nBinRow)
          ! ArrayInv(:,       k)=HeldRow              ! call cmove(HeldRow             ,ArrayInv(1,       k),I4_nBinRow)
            call CopyLinRVec1(ArrayOld(1,PivotRow),HeldRow,nRows)
            call CopyLinRVec1(ArrayOld(1,k),ArrayOrg(1,PivotRow),nRows)
            call CopyLinRVec1(HeldRow             ,ArrayOld(1,k),nRows)
            call CopyLinRVec1(ArrayInv(1,PivotRow),HeldRow,nRows)
            call CopyLinRVec1(ArrayInv(1,k),ArrayInv(1,PivotRow),nRows)
            call CopyLinRVec1(HeldRow             ,ArrayInv(1,k),nRows)
          end if
        ! pivot element being in ArrayOld(k,k),
        ! zero out elements below it in column k
          PivotElement=ArrayOld(k,k)
          if(Nearly0(PivotElement,0)) call HaltWith(nRows,k, &
            'InvertMatrix aborted with small pivot element in row')
          do i=k+1,nRows
            Factor=ArrayOld(k,i)/PivotElement
            do j=k,nRows
              ArrayOld(j,i)=ArrayOld(j,i)-Factor*ArrayOld(j,k)
            end do
            do j=1,nRows
              ArrayInv(j,i)=ArrayInv(j,i)-Factor*ArrayInv(j,k)
            end do
          end do ! loop on i
        end do ! loop on k
!       having now an upper triangular ArrayOld, diagonalize & normalize it
        do i=nRows,1,-1
          do j=i+1,nRows
            HeldElement=ArrayOld(j,i)
            do k=1,nRows
              ArrayInv(k,i)=ArrayInv(k,i)-HeldElement*ArrayInv(k,j)
            end do
          end do ! loop on j
          Factor=1.0/ArrayOld(i,i)
          do j=1,nRows
            ArrayInv(j,i)=ArrayInv(j,i)*Factor
          end do
        end do ! loop on i
        deallocate(ArrayOld,stat=AS)
        call CkAllocSt('AOld',1)
      return ! entry InvertMatrix


      entry ShowVectors ! in order of Bloom's tables
!       character*47 FmtNoPhi1,FmtNoPhi2
!       parameter(FmtNoPhi1='(1x,2i3,a,f8.2 ,f12.5,f9.3,f19.2,f9.3,f10.6)',
!    +            FmtNoPhi2='(1x,2i3,a,i5,i3,f12.5,f9.3,f19.2,f9.3,f10.6)')
!       real*4 AvgDurat(LimUnknowns),PrevAvg,Avg2ndP,PrevCap,Cap2ndP
!       character*4 ChrVS(LimUnknowns),ChrState(0:3)
!       character*2 Warning
!       data ChrState/' nB0',' nB1',' Key',' BnK'/

        write(PrtUni,Fmt1a) ' jV kF Sta RHS/U&B'//'        x       !        è         Dual   Capac   AvgDuratn'
        do j=1,nVar
          AvgDurat(j)=x(j)/(Capacity(j)*EquivAvail(j)) ! *Duration) needed if Duration=1.0
          ChrVS(j)=ChrState(VarState(j))
        end do

        do j=1,nVar
          if((VarState(j)==BasicNKy).and.(FctOwning(j)==InvalidFacet)) &
            write(PrtUni,FmtNoPhi1) &
              j,InvalidFacet,ChrVS(j),AuxRHS(AuxRow(j)), &
              x(j),cOrg(j),DualVar(j),Capacity(j),AvgDurat(j)
        end do

        do j=1,nVar
          if((VarState(j)==BasicNKy).and.(FctOwning(j)/=InvalidFacet)) &
            write(PrtUni,FmtNoPhi1) &
              j,FctOwning(j),ChrVS(j),AuxRHS(AuxRow(j)), &
              x(j),cOrg(j),DualVar(j),Capacity(j),AvgDurat(j)
        end do

        Avg2ndP=1.0
        PrevAvg=1.0
        Cap2ndP=0.0
        PrevCap=0.0
        do k=1,nActiveFacets
          j=KeyVar(k)
!         if((AvgDurat(j)>0.0000015).and.(AvgDurat(j)/NoiseFactor>PrevAvg)) then
          if((k>2).and. &
            (AvgDurat(j)>0.0000015).and.(AvgDurat(j)/NoiseFactor> &
            PrevAvg+(Avg2ndP-PrevAvg)*PrevCap/(Cap2ndP+PrevCap))) then
            Warning=' ^' ! note error:  avg. duration > peak duration in prev. key
            ! as estimated by back-casting half of PrevCap using a linear model
          else
            Warning=' '
          end if
          Avg2ndP=PrevAvg
          PrevAvg=AvgDurat(j)
          Cap2ndP=PrevCap
          PrevCap=Capacity(j)
          write &
            (PrtUni,'(1x,2i3,a,i5,i3,f12.5,2f9.3,f10.2,f9.3,f10.6,a)') &
            j,k,ChrVS(j),UnitOwning(j),BlockPos(j),x(j), &
            cOrg(j),Phi(k),DualVar(j),Capacity(j),AvgDurat(j),Warning
        end do



        do j=1,nVar
          if(VarState(j)==BoundNB1) write &
            (PrtUni,FmtNoPhi2)j,FctOwning(j),ChrVS(j),UnitOwning(j), &
            BlockPos(j),x(j),cOrg(j),DualVar(j),Capacity(j),AvgDurat(j)
        end do

        do j=1,nVar
          if(VarState(j)==BoundNB0) write &
            (PrtUni,FmtNoPhi2)j,FctOwning(j),ChrVS(j),UnitOwning(j), &
            BlockPos(j),x(j),cOrg(j),DualVar(j),Capacity(j),AvgDurat(j)
        end do
      return ! entry ShowVectors


      entry ShowStates
!       integer*2 k2
!       logical*1 FirstItem

        if(PrtDetail>3) then
          k2=0
          call wnaa(' Key:')
          do j2=1,nVar
            if(VarState(j2)==BasicKey) then
              call wnai(One2,j2)
              k2=k2+1
              if(mod(k2,i232)==31) write(PrtUni,Fmt1a)
            end if
          end do
          if(mod(k2,i232)/=31) write(PrtUni,Fmt1a)

          call wnaa(' Sta:')
          do j2=1,nVar
            call wnai(One2,VarState(j2))
            if(mod(j2,i232)==31) write(PrtUni,Fmt1a)
          end do
          if(mod(nVar,i232)/=31) write(PrtUni,Fmt1a)
        end if

        call wnaa(' BnK:')
        do j2=1,nVar
          if(VarState(j2)==BasicNKy) call wnai(One2,j2)
        end do
        write(PrtUni,Fmt1a)

        call wnaa(' nBa:')
        do j2=1,nVar
          if(VarState(j2)<BasicKey) call wnai(One2,j2)
        end do
        write(PrtUni,Fmt1a)

        call wnaa(' Fct: 0[')
        FirstItem=.true.
        do j2=1,nVar
          if(FctOwning(j2)==InvalidFacet) then
!           if(.not.FirstItem) call wnaa(',')
!           call wnai(Zer2,j2)
            if(FirstItem) then
              call wnai(Zer2,j2)
            else
              call wnai(One2,j2)
            end if
            FirstItem=.false.
          end if
        end do
        call wnaa(']')
        do k2=1,nActiveFacets
          call wnai(One2,k2)
          call wnaa('{')
          call wnai(Zer2,KeyVar(k2))
          do j2=1,nVar
            if((FctOwning(j2)==k2).and.(j2/=KeyVar(k2))) then

              call wnai(One2,j2)
            end if
          end do
          call wnaa('}')
          if(mod(k2,Ten2)==9) write(PrtUni,Fmt1a)
        end do
        if(mod(nActiveFacets,Ten2)/=9) write(PrtUni,Fmt1a)
      return ! entry ShowStates


      entry ShowCostAtIter(UnsvdQ)


        xAccum=0.0
        do j=1,nVar
          if(FctOwning(j)/=InvalidFacet) xAccum=xAccum+x(j)
        end do
        UnsvdQ=MeanLoad-xAccum+ChgCapac+x(nVar) ! x(nVar) is a pseudo-contract

        write(PrtUni,'(1x,f13.5,a,f13.5,a,f15.2,a\)') &
          UnsvdQ,'=UQ;',xAccum,'=äx;',InnerProduct(cOrg,x,nVar),'=cost'
        if(nIter==0) then
          write(PrtUni,Fmt1a)'at initialization'
        else
          call wnaa(' after iteration')
          call wnai(One2,nIter)
          write(PrtUni,Fmt1a)
        end if
        if(PrtDetail>1) write(PrtUni,Fmt1a)
      return ! entry ShowCostAtIter


      entry ShowLDC(yOrg,yNow)


        do j2=0,nEqPoints
          write(PrtUni,'(1x,i3,3f11.6)')j2,EqLoad(j2),yOrg(j2),yNow(j2)
        end do
      return ! entry ShowLDC


      entry IncludeOutages(Capac,jB,jC) ! assumes variable DxLoad


        if(UsingCumul) then
          if(PrtDetail>3) then
            write(PrtUni,Fmtf10)jB,UnitOwning(jB),BlockPos(jB),Capac, &
              ' Incl',VarCumul(1,jC),EqLCumul(1,0),sqrt(EqLCumul(2,0))
            if(PrtDetail>3) then
              write(PrtUni,Fmt6e) 0,(EqLCumul(j2, 0),j2=1,nKappa)
              write(PrtUni,Fmt6e)jC,(VarCumul(j2,jC),j2=1,nKappa)
            end if
          end if
          do j2=1,nKappa
            EqLCumul(j2,0)=EqLCumul(j2,0)+VarCumul(j2,jC)
          end do
          do j2=1,nAbnormalModes ! use a Normal approximation
            EqLCumul(1,j2)=EqLCumul(1,j2)+VarCumul(1,jC)
            EqLCumul(2,j2)=EqLCumul(2,j2)+VarCumul(2,jC)
          end do
        elseif((0.001<=EquivAvail(jB)).and.(EquivAvail(jB)<=0.999)) then
          EqAv=EquivAvail(jB)
          EFOR_loc=1.0-EqAv
        ! EDIncl=EqLDur ! call cmove(EqLDur,EDIncl,I4_4nEqPtsp1) ! the leading 1 and trailing zeroes will be retained
          call CopyLinRVec0(EqLDur,EDIncl,1+nEqPoints)
          if(PrtDetail>3) write(PrtUni,Fmtf68)EFOR_loc,Capac,' Incl'
          j2=0 ! EDIncl(0) need not be modified
          do while(j2<nEqPoints)
            j2=j2+1
            xShifted=EqLoad(j2)-Capac
            if(xShifted<=0.0) then
              yAtxShifted=Duration
            else
              jGLB=j2 ! the index of the greatest EqLoad<=xShifted
              do while((jGLB>0).and.(EqLoad(jGLB)>xShifted))
                jGLB=jGLB-1
              end do
              yAtxShifted=yInterpolate(xShifted, &
                EqLoad(jGLB),EqLoad(jGLB+1), &
                EqLDur(jGLB),EqLDur(jGLB+1))
            end if
            EDIncl(j2)=EqAv*EqLDur(j2)+EFOR_loc*yAtxShifted
            if(EDIncl(j2)<=0.0) j2=nEqPoints
          end do
          if(PrtDetail>3) call ShowLDC(EqLDur,EDIncl)
        ! EqLDur=EDIncl ! call cmove(EDIncl,EqLDur,I4_4nEqPtsp1) ! copying is probably faster than double-indexing
          call CopyLinRVec0(EDIncl,EqLDur,1+nEqPoints)
        end if
      return ! entry IncludeOutages


      entry ExcludeOutages(Capac,jB,jC) ! assumes variable DxLoad


        if(UsingCumul) then
          if(PrtDetail>3) then
            write(PrtUni,Fmtf10)jB,UnitOwning(jB),BlockPos(jB),-Capac, &
              ' Excl',VarCumul(1,jC),EqLCumul(1,0),sqrt(EqLCumul(2,0))
            if(PrtDetail>3) then
              write(PrtUni,Fmt6e) 0,(EqLCumul(j2, 0),j2=1,nKappa)
              write(PrtUni,Fmt6e)jC,(VarCumul(j2,jC),j2=1,nKappa)
            end if
          end if
          do j2=1,nKappa
            EqLCumul(j2,0)=EqLCumul(j2,0)-VarCumul(j2,jC)
          end do
          do j2=1,nAbnormalModes ! use a Normal approximation
            EqLCumul(1,j2)=EqLCumul(1,j2)-VarCumul(1,jC)
            EqLCumul(2,j2)=EqLCumul(2,j2)-VarCumul(2,jC)
          end do
        elseif((0.001<=EquivAvail(jB)).and.(EquivAvail(jB)<=0.999)) then
          EqAv=EquivAvail(jB)
          EFOR_loc=1.0-EqAv
        ! EDExcl=EqLDur ! call cmove(EqLDur,EDExcl,I4_4nEqPtsp1) ! the leading 1 and trailing zeroes will be retained
          call CopyLinRVec0(EqLDur,EDExcl,1+nEqPoints)
          if(PrtDetail>3) write(PrtUni,Fmtf68)EFOR_loc,-Capac,' Excl'
          if(EqAv>=0.5) then ! apply deconvolution to increasing abscissas
            j2=0 ! EDExcl(0) need not be modified
            do while(j2<nEqPoints)
              j2=j2+1
              xShifted=EqLoad(j2)-Capac
              if(xShifted<=0.0) then
!               EDExcl(j2)=(EqLDur(j2)-EFOR_loc*Duration)/EqAv
                EDExcl(j2)=(EqLDur(j2)-EFOR_loc         )/EqAv
              else
                jGLB=j2 ! the index of the greatest EqLoad<=xShifted
                do while((jGLB>0).and.(EqLoad(jGLB)>xShifted))
                  jGLB=jGLB-1
                end do
                if(jGLB+1==j2) then ! interpolation on EDExcl is impossible
                  FracStep=Capac/(EqLoad(j2)-EqLoad(j2-1)) ! less than unity
                  if(FracStep>NoiseFactor) call HaltWith &
                    (j2,jGLB,'ExcludeOutages error in extrapolation')
                  EDExcl(j2)=(EqLDur(j2)-EFOR_loc*FracStep*EDExcl(j2-1))/ &
                    (EqAv+EFOR_loc*(1.0-FracStep))
                else
                  EDExcl(j2)=(EqLDur(j2)-EFOR_loc*yInterpolate(xShifted, &
                    EqLoad(jGLB),EqLoad(jGLB+1), &
                    EDExcl(jGLB),EDExcl(jGLB+1)))/EqAv
                end if
              end if
            ! if(EDExcl(j2)>Duration) EDExcl(j2)=Duration
              if(EDExcl(j2)<=0.0) then
                do while(j2<=nEqPoints)
                  EDExcl(j2)=0.0
                  j2=j2+1
                end do
              end if
            end do
          else ! EqAv<0.5 => apply deconvolution to decreasing abscissas
            j2=nEqPoints
            do while((j2>0).and.(EqLDur(j2)==0.0))
              j2=j2-1
            end do ! index of final non-zero
            if(j2==nEqPoints) then
              jFirst0=j2
            else
              jFirst0=j2+1 ! index of the first 0-valued ordinate
            end if
            xZero=EqLoad(jFirst0) ! abscissa of the first zero-valued ordinate
            do while(j2>1) ! EDExcl(0) need not be modified
              j2=j2-1
              xShifted=EqLoad(j2)+Capac
              if(xShifted>=xZero) then
                EDExcl(j2)=0.0
              else
                jLUB=j2 ! the index of the least EqLoad>=xShifted
                do while((jLUB<jFirst0).and.(EqLoad(jLUB)>xShifted))
                  jLUB=jLUB+1
                end do
              ! yAtxShifted=yInterpolate(xShifted,
              !   EqLoad(jLUB-1),EqLoad(jLUB),
              !   EqLDur(jLUB-1),EqLDur(jLUB))
              ! yExAtxShifted=yInterpolate(xShifted,
              !   EqLoad(jLUB-1),EqLoad(jLUB),
              !   EDExcl(jLUB-1),EDExcl(jLUB))
              ! EDExcl(j2)=(yAtxShifted-EqAv*yExAtxShifted)/EFOR_loc
                EDExcl(j2)=yInterpolate(xShifted, &
                  EqLoad(jLUB-1),EqLoad(jLUB), &
                  EqLDur(jLUB-1)-EqAv*EDExcl(jLUB-1), &
                  EqLDur(jLUB  )-EqAv*EDExcl(jLUB  ))/EFOR_loc
              ! the 5 extra ops above save 6+3 ops and a function call
              end if
            ! if(EDExcl(j2)<0.0) EDExcl(j2)=0.0
              if(EDExcl(j2)>=Duration) then
                do while(j2>0)
                  EDExcl(j2)=Duration
                  j2=j2-1
                end do
              end if ! with j2=0, whose EDExcl need not be modified
            end do
          end if
          if(PrtDetail>3) call ShowLDC(EqLDur,EDExcl)
        ! EqLDur=EDExcl ! call cmove(EDExcl,EqLDur,I4_4nEqPtsp1) ! copying is probably faster than double-indexing
          call CopyLinRVec0(EDExcl,EqLDur,1+nEqPoints)
        end if
      return ! entry ExcludeOutages


      entry CopyEqLoad(FromHold)


        if(UsingCumul) then
          if(FromHold) then ! copy from the captured array
          ! EqLCumul=HeldCumul ! call cmove(HeldCumul,EqLCumul,I4_4LCmBnM)
            call CopyLinRVec1(HeldCumul,EqLCumul,int(I4_4LCmBnM/4,2))
          else              ! capture the current array
          ! HeldCumul=EqLCumul ! call cmove(EqLCumul,HeldCumul,I4_4LCmBnM)
            call CopyLinRVec1(EqLCumul,HeldCumul,int(I4_4LCmBnM/4,2))
          end if
        else
          if(FromHold) then
          ! EqLDur=HeldLDur ! call cmove(HeldLDur,EqLDur,I4_4nEqPtsp1)
            call CopyLinRVec0(HeldLDur,EqLDur,1+nEqPoints)
          else
          ! HeldLDur=EqLDur ! call cmove(EqLDur,HeldLDur,I4_4nEqPtsp1)
            call CopyLinRVec0(EqLDur,HeldLDur,1+nEqPoints)
          end if
        end if
      return ! entry CopyEqLoad


      entry CopyEqLoad0(FromHold0)
!       logical*1 FromHold0

        if(UsingCumul) then
          if(FromHold0) then ! copy from the array including outages for kInf-1
          ! EqLCumul=Ld0Cumul ! call cmove(Ld0Cumul,EqLCumul,I4_4LCmBnM)
            call CopyLinRVec1(Ld0Cumul,EqLCumul,int(I4_4LCmBnM/4,2))
          else              ! copy into the array including outages for kInf-1
          ! Ld0Cumul=EqLCumul ! call cmove(EqLCumul,Ld0Cumul,I4_4LCmBnM)
            call CopyLinRVec1(EqLCumul,Ld0Cumul,int(I4_4LCmBnM/4,2))
          end if
        else
          if(FromHold0) then
          ! EqLDur=EqLDn0 ! call cmove(EqLDn0,EqLDur,I4_4nEqPtsp1)
            call CopyLinRVec0(EqLDn0,EqLDur,1+nEqPoints)
          else
          ! EqLDn0=EqLDur ! call cmove(EqLDur,EqLDn0,I4_4nEqPtsp1)
            call CopyLinRVec0(EqLDur,EqLDn0,1+nEqPoints)
          end if
        end if
      return ! entry CopyEqLoad0


      entry ChkPriorBlock(PrBkPos,jCPB,kFct,InclThisFct,PreTestSet)


        jBlkPair=jCPB ! default: cumulants use incoming block index
        PriorBlockLoaded=max(BlockPos(jCPB),int(1,2))/=PrBkPos

        if(PriorBlockLoaded) then
          jPrBk=BlockOfSameUnitAs(jCPB,PrBkPos)
          if(PreTestSet.and..not.InclThisFct) then ! define booleans tested below
            InclThisFct=FctOwning(jPrBk)==kFct ! jPrBk is always loaded before jCPB
            TestSetOwns(jPrBk)=.false. ! needed in case not InclThisFct
          end if
          if(jPrBk==InvalidIndex) then
            PriorBlockLoaded=.false.
          elseif(InclThisFct) then
            PriorBlockLoaded=(FctOwning(jPrBk)/=InvalidFacet).and. &
                             (FctOwning(jPrBk)<=kFct)
          elseif(.not.TestSetOwns(jPrBk)) then ! TestSetOwns(jPrBk)=>PriorBlockLoaded
            PriorBlockLoaded=(FctOwning(jPrBk)/=InvalidFacet).and. &
                             (FctOwning(jPrBk)< kFct)
          end if
        end if
        if(PriorBlockLoaded) then
          if(UsingCumul) then ! combine cumulants of pair of blocks (not independent)
            jBlkPair=-1 ! store result as if the combined-block index were -1
            EscalationFactor=1.0+(Capacity(jCPB)/Capacity(jPrBk)) ! same EFOR_loc applies
            FactorToKthPower=1.0
            do kCPB=1,nKappa ! cumulants are proportional to the kth power of capacity
              FactorToKthPower=FactorToKthPower*EscalationFactor
              VarCumul(kCPB,-1)=VarCumul(kCPB,jPrBk)*FactorToKthPower
            end do
            if(PrtDetail>3) then
              write(PrtUni,Fmtf8p2) jPrBk,Capacity(jPrBk), &
                (VarCumul(kCPB,jPrBk),kCPB=1,nKappa)
              write(PrtUni,Fmtf8p2) jCPB ,Capacity(jCPB ), &
                (VarCumul(kCPB,jCPB ),kCPB=1,nKappa)
              write(PrtUni,Fmtf8p5) -1  ,EscalationFactor, &
                (VarCumul(kCPB,-1   ),kCPB=1,nKappa)
            end if
          end if
          CapConv=CapConv+Capacity(jPrBk)
        end if
      return ! entry ChkPriorBlock


!     here to ObtainBasicFeasibleSolution, program units are needed only thereby


      entry LimitAccum(jLA,xAccumLA)


        CapConv=Capacity(jLA)
        call ChkPriorBlock(One2,jLA,nActiveFacets,FalseByte,True_Byte) ! remove effects of its outages:
        if(PriorBlockLoaded) &
          call ExcludeOutages(Capacity(jPrBk),jPrBk,jPrBk)
        if(UsingCumul) then ! avoid prior logical error when UsingCumul:
        ! multiplication of TailUQ by a fixed probability treats outages as 2-state;
        ! to keep the treatment logically consistent, one must view outages as having
        ! a continuous distribution of values, as defined by VarCumul(j) above; the
        ! area served by var jLA is just the difference in unserved quantities:

          if(PriorBlockLoaded) call FindTailArea(xLoadPoint) ! TailUQ is obsolete ...
          OrigUQ=TailUQ ! ... else value from the prior call to FindTailArea is valid
          call IncludeOutages(Capacity(jLA),jLA,jLA) ! was CapConv,jLA,jBlkPair)
          call FindTailArea(xLoadPoint+Capacity(jLA))
          x(jLA)=OrigUQ-TailUQ ! the decrease in UQ after convolution of just var jLA
          if(PriorBlockLoaded) then ! remove jLA's outages, include those of the pair
            call ExcludeOutages(Capacity(jLA),jLA,jLA)
            call IncludeOutages(CapConv      ,jLA,jBlkPair)
            call FindTailArea(xLoadPoint+Capacity(jLA)) ! with both blocks included
          end if
        else
          call FindAreaBetween(xLoadPoint,Capacity(jLA))
          call IncludeOutages(CapConv,jLA,jBlkPair)
          x(jLA)=EquivAvail(jLA)*TailUQ
        end if
        if(x(jLA)>LUB(jLA)) then
          x(jLA)=LUB(jLA)
        elseif(x(jLA)<GLB(jLA)) then
          x(jLA)=GLB(jLA)
        end if
        xAccumLA=xAccumLA+x(jLA)
        return ! entry LimitAccum


      entry ShowItem(jSI,xAccumSI)


        write(PrtUni, &
          '(1x,2i3,i2,L2,f10.1,f9.3,2f10.3,f7.4,f13.6,f10.3)') &
          jSI,FctOwning(jSI), & !  nActiveFacets is misleading as a surrogate
          VarState(jSI),AuxiCovers(jSI),xLoadPoint,Capacity(jSI), &
          xAccumSI,cMod(jSI),EquivAvail(jSI),x(jSI),LUB(jSI)
      return ! entry ShowItem


      entry MakeBound(jMB)


        if(x(jMB)-GLB(jMB)<LUB(jMB)-x(jMB)) then
          VarState(jMB)=BoundNB0
          if(GLB(jMB)==0.0) FctOwning(jMB)=nActiveFacets ! dubious if GLB(jMB)>0
        else
          VarState(jMB)=BoundNB1
          jPrBk=BlockOfSameUnitAs(jMB,1)
          if(jPrBk==InvalidIndex) then
            kPrBk=InvalidFacet
          else
            kPrBk=FctOwning(jPrBk)
          end if

          if(AuxiCovers(jMB).or.((BlockPos(jMB)==2).and. &
            (InvalidFacet/=kPrBk).and.(kPrBk<=kInf))) then
            FctOwning(jMB)=kInf ! kPrBk is on [1,kInf]
          elseif(jPrBk==InvalidIndex) then ! exclude from optimization
            FctOwning(jMB)=kInf-1
            call CopyEqLoad(True_Byte) ! restore values from
          ! augment (kInf-1)'s EqLDur by newly base-loaded unit's outages
            CapConv=Capacity(jMB)
            xLoadPtInf=xLoadPtInf+CapConv
            call CopyEqLoad0(True_Byte) ! from the former array, then ...
            call IncludeOutages(CapConv,jMB,jMB) ! no need to ChkPriorBlock()
            call CopyEqLoad0(FalseByte) ! ... save (kInf-1) vars' outages
          elseif((kPrBk/=InvalidFacet).and.(kPrBk<=FctOwning(jMB))) then
          ! place blocks of same unit together in attempt to keep facet kInf small
            FctOwning(jMB)=kPrBk
          else
            FctOwning(jMB)=kInf
          end if
        end if
      return ! entry MakeBound


      entry CommitStgPair(jCSV,xAcCSV,UnityRetCF)


!       commit the return vars and their discharge complements;
!       employ a fictitious 'return' plant to offset the assumption of CF=1

        iAux=AuxRow(jCSV) ! if storage vars appear in only 1 aux constraint
        do jAux=1,nVar
          if(AuxCovers(iAux,jAux).eqv..true.) then
            if(jAux==jCSV) then ! treat return var as if loaded first
              FctOwning(jCSV)=nActiveFacets
              cMod(jCSV)=0.0 ! only the return var's cost was manipulated
              ! preclude overgeneration beyond the limit imposed by AuxRHS:
              if(Nearly0(AuxRHS(iAux),1)) then
                AuxRHS(iAux)=LUB(jCSV)*AColumn(iAux,jCSV)
              elseif(LUB(jCSV)>(AuxRHS(iAux)/AColumn(iAux,jCSV))) then
                     LUB(jCSV)= AuxRHS(iAux)/AColumn(iAux,jCSV)
              end if
              if(UnityRetCF) then
!               assume that return vars are base-loaded at their LUB limit, since
!               the pumps have unity availability (no outages are convolved):
                VarState(jCSV)=BasicNKy
                x(jCSV)=LUB(jCSV)
                xAcCSV=xAcCSV+x(jCSV)
              else ! return vars become BasicKey, discharge vars BasicNKy
                KeyVar(nActiveFacets)=jCSV
                VarState(jCSV)=BasicKey
                call LimitAccum(jCSV,xAcCSV)
                nActiveFacets=nActiveFacets+1
              end if
            else ! treat discharge var as initially minimally used at the peak
              FctOwning(jAux)=InvalidFacet ! default implying need for later assignment
              VarState(jAux)=BasicNKy ! even if degenerate at x(jAux)=0
              if(UnityRetCF) then
                LUB(jAux)=LUB(jCSV)*AColumn(iAux,jCSV)/ &
                  AColumn(iAux,jAux)
                x(jAux)=0.0 ! force initial degeneracy in discharge vars
              else
                x(jAux)=(AuxRHS(iAux)-AColumn(iAux,jCSV)*x(jCSV))/ &
                  AColumn(iAux,jAux)
                if(x(jAux)<0.0) x(jAux)=0.0 ! preclude noise implying it is aux slack
              end if
            ! do not advance xLoadPoint, as x(jAux) is area near the peak
            end if
            if(PrtDetail>2) call ShowItem(jAux,xAcCSV)
            if(jAux==jCSV) xLoadPoint=xLoadPoint+Capacity(jCSV) ! only after ShowItem
          end if
        end do
      return ! entry CommitStgPair


      entry DeleteFacet(kDel)


        nActiveFacets=nActiveFacets-1
        do k=kDel,nActiveFacets ! shift to compress
          KeyVar(k)=KeyVar(k+1)
        end do
        do j=1,nVar ! shift to compress
          if(FctOwning(j)>=kDel) FctOwning(j)=FctOwning(j)-1
        end do
      return ! entry DeleteFacet


      entry ChkAreaOfBNK(iLastRetVar,iDisp,kFrm,xAccumCAB,xDischgRem)

        InPriorFct=.false.
        iLastRetVarInit=iLastRetVar
        PrevUQ=TailUQ ! after loading only the key var
        xAccumInit =xAccumCAB
        xDischgInit=xDischgRem
        xDischgUsed=0.0
        CDischgUsed=0.0
        xAggNoSplit=0.0 ! discharge vars can be treated as an increasingly large
!       aggregate because they have 0 outage rates and are limited by capacity only;
!       each of the candidate discharge vars' x-values has been assigned within
!       CommitStgPair to balance with its corresponding return unit in aux equation
        xKeyDecr=0.0 ! cumulative amount yielded by reductions of x(jKey)
        iLRVInf=1 ! default valid when not InPriorFct
        do while(iLastRetVar>=iLRVInf) ! probe:  can x(jDis) be placed under the ...
        ! ... curve by loading jDis just after jKey in the current facet?
          jDis=VarOfDispRank(iLastRetVar)+1 ! last return var is the most economic,
          ! so probe its discharge var, whose index is just one greater
          xDischgUsed=xDischgUsed+x(jDis) ! used by CommitStgPair to balance aux eqn.
          CDischgUsed=CDischgUsed+Capacity(jDis)
          xAggPrev=xAggNoSplit
          FirmUQ=TailUQ ! UQ just before probing jDis
          if(UsingCumul) then
            call FindTailArea(xLoadPoint+CDischgUsed)
            xAggNoSplit=PrevUQ-TailUQ ! decrease in unserved quantity after loading
          else
            call FindAreaBetween(xLoadPoint,CDischgUsed)
            xAggNoSplit=TailUQ
          end if
          if(PrtDetail>2) &
            write(PrtUni,'(1x,2i3,5f13.6,a)')iLastRetVar,jDis, &
            CDischgUsed,PrevUQ,TailUQ,xAggNoSplit,xDischgUsed,' probe'

          jDisFits=xAggNoSplit+xKeyDecr<xDischgUsed
          if((PrevUQ<xDischgRem).or.jDisFits.or.InPriorFct) then
          ! assign jDis to the facet; x(jDis) 'splits' x(jKey)
            jKey=KeyVar(nActiveFacets)
            xKeyPrev=x(jKey) ! before possible reduction to meet needs of jDis
            xDisPrev=x(jDis)
            SameStgEqn=.false.
            if(AuxiCovers(jKey).eqv..true.) then
              iAux=0
              do while((iAux<nAux).and.(.not.SameStgEqn))
                iAux=iAux+1
                SameStgEqn=(AuxCovers(iAux,jKey).and.AuxCovers(iAux,jDis))
              end do
            end if
            if(PrtDetail>2) &
              write(PrtUni,'(1x,2i3,i2,L2,6f9.4,a)')jKey,nActiveFacets, &
              jDis,SameStgEqn,xDischgRem,TailUQ,xKeyDecr,xAggNoSplit, &
              xDischgUsed,x(jKey),' split by dischg var'
            if(jDisFits) then ! reduce x(jKey) to fit jDis under the curve
              if(SameStgEqn) then ! return and discharge quantities must fit StgEqn;
              ! solve 2 equations in 2 unknowns, assuming AColumn(iAux,jDis)=1:
              ! AColumn(iAux,jKey)*x(jKey)+AColumn(iAux,jDis)*x(jDis)=AuxRHS(iAux)
              ! [return var]       x(jKey)+[discharge var]    x(jDis)=PairsQ
                PairsQ=x(jKey)+xKeyDecr+xAggNoSplit-xAggPrev
                x(jKey)=(PairsQ-AuxRHS(iAux))/(1.0-AColumn(iAux,jKey)) ! return var
                x(jDis)=PairsQ-x(jKey)                                 ! dischg var
              else ! retain x(jDis) as computed by CommitStgPair
                x(jKey)=x(jKey)+xKeyDecr+xAggNoSplit-xDischgUsed ! possibly negative
              end if
              xKeyDecr=xKeyDecr+xKeyPrev-x(jKey) ! increases as x(jKey) decreases
              if(PrtDetail>2) write &
                (PrtUni,'(1x,2i3,i2,L2,6f9.4,a)')jKey,nActiveFacets, &
                jDis,SameStgEqn,xDischgRem,TailUQ,xKeyDecr,xAggNoSplit, &
                xDischgUsed,x(jKey),' reduced key-var'
!           else ! area is not binding on jDis; retain x(jDis) from CommitStgPair
!             if(PrevUQ<xDischgRem) x(jKey)=xKeyPrev ! neither reduction nor regression is needed
            end if
            if(x(jKey)>=0.0) then ! xLoadPoint for jDis remains as after jKey
              xAccumCAB=xAccumCAB-xKeyPrev+x(jKey)
              if(PrtDetail>2) then
                xLoadPoint=xLoadPoint-Capacity(jKey)
                call ShowItem(jKey,xAccumCAB)
                xLoadPoint=xLoadPoint+Capacity(jKey)
              end if
            elseif(x(jKey)>-SmallReal) then
              x(jKey)=0.0 ! map noise into zero
            end if
!           if((x(jKey)<0.0).and..not.InPriorFct) then ! regress once to prior facet
            if((x(jKey)<0.0).and.(nActiveFacets>kFrm)) then ! regress to prior facet
              if(PrtDetail>2) &
                write(PrtUni,'(1x,3i3,2x,6f9.4,a)')iLastRetVar, &
                nActiveFacets,jDis,CDischgUsed,TailUQ,xKeyDecr, &
                xAggNoSplit,xDischgUsed,x(jKey),' regress'
              xLoadPoint=xLoadPoint-Capacity(jKey) ! assuming none but jKey in facet
              call ExcludeOutages(Capacity(jKey),jKey,jKey)
              call ChkPriorBlock(One2,jKey,nActiveFacets, &
                FalseByte,True_Byte) ! restore prior-block's outages
              if(PriorBlockLoaded) &
                call IncludeOutages(Capacity(jPrBk),jPrBk,jPrBk)
            ! restore prior TailUQ assuming none but jKey exists in prior facet
              if(UsingCumul) call FindTailArea(xLoadPoint)
              PrevUQ=TailUQ ! after loading only the key var
            ! TailUQ=UQAftPrevKey ! allow for some non-key vars in prior facet
              xAccumCAB=xAccumInit-xKeyPrev
              VarState(jKey)=BoundNB0
              xKeyDecr=0.0
              x(jKey)=-SmallReal ! unnecessary, but indicates need for reassignment
              x(jDis)=xDisPrev
              nActiveFacets=nActiveFacets-1
              jKey=KeyVar(nActiveFacets)
              iLRVInf=iLastRetVar ! assign no further than the current var
              iLastRetVar=iLastRetVarInit ! start over with the incoming values
              iDisp=iDisp-1 ! inform caller of regression
              xDischgRem=xDischgInit
              xDischgUsed=0.0
              CDischgUsed=0.0
              xAggNoSplit=0.0
              InPriorFct=.true.
              cycle ! after just regressing
            else ! commit var jDis to this facet

              xAccumCAB =xAccumCAB +x(jDis)
              xDischgRem=xDischgRem-x(jDis)
              FctOwning(jDis)=nActiveFacets ! VarState remains BasicNKy
              if(PrtDetail>2) call ShowItem(jDis,xAccumCAB)
              iLastRetVar=iLastRetVar-1 ! next most-economic storage var to be considered

            end if
          else ! area not binding on jDis, end assignment of disch. vars to this facet
            xLoadPoint=xLoadPoint+CDischgUsed-Capacity(jDis)
            TailUQ=FirmUQ ! needed only if UsingCumul
            return ! later-loaded discharge vars have lower CF
          end if
        end do
        xLoadPoint=xLoadPoint+CDischgUsed ! final TailUQ is correct
      return ! entry ChkAreaOfBNK


      entry MakeKeyVar(jMKV,xAcMKV) ! make var jMKV key in facet nActiveFacets


        VarState(jMKV)=BasicKey
        FctOwning(jMKV)=nActiveFacets
        KeyVar(nActiveFacets)=jMKV
        call LimitAccum(jMKV,xAcMKV)
        if(PrtDetail>2) call ShowItem(jMKV,xAcMKV)
        xLoadPoint=xLoadPoint+Capacity(jMKV) ! only after ShowItem
      return ! entry MakeKeyVar


      entry SetCumulModelParams
        nGamma=nKappa-2
        if(nGamma>0) then
          SupiHP=3*nGamma-1
        else
          SupiHP=0
        end if
        I4_SupiHP=SupiHP
        I4_nKappa=nKappa
        I4_4nKappa=4*nKappa
      return ! entry SetCumulModelParams


      entry ObtainBasicFeasibleSolution
!       integer*2 jPt,jBFS,nStgVar,iLastRetVoDR,kFirm
!       logical*1 Monotonic
!       real*4 bAccum,xAHold,xjHold,DisCapac,ChgQ,DisQ,SumStgRHS,
!      +  Efficiency,InfEfficiency
!       real*8 ThisUQ,ChgUQ

        if(PrtDetail>2) write(PrtUni,Fmt1a)'ObtainBasicFeasibleSolution'
!       call cinitw(PrevBDIRow,I4_nVar,InvalidIndex)
!       call cinitw(PrevBKyVar,I4_nVar,InvalidIndex)
!       call cinitw(VarState,I4_nVar,BoundNB0) ! default implying var j is lower-bound
!       call cinitw(FctOwning,I4_nVar,0) ! perhaps wasted effort re merit-order dispatch
!       call cinitb(WouldBeZero,I4_nVar,.false.) ! enable all vars for basis entry
        call initw1(PrevBDIRow,nVar,InvalidIndex)
        call initw1(PrevBKyVar,nVar,InvalidIndex)
        call initw1(VarState,nVar,BoundNB0) ! default implying var j is lower-bound
        call initw1(FctOwning,nVar,0) ! perhaps wasted effort re merit-order dispatch
        call initb1(WouldBeZero,nVar,FalseByte) ! enable all vars for basis entry
        if(UsingCumul) then
        ! EqLDur=EqLDn0 ! call cmove(EqLDn0,EqLDur,int4(4*(1+nPoints))) ! with 0 outages included by convolution
          call CopyLinRVec0(EqLDn0,EqLDur,1+nEqPoints)
          nAbnormalModes=0
          call SetCumulModelParams
          DistrWeight(0)=1.0 ! possibly revised
          call GetLoadCumulants ! for abnormal modes and the residual
          I4_4LCmBnM=4*int(LimCumulants,4)*(1+nAbnormalModes) ! residual has index 0
          call CopyEqLoad0(True_Byte) ! without any plant-outage effects
!         FixingOvershoots=.false.
          FixingOvershoots=.true.
          if(PrtDetail>2) then ! show extreme z values
            write(PrtUni,'(1x,a,i3,2f11.4,4e13.6)') &
              'Kd0',0,EqLCumul(1,0), &
              sqrt(EqLCumul(2,0)),(EqLCumul(k,0),k=3,nKappa)
            write(PrtUni,'(1x,a,i3,2f11.6)')'BPz',0, &
              (BaseLoad       -MeanLoad)/sqrt(EqLCumul(2,0)), &
              (EqLoad(nPoints)-MeanLoad)/sqrt(EqLCumul(2,0))
            call FindTailArea(BaseLoad)
            call FindTailArea(EqLoad(nPoints))
          end if
!         do
!           call ChkLoadModel(Monotonic)
!           if(Monotonic.or.(nKappa<=2)) exit
!           nKappa=nKappa-1 ! make another attempt using a lower-order model
!           call SetCumulModelParams
!         end do
!         FixingOvershoots=.true.
          call FillPascalTriangle(nKappa)
          call GetPlantCumulants
        else
          MeanDxLoad=(EqLoad(nPoints)-EqLoad(1))/real(nPoints-1)
          nEqPoints=1+int((3.0*EqLoad(nPoints)-EqLoad(1))/MeanDxLoad)
          if(nEqPoints>LimEqPoints) nEqPoints=LimEqPoints
          I4_4nEqPtsp1=4*(1+nEqPoints)
          do jPt=nPoints+1,nEqPoints
            EqLoad(jPt)=EqLoad(jPt-1)+MeanDxLoad
            EqLDn0(jPt)=0.0
          end do
          call CopyEqLoad0(True_Byte) ! without any plant-outage effects
        end if

        xAccum=0.0 ! sum of quantities accumulated (for debugging use only)
        bAccum=0.0 ! sum of bounded quantities
        ChgCapac=0.0 ! sum of storage-charging capacities
        DisCapac=0.0 ! sum of discharge vars' capacities
        SumStgRHS=0.0 ! sum of storage aux equations' right-hand sides
        InfEfficiency=1.0 ! least storage efficiency
!       SupcDisch=0.0 ! largest discharge cost

        nStgVar=nStgEqn*2
        do j=1,nStgVar,2
!         if(mod(j,2)==1) then ! for return vars (odd j)
            ChgCapac=ChgCapac+Capacity(j) ! *EquivAvail(j) to derate
            cOrg(j)=0.0 ! to report correct cost in tables and InnerProduct
            Efficiency=Capacity(j+1)/Capacity(j) ! less than unity
            if(InfEfficiency>Efficiency) InfEfficiency=Efficiency

          iAux=AuxRow(j) ! assume storage vars appear in only 1 aux constraint
          if(AuxCovers(iAux,j).eqv..true.) &
            SumStgRHS=SumStgRHS+AuxRHS(iAux)
        end do

        if(PrtDetail>2) write(PrtUni,Fmt1a) ' jV kF s AuxC PriCap'// &
          '   Capacy    xAccum      cost EqvAvl       xValue       LUB'
        kInf=1 ! default, valid if no var can a priori be excluded from optimization
        xLoadPoint=-ChgCapac ! return vars initially have unity capacity-factor
        if(UsingCumul) call FindTailArea(xLoadPoint) ! retain TailUQ for LimitAccum
        jBFS=InvalidIndex
        nActiveFacets=1 ! initially embracing at least the UQ server
        do i=1,nDisp ! commit dispatch of BP==0 base-loaded vars in facet 1
          j=VarOfDispRank(i)
          if(j<=nStgVar) cycle ! storage vars will be dispatched later
          if(AuxiCovers(j) & ! cannot ignore effects of constraints
!       +      .or.((BlockPos(j)/=0).and.(xLoadPoint+Capacity(j)>BaseLoad))) exit
            .or.(BlockPos(j)/=0)) exit ! commitment up to base-load is uneconomic here
          FctOwning(j)=1
          VarState(j)=BoundNB1
          call LimitAccum(j,xAccum)
          if(PrtDetail>2) call ShowItem(j,xAccum)
          xLoadPoint=xLoadPoint+Capacity(j) ! only after ShowItem
          jBFS=j
        end do
        xLoadPtInf=xLoadPoint ! the starting load point for optimizations in kInf
        if(jBFS/=InvalidIndex) then ! at least one BP==0 var was base-loaded
          VarState(jBFS)=BasicKey ! enkey the costliest var in the first facet ...
          KeyVar(1)=jBFS ! ... so DualVar will show vars are in economic order
          kInf=2
          nActiveFacets=kInf
          call CopyEqLoad0(FalseByte) ! save EqLDur including prior-loaded vars' outages
        end if
        do i=1,nDisp ! commit dispatch of other base-loaded vars after facet 1,
                     ! but before the return vars (for speed advantage)
          j=VarOfDispRank(i)
          if((VarState(j)/=BoundNB0) & !  skip BP==0 vars committed above
            .or.(j<=nStgVar)) cycle ! storage vars will be dispatched later
          if(AuxiCovers(j).or.(xLoadPoint+Capacity(j)>BaseLoad)) exit
        ! save (for later reference) values prior to assigning var j to base load
          PrevUQ=TailUQ
          xAHold=xAccum
          xjHold=x(j)
          call CopyEqLoad(FalseByte)
          call MakeKeyVar(j,xAccum) ! subject to approval w.r.t. SumStgRHS
          ThisUQ=TailUQ ! value returned from just-prior call to FindTailArea
        ! test whether sufficient tail UQ exists to satisfy return/discharge equations
        ! no need to invoke IncludeOutages, since chg/discharge vars have 0 outages
          call FindTailArea(xLoadPoint+ChgCapac) ! probe loading return vars just ...
          ChgQ=ThisUQ-TailUQ
          ChgUQ=TailUQ ! (UQ after loading return vars together)
          call FindTailArea(xLoadPoint+ChgCapac+DisCapac) ! ... before discharge vars
          DisQ=ChgUQ-TailUQ
          if(PrtDetail>3) write(PrtUni,'(1x,a,f6.4,3f9.2)') &
            'Contiguous-storage questioned; n*ChgQ+DisQ < äRHS? ', &
            InfEfficiency,ChgQ,DisQ,SumStgRHS
          if((InfEfficiency*ChgQ+DisQ)<SumStgRHS) then ! with ret & dischg adjacent,
          ! tail area is insufficient to guarantee satisfying all stg constraints;
          ! revoke effects of MakeKeyVar, restoring values prior to loading var j
            call CopyEqLoad(True_Byte)
            TailUQ=PrevUQ
            xAccum=xAHold
            x(j)=xjHold
            xLoadPoint=xLoadPoint-Capacity(j)
            VarState(j)=BoundNB0
            FctOwning(j)=InvalidFacet
            KeyVar(nActiveFacets)=InvalidIndex
            if(PrtDetail>2) write(PrtUni,'(1x,a,f6.4,3f9.2)') &
              'Base-loading terminated; n*ChgQ+DisQ < äRHS: ', &
              InfEfficiency,ChgQ,DisQ,SumStgRHS
            exit ! base-loading var j might preclude balance of return/discharge
          end if
          TailUQ=ThisUQ ! restore tail area prior to probing, valid as var j loaded
          nActiveFacets=nActiveFacets+1
        end do
        kFirm=nActiveFacets ! least index of firmly committed facets

        DisQ=0.0
!       SplitCrit=0.0 ! .01*SmallArea ! SmallArea proved too conservative in ChkAreaOfBNK
        iLastRetVoDR=nStgEqn ! VarOfDispRank index of last return var committed
        i=1
        do while(i<=nDisp) ! perform merit-order dispatch of BoundNB0 vars
          j=VarOfDispRank(i)
          if((j<=nStgVar).and.(VarState(j)==BoundNB0).and. &
            (mod(j,Two2)==1)) then ! j indexes the 'return' var in the pair
            TempLog1=(jBFS==InvalidIndex)
            call CommitStgPair(j,xAccum,TempLog1)
            DisQ=DisQ+x(j+1) ! sum of discharge vars' Q when BoundNB0
          ! kFirm=nActiveFacets ! least index of firmly committed facets
          ! allow the return var's facet to be split (if required) by a regression
          elseif((j>nStgVar).and.(VarState(j)==BoundNB0).and. &
            (GLB(j)==0.0).and.(.not.AuxiCovers(j))) then ! put var j in separate facet
!    +      .and.(cMod(j)>0.0)) then ! additional condition keeps aux slacks BoundNB0
                                     ! but in any case, aux slacks' i exceed nDisp
            call MakeKeyVar(j,xAccum)
            call ChkAreaOfBNK(iLastRetVoDR,i,kFirm,xAccum,DisQ)
            nActiveFacets=nActiveFacets+1
          end if
          i=i+1
        end do

        do j=1,nVar-1
          if(VarState(j)==BoundNB0) then
            FctOwning(j)=nActiveFacets ! dubious if GLB(j)>0
            if((j>nStgVar).and.(GLB(j)>0.0).and.(x(j)==0.0)) then
            
                x(j)=GLB(j)





              bAccum=bAccum+x(j) ! to be deducted from the UQ server generation
!             xLoadPoint need not be advanced when bAccum accounts for the x(j),
!             since prior to the deduction, x of the UQ server includes these x(j)
              if(PrtDetail>2) call ShowItem(j,xAccum)
            end if
          end if
        end do
        call MakeKeyVar(nVar,xAccum) ! the UQ server
        call ChkAreaOfBNK(iLastRetVoDR,i,kFirm,xAccum,DisQ)
        ! place the last discharge vars
        do j=2,nStgVar,2 ! cover rare case of discharge var being zero
          if(FctOwning(j)==InvalidFacet) FctOwning(j)=nActiveFacets
        end do

        x(nVar)=x(nVar)-bAccum ! dubious deduction of x at GLB from tail quantity
!       do while(x(nVar)<bAccum) ! offload the penultimate var by the UQ server
        do while(bAccum>0.0) ! offload the penultimate var by the UQ server
          k=nActiveFacets-1
          if(k<=kInf) exit
          j=KeyVar(k)
          if(PrtDetail>2) write(PrtUni,'(1x,2i3,3f14.6,a)') &
            k,j,x(j),x(nVar),bAccum,' Offloaded'
          x(nVar)=x(nVar)+x(j)
          bAccum=bAccum-x(j) ! was not reduced
          x(j)=0.0
          VarState(j)=BoundNB0
          FctOwning(j)=nActiveFacets
          call DeleteFacet(k)
        end do

!       the following block of code is dubious, needs more thought:
        do j=1,nVar-1 ! the UQ server may not be an aux slack
          if(x(j)<0.0) then ! initialize aux slacks
            if(PrtDetail>2) write(PrtUni,'(1x,i3,e13.6,a)')j,x(j), &
              ' treated as aux slack'
            er_message='unexpected aux-slack'
            call end_program(er_message)
            FctOwning(j)=InvalidFacet ! aux slack vars are in no facet
            VarState(j)=BasicNKy ! since the other vars in constraint are Bound
            iAux=AuxRow(j) ! unique for aux slacks
!           x(j)=AuxRHS(iAux)/AColumn(iAux,j); ! only aux slacks & UQ are non-zero
            x(j)=AuxRHS(iAux) ! since not only aux slacks & UQ are non-zero
            do jAux=1,nVar
              if((jAux/=j).and.AuxCovers(iAux,jAux)) then
                x(j)=x(j)-AColumn(iAux,jAux)*x(jAux) ! possibly non-zero decrements
              end if
            end do
            x(j)=x(j)/AColumn(iAux,j)
!           if(x(j)<GLB(j)) x(j)=GLB(j) ! possible conflict with constraint
            if(PrtDetail>2) call ShowItem(j,xAccum)
          end if
        end do

        nIter=0
        Base2LogSize=1 ! sufficient for InitSort
        InitSort=.true. ! before adding loops to perform merit-order dispatch
        BasisStrucObsolete=.true.
        if(PrtDetail>2) write(PrtUni,'(1x,a,i3,2f11.4,4e13.6)')'KdF',0, &
          EqLCumul(1,0),sqrt(EqLCumul(2,0)),(EqLCumul(k,0),k=3,nKappa)
        if(PrtDetail>2) call ShowStates
        deallocate(VarOfDispRank,stat=AS) ! array is no longer useful
        call CkAllocSt('VoDR',1)
!       call HaltWith(0,0,'end of OBFS')
      return ! entry ObtainBasicFeasibleSolution


!     end of initialization code; following is iterative:

      entry ChkUnplacedBNK(UnplacedBNK)
!       each var j in B0 appears in exactly one aux equation (j is aux slack);
!       each var j in Bk appears in at least one aux equation, with the
!       multiplicity>1 for overlapping-group constraints; to guarantee that
!       each var j in Bk gets properly placed in BasisDIRow and BNKinRow, even
!       when it preceeds a j in B0 that the same AuxCovers, the entire loop
!       for B0 must be completed first (reserving places via BNKinRow) before
!       the loop for Bk is entered; the order implied in associating var j
!       with column i of DColumn (and therefore with row i of DInvRow) must
!       match that implied by the coefficients of the AColumn matrix
!       integer*2 jCU,kCU,UnplacedBNK

!       if(nAux>0) call cinitw(BNKinRow,I4_nAux,InvalidIndex)
        if(nAux>0) call initw1(BNKinRow,nAux,InvalidIndex)
        UnplacedBNK=InvalidIndex ! changed if any BasicNKy cannot be placed
        do jCU=1,nVar
          if(VarState(jCU)==BasicNKy) then
            kCU=FctOwning(jCU)
            if(kCU==InvalidFacet) then ! B0owns(j)
              i=AuxRow(jCU) ! unique for aux slacks
              BNKinRow(i)=jCU
              if(PrtDetail>3) write(PrtUni,'(1x,i3,a,i3)') &
                jCU,' is in B0, aux row',i
            end if
          end if
        end do
        do jCU=1,nVar
          if(VarState(jCU)==BasicNKy) then
            kCU=FctOwning(jCU)
            if(kCU/=InvalidFacet) then ! BkOwns(j) for valid k
              i=1
              do while((i<=nAux).and.(.not. &
                (AuxCovers(i,jCU).and.(BNKinRow(i)==InvalidIndex))))
                i=i+1
              end do
              if(i<=nAux) then
                BNKinRow(i)=jCU
              else
                UnplacedBNK=jCU
              end if
              if(PrtDetail>2) write(PrtUni,'(1x,i3,a,i3,a,i3)') &
                jCU,' is in B',kCU,', aux row',i
            end if ! else the defaults for BNKinRow are appropriate
          end if
        end do
      return ! entry ChkUnplacedBNK


      entry SubtractColumn(jSC,AColumnSC,AccumColumnSC)
!       subtract column jSC of AColumnSC from AccumColumnSC
!       integer*2 jSC
!       real*4 AColumnSC(nAux,*),AccumColumnSC(*)

        do iAux=1,nAux
          AccumColumnSC(iAux)=AccumColumnSC(iAux)-AColumnSC(iAux,jSC)
        end do
      return ! entry SubtractColumn


!     here to InvertAuxBasis, program units are needed only thereby


      entry AddScalarMult(Scalar,RowVect,v,nASM) ! augment v by Scalar*RowVect
!       integer*2 jASM,nASM
!       real*4 Scalar,RowVect(*),v(*)

        do jASM=1,nASM
          v(jASM)=v(jASM)+Scalar*RowVect(jASM)
        end do
      return ! entry AddScalarMult


      entry ModifyInverse(RowPrimary,NewCol,kMI,nMI)
!       using O(n^2) operations versus O(n^3) for a full inversion, compute
!       the inverse of an nxn matrix given its former inverse in RowPrimary
!       form and the new vector NewCol to be inserted in the kth column of
!       the original matrix before inversion; note that this technique is
!       applicable only to single-vector changes, since the prior inverse
!       must be appropriate to the basis just before the vector insertion;
!       source:  Best & Ritter, Linear Programming, Prentice-Hall, p. 23
!       (and obscurely Hadley, Linear Programming, Addison-Wesley, p. 49)
!       integer*2 kMI,nMI
!       real*4 kDotProd,RowPrimary(nMI,nMI),NewCol(nMI)

        kDotProd=InnerProduct(RowPrimary(1,kMI),NewCol,nMI)
        if(Nearly0(kDotProd,0)) call HaltWith(nMI,kMI, &
          'ModifyInverse aborted with small pivot result from row')
        do j=1,nMI
          RowPrimary(j,kMI)=RowPrimary(j,kMI)/kDotProd
        end do
        do i=1,nMI
          if(i/=kMI) call & !  row kMI must be changed first
            AddScalarMult(-InnerProduct(RowPrimary(1,i),NewCol,nMI), &
            RowPrimary(1,kMI),RowPrimary(1,i),nMI)
        end do
      return ! entry ModifyInverse


      entry TransposeAndInvert
!       the transpose of the inverse is the inverse of the transpose
        do i=1,nAux
          do j=1,nAux
            DOrgRow(j,i)=DColumn(i,j)
          end do
        end do
        if(nAux>0) call InvertMatrix(DOrgRow,DInvRow,nAux)
      return ! entry TransposeAndInvert


      entry ShowMat(PreInv,FullInv)
!       logical*1 PreInv,FullInv
!       character*12 MethodGJ
!       character*10 MethodSR
!       character*22 PostInv
!       parameter(MethodGJ='Gauss-Jordan',
!    +            MethodSR='Single-Row',
!    +            PostInv =' post-inversion using ')

        do i=1,nAux
          call wnaa(' ')
          do j=1,nAux
            write(PrtUni,'(f7.2\)')DColumn(i,j)
          end do
          do j=1,nAux
            write(PrtUni,'(f7.2\)')DInvRow(j,i)
          end do
          if(PreInv) then
            write(PrtUni,'(a)')' pre-inversion'
          elseif(FullInv) then
            write(PrtUni,'(a,a)')PostInv,MethodGJ
          else
            write(PrtUni,'(a,a)')PostInv,MethodSR
          end if
        end do
      return ! entry ShowMat


      entry CheckProduct
!       integer*2 iPass
!       logical*1 Errant
!       real*4 sCP

        Errant=.false.
        do iPass=1,2
          do i=1,nAux
            do j=1,nAux
              sCP=0.0
              do k=1,nAux
                sCP=sCP+DInvRow(k,i)*DColumn(k,j)
              end do
              if(iPass==1) then
                if(i==j) then
                  Errant=Errant.or.(.not.Nearly0(sCP-1.0,0))
                else
                  Errant=Errant.or.(.not.Nearly0(sCP    ,0))
                end if
              else  ! ==2
                write(PrtUni,'(1x,e9.2\)')sCP
              end if
            end do
            if(iPass==2) write(PrtUni,Fmt1a)
          end do
          if(.not.Errant) exit ! quit immediately after iPass=1
        end do
        if(Errant) call HaltWith(nAux,nAux, &
          '(D^-1)*D differs from the identity matrix:')
      return ! entry CheckProduct


      entry InvertAuxBasis
!       integer*2 jPrev,PrevKey,ThisKey,nDifferent,RowChanged
!       logical*1 InvObsolete

!       call cinitw(BasisDIRow,I4_nVar,InvalidIndex)
!       call cinitw(BasisKyVar,I4_nVar,InvalidIndex)
        call initw1(BasisDIRow,nVar,InvalidIndex)
        call initw1(BasisKyVar,nVar,InvalidIndex)
        if(BasisStrucObsolete) then
          call ChkUnplacedBNK(j) ! check whether each BasicNKy var has a DOrgRow
          if(j/=InvalidIndex) call HaltWith(j,nAux,Unplaceable)
          BasisStrucObsolete=.false.
        end if
        do i=1,nAux ! explete the non-key vars' basis structure
          j=BNKinRow(i)
          if(j==InvalidIndex) call HaltWith &
            (j,i,'var invalid in IAB, BNKinRow: ')
          k=FctOwning(j)
          BasisDIRow(j)=i
        ! DColumn(:,i)=AColumn(:,j) ! call cmove(AColumn(1,j),DColumn(1,i),I4_4nAux)
          call CopyLinRVec1(AColumn(1,j),DColumn(1,i),nAux)
          if(k/=InvalidFacet) then
            BasisKyVar(j)=KeyVar(k)
            call SubtractColumn(KeyVar(k),AColumn,DColumn(1,i))
          end if
        end do

        if(nIter==0) then
          call TransposeAndInvert
        else ! for nIter>0
          if(PrtDetail>3) then
            write(PrtUni,'(15(1x,2z2))') &
              (PrevBDIRow(j),PrevBKyVar(j),j=1,nVar)
            write(PrtUni,'(15(1x,2z2))') &
              (BasisDIRow(j),BasisKyVar(j),j=1,nVar)
          end if
          InvObsolete=.false.
          nDifferent=0
          do j=1,nVar
            if((BasisDIRow(j)/=PrevBDIRow(j)).or. &
               (BasisKyVar(j)/=PrevBKyVar(j))) then
              i=BasisDIRow(j) ! InvalidIndex for vars leaving basis
              if(i/=InvalidIndex) then
                InvObsolete=i/=PrevBDIRow(j)
                if(.not.InvObsolete) then
                ! KeyVars must differ, since DIRow-indices are identical
                  PrevKey=PrevBKyVar(j) ! possibly InvalidIndex
                  ThisKey=BasisKyVar(j) ! assuredly valid
                ! unless AuxiCovers, AColumn(....Key) is zero in SubtractColumn, so
                ! the resultant DColumn is the same as previously, so not InvObsolete
                  if(PrevKey==InvalidIndex) then
                    InvObsolete=AuxiCovers(ThisKey)
                  elseif(ThisKey==InvalidIndex) then
                    InvObsolete=AuxiCovers(PrevKey)
                  else
                    InvObsolete=AuxiCovers(PrevKey) &
                            .or.AuxiCovers(ThisKey)
                  end if
                end if
                if(InvObsolete) then
                  nDifferent=nDifferent+1
                  RowChanged=i
                  if(PrtDetail>1) write(PrtUni,'(1x,i3,2(1x,2z2),i3,a)') &
                    j,PrevBDIRow(j),PrevBKyVar(j), &
                    BasisDIRow(j),BasisKyVar(j), &
                    i,' row differs; matrix-inverse obsolete'
                end if
              end if
            end if
          end do
!         if(InvObsolete) then
          if(nDifferent>0) then
            if(PrtDetail>2) call ShowMat(True_Byte,True_Byte)
            if(nDifferent==1) then ! use the single-vector-changed algorithm
              call ModifyInverse(DInvRow,DColumn(1,RowChanged), &
                RowChanged,nAux)
            else
              call TransposeAndInvert
            end if
            if(PrtDetail>2) then
              TempLog1=(nDifferent>1)
              call ShowMat(FalseByte,TempLog1)
              call CheckProduct
            end if
          elseif(PrtDetail>1) then
            write(PrtUni,Fmt1a) 'non-key basis-structure'// &
              ' same as prior; matrix-inverse unchanged'
          end if
        end if
      ! PrevBDIRow=BasisDIRow ! call cmove(BasisDIRow,PrevBDIRow,I4_2nVar)
      ! PrevBKyVar=BasisKyVar ! call cmove(BasisKyVar,PrevBKyVar,I4_2nVar)
        call CopyLinIVec1(BasisDIRow,PrevBDIRow,nVar)
        call CopyLinIVec1(BasisKyVar,PrevBKyVar,nVar)

        if(PrtDetail>2) call ShowStates
      ! if(nAux>0) AuxDualMult=0.0 ! call cinitd(AuxDualMult,I4_nAux,0.0) ! called Pi by Bloom
        if(nAux>0) call initd1(AuxDualMult,nAux,0.0) ! called Pi by Bloom
        do j=1,nVar
          if(VarState(j)==BasicNKy) then ! using eqn 13
            k=FctOwning(j)
            if(k==InvalidFacet) then ! B0owns(j)
              if(PrtDetail>3) then
                call wnai(One2,j)
                call wnaa(' is in B0')
                write(PrtUni,Fmt71)cMod(j)
              end if
              if(nAux>0) call AddScalarMult(cMod(j), &
                DInvRow(1,BasisDIRow(j)),AuxDualMult,nAux)
            else ! BkOwns(j) for valid k
              if(PrtDetail>3) then
                call wnai(One2,j)
                call wnaa(' is in B')
                call wnai(Zer2,k)
                write(PrtUni,Fmt71)cMod(j)-cMod(KeyVar(k))
              end if
              if(nAux>0) call AddScalarMult(cMod(j)-cMod(KeyVar(k)), &
                DInvRow(1,BasisDIRow(j)),AuxDualMult,nAux)
            end if
            if(PrtDetail>3) then
              do i=1,nAux
                write(PrtUni,Fmt71)DInvRow(i,BasisDIRow(j))
              end do
              do i=1,nAux
                write(PrtUni,Fmt71)AuxDualMult(i)
              end do
              write(PrtUni,'(a)')' ADM'
            end if
          end if
        end do
        do k=1,nActiveFacets ! compute Phi(k) using equation 11
          jKey=KeyVar(k)
          Phi(k)=cMod(jKey)
          if(nAux>0) Phi(k)=Phi(k)-InnerProduct(AuxDualMult, &
            AColumn(1,jKey),nAux)
          if(k>1) DualVar(jPrev)=Phi(k-1)-Phi(k) ! p.13 facet slack's dual
          if(PrtDetail>3) then
            if(k>1) then
              write(PrtUni,'(1x,2i3,2f7.1,a,i3,2f7.1)') &
                k,jKey,cMod(jKey),Phi(k),' page 11', &
                jPrev,Phi(k-1),DualVar(jPrev)
            else
              write(PrtUni,'(1x,2i3,2f7.1,a)') & !  could use same format
                k,jKey,cMod(jKey),Phi(k),' page 11'
            end if
          end if
          jPrev=jKey
        end do
        DualVar(jKey)=Phi(nActiveFacets) ! the last facet includes UQ server

        if(PrtDetail>3) then
          call wnaa(' ')
          do j=1,nVar
            call wnai(One2,BasisDIRow(j))
          end do
          write(PrtUni,'(a)')' BasisDIRow vector'
          call wnaa(' ')
          do i=1,nAux
            write(PrtUni,Fmt71)AuxDualMult(i)
          end do
          write(PrtUni,'(a)')' ADM vector'
        end if
        do j=1,nVar
          if(VarState(j)==BasicNKy) then
            if(nAux>0) DualVar(j)=AuxDualMult(BasisDIRow(j))
            if(PrtDetail>3) write(PrtUni,'(1x,2i3,2f7.1,a)') &
              j,FctOwning(j),cMod(j),DualVar(j),' DualVar for BasicNKy'
          elseif(VarState(j)<BasicKey) then ! put reduced cost into DualVar
          ! if(nAux>0) AuxColumn=AColumn(:,j) ! call cmove(AColumn(1,j),AuxColumn,I4_4nAux)
            if(nAux>0) call CopyLinRVec1(AColumn(1,j),AuxColumn,nAux)
            k=FctOwning(j)
            if(k/=InvalidFacet) then ! using equation 21a
              if(PrtDetail>3) then
                write(PrtUni,'(1x,3i3,2f7.1)')j,k,KeyVar(k), &
                  cMod(j),cMod(KeyVar(k))
                call wnaa(' ')
                do i=1,nAux
                  write(PrtUni,Fmt71)AuxColumn(i)
                end do
                write(PrtUni,'(a)')' before SubCol'
              end if
              if(nAux>0)call SubtractColumn(KeyVar(k),AColumn,AuxColumn)
              if(PrtDetail>3) then
                call wnaa(' ')
                do i=1,nAux
                  write(PrtUni,Fmt71)AuxColumn(i)
                end do
                write(PrtUni,'(a)')' after  SubCol'
              end if
              DualVar(j)=cMod(j)-cMod(KeyVar(k))
            else
              DualVar(j)=cMod(j) ! using equation 21b
            end if
            if(PrtDetail>3) then
              if(k<=9) then
                i=2
              elseif(k<=99) then
                i=1
              else
                i=0
              end if
              call wnai(One2,j)
              call wnai(i,k)
              write(PrtUni,Fmt71)cMod(j)
              write(PrtUni,Fmt71)DualVar(j)
            end if
            if(nAux>0) DualVar(j)=DualVar(j)-InnerProduct &
              (AuxDualMult,AuxColumn,nAux)
            if(PrtDetail>3) then
              call wnaa(' ')
              do i=1,nAux
                write(PrtUni,Fmt71)AuxDualMult(i)
              end do
              do i=1,nAux
                write(PrtUni,Fmt71)AuxColumn  (i)
              end do
              write(PrtUni,'(f7.1,a)') &
                DualVar(j),' red cost for BoundNBa'
            end if
          end if
        end do
      return ! entry InvertAuxBasis


      entry ChooseDescentDirection
!       integer*2 jPrevOE ! used to break pathological cycling
!       real*4 OptDual

        if(PrtDetail>2) write(PrtUni,Fmt1a) &
          'Step1:  choose pivot variable having a cost-reducing dual'
        call LimitFacetSize ! to preclude slowness due to 2**n probes in GrayOrder
        jPrevOE=jOrdEnt
        jOrdEnt=InvalidIndex
        kLvg=InvalidFacet
        ! check (upper-bounded) facets' slacks for positive duals
        OptDual=SmallCost !SmallReal !0.0 ! scan for largest positive DualVar
        do k=kInf,nActiveFacets-1
          if(PrtDetail>3) write(PrtUni,'(1x,2i3,2f12.6,a)') &
            k,KeyVar(k),DualVar(KeyVar(k)),OptDual,' fSlk in CDD'
          if(OptDual<DualVar(KeyVar(k))) then
            if(PrtDetail>3) write(PrtUni,'(1x,2i3,2f12.6,a)') &
              k,KeyVar(k),DualVar(KeyVar(k)),OptDual,' kLvg in CDD'
            OptDual=NoiseFactor*DualVar(KeyVar(k))
            kLvg=k
          end if
        end do
        OrdVarEntering=kLvg==InvalidFacet ! facets' slacks are not ordinary
        if(OrdVarEntering) then ! no valid facet-index was assigned to kLvg above
          iOVE=1 ! Fortran requires array subscripts to be integer, not logical
          OptDual=0.0
          j=0
          do while(j<nVar)
            j=j+1
            if((VarState(j)<BasicKey).and.(.not.WouldBeZero(j))) then
            ! check duals of BoundNBa ordinary vars (not excluded as degenerates)
              if(AuxiCovers(j).and.AuxCycled(j)) cycle ! j previously entered,
              ! so break the cycling of aux vars from Basic to Bound to Basic etc.
              k=FctOwning(j)
              if(((VarState(j)==BoundNB0).and.(DualVar(j)<-OptDual)).or. &
                 ((VarState(j)==BoundNB1).and.(DualVar(j)> OptDual).and. &
                  (k>=kInf))) then
                if(PrtDetail>3) write(PrtUni,'(1x,i3,2f12.6,l2,a)') &
                  j,DualVar(j),x(j),AuxiCovers(j),' BoundNBa in CDD'
!               if((k<nActiveFacets).or.(x(KeyVar(nActiveFacets))>0.0)
!    +            .or.AuxiCovers(j)) then
                  jOrdEnt=j
                  if(BloomsPath) then ! end scan at the first qualifying BoundNBa
                    j=nVar
                  else ! scan for the largest magnitude (perhaps < SmallCost)
                    OptDual=NoiseFactor*abs(DualVar(j))
                  end if
              ! else key var (j in final facet) would limit entering var to 0
!               end if
              end if
            end if
          end do
        else
          iOVE=0
        end if
!       abort optimization in case of pathological cycling
        Optimal=(OrdVarEntering.and.((jOrdEnt==InvalidIndex) &
          .or.(jOrdEnt==jPrevOE))).or.(nIter>nAux*nVar)
!       if(Optimal) write(PrtUni,'(1x,a,5i3,l2)')'CDD end-critical vars ',
!      +  nAux,nVar,nIter,jPrevOE,jOrdEnt,OrdVarEntering
        if((.not.Optimal).and.OrdVarEntering.and.AuxiCovers(jOrdEnt)) &
          AuxCycled(jOrdEnt)=.true. ! jOrdEnt is ineligible for later re-entry
      return ! entry ChooseDescentDirection


!     here to FigureDescentExtent, program units are needed only thereby


      entry LimitFacetSize
!       integer*2 kLFS,nVarsInFct,nNB1sInFct,nVarsIFLim,BindSetLim,
!    +    BindSetVar(6)

!       nVarsIFLim=max0(6,nStgEqn+1) ! limit on # vars in 1 facet; no need for more
        nVarsIFlim=5 ! to limit the result of a merger to 10 vars, ignoring nStgEqn
        BindSetLim=min0(6,nVarsIFLim/2) ! when implicitly 1, need for split recurred
        do kLFS=kInf,nActiveFacets-1 ! allow final facet to contain many NB0's
          nVarsInFct=0
          nNB1sInFct=0
          jKey=KeyVar(kLFS)
          jBind=InvalidIndex
          do j=1,nVar
            if(FctOwning(j)==kLFS) then
              nVarsInFct=nVarsInFct+1
!             if(VarState(j)==BoundNB1) jBind=j
!             ! retain the index of the last bound NB1 var
              if((VarState(j)==BoundNB1).and. &
                (nNB1sInFct<BindSetLim)) then
              ! tentatively assign BoundNB1 vars to BindSet
                if(jBind==InvalidIndex) jBind=j ! index of the first bound NB1 var
                nNB1sInFct=nNB1sInFct+1
                BindSetVar(nNB1sInFct)=j
              end if
            end if
          end do
          if((nVarsInFct>nVarsIFLim).and.(jBind/=InvalidIndex)) then
          ! split this facet into two; make jBind key in the lower-order facet
          ! BindSetOwns=.false. ! call cinitb(BindSetOwns,I4_nVar,.false.)
            call initb1(BindSetOwns,nVar,FalseByte)
!           BindSetOwns(jBind)=.true. ! valid when BindSetLim was implictly 1
            do j=1,nNB1sInFct ! move as many as BindSetLim BoundNB1 vars to BindSet
              BindSetOwns(BindSetVar(j))=.true.
            end do
            kBind=kLFS
            if(PrtDetail>2) write(PrtUni,'(1x,i4,a,i4)') &
              nVarsInFct,' exceeds facet-size limit in facet ',kLFS
            call SplitBindingFacet(True_Byte) ! {kBind} into {BindSet,kBind-BindSet}
            KeyVar(kLFS)=jBind
            KeyVar(kLFS+1)=jKey
            VarState(jBind)=BasicKey ! state of all vars in facet k+1 is unchanged
            if(PrtDetail>2) call ShowStates
          end if
        end do
      return ! entry LimitFacetSize

      entry FindMostRestrictive
!       character InfChr ! useful only if PrtDetail>0
!       character*10 StrRHS ! internal file useful only if PrtDetail>0
!       ! index number of the limiting inequality, according to Bloom's paper
!       integer*2 LimitingIneq(0:2,0:1)
!       data LimitingIneq/18,19,0,24,25,27/
!       logical*1 OrdVarFinalFct,FctVoidOfDj
!       integer*2 jLoadedLast,kPrior0,kPrior1,iMSb,iMarginal,nMarginal,
!    +    jMarginal(:)
!       integer*4 HalfSize,GrayOrder(:),HoldOrder(:), ! temporary storage
!    +    SupiGr, ! may grow as large as LimFacetSize
!    +    iGray,fGray,
!    +    PrevBitPattern,
!    +    ThisBitPattern,
!    +    DiffBitPattern,
!    +    InclBitPattern ! ,
!     !   FullBitPattern ! large enough for 32 vars
!      !real*4 PrevCapac,CapIncr,AccumArea,AccumXInSet,AccumDj
!       real*8 PrevCapac,AccumArea,AccumXInSet,AccumDj
!       real*4 CapIncr

        if(.not.allocated(jMarginal)) then
        ! allocate(jMarginal(SupB2LogSize),stat=AS)
          allocate(jMarginal(LimUnknowns),stat=AS) ! need to count even if FctVoidOfDj
          call CkAllocSt('jMgn',0)
          SupiGr=1 ! subject to upward revision in AugmentOrderGray
          allocate(GrayOrder(0:SupiGr),stat=AS)
          call CkAllocSt('Gray',0)
          GrayOrder(0)=0
          GrayOrder(1)=1
        end if
        if(PrtDetail>2) then
          call ShowStates
          write(PrtUni,Fmt1a) &
            'find the most restrictive potentially-active facet:'
          write(PrtUni,Fmt1a) '  n iGr jV Hex PrCap  GenCapac'// &
            '  AccumArea  AccumXinS AccmDj       RHS kF BindSet'
        end if
      ! BindSetOwns=.false. ! call cinitb(BindSetOwns,I4_nVar,.false.) ! the most restrictive potential set
        call initb1(BindSetOwns,nVar,FalseByte) ! the most restrictive potential set
        InfRHS=LargeArea
        kBind=InvalidFacet ! index of most restrictive Psik
        jBind=InvalidIndex ! useful only if PrtDetail>0
        xLoadPoint=xLoadPtInf ! abscissa of first var to be considered in facet kInf
        call CopyEqLoad0(True_Byte) ! with k<kInf plant-outage effects
        call CopyEqLoad(FalseByte) ! save initial values for later reference
        do k=kInf,nActiveFacets
          if(k>kInf) call CopyEqLoad(True_Byte) ! restore values at end of last k-iteration
        ! InitSort becomes false immediately after LimitCondit/=(24 or 25 or 27)
          OrdVarFinalFct=InitSort.and.OrdVarEntering.and. &
            (k==nActiveFacets).and.(FctOwning(jOrdEnt)==k) ! not all ordinary vars enter from final facet
          if((k>kInf).and.(OrdVarEntering.or.(k/=kLp1))) then
            if(OrdVarEntering.or.(k/=kLp1+1)) then
              kPrior0=k-1
            else
              kPrior0=k-2
            end if
            kPrior1=k-1
            do j=1,nVar ! consider any BoundNBa var at its GLB as not (fully) loaded;
              ! note that all such vars should have FctOwning(j)==nActiveFacets (?)
              if((kPrior0<=FctOwning(j)).and.(FctOwning(j)<=kPrior1)) &
                then
!               if(VarState(j)==BoundNB0) then
!                 if(.not.Nearly0(LUB(j),0)) ! incr xLoadPoint by derated capacity
!    +            xLoadPoint=xLoadPoint+Capacity(j)*x(j)/LUB(j) ! x(j) may be zero
!               else
                  xLoadPoint=xLoadPoint+Capacity(j)
!                 if(PrtDetail>2) write(PrtUni,'(1x,a,2i3,3f13.3)')
!    +              'xLP ',k,j,Capacity(j),xLoadPoint,EqLCumul(1)
!               end if
              end if
            end do
          end if
          PrevCapac=xLoadPoint
          AccumArea=0.0

          if(OrdVarFinalFct) then
            FctVoidOfDj=.false. ! RHS of final facet needs to be computed
            nMarginal=0
          else
            if(k/=kLp1) then
              FctVoidOfDj=.true. ! unless at least one d(j) within facet is nonzero
              nMarginal=0 ! else effectively merge facets kLvg,kLp1
          ! else retain FctVoidOfDj and nMarginal from the prior iteration of k-loop
            end if
            j=1
            do while(FctVoidOfDj.and.(j<=nVar))
            ! avoid reallocating GrayOrder if FctVoidOfDj
              if((FctOwning(j)==k).and.(d(j)/=0.0)) FctVoidOfDj=.false.
              j=j+1
            end do
          end if

          do j=1,nVar ! nMarginal is conceivably as large as LimAuxEquations+1
            if(FctOwning(j)==k) then
              if((nMarginal>=Base2LogSize).and.(.not.OrdVarFinalFct) &
                .and.(.not.FctVoidOfDj)) then ! RHS of various facets needs be computed
                if(Base2LogSize<SupB2LogSize) then
                ! call AugmentOrderGray(Base2LogSize) ! inline to allow reallocation
                ! fill the array GrayOrder with indices that differ in only 1 bit
                  allocate(HoldOrder(0:SupiGr),stat=AS)
                  call CkAllocSt('Hold',0)
                ! HoldOrder=GrayOrder ! call cmove(GrayOrder,HoldOrder,4*(1+SupiGr))
                  call CopyLinRVec0(GrayOrder,HoldOrder,1+SupiGr) ! copy as if real*4
                ! hold the prior array data until space is allocated
                  HalfSize=ishft(1,Base2LogSize)
                  SupiGr=HalfSize+HalfSize-1
                  deallocate(GrayOrder,stat=AS)
                  call CkAllocSt('Gray',1)
                  allocate(GrayOrder(0:SupiGr),stat=AS)
                  call CkAllocSt('Gray',0)
                ! GrayOrder(0:HalfSize-1)=HoldOrder ! call cmove(HoldOrder,GrayOrder,4*HalfSize)
                  call CopyLinRVec0(HoldOrder,GrayOrder,HalfSize) ! copy as if real*4
                  deallocate(HoldOrder,stat=AS)
                  call CkAllocSt('Hold',1)
                  do iGray=1,HalfSize
                    GrayOrder(HalfSize+iGray-1)=ior( &
                    GrayOrder(HalfSize-iGray),HalfSize)
!                   GrayOrder(HalfSize+iGray-1)= ! since lower half of GrayOrder<HalfSize ...
!    +              GrayOrder(HalfSize-iGray)+HalfSize ! ... there's no carries
                  end do
                  Base2LogSize=Base2LogSize+1
                  if(PrtDetail>1) write(PrtUni,'(1x,a,i2)') &
                    'size of GrayOrder:  2**',Base2LogSize
!                 return ! entry AugmentOrderGray
                else
                  call HaltWith(k,nMarginal,'facet '// &
                    '@FindMostRestrictive count of marginal vars: ')
                end if
              end if
!             if(.not.((k==kLp1).and.(k==nActiveFacets).and.(VarState(j)==BoundNB0)
!    +          .and.Nearly0(x(j)-GLB(j),1))) then ! skip vars bound at GLB in merger
!             if(.not.((.not.InitSort).and.(k==nActiveFacets).and.
!    +          (VarState(j)==BoundNB0).and.Nearly0(x(j),1))) then
!             ! skip vars bound at 0 in final facet after InitSort (to save time)
              if(AuxiCovers(j).or.(k<nActiveFacets).or. &
                (VarState(j)/=BoundNB0).or.(j==jOrdEnt)) then
              ! skip non-jOrdEnt, non-aux vars bound at 0 in final facet (save time)
                nMarginal=nMarginal+1
!               if(PrtDetail>2) write(PrtUni,'(1x,a,5i4,2l2)')
!    +            'jMarginal ',j,FctOwning(j),nMarginal,Base2LogSize,
!    +            SupB2LogSize,OrdVarFinalFct,FctVoidOfDj
                if(.not.OrdVarFinalFct) jMarginal(nMarginal)=j
              end if
            end if
          end do

!         if(PrtDetail>2) write(PrtUni,'(1x,4i4,2l2)')
!    +      nActiveFacets,k,kLvg,nMarginal,OrdVarEntering,FctVoidOfDj

          if((nMarginal>1).and.(.not.FctVoidOfDj).and.(OrdVarEntering &
            .or.(k/=kLvg))) then
          ! since the order of vars within a facet is not important, one must
          ! investigate all possible non-void, non-full combinations of vars;
          ! e.g., if nMarginal=5 then there are (2^5)-2=30 cases to examine;
          ! we follow Bloom's paper in using the Gray order to exhaustively
          ! cover all cases with minimal effort in computing TailUQ.
            PrevBitPattern=0
            AccumXInSet=0.0
            AccumDj=0.0
          ! TestSetOwns=.false. ! call cinitb(TestSetOwns,I4_nVar,.false.)
            call initb1(TestSetOwns,nVar,FalseByte)
            if(OrdVarFinalFct) then ! avoid wasting time searching 2^nMarginal space
              fGray=2 ! as if nMarginal were 1
!             FullBitPattern=2 ! anything but 1, thus unmatchable by ThisBitPattern
            else
              fGray=ishft(1,nMarginal)
!             FullBitPattern=fGray-1
            end if
            do iGray=2,fGray ! skip the case with BindSet null at iGray==1
              ThisBitPattern=GrayOrder(iGray-1)
              if(OrdVarFinalFct) then
              ! determine TestSetOwns using the fact that the final marginal facet
              ! has the UQ server as BasicKey, return vars (only initially) as
              ! BasicNKy, and all other vars as BoundNBa at their GLB
                CapIncr=0.0
                if(UsingCumul) then ! find area from PrevCapac to infinity
                  call FindTailArea(sngl(PrevCapac))
                  AccumArea=AccumArea+TailUQ
                end if
                do j=1,nVar
!                  if((FctOwning(j)==nActiveFacets).and.
!    +               ((j==jOrdEnt).or.(VarState(j)==BasicNKy))) then
!                  if((FctOwning(j)==nActiveFacets).and.
!    +               ((j==jOrdEnt).or.((VarState(j)==BasicNKy).and.(.not.
!    +               (Nearly0(x(j)-LUB(j),1).or.Nearly0(x(j)-GLB(j),1)))))) then
!                 if((FctOwning(j)==nActiveFacets).and.
!    +              ((j==jOrdEnt).or.((VarState(j)==BasicNKy).and.
!    +              (.not.Nearly0(x(j)-GLB(j),1))))) then
                  if((FctOwning(j)==nActiveFacets).and. &
                    ((j==jOrdEnt).or.AuxiCovers(j))) then
                    TestSetOwns(j)=.true.
                    CapConv=Capacity(j)
                    call ChkPriorBlock(One2,j,k,FalseByte,FalseByte)
                    if(PriorBlockLoaded) & !  first remove effects of its outages
                      call ExcludeOutages(Capacity(jPrBk),jPrBk,jPrBk)
                    AccumDj=AccumDj+d(j)
                    AccumXInSet=AccumXInSet+x(j)
!                   if(PrtDetail>2) write(PrtUni,'(1x,i3,5f10.3,a)')
!    +                j,Capacity(j),PrevCapac,CapIncr,EqLCumul(1),
!    +                sqrt(EqLCumul(2)),' OVFF'
                    if(UsingCumul) then ! no need to find area for every load point
!                     call FindTailArea(sngl(PrevCapac+CapIncr))
!                     AccumArea=AccumArea+TailUQ
                      CapIncr=CapIncr+Capacity(j)
                      call IncludeOutages(CapConv,j,jBlkPair)
!                     call FindTailArea(sngl(PrevCapac+CapIncr))
!                     AccumArea=AccumArea-TailUQ
                    else
                      call FindAreaBetween(sngl(PrevCapac+CapIncr), &
                        Capacity(j))
                      call IncludeOutages(CapConv,j,jBlkPair)
                      AccumArea=AccumArea+EquivAvail(j)*TailUQ
                      CapIncr=CapIncr+Capacity(j)
                    end if
                  end if
                end do
                if(UsingCumul) then ! find final area to infinity
                  call FindTailArea(sngl(PrevCapac+CapIncr))
                  AccumArea=AccumArea-TailUQ
                end if
                j=jOrdEnt ! useful only if PrtDetail>2
              else
                DiffBitPattern=ieor(PrevBitPattern,ThisBitPattern)
                iMarginal=1
                do while((iMarginal<nMarginal).and.(DiffBitPattern/= &
                  ishft(1,iMarginal-1)))
                  iMarginal=iMarginal+1
                end do
                j=jMarginal(iMarginal)
                CapConv=Capacity(j)
                if(ThisBitPattern>PrevBitPattern) then ! # in set increases
                  TestSetOwns(j)=.true.
                  CapIncr= CapConv
                  AccumDj=AccumDj+d(j)
                  AccumXInSet=AccumXInSet+x(j)
                else ! # in set decreases
                  TestSetOwns(j)=.false.
                  CapIncr=-CapConv
                  AccumDj=AccumDj-d(j)
                  AccumXInSet=AccumXInSet-x(j)
                end if
                if(UsingCumul) then
!                 if(PrtDetail>2) write(PrtUni,'(1x,a,2f11.4,4e14.6)')
!    +              'Kap',EqLCumul(1),sqrt(EqLCumul(2)),
!    +              (EqLCumul(j2),j2=3,nKappa)
                  ! on first iGray iteration, find area from PrevCapac to infinity
                  if(iGray==2) call FindTailArea(sngl(PrevCapac))
!                 else use the prior TailUQ value computed after de/convolution
                  AccumArea=AccumArea+TailUQ
                  call ChkPriorBlock(One2,j,k,True_Byte,FalseByte) ! even if in facet k
                  if(TestSetOwns(j).eqv..true.) then
                    if(PriorBlockLoaded) & !  first remove effects of its outages
                      call ExcludeOutages(Capacity(jPrBk),jPrBk,jPrBk)
                    call IncludeOutages(CapConv,j,jBlkPair)
                  else ! var j is being deleted; CapIncr<0; exclude var j's outages
                    call ExcludeOutages(CapConv,j,jBlkPair)
                    if(PriorBlockLoaded) & !  restore effects of its outages
                      call IncludeOutages(Capacity(jPrBk),jPrBk,jPrBk) ! added 980809
                  end if
                  call FindTailArea(sngl(PrevCapac+CapIncr))
                  AccumArea=AccumArea-TailUQ
                else ! not UsingCumul
                  if(TestSetOwns(j).eqv..true.) then
                    InclBitPattern=ThisBitPattern
                  else
                    InclBitPattern=PrevBitPattern ! retain the bit for var j deleted
                  end if
                  iMSb=nMarginal ! 1+index of MSb of largest possible BitPattern
                  do while((iMSb>1).and. &
                    (iand(InclBitPattern,ishft(1,iMSb-1))==0))
!     +              (mod(InclBitPattern/2**(iMSb-1),2)==0))
!                   if(PrtDetail>2) write(PrtUni,'(1x,2i4,1x,z4,a)')
!    +                nMarginal,iMSb,2**(iMSb-1),' in loop'
                    iMSb=iMSb-1
                  end do ! with iMSb=1+index of MSb of InclBitPattern
                  jLoadedLast=jMarginal(iMSb)
!                 if(PrtDetail>2) write(PrtUni,'(1x,2i4,1x,z4,a)')
!    +              jLoadedLast,iMSb,ishft(1,iMSb-1),' ex loop'
                ! include outages independently of the order of selection of vars:
                ! include in equivalent load outages of all vars but jLoadedLast,
                ! with the implicit assumption that vars within a facet-margin are
                ! loaded in order of increasing j
                  call CopyEqLoad(True_Byte) ! restore values at end of last k-iteration
                  do j2=1,nVar
                    if(((j2/=jLoadedLast).and.     TestSetOwns(j2)))then ! .or.
!    +                 ((j2==j)          .and..not.TestSetOwns(j )))then
!                     if(PrtDetail>2) write(PrtUni,'(1x,2i4,l2,f9.2,4f11.4,a)')
!    +                  jLoadedLast,j2,TestSetOwns(j2),Capacity(j2),
!    +                  VarCumul(1,j2),sqrt(VarCumul(2,j2)),
!    +                  EqLCumul(1)   ,sqrt(EqLCumul(2)),' j2 loop'
                      call ChkPriorBlock(One2,j2,k,True_Byte,FalseByte) ! even if in facet k ...
                      if(PriorBlockLoaded) & !  first remove effects of its outages
                        call ExcludeOutages(Capacity(jPrBk),jPrBk,jPrBk)
                      call IncludeOutages(CapConv,j2,jBlkPair)
                    end if
                  end do
                  call FindAreaBetween(sngl(PrevCapac),CapIncr)
                  AccumArea=AccumArea+EquivAvail(j)*TailUQ
                end if
              end if
!             if((AccumDj>SmallReal).and.(ThisBitPattern/=FullBitPattern)) then
              ! skip the full-bit case, corresponding to a prior facet ! dubious
              if(AccumDj>SmallReal) then
                RHS=(AccumArea-AccumXInSet)/AccumDj
!               if((InfRHS>RHS*NoiseFactor).and.(RHS>=0.0)) then
                if((InfRHS>RHS*NoiseFactor).and.(RHS>=-SmallReal)) then
!                 phrase '.and.(RHS>=0.0)' rejected negative noise as a valid
!                 lower bound when it should have interpreted it as zero
                  InfRHS=amax1(RHS,0.0) ! = above allows degeneracy but not regression
                ! BindSetOwns=TestSetOwns ! call cmove(TestSetOwns,BindSetOwns,I4_nVar) ! most restrictive on kLvg's slack
                  call CopyLinLVec1(TestSetOwns,BindSetOwns,nVar)
                  kBind=k ! index of facet to be merged or split
                  InfChr='*'
                else
                  InfChr=' '
                end if
              else ! RHS was not computed
                InfChr='-'
              end if
!             if((PrtDetail>3).or.((PrtDetail>2).and.(InfChr/='-')))then
              if( PrtDetail>2) then
                if(InfChr=='-') then
                  StrRHS='          ' ! 0 is misleading
                else
                  write(StrRHS,'(f10.6)')RHS
                end if
                write(PrtUni, &
                  '(1x,i3,i4,i3,z4,i6,f10.3,2e11.4,f7.3,a,i3,1x,2a\)') &
                  nMarginal,iGray,j,ThisBitPattern,PrevCapac,CapIncr, &
                  AccumArea,AccumXInSet,AccumDj,StrRHS,k,InfChr,'{'
                FirstItem=.true.
                if(OrdVarFinalFct) then
                  do j=1,nVar
                    if(TestSetOwns(j).eqv..true.) then
                      if(FirstItem) then
                        call wnai(Zer2,j)
                      else
                        call wnai(One2,j)
                      end if
                      FirstItem=.false.
                    end if
                  end do
                else ! display vars in order of loading during probing
                  do iMarginal=1,nMarginal
                    j=jMarginal(iMarginal)
                    if(TestSetOwns(j).eqv..true.) then
                      if(FirstItem) then
                        call wnai(Zer2,j)
                      else
                        call wnai(One2,j)
                      end if
                      FirstItem=.false.
                    end if
                  end do
                end if
                write(PrtUni,'(a)')'}'
              end if
              PrevCapac=PrevCapac+CapIncr
              PrevBitPattern=ThisBitPattern
            end do ! loop on iGray
!         elseif((k<nActiveFacets).and.(k/=kLvg)) then
          end if
          if((k<nActiveFacets).and.(k/=kLvg)) then
          ! outages must be incorporated in equivalent load for next iteration on k
            call CopyEqLoad(True_Byte) ! restore values at end of last k-loop iteration
            do iMarginal=1,nMarginal
              j=jMarginal(iMarginal)
              CapConv=Capacity(j)
              call ChkPriorBlock(One2,j,k,True_Byte,FalseByte) ! last arg is irrelevant
              if(PriorBlockLoaded) & !  first remove effects of its outages
                call ExcludeOutages(Capacity(jPrBk),jPrBk,jPrBk)
              ! include outage of block-pair
              call IncludeOutages(CapConv,j,jBlkPair) ! PrevCapac=xLoadPoint above
            end do
            call CopyEqLoad(FalseByte) ! save values at end of k-loop iteration
          end if
        end do ! k loop

!       deallocate(jMarginal,stat=AS) ! not when using save
!       call CkAllocSt('jMgn',1)
!       deallocate(GrayOrder,stat=AS)
!       call CkAllocSt('Gray',1)

        LimitCondit=LimitingIneq(0,iOVE)
        if(PrtDetail>2) write(PrtUni,Fmt1a) &
          'limit change by imposing each var''s upper and lower bounds:'
        do j=1,nVar
          if(((.not.OrdVarEntering).or.(j/=jOrdEnt)).and. &
            (.not.Nearly0(d(j),0))) then
            if(d(j)>0.0) then
              RHS=LUB(j)
            else ! d(j)<0.0
              RHS=GLB(j)
            end if
            if(PrtDetail>3) write(PrtUni,'(1x,i3,5e13.6)') &
              j,GLB(j),LUB(j),RHS,x(j),d(j)
            RHS=(RHS-x(j))/d(j)
!           if((InfRHS>RHS*NoiseFactor).and.(RHS>=0.0)) then
!           if((InfRHS>RHS*NoiseFactor).and.(RHS>=-SmallArea)) then
            if((InfRHS>RHS).and.(RHS>=-SmallArea)) then
              if(InfRHS>RHS*NoiseFactor) then ! prefer retaining #18/24 over #19/25
!             if((InfRHS>RHS*NoiseFactor).and.((.not.OrdVarEntering).or.
!    +          (RHS>SmallArea).or.AuxiCovers(jOrdEnt)) then
!             ! prefer retaining #18/24 over #19/25
!             ! non-aux jOrdEnt may not enter basis as BasicKey at 0, nor may it
!             ! become BasicNKy, yet only BasicNKy may be degenerate
!             above restriction is now incorporated in array WouldBeZero
                LimitCondit=LimitingIneq(1,iOVE)
                jBind=j ! useful in cases 19 & 25 below
              end if
              InfRHS=amax1(RHS,0.0) ! interpret small negative noise as zero
              InfChr='*'
            else
              InfChr=' '
            end if
            if(PrtDetail>2) write(PrtUni,'(1x,2i3,2f14.6,a)') &
              j,LimitCondit,x(j),RHS,InfChr
          end if
        end do
        if(OrdVarEntering) then
          RHS=LUB(jOrdEnt)-GLB(jOrdEnt) ! always positive
          if(InfRHS>RHS) then ! prefer retaining #24/25 over #27
            if(InfRHS>RHS*NoiseFactor) LimitCondit=LimitingIneq(2,iOVE)
            InfRHS=RHS
            InfChr='*'
          else
            InfChr=' '
          end if
          if(PrtDetail>2) write(PrtUni,'(1x,2i3,2f14.6,a)') &
            jOrdEnt,LimitCondit,GLB(jOrdEnt),LUB(jOrdEnt),InfChr
        end if
      return ! entry FindMostRestrictive


      entry FigureDescentExtent
!       real*4 Alpha

!       get d(j), the 1st derivative of x(j) with respect to x(jOrdEnt or jKey);
!       any motion within basis is linear, so all higher-order derivatives vanish
      ! d=0.0 ! call cinitd(d,I4_nVar,0.0) ! default
        call initd1(d,nVar,0.0) ! default
        if(PrtDetail>2) write(PrtUni,Fmt1a) 'Step2:  '// &
          'limit extent by that bound or facet which is most binding'

        if(OrdVarEntering) then
          if(PrtDetail>1) write(PrtUni,'(1x,a,i3,a)') &
            'd[j] for OrdVar',jOrdEnt,' entering:'
          kLp1=InvalidFacet
          ! GLB is not always 0, so the test must be re GLB:
          if(Nearly0(x(jOrdEnt)-GLB(jOrdEnt),1)) then
            Alpha= 1.0
          else
            Alpha=-1.0
          end if
          d(jOrdEnt)=Alpha ! using equation 21c
          k=FctOwning(jOrdEnt)
          if(nAux>0) &
        !+  call cmove(AColumn(1,jOrdEnt),AuxColumn,I4_4nAux) ! default valid for jOrdEnt in Psi0
        !+  AuxColumn=AColumn(:,jOrdEnt) ! default valid for jOrdEnt in Psi0
            call CopyLinRVec1(AColumn(1,jOrdEnt),AuxColumn,nAux) ! default valid for jOrdEnt in Psi0
          if((k/=InvalidFacet).and.(nAux>0)) &
            call SubtractColumn(KeyVar(k),AColumn,AuxColumn)

          do j=1,nVar
            if(VarState(j)==BasicNKy) then ! using equations 22ff
              if(PrtDetail>3) then
                call wnaa(' ')
                do iAux=1,nAux
                  write(PrtUni,Fmt71)DInvRow(iAux,BasisDIRow(j))
                end do
                do iAux=1,nAux
                  write(PrtUni,Fmt71)AuxColumn(iAux)
                end do
              end if
              if(nAux>0) d(j)=-Alpha*InnerProduct &
                (DInvRow(1,BasisDIRow(j)),AuxColumn,nAux)
              if(PrtDetail>3) write(PrtUni,'(3i3,f9.3,a)') &
                k,BasisDIRow(j),j,d(j),' aux d[j] in 22ff'
            end if
          end do

          do k=kInf,nActiveFacets ! compute d(k) using equations 23ff
            jKey=KeyVar(k)
            do j=1,nVar
              if((FctOwning(j)==k).and.(VarState(j)==BasicNky)) &
                d(jKey)=d(jKey)-d(j)
            end do ! BkOwns(j)
            if(FctOwning(jOrdEnt)==k) d(jKey)=d(jKey)-Alpha !incorporate ëkn
            if(PrtDetail>3) write(PrtUni,'(1x,4i3,f9.3,a)') &
              k,jOrdEnt,FctOwning(jOrdEnt),jKey,d(jKey), &
              ' facet dk in 23ff'
          end do
!         end re d() for ordinary (not facet-slack) var entering basis

        else ! facet-variable's slack enters basis
          if(PrtDetail>1) write(PrtUni,'(1x,a,i3,a)') &
            'd[j] for facet',kLvg,' slack entering:'
          kLp1=kLvg+1
          if(nAux>0) & 
         ! +  AuxColumn=AColumn(:,KeyVar(kLvg)) ! call cmove(AColumn(1,KeyVar(kLvg)),AuxColumn,I4_4nAux)
            call CopyLinRVec1(AColumn(1,KeyVar(kLvg)),AuxColumn,nAux)
          if(nAux>0) call SubtractColumn(KeyVar(kLp1),AColumn,AuxColumn)

          do j=1,nVar
            if(VarState(j)==BasicNKy) then ! using equation 16a
              if(PrtDetail>3) then
                call wnaa(' ')
                do iAux=1,nAux
                  write(PrtUni,Fmt71)DInvRow(iAux,BasisDIRow(j))
                end do
                do iAux=1,nAux
                  write(PrtUni,Fmt71)AuxColumn(iAux)
                end do
              end if
              if(nAux>0) d(j)=InnerProduct &
                (DInvRow(1,BasisDIRow(j)),AuxColumn,nAux)
              if(PrtDetail>3) write(PrtUni,'(1x,3i3,f9.3,a)') &
                kLp1,BasisDIRow(j),j,d(j),' aux d[j] in 16a'
            end if
          end do

          do k=kInf,nActiveFacets ! compute d(k) using equation 17a
            jKey=KeyVar(k)
            do j=1,nVar
              if((FctOwning(j)==k).and.(VarState(j)==BasicNky)) &
                d(jKey)=d(jKey)-d(j)
            end do ! BkOwns(j)
            if(k==kLvg) then
              d(jKey)=d(jKey)-1.0
            elseif(k==kLp1) then
              d(jKey)=d(jKey)+1.0
            end if ! incorporate -ëkl
            if(PrtDetail>3) write(PrtUni,'(1x,2i3,f9.3,a)') &
              k,jKey,d(jKey),' facet dk in 17a'
          end do
        ! end re d() for facet-slack var entering basis
        end if

        if(PrtDetail>1) then
          do j=1,nVar
            if(.not.Nearly0(d(j),0)) then
              call wnaa(' d')
              call wnai(Zer2,j)
              call wnaa('=')
              write(PrtUni,'(f7.3\)')d(j)
            end if
          end do
          write(PrtUni,'(a)')' [others are 0]'
        end if
!       impose the most-binding of potentially active constraints:
!       call LimitFacetSize ! to preclude slowness due to 2**n probes in GrayOrder
        call FindMostRestrictive
      return ! entry FigureDescentExtent


!     here to ReviseBasisVectors, program units are needed only thereby


      entry FindAuxComplement(jOrg,jCompl)
!       return the index of the first non-jOrg var in one of the aux constraints
!       covering jOrg, having PrevState unequal that of jOrg
!       integer*2 jOrg,jCompl,PrStjOrg
!       character*46 NoCompl
!       parameter(NoCompl='var has no aux complement in state unequal to')

        if(AuxiCovers(jOrg).eqv..true.) then
          PrStjOrg=PrevState(jOrg) ! valid state before any changes were made
          jCompl=InvalidIndex
          iAux=0
          do while(.true.)
            iAux=iAux+1
            if(AuxCovers(iAux,jOrg).eqv..true.) then
              j=1
              do while((j<=nVar).and.((j==jOrg).or.( &
                .not.(AuxCovers(iAux,j).and.(PrevState(j)/=PrStjOrg)))))
                j=j+1
              end do
              if(j<=nVar) jCompl=j ! a complement in same constraint as jOrg
            end if
            if((iAux>=nAux).or.(jCompl/=InvalidIndex)) exit
          end do
          if(jCompl>nVar) call HaltWith(jOrg,PrStjOrg,NoCompl)
        else
          call HaltWith(jOrg,PrStjOrg,NoCompl)
        end if
      return ! entry FindAuxComplement


      entry MergeLeavingFacet
!       integer*2 jUnionKey

!       decision re k is not arbitrary, as a non-aux var cannot be BasicNKy
        k=0
        do while((k<2).and.(.not.AuxiCovers(KeyVar(kLvg+k))))
          k=k+1
        end do
!       if((k==2).and.(nAux>0)) call HaltWith(kLvg,kLp1,
!    +    'neither facet has an aux var key')
        if((k==2).and.(PrtDetail>2)) write(PrtUni,'(1x,a,2i4)') &
          'neither facet has an aux var as key',kLvg,kLp1
        if((nAux>0).and.(k<2)) then
          jNewlyNKy=KeyVar(kLvg+k)
          jUnionKey=KeyVar(kLp1-k)
          KeyVar(kLvg)=jUnionKey ! move former key to the new kLvg union
          VarState(jNewlyNKy)=BasicNKy ! make former key newly BasicNKy
          if(PrtDetail>2) then
            write(PrtUni,'(1x,3i3,a)')jNewlyNKy,jUnionKey,k,' MLF1'
            call ShowStates
          end if
        elseif(k==2) then ! jBind always becomes BoundNBa under #19
          jNewlyNKy=InvalidIndex ! preclude later attempts to juggle aux vars
          if(kBind==kLvg) then ! make jLp1 BasicKey
          ! VarState(jLp1)=BasicKey ! redundant
            call MakeBound(jLvg) ! perhaps redundant to jBind
            KeyVar(kLvg)=jLp1
          else ! kBind==kLp1 => KeyVar(kLvg) is retained
            call MakeBound(jLp1) ! perhaps redundant to jBind
          end if
        elseif(kBind==kLvg) then ! (nAux==0) and (k<2)
          jNewlyNKy=KeyVar(kLvg) ! eventually assigned to BoundNBa
          KeyVar(kLvg)=KeyVar(kLp1)
        else
          jNewlyNKy=KeyVar(kLp1) ! KeyVar(kLvg) is retained
        end if

        if(kBind>kLvg) kBind=kBind-1
        call DeleteFacet(kLp1)
        if(PrtDetail>2) then
          write(PrtUni,Fmt1a)'MLF2'
          call ShowStates
        end if
      return ! entry MergeLeavingFacet


      entry PartReBindSetFacets(k0,k1) ! assumes k1=k0+1
!       exclude BindSet from union (k1) of 2 facets, placing it into k0
!       integer*2 k0,k1

        do j=1,nVar
          if((k0==FctOwning(j)).or.(FctOwning(j)==k1)) then
            if(BindSetOwns(j)) then
              FctOwning(j)=k0
            else
              FctOwning(j)=k1
            end if
          end if
        end do
        ! KeyVar is as yet indeterminate for these two facets
        KeyVar(k0)=InvalidIndex ! needed when LimitCondit=18
        KeyVar(k1)=InvalidIndex
      return ! entry PartReBindSetFacets


      entry SplitBindingFacet(Partition)
!       logical*1 Partition

        if(PrtDetail>2) write(PrtUni,'(1x,a,3i4)') &
          'SBF ',jBind,kBind,nActiveFacets
        nActiveFacets=nActiveFacets+1
        do k=nActiveFacets,kBind+2,-1 ! shift to open
          KeyVar(k)=KeyVar(k-1)
        end do
        do j=1,nVar ! shift to open
          if(FctOwning(j)>kBind) FctOwning(j)=FctOwning(j)+1
        end do
        if(Partition) call PartReBindSetFacets(kBind,kBind+One2) ! split union of facets into two
      return ! entry SplitBindingFacet


      entry IntersectWithBNK(kIWBnk)
!       integer*2 kIWBnk,jIWB

!       BkOwns(jIWB) iff FctOwning(jIWB)==kIWBnk and VarState(jIWB)==BasicNKy
      ! BkOwns=.false. ! call cinitb(BkOwns,I4_nVar,.false.)
        call initb1(BkOwns,nVar,FalseByte)
        do jIWB=1,nVar
          if((FctOwning(jIWB)==kIWBnk).and.(VarState(jIWB)==BasicNKy)) &
            BkOwns(jIWB)=.true.
        end do
!       do jIWB=1,nVar ! possibly faster than code above
!         BkOwns(jIWB)=(FctOwning(jIWB)==kIWBnk).and.(VarState(jIWB)==BasicNKy)
!       end do
      return ! entry IntersectWithBNK


      entry FindFirstBNKnot(jSkip,jBnk)
!       return the index of the first var (not jSkip) that BkOwns
!       integer*2 jSkip,jBnk

        jBnk=1
        do while &
          ((jBnk<=nVar).and.(.not.((jBnk/=jSkip).and.BkOwns(jBnk))))
          jBnk=jBnk+1
        end do ! return with jBnk<=nVar iff BkOwns some var other than jSkip
      return ! entry FindFirstBNKnot


      entry FindNewlyKeyAuxIn(kFctNKAI,jNKyToBe,AnyConstr)
!       return the index of a basic non-key var in facet kFctNKAI,
!       insisting on one which complements var jNKyToBe (if not AnyConstr)
!       integer*2 kFctNKAI,jNKyToBe,kNKyToBe
!       logical*1 AnyConstr
!       character*36 NoBNK
!       parameter(NoBNK='facet contains no basic non-key var')

        call IntersectWithBNK(kFctNKAI)
        jAux=InvalidIndex
        iAux=0
        do while(.true.) ! search for a BasicNKy in same aux constraint as jNKyToBe
          iAux=iAux+1
          if(AuxCovers(iAux,jNKyToBe).eqv..true.) then
            j2=1
            do while((j2<=nVar).and.((j2==jNKyToBe).or.(.not. &
              (AuxCovers(iAux,j2).and.BkOwns(j2)))))
              j2=j2+1
            end do
            if(j2<=nVar) jAux=j2 ! BasicNKy in same constraint as jNKyToBe
          end if
          if((iAux>=nAux).or.(jAux/=InvalidIndex)) exit
        end do
        if(PrtDetail>2) then
          write(PrtUni,'(1x,a,3i3)')'NKAI1',kFctNKAI,jAux,jNKyToBe
          call ShowStates
        end if

        if(jAux==InvalidIndex) then
          if(AnyConstr) then
            ! allow any BasicNKy in kFctNKAI to become key
            call IntersectWithBNK(kFctNKAI)
            call FindFirstBNKnot(InvalidIndex,jAux)
            if(jAux>nVar) call HaltWith(kFctNKAI,jAux,NoBNK)
          else ! insist that jNKyToBe and a BasicNKy be in the same aux constraint
            call HaltWith(kFctNKAI,jNKyToBe,NoBNK//' re')
          end if
        end if
        j2=KeyVar(kFctNKAI) ! kFctNKAI key is reassigned below to jAux
        if(j2/=InvalidIndex) then
          if(AuxiCovers(j2).eqv..true.) then
            VarState(j2)=BasicNKy
          else
            call MakeBound(j2)
          end if
        end if
        if(PrtDetail>2) then
          write(PrtUni,Fmt1a)'NKAI2'
          call ShowStates
        end if
        if(VarState(jNKyToBe)==BasicKey) then ! enkey another var within facet
          kNKyToBe=FctOwning(jNKyToBe)
          call IntersectWithBNK(kNKyToBe) ! prefer to enkey a BasicNKy var
          call FindFirstBNKnot(InvalidIndex,j2)
          if(j2>nVar) then ! no BasicNKy extant in kNKyToBe => try the BoundNBa's
            j2=1
            do while((j2<=nVar).and.(VarState(j2)>BoundNB1))
              j2=j2+1
            end do
            if(j2>nVar) call HaltWith(kNKyToBe,0_2,NoBNK)
          end if
          KeyVar(kNKyToBe)=j2
          VarState(j2)=BasicKey
        end if ! else jNKyToBe was BoundNBa => no change to its facet's KeyVar
        VarState(jNKyToBe)=BasicNKy
        VarState(jAux)=BasicKey ! possibly jAux=jNKyToBe
        KeyVar(kFctNKAI)=jAux
        if(PrtDetail>2) then
          write(PrtUni,Fmt1a)'NKAI3'
          call ShowStates
        end if
      return ! entry FindNewlyKeyAuxIn


!     here to AlignAuxRows, program units are needed only thereby


      entry SwapStates(jAlt,jNBa,jNKy,kFctSS,AlignedSS) ! execute intra-facet swap
!       integer*2 kFctSS,jAlt,jNBa,jNKy
!       logical*1 AlignedSS

        if(kFctSS==InvalidFacet) then
          call MakeBound(jAlt)    ! formerly in B0
          VarState(jNBa)=BasicNKy ! formerly in N0
          jKey=jNBa ! useful only if PrtDetail>0
        else
          if(PrtDetail>2) call ShowStates
          jKey=KeyVar(kFctSS)
          KeyVar(kFctSS)=jAlt
          VarState(jAlt)=BasicKey ! formerly BasicNKy
          VarState(jKey)=BasicNKy ! formerly BasicKey
        end if
        if(PrtDetail>2) then
          call wnai(One2,jNKy)
          call wnai(One2,jKey)
          call wnai(One2,jAlt)
          call wnai(One2,kFctSS)
          write(PrtUni,Fmt1a)'SwapStates'
          call ShowStates
        end if
        call ChkUnplacedBNK(jNKy) ! check the result
        AlignedSS=jNKy==InvalidIndex
      return ! entry SwapStates


      entry FindRelatedBNK(jNBa,jNKy,kFctFR,AlignedFR)
!       integer*2 kFctFR
!       logical*1 AlignedFR

        call IntersectWithBNK(kFctFR)
        iAux=1
        do while((iAux<=nAux).and.(.not.AlignedFR))
          if(AuxCovers(iAux,jNKy).eqv..true.) then
            jAux=1
            do while((jAux<=nVar).and.(.not.AlignedFR)) ! find a related BNK
              if(BkOwns(jAux).and.AuxCovers(iAux,jAux)) then
                if(kFctFR==InvalidFacet) then ! search for a swappable BoundNBa var
                  jNBa=1
                  do while((jNBa<=nVar).and.(.not.((FctOwning(jNBa)== &
                    kFctFR).and.(VarState(jNBa)<BasicKey))))
                    jNBa=jNBa+1
                  end do
                  if(jNBa<=nVar) call SwapStates(jAux,jNBa,jNKy,kFctFR, &
                    AlignedFR)
                else
                  call SwapStates(jAux,jNBa,jNKy,kFctFR,AlignedFR)
                end if
              end if
              jAux=jAux+1
            end do
          end if
          iAux=iAux+1
        end do
      return ! entry FindRelatedBNK


      entry AlignAuxRows
!       integer*2 jNKyAAR,jNBaAAR,kAAR
!       logical*1 Aligned

        call ChkUnplacedBNK(jNKyAAR) ! check whether each BasicNKy var has a DOrgRow
        Aligned=jNKyAAR==InvalidIndex
        if(.not.Aligned) then
          kAAR=FctOwning(jNKyAAR) ! InvalidFacet for jNKyAAR in Psi0
          if(kAAR/=InvalidFacet) then
            if(AuxiCovers(KeyVar(kAAR)).eqv..true.) &
              call SwapStates(jNKyAAR,jNBaAAR,jNKyAAR,kAAR,Aligned)
          end if
          if(PrtDetail>2) write(PrtUni,'(1x,2i3,2l2,a)') &
            jNKyAAR,kAAR,AuxiCovers(KeyVar(kAAR)),Aligned,' AAR1'
          kAAR=kInf ! second attempt:  check all facets for possible swaps
          do while((kAAR<=nActiveFacets).and.(.not.Aligned))
            if(AuxiCovers(KeyVar(kAAR)).eqv..true.) &
              call FindRelatedBNK(jNBaAAR,jNKyAAR,kAAR,Aligned)
            if(PrtDetail>2) write(PrtUni,'(1x,2i3,2l2,a)') &
              jNKyAAR,kAAR,AuxiCovers(KeyVar(kAAR)),Aligned,' AAR2'
            kAAR=kAAR+1 ! KeyVar(k) above can become BasicNKy
          end do
          kAAR=InvalidFacet ! final attempt:  swap BasicNKy/BoundNBa in Psi0
          if(.not.Aligned) &
            call FindRelatedBNK(jNBaAAR,jNKyAAR,kAAR,Aligned)
          if(PrtDetail>2) write(PrtUni,'(1x,2i3,2x,l2,a)') &
            jNKyAAR,kAAR,Aligned,' AAR3'
!       else
!         if(PrtDetail>3) write(PrtUni,'(1x,a)') 'already aligned'
!         call ShowStates
        end if
!       if(.not.Aligned) call HaltWith(jNKyAAR,nAux,Unplaceable)
        if(.not.Aligned) then ! former BasicNKy becomes BoundNBa
          call FindAuxComplement(jNKyAAR,jNBaAAR)
          call MakeBound(jNBaAAR)
        end if
        BasisStrucObsolete=.not.Aligned !.false. if Aligned
      return ! entry AlignAuxRows


      entry SplitAndRevise ! OrdVarEntering <=> (LimitCondit==24)
!       integer*2 jKeyOE,kOE,kOfs

        jKey=KeyVar(kBind)
        if(BindSetOwns(jKey)) then
          kOfs=0
        else
          kOfs=1
        end if ! kBind+kOfs will retain jKey
        if(OrdVarEntering) then
          jKeyOE=KeyVar(FctOwning(jOrdEnt))
        else
          call MergeLeavingFacet ! assigns jNewlyNKy
        end if
        call SplitBindingFacet(True_Byte) ! FM(kBind) into (BindSet,FM(kBind)-BindSet)
        if(OrdVarEntering) then
          if(AuxiCovers(jOrdEnt).and.BindSetOwns(jOrdEnt)) then
            call IntersectWithBNK(kBind)
            call FindFirstBNKnot(jOrdEnt,j)
            FacetOwnsBNK=j<=nVar ! kBind owns a BasicNKy
          else
            FacetOwnsBNK=.true. ! assumption resolved by AlignAuxRows
          end if
          if(AuxiCovers(jOrdEnt).and.FacetOwnsBNK) then
            KeyVar(kBind+kOfs)=jKey ! formerly key in the entire kBind facet
            VarState(jOrdEnt)=BasicNKy ! formerly BoundNBa
            call FindNewlyKeyAuxIn(kBind+One2-kOfs,jOrdEnt,True_Byte)
            call AlignAuxRows
          else ! jOrdEnt cannot replace any var as BasicNKy, => BasicKey
            if(PrtDetail>2) then
              call ShowStates
              write(PrtUni,'(1x,6i3,i2,i3,a)')kBind,kLvg,kLp1,jKey, &
                KeyVar(kBind),KeyVar(kBind+1),kOfs,jKeyOE,' #24 SaR'
            end if
            VarState(jOrdEnt)=BasicKey ! in same facet as before
            kOE=FctOwning(jOrdEnt)
            KeyVar(kOE)=jOrdEnt
            KeyVar (kBind  +kOfs)=jKey
            if(kOE/=kBind+1-kOfs) then ! jOrdEnt replaces jKeyOE as KeyVar
            ! VarState(jKeyOE)=BasicNKy
              jNewlyNKy=jKeyOE
              call FindNewlyKeyAuxIn(kBind+One2-kOfs,jNewlyNKy, &
                True_Byte)
              call AlignAuxRows
            end if
          end if
        else ! not OrdVarEntering
          KeyVar(kBind+kOfs)=jKey ! formerly key in the entire kBind facet
          call FindNewlyKeyAuxIn(kBind+One2-kOfs,jNewlyNKy,True_Byte)
          call AlignAuxRows
        end if
        if(PrtDetail>2) write(PrtUni,'(1x,6i3,i2,a)')kBind,kLvg,kLp1, &
          jKey,KeyVar(kBind),KeyVar(kBind+1),kOfs,' 18ii/24'
      return ! entry SplitAndRevise


      entry CastAsBoundIfSingular(jCIS,NBaState)
!       integer*2 jCIS,NBaState
!       logical*1 Singular

        Singular=.not.AuxiCovers(jCIS)
        j2=1
        do while((j2<=nVar).and.Singular)
          if((j2/=jCIS).and.(FctOwning(j2)==kLvg)) Singular=.false.
          j2=j2+1
        end do
        if(Singular) then ! delete facet kLvg, casting its key into BoundNBa
          if(PrtDetail>3) write(PrtUni,'(1x,a,i1,2i3,f9.6)') &
            'singular BoundNB',NBaState,kLvg,jCIS,x(jCIS)/LUB(jCIS)
          VarState(jCIS)=NBaState
          FctOwning(jCIS)=nActiveFacets ! changed later if VarState==BoundNB1
          call DeleteFacet(kLvg)
          if(PrtDetail>3) call ShowStates
        end if
      return ! entry CastAsBoundIfSingular


      entry ReviseBasisVectors
!       logical*1 SameSubset

        if(PrtDetail>1) then
          call wnaa(' Step3:  revise facet constraints')
          write(PrtUni,'(1x,a,i2,4i3,1x,e13.6)')'using inequality #', &
            LimitCondit,kLvg,kBind,jBind,jOrdEnt,InfRHS
          if(PrtDetail>2) call ShowStates
        end if
        if(InfRHS>SmallReal) then ! remove ineligibility for ChooseDescentDirection
        ! WouldBeZero=.false. ! call cinitb(WouldBeZero,I4_nVar,.false.) ! enable all vars for basis entry
          call initb1(WouldBeZero,nVar,FalseByte) ! enable all vars for basis entry
        elseif((LimitCondit==25).and.(.not.AuxiCovers(jOrdEnt))) then
        ! reject the current jOrdEnt as inappropriate for basis entry;
        ! non-aux jOrdEnt may not enter basis as BasicKey at 0, nor may it
        ! become BasicNKy, yet only BasicNKy may be degenerate (have x=0)
          if(PrtDetail>1) write(PrtUni,'(1x,a,i3,a/)') &
            'reconsider choice of pivot; non-aux var',jOrdEnt, &
            ' cannot become basic at value 0'
          WouldBeZero(jOrdEnt)=.true. ! mark jOrdEnt as ineligible until InfRHS>0
          return ! recall ChooseDescentDirection; do not count this as an iteration
        end if
        nIter=nIter+1
!       InitSort=InitSort.and.(
!    +    (LimitCondit==24).or.
!    +    (LimitCondit==25).or.
!    +    (LimitCondit==27))
      ! PrevState=VarState ! call cmove(VarState,PrevState,I4_2nVar) ! refresh the criteria for AuxComplement
        call CopyLinIVec1(VarState,PrevState,nVar) ! refresh the criteria for AuxComplement
        BasisStrucObsolete=.true. ! until ChkUnplacedBNK is called

        if(LimitCondit==18) then
          jLvg=KeyVar(kLvg)
          if(kBind==kLp1) then ! repartition (kLvg)+(kLp1)
            jLp1=KeyVar(kLp1)
            if(PrtDetail>3) write(PrtUni,'(1x,5i3,a)')kBind,kLvg,kLp1, &
              jLvg,jLp1,' 18iii'
            call PartReBindSetFacets(kLvg,kLp1)
            SameSubset=.false. ! check new location of both formerly-key vars
            do k=0,1
              if((FctOwning(jLvg)==kLvg+k).and. &
                 (FctOwning(jLp1)==kLvg+k)) then
                SameSubset=.true.
                if(AuxiCovers(jLvg).eqv..true.) then
                  j=jLvg
                else
                  j=jLp1
                end if
                VarState(j)=BasicNKy ! make first formerly-key AuxiCovers NKy
                if(j==jLvg) then
                  KeyVar(kLvg+k)=jLp1 ! retain the other as key
                else
                  KeyVar(kLvg+k)=jLvg
                end if
                call FindNewlyKeyAuxIn(kLp1-k,j,True_Byte)
                call AlignAuxRows
                if(PrtDetail>3) write(PrtUni,'(1x,5i3,a)') &
                  k,kLvg+k,kLp1-k,jLvg,KeyVar(kLp1-k),' same subset'
              end if
            end do
            if(.not.SameSubset) then
            ! each remains key, but perhaps in a subset different from before
              if(FctOwning(jLvg)==kLp1) then ! swap key positions
                KeyVar(kLvg)=jLp1
                KeyVar(kLp1)=jLvg
              else ! each remains key in same subset order as before
                KeyVar(kLvg)=jLvg ! overwrite the InvalidIndex assigned ...
                KeyVar(kLp1)=jLp1 ! ... in PartReBindSetFacets
              end if
            end if
          else
            call SplitAndRevise ! kBind is neither kLvg nor kLp1
          end if
          ! end case of #18
        elseif(LimitCondit==24) then
          call SplitAndRevise ! jOrdEnt replaces the BasicNKy in the split
        elseif((LimitCondit==19).or.(LimitCondit==25)) then
          ! the corresponding var jBind leaves the basis
          kBind=FctOwning(jBind) ! default was valid for LimitCondit==18 or 24
          call IntersectWithBNK(kBind) ! define BkOwns
          FinalState=BasicNKy ! default for jOrdEnt
          if(OrdVarEntering) then ! #25
            FacetOwnsBNK=.false. ! default
            if(AuxiCovers(jOrdEnt).eqv..true.) then
              if(VarState(jBind)==BasicNKy) then ! per #25iia
                FacetOwnsBNK=.true.
              elseif(VarState(jBind)==BasicKey) then ! per #25iic
                call FindFirstBNKnot(jBind,j)
                FacetOwnsBNK=j<=nVar
              end if
            end if ! else jBind, if BasicNKy, remains so
            if(FacetOwnsBNK) then ! jOrdEnt can become BasicNKy per #25iia&c
              FinalState=BasicNKy
              if(AuxiCovers(jBind).and.(VarState(jBind)<BasicKey)) then
              ! jBind cannot leave basis; search for its BasicNKy complement
                call FindAuxComplement(jBind,jAux)
                if(PrtDetail>2) write(PrtUni,'(1x,5i3,i2,i3,a)') &
                  kLvg,kLp1,jOrdEnt,kBind,jBind, &
                  VarState(jBind),LimitCondit,' jBind was BoundNBa'
                if(VarState(jAux)/=BasicNKy) call HaltWith(kBind,jBind, &
                  'facet var aux complement not BasicNKy')
                jBind=jAux
!               kBind=FctOwning(jBind)
              end if
              kBind=FctOwning(jBind)
!             if(kBind/=InvalidFacet) FctOwning(jOrdEnt)=kBind
              FctOwning(jOrdEnt)=kBind
            else
              FinalState=BasicKey ! jOrdEnt cannot become BasicNKy per #25iib&d
              if(nAux==0) then
!               call MakeBound(KeyVar(kBind)) done below, as KeyVar(kBind)=jBind
                VarState(jOrdEnt)=BasicKey
                FctOwning(jOrdEnt)=kBind
                KeyVar(kBind)=jOrdEnt
              end if
            end if
            if((FinalState==BasicNKy).and.Nearly0(x(jOrdEnt),1)) & !  put in final facet
              FctOwning(jOrdEnt)=nActiveFacets ! to allow proper computation of dual
            VarState(jOrdEnt)=FinalState
            jNewlyNKy=jOrdEnt ! becomes newly key in some cases below
          else ! #19
            jLvg=KeyVar(kLvg)
            jLp1=KeyVar(kLp1)
            call MergeLeavingFacet ! assigns jNewlyNKy
          end if
          if(PrtDetail>3) write(PrtUni,'(1x,5i3,i2,i3)') &
            kLvg,kLp1,jNewlyNKy,kBind,jBind,VarState(jBind),LimitCondit

          if((nAux>0).and.(jNewlyNKy/=InvalidIndex)) then ! BasicNKy state is possible
          ! by this point, jBind is either BasicNKy or BasicKey
            if(VarState(jBind)==BasicNKy) then
!             if(FinalState==BasicNKy) then ! jNewlyNKy replaced jBind as BasicNKy
!      wrong:   if(kBind/=InvalidFacet) FctOwning(jNewlyNKy)=kBind ! deleted 980813
!      above caused non-monotonicity in the vars' average CF, is not defensible
                ! KeyVar is unchanged since neither jBind was key, nor is jNewlyNKy
              ! else jBind was key => was in Psik; make it non-basic
!             elseif(kBind/=InvalidFacet) then ! FinalState=BasicKey
!      dubious  VarState(KeyVar(kBind))=BoundNB0 ! without casting into nActiveFacets
!      dubious  KeyVar(kBind)=jNewlyNKy
!             end if
            elseif(kBind/=InvalidFacet) then
              call FindFirstBNKnot(InvalidIndex,j)
              if(AuxiCovers(jNewlyNKy).and.(FinalState==BasicNKy).and. &
                (j<=nVar)) then ! kBind owns aux jNewlyNKy and BasicNKy j
                call FindNewlyKeyAuxIn(kBind,jNewlyNKy, &
                  .not.AuxiCovers(jBind))
              else ! make jNewlyNKy key
                if(PrtDetail>2) write(PrtUni,'(1x,4i4,a)') &
                  jNewlyNKy,VarState(jBind),jBind,kBind, &
                  ' jNewlyNKy not aux, or kBind has no BasicNKy'
                VarState(jNewlyNKy)=BasicKey ! no BasicNKy has left ...
                FctOwning(jNewlyNKy)=kBind ! ... room for xn to be BasicNKy
                KeyVar(kBind)=jNewlyNKy
              end if
            end if
          end if
          if(AuxiCovers(jBind).eqv..false.) call MakeBound(jBind) ! always valid for #19 and #25
!         call MakeBound(jBind) ! always valid for #19 and #25? (980811)
          if(PrtDetail>2) write(PrtUni,'(1x,4i4,a)') &
            jNewlyNKy,VarState(jBind),jBind,kBind,' before AlignAuxRows'
          call AlignAuxRows
        end if

!       x(j)=x(j)+d(j)*InfRHS ! call AddScalarMult instead of looping
        call AddScalarMult(InfRHS,d,x,nVar)

!       generic cleanup appropriate in all cases:
!       eliminate any facet if its KeyVar is singular, non-aux, and near LUB or GLB
        kLvg=nActiveFacets-1 ! the final facet is not likely to be singular
        do while(kLvg>=kInf)
          j=KeyVar(kLvg)
          if(PrtDetail>3) write(PrtUni,'(1x,a,2i3,f9.6)') &
            'test for BoundNB0',kLvg,j,x(j)/LUB(j)
          if(x(j)>0.00001*LUB(j)) exit ! no lower facet has a KeyVar with lesser CF
        ! use of  0.01 above kept significant peakers' energy from being offloaded
        ! to the pumped-storage generators (in the case of Duke Power);
        ! use of  0.0001 above allowed premature exit on condition of jOrdEnt cycling
          call CastAsBoundIfSingular(j,BoundNB0)
          kLvg=kLvg-1
        end do

        if(PrtDetail>2) call ShowStates
      return ! entry ReviseBasisVectors


      end ! subroutine FacetAlgorithm
