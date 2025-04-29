
!     LPSOLVER.for
!     Copyright(c) M.S.Gerber & Associates 2000
!
!     Created: 9/10/2003 12:39:18 PM
!     Author : GREG TURK
!     Last change: GAT 9/10/2003 3:06:33 PM


      recursive subroutine LPsolver(NewStructure)

      use SpinDriftLib
      USE PROD_ARRAYS_DIMENSIONS
      use lpintcom

      ! apply Convex Separable Programming to the optimization of current flows;
      ! use the Upper-Bounded Simplex method to deal with UB constraints on wij;
      ! use the Dual-Simplex method to drive an infeasible optimal solution into
      ! feasibility

      ! solution of linear-programming problem by Simplex method, according to
      ! G. Hadley, Linear Programming, Addison-Wesley, 1963

      ! units implicit below are MW on power, kV on voltage, kA on current,
      ! ohms on impedance, mhos or Siemens on admittance

      ! parameter constants
      character (len=1) :: ABlank,LtChar,GtChar,EqlChar,NeqChar,LimChar
      character (len=4) :: Fmtanr
      character (len=6) :: Fmtxa
      character (len=8) ::  Fmti4i8
      character (len=10) :: Fmti4e8p1,Fmti4f8p2,Fmti4f8p4
      character (len=11) :: Fmtx2ianr,Fmt3af8,ClkTime
      character (len=12) :: Fmta53i,Fmtxf12a
      character (len=14) :: SelectingkIns,SelectingrDel,SelectedrDel
      character (len=20) :: Fmta3i2e
      character (len=21) :: Fmtif9if12
      character (len=25) :: Fmta2i2f2e
      character (len=30) :: TPLiAL
      character (len=20) :: IfZRes
      character (len=45) :: AVIB
      character (len=49) :: FmtLong
      character (len=74) :: BasisDesc
      integer (kind=2) :: &
          InvalidIndx, &
          LimOrgNodes, &
          LimTraLines, &
          LimConstrts, &
          LimUnknowns, &
          SupNodeIdx, &
          SupLineIdx, &
          SupConsIdx, &
          SupUnknown
        real (kind=4) :: &
          ConstrtTol, &
          CUB
        parameter(ABlank=' ', &
                  LtChar='<', &
                  GtChar='>', &
                  EqlChar='=', &
                  NeqChar='Ø', &
                  LimChar='œ', &
                  Fmtanr    ='(a\)', &
                  Fmtxa     ='(1x,a)', &
                  Fmti4i8   ='(i4,i8\)', &
                  Fmti4e8p1 ='(i4,e8.1\)', &
                  Fmti4f8p2 ='(i4,f8.2\)', &
                  Fmti4f8p4 ='(i4,f8.4\)', &
                  Fmtx2ianr ='(1x,2i4,a\)', &
                  Fmt3af8   ='(3a1,f8.3\)', &
                  ClkTime   ='clock time ', &
                  Fmtxf12a  ='(1x,f12.6,a)', &
                  Fmta53i   ='(1x,a5,3i4\)', &
                  Fmta3i2e  ='(1x,a,3i4,2e10.3,i4)', &
                  Fmtif9if12='(1x,i4,f9.4,i5,f12.9)', &
                  Fmta2i2f2e='(1x,a,i4,i8,2f9.3,2e13.4)')
        parameter(SelectingkIns='selecting kIns', &
                  SelectingrDel='selecting rDel', &
                  SelectedrDel ='selected  rDel', &
                  FmtLong  ='(i5,a1,2i4,1x,a5,1x,e10.3,f12.6,f15.9,f9.3,2x,a5)', &
                  AVIB     ='zero-valued artificial vector(*) in basis => ', &
                  BasisDesc='   i   iR   j xType x(j)=xB(i)  x(j)=xB(i)'//'     (B(iR),xB)    b(iR) ReType', &
                  TPLiAL='total power loss in all lines=', &
                  IfZRes=' if Z were resistive', &
                  InvalidIndx=9990, & ! always at least LimUnknowns (4095 was confusing)
                  LimOrgNodes= 100, & ! limits nOrigVar=# of active and passive nodes
                  LimTraLines=2000, & ! limits m; for at most 20 tie-lines per node
                  LimConstrts=2100, & ! sum of the above two
                  LimUnknowns=2100, & ! same as the above
                  SupNodeIdx =  99, & ! always LimOrgNodes-1
                  SupLineIdx =1999, & ! always LimTraLines-1
                  SupConsIdx =2099, & ! always LimConstrts-1
                  SupUnknown =2099, & ! always LimUnknowns-1
      !   NoiseThresh=1.000001, ! for consistency in changing basis vectors
                  ConstrtTol=0.0001, & ! tolerance on meeting the constraint bounds, 1e-6 for Pascal
                  CUB=1.0) ! common upper bound on solution variables
      ! end of parameters



      ! allocatable local arrays
        logical (kind=1) :: &
           UpperBounded(:), &  ! (j) iff xj is at its upper bound (of unity)
           SymmetricLimits(:) ! (j) iff line j has equal pos & neg limits
        integer (kind=2) :: &
           LineEnd(:,:), &
           BasisCol(:), & ! (j) locates column j of B in A
           BasisPos(:), & ! (j) locates column j of A in B
           PivotRow(:) ! (j) locates row    j of LU in original A
        real (kind=4) :: &
           SwapVector(:),SupMagInRow(:), &
           b(:),c(:),UB(:),xB(:),ConstrtRow(:),yLine(:),ej(:), &
           ConsRHS(:),SegWeight(:),CurrLimit(:,:), & ! 2 x n
        ! aj=B*yj => A=B*Y => Y=BInv*A
           AMatrix(:,:), & ! m x n; (i,j)=> row i, col j (col-primary)
           GMatrix(:,:), & ! m x m; (j,i)=> col j, row i (row-primary)
           GMatLUF(:,:), & ! m x m; (j,i)=> col j, row i (row-primary)
           BMatrix(:,:), & ! m x m; (j,i)=> col j, row i (row-primary)
           BMatLUF(:,:), & ! m x m; (j,i)=> col j, row i (row-primary)
           BMatInv(:,:), & ! m x m; (i,j)=> row i, col j (col-primary)
           NodeVoltage(:),NodeCurrent(:), & ! vectors used in main-entry only
           Phi(:),yj(:),yk(:),z_c(:),zStar_c(:),zOrg_c(:),cB(:)
        character (len=32) :: &
           LineName(:)
        allocatable :: &
           UpperBounded,SymmetricLimits, &
           LineEnd,BasisCol,BasisPos,PivotRow, &
           SwapVector,SupMagInRow, &
           b,c,UB,xB,ConstrtRow,yLine,ej,ConsRHS,SegWeight,CurrLimit, &
           AMatrix,GMatrix,GMatLUF,BMatrix,BMatLUF,BMatInv, &
           NodeVoltage,NodeCurrent,Phi,yj,yk,z_c,zStar_c,zOrg_c,cB, &
           LineName
        save &
           UpperBounded,SymmetricLimits, & 
           LineEnd,BasisCol,BasisPos,PivotRow, &
           SwapVector,SupMagInRow, &
           b,c,UB,xB,ConstrtRow,yLine,ej,ConsRHS,SegWeight,CurrLimit, &
           AMatrix,GMatrix,GMatLUF,BMatrix,BMatLUF,BMatInv, &
           NodeVoltage,NodeCurrent,Phi,yj,yk,z_c,zStar_c,zOrg_c,cB, &
           LineName

      ! local variables and non-allocatable arrays
        character (len=1) :: RelChar
        logical (kind=1) :: Feasible,Optimal,Singular,InBounds, &
                            NewStructure
        integer (kind=2) :: &
           nVar,nSup,mReq,mSup,siNode,siLine,siLnX4,siCons,siRank,siRest, &
           nNodes,nLines,nLinX4,nIter,nRestr, &
           i,j,k,jCol,iLine,iNode,jNode, &
           iMostInfeas, &
           TRANSACTION_GROUPS_W_PATHS,PATH_FIRST_WHEELS
        integer (kind=4) :: Int4mReq
        real (kind=4) :: r, &
          zO, & ! functional result to be optimized, denoted z by Hadley
      !   OverloadLimit =0.1, ! this may be reduced to the level of overloads
      !                         that are actually intolerable, e.g., 0.01 to
      !                         require that final solution have overloads<=1%
          OverloadLimit, & ! fractional nominal overload tolerated in final solution
      !   OverloadFactor=1000.0, ! cost multiplier for currents > nominal limit
          OverloadFactor, & ! cost multiplier for currents above the nominal limit
          TrVoltage, & ! common nominal input voltage of the transmission network
          CompCurrent,LinePowerLoss,LinePwrLosses, &
          LinePowerLimit(0:1),u(0:1)
        save zO,OverloadLimit,OverloadFactor

      ! declarations of common functions called
        character (len=11) :: clk
        character (len=4) :: Char4Eqv
        real (kind=4) :: ContInnerProduct,DiscInnerProduct

      ! declarations of entry points' local vars
        logical (kind=4) :: AlreadyOpen
        real (kind=4) :: SystemPwrI,SystemPwrO,PowerLevel, &
                         LineVoltageLevel,g
        integer (kind=2) :: rMI,iMI,jMI
        real (kind=4) :: rRow(0:*)
        integer (kind=2) :: jAZ
        integer (kind=2) :: jGYC,iGYC
        real (kind=4) :: yjGYC(0:*)
        integer (kind=2) :: iAxBfUB
        real (kind=4) :: yColumn(0:*),Bound
        real (kind=4) :: Violation,SupViolation
        logical (kind=1) :: Unbounded,UBkIns,UBrDel !,RobustPair
        integer (kind=2) :: iDel,jA,jDel,jCd4,kIns,rDel,rDelPos,rDelNeg
        real (kind=4) :: xBr,yik,yrj,yrk,z_cSup,z_ckIns, &
                Ratio,InfRatio,InfRatioPos,InfRatioNeg !,InfRatioLim
        real (kind=4) :: xj
        logical (kind=1) :: vWbV(0:*)
        character (len=*) :: TitleWbV
        integer (kind=2) :: jWbV,iWbV
        real (kind=4) :: vWmV(0:*)
        character (len=*) :: TitleWmV
        integer (kind=2) :: jWmV
        real (kind=4) :: vWnV(0:*)
        character (len=*) :: TitleWnV
        integer (kind=2) :: jWnV
        real (kind=4) :: ValueCE,LimitCE,AbsD,AbsL
        real (kind=4) :: ValueCL,LimitCL

        call InitiatePrecomputedSolution
        Feasible=.true.
        write(OF1,*) ClkTime,clk(),' before invoking LPsolver'
        do ! assume initial solution is Feasible and not Optimal
          call OptimizeSolution ! using Revised-Simplex method
          if(Optimal) exit
        end do
        call CheckFeasibility
        do while(.not.Feasible)
        ! use the Dual-Simplex method to gain feasibility, retaining optimality }
          call OptimizeSolution
          call CheckFeasibility
        end do

      ! call DeallocLPSInternals ! the caller now must perform disposal
      return ! end of main entry-point


      entry AllocLPSInternals ! excluding BMatInv and SwapVector
        ! check allocation status after each call to allocate arrays
        allocate(BMatrix(0:mSup,0:mSup)    ,stat=AS)
        call CkAllocSt('BMat',0)
        allocate(BMatInv(0:mSup,0:mSup)    ,stat=AS)
        call CkAllocSt('BInv',0)
        allocate(SwapVector (0:mSup)       ,stat=AS)
        call CkAllocSt('SVec',0)
        allocate(Phi(0:mSup)               ,stat=AS)
        call CkAllocSt('PhiV',0)
        allocate(yj (0:mSup)               ,stat=AS)
        call CkAllocSt('yj_V',0)
        allocate(yk (0:mSup)               ,stat=AS)
        call CkAllocSt('yk_V',0)
        allocate(z_c(0:nSup)               ,stat=AS)
        call CkAllocSt('z_cV',0)
        allocate(zStar_c(0:nSup)           ,stat=AS)
        call CkAllocSt('z*_c',0)
        allocate(zOrg_c(0:nSup)            ,stat=AS)
        call CkAllocSt('zO_c',0)
        allocate(GMatrix(0:siRank,0:siRank),stat=AS)
        call CkAllocSt('GMat',0) ! note siRank<siNode
        allocate(GMatLUF(0:siRank,0:siRank),stat=AS)
        call CkAllocSt('GLUF',0)
      return ! entry AllocLPSInternals


      entry DeallocLPSInternals
        ! check allocation status after each call to deallocate arrays
        deallocate(BMatrix   ,stat=AS)
        call CkAllocSt('BMat',1)
        deallocate(BMatInv   ,stat=AS)
        call CkAllocSt('BInv',1)
        deallocate(SwapVector,stat=AS)
        call CkAllocSt('SVec',1)
        deallocate(Phi       ,stat=AS)
        call CkAllocSt('PhiV',1)
        deallocate(yj        ,stat=AS)
        call CkAllocSt('yj_V',1)
        deallocate(yk        ,stat=AS)
        call CkAllocSt('yk_V',1)
        deallocate(z_c       ,stat=AS)
        call CkAllocSt('z_cV',1)
        deallocate(zStar_c   ,stat=AS)
        call CkAllocSt('z*_c',1)
        deallocate(zOrg_c    ,stat=AS)
        call CkAllocSt('zO_c',1)
        deallocate(GMatrix   ,stat=AS)
        call CkAllocSt('GMat',1)
        deallocate(GMatLUF   ,stat=AS)
        call CkAllocSt('GLUF',1)
      return ! entry DeallocLPSInternals


      entry AllocLPSVariables
        ! check allocation status after each call to allocate arrays
        if(allocated(UpperBounded)) return
        allocate(UpperBounded(0:nSup)    ,stat=AS)
        call CkAllocSt('Bded',0)
        allocate(SymmetricLimits(0:nSup) ,stat=AS)
        call CkAllocSt('SLim',0)
        allocate(LineEnd(0:1,0:siLine)   ,stat=AS)
        call CkAllocSt('LinE',0)
        allocate(BasisCol(0:mSup)        ,stat=AS)
        call CkAllocSt('BCol',0)
        allocate(BasisPos(0:nSup)        ,stat=AS)
        call CkAllocSt('BPos',0)
        allocate(b(0:siCons)             ,stat=AS)
        call CkAllocSt('bVec',0)
        allocate(c(0:nSup)               ,stat=AS)
        call CkAllocSt('cVec',0)
        allocate(UB(0:nSup)              ,stat=AS)
        call CkAllocSt('UBnd',0)
        allocate(xB(0:mSup)              ,stat=AS)
        call CkAllocSt('xBas',0)
        allocate(ConstrtRow(0:nSup)      ,stat=AS)
        call CkAllocSt('cRow',0)
        allocate(yLine(0:siLine)         ,stat=AS)
        call CkAllocSt('yLin',0)
        allocate(ej(0:mSup)              ,stat=AS)
        call CkAllocSt('ej_V',0)
        allocate(ConsRHS(0:siCons)       ,stat=AS)
        call CkAllocSt('cRHS',0)
        allocate(SegWeight(0:nSup)       ,stat=AS)
        call CkAllocSt('SegW',0)
        allocate(CurrLimit(0:1,0:siLine) ,stat=AS)
        call CkAllocSt('iLim',0)
        allocate(AMatrix(0:siCons,0:nSup),stat=AS)
        call CkAllocSt('cMat',0) ! note (row,col) order
        allocate(LineName(0:siLine)      ,stat=AS)
        call CkAllocSt('LiNm',0)
      return ! entry AllocLPSVariables


      entry DeallocLPSVariables
        ! check allocation status after each call to deallocate arrays
        deallocate(UpperBounded   ,stat=AS)
        call CkAllocSt('Bded',1)
        deallocate(SymmetricLimits,stat=AS)
        call CkAllocSt('SLim',1)
        deallocate(LineEnd        ,stat=AS)
        call CkAllocSt('LinE',1)
        deallocate(BasisCol       ,stat=AS)
        call CkAllocSt('BCol',1)
        deallocate(BasisPos       ,stat=AS)
        call CkAllocSt('BPos',1)
        deallocate(b              ,stat=AS)
        call CkAllocSt('bVec',1)
        deallocate(c              ,stat=AS)
        call CkAllocSt('cVec',1)
        deallocate(UB             ,stat=AS)
        call CkAllocSt('UBnd',1)
        deallocate(xB             ,stat=AS)
        call CkAllocSt('xBas',1)
        deallocate(ConstrtRow     ,stat=AS)
        call CkAllocSt('cRow',1)
        deallocate(yLine          ,stat=AS)
        call CkAllocSt('yLin',1)
        deallocate(ej             ,stat=AS)
        call CkAllocSt('ej_V',1)
        deallocate(ConsRHS        ,stat=AS)
        call CkAllocSt('cRHS',1)
        deallocate(SegWeight      ,stat=AS)
        call CkAllocSt('SegW',1)
        deallocate(CurrLimit      ,stat=AS)
        call CkAllocSt('iLim',1)
        deallocate(AMatrix        ,stat=AS)
        call CkAllocSt('cMat',1)
        deallocate(LineName       ,stat=AS)
        call CkAllocSt('LiNm',1)
      return ! entry DeallocLPSVariables


      entry SetupConstraints
      ! note that TrVoltage and PowerLevel are re input, per Greg Turk
      ! logical*4 AlreadyOpen
      ! real*4 SystemPwrI,SystemPwrO,PowerLevel,LineVoltageLevel,g

      ! begin lines lifted from LPdriver.for
        PrtDetail = 3
        if(PrtDetail>0) then
          OF1=861 ! arbitrarily
          inquire(unit=OF1,opened=AlreadyOpen)
          if(.not.AlreadyOpen) &
            open(unit=OF1,file='LoadFlow.dbg',CarriageControl='fortran')
        else
          OF1=6 ! StdOut is always available on unit 6
        end if
        OF0=860
      ! AIU=859
        inquire(unit=OF1,opened=AlreadyOpen)
        if(.not.AlreadyOpen) &
            open(unit=OF0,file='LoadFlow.out',CarriageControl='fortran')
      ! end lines lifted from LPdriver.for

      ! open(unit=AIU,file='LoadFlow.dat',CarriageControl='fortran',
      !   status='old')
      ! read(AIU,*) siNode,siLine,TrVoltage,OverloadLimit,OverloadFactor
        siNode=TRANSACTION_GROUPS_W_PATHS()-1 ! NUMBER_OF_TG-1 ! PATHS FILE
        siLine=PATH_FIRST_WHEELS()-1          ! NUMBER_OF_WHEELS(1)-1 ! PATHS FILE
        TrVoltage=10.0        ! HARD WIRED UNTIL MORE INFORMATION
        OverloadLimit=0.1     ! HARD WIRED UNTIL MORE INFORMATION
        OverloadFactor=1000.0 ! HARD WIRED UNTIL MORE INFORMATION
      ! GAT. 7/9/98 replaced reading parameters with function calls & assignments

        if(siNode>SupNodeIdx) call HaltWith(siNode,SupNodeIdx, &
          'largest voltage-node index exceeds limit of')
        nNodes=siNode+1 ! at first include the redundant constraint(s)
        nLines=siLine+1
        nLinX4=nLines*4 ! pos & neg current, weight to and above breakpoint
        siLnX4=nLinX4-1
        nRestr=nLinX4 ! preclude node voltages from entering the basis
        siRest=nRestr-1
        nVar=nLinX4+nNodes ! incl. node voltages in equations defining weights
        nSup=nVar-1
        siRank=siNode-1 ! exclude the redundant constraint
        siCons=nLines+siNode ! including constraints for sum of wijk*uijk=vi-vk
        mSup  =nLines+siRank
        mReq  =mSup+1
        if(siCons>SupConsIdx) call HaltWith(siCons,SupConsIdx, &
          'largest constraint index exceeds limit of')
        call AllocLPSVariables ! allocatable arrays may not be passed via common blocks
        call AllocLPSInternals
      ! all dimensions are determined by here; arrays can be allocated no sooner;
      ! zero the sparse constraint and admittance matrices
        AMatrix = 0.
        GMatrix = 0.

        if(PrtDetail>0) write(OF1,'(1x,a,i1,f5.2,i6)') &
          'PrtDetail, Overload Limit & Factor:  ', &
          PrtDetail,OverloadLimit,OverloadFactor
        if(PrtDetail>1) write(OF1,Fmtxa) &
          'Node PowerLevel SysPwrI SysPwrO'
      !   units implicit below are MW on power, kV on voltage, kA on current,
      !   ohms on impedance, mhos or Siemens on admittance
        SystemPwrI=0.0 ! power input
        SystemPwrO=0.0 ! power output
        do iNode=0,siNode
        ! read sours & sinks mixed in arbitrary order, as if dynamically mutable
      !   read(AIU,*) PowerLevel ! negative for sinks
          call GetTransPowerSold(iNode+1,PowerLevel) ! positive for seller, neg for buyer
      ! GAT. 7/9/98 replaced reading PowerLevel with function call
      ! INFORMATION PASSED FROM SIMULATE_MULTI_PARTY
          if(PowerLevel>0.0) then
            SystemPwrI=SystemPwrI+PowerLevel
          else
            SystemPwrO=SystemPwrO-PowerLevel
          end if
          if(PrtDetail>1) write(OF1,'(1x,i4,f11.3,2f8.1)') &
            iNode,PowerLevel,SystemPwrI,SystemPwrO
        ! each node generates one equation per Kirchoff's laws
          ConsRHS(nLines+iNode)=PowerLevel/TrVoltage ! treat sources & sinks as fixed currents
        end do ! values assigned for siNode may be ignored below
        if(abs(SystemPwrI-SystemPwrO)>ConstrtTol*SystemPwrI) &
          call HaltWith(0,0,'imbalance in input/output power levels')

        if(PrtDetail>1) write(OF1,Fmtxa) 'Line  iN  jN vLevel '// &
          'PosPwrLim NegPwrLim impedn impedn PosILim NegILim LineName'
        do iLine=0,siLine ! read parameters of tie-lines
      !   read(AIU,*) iNode,jNode,r,LineVoltageLevel,
      !     LinePowerLimit(0),LinePowerLimit(1),LineName(iLine)
      ! GAT. 7/9/98 replaced reading parameters with function calls & assignments
      ! INFORMATION FROM THE PATHS FILE
          call GET_TRANS_PATH_PARAMS(iLine,iNode,jNode,r, &
               LineVoltageLevel,LinePowerLimit(0))
          LinePowerLimit(1)=-LinePowerLimit(0) ! assume symmetry for now (980716)
          LineName(iLine)='from '//Char4Eqv(iNode)//' to '//Char4Eqv(jNode) ! simple

          SymmetricLimits(iLine)=((1.0+LinePowerLimit(1)/LinePowerLimit(0))<ConstrtTol)
          LineEnd(0,iLine)=iNode
          LineEnd(1,iLine)=jNode ! iNode and jNode are distinct
          if(PrtDetail>1) write(OF1,'(1x,3i4,f7.1,2f10.2,f7.3\)') & ! Here "\" should be removed 
            iLine,iNode,jNode,LineVoltageLevel, &
            LinePowerLimit(0),LinePowerLimit(1),r
        ! transform the nominal impedance to that effective at common TrVoltage:
          r=r*(TrVoltage/LineVoltageLevel)**2 ! proportional to voltage squared
          g=1.0/r
          yLine(iLine)=g
        ! impose LinePowerLimit in terms of current at common TrVoltage:
          do j=0,1 ! save input-power limits as current limits
            CurrLimit(j,iLine)=LinePowerLimit(j)/TrVoltage
          end do
          if(PrtDetail>1) write(OF1,'(f7.4,2f8.2,1x,a)') &
            r,CurrLimit(0,iLine),CurrLimit(1,iLine),LineName(iLine)

        ! accumulate symmetric tie-line admittances;
        ! omit entries outside array bounds of (0:siRank,0:siRank)
          if(iNode<=siRank) &
            GMatrix(iNode,iNode)=GMatrix(iNode,iNode)+g ! augment the 2 ...
          if(jNode<=siRank) &
            GMatrix(jNode,jNode)=GMatrix(jNode,jNode)+g ! ... diagonal terms
          if((iNode<=siRank).and.(jNode<=siRank)) then
            GMatrix(iNode,jNode)=GMatrix(iNode,jNode)-g
            GMatrix(jNode,iNode)=GMatrix(iNode,jNode) ! by symmetry
          end if

          do i=0,1 ! 0=>pos, 1=>neg current
            u(0)=CurrLimit(i,iLine) ! negative for i=1
            u(1)=u(0)*OverloadLimit
            do j=0,1 ! 0=>below, 1=>above break-point
              jCol=iLine*4+i*2+j
            ! objective is to minimize sum of branch-voltage drops below BP
              c(jCol)=r*abs(u(j)) ! all c(.) are positive; r is line impedance
              if(j==1) c(jCol)=c(jCol)*OverloadFactor
            ! fill in coefficients in the constraints
              AMatrix(nLines+iNode,jCol)= u(j) ! for current from iNode to jNode
              AMatrix(nLines+jNode,jCol)=-u(j)
              if(j==0) then ! use linear imped.
                AMatrix(iLine,jCol)=u(j)*r
              else ! voltage drop is greater
                AMatrix(iLine,jCol)=u(j)*r*OverloadFactor
              end if
            end do
          end do
          AMatrix(iLine,nLinX4+iNode)=-1.0 ! defining eqn. for this iLine:
          AMatrix(iLine,nLinX4+jNode)= 1.0 ! ä(wk*uk)-vi+vj=0, k on [0,3]
          ConsRHS(iLine)= 0.0
        end do ! loop over lines
      ! close(AIU)
        write(OF1,*) ClkTime,clk(),' after setting-up constraints'
      return ! entry SetupConstraints


      entry InitiatePrecomputedSolution ! i.e., set xB=(a precomputed vector)
      ! use solution of G*v=i to derive the initial BFS for the linear system

      ! local vectors useful only within this entry-point
        allocate(cB(0:mSup)           ,stat=AS)
        call CkAllocSt('cB_V',0)
        allocate(NodeVoltage(0:siNode),stat=AS)
        call CkAllocSt('NodV',0)
        allocate(NodeCurrent(0:siNode),stat=AS)
        call CkAllocSt('NodC',0)

      ! G is used here for admittance in lieu of Y to avoid confusion of this
      ! matrix with Hadley's non-square matrix Y (referenced in GetYColumn)
        NodeVoltage(siNode)=0.0 ! allows deletion of column siNode from GMatrix
        if(NewStructure) then
        ! preserve the original GMatrix before LU-factorization
        ! GMatLUF=GMatrix ! call cmove(GMatrix,GMatLUF,(int4(siNode)**2)*4)
          call CopySqRMat0(GMatrix,GMatLUF,siNode)
          Singular=.false. ! full-dim G is likely Singular due to redundant loops
          call LUFactorByCroutsMM(siRank,GMatLUF,Singular) ! G is symmetric & diag-dominant
          if(Singular) call HaltWith(1,siRank,'rank of the admittance matrix is no larger than')
        end if ! else prior-computed GMatLUF is still valid
      ! NodeCurrent=ConsRHS(nLines:siCons) ! call cmove(ConsRHS(nLines),NodeCurrent,int4(nNodes)*4)
        call CopyLinRVec0(ConsRHS(nLines),NodeCurrent,nNodes)
        call SolveByCroutsMM(siRank,GMatLUF,NodeVoltage,NodeCurrent)
        if(PrtDetail>2) then ! show the product of GMatrix and NodeVoltage
          write(OF1,Fmtxa) '   i       vi     Gi*v'
          do i=0,siNode
            r=0.0
            do j=0,siNode
              r=r+GMatrix(i,j)*NodeVoltage(j)
            end do
            write(OF1,'(1x,i4,2f9.4)') i,NodeVoltage(i),r
          end do
        end if
        write(OF1,*) ClkTime,clk(),' after solving G*v=i'

        do i=0,mSup
          BasisCol(i)=InvalidIndx
        end do
        do j=0,nSup
          BasisPos(j)=InvalidIndx
          SegWeight(j)=0.0 ! last nNodes entries are node voltages
          UpperBounded(j)=.false.
          if(j<nLinX4) then
            UB(j)=CUB ! SegWeights are limited to unity
          else
            UB(j)=LargeReal ! node voltages are unlimited
            c(j)=0.0 ! node voltages are not subject to optimization
          end if
          if(PrtDetail>3) &
            call WritemVector(AMatrix(0,j),' Constrt col '//Char4Eqv(j))
        end do
      ! preserve the original RHS for verification of results
      ! b=ConsRHS ! call cmove(ConsRHS,b,int4(siCons+1)*4)
        call CopyLinRVec0(ConsRHS,b,siCons+1)

        if(PrtDetail>0) write(OF1,Fmtxa) &
          ' iLn iNd jNd   j j/4 j%4 Current SgWeight Bound'
        do iLine=0,siLine ! assign SegWeight values
        ! all lines must be checked re upper bounds for a complete solution
          iNode=LineEnd(0,iLine)
          jNode=LineEnd(1,iLine)
          jCol=iLine*4 ! for in-limit positive current from iNode to jNode
          CompCurrent=yLine(iLine)*(NodeVoltage(iNode)-NodeVoltage(jNode))
          if(PrtDetail>3) write(OF1,'(1x,6i4,f8.2,17x,a)') iLine,iNode, &
            jNode,jCol,jCol/4,mod(jCol,int(4,2)),CompCurrent, &
            LineName(iLine)
          if(CompCurrent>=0.0) then
            u(0)=CurrLimit(0,iLine)
          else
            u(0)=CurrLimit(1,iLine)
            jCol=jCol+2 ! for in-limit negative current from iNode to jNode
          end if
          SegWeight(jCol)=CompCurrent/u(0) ! ratio>1 => infeasibility
          xB(iLine)=SegWeight(jCol)
          BasisCol(iLine)=jCol
          BasisPos(jCol)=iLine
          if(PrtDetail>0) write(OF1,'(1x,6i4,f8.2,f9.6,1x,L1,1x,a)') &
           iLine,iNode,jNode,jCol,jCol/4,mod(jCol,int(4,2)),CompCurrent, &
           SegWeight(jCol),UpperBounded(jCol),LineName(iLine)
        end do

      ! xB(nLines:mSup)=NodeVoltage ! call cmove(NodeVoltage,xB(nLines),int4(siNode)*4) ! explete xB
        call CopyLinRVec0(NodeVoltage,xB(nLines),siNode) ! explete xB
        do j=0,siRank ! the last basis vars are the node voltages
          BasisPos(nLinX4+j)=nLines+j
          BasisCol(nLines+j)=nLinX4+j
        end do

        if(PrtDetail>3) then ! show the product of AMatrix and SegWeight
          do j=nNodes,mSup
            NodeVoltage(j)=0.0
          end do
          call WritemVector(NodeVoltage,' NV')
          call WritemVector(xB,' xB')
          zO=0.0
          do j=0,nSup
            if(UpperBounded(j).eqv..true.) then
              zO=zO+c(j)*UB(j)
            else
              zO=zO+c(j)*SegWeight(j)
            end if
          end do
          write(OF1,Fmtxf12a) zO,'=z by def'
          write(OF1,Fmtxa) '   i ä(Aij*xj), j on [0,siLnX4]'
          do i=0,siCons
            r=0.0
            do j=0,siLnX4
              if(UpperBounded(j).eqv..true.) then
                r=r+AMatrix(i,j)*UB(j)
              else
                r=r+AMatrix(i,j)*SegWeight(j)
              end if
            end do
            write(OF1,Fmtif9if12) i,r
          end do ! last nNodes elements of SegWeight are still zero
        ! note that the LP ignores SegWeight, uses xB in its stead
        ! SegWeight(nLinX4:nSup)=NodeVoltage ! call cmove(NodeVoltage,SegWeight(nLinX4),int4(nNodes)*4)
          call CopyLinRVec0(NodeVoltage,SegWeight(nLinX4),nNodes)
          write(OF1,Fmtxa) '   i ä(Aij*xj), j on [0,nSup]'
          do i=0,siCons
            r=0.0
            do j=0,nSup
              if(UpperBounded(j).eqv..true.) then
                r=r+AMatrix(i,j)*UB(j)
              else
                r=r+AMatrix(i,j)*SegWeight(j)
              end if
            end do
            write(OF1,Fmtif9if12) i,r
          end do
        end if

        Int4mReq=int(mReq,4)
        if(NewStructure) then ! compute a fresh version of BMatInv
          do i=0,mSup ! assign basic columns of AMatrix to BMatrix
            j=BasisCol(i)
            if(j==InvalidIndx) call HaltWith(1,1,'invalid BC')
            do k=0,mSup
              BMatrix(i,k)=AMatrix(k,j) ! discontiguous in row-primary B
            end do
          end do
          if(PrtDetail>2) then
            do i=0,mSup
              call WritemVector(BMatrix(0,i),' BMat row '//Char4Eqv(i))
            end do
          end if
          allocate(BMatLUF(0:mSup,0:mSup),stat=AS)
          call CkAllocSt('BLUF',0) ! deallocated about 30 lines below
          allocate(PivotRow      (0:mSup),stat=AS)
          call CkAllocSt('PRow',0) ! deallocated about 30 lines below
          allocate(SupMagInRow   (0:mSup),stat=AS)
          call CkAllocSt('SMiR',0) ! deallocated a few lines below
        ! BMatLUF=BMatrix ! call cmove(BMatrix,BMatLUF,(Int4mReq**2)*4) ! preserve the original B before LU-factorization
          call CopySqRMat0(BMatrix,BMatLUF,mReq) ! preserve the original B before LU-factorization
          call LUFactorByCroutsMMwPP(mSup,BMatLUF,Singular,PivotRow,SupMagInRow) 
                                                                  ! ,SwapVector) ! B is generally not diagonally dominant
          deallocate(SupMagInRow,stat=AS)
          call CkAllocSt('SMiR',1)
          if(Singular) call HaltWith(1,mReq,'B-matrix has rank <')
          do j=0,mSup ! get inverse by solution of B*BInv(j)=ej
            ej = 0.
            ej(j)=1.0
            call SolveByCroutsMMwPP(mSup,BMatLUF,BMatInv(0,j),ej,PivotRow)
            if(PrtDetail>3) then
              write(OF1,Fmta3i2e)'   i       Xi RHSi B*X-RHS @BInvCol ',j
              do i=0,mSup ! row index
                r=-ej(i)
                do k=0,mSup
                  r=r+BMatrix(k,i)*BMatInv(k,j) ! B is row-primary, BInv is col-primary
                end do
                write(OF1,Fmtif9if12) i,BMatInv(i,j),ej(i),r
              end do
            end if
            if(PrtDetail>2) call WritemVector(BMatInv(0,j),' BInv col '//Char4Eqv(j))
          end do
          deallocate(PivotRow,stat=AS)
          call CkAllocSt('PRow',1)
          deallocate(BMatLUF ,stat=AS)
          call CkAllocSt('BLUF',1)
          write(OF1,*) ClkTime,clk(),' after inverting B'

          if(PrtDetail>3) then ! check whether basis satisfies constraints
            call WritemVector(xB,' xB')
            zO=0.0
            do j=0,nSup
              if(UpperBounded(j).eqv..true.) then
                zO=zO+c(j)*UB(j)
              elseif(BasisPos(j)/=InvalidIndx) then
                zO=zO+c(j)*SegWeight(j)
              end if
            end do
            write(OF1,Fmtxf12a) zO,'=z (basis and UB only)'
            write(OF1,Fmtxa) '   i    Ai*sw (basis and UB only)'
            do i=0,siCons
              r=0.0
              do j=0,nSup
                if(UpperBounded(j).eqv..true.) then
                  r=r+AMatrix(i,j)*UB(j)
                else
                  r=r+AMatrix(i,j)*SegWeight(j)
                end if
              end do
              write(OF1,Fmtif9if12) i,r
            end do
            write(OF1,Fmtxa) '   i    Bi*xB (including UB)'
            do i=0,mSup ! row index
              r=0.0
              do j=0,mSup ! col index
                r=r+BMatrix(j,i)*SegWeight(BasisCol(j))
              end do
              do j=0,nSup
                if(UpperBounded(j).eqv..true.) r=r+AMatrix(i,j)*UB(j)
              end do
              write(OF1,Fmtif9if12) i,r
            end do
          end if
        ! BMatrix=BMatInv ! call cmove(BMatInv,BMatrix,(Int4mReq**2)*4) ! preserve the inverse of B
          call CopySqRMat0(BMatInv,BMatrix,mReq) ! preserve the inverse of B
        else ! AMatrix has not changed; reinstate the original BMatInv
        ! BMatInv=BMatrix ! call cmove(BMatrix,BMatInv,(Int4mReq**2)*4) ! recall the inverse of B
          call CopySqRMat0(BMatrix,BMatinv,mReq) ! recall the inverse of B
        end if ! re NewStructure
      ! xB, UB, BMatInv, BasisPos, BasisCol, & UpperBounded are well-defined by here

        if(PrtDetail>0) then
          write(OF1,Fmtxa)
          if(PrtDetail>2) then
            do j=0,nSup
              call WritemVector(AMatrix(0,j),' Constrt col '//Char4Eqv(j))
            end do
          end if
          call WritemVector(b,' b')
          call WritenVector(c,' c')
          Optimal=.false.
        end if

        do i=0,mSup
          cB(i)=c(BasisCol(i))
        end do
        if(PrtDetail>2) call WritemVector(cB,' cB')

        if(NewStructure) then
        ! use Y=BInv*A to get z(j)-c(j) corresponding to the precomputed xB
          do j=0,nSup ! loop across columns of AMatrix
          ! O((n-m)*2*m^2) operations just to get initial z_c(j)
            if(BasisPos(j)/=InvalidIndx) then
              z_c(j)=0.0 ! aj is in basis
            else  ! aj is not in basis; z_c(j)=(cB,yj)-c(j)
              call GetYColumn(j,yj) ! O(2*m^2) operations
              if(PrtDetail>3) call WritemVector(yj,' Y col '//Char4Eqv(j))
            ! zj=sum (over i on [0,m-1] of) yji*cBi
              z_c(j)=ContInnerProduct(cB,yj,mSup)-c(j) ! =z(j)-c(j) ; O(2*m) ops
            end if
          end do
          NewStructure=.false. ! => GMatrix and BMatrix need not be inverted again
        ! zOrg_c=z_c ! call cmove(z_c,zOrg_c,int4(nVar)*4) ! preserve the original vector
          call CopyLinRVec0(z_c,zOrg_c,nVar) ! preserve the original vector
        else ! assume cost vector does not change until NewStructure is true again
        ! z_c=zOrg_c ! call cmove(zOrg_c,z_c,int4(nVar)*4) ! recall the original vector
          call CopyLinRVec0(zOrg_c,z_c,nVar) ! recall the original vector
        end if

        if(PrtDetail>2) then
          zO=ContInnerProduct(cB,xB,mSup)
          call AugmentZoByBoundedNB
        end if
        nIter=0
      ! these local vectors are not useful outside this entry-point
        deallocate(cB         ,stat=AS)
        call CkAllocSt('cB_V',1)
        deallocate(NodeVoltage,stat=AS)
        call CkAllocSt('NodV',1)
        deallocate(NodeCurrent,stat=AS)
        call CkAllocSt('NodC',1)
      return ! entry InitiatePrecomputedSolution


      entry ModifyInvPerColSwap(rMI,rRow)
      ! effect the basis-swap in BInv:  BInv column rDel for column kIns of A
      ! integer*2 rMI,iMI,jMI
      ! real*4 rRow(0:*)

        do i=0,mSup
          rRow(i)=BMatInv(rMI,i) ! capture row r of BInv before modification
        end do
        if(PrtDetail>3) then
          call WritemVector(rRow,' orgBInv row '//Char4Eqv(rMI))
          do jMI=0,mSup
            call WritemVector(BMatInv(0,jMI),' orgBInv col '//Char4Eqv(jMI))
          end do
        end if
        do iMI=0,mSup ! add Phi*rRow to BInv as if Eta in col r of Im
          do jMI=0,mSup
            BMatInv(iMI,jMI)=BMatInv(iMI,jMI)+Phi(iMI)*rRow(jMI) ! O(2*m^2) operations
          end do
        end do
        if(PrtDetail>3) then
          do jMI=0,mSup
            call WritemVector(BMatInv(0,jMI),' modBInv col '//Char4Eqv(jMI))
          end do
        end if
      return ! entry ModifyInvPerColSwap


      entry AugmentZoByBoundedNB
      ! integer*2 jAZ

        if(PrtDetail>3) call WritebVector(UpperBounded,' UpperBounded')
        do jAZ=0,nSup
          if(UpperBounded(jAZ).eqv..true.) then !.and. (BasisPos(jAZ)==InvalidIndx)
            if(PrtDetail>3) write(OF1,'(1x,i4,f9.3,f6.1,f12.6)')jAZ,c(jAZ),UB(jAZ),zO
            zO=zO+c(jAZ)*UB(jAZ)
            if(PrtDetail>3) write(OF1,Fmtxf12a) zO,' AZBBNB'
          end if
        end do
      return ! entry AugmentZoByBoundedNB


      entry GetYColumn(jGYC,yjGYC) ! O(2*m^2) operations

        do iGYC=0,mSup ! compute column j of Y per yj=BInv*aj ! O(2*m) operations
          yjGYC(iGYC)=DiscInnerProduct(BMatInv,AMatrix(0,jGYC),iGYC,mSup)
        end do
      return ! entry GetYColumn


      entry AdjustxBforUB(yColumn,Bound)

       ! adjust xB for the inserted/deleted UB slack
        if(PrtDetail>2) call WritemVector(xB,' xB pre-adjReUB')
        do iAxBfUB=0,mSup
          xB(iAxBfUB)=xB(iAxBfUB)+Bound*yColumn(iAxBfUB) ! O(2*m) operations
        end do
        if(PrtDetail>2) call WritemVector(xB,' xB aft-adjReUB')
      return ! entry AdjustxBforUB


      entry CheckFeasibility ! O(5*m) operations
      ! check restricted vars for non-negative and UB-overflow values
      ! real*4 Violation,SupViolation

        if(PrtDetail>3) write(OF1,'(1h0,a)') 'FeasibilityCheck:'
        iMostInfeas=InvalidIndx
        SupViolation=SmallReal ! tolerate a small excess due to numeric errors
        do i=0,mSup
          j=BasisCol(i)
          if(j<=siRest) then ! check vars restricted to [0,UB]
            if(xB(i)<0.0) then
              Violation=-xB(i)
            else
              Violation= xB(i)-UB(j)
            end if
            if(SupViolation<Violation) then ! save index of worst-case var
              SupViolation=Violation
              iMostInfeas=i
            end if
          end if ! else var j is unrestricted in sign and magnitude
          if(PrtDetail>3) write(OF1,'(1x,a,3i4,f9.5,3e10.3)') &
            SelectingrDel,iMostInfeas,i,j,xB(i),UB(j), &
            Violation,SupViolation
        end do
        Feasible=(iMostInfeas==InvalidIndx)
        if(PrtDetail==2) write(OF1,'(1x,a,i4,e10.3)') &
          SelectedrDel,iMostInfeas,SupViolation
        if(PrtDetail>2) write(OF1,'(1x,a,L1)') 'FeasibilityChecked ', &
          Feasible
      return ! entry CheckFeasibility


      entry OptimizeSolution ! with notation per Hadley, pp. 110-112
      ! with treatment of upper-bounded xj per Hadley, pp. 387-394
      ! label SelectInsAndDel
      ! logical*1 Unbounded,UBkIns,UBrDel !,RobustPair
      ! integer*2 iDel,jA,jDel,jCd4,kIns,rDel,rDelPos,rDelNeg
      ! real*4 xBr,yik,yrj,yrk,z_cSup,z_ckIns,
      !+  Ratio,InfRatio,InfRatioPos,InfRatioNeg !,InfRatioLim

      ! zStar_c=z_c ! call cmove(z_c,zStar_c,int4(nVar)*4)
        call CopyLinRVec0(z_c,zStar_c,nVar)
        do j=0,nSup
          if(UpperBounded(j).eqv..true.) zStar_c(j)=-z_c(j)
        end do
        if(PrtDetail>0) rDel=InvalidIndx
        if(PrtDetail>1) call WritebVector(UpperBounded,' UpperBounded')
        if(PrtDetail>1) call WritemVector(xB,' xB')
        if(PrtDetail>2) call WritenVector(z_c,' z-c')
        if(PrtDetail>1) call WritenVector(zStar_c,' z*-c')

      ! SelectInsAndDel:

        if(Optimal.and..not.Feasible) then ! use Dual-Simplex basis-change criteria
          if(PrtDetail>1) &
             write(OF1,Fmtxa) 'optimal but infeasible => Dual-Simplex:'
          UBrDel=xB(iMostInfeas)>0.0
          if(UBrDel) then ! force use of case 1b below
            rDelPos=InvalidIndx
            rDelNeg=iMostInfeas
            rDel=rDelNeg
          else  ! force use of case 1a below
            rDelPos=iMostInfeas
            rDelNeg=InvalidIndx
            rDel=rDelPos
          end if
          InfRatioNeg=LargeReal ! infimum across ratios of z_c/yrj with yrj<0
          if(PrtDetail==2) Ratio=LargeReal
          kIns=InvalidIndx ! indicating not yet assigned
        ! to maintain a basis of full rank, the column inserted into B must, in
        ! the current case of dependent columns of A located in contiguous groups
        ! of 4, be that basis vector with the same group index as that deleted
          jCd4=BasisCol(rDel)/4 ! group index
          jDel=jCd4*4 ! least index within group
          do j=jDel,jDel+3
            if(BasisPos(j)==InvalidIndx) then
            ! scan non-basic vars in row rDel of Y for the most-limiting ratio
              yrj=DiscInnerProduct(BMatInv,AMatrix(0,j),rDel,mSup) ! O(2*m) operations
              if(UBrDel) yrj=-yrj
            ! negation above is due to rDel becoming UpperBounded, Hadley page 391
            ! if((mod(j,4)>1) yrj=-yrj; ! dubious
              if(yrj<-SmallReal) then ! restrict comparison to negative yrj
                Ratio=zStar_c(j)/yrj ! positive since zStar_c(j)<=0 ideally
                if((InfRatioNeg>Ratio).and.(zStar_c(j)<=0.0)) then ! reject pos noise
                  InfRatioNeg=Ratio
                  kIns=j ! retain the infimum, with index kIns
                end if
                if(PrtDetail>2) write(OF1,Fmta2i2f2e) &
                  SelectingkIns,kIns,j,zStar_c(j),yrj,Ratio,InfRatioNeg
              else
                if(PrtDetail>2) write(OF1,'(15x,i12,2f9.3,e13.4)') &
                  j,zStar_c(j),yrj,yrj
              end if
              if(PrtDetail==2) write(OF1,Fmta2i2f2e) &
                  SelectingkIns,kIns,j,zStar_c(j),yrj,Ratio,InfRatioNeg
              InfRatioPos=InfRatioNeg ! to keep later branches happy
            end if
          end do ! with kIns assigned

          call GetYColumn(kIns,yk) ! O(2*m^2) operations
        ! do not check here for an unbounded solution; Hadley page 247:  'if the
        ! primal has an unbounded solution, [one] can never find a basic solution
        ! to the primary with all zj-cj>=0.' [in a maximization context]
          if(PrtDetail>2) &
            call WritemVector(yk,' yk a priori@ '//Char4Eqv(kIns))
        ! end use of Dual-Simplex basis-change criteria
        else ! use Revised-Simplex basis-change criteria
          if(PrtDetail>1) &
            write(OF1,Fmtxa) 'feasible => Revised Simplex:'
          Optimal=.true. ! default hypothesis, rejected below if zO is improvable
          kIns=InvalidIndx ! indicating not yet assigned
        ! z_cInf=0.0; ! search for most-negative z_c; if maximizing
          z_cSup=0.0 ! search for most-positive z_c
        ! do j=0,siRest ! selection of vector to be inserted
        ! above allowed co-linear vectors to exist in the RS-phase solution which
        ! mistakenly cancelled costs; restrict RS-phase to even indices kIns
          do j=0,siRest,2 ! selection of vector to be inserted
          ! j is limited since unrestricted variables never leave the basis
          ! if(zStar_c(j)<-SmallReal) then  ! if maximizing
            if(zStar_c(j)> SmallReal) then
            ! Optimal=false;
            ! assign kIns to that j not in basis with the most favorable z_c
              if((BasisPos(j)==InvalidIndx).and. & ! aj is not in basis
                (z_cSup<zStar_c(j))) then
                Optimal=.false.
                z_cSup=zStar_c(j) !*NoiseThresh; ! make next comparison harder to pass
                kIns=j ! column of A to be inserted into basis
              end if
              if(PrtDetail==2) write(OF1,Fmta3i2e) &
                SelectingkIns,kIns,j,BasisPos(j),zStar_c(j),z_cSup
            end if
            if(PrtDetail>2) write(OF1,Fmta3i2e) &
              SelectingkIns,kIns,j,BasisPos(j),zStar_c(j),z_cSup
          end do ! with kIns on [0,nSup] if solution is not Optimal

          if(.not.Optimal) then ! replace basis vector B(.,rDel) with A(.,kIns)
            call GetYColumn(kIns,yk) ! O(2*m^2) operations
            Unbounded=.true.
            i=-1 ! 0 for 1-based vectors
            do while((i<mReq).and.Unbounded) ! check only the inserted vector
              i=i+1
              Unbounded=(yk(i)<=0.0) ! quit loop at first positive yik
            end do
            if(Unbounded.and.(PrtDetail>1)) call WritemVector(yk,' Yk')
            if(Unbounded) call HaltWith(i,kIns,'unbounded solution involving var')

            if(PrtDetail==2) Ratio=LargeReal
            if(PrtDetail>2) &
              call WritemVector(yk,' yk a priori@ '//Char4Eqv(kIns))
            InfRatioPos=UB(kIns) ! infimum across positive elements of yk
            InfRatioNeg=UB(kIns) ! infimum across negative elements of yk
      !     InfRatioLim=LargeReal ! possibly reassigned to Ratio for iDel
            rDelPos=InvalidIndx ! indicating not yet assigned
            rDelNeg=InvalidIndx ! indicating not yet assigned
          ! to maintain a basis of full rank, the column deleted from B must, in
          ! the current case of dependent columns of A located in contiguous groups
          ! of 4, be that basis vector with the same group index as that inserted
            jCd4=kIns/4 ! group index
            iDel=0
            do while(((BasisCol(iDel)/4)/=jCd4).and.(iDel<mSup))
              iDel=iDel+1
            end do
            if((BasisCol(iDel)/4)/=jCd4) call HaltWith(iDel,jCd4,'cannot match group')
            i=iDel
            yik=yk(i)
            if(UpperBounded(kIns).eqv..true.) yik=-yik ! Hadley page 391
            if(yik>SmallReal) then ! restrict comparison to positive yik
            ! when the criterion above was 0, nonsensical solutions were reached
              Ratio=xB(i)/yik ! non-negative since xBi>=0 ideally
      !       if(i==iDel) InfRatioLim=Ratio
              if((InfRatioPos>Ratio).and.(xB(i)>=0.0)) then ! reject neg noise
              ! for x values normalized to unity, 1e-6 above caused some negative x
              ! InfRatioPos=Ratio/NoiseThresh ! make next comparison harder to pass
                InfRatioPos=Ratio ! ideal InfRatioPos = xBr/yrk on exit of the loop
                rDelPos=i ! retain the infimum, with index rDelPos
              end if
              if(PrtDetail>2) write(OF1,'(1x,a,a,i4,i8,2f9.3,2e13.4)') &
                SelectingrDel,'Pos',rDelPos,i,xB(i),yik, &
                Ratio,InfRatioPos 
            elseif((yik<-SmallReal) & ! restrict comparison to negative yik
              .and.(BasisCol(i)<nRestr)) then ! unrestricted vars have no upper bound
              jA=BasisCol(i)
              Ratio=(xB(i)-UB(jA))/yik ! non-negative since xBi<=1 ideally
              if((InfRatioNeg>Ratio).and.(xB(i)<=UB(jA))) then ! reject pos noise
                InfRatioNeg=Ratio ! ideal InfRatioNeg = (UB-xBr)/-yrk on exit of the loop
                rDelNeg=i ! retain the infimum, with index rDelNeg
              end if
              if(PrtDetail>2) write(OF1,'(1x,a,a,i8,i4,2f9.3,2e13.4)') &
                SelectingrDel,'Neg ',rDelNeg,i,xB(i),yik, &
                Ratio,InfRatioNeg
            end if
            if(PrtDetail==2) write(OF1,'(1x,a,3i4,2f9.3,3e13.4)') &
              SelectingrDel,rDelPos,rDelNeg,i, &
              xB(i),yik,Ratio,InfRatioPos,InfRatioNeg
          ! end selection of vector to be replaced, with rDel on [0,mSup]
          end if ! not Optimal
          if(PrtDetail>0) then
            rDel=InvalidIndx
            jDel=InvalidIndx
          end if
        end if ! use of Revised-Simplex basis-change criteria

        if(.not.(Optimal.and.Feasible)) then
          if((rDelPos/=InvalidIndx).and.(rDelNeg/=InvalidIndx)) then
            if(InfRatioPos<InfRatioNeg) then
              rDel=rDelPos
            else
              rDel=rDelNeg
            end if
          elseif(rDelPos/=InvalidIndx) then
            rDel=rDelPos
          elseif(rDelNeg/=InvalidIndx) then
            rDel=rDelNeg
          else
            rDel=InvalidIndx ! both had InvalidIndx
          end if
          if(PrtDetail>0) then
            if(rDel==InvalidIndx) then
              jDel=InvalidIndx
            else
              jDel=BasisCol(rDel)
            end if
            if(PrtDetail>1) write(OF1,Fmta3i2e) SelectedrDel,rDel, &
              rDelPos,rDelNeg,InfRatioPos,InfRatioNeg,jDel
          end if

    !     if( ((rDel==rDelPos).and.(InfRatioPos<ConstrtTol))
    !+      .or.((rDel==rDelNeg).and.(InfRatioNeg<ConstrtTol))) then
    !     ! continue searching for a more-robust basis-change-pair, since
    !     ! the effect of InfRatio upon xB would be insignificant
    !       zStar_c(kIns)=0.0 ! exclude kIns from further consideration ...
    !       if(PrtDetail>2) write(OF1,FmtLong)
    !+        kIns,' ignored as not robust for insertion'
    !       goto SelectInsAndDel ! ... until a later iteration
    !     end if

          nIter=nIter+1
          UBkIns=UpperBounded(kIns)
          if(UBkIns) then ! case 2 on Hadley page 394
            if(rDel==InvalidIndx) then ! case 2c
            ! kIns becomes basic after being upper-bounded
              UpperBounded(kIns)=.false.
            ! delay adjustment of xB until after transformation;
      !         BMatInv, BasisCol and BasisPos remain as before
              if(PrtDetail>1) write(OF1,Fmtxa) 'case 2c'
              InfRatio=1.0 ! useful only for PrtDetail>0
            elseif(rDel==rDelPos) then
              InfRatio=InfRatioPos ! case 2a
            else
              InfRatio=InfRatioNeg ! case 2b; rDel==rDelNeg
            end if
          else  ! case 1 on Hadley page 394
            if(rDel==InvalidIndx) then ! case 1c
            ! kIns becomes upper-bounded after being basic
              UpperBounded(kIns)=.true.
            ! delay adjustment of xB until after transformation;
            ! BMatInv, BasisCol and BasisPos remain as before
              if(PrtDetail>1) write(OF1,Fmtxa) 'case 1c'
              InfRatio=1.0 ! useful only for PrtDetail>0
            elseif(rDel==rDelPos) then
              InfRatio=InfRatioPos ! case 1a (normal)
            else
              InfRatio=InfRatioNeg ! case 1b; rDel==rDelNeg
            end if
          end if

          if(rDel/=InvalidIndx) then
          ! InfRatio=InfRatio*NoiseThresh ! restore ideal value
            jDel=BasisCol(rDel)
            BasisPos(jDel)=InvalidIndx
            BasisPos(kIns)=rDel
            BasisCol(rDel)=kIns ! effect the basis column-vector replacement
            z_ckIns=z_c(kIns)
            yrk=yk(rDel)
            xBr=xB(rDel)
            do i=0,mSup
              Phi(i)=-yk(i)/yrk ! O(2*m) operations
            end do
            Phi(rDel)=(1.0/yrk)-1.0 ! cf. Hadley pages 50,134
            if(PrtDetail>2) call WritemVector(Phi,' Phi')

            do i=0,mSup ! modify xB using Phi, with O(2*m) operations
              xB(i)=xB(i)+xBr*Phi(i)
            end do
            if(PrtDetail>2) call WritemVector(xB,' xB aft Phi-mod')

            do j=0,nSup ! revise z_c, O((n-m)*2*m) operations
            ! first principles z_c(j)=(cB,yj)-c(j) uses O((n-m)*2*m^2) ops
              if(BasisPos(j)/=InvalidIndx) then
                z_c(j)=0.0 ! aj is in basis
              else ! aj is not in basis; z_c(j)=z_c(j)-z_c(kIns)*yrj/yrk
                z_c(j)=z_c(j) & ! note:  uses matrix Y via BInv before transformation
                  -z_ckIns*DiscInnerProduct(BMatInv, &
                  AMatrix(0,j),rDel,mSup)/yrk
              end if
            end do
          ! the potential time-savings warrant checking for dependent-col swaps
            jCd4=jDel/4
          ! if((iabs(kIns-jDel)==2).and.(jCd4==(kIns/4)).and.
            if((iabs(kIns-jDel)==2).and. &
                 (SymmetricLimits(jCd4).eqv..true.)) then ! assume j/4 = k/4
            ! columns swapped are linearly dependent, negatives of one another;
            ! negating column rDel of BMatrix requires
            ! negating row    rDel of BMatInv to maintain Identity as product
              do i=0,mSup
                BMatInv(i,rDel)=-BMatInv(i,rDel) ! O(m) ops
              end do
            else
              call ModifyInvPerColSwap(rDel,SwapVector) ! O(2*m^2) operations
            end if

            if(UpperBounded(kIns).eqv..true.) then ! cases per Hadley page 394
              UpperBounded(kIns)=.false.
              xB(rDel)=xB(rDel)+UB(kIns) ! adjust for the inserted UB slack
              if(PrtDetail>2) call WritemVector(xB,' xB aft aUB@2ab')
              if(rDel==rDelPos) then
                if(PrtDetail>1) write(OF1,Fmtxa) 'case 2a'
              else  ! =rDelNeg
                UpperBounded(jDel)=.true.
                call GetYColumn(jDel,yj) ! O(2*m^2) operations
                call AdjustxBforUB(yj,-UB(jDel)) ! O(2*m) operations
                if(PrtDetail>1) write(OF1,Fmtxa) 'case 2b'
              end if
            elseif(rDel==rDelPos) then
              UpperBounded(jDel)=.false.
              if(PrtDetail>1) write(OF1,Fmtxa) 'case 1a'
            elseif(rDel==rDelNeg) then
              UpperBounded(jDel)=.true.
              call GetYColumn(jDel,yj) ! O(2*m^2) operations
              call AdjustxBforUB(yj,-UB(jDel)) ! O(2*m) operations
              if(PrtDetail>1) write(OF1,Fmtxa) 'case 1b'
            end if
          else ! BMatInv, BasisCol and BasisPos remain as before
            if(UBkIns) then
              call AdjustxBforUB(yk, UB(kIns))  ! case 2c ; O(2*m) ops
            else
              call AdjustxBforUB(yk,-UB(kIns)) ! case 1c ; O(2*m) ops
            end if
          end if ! rDel==InvalidIndx
        end if ! not (Optimal and Feasible)

        if(PrtDetail>0) then
          zO=0.0
          do i=0,mSup
            zO=zO+c(BasisCol(i))*xB(i)
          end do
          if(PrtDetail>3) &
            write(OF1,'(1x,f24.3,a)') zO,'=z by IP, excl. UB effects'
          call AugmentZoByBoundedNB
          if(Optimal) InfRatio=0.0
          if(nIter==1) write(OF1,Fmtxa) &
            'Iter Del Ins           z  InfRatio  '// &
            '(Inf)z-c <0 if suboptimal'
          write(OF1,'(1x,3i4,f12.3,2e10.3)') &
            nIter,jDel,kIns,zO,InfRatio,z_cSup
! 
        end if
        if(nIter>(mReq*2)) & ! cf. Hadley page 134
          call HaltWith(nIter,mReq*2,'iterations exceed 2m-limit of')
      return ! entry OptimizeSolution


      entry LPreport
      !

        write(OF1,*) ClkTime,clk(),' after obtaining the solution'
        if(PrtDetail>0) then ! post-solution reporting of results
          InBounds=.true.
          write(OF1,'(1x,i4,a,f12.3,a)') nIter,'=nIter ',zO,'=z'
          call WritebVector(UpperBounded,' UpperBounded')
          call WritemVector(xB,' xB')

          write(OF1,Fmtxa)
          write(OF1,Fmtxa) 'variables in order of constraint relations:'
          write(OF1,Fmtxa) &
            '   i   j xType x(j)=xB(i)  x(j)=xB(i)      '// &
            '(B(i),xB)     b(i) ReType'
          do i=0,siCons ! check whether constraints are met by xB
            j=BasisCol(i)
            if(i>mSup) then
              xj=0.0
            else
              xj=xB(i)
            end if
            do k=0,nSup
              ConstrtRow(k)=AMatrix(i,k)
            end do
            r=0.0
            do k=0,mSup
              r=r+ConstrtRow(BasisCol(k))*xB(k)
            end do
            do k=0,nSup ! incr IP by boundeds
              if(UpperBounded(k).eqv..true.) r=r+ConstrtRow(k)*UB(k)
            end do
            if(i>mSup) then
              write(OF1,Fmtx2ianr) i,j,' Slack'
            else
              write(OF1,Fmtx2ianr) i,j,' Prime'
            end if
            write(OF1,'(1x,e10.3,f12.6,f15.9,f9.3,a)') &
              xj,xj,r,ConsRHS(i),' Equty'
          end do

          write(OF1,'(1x,2i4,a)') mReq,nVar,' basis-size and # vars'
          write(OF1,Fmtxa)
        end if

        if(PrtDetail>0) write(OF1,Fmtxa) 'jVar   SegWeight iTL Bound'
        do j=0,nSup ! obtain values of original vars
          i=BasisPos(j)
          if(i==InvalidIndx) then ! aj is not in basis
            if(UpperBounded(j).eqv..true.) then
              SegWeight(j)=UB(j)
            else
              SegWeight(j)=0.0
            end if
          else
            SegWeight(j)=xB(i) ! aj is in basis
          end if
          if(PrtDetail>0) then
            write(OF1,'(1x,i4,f12.6\)') j,SegWeight(j)
            if(j<=siLnX4) then
              write(OF1,'(i4,1x,L1)') j/4,UpperBounded(j)
              if(mod(j,int(4,2))==3) write(OF1,Fmtxa)
            else
              write(OF1,Fmtxa)
            end if
          end if
        end do

        if(PrtDetail>0) then
          write(OF1,Fmtxa) &
            'iRow     Current re   Kirchoff-nodal constraint'
          do i=nLines,siCons
            do j=0,nSup
              ConstrtRow(j)= AMatrix(i,j) ! discontiguous in A
            end do
            if(PrtDetail>3) then
              call WritenVector(ConstrtRow,'Constrt row '//Char4Eqv(i))
              call WritenVector(SegWeight,'SegWeight')
            end if
            CompCurrent=ContInnerProduct(ConstrtRow,SegWeight,nSup)
            call CheckEql(CompCurrent,ConsRHS(i))
            write(OF1,'(1x,i4,f12.6,2a1,f12.6)') &
              i,CompCurrent,ABlank,RelChar,ConsRHS(i)
          end do
        end if

        write(OF0,'(1x,a,i4,a)') &
          'The following numerics apply to a network operating at', &
          TrVoltage,' kV;'
        write(OF0,Fmtxa) &
          'voltages are in kV, currents in kA, powers in MW.'
        write(OF0,Fmtxa)
        write(OF0,Fmtxa) &
          ' iTL Beg End VoltageDrop Current LinePwrLoss LineName'
        if(PrtDetail>0) write(OF1,Fmtxa) &
          'jVar iTL Beg End VoltageDrop Current re   Limit '// &
          'LinePwrLoss LineName'
        LinePwrLosses=0.0
        do iLine=0,siLine
          jCol=iLine*4
          CompCurrent=0.0
          do i=0,1 ! 0=> positive, 1=> negative current
            u(0)=CurrLimit(i,iLine) ! below breakpoint
            u(1)=u(0)*OverloadLimit ! above breakpoint
            do j=0,1
              CompCurrent=CompCurrent+u(j)*SegWeight(jCol+2*i+j)
            end do
          end do
          r=CompCurrent/yLine(iLine) ! voltage drop
          LinePowerLoss=abs(CompCurrent*r)
          LinePwrLosses=LinePwrLosses+LinePowerLoss
          write(OF0,'(1x,3i4,f12.6,f8.3,f12.4,1x,a)') iLine, &
            LineEnd(0,iLine), &
            LineEnd(1,iLine),r,CompCurrent,LinePowerLoss,LineName(iLine)
          if(PrtDetail>0) then
            write(OF1,'(1x,4i4,f12.6,f8.3\)') &
              jCol,iLine,LineEnd(0,iLine),LineEnd(1,iLine),r,CompCurrent
            if(CompCurrent>=0.0) then ! check re pos limit
              call CheckLeq( CompCurrent, CurrLimit(0,iLine)*(1.0+OverloadLimit))
              write(OF1,Fmt3af8) ABlank,LtChar,RelChar,CurrLimit(0,iLine)
              RelChar=ABlank
              if(CompCurrent>CurrLimit(0,iLine)) RelChar=LimChar
            else ! CompCurrent<0.0; check re neg limit
              call CheckLeq(-CompCurrent,-CurrLimit(1,iLine)*(1.0+OverloadLimit))
              write(OF1,Fmt3af8) ABlank,GtChar,RelChar,CurrLimit(1,iLine)
              RelChar=ABlank
              if(CompCurrent<CurrLimit(1,iLine)) RelChar=LimChar
            end if
            write(OF1,'(a1,f11.4,1x,a)') RelChar,LinePowerLoss,LineName(iLine)
          end if
        end do
        write(OF0,'(1x,a,f14.4,a)') TPLiAL,LinePwrLosses,IfZRes

        if(PrtDetail>0) then
          write(OF1,'(1x,a,f29.4,a)') TPLiAL,LinePwrLosses,IfZRes
          if(InBounds) then
            write(OF1,Fmtxa) 'constraints and overload limits were met'
          else
            write(OF1,Fmtxa) &
              'constraint(s) or limit(s) violated; consider inc. OLL'
          end if
        end if
        write(OF1,*) ClkTime,clk(),' after checking the results'

      ! close(OF1)
      ! write(OF0,Fmtxa) 'Detailed report is in file LoadFlow_right.dbg'
      ! close(OF0)
      ! OF1=6
      ! write(*,Fmtxa) 'Output report is in file LoadFlow.out'
      ! call DeallocLPSVariables ! retain these until caller is done looping
      ! call HaltWith(0,0,'LPsolver completed execution')
      return ! entry LPreport


      entry WritebVector(vWbV,TitleWbV)
      ! logical*1 vWbV(0:*)
      ! character*(*) TitleWbV
      ! integer*2 jWbV,iWbV

        do jWbV=0,nSup
          if(vWbV(jWbV).eqv..true.) then
            iWbV=1
          else
            iWbV=0
          end if
          write(OF1,'(i2\)') iWbV
          if(mod(jWbV,int(32,2))==31) write(OF1,Fmtxa)
        end do
        write(OF1,Fmtxa) TitleWbV
      return ! entry WritebVector


      entry WritemVector(vWmV,TitleWmV)
      ! real*4 vWmV(0:*)
      ! character*(*) TitleWmV
      ! integer*2 jWmV

        do jWmV=0,mSup
          if(mod(jWmV,int(6,2))==0) write(OF1,Fmtanr) ABlank
          if    (abs(vWmV(jWmV))<99.9999) then
            write(OF1,Fmti4f8p4) BasisCol(jWmV),vWmV(jWmV)
          elseif(abs(vWmV(jWmV))<9999.99) then
            write(OF1,Fmti4f8p2) BasisCol(jWmV),vWmV(jWmV)
          elseif(abs(vWmV(jWmV))>99999999) then
            write(OF1,Fmti4e8p1) BasisCol(jWmV),vWmV(jWmV)
          else
            write(OF1,Fmti4i8  ) BasisCol(jWmV),vWmV(jWmV)
          end if
          if(mod(jWmV,int(6,2))==5) write(OF1,Fmtxa)
        end do
        write(OF1,Fmtxa) TitleWmV
      return ! entry WritemVector


      entry WritenVector(vWnV,TitleWnV)
      ! real*4 vWnV(0:*)
      ! character*(*) TitleWnV
      ! integer*2 jWnV

        do jWnV=0,nSup
          if(mod(jWnV,int(6,2))==0) write(OF1,Fmtanr) ABlank
          if    (abs(vWnV(jWnV))<99.9999) then
            write(OF1,Fmti4f8p4) jWnV,vWnV(jWnV)
          elseif(abs(vWnV(jWnV))<9999.99) then
            write(OF1,Fmti4f8p2) jWnV,vWnV(jWnV)
          elseif(abs(vWmV(jWmV))>99999999) then
            write(OF1,Fmti4e8p1) jWmV,vWmV(jWmV)
          else
            write(OF1,Fmti4i8  ) jWnV,vWnV(jWnV)
          end if
          if(mod(jWnV,int(6,2))==5) write(OF1,Fmtxa)
        end do
        write(OF1,Fmtxa) TitleWnV
      return ! entry WritenVector


      entry CheckEql(ValueCE,LimitCE)
      ! real*4 ValueCE,LimitCE,AbsD,AbsL

        AbsD=abs(ValueCE-LimitCE)
        AbsL=abs(LimitCE)
        if( (AbsD<=ConstrtTol*AbsL).or. &
           ((AbsD<=ConstrtTol).and.(AbsL<=SmallReal))) then
          RelChar=EqlChar
        else
          RelChar=NeqChar
          InBounds=.false.
        endif
      return ! entry CheckEql


      entry CheckLeq(ValueCL,LimitCL) ! assumes both parameters are non-negative
      ! real*4 ValueCL,LimitCL

        if((1.0-ConstrtTol)*ValueCL<=LimitCL) then
          RelChar=EqlChar
        else
          RelChar=NeqChar
          InBounds=.false.
        end if
      return ! entry CheckLeq


      end ! subroutine LPsolver
