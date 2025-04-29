! 
!Tue May  7 13:53:17 EDT 2024
! chhanged spinfunc.for to spinfunc.f90
!
!Last change: MSG 1/22/2004 12:56:02 PM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! following are approximations of the documented Spindrift routines:
! they require linking with .obj-files extracted from LF9555\lib\cw32.lib:
! they is useful under DOS and WinConsole, but not under Windows GUI.


module SpinFuncLocals
  integer (kind=2) ::   &
    ScnBufSiz, &
    SupCAOfs , &
    LimitEnvSize, &
    SupEnvOffset
  integer (kind=4) ::   &
    pVGATextPage, &
    pMDATextPage
!!colors and attributes defined by Spindrift:
  integer (kind=2) ::      &
    BLKCHR   , &
    DBLUCHR  , &
    GREENCHR , &
    LBLUCHR  , &
    REDCHR   , &
    LAVCHR   , &
    YELLOCHR , &
    WHITECHR , &
    BLKBKG   , &
    DBLUBKG  , &
    GREENBKG , &
    LBLUBKG  , &
    REDBKG   , &
    LAVBKG   , &
    YELLOBKG , &
    WHITEBKG , &
    UNDERLINE, &
    NORMAL   , &
    BRITE    , &
    BRIGHT   , &
    INVERSE  , &
    FLASH
  parameter( &
    ScnBufSiz=4000, & ! 80x25x2
    SupCAOfs =3999, & ! always ScnBufSiz-1
    LimitEnvSize=4096, & ! sufficient to reach beyond end of env. table
    SupEnvOffset=4095, & ! always LimitEnvSize-1
!    pVGATextPage=Z'b8000', &
!    pMDATextPage=Z'b0000', &
!!colors and attributes defined by Spindrift:
    BLKCHR  =0, &
    DBLUCHR =1, &
    GREENCHR=2, &
    LBLUCHR =3, &
    REDCHR  =4, &
    LAVCHR  =5, &
    YELLOCHR=6, &
    WHITECHR=7, &
    BLKBKG  =  0, &
    DBLUBKG = 16, &
    GREENBKG= 32, &
    LBLUBKG = 48, &
    REDBKG  = 64, &
    LAVBKG  = 80, &
    YELLOBKG= 96, &
    WHITEBKG=112, &
    UNDERLINE=  1, &
    NORMAL   =  7, &
    BRITE    =  8, &
    BRIGHT   = 15, &
    INVERSE  =112, &
    FLASH    =128)

  character (len=1) :: NullChar,FindFileDataStruc(0:42) ! 43=21+1+2+2+4+13 bytes per various documents
  character (len=12) :: NameField
  character (len=16) :: FileName
  character (len=128) ::  FindPrefix
  character (len=1024) ::  sWork
  integer (kind=1) ::   FFSearchAttr,AttrByte ! Z'000e' for non-blinking yellow on black background
  integer (kind=2) ::   i,n,jBS,PosNullCh
  integer (kind=4) ::   FileSize32b,DateTime32b,cResult,DosError
  equivalence(FileSize32b,FindFileDataStruc( 4)) ! determined experimentally
  equivalence(DateTime32b,FindFileDataStruc(12))
  equivalence(NameField  ,FindFileDataStruc(16)) ! (30) per various documents
! declarations shared by Dat() and Clk()
  character (len=8) ::  DateStr ! in CCYYMMDD format
  character (len=10) :: TimeStr ! in HHMMSS.SSS format
  character (len=5) :: ZoneStr
  integer (kind=4) ::   DaTiValues(8)
  data FFSearchAttr/0/ ! 0 for 'normal' files; 63 for volume-label only
  save FFSearchAttr
end module

  subroutine cls(ULRow,ULColumn,LRRow,LRColumn)
  integer, optional :: ULRow,ULColumn,LRRow,LRColumn

     if(PRESENT(LRColumn)) then
       call cls4(ULRow,ULColumn,LRRow,LRColumn)
     elseif(PRESENT(LRRow)) then
       call cls3(ULRow,ULColumn,LRRow)
     elseif(PRESENT(ULColumn)) then
       call cls2(ULRow,ULColumn)
     else
       call cls0
     endif
  end subroutine cls

subroutine cls4(ULRow,ULColumn,LRRow,LRColumn)
      use SpinDriftLib
  integer :: ULRow,ULColumn,LRRow,LRColumn,iRow,jCol
! use SpinFuncLocals

! Regs(1)=Z'0600' ! video BIOS function to scroll/clear a window
! Regs(2)=AttrByte
! Regs(3)=(ULRow shl 8).or.ULColumn ! top row of window, leftmost column
! Regs(4)=(LRRow shl 8).or.LRColumn ! end row of window, rightmost column
! call intrup(Regs,Z'10')
  return ! (do nothing)
!  do iRow=ULRow,LRRow
!    call locate(iRow,ULColumn) ! ClrEol would be presumptuous
!    write(*,'(80a1)',advance='no') (' ',jCol=ULColumn,LRColumn)
!  end do
end


subroutine cls3(iRow,BegColumn,EndColumn)
  integer :: iRow,BegColumn,EndColumn

! Regs(1)=Z'0600' ! video BIOS function to scroll/clear a window
! Regs(2)=AttrByte
! Regs(3)=(iRow shl 8).or.BegColumn ! top row of window, leftmost column
! Regs(4)=(iRow shl 8).or.EndColumn ! end row of window, rightmost column
! call intrup(Regs,Z'10')
  call cls4(iRow,BegColumn,iRow,EndColumn)
end


subroutine cls2(i0,i1) ! 0-based to match SpinDrift cls arguments
      use SpinDriftLib
  integer :: i0,i1,i ! ,j,j1 ! 80th char written on row i==24 causes scrolling

end

subroutine cls0()
      use SpinDriftLib
! integer (kind=2) ::   iMode

! Regs(1)=Z'0f00' ! video BIOS function to get current mode
! Regs(2)=AttrByte
! call intrup(Regs,Z'10')
! iMode=iand(Regs(1),Z'ff')
! VideoMod(iMode) ! clears screen as it resets the current mode

! call cls4(0,0,24,79)
! (do nothing)  call clrsc()
  call locate(0,0)
end

integer (kind=2)   function IsError()
  use SpinFuncLocals

  IsError=DosError
end


character (len=1) function GetDrive()
  USE SERVICE_ROUTINES
  character (len=256) :: CURRENT_DIRECTORY
  INTEGER :: IY
   IY = GETCWD(CURRENT_DIRECTORY)
   IF(IY <= 0) THEN
      GetDrive = CURRENT_DIRECTORY(1:1)
   ELSE
      GetDrive = 'C'
   ENDIF
end

subroutine CurDir(DriveCharIn,CurPath) ! returns without trailing '\'
  USE SERVICE_ROUTINES
  character (len=1) :: DriveCharIn
  character (len=*) :: CurPath
  character (len=1024) :: CURRENT_DIRECTORY
  integer :: IY
  IY = GETCWD(CURRENT_DIRECTORY)
  IF(IY == 0) THEN
     IY = LEN_TRIM(CURRENT_DIRECTORY)
     IF(CURRENT_DIRECTORY(IY:IY) == '\') THEN
        CURRENT_DIRECTORY(IY:IY) = ' '
     ENDIF
     CurPath = TRIM(CURRENT_DIRECTORY)
  ELSE
     CurPath = ' '
  ENDIF
end ! subroutine CurDir




subroutine GetArgs(ArgV,ArgC)
! use SpinFuncLocals
  integer (kind=4) ::   ArgC
  character (len=*) :: ArgV(ArgC) ! was *16 ArgV(*), conflicted with caller
  character (len=1) :: ABlank
  parameter(ABlank=' ')
  character (len=1) :: ThisChar,PrevChar ! ,pCmdChar
  integer (kind=2) ::   i ! ,pEnvSeg,PSPSeg
! integer (kind=1) ::   pTailCount
  character (len=128) :: CmdString

  call getcl(CmdString)



  ArgC=0
  ThisChar=ABlank
  do i=1,len_trim(CmdString)
    PrevChar=ThisChar
    ThisChar=CmdString(i:i)
    if(ThisChar/=ABlank) then
      if(PrevChar==ABlank) then
        ArgC=ArgC+1 ! if only blanks delimit parameters
        ArgV(ArgC)=''
      end if
      ArgV(ArgC)=trim(ArgV(ArgC))//ThisChar
!     write(*,*) ArgC,i,ThisChar,ArgV(ArgC),CmdString
    end if
  end do
end ! subroutine GetArgs


logical (kind=4)  function iScroll()
! use SpinFuncLocals

! Regs(1)=Z'0200' ! BIOS function used to read the kbd flags
! call intrup(Regs,Z'16') ! AGT found no DOS-level function for this
! iScroll=iand(Regs(1),16)>0
  iScroll=.false.
end


logical (kind=4)  function KeyAvail()
! use SpinFuncLocals
!  integer (kind=4) ::   kbhi

! Regs(1)=Z'0b00' ! function used to check kbd status
! call intrup(Regs,Z'21')
! KeyAvail=iand(Regs(1),Z'ff')>0
  KeyAvail=.false.
end


subroutine ClrKb()
! use SpinFuncLocals

! Regs(1)=Z'0c08' ! flush kbd buffer
! call intrup(Regs,Z'21')
  call flush(5)
end


subroutine ReadKey(AsciiCode)
! use SpinFuncLocals
  integer (kind=1) ::   AsciiCode
  integer (kind=4) ::   getc
! character*1 AnyChar

! Regs(1)=Z'0800' ! get character without echo
! call intrup(Regs,Z'21')
! AsciiCode=iand(Regs(1),Z'ff')

! read(5,*) AnyChar ! note that this requires a terminating <Enter>
! AsciiCode=IAChar(AnyChar)
  AsciiCode=getc()
end


subroutine AnyKey() ! i.e., wait for any alphanumeric key to be struck
! use SpinFuncLocals
  integer (kind=1) ::   AsciiCode

  call flush(5)
  do
!   Regs(1)=Z'0c08' ! flush kbd buffer, get character without echo
!   call intrup(Regs,Z'21')
!   AsciiCode=iand(Regs(1),Z'ff')
! write(*,*)AsciiCode,char(AsciiCode)
    call ReadKey(AsciiCode)
    if(((48<=AsciiCode).and.(AsciiCode<= 57)) .or. & ! '0'..'9'
       ((65<=AsciiCode).and.(AsciiCode<= 90)) .or. & ! 'A'..'Z'
       ((97<=AsciiCode).and.(AsciiCode<=122))) exit  ! 'a'..'z'
  end do
end


character (len=1) function GetCh(KeyType)
  use SpinFuncLocals
  integer (kind=1) ::   KeyType,B,AsciiCode
  character (len=1) :: KeyedChar,UCKC,UpCase
  logical (kind=1) :: AltPrefix,CtlPrefix,NulPrefix,EscKeyStruck

! do
!   Regs(1)=Z'0200' ! BIOS function used to read the kbd flags
!   call intrup(Regs,Z'16')
!   B=iand(Regs(1),Z'ff')
!   AltPrefix=iand(B,Z'08')>0
!   CtlPrefix=iand(B,Z'04')>0
!   if(KeyAvail()) exit
! end do

  AltPrefix=.false.
  CtlPrefix=.false.
  call ReadKey(AsciiCode)
  EscKeyStruck=AsciiCode==27
  NulPrefix=AsciiCode==0
!!write(*,*)AltPrefix,CtlPrefix,NulPrefix,EscKeyStruck,AsciiCode,char(AsciiCode)
  if(NulPrefix) then ! extended-ASCII keys return a 0-byte prefix
!   if(KeyAvail()) call ReadKey(AsciiCode) ! flush the keystroke buffer
                   call ReadKey(AsciiCode) ! flush the keystroke buffer
  end if
!!write(*,*)AltPrefix,CtlPrefix,NulPrefix,EscKeyStruck,AsciiCode,char(AsciiCode)
  KeyedChar=char(AsciiCode)
  UCKC=UpCase(KeyedChar)

!!if(AltPrefix .and.((15<=AsciiCode).and.(AsciiCode<=50))) then
!!  KeyedChar=AltChar(AsciiCode)
!!  AsciiCode=iAChar(KeyedChar)
!!end if ! decoding the keyboard-specific scan-codes

!!KeyType is coded to conform with Spindrift; ignore Function keys F11 & F12
  if(NulPrefix) then
    if    ((59<=AsciiCode).and.(AsciiCode<=68)) then
      KeyType=0 ! Function-key
    elseif(((71<=AsciiCode).and.(AsciiCode<=73)).or. &
           ((75<=AsciiCode).and.(AsciiCode<=77)).or. &
           ((79<=AsciiCode).and.(AsciiCode<=83))) then
      KeyType=8 ! Edit-key
    end if
  elseif(AltPrefix) then; KeyType=64 ! Alt-key
  elseif(AsciiCode==13) then; KeyType=4 ! Enter-key
  elseif(EscKeyStruck) then; KeyType=16 ! Esc-key
  elseif(((48<=AsciiCode).and.(AsciiCode<=57)).or. & ! '0'..'9'
    (KeyedChar=='+').or. &
    (KeyedChar=='-').or. &
    (KeyedChar=='.')) then; KeyType=3 ! Numeric
  elseif(CtlPrefix) then
  ! if    (KeyedChar=='m') then; KeyType=4 ! Enter-key
    if    (KeyedChar=='h') then; KeyType=8 ! Edit-key (backspace)
    elseif(KeyedChar=='i') then; KeyType=8 ! Edit-key (tab)
    elseif(KeyedChar=='j') then; KeyType=8 ! Edit-key (linefeed)
    elseif(KeyedChar=='(') then; KeyType=16 ! Esc-key
    else                       ; KeyType=32 ! Ctrl-key
    end if
  elseif((8<=AsciiCode).and.(AsciiCode<=10)) then; KeyType=8 ! Edit-key (BS,tab,LF)
!!elseif(('A'<=UCKC).and.(UCKC<='Z')) then; KeyType:= 2; ! Alphabetic
  else; KeyType=2 ! Alphabetic (seems the safest default)
  end if
!!write(*,*)KeyType,AsciiCode,KeyedChar
  GetCh=KeyedChar
end ! function GetCh


logical (kind=4)  function escape()
  use SpinFuncLocals
  integer (kind=1) ::   AsciiCode
  logical (kind=4) :: KeyPending,KeyAvail

  KeyPending=KeyAvail()
  if(KeyPending) then
    call ReadKey(AsciiCode)
    escape=AsciiCode==27
  else
    escape=.false.
  end if
end


subroutine AppendNullTerm(s)
  use SpinFuncLocals
  character (len=*) :: s

  PosNullCh=min(len(s),len_trim(s)+1)
  s(PosNullCh:PosNullCh)=NullChar ! c-language strings end in a null byte
end


subroutine TrapDosError()
  use SpinFuncLocals
  USE SERVICE_ROUTINES
  integer (kind=4) ::   errn,doserrno

   DosError= IERRNO()
end



 subroutine DisplayCString(s)
   character (len=*) :: s
   integer (kind=2) ::   j
   do j=1,1024
!     write(*,*) j,iachar(s(j:j)),' ',s(j:j)
     if(iachar(s(j:j))==0) exit
   ! if(s(j:j)==' ') exit
   end do
   call AnyKey()
 end




subroutine erase(PathAndName)

  character (len=*) :: PathAndName
  integer :: IOS
  open(10,file=PathAndName,IOSTAT=IOS)
  close(10,status='delete',IOSTAT=IOS)
!  call system("DEL "//PathAndName)
end ! subroutine erase


subroutine SetChr(CharVar,iValue)
  character (len=1) :: CharVar
  integer (kind=1) ::  iValue

  CharVar=char(iValue)
end


! TODO: This routine calls system and probably works across
! compilers on Windows but isn't an easy way to detect copying
! errors.  Additionally, it will compile on Linux but won't 
! copy the file. TrapDosError() only helps with this on Windows.
! So on Linux, this routine will likely run but either always
! raise an error or never raise it.
! Rewrite this routine so that it works on all three major
! OSes. MacOS may require call to BSD-specific routines.
! (Name should be copyfile or something more specific to purpose.)
subroutine copy(SourFile,DestFile)
  character (len=*) :: SourFile,DestFile

!string sent to system is limited to 122 characters; 
  call system("copy "//trim(SourFile)//" "//trim(DestFile)) 
  call TrapDosError()
end

! TODO:  To make this routine work across platforms (and possibly
! after compiling with a modern compiler), this routine needs to 
! be modified. Leverage conditional compilation to create a routine
! that works under any Fortran compiler and builds/runs for any
! OS. 
integer (kind=4)   function FilSiz(PathAndName)
  use SpinFuncLocals
  character (len=*) ::  PathAndName
  integer (kind=4) ::   LenInBytes
  logical (kind=4) ::   Extant

  inquire(file=PathAndName,exist=Extant,fLen=LenInBytes)
  if(Extant) then
    DosError=0
    FilSiz=LenInBytes
  else
    DosError=2 ! file not found
    FilSiz=0
  end if
end


character (len=11)  function Clk() ! in HH:MM:SS.SS format
  use SpinFuncLocals

  call Date_And_Time(DateStr,TimeStr,ZoneStr,DaTiValues)
  Clk=TimeStr(1:2)//':'//TimeStr(3:4)//':'//TimeStr(5:9)
end


character (len=8) function Dat() ! in MM/DD/YY format
  use SpinFuncLocals

  call Date_And_Time(DateStr,TimeStr,ZoneStr,DaTiValues)
  Dat=DateStr(5:6)//'/'//DateStr(7:8)//'/'//DateStr(3:4)
end


subroutine Tone2(f1,f2,duration)
  integer (kind=2) ::   f1,f2,duration ! in Hz and hundredths of a second

!  write(*,*) char(7) ! sound the bell
end


subroutine QuickR(nItems,a,iPos) ! sorts nItems items into ascending order
!!based on values in (1-based) key array a;
!!Shell sort adapted from the source on page 110 of
!!SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
  integer (kind=4) ::   nItems,i,j,k,Gap,iPsHeld,iPos(*)
  real (kind=4) :: a(*)

  do i=1,nItems ! assume that the caller has not initialized the index array
    iPos(i)=i
  end do
  Gap=nItems/2
  do while(Gap>0)
    do i=Gap,nItems
      j=i-Gap
      do while(j>0)
        k=j+Gap
        if(a(iPos(j))<=a(iPos(k))) then
          j=0 ! break the while loop (assign j=-1 for 0-based arrays)
        else ! interchange the indexing array values
          iPsHeld=iPos(k)
          iPos(k)=iPos(j)
          iPos(j)=iPsHeld
        end if
        j=j-Gap
        CALL RW_PROCESS_MESSAGES()
      end do
      CALL RW_PROCESS_MESSAGES()
    end do
    Gap=Gap/2
    CALL RW_PROCESS_MESSAGES()
  end do
end ! subroutine QuickR

subroutine QuickI(nItems,a,iPos) ! sorts nItems items into ascending order
!!based on values in (1-based) key array a;
!!Shell sort adapted from the source on page 110 of
!!SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
  integer (kind=4) ::   nItems,i,j,k,Gap,iPsHeld,iPos(*)
  integer (kind=2) ::   a(*)

  do i=1,nItems ! assume that the caller has not initialized the index array
    iPos(i)=i
  end do
  Gap=nItems/2
  do while(Gap>0)
    do i=Gap,nItems
      j=i-Gap
      do while(j>0)
        k=j+Gap
        if(a(iPos(j))<=a(iPos(k))) then
          j=0 ! break the while loop (assign j=-1 for 0-based arrays)
        else ! interchange the indexing array values
          iPsHeld=iPos(k)
          iPos(k)=iPos(j)
          iPos(j)=iPsHeld
        end if
        j=j-Gap
        CALL RW_PROCESS_MESSAGES()
      end do
      CALL RW_PROCESS_MESSAGES()
    end do
    Gap=Gap/2
    CALL RW_PROCESS_MESSAGES()
  end do
end ! subroutine QuickI

subroutine ConditionalQuickI(nItems,a,iPos,Condition) ! sorts nItems items into ascending order
!!based on values in (1-based) key array a;
!!Shell sort adapted from the source on page 110 of
!!SOFTWARE TOOLS IN PASCAL, by Kernighan & Plauger, Addison-Wesley 1981
  integer (kind=4) ::   nItems,i,j,k,Gap,iPsHeld,iPos(*)
  integer (kind=2) ::   a(nItems),Condition(nItems)

  do i=1,nItems ! assume that the caller has not initialized the index array
    iPos(i)=i
  end do
  Gap=nItems/2
  do while(Gap>0)
    do i=Gap,nItems
      j=i-Gap
      do while(j>0)
        k=j+Gap
        if(a(iPos(j))<=a(iPos(k)) .or. Condition(iPos(j))<=Condition(iPos(k))) then
          j=0 ! break the while loop (assign j=-1 for 0-based arrays)
        else ! interchange the indexing array values
          iPsHeld=iPos(k)
          iPos(k)=iPos(j)
          iPos(j)=iPsHeld
        end if
        j=j-Gap
        CALL RW_PROCESS_MESSAGES()
      end do
      CALL RW_PROCESS_MESSAGES()
    end do
    Gap=Gap/2
    CALL RW_PROCESS_MESSAGES()
  end do
end ! subroutine QuickI


integer (kind=2)   function VideoTyp()
! use SpinFuncLocals

! Regs(1)=Z'0f00'
! call intrup(Regs,Z'10')
!!nCol:= Regs(1) shr 8;
! VideoTyp=iand(Regs(1),Z'ff')
  VideoTyp=3 ! for 80x25 color
end


subroutine VideoMod(iMode) ! sets the video mode
! use SpinFuncLocals
  integer (kind=2) ::   iMode

! Regs(1)=iand(iMode,Z'ff')
! call intrup(Regs,Z'10')
end


subroutine CaptScn(DestBuffer)
  use SpinFuncLocals
  character (len=1) :: DestBuffer(0:*)
  integer (kind=2) ::   VideoTyp  ,j

  if(VideoTyp()<7) then
!    call memcp(DestBuffer,%val(pVGATextPage),cArg(ScnBufSiz))
  else
!    call memcp(DestBuffer,%val(pMDATextPage),cArg(ScnBufSiz))
  end if
!  write(*,'(1x,40a1)') (DestBuffer(j),j=0,158,2)
end


subroutine DispScn(SourBuffer)
  use SpinFuncLocals
  integer :: SourBuffer(*)
  integer (kind=2) ::   VideoTyp

  if(VideoTyp()<7) then
!    call memcp(%val(pVGATextPage),SourBuffer,cArg(ScnBufSiz))
  else
!    call memcp(%val(pMDATextPage),SourBuffer,cArg(ScnBufSiz))
  end if
end


subroutine CurOn()
! use SpinFuncLocals

! Regs(1)=Z'0300' ! video BIOS function to get cursor position & size
! Regs(2)=0 ! for page 0
! call intrup(Regs,Z'10')

! Regs(3)=iand(Regs(3),Z'1fff') ! clear bit 5 of CH to force cursor on
! Regs(1)=Z'0100' ! video BIOS function to set cursor size
! Regs(2)=0 ! for page 0
! call intrup(Regs,Z'10')
end





subroutine locate_22(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer (kind=2) ::   Row,Col
  integer (kind=4) ::   cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end
subroutine locate_24(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer (kind=2) ::   Row
  integer (kind=4) ::   Col
  integer (kind=4) ::   cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end
subroutine locate_42(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer (kind=4) ::   Row
  integer (kind=2) ::   Col
  integer (kind=4) ::   cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end
subroutine locate_44(Row,Col) ! 0-based (Row,Col) to match Spindrift arguments
  integer (kind=4) ::   Row,Col
  integer (kind=4) ::   cResult,movecursor

!  cResult=movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
!  write(*,'(a1)',advance='no') ' '
! (do nothing) call movecursor(cArg(Col),cArg(Row)) ! c function uses 0-based args
end


subroutine ClrScr()
      use SpinDriftLib
! (do nothing)  call clrsc()
  call locate(0,0)
end

subroutine SetAttr(iVal)
  use SpinFuncLocals
  integer (kind=2) ::   iVal

!!set global variable for use in cls and PrintS subroutines
  AttrByte=iVal
end

character (len=1)  function UpCase(c)
  character (len=1) :: c

  if(('a'<=c).and.(c<='z')) then
    UpCase=char(iAChar(c)-32)
  else
    UpCase=c
  end if
end

character (len=1)  function LowCase(c)
  character (len=1) ::  c

  if(('A'<=c) .and. (c<='Z')) then
    LowCase = char(iAChar(c)+32)
  else
    LowCase = c
  end if
end


subroutine UpC(sInp,sOut)
  character (len=1024) ::  sWork
  character (len=*) ::  sInp,sOut
  character (len=1) :: UpCase
  INTEGER :: UPCHARS,I

        sWork = trim(sInp)
        UPCHARS = LEN_TRIM(sWork)
        DO I = 1, UPCHARS
           sWork(I:I) = UpCase(sWork(I:I))
        END DO
!        call strup(sWork)
        sOut= trim(sWork)
end


subroutine LwC(sInp,sOut)
  character (len=1024) :: sWork
  character (len=*)   ::  sInp,sOut
  INTEGER :: UPCHARS,I
  character (len=1) :: LowCase

        sWork = trim(sInp)
        UPCHARS = LEN_TRIM(sWork)
        DO I = 1, UPCHARS
           sWork(I:I) = LowCase(sWork(I:I))
        END DO
        sOut= trim(sWork)
end

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the following text-window routines are undocumented in the Spindrift
! Utility Library Reference manual, Revision E, April 1992 that AGT had;
! hence they are merely stubbed out to satisfy the linker;
! the call to undocumented UNDER0() in MIDAS.F90 was disabled.


subroutine WINDBUF(ScreenBuf,BorderTitle,BorderAttr)
  integer (kind=4) ::   ScreenBuf(*)
  integer (kind=2) ::   BorderAttr
  character (len=*) ::  BorderTitle
end


subroutine OPENWIND(WindowHandle,ULRow,ULCol,LRRow,LRCol,iDummy,CharAttr)
  integer :: WindowHandle,ULRow,ULCol,LRRow,LRCol,iDummy
  integer (kind=2) ::   CharAttr
end


subroutine SETATTRW(WindowHandle,CharAttr)
  integer (kind=2) ::   WindowHandle,CharAttr
end


subroutine FILLB(WindowHandle,FillString,CharAttr)
  integer :: WindowHandle
  integer (kind=2) ::   CharAttr
  character (len=*) ::  FillString
end


subroutine BLDB(WindowHandle,Row,Col,FillString,CharAttr)
  integer :: WindowHandle,Row,Col
  integer (kind=2) ::   CharAttr
  character (len=*) ::  FillString
end


subroutine CLRW(WindowHandle)
  integer (kind=2) ::   WindowHandle
end

subroutine LOCATEW_444(WindowHandle,Row,Col)
  integer (kind=4) ::   WindowHandle,Row,Col
end

subroutine LOCATEW_442(WindowHandle,Row,Col)
  integer (kind=4) ::   WindowHandle,Row
  integer (kind=2) ::   Col
end

subroutine LOCATEW_424(WindowHandle,Row,Col)
  integer (kind=4) ::   WindowHandle,Col
  integer (kind=2) ::   Row
end

subroutine LOCATEW_422(WindowHandle,Row,Col)
  integer (kind=4) ::   WindowHandle
  integer (kind=2) ::   Row,Col
end

subroutine LOCATEW_244(WindowHandle,Row,Col)
  integer (kind=4) ::   Row,Col
  integer (kind=2) ::   WindowHandle
end

subroutine LOCATEW_242(WindowHandle,Row,Col)
  integer (kind=4) ::   Row
  integer (kind=2) ::   WindowHandle,Col
end

subroutine LOCATEW_224(WindowHandle,Row,Col)
  integer (kind=4) ::   Col
  integer (kind=2) ::   WindowHandle,Row
end

subroutine LOCATEW_222(WindowHandle,Row,Col)
  integer (kind=2) ::   WindowHandle,Row,Col
end

subroutine PRINTW(WindowHandle,ThisString)
  integer :: WindowHandle
  character (len=*) :: ThisString
end


subroutine OPENVS(nLines,LineLeng,iDummy)
  integer (kind=2) ::   nLines,LineLeng,iDummy
end


subroutine CLSVS(nLines)
  integer (kind=2) ::   nLines
end


subroutine LOCVS(nLines,Row,Column)
  integer (kind=2) ::   nLines,Row,Column
end


subroutine PRINTV(nLines,ThisString)
  integer (kind=2) ::   nLines
  character (len=*) ::  ThisString
end


subroutine VIEW(nLines,nRows,Row,Col)
  integer (kind=2) ::   nLines,nRows,Row,Col
end


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! the following are not related to the Spindrift routines, but deal with
! other discrepancies between F77L3 and the current (LF95) compiler:


subroutine InitDAFile(DafUnit,RecordLeng,FileName)
! write the direct-access file's header, compatible with existing F77L3 files
  integer (kind=2) ::   DafUnit,RecordLeng
  character (len=*) ::  FileName
  integer (kind=1) ::   F7
  F7 = z"f7"                                     ! status='REPLACE' ?
  open(DafUnit,file=FileName,access='TRANSPARENT',status='UNKNOWN') ! access='BINARY' failed to compile
! header is considered to have index rec=0 in F77L3, rec=1 in LF95
  write(DafUnit,rec=1) F7,RecordLeng,-1_1 ! item lengths in bytes:  1,2,1
! write(DafUnit,rec=1) -9_1,RecordLeng,-1_1 ! item lengths in bytes:  1,2,1
! HeaderInitByte is always z'f7'=-9_1 for RecordLeng>2
! EndFile(DafUnit) ! appropriate only to sequential files, failed to solve
                   ! the problem of only the LSB of RecordLeng being retained
                   ! in the file's 2nd byte, with the 3rd byte amazingly zero;
                   ! AGT hypothesizes that the C-language underpinnings of LF95
                   ! may truncate the file-size to an integer :: number of 16-bit
                   ! words when executing the close statement
! call flush(DafUnit) ! neither did this solve the above problem
! AGT 20000915 circumvented the problem by writing -1_1 to the 4th byte,
! thus filling out an integer :: number of 16-bit words (and an entire 32-bit
! double-word), but believes this is a bug in the LF95 v5.50f compiler
  close(DafUnit)
end


subroutine GetDAFileRecLen(DafUnit,RecordLeng,FileName)
  integer (kind=2) ::   DafUnit,RecordLeng
  character (len=*) ::  FileName
  character (len=1) ::  HeaderInitByte

  RecordLeng=0
  open(DafUnit,file=FileName,access='transparent',status='old',err=9)
  read(DafUnit) HeaderInitByte,RecordLeng
9 close(DafUnit)
end


integer (kind=2)   function NextRecord(DafUnit)
  integer (kind=2) ::   DafUnit
  integer (kind=4) ::   ios,nr
  logical (kind=4) ::   opnd

  inquire(unit=DafUnit,IoStat=ios,opened=opnd,NextRec=nr)
  if((ios==0).and.opnd.and.(nr>0)) then
    NextRecord=nr
  else
    NextRecord=0
  end if
end


subroutine WriteStarNA(s)
  character (len=*) ::  s

!  write(*,'(a)',advance='no') s
end
