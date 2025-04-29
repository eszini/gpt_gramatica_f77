    ! Following functions were copied intact from MSGoldSa.for on 20010227:

      real*8 function MixRand32() ! on (0.0,1.0], excluding 0.0
      integer*4 iRan32

      MixRand32=dble(iRan32())/2147483647.0d0 ! 2147483647=(2**31)-1
      end
c-----
      integer*4 function iRan32() ! on [1,2147483647], excluding 0
    ! prime modulus multiplicative linear congruential random number generator;
    ! f(z)=(16807*z) mod 2147483647; 2147483647=(2**31)-1;
    ! Reference:  Communications of the ACM vol 31 no 10
      integer*4 LongSeed,NextSeed,low,high,test,W32b,
     +  SeedRan32,SeedRan321
      integer*2 W12b,W14b,W5b
      data LongSeed/1/
      save LongSeed

      high=LongSeed/127773 ! 127773==2147483647/16807
      low =mod(LongSeed,127773)
      test=16807*low-2836*high ! 2836==mod(2147483647,16807)
      if(test>0) then
        LongSeed=test
      else
        LongSeed=test+2147483647
      end if
c     write(4,'(i12,a)') LongSeed,' iRan32 LongSeed' ! for Ron Sobey
      iRan32=LongSeed
      return ! function iRan32

      entry SeedRan32(W12b,W14b,W5b) ! allow caller to specify seed by parts
    ! NextSeed=((int4(W12b)*16384)+W14b)*32+W5b ! 12+14+5=31 bits
      NextSeed=((int4(W14b)* 4096)+W12b)*32+W5b ! 12+14+5=31 bits
c     write(4,'(3i6,i12,a)') W12b,W14b,W5b,NextSeed,' SR32'
    ! 20020108 switched order of use of arguments above to make W14b dominant
      if(NextSeed>0) LongSeed=NextSeed ! else retain current seed
      SeedRan32=LongSeed ! merely return current seed if all 3 args are zero
      return

      entry SeedRan321(W32b) ! allow caller to specify seed explicitly
      if(W32b/=0) LongSeed=W32b ! else retain current seed
      SeedRan321=LongSeed ! merely return current seed if arg is zero
      return
      end ! function iRan32
c-----
