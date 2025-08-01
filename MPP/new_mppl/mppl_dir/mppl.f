c Mppl was written by Paul F. Dubois, Lee Busby, Peter Willmann, and
c Janet Takemoto at Lawrence Livermore National Laboratory.
c All files in the Basis system are Copyright 1994-2002, by the Regents of
c the University of California.  All rights reserved.
c See "Copyright" for complete legal notice.

c%%%%%-included-file:mppl.i 
cparameter file for mppl
c%%%%%-included-file:scon.h 
cdefine	VARNAMESIZE 129
cdefine	MPPL_MAXINLEV 5
cdefine	MPPL_MAXCARD 200
cdefine	FILENAMESIZE 256
c sizes
c lexical
c can't collide with normal text or flags below
c
c builtin flags
c
c 110 defined above
c 119 defined above

cvalues for do statement types
c select parameters
c value for domath



c include files
c IRSET

c STDUNITS

c FNAME

c IDIRCOM

c ICOM

c PATHVARS

c CTLVARS

c CARGS

c CFILE

c CLABS

c CMACRO
ccurrent call stack pointer
cnext free position in evalst
cline,file at which macro began
cevaluation stack

c CSTAT
c current select entry; init = 0
c next available position; init = 1
c select information
c select depth
c label that started this select

c CTYPE

c array of actions(shift,reduce,etc)
c array  of states corresponding to act1
c array of next states after reduction
c stack of states,stack top
c stack of tokens, stack top

      block data mppldata

      integer ap,argstk(500),callst(200),nlb,plev(200)
      common /cargs/ ap,argstk,callst,nlb,plev


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      character*(8) act1
      integer act2, move, stray,stop, numray,numtop
      common /ceo/ act1(26)
      common /ceo1/ act2(26,8), move(26,4)
      common /ceo2/ stray(125), stop, numray(125), numtop


      integer labnxt,laborg
      common /labg/labnxt,laborg


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer icompile
      common /icom/icompile

      integer i
      data verbose/.false./
      data laborg/23000/
      data col72/72/
      data indlev/3/
      data colcom/-1/
c###
c Debug flags
c        data optlang/LANG_F90/
c        data optmacro/NO/
c        data optpretty/NO/
c        data optrel/LANG_F77/
c        data hnl/YES/
c###
      data optlang/2/
      data optmacro/1/
      data optpretty/1/
      data optrel/2/
      data hnl/0/
      data optalloc/0/
      data optnum/1/
      data firstfil /.true./
c
c act1,act2 indexed by current state and input
c columns 1-8 correspond to: ( ) * + int - eof /
c
      data act1(1)/'eeeeeeee'/
      data act1(2)/'seeessee'/
      data act1(3)/'seeessee'/
      data act1(4)/'seeeseee'/
      data act1(5)/'errrerrr'/
      data act1(6)/'eeesesee'/
      data act1(7)/'eeeeeeae'/
      data act1(8)/'errrerrr'/
      data act1(9)/'ersrerrs'/
      data act1(10)/'eseeeeee'/
      data act1(11)/'eeesesee'/
      data act1(12)/'ersrerrs'/
      data act1(13)/'seeeseee'/
      data act1(14)/'seeeseee'/
      data act1(15)/'eeeeeeae'/
      data act1(16)/'seeeseee'/
      data act1(17)/'seeeseee'/
      data act1(18)/'errrerrr'/
      data act1(19)/'seeeseee'/
      data act1(20)/'seeeseee'/
      data act1(21)/'ersrerrs'/
      data act1(22)/'ersrerrs'/
      data act1(23)/'errrerrr'/
      data act1(24)/'errrerrr'/
      data act1(25)/'ersrerrs'/
      data act1(26)/'ersrerrs'/
      data (act2(1,i), i=1,8) /8*1/
      data (act2(2,i), i=1,8) /3,3*1,5,4,2*1/
      data (act2(3,i), i=1,8) /3,1,1,1,5,4,1,1/
      data (act2(4,i), i=1,8) /3,3*1,5,3*1/
      data (act2(5,i), i=1,8) /1,3*14,1,3*14/
      data (act2(6,i), i=1,8) /3*1,13,1,14,2*1/
      data (act2(7,i), i=1,8) /8*1/
      data (act2(8,i), i=1,8) /1,3*11,1,3*11/
      data (act2(9,i), i=1,8) /1,2,16,8,1,8,2,17/
      data (act2(10,i), i=1,8) /1,18,6*1/
      data (act2(11,i), i=1,8) /3*1,19,1,20,2*1/
      data (act2(12,i), i=1,8) /1,3,16,8,1,8,3,17/
      data (act2(13,i), i=1,8) /3,3*1,5,3*1/
      data (act2(14,i), i=1,8) /3,3*1,5,3*1/
      data (act2(15,i), i=1,8) /8*1/
      data (act2(16,i), i=1,8) /3,3*1,5,3*1/
      data (act2(17,i), i=1,8) /3,3*1,5,3*1/
      data (act2(18,i), i=1,8) /1,3*15,1,3*15/
      data (act2(19,i), i=1,8) /3,3*1,5,3*1/
      data (act2(20,i), i=1,8) /3,3*1,5,3*1/
      data (act2(21,i), i=1,8) /1,4,16,9,1,9,4,17/
      data (act2(22,i), i=1,8) /1,6,16,10,1,10,6,17/
      data (act2(23,i), i=1,8) /1,3*12,1,3*12/
      data (act2(24,i), i=1,8) /1,3*13,1,3*13/
      data (act2(25,i), i=1,8) /1,5,16,9,1,9,5,17/
      data (act2(26,i), i=1,8) /1,7,16,10,1,10,7,17/
c
c move indexed by stray(stop) and current non-terminal
c from grammar
c columns 1-4 correspond to: E  E'  T  F
c
      data (move(1,i), i=1,4) /4*1/
      data (move(2,i), i=1,4) /7,6,9,8/
      data (move(3,i), i=1,4) /10,6,9,8/
      data (move(4,i),i=1,4) /1,11,12,8/
      data (move(5,i), i=1,4) /4*1/
      data (move(6,i), i=1,4) /4*1/
      data (move(7,i), i=1,4) /4*1/
      data (move(8,i), i=1,4) /4*1/
      data (move(9,i), i=1,4) /4*1/
      data (move(10,i), i=1,4) /4*1/
      data (move(11,i), i=1,4) /4*1/
      data (move(12,i), i=1,4) /4*1/
      data (move(13,i), i=1,4) /2*1,21,8 /
      data (move(14,i), i=1,4) /2*1,22,8/
      data (move(15,i), i=1,4) /4*1/
      data (move(16,i), i=1,4) /3*1,23/
      data (move(17,i), i=1,4) /3*1,24/
      data (move(18,i), i=1,4) /4*1/
      data (move(19,i), i=1,4) /2*1,25,8/
      data (move(20,i), i=1,4) /2*1,26,8/
      data (move(21,i), i=1,4) /4*1/
      data (move(22,i), i=1,4) /4*1/
      data (move(23,i), i=1,4) /4*1/
      data (move(24,i), i=1,4) /4*1/
      data (move(25,i), i=1,4) /4*1/
      data (move(26,i), i=1,4) /4*1/
c
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine aampp
cProlog

      external getarg, iargc
      integer i, iargc, jcd, idone
      character*128 argv(200)
      integer arg1, argc, firstfile
      character*(256) basisv
      logical obckeep, bv
      integer lnb
      external lnb


      logical ireset,rreset
      common /irset/ ireset,rreset


      character*(256) std,basis,sys,system
      common /fname/ std,basis,sys,system


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer lexwarn,realsize,intsize,wordsize
      integer inlev,finerr,nodblquo,yyline(5)
      common /ctlvars/ lexwarn,realsize,intsize,wordsize
      common /ctlvars/ inlev,finerr,nodblquo,yyline


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      character*(256) argv0
      character*(256) realname
      character*(256) realpath
      integer len1,len2,len3
      common /cpathvars/argv0,realname,realpath
      common /ipathvars/len1,len2,len3


      call init0(stdin,stdout,stderr,intsize,realsize,wordsize,system)
      call init
      call tabini

      idone = 0
      finerr = 0
      nodblquo = 0
      lexwarn = 0
      ireset = .false.
      rreset = .false.

c There is a chicken and egg problem in setting intsize, realsize, and
c wordsize.  They need to be set before reading any input which contains
c literal integers or floating point constants, but you need to read
c "mppl.sys" in order to (properly) set wordsize.  Then that value of
c wordsize is used to set the default values for intsize and realsize for
c a given SYSTEM (which may have been set with a "-t" option).  However,
c the user may have set values for intsize and realsize by the command line
c options "-i" or "-r", so we need to check for that before (re)setting the
c wordsize-based defaults.  The upshot is that we compile in provisional
c values for intsize, realsize, and wordsize, check the command line for any
c changes to intsize or realsize, and use those while reading "mppl.sys".
c Finally, we use the wordsize read in to reset intsize or realsize,
c if they weren't set on the command line.  Given this order of events,
c it would be foolhardy to put any floating point literals in "mppl.sys".
c
c       1) Set provisional values in init0 for intsize, realsize, wordsize.
c       2) Check command line - may reset SYSTEM.
c       3) Read mppl.sys - sets WORDSIZE as function of SYSTEM.
c       4) Reread command line to over-ride default MACHINE or COMPILER.
c       5) Set intsize, realsize per wordsize unless given on command line.
c       6) Read mppl.std.
c       7) Read mppl.BASIS.
c       8) Process remaining input files.
c
c Intsize, realsize, wordsize changes after step 5 will produce
c undefined behavior.

      arg1 = 1
      argc = iargc()
      call getarg(arg1-1,argv0)
      do 23000 i = arg1, argc
         call getarg(i,argv(i))
23000 continue

      bckeep = .true.
c -- for([i=arg1,argv(i)(1:1) == '-' & argv(i) <> '-',i=i+1])
      i=arg1
      go to 23002
23003 continue
         i=i+1
23002    if (.not.(argv(i)(1:1) .eq. '-' .and. argv(i) .ne. '-')) go to 
     &         23004
         call setopt(argv(i)(2:))
c -- repeat
      go to 23003
23004 continue
c endfor
      firstfile = i
      obckeep = bckeep
      call init1

      realname = " "
      realpath = " "
      len1 = lnb(argv0)
      len2 = len(realname)
      len3 = len(realpath)
      call nameme0()
      jcd = lnb(realpath)
      sys=realpath(1:jcd)//"/mppl.sys"
      std=realpath(1:jcd)//"/mppl.std"
      basis=realpath(1:jcd)//"/mppl.BASIS"
      basisv=realpath(1:jcd)//"/BASIS_VERSION"

c These must be called after casesen is set ( -u )
      call bltin
      if (optmacro .eq. 1) then
         call sharedconstants
         call cnstal('SYSTEM',system(1:lnb(system)))
c Turn off blank and comment line output while processing startup files.
         bckeep = .false.
         call dofile(sys)
c Reprocess the option list to over-ride defaults just read in.
c -- for([i=arg1,argv(i)(1:1) == '-' & argv(i) <> '-',i=i+1])
         i=arg1
         go to 23005
23006    continue
            i=i+1
23005       if (.not.(argv(i)(1:1) .eq. '-' .and. argv(i) .ne. '-')) 
     &            go to 23007
            call setopt(argv(i)(2:))
c -- repeat
         go to 23006
23007    continue
c endfor
c        endif

c Set intsize and realsize to final values.
         if (.not.ireset) intsize = 4
         call getwdsz(wordsize)
         if (.not.rreset)then
            if (wordsize .lt. 64)then
               realsize = 4
            else
               realsize = 8
            endif
         endif
         call setsize('IntSize',intsize)
         call setsize('RealSize',realsize)

c        if (optmacro == YES) then
         if (optnum .eq. 1) call dofile(std)
         if (basis .ne. " ") then
            call dofile(basis)
            inquire(file=basisv, exist=bv)
            if (bv) call dofile(basisv)
         endif
      endif

c Process remaining arguments as input files.
      bckeep = obckeep
      do 23008 i = firstfile, argc
         call dofile(argv(i))
23008 continue
      if (firstfile .gt. argc) call dofile('-')
      call finish(finerr)
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine wrline(unit,msg,n)
cProlog
      integer n,unit
      character*(*) msg
      write(unit,100) msg(1:n)
  100 format(a)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine rem(msg,n)
cProlog
      character*(*) msg

      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr

      integer n
      write(stderr,100) msg(1:n)
  100 format(1x,a)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c ZPAKCHRZ - translate the integer string representation in AIN to
c          - a character representation in SOUT

      function zpakchrz(sout,lsout, ain,n)
cProlog
      integer zpakchrz
      character*(*) sout
      integer ain(*), n, i, lsout

      zpakchrz = min(lsout, n)

      do 23000 i = 1, zpakchrz
         sout(i:i) = char(ain(i))
23000 continue

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine bltin
cProlog
c install names and defns of define, ifelse, etc... in table

      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins

      character*1 ceos
      character*2 c

      ceos=char(27)

      c=char(127 )

      c(2:) = char(65)
      call cnstal('mppl_define',c)

      c(2:) = char(66)
      call cnstal('mppl_ifelse',c)

      c(2:)=char(71)
      call cnstal('mppl_Immediate',c)

      if (optmacro .eq. 1) then
         call cnstal('Prolog','#Prolog')
c         call cnstal('Prolog',cchar//'Prolog')

         c(2:) = char(65)
         call cnstal('define',c)

         c(2:) = char(73)
         call cnstal('Module',c)

         c(2:) = char(67)
         call cnstal('ifdef',c)

         c(2:) = char(66)
         call cnstal('ifelse',c)

         c(2:) =char(68)
         call cnstal('Errprint',c)

         c(2:) =char(110)
         call cnstal('Infoprint',c)

         c(2:)=char(69)
         call cnstal('Dumpdef',c)

         c(2:)=char(70)
         call cnstal('Evaluate',c)

         c(2:)=char(71)
         call cnstal('Immediate',c)

         c(2:)=char(92)
         call cnstal('include',c)

         c(2:)=char(72)
         call cnstal('Undefine',c)

         c(2:)=char(114)
         call cnstal('Remark',c)

      endif

      if (optlang .eq. 2) then

         c(2:)=char(77)
         call cnstal('do',c)

         c(2:)=char(74)
         call cnstal('enddo',c)

         c(2:)=char(76)
         call cnstal('next',c//' $1')

         c(2:)=char(75)
         call cnstal('break',c//' $1')

         c(2:)=char(94)
         call cnstal('program',c)

         c(2:)=char(95)
         call cnstal('subroutine',c)

         c(2:)=char(96)
         call cnstal('function',c)

         c(2:)=char(98)
         call cnstal('end',c)

         c(2:)=char(97)
         call cnstal('return',c//'mppl_ifelse($1,,,([$1]))')

         c(2:)=char(78)
         call cnstal('while',c//
     &      'mppl_ifelse($1,,,[mppl_Immediate([c while($1)])])([$1])')

         c(2:)=char(79)
         call cnstal('endwhile',c)

         c(2:)=char(80)
         call cnstal('until',c//
     &      'mppl_ifelse($1,,,[mppl_Immediate([c until($1)])])([$1])')

         c(2:)=char(81)
         call cnstal('if',c//'([$1])')

         c(2:)=char(82)
         call cnstal('else',c)

         c(2:)=char(84)
         call cnstal('elseif',c//'([$1])')

         c(2:)=char(83)
         call cnstal('endif',c)

         c(2:)=char(85)
         call cnstal('then',c)

         c(2:)=char(86)
         call cnstal('select',c//'mppl_ifelse($1,,, = $1)')

         c(2:)=char(87)
         call cnstal('case',c//'$1')

         c(2:)=char(88)
         call cnstal('default',c//'$1')

         c(2:)=char(89)
         call cnstal('endselect',c)

         c(2:)=char(90)
         call cnstal('for',c//'mppl_ifelse([$2],,,'// 
     &      '[mppl_Immediate([c -- for([$*])])[$1];go to @1;do;[$3];@1 i
     &f(.not.([$2])) break])')

         c(2:)=char(91)
         call cnstal('endfor',c)

         c(2:)=char(93)
         call cnstal('block',c)

         c(2:)=char(99)
         call cnstal('interface',c)

      elseif (optlang .eq. 3) then

         c(2:)=char(102)
         call cnstal('do',c)

         c(2:)=char(74)
         call cnstal('enddo',c)

         c(2:)=char(101)
         call cnstal('next',c//' $1')

         c(2:)=char(100)
         call cnstal('break',c//' $1')

         c(2:)=char(94)
         call cnstal('program',c)

         c(2:)=char(95)
         call cnstal('subroutine',c)

         c(2:)=char(96)
         call cnstal('function',c)

         c(2:)=char(98)
         call cnstal('end',c)

         c(2:)=char(97)
         call cnstal('return',c//'mppl_ifelse($1,,,([$1]))')

         c(2:)=char(103)
         call cnstal('while',c//'([$1])')

         c(2:)=char(104)
         call cnstal('endwhile',c)

         c(2:)=char(105)
         call cnstal('until',c//'([$1])')

         c(2:)=char(81)
         call cnstal('if',c//'([$1])')

         c(2:)=char(82)
         call cnstal('else',c)

         c(2:)=char(84)
         call cnstal('elseif',c//'([$1])')

         c(2:)=char(83)
         call cnstal('endif',c)

         c(2:)=char(85)
         call cnstal('then',c)

         c(2:)=char(106)
         call cnstal('select',c//'mppl_ifelse($1,,, $1)')

         c(2:)=char(107)
         call cnstal('case',c//'$1')

         c(2:)=char(108)
         call cnstal('default',c//'$1')

         c(2:)=char(109)
         call cnstal('endselect',c)

         c(2:)=char(90)
         call cnstal('for',c// 'mppl_define([mppl_foriter],[$3])' // 
     &      'mppl_ifelse([$1],,,[$1]'//ceos//')' // 
     &      'mppl_ifelse([$2],,,do while ([$2]))')

         c(2:)=char(74)
         call cnstal('endfor',c)

         c(2:)=char(93)
         call cnstal('block',c)

         c(2:)=char(99)
         call cnstal('interface',c)

      elseif (optmacro .eq. 1) then
c Define macros that enable us to define Module

         c(2:)=char(94)
         call cnstal('program',c)

         c(2:)=char(95)
         call cnstal('subroutine',c)

         c(2:)=char(96)
         call cnstal('function',c)

         c(2:)=char(98)
         call cnstal('end',c)

         c(2:)=char(93)
         call cnstal('block',c)

         c(2:)=char(99)
         call cnstal('interface',c)

         if (optpretty .eq. 1) then
            c(2:)=char(111)
            call cnstal('do',c)

            c(2:)=char(74)
            call cnstal('enddo',c)

c            c(2:)=char(SLOOPI)
c            call cnstal('while',c)

c            c(2:)=char(SLOOPD)
c            call cnstal('endwhile',c)

c            c(2:)=char(SUNTIL)
c            call cnstal('until',c)

            c(2:)=char(81)
            call cnstal('if',c//'([$1])')

            c(2:)=char(82)
            call cnstal('else',c)

            c(2:)=char(84)
            call cnstal('elseif',c//'([$1])')

            c(2:)=char(85)
            call cnstal('then',c)

            c(2:)=char(83)
            call cnstal('endif',c)

            c(2:)=char(112)
            call cnstal('select',c)

c           case (1)   |   case default
            c(2:)=char(113)
            call cnstal('case',c//'mppl_ifelse($1,,, ([$*]))')

c            c(2:)=char(SDEFAULTI)
c            call cnstal('default',c)

            c(2:)=char(109)
            call cnstal('endselect',c)

c            c(2:)=char(SFOR)
c            call cnstal('for',c//' ([$*])')

c            c(2:)=char(SEFOR)
c            call cnstal('endfor',c)

         endif

      endif

      if (optalloc .eq. 1) then
         c(2:)=char(116)
         call cnstal('allocate',c//'([$*])')

         c(2:)=char(117)
         call cnstal('deallocate',c//'([$*])')

         c(2:)=char(118)
         call cnstal('Shape',c//'([$*])')
      endif

      c(2:) = char(119)
      call cnstal('Setsuppress',c)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sharedconstants
cProlog
c Install definitions for some basis and mppl constants.  It is convenient
c to do this here, because these constants receive their fundamental
c definition in scon.h.  They are also known and required by various
c C source routines.
      character *32 s
      integer scon, fnb
      external scon, fnb

      write(s,'(i8)') scon(1)
      call cnstal('VARNAMESIZE',s(fnb(s):))

      write(s,'(i8)') scon(2)
      call cnstal('MPPL_MAXINLEV',s(fnb(s):))

      write(s,'(i8)') scon(3)
      call cnstal('MPPL_MAXCARD',s(fnb(s):))

      write(s,'(i8)') scon(4)
      call cnstal('FILENAMESIZE',s(fnb(s):))

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function fnb(s)
cProlog
c Return the index of the first non-blank character in s.
      integer fnb
      character *(*) s
      fnb = 1
c while( (s(fnb:fnb) = ' ') & (fnb < len(s)) )
23000 if ( (s(fnb:fnb) .eq. ' ') .and. (fnb .lt. len(s)) ) then
         fnb = fnb+1
         go to 23000
      endif
c endwhile
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine cnstal(cname,cdef)
cProlog
cinstall a definition, character string
cmaximum lengths for name and defn 32 and 256 THIS ROUTINE ONLY
      integer name(32),def(256),i,lnb
      external lnb
      character*(*) cname,cdef
      integer lname,ldef
c
      lname = len(cname)
      ldef = lnb(cdef)
      if (lname .gt. 32 .or. ldef .gt. 256) call oops(
     &      'cnstal: name or defn too long')
      do 23000 i=1,lname
         name(i) = ichar(cname(i:i))
23000 continue
      do 23002 i=1,ldef
         def(i) = ichar(cdef(i:i))
23002 continue
      call instal(name,lname,def,ldef)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine cpbstr(str)
cProlog
cpushes back character string str
      character*(*) str
      integer i,n

      n = len(str)
      do 23000 i=n,1,-1
         call putbak(ichar(str(i:i)))
23000 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine tostr(out, token, ltoken)
cProlog

      character*(*) out
      integer token(*), ltoken
      integer i
c      if (ltoken > len(out)) error
      out = " "
      do 23000 i=1,ltoken
         out(i:i) = char(token(i))
23000 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doblock
cProlog

      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer t,gnbtok
      external gnbtok
c
      call outtab(0,0)
      call putqs('block ')
      t = gnbtok(token,ltoken)
      call putchr(token,ltoken)
      if (ltoken .eq. 4) then
c look for 'data' in lower or upper case
         if ((token(1).eq.100 .or. token(1) .eq. 68) .and. (token(2).eq.
     &       97 .or. token(2) .eq. 65) .and. (token(3).eq.116 .or. token
     &      (3) .eq. 84) .and. (token(4).eq. 97 .or. token(4) .eq. 65) ) 
     &         then
c next should be the module name
            call putqs(' ')
            t = gnbtok(token,ltoken)
            call putchr(token,ltoken)
            if (ltoken .gt. 32) call warn('block module name too long')
            ltoken = min(ltoken,32)
            modtype(ifacelev) = 93
            call setmod(token,ltoken)
         endif
      endif
      call eatup(0)
      return
      end
c
c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function docond()
cProlog
c evaluate and put out conditional
c translate >, >=, <,<=,~, ~=, <>, = or ==, ~ , ~=, &, |
c argument is a dummy, not used
c returns 0 if o.k., 1 otherwise
      integer docond
      character*(20000) oper
      integer t,nlev,loper,wswid
      integer gtok,gnbtokw
      external gtok,gnbtokw

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins

      t = gnbtokw(token,ltoken,wswid)
      if ( t .ne. 40) then
         call warn('Missing condition.')
         docond = 1
         return
      endif
      wswid = 1
c      if (optpretty == YES) wswid = 1
      call outfil(wswid)
      call putqs('(')
      nlev = 1
      if (optrel .eq. 2) then
c while(nlev > 0)
23000    if (nlev .gt. 0) then
            t = gtok(token,ltoken)
            if (t .eq. 40) then
               nlev = nlev + 1
               code(1) = 40
               call putchr(code,1)
            elseif (t .eq. 41) then
               nlev = nlev - 1
               code(1) = 41
               call putchr(code,1)
            elseif ( t .eq. 38 ) then
               call putqs('.and.')
            elseif ( t .eq. 124) then
               call putqs('.or.')
            elseif ( t .eq. 61) then
               call putqs('.eq.')
               t = gtok(token,ltoken)
c ok to have = or ==
               if ( t .ne. 61) then
                  call pbstr(token,ltoken)
               endif
            elseif ( t .eq. 126) then
               t = gtok(token,ltoken)
               if ( t .ne. 61) then
                  call pbstr(token,ltoken)
                  call putqs('.not.')
               else
                  call putqs('.ne.')
               endif
            elseif ( t .eq. 60) then
               t = gtok(token,ltoken)
               if ( t .eq. 61) then
                  call putqs('.le.')
               elseif (t .eq. 62) then
                  call putqs('.ne.')
               else
                  call pbstr(token,ltoken)
                  call putqs('.lt.')
               endif
            elseif ( t .eq. 62) then
               t = gtok(token,ltoken)
               if ( t .eq. 61) then
                  call putqs('.ge.')
               elseif (t .eq. 60) then
                  call putqs('.ne.')
               else
                  call pbstr(token,ltoken)
                  call putqs('.gt.')
               endif
            elseif (t .eq. 1) then
               call endcomment(1, 1, token, ltoken, 0)
               call outtab(0,0)
            elseif ( t .eq. 10) then
               call conlin
            elseif ( t .eq. 13) then
               call warn('Missing right parenthesis in condition.')
            else
               call putchr(token,ltoken)
            endif
            go to 23000
         endif
c endwhile
      elseif (optrel .eq. 3) then
c F90 constructs
c while(nlev > 0)
23002    if (nlev .gt. 0) then
            t = gtok(token,ltoken)
            if (t .eq. 40) then
               nlev = nlev + 1
               code(1) = 40
               call putchr(code,1)
            elseif (t .eq. 41) then
               nlev = nlev - 1
               code(1) = 41
               call putchr(code,1)
            elseif ( t .eq. 38 ) then
               call putqs('.and.')
            elseif ( t .eq. 124) then
               call putqs('.or.')
            elseif ( t .eq. 61) then
               call putqs('==')
               t = gtok(token,ltoken)
c ok to have = or ==
               if ( t .ne. 61) then
                  call pbstr(token,ltoken)
               endif
            elseif ( t .eq. 126) then
               t = gtok(token,ltoken)
               if ( t .ne. 61) then
                  call pbstr(token,ltoken)
                  call putqs('.not.')
               else
                  call putqs('/=')
               endif
            elseif ( t .eq. 60) then
               t = gtok(token,ltoken)
               if ( t .eq. 61) then
                  call putqs('<=')
               elseif (t .eq. 62) then
                  call putqs('/=')
               else
                  call pbstr(token,ltoken)
                  call putqs('<')
               endif
            elseif ( t .eq. 62) then
               t = gtok(token,ltoken)
               if ( t .eq. 61) then
                  call putqs('>=')
               elseif (t .eq. 60) then
                  call putqs('/=')
               else
                  call pbstr(token,ltoken)
                  call putqs('>')
               endif
            elseif (t .eq. 16) then
               call tostr(oper,token,ltoken)
               loper = ltoken
               if (oper .eq. ".lt.") then
                  call putqs("<")
               elseif (oper .eq. ".le.") then
                  call putqs("<=")
               elseif (oper .eq. ".eq.") then
                  call putqs("==")
               elseif (oper .eq. ".ne.") then
                  call putqs("/=")
               elseif (oper .eq. ".gt.") then
                  call putqs(">")
               elseif (oper .eq. ".ge.") then
                  call putqs(">=")
               else
                  call putqs(oper(1:loper))
               endif
            elseif (t .eq. 1) then
               call endcomment(1, 1, token, ltoken, 0)
               call outtab(0,0)
            elseif ( t .eq. 10) then
               call conlin
            elseif ( t .eq. 13) then
               call warn('Missing right parenthesis in condition.')
            else
               call putchr(token,ltoken)
            endif
            go to 23002
         endif
c endwhile
      elseif (optrel .eq. 5) then
         call oops("docond with LANG_IS_f90")
      endif
      docond = 0
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine dodef(argstk,i,j)
cProlog
cdodef -- install definition in table
      integer a1,a2,a3,argstk(1),i,j,jloc,k,ilo,ihi,lname

      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6

      integer t,gettok
      external gettok
      if (j .gt. i + 3) call oops(
     &      ' Wrong number of arguments in define.')
      jloc = j
   50 continue
c if called as a macro:
      a1 = argstk(i+2)
      a2 = argstk(i+3)
      if ( a2-a1 .eq. 0 ) go to 100
      if ( jloc .eq. i+3) then
         a3 = argstk(i+4)
      else
         a3 = a2
      endif
      do 23000 ilo = a1, a2-1
         if (type(evalst(ilo)).ne.5) go to 23001
23000 continue
23001 continue
      do 23002 ihi = a2-1, a1, -1
         if (type(evalst(ihi)).ne.5) go to 23003
23002 continue
23003 continue
      lname = ihi -ilo + 1
      if (lname .le.0) call oops(' Empty name in define macro.')
      do 23004 k = ilo, ihi
         if (alphan(evalst(k)).ne.1) call oops(
     &         ' Name not alphanumeric in define macro.')
23004 continue
      call instal(evalst(ilo),lname,evalst(a2),a3-a2)
      return
carguments not supplied, go get them
  100 continue
c note that this routine is called from eval called from gtok,
c so we can only use gettok , not gnbtok or gtok
      t = gettok(token,ltoken)
      if (t .eq. 5) t = gettok(token,ltoken)
      if (t .ne. 4) call oops('define not followed by name')
      do 23006 k=1,ltoken
         evalst(argstk(i+2)-1+k)=token(k)
23006 continue
      argstk(i+3) = argstk(i+2) + ltoken
      argstk(i+4) = argstk(i+3)
      jloc = i + 3
c eatup rest of line as definition
      t = gettok(token,ltoken)
      if (t.eq.5) t=gettok(token,ltoken)
c while(t <> 13)
23008 if (t .ne. 13) then
         if (t .eq. 1) go to 23009
         do 23010 k=1,ltoken
            evalst(argstk(i+4) - 1 + k) = token(k)
23010    continue
         argstk(i+4) = argstk(i+4) + ltoken
         t = gettok(token,ltoken)
         go to 23008
      endif
c endwhile
23009 continue
      go to 50
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c handles end, end do, end while, end if

      subroutine doend
cProlog
      integer gnbtokw
      external gnbtokw
      integer t, wswid
      integer i23000

      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


c sure, we've seen an end, but did they really mean it ?
      t = gnbtokw(token,ltoken,wswid)
      if (t .eq. 4) then
         call sendalpha
      elseif (t .ne. 127 ) then
         call sendmod(t, wswid)
      else
c select
         i23000= token(2)
            go to 23000
c -- case 77
c -- case 102
c -- case 111
23002    continue

            call sedo
23003       go to 23001
c -- case 78
23004    continue

            call sewhile
23005       go to 23001
c -- case 81
23006    continue

            call sendif
23007       go to 23001
c -- case 90
23008    continue

            call sefor
23009       go to 23001
c -- case 86
23010    continue

            call seselect
23011       go to 23001
c -- case 93
c -- case 95
c -- case 96
c -- case 94
23012    continue

            call sendmod(t,wswid)
23013       go to 23001
c -- case 99
23014    continue

            call sendiface
23015       go to 23001
c -- case 103
23016    continue

            call sewhile90
23017       go to 23001
c -- case 106
c -- case 112
23018    continue

            call seselect90
23019       go to 23001
c -- case (default)
23020    continue

            call warn('end followed by improper keyword')
c -- dispatch area for select
23021    go to 23001
23000    continue
         i23000=i23000-76
         if (i23000.lt. 1 .or. i23000.gt.36) go to 23020
         go to (23002,23004,23020,23020,23006,23020,23020,23020,23020,
     &      23010,23020,23020,23020,23008,23020,23020,23012,23012,23012,
     &      23012,23020,23020,23014,23020,23020,23002,23016,23020,23020,
     &      23018,23020,23020,23020,23020,23002,23018), i23000
23001    continue
c endselect
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sendalpha
cProlog
c This handles the case where select has been undefined.
c It will now be an ALPHA instead of SSELECT.
c also recognize 'end type'
      character*32 endmod
      integer lenmod, i

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      endmod = ' '
      do 23000 i=1,ltoken
         endmod(i:i) = char(token(i))
23000 continue
      lenmod = ltoken

      call outtab(0,0)

      if (endmod .eq. 'module') then
c the trailing space is necessary if a name is given:  'end module foo'
         call putqs('end module ')
      elseif (endmod .eq. 'select') then
         call putqs('end select')
      elseif (endmod .eq. 'type') then
         call putqs('end type')
      elseif (endmod .eq. 'where') then
         call putqs('end where')
      else
         call warn('end followed by improper keyword')
      endif

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sendmod(t,wswid)
cProlog
c end a module (subroutine or function)

      integer gnbtokw
      external gnbtokw
      integer t, wswid
      integer ierr, i
      integer i23002
      integer lnb
      external lnb
      character*32 endname

      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      endname = ' '

      ierr = 0
      if (level .ne. 0) then
         call inform('do-block level not 0 at end.')
         ierr = 1
      elseif (iflevel .ne. 0) then
         call inform('if-block level not 0 at end.')
         ierr = 1
      elseif (sellev .ne. 0) then
         call inform('select statement level not 0 at end.')
         ierr = 1
      endif
      level = 0
      iflevel = 0
      sellev = 0

      call outtab(0,0)
      call putqs('end')

      if (token(1) .eq. 127 ) then

         if (modtype(ifacelev) .eq. token(2)) then
            if (token(2) .eq. 96) then
               call putqs(' function')
            elseif (token(2) .eq. 95) then
               call putqs(' subroutine')
            elseif (token(2) .eq. 94) then
               call putqs(' program')
            elseif (token(2) .eq. 93) then
               call putqs(' block data')
            endif
         else
            ierr = 1
            if (token(2) .ne. 96) then
               call warn('Should be "end function"')
               call putqs(' function')
            elseif (token(2) .ne. 95) then
               call warn('Should be "end subroutine"')
               call putqs(' subroutine')
            elseif (token(2) .ne. 94) then
               call warn('Should be "end program"')
               call putqs(' program')
            elseif (token(2) .ne. 93) then
               call warn('Should be "end block data"')
               call putqs(' block data')
            endif
         endif

         if (token(2) .eq. 93) then
c assume 'data'
            t = gnbtokw(token,ltoken,wswid)
         endif

c look for optional name
         t = gnbtokw(token,ltoken,wswid)
         if (t .eq. 4) then
            endname = ' '
            do 23000 i=1,ltoken
               endname(i:i) = char(token(i))
23000       continue
            if (endname .ne. modname(ifacelev)) then
               call warn('Module name does not match')
               ierr = 1
            else
               call putqs(' ')
               call putqs(endname(1:ltoken))
            endif
            t = gnbtokw(token,ltoken,wswid)
         endif
      elseif (optlang .eq. 3) then
c select
         i23002= modtype(ifacelev)
            go to 23002
c -- case 96
23004    continue
            call putqs(' function ')
23005       go to 23003
c -- case 95
23006    continue
            call putqs(' subroutine ')
23007       go to 23003
c -- case 94
23008    continue
            call putqs(' program ')
23009       go to 23003
c -- case 93
23010    continue
            call putqs(' block data ')
c -- dispatch area for select
23011    go to 23003
23002    continue
         i23002=i23002-92
         if (i23002.lt. 1 .or. i23002.gt.4) go to 23003
         go to (23010,23008,23006,23004), i23002
23003    continue
c endselect
c GOTCHA - putting the subroutine name confuses the Guide90 compiler
c         if (modname(ifacelev) <> "?")
c            call putqs(modname(ifacelev)(1:lnb(modname(ifacelev))))
      endif

      if (ifacelev .eq. 0) then
         call setmod(63,1)
         modfun = 1
         modtype(ifacelev) = 0
c reset the label generator
         call reset
      endif
      if (t .eq. 1) then
         call endcomment(1, 0, token, ltoken, wswid)
      elseif (t .eq. 13) then
         call outlin
      elseif (ierr .eq. 0) then
         call spit(1)
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doerrp(argstk,i,j)
cProlog
      integer a1,a2,argstk(1),i,j,kk, iarg
      character*80 msg

      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6

      do 23000 iarg = i + 2, j
         a1 = argstk(iarg)
         a2 = argstk(iarg+1)
         write(msg,'(80a)') (char(evalst(a1-1+kk)), kk=1, a2-a1)
         call warn(msg(1:a2-a1))
23000 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doinfop(argstk,i,j)
cProlog
      integer a1,a2,argstk(1),i,j,kk, iarg
      character*80 msg

      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6

      do 23000 iarg = i + 2, j
         a1 = argstk(iarg)
         a2 = argstk(iarg+1)
         write(msg,'(80a)') (char(evalst(a1-1+kk)), kk=1, a2-a1)
         call rem(msg,a2-a1)
23000 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine dofile(fil)
cProlog
      character*(*) fil
      integer gnbtok,gtok,toktoi,zpakchrz
      external gnbtok,gtok,toktoi,zpakchrz
      character*(200) msg
      integer t,blanks(5),i,k
      integer idoflg, kesc
      integer i23007
      integer initio
      external initio


      character dirsep
      character*(256) incldirs(200)
      integer inclnum
      common /idircom/ inclnum
      common /idircoc/ dirsep, incldirs


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer ap,argstk(500),callst(200),nlb,plev(200)
      common /cargs/ ap,argstk,callst,nlb,plev


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      do 23000 i=1,5
         blanks(i) = 32
23000 continue

      indpc = 0
      indflg = 0

      if (initio(fil) .eq. 1) return

c reset the label counter
      call reset

c initialize the argument stack
      cp = 0
      ap = 1
      ep = 1

c count of square brackets
      nlb = 0

c do/while/if levels
      level = 0
      iflevel = 0
      dotype(0) = 0
      labn(0) = 0
      labb(0) = 0
      usen(0) = 0
      useb(0) = 0

c select stuff
      seltop = 0
      sellev = 0
      sellast = 1

c module name
      ifacelev = 0
      modtype(ifacelev) = 0
      call setmod(63,1)
      icontinue = 0

c idoflg = YES after seeing the label which terminates a regular do
      idoflg = 0

c process this file
c gtok returns the next token after expanding macros
c each for loop does one statement
c -- for([t=gtok(token,ltoken), t <> 28 , t=gtok(token,ltoken)])
      t=gtok(token,ltoken)
      go to 23002
23003 continue
         t=gtok(token,ltoken)
23002    if (.not.( t .ne. 28 )) go to 23004

c if the previous statement was the end of a standard do statement...
         if (idoflg .eq. 1) then
            call sedo
            idoflg = 0
         endif

         if (t .eq. 5) then
            indpc = ltoken
            t = gtok(token,ltoken)
c Useful if multiple white space tokens appear, but this should only happen
c if a macro expands to white space, perhaps that should signal a problem
c             indpc = 0
c             while (t == WHITE)
c                indpc = indpc + ltoken
c                t = gtok(token,ltoken)
c             endwhile
         else
            if (indflg .lt. 0) then
               indpc = indpc + indlev
               indflg = 0
            elseif (indflg .gt. 0) then
               indpc = indflg
               indflg = 0
            else
               indpc = 0
            endif
         endif

c look for a label
         if ( t .eq. 3) then
            if (optpretty .eq. 1) then
               call putchr(blanks,5-ltoken)
            else
               call outtab(0,0)
               indpc = indpc + ltoken
            endif
            call putchr(token,ltoken)
            if (dotype(level) .eq. 1) then
               if (labn(level) .eq. toktoi(token,ltoken)) then
                  idoflg = 1
               endif
            endif
            t = gtok(token,ltoken)
            if (t .eq. 5) then
               indpc = indpc + ltoken
               t = gtok(token,ltoken)
            endif
            call outtab(0,0)
         endif

c skip over any leading white space that may be inserted by macros.
c Another possible solution would be to remove leading and trailing white space
c from elseif bodies similar to the way it is remove from defines.
c while(t == 5)
23005    if (t .eq. 5) then
            t = gtok(token,ltoken)
            go to 23005
         endif
c endwhile

 1000    continue
         if ( t .eq. 127 ) then
c select
            i23007= token(2)
               go to 23007
c -- case 93
23009       continue

               call doblock
23010          go to 23008
c -- case 77
23011       continue

               call sdo
23012          go to 23008
c -- case 74
23013       continue

               call sedo
23014          go to 23008
c -- case 76
23015       continue

               call snext
23016          go to 23008
c -- case 75
23017       continue

               call sbrks
23018          go to 23008
c -- case 80
23019       continue

               call suntil
23020          go to 23008
c -- case 78
23021       continue

               call swhile
23022          go to 23008
c -- case 79
23023       continue

               call sewhile
23024          go to 23008
c -- case 81
23025       continue

               call sif
23026          go to 23008
c -- case 84
23027       continue

               call selif
23028          go to 23008
c -- case 82
23029       continue

               call selse
23030          go to 23008
c -- case 83
23031       continue

               call sendif

c MSUBROUTINE, MPROGRAM, MFUNCTION
c domod might need to be called by eatup to handle 'type function blahblah..'
c so domod does not do the eatup since it might get called by eatup
23032          go to 23008
c -- case 95
c -- case 94
c -- case 96
23033       continue

               call outtab(0,0)
               kesc = token(2)
               call domod(kesc)
               call eatup(0)
               call prolog
23034          go to 23008
c -- case 98
23035       continue

               call doend
23036          go to 23008
c -- case 97
23037       continue

               call doret(.false.)
23038          go to 23008
c -- case 92
23039       continue

               call doinc

c for -- discard marker character (used just to detect 'end for')
c rest of for accomplished with what was pushed back from arguments
23040          go to 23008
c -- case 90
23041       continue

               t = gnbtok(token,ltoken)
               go to 1000
23042          go to 23008
c -- case 91
23043       continue

               call sefor
23044          go to 23008
c -- case 86
23045       continue

               call sselect
23046          go to 23008
c -- case 87
c -- case 88
23047       continue

               kesc = token(2)
               call scase(kesc)
23048          go to 23008
c -- case 89
23049       continue

               call seselect
23050          go to 23008
c -- case 99
23051       continue

               call doiface

c then (N. B. this only occurs if 'then' is illegally placed)
23052          go to 23008
c -- case 85
23053       continue

               call warn("'then' is illegally placed in code")
23054          go to 23008
c -- case 102
23055       continue

               call sdo90
23056          go to 23008
c -- case 101
23057       continue

               call snext90
23058          go to 23008
c -- case 100
23059       continue

               call sbrks90
23060          go to 23008
c -- case 105
23061       continue

               call suntil90
23062          go to 23008
c -- case 103
23063       continue

               call swhile90
23064          go to 23008
c -- case 104
23065       continue

               call sewhile90
23066          go to 23008
c -- case 106
23067       continue

               call sselect90
23068          go to 23008
c -- case 107
c -- case 108
23069       continue

               kesc = token(2)
               call scase90(kesc)
23070          go to 23008
c -- case 109
23071       continue

               call seselect90

23072          go to 23008
c -- case 111
23073       continue

               call sdop
23074          go to 23008
c -- case 112
23075       continue

               call sselectp
23076          go to 23008
c -- case 113
23077       continue

               call scasep

23078          go to 23008
c -- case 115
23079       continue

               call doompdir
23080          go to 23008
c -- case 116
23081       continue

               call doalloc("Allocate")
23082          go to 23008
c -- case 117
23083       continue

               call doalloc("Deallocate")
23084          go to 23008
c -- case 118
23085       continue

               call doshape
c -- dispatch area for select
23086       go to 23008
23007       continue
            i23007=i23007-73
            if (i23007.lt. 1 .or. i23007.gt.45) go to 23008
            go to (23013,23017,23015,23011,23021,23023,23019,23025,23029
     &         ,23031,23027,23053,23045,23047,23047,23049,23041,23043,
     &         23039,23009,23033,23033,23033,23037,23035,23051,23059,
     &         23057,23055,23063,23065,23061,23067,23069,23069,23071,
     &         23008,23073,23075,23077,23008,23079,23081,23083,23085), i
     &         23007
23008       continue
c endselect
         elseif (t .eq. 6) then
            if (bckeep) then
               token(1) = cichar
               k = zpakchrz(msg, len(msg), token, ltoken - 1)
               call wrline(stdout, msg, k)
            endif
         elseif ( t.eq. 13) then
            if (bckeep) then
               call outlin
            endif
         elseif ( t.eq. 10) then
            if (optpretty .eq. 0) then
               call outfil(indpc)
            endif
         elseif ( t.eq. 1) then
            if (bckeep) then
               call endcomment(1, 0, token, ltoken, 0)
            endif
         else
cget past label field
            call outtab(0,0)
            call putchr(token,ltoken)
            call eatup(0)
         endif
c -- repeat
      go to 23003
23004 continue
c endfor

      if (level .ne. 0) call warn('do-block level not 0 at end of file.'
     &      )

      if (iflevel .ne. 0) call warn(
     &      'if-block level not 0 at end of file.')

      if (sellev .ne. 0) call warn(
     &      'select statement level not 0 at end of file.')

      if ( modname(0) .ne. '?') call warn(
     &      'Probably a missing end statement.')

c note, the file got closed by getlin
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doif (argstk,i,j)
cProlog
      integer argstk(1),i,j
      integer a1,a2,a3,a4,a5,i0,e1,e2,k,p,q
      save a1,a2,a3,a4,a5,i0,e1,e2,k,p,q
      logical match
      save match

      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6

      i0 = i + 2
      if (i0 + 2 .le. j) then
         a1 = argstk(i0+0)
         a2 = argstk(i0+1)
         a3 = argstk(i0+2)
         a4 = argstk(i0+3)
         e1 = a2 - 1
         e2 = a3 - 1
c while((a1 < a2) & (type(evalst(a1)) = 5))
23000    if ((a1 .lt. a2) .and. (type(evalst(a1)) .eq. 5)) then
            a1 = a1 + 1
            go to 23000
         endif
c endwhile
c while((e1 >= a1) & (type(evalst(e1)) = 5))
23002    if ((e1 .ge. a1) .and. (type(evalst(e1)) .eq. 5)) then
            e1 = e1 - 1
            go to 23002
         endif
c endwhile

c while((a2 < a3) & (type(evalst(a2)) = 5))
23004    if ((a2 .lt. a3) .and. (type(evalst(a2)) .eq. 5)) then
            a2 = a2 + 1
            go to 23004
         endif
c endwhile
c while((e2 >= a2) & (type(evalst(e2)) = 5))
23006    if ((e2 .ge. a2) .and. (type(evalst(e2)) .eq. 5)) then
            e2 = e2 - 1
            go to 23006
         endif
c endwhile
c At this point, a1 is the index of the first non-blank character of the
c first arg, e1 is the index of the last non-blank character of the first
c argument.  (Hence, if the argument is one character long, a1=e1.)  If
c the argument is null, (0 or more blank characters) then e1 = a1-1.
c Thus the length of the argument is always e1-a1+1, where null arguments
c have length = 0 by definition.  Similar statements apply for a2, e2.

c Argument 2 may be composed of several subexpressions, some of which
c may be null: "abc|xyz|" has 3 subexpressions, the last of which is null.
c Leading and trailing blanks are not significant, but embedded space is.
c p and q are used to point at the beginning and end of each subexpression
c in succession.  If a subexpression is null, then q = p-1.

         match = .false.
23008    continue
            p = a2
            q = min(p,e2)
c while((q<e2) & (type(evalst(q)) <> 124))
23010       if ((q.lt.e2) .and. (type(evalst(q)) .ne. 124)) then
               q = q + 1
               go to 23010
            endif
c endwhile
            if ((q.ge.p) .and. (type(evalst(q)) .eq. 124))then
c Move a2 to the beginning of the next subexpression
               a2 = q + 1
               q = q - 1
c while((q>=p) & (type(evalst(q)) = 5))
23012          if ((q.ge.p) .and. (type(evalst(q)) .eq. 5)) then
                  q = q - 1
                  go to 23012
               endif
c endwhile
c while((a2<=e2) & (type(evalst(a2)) = 5))
23014          if ((a2.le.e2) .and. (type(evalst(a2)) .eq. 5)) then
                  a2 = a2 + 1
                  go to 23014
               endif
c endwhile
            endif
            if (e1-a1 .eq. q-p) then
               match = .true.
               do 23016 k = 0,e1-a1
                  if (evalst(a1+k) .ne. evalst(p+k)) match = .false.
23016          continue
               if (match) go to 23009
            endif
            if (q .eq. e2) go to 23009
c -- repeat
         go to 23008
23009    continue

         if (match)then
c Push back arg3
            call pbstr(evalst(a3),a4-a3)
c Push back arg4, if it's there
         else
            if ( i0 + 3 .le.j) then
               a5 = argstk(i0+4)
               call pbstr(evalst(a4),a5-a4)
            endif
         endif
      endif
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doifdf(argstk,i,j)
cProlog
      integer lookup
      external lookup
      integer a1,a2,a3,a4,e1,argstk(1),i,j, i0

      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst


      i0=i+2
      if ( i0 + 1 .gt.j) return
      a1= argstk(i0)
      a2= argstk(i0+1)
      a3= argstk(i0+2)
c while((a1 < a2) & (type(evalst(a1)) = 5))
23000 if ((a1 .lt. a2) .and. (type(evalst(a1)) .eq. 5)) then
         a1 = a1 + 1
         go to 23000
      endif
c endwhile
      e1 = a2-1
c while((e1 > a1) & (type(evalst(e1)) = 5))
23002 if ((e1 .gt. a1) .and. (type(evalst(e1)) .eq. 5)) then
         e1 = e1 - 1
         go to 23002
      endif
c endwhile
      e1 = e1 + 1
      if (lookup(e1-a1,evalst(a1)).ge. 0 ) then
c arg1 IS defined
         call pbstr(evalst(a2),a3-a2)
      else
         if (i0+2 .le.j) then
c it isn't
            a4 = argstk(i0+3)
            call pbstr(evalst(a3),a4-a3)
         endif
      endif
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doimed(argstk,i,j)
cProlog
c if argument exists, blast it out immediately to output file
      integer a1,a2,argstk(1),i,j,larg,iarg
      character*(200) msg

      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr

      integer zpakchrz, idummy
      external zpakchrz
      do 23000 iarg = i + 2, j
         a1 = argstk(iarg)
         a2 = argstk(iarg+1)
         larg = min(a2-a1,73)
         if (larg .eq. 0) return
         idummy = zpakchrz(msg, len(msg), evalst(a1),larg)
         call wrline(stdout,msg,larg)
23000 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c if argument exists, blast it out as a comment to output file
      subroutine doremk(argstk,i,j)
cProlog
      integer a1,a2,argstk(1),i,j,larg,iarg
      character*(200) msg

      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr

      integer zpakchrz, idummy
      external zpakchrz
      msg(1:1) = cchar
      do 23000 iarg = i + 2, j
         a1 = argstk(iarg)
         a2 = argstk(iarg+1)
         larg = min(a2-a1,73)
         if (larg .eq. 0) return
         idummy = zpakchrz(msg(2:), len(msg), evalst(a1),larg)
         call wrline(stdout,msg,larg+1)
23000 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doinc
cProlog
c include file named in next token
      integer gtok, gnbtok, lnb
      external gtok, gnbtok, lnb

      character dirsep
      character*(256) incldirs(200)
      integer inclnum
      common /idircom/ inclnum
      common /idircoc/ dirsep, incldirs


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer lexwarn,realsize,intsize,wordsize
      integer inlev,finerr,nodblquo,yyline(5)
      common /ctlvars/ lexwarn,realsize,intsize,wordsize
      common /ctlvars/ inlev,finerr,nodblquo,yyline


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr

      character*(256) fil
c Twice as long as a Filename.
      character*512 ifil, tmpfil
      character*512 inccom
      integer t, i, j, ierr, iind, n, wswid
c
      fil = ' '
      n = len(fil)
      j = 0
      t=gnbtok(token,ltoken)
c check for quoted string
      if (t .eq. 34) then
         call outtab(0,0)
         call putqs('include ')
         call putchr(token,ltoken)
         call eatup(0)
         return
      endif

c save all tokens up to the newline
      wswid = 0
23000 continue
         if (t .eq. 13) then
            go to 23001
         elseif (t .eq. 1) then
            call putchr(token,ltoken)
            call outlin
            go to 23001
         elseif (t .eq. 5) then
            wswid = ltoken
         else
            if (j + ltoken .gt. n) call oops(
     &            'Filename too long in include')
            do 23002 i=1,ltoken
               fil(j+i:j+i) = char(token(i))
23002       continue
            j = j + ltoken
         endif
         t=gtok(token,ltoken)
c -- repeat
      go to 23000
23001 continue

c save current contents of pbbuf
      if (pbbuf(1) .eq. 1) then
         cbsav(inlev) = cbuf(1:lcbuf)
         lcsav(inlev) = lcbuf
      endif

      pblinl(inlev) = bp
      do 23004 i=1,bp
         pblins(i,inlev) = pbbuf(i)
23004 continue

c arrange for input to come from the new file
      bp = 0
      if (inlev .eq. 5) call oops('Includes nested too deeply')
      inlev = inlev + 1
      filnam(inlev) = fil
      yyline(inlev) = 0
      tmpfil = fil
      call filopn(iusin(inlev),ierr,lnb(tmpfil),tmpfil)
      if (ierr .ne. 0) then
c -- for([j = 1, j <= inclnum, j = j + 1])
         j = 1
         go to 23006
23007    continue
            j = j + 1
23006       if (.not.( j .le. inclnum)) go to 23008
            ifil = incldirs(j)
            iind = index(ifil," ") - 1
            if (ifil(iind:iind) .ne. dirsep) then
               iind = iind + 1
               ifil(iind:iind) = dirsep
            endif
            ifil = ifil(:iind) // filnam(inlev)
            tmpfil = ifil
            call filopn(iusin(inlev),ierr,lnb(tmpfil),tmpfil)
            if (ierr .eq. 0) then
               inccom = cchar // '%%%%%-included-file:' // ifil(1:lnb(
     &            ifil))
               call wrline( stdout, inccom, index(inccom,' ') )
               return
            endif
c -- repeat
         go to 23007
23008    continue
c endfor
         inlev = inlev - 1
         call oops('Could not open include file '//fil)
      endif

      inccom = cchar // '%%%%%-included-file:' // fil
      call wrline( stdout, inccom, index(inccom,' ') )

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c Set the macro supression character.
c This will prevent the macro from being expanded if it is followed by a
c specific character.
c This is useful to prevent 'real*8' from being expanded to 'doubleprecision*8'

      subroutine dosetsup(argstk,i,j)
cProlog

      integer a1,a2,a3,e1,argstk(1),i,j, i0, addr
      integer lookup
      external lookup

      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst


      i0=i+2
      if ( i0 + 1 .gt.j) return
      a1= argstk(i0)
      a2= argstk(i0+1)
      a3= argstk(i0+2)
c while((a1 < a2) & (type(evalst(a1)) = 5))
23000 if ((a1 .lt. a2) .and. (type(evalst(a1)) .eq. 5)) then
         a1 = a1 + 1
         go to 23000
      endif
c endwhile
      e1 = a2-1
c while((e1 > a1) & (type(evalst(e1)) = 5))
23002 if ((e1 .gt. a1) .and. (type(evalst(e1)) .eq. 5)) then
         e1 = e1 - 1
         go to 23002
      endif
c endwhile
      e1 = e1 + 1
      addr = lookup(e1-a1,evalst(a1))
      if (addr .ge. 0 ) then
c arg1 IS defined
         call setsup(addr, evalst(a2))
c       a3 - a2 should be 1
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c DOMOD - handles program,subroutine,function

      subroutine domod(td)
cProlog
      integer td,gnbtok
      external gnbtok
      integer t


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      if (ifacelev .eq. 0) then
         if (modname(0) .ne. '?') then
            call warn('Nested subroutine, function or program.')
            return
         endif
         if (td .eq. 96) then
            modfun = 1
         else
            modfun = 0
         endif
      endif

      t = gnbtok(token,ltoken)
      if (t .ne. 4) then
         call warn('subroutine,function, or program statement error.')
         return
      endif
      if (ltoken .gt. 32) then
         call warn('Module name too long.')
         return
      endif
      call setmod(token,ltoken)
      modtype(ifacelev) = td

      if (td .eq. 94) then
         call putqs('program ')
      elseif (td .eq. 95) then
         call putqs('subroutine ')
      else
         call putqs('function ')
      endif
      call putchr(token,ltoken)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine doiface
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      call outtab(0,0)
      call putqs('interface ')
      ifacelev = ifacelev + 1

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sendiface
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      ifacelev = ifacelev - 1
      call outtab(0,0)
      call putqs('end interface')
      return
      end
c--------------------------------------------------------------------------
c--------------------------------------------------------------------------
c
c ifstmt : logical .true. if the return is after an if statement.
c         In this case we must indent and add the trailing endif
c indent : indent column for if block.
c
      subroutine doret(ifstmt)
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      logical ifstmt
      integer t, iblock
      integer gnbtokw,gtok,lnb
      external gnbtokw,gtok,lnb
      integer nlev, wswid

      t = gnbtokw(token,ltoken,wswid)
      if ( t .eq. 40) then
creturn for function value
         if (modfun .eq. 0) then
            call warn('Attempt to return value from subroutine.')
            return
         endif
         if (ifstmt) then
            call putqs('then')
            call outlin
            iblock = 1
         else
            iblock = 0
         endif
         call outtab(0,iblock)
         call putqs(modname(0)(1:lnb(modname(0))))
         call putqs(' = ')
         nlev = 1
23000    continue
            t = gnbtokw(token,ltoken,wswid)
            if (t .eq. 40) then
               nlev = nlev + 1
            elseif (t .eq. 41) then
               nlev = nlev - 1
               if (nlev .eq. 0) go to 23001
            elseif ( t .eq. 13) then
               call warn('Missing right parenthesis.')
               return
            endif
            if (wswid .gt. 0) call outfil(wswid)
            call putchr(token,ltoken)
c -- repeat
         go to 23000
23001    continue
         call eatup(0)
         call outtab(0,iblock)
         call putqs('return')
         call outlin
         if (ifstmt) then
            iflevel = iflevel - 1
            call outtab(0,0)
            call putqs('endif')
            call outlin
            iflevel = iflevel + 1
         endif
cregular return statement
      else
         call outtab(0,0)
c XXX - extra space for test suite
         call putqs('return')
         if (t .eq. 1) then
            call endcomment(1, 0, token, ltoken, 0)
         elseif (t .eq. 13) then
            call outlin
         else
            call putchr(token,ltoken)
            call eatup(0)
         endif
      endif
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c DOOMPDIR - handle the exceptions for an OMP directive

      subroutine doompdir
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer gtok
      external gtok
      integer i, t, kesc, iblank, iprev, i23008

      call putqs(cchar)
      call putqs('$omp')
23000 continue
         t = gtok(token,ltoken)

         if (t .eq. 10) then
c --nopretty
            call conlin
            indpc = 0
         elseif (t .eq. 13) then
c We've found the end of this directive, must look ahead for another.
c If we find it, and column 6 is non-white, then it is a continuation
c of this directive.
c We only allow blank lines between continued directives
            iblank = 0
            iprev = 0
23002       continue
               t = gtok(token,ltoken)
               if (t .ne. 13 .and. t .ne. 10) go to 23003
               iblank = iblank + 1
               iprev = t
c -- repeat
            go to 23002
23003       continue
            if (t .eq. 127 .and. token(2) .eq. 115) then
               if (iprev .eq. 10) then
                  if (optlang .eq. 3 .or. optlang .eq. 5) then
                     call putqs(' &')
                  endif
                  call outlin
c Put out all the blank lines we found
                  do 23004 i=1,iblank
                     call outlin
23004             continue
                  call putqs(cchar)
                  call putqs('$omp')
                  go to 23000
               endif
            endif
            call outlin
c Put out all the blank lines we found
            do 23006 i=1,iblank
               call outlin
23006       continue
            call pbstr(token,ltoken)
            go to 23001
         elseif (t.ne. 127 ) then
            call putchr(token,ltoken)
         else
            kesc = token(2)
c select
            i23008= kesc
               go to 23008
c -- case 98
23010       continue

               call putqs('end')

23011          go to 23009
c -- case 77
c -- case 102
c -- case 111
23012       continue

               call putqs('do')

23013          go to 23009
c -- case 81
23014       continue

               call putqs('if')

23015          go to 23009
c -- case 90
23016       continue

               call putqs('for')

23017          go to 23009
c -- case 88
c -- case 108
23018       continue

               call putqs('default(')
               t = gtok(token,ltoken)
               call putchr(token,ltoken)
               call putqs(')')

23019          go to 23009
c -- case 115
23020       continue

c This is trigger by a continued omp directive
               if (nonblk .eq. 1) call conlin
c Reset lastpc since an omp directive starts at the begining of the line
               lastpc = 0
               call putqs(cchar)
               call putqs('$omp')

23021          go to 23009
c -- case (default)
23022       continue

               call warn('Unexpected keyword in directive')
c -- dispatch area for select
23023       go to 23009
23008       continue
            if ( i23008 .eq. 77) go to 23012
            if ( i23008 .eq. 81) go to 23014
            if ( i23008 .eq. 88) go to 23018
            if ( i23008 .eq. 90) go to 23016
            if ( i23008 .eq. 98) go to 23010
            if ( i23008 .eq. 102) go to 23012
            if ( i23008 .eq. 108) go to 23018
            if ( i23008 .eq. 111) go to 23012
            if ( i23008 .eq. 115) go to 23020
            go to 23022
23009       continue
c endselect
         endif
c -- repeat
      go to 23000
23001 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------
c
c Process a f90 allocate statement of the form:
c allocation ::=  ALLOCATE ( allocate-shape-spec-list )
c allocate-shape-spec ::=  [ lower-bound : ] upper-bound
c Genrate a call to macro Allocate with arguments of
c lower-bound and upper-bound, where lower-bound is explicitly set to 1 if not
c present in the ALLOCATE.
c
c allocate(foo(10))  =>   name(foo,(10))
c
      subroutine doalloc(name)
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      character*(*) name

      integer gnbtok
      external gnbtok
      integer t, iparen

      integer i, j, founde
      integer abuf(50000), iabuf
      integer index(3,10), iindex
c    1 = start of name
c    2 = start of dimension
c    3 = end of dimension

      iparen = 0
      iabuf = 1
      iindex = 1
      founde = 0
23000 continue
         t = gnbtok(token,ltoken)
         if (t .eq. 40) then
            iparen = iparen + 1
            abuf(iabuf) = 40
            if (iparen .eq. 1) then
c start of name
               index(1,iindex) = iabuf + 1
               index(2,iindex) = 0
               index(3,iindex) = 0
            elseif (iparen .eq. 2) then
c start of shape
               index(2,iindex) = iabuf
            endif
            iabuf = iabuf + 1
         elseif (t .eq. 41) then
            abuf(iabuf) = 41
            if (iparen .eq. 1) then
               if (index(2,iindex) .eq. 0) then
c no shape found
                  index(2,iindex) = iabuf
               endif
               if (founde .ne. 0) then
                  index(2,iindex) = iabuf - 1
               endif
            elseif (iparen .eq. 2) then
               index(3,iindex) = iabuf
            endif
            iabuf = iabuf + 1
            iparen = iparen - 1
            if (iparen .eq. 0) go to 23001
         elseif (t .eq. 44) then
            abuf(iabuf) = 44
            iabuf = iabuf + 1
            if (iparen .eq. 1) then
               if (index(2,iindex) .eq. 0) then
c no shape found
                  index(2,iindex) = iabuf - 1
               endif
               iindex = iindex + 1
               index(1,iindex) = iabuf
               index(2,iindex) = 0
               index(3,iindex) = 0
            endif
         elseif (t .eq. 61) then
            founde = iindex
            abuf(iabuf) = 61
            iabuf = iabuf + 1
         else
            do 23002 i=1,ltoken
               abuf(iabuf) = token(i)
               iabuf = iabuf + 1
23002       continue
         endif
c -- repeat
      go to 23000
23001 continue

      if (founde .gt. 0) iindex = iindex - 1

      do 23004 j = iindex, 1, -1
         call putbak(13)
         call putbak(41)
         do 23006 i = index(3,j), index(2,j), -1
            call putbak(abuf(i))
23006    continue
c no shape
         if (index(3,j) .ne. 0) call putbak(44)
         do 23008 i = index(2,j)-1, index(1,j), -1
            call putbak(abuf(i))
23008    continue

         call cpbstr('(')
         call cpbstr(name)
c         call cpbstr('      Allocate(')
23004 continue

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------
c
c Process a f90 array shape
c
c (10)     =>   1,10
c (2:10)   =>   2,10
c (20,30)  => 1,20,1,30
c
      subroutine doshape()
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer gnbtok
      external gnbtok
      integer t, iparen, istart

      integer abuf(50000), iabuf, i

      iparen = 0
      iabuf = 1
      istart = 0
23000 continue
         t = gnbtok(token,ltoken)
         if (t .eq. 40) then
            iparen = iparen + 1
            if (iparen .eq. 2) then
c assume lower limit is 1
c '1'
               abuf(iabuf) = 49
               abuf(iabuf+1) = 44
               iabuf = iabuf + 2
               istart = iabuf
            elseif (iparen .gt. 2) then
               abuf(iabuf) = 40
               iabuf = iabuf + 1
            endif
         elseif (t .eq. 41) then
            if (iparen .gt. 2) then
               abuf(iabuf) = 41
               iabuf = iabuf + 1
            endif
            iparen = iparen - 1
            if (iparen .eq. 0) go to 23001
         elseif (t .eq. 44) then
            if (iparen .eq. 2) then
               abuf(iabuf) = 44
c '1'
               abuf(iabuf+1) = 49
               abuf(iabuf+2) = 44
               iabuf = iabuf + 3
               istart = iabuf
            endif
         elseif (t .eq. 58) then
            if (iparen .eq. 2) then
c get rid of the assumed lower limit of 1
               do 23002 i = istart, iabuf
                  abuf(i-2) = abuf(i)
23002          continue
               iabuf = iabuf - 2
               abuf(iabuf) = 44
               iabuf = iabuf + 1
            endif
         else
            do 23004 i=1,ltoken
               abuf(iabuf) = token(i)
               iabuf = iabuf + 1
23004       continue
         endif
c -- repeat
      go to 23000
23001 continue

c      call putbak(NEWLINE)
c      do i = iabuf, 1, -1
c         call putbak(abuf(i))
c      enddo
c      call cpbstr('      Allocate')
      call putchr(abuf,iabuf-1)

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine eatup(lcode)
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer lcode
      integer wswid, icont
      integer gtok,gnbtokw
      external gtok,gnbtokw
      integer t, needlog, kesc, lcode0

      needlog = 0
      wswid = 0
      t = 0
      lcode0 = lcode
23000 continue
         if (t .eq. 0) t = gtok(token,ltoken)
         icont = lnbt

         if (t .eq. 10) then
c --nopretty
            call conlin
            indpc = 0
            t = gtok(token,ltoken)
         elseif (t .eq. 1) then
            if (icont .eq. 0 .and. lcode0 .gt. 0) then
               call putchr(code,lcode)
               lcode0 = 0
            endif
            call endcomment(1, icont, token, ltoken, wswid)
            if (icont .eq. 1) then
               call outtab(0,0)
               t = 0
               go to 23000
            else
               go to 23001
            endif
         elseif (t .eq. 13) then
            if (lcode0 .gt. 0) then
               call putchr(code,lcode)
            endif
            call outlin
            go to 23001
         elseif (t .eq. 59) then
            if (lcode0 .gt. 0) call putchr(code,lcode)
c Read ahead to get token length, then put back
            t = gnbtokw(token,ltoken,wswid)
            call pbstr(token,ltoken)
c If the white space and token fit on this line, put semicolon and white space
            if (lastpc + wswid + ltoken + 1 .le. col72) then
               call putqs(';')
               call outfil(wswid)
               go to 23001
            else
c otherwise, treat as an end of statement
               t = 27
            endif
         elseif (wswid .ne. 0) then
            call outfil(wswid)
            wswid = 0
         endif

         if (t .eq. 5) then
c trim trailing whitespace
            wswid = ltoken
         elseif (t .eq. 27) then
            call outlin
c preserve indention level
            indflg = indpc
            go to 23001
         elseif (t.ne. 127 ) then
            call putchr(token,ltoken)
         else
            kesc = token(2)
            if (kesc .eq. 96 .or. kesc .eq. 95) then
c function can follow a type
               call domod(kesc)
               needlog = 1
            elseif ( kesc .eq. 98 ) then
c for end=label in open statements
               call putqs('end')
            elseif (kesc .eq. 118) then
               call doshape
            else
               call warn('Misused keyword (?)')
            endif
         endif
         t = 0
c -- repeat
      go to 23000
23001 continue
      if ( needlog .ne. 0) call prolog
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine eval(argstk,i,j)
cProlog
ceval --expand args i through j; evaluate builtin or push back defn
      integer argstk(500),i,j,k,m,n,t,td, ano, ml
      integer lbl(5),lenlbl,nl,nat,natl
      integer labgen, i23000
      integer k2
      external labgen

      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      t = argstk(i)
      if (argstk(i+1) .eq. t ) then
         td = 0
      else
         td = evalst(t)
      endif

      if ( td .eq. 127 ) then
c select
         i23000= evalst(t+1)
            go to 23000
c -- case 65
23002    continue
            call dodef(argstk,i,j)
23003       go to 23001
c -- case 67
23004    continue
            call doifdf(argstk,i,j)
23005       go to 23001
c -- case 66
23006    continue
            call doif (argstk,i,j)
23007       go to 23001
c -- case 68
23008    continue
            call doerrp(argstk,i,j)
23009       go to 23001
c -- case 110
23010    continue
            call doinfop(argstk,i,j)
23011       go to 23001
c -- case 69
23012    continue
            call dodmp(argstk,i,j)
23013       go to 23001
c -- case 70
23014    continue
            call domath(argstk,i,j)
23015       go to 23001
c -- case 71
23016    continue
            call doimed(argstk,i,j)
23017       go to 23001
c -- case 114
23018    continue
            call doremk(argstk,i,j)
23019       go to 23001
c -- case 72
23020    continue
            call doundf(argstk,i,j)
23021       go to 23001
c -- case 73
23022    continue
            call domodn
23023       go to 23001
c -- case 119
23024    continue
            call dosetsup(argstk,i,j)
23025       go to 23001
c -- case (default)
23026    continue
            go to 10
c -- dispatch area for select
23027    go to 23001
23000    continue
         if ( i23000 .eq. 65) go to 23002
         if ( i23000 .eq. 66) go to 23006
         if ( i23000 .eq. 67) go to 23004
         if ( i23000 .eq. 68) go to 23008
         if ( i23000 .eq. 69) go to 23012
         if ( i23000 .eq. 70) go to 23014
         if ( i23000 .eq. 71) go to 23016
         if ( i23000 .eq. 72) go to 23020
         if ( i23000 .eq. 73) go to 23022
         if ( i23000 .eq. 110) go to 23010
         if ( i23000 .eq. 114) go to 23018
         if ( i23000 .eq. 119) go to 23024
         go to 23026
23001    continue
c endselect
         return
      endif

   10 continue
c do replacement on text
c first scan for how many @n's needed
      nat = 0
      do 23028 k=t,argstk(i+1)-2
         if (evalst(k) .eq. 64) then
            n = evalst(k+1) - 48
            if (n .lt. 1 .or. n .gt. 9) then
               call oops('Bad at sign argument in macro definition.')
            endif
            nat = max(nat,n)
         endif
23028 continue

      if (nat .gt. 0) natl = labgen(nat)

c now scan text backwards looking for $n, @n
c -- for([k = argstk(i+1) - 1, k>t , k = k-1])
      k = argstk(i+1) - 1
      go to 23030
23031 continue
         k = k-1
23030    if (.not.( k.gt.t )) go to 23032
         if (evalst(k-1) .ne. 64 .and. evalst(k-1) .ne. 36) then
            call putbak(evalst(k))
            go to 23031
         endif

c now decide if in COL1C area; if so, no expansion
         do 23033 k2 = k-1, t, -1
            if (evalst(k2) .eq. 6) then
               call putbak(evalst(k))
               go to 23031
            endif
            if (evalst(k2) .eq. 13) then
               go to 23034
            endif
23033    continue
23034    continue

         if (evalst(k-1) .eq. 64) then
            n = evalst(k) - 48
            nl = natl + n - 1
            call itotok(nl,lbl,lenlbl)
            call pbstr(lbl,lenlbl)
            k = k - 1
c $n or $* or $- or $$ or $(other)
         else
            if (evalst(k) .eq. 42) then
c -- for([ano=j-i-1,ano>0,ano=ano-1])
               ano=j-i-1
               go to 23035
23036          continue
                  ano=ano-1
23035             if (.not.(ano.gt.0)) go to 23037
                  n = i + ano + 1
                  m = argstk(n)
                  ml = argstk(n+1)-argstk(n)
                  call pbstr(evalst(m),ml)
                  if (ano.gt.1) call pbstr(44,1)
c -- repeat
               go to 23036
23037          continue
c endfor
            elseif (evalst(k) .eq. 45) then
c -- for([ano=j-i-1,ano>1,ano=ano-1])
               ano=j-i-1
               go to 23038
23039          continue
                  ano=ano-1
23038             if (.not.(ano.gt.1)) go to 23040
                  n = i + ano + 1
                  m = argstk(n)
                  ml = argstk(n+1)-argstk(n)
                  call pbstr(evalst(m),ml)
                  if (ano.gt.2) call pbstr(44,1)
c -- repeat
               go to 23039
23040          continue
c endfor
            elseif (evalst(k) .eq. 36) then
               continue
clook for digit
            else
               ano = evalst(k) - 48
               if (ano .ge.0 .and. ano .le.9) then
                  if ( ano .lt. j-i) then
                     n = i + ano + 1
                     m = argstk(n)
                     ml = argstk(n+1)- argstk(n)
                     call pbstr( evalst(m),ml)
                  endif
c other, leave it alone
               else
                  call putbak(evalst(k))
               endif
            endif
cskip over $
            k = k - 1
         endif
c -- repeat
      go to 23031
23032 continue
c endfor

cdo last character
      if (k .eq. t)
     &   call putbak(evalst(k))

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c GETLEV - find integer after break,next statements if any

      subroutine getlev(nlev,argstk,i,j)
cProlog

      integer nlev,toktoi
      integer i,j,argstk(1)
      integer a1,a2


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      if ( j .gt. i + 2) call warn('Too many arguments to break or next'
     &      )

      a1 = argstk(i+2)
      a2 = argstk(i+3)

      if (a2 .gt. a1) then
cthere is an argument
         nlev = toktoi(evalst(a1),a2-a1) - 1
      else
         nlev = 0
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c ISOMPDIR - return 1 if contents of cbuf is an OMP directive
c          - return 0 otherwise

      integer function isompdir(s)
cProlog
      character*(*) s

      if (s(2:5) .eq. '$omp') then
         isompdir = 1
      else
         isompdir = 0
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c GETLIN - read the next line
c        - send comments out immediately

      subroutine getlin
cProlog


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer icompile
      common /icom/icompile


      integer lexwarn,realsize,intsize,wordsize
      integer inlev,finerr,nodblquo,yyline(5)
      common /ctlvars/ lexwarn,realsize,intsize,wordsize
      common /ctlvars/ inlev,finerr,nodblquo,yyline


      integer i,c1,nextline,ompdir

      integer lnb, isompdir
      external lnb, isompdir

      ompdir = 0
      lcbuf = nextline(optnum)
      yyline(inlev) = yyline(inlev) + 1
c####END OF FILE
      if ( lcbuf .lt. 0)
     &   go to 2000
      if ( lcbuf .eq. 0) go to 200
      c1 = ichar(cbuf(1:1))
calternate method for end-of-file
      if ( c1 .eq. 46)
     &   go to 2000

c is this a comment?
      if (col1(c1) .or. type(c1) .eq. 1) then
         pbbuf(1) = 1
         bp = 1
         cbuf(1:1) = cchar
         if (isompdir(cbuf) .eq. 0) then
            return
         else
            ompdir = 1
         endif
      endif
  200 continue
      bp = lcbuf + 1
      pbbuf(1) = 13

c line goes backward into pbbuf
      do 23000 i=1,lcbuf
         pbbuf(bp + 1 - i) = ichar(cbuf(i:i))
23000 continue

c is this a continued line ?
      if (ompdir .eq. 1) then
c delete the '$omp" part.
         bp = bp - 4
c Replace the comment character with escape sequence
         pbbuf(bp) = 115
         pbbuf(bp + 1) = 127
         bp = bp + 1
c Look for continued omp directive
         if (bp .ge. 3 .and. col6 .and. type(pbbuf(bp-2)) .ne. 5) then
c replace continuation character with space and add NULL marker
            if (optlang .eq. 3) pbbuf(bp-2) = 32
c use LINEFEED as a continuation marker
c chkcont will replace the NULL with LINEFEED if hnl == YES
            if (hnl .eq. 0) then
               bp = bp + 1
               pbbuf(bp) = 10
            endif
            bp = bp + 1
            pbbuf(bp) = 0
         endif
      elseif (bp .ge. 7 .and. col6 ) then
         if (cbuf(1:5) .eq. '     ' .and. type(pbbuf(bp-5)) .ne. 5) then
            if (optlang .eq. 2) then
c delete the unwanted part
               bp = bp - 5
               pbbuf(bp) = 0
            elseif (optlang .eq. 3) then
c replace continuation character with space and add NULL marker
               pbbuf(bp-5) = 32
               bp = bp + 1
               pbbuf(bp) = 0
            endif
         endif
      elseif (optlang .eq. 5) then
         do 23002 i=bp,1,-1
            if (pbbuf(i) .eq. 38) then
               bp = i - 1
               go to 23003
            elseif (type(pbbuf(i)) .ne. 5 ) then
               go to 23003
            endif
23002    continue
23003    continue
      endif
      return

c end-of-file
 2000 continue
      call filcls(iusin(inlev))
      inlev = inlev - 1
      if (inlev .gt. 0) then
cback to previous file
         call popyin(iusin(inlev),lnb(filnam(inlev)),filnam(inlev))
         bp = pblinl(inlev)
         do 23004 i=1,bp
            pbbuf(i) = pblins(i,inlev)
23004    continue
         if (pbbuf(1) .eq. 1) then
            cbuf = cbsav(inlev)
            lcbuf = lcsav(inlev)
         endif
      else
         bp = 1
         pbbuf(bp) = 28
      endif
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c GETTOK - return next token, length of which is toksiz
c        - on NEWLINE, check for following NULL which means line is
c        - continued
c        - check if last non-white token indicates continuation
c        - Splitting one token across two lines works with f77, but not
c        - f90 free form.  Thus logical operators, real numbers, and
c        - muticharacter operators (//)
c        - must be recognized as one token.

      function gettok(token,toksiz)
cProlog
      integer gettok
      integer toksiz
      integer token(2)
      integer i,j,n,iamp
      integer i23002
      integer toktoi, zpakchrz
      character*(200) msg
      external toktoi, zpakchrz


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      character qte

      integer isompdir
      external isompdir

c look at next character first
c gettok is the only routine allowed to take anything OUT of pbbuf
23000 continue
         if (bp .eq. 0) call getlin

         token(1) = pbbuf(bp)
         gettok = type(token(1))
         if (gettok .eq. 28) go to 23001

         bp = bp - 1
         toksiz = 1

c pbbuf has only a NEWLINE, EOF, or COMMENT in position 1
c in the case of COMMENT, the text is still in cbuf
c a newline might not count if the following character is NULL
c ALPHAS, DIGITS can not be broken over continued lines
c select
         i23002= gettok
            go to 23002

c NEWLINE
c -- case 13
23004    continue


c check ahead for possible continuation
            if (bp .eq. 0) then
               call getlin
               call chkcont
               if (lnbt .eq. 1) go to 23000
               lnbt = 0
            endif
            go to 23001

c LETTER
23005       go to 23003
c -- case 2
23006    continue

            do 23007 i=bp,1,-1
               if (alphan(pbbuf(i)) .eq. 0) go to 23008
23007       continue
23008       continue
            do 23009 j=bp,i+1,-1
               token(2+bp-j) = pbbuf(j)
23009       continue
            toksiz = 1 + bp - i
            bp = i
            gettok = 4
            lnbt = 0
            go to 23001

c QUOTE
23011       go to 23003
c -- case 34
23012    continue

            i = bp
            if (optlang .eq. 5) then
               i = bp
c location of ampersand
               iamp = 0
23013          continue
                  if (pbbuf(i) .eq. 13) then
                     if (iamp .eq. 0) then
                        call inform('Missing continuation in string')
                        lnbt = 0
                        call putbak(13)
                        go to 23014
                     endif
                     do 23015 j=bp,iamp,-1
                        token(toksiz+bp+1-j) = pbbuf(j)
23015                continue
                     toksiz = toksiz + 1 + bp - iamp
                     call getlin
                     i = bp
c skip past first amperand (if present)
23017                continue
                        if (type(pbbuf(i)) .ne. 5 ) go to 23018
                        i = i - 1
c -- repeat
                     go to 23017
23018                continue
                     if (pbbuf(i) .eq. 38) then
                        i = i - 1
                        bp = i
                     else
                        i = bp
                     endif
                  elseif (pbbuf(i) .eq. 38) then
                     iamp = i + 1
                     i = i - 1
                  elseif (pbbuf(i) .eq. token(1)) then
                     if (pbbuf(i-1) .ne. token(1)) go to 23014
                     i = i - 1
                  else
                     i = i - 1
                  endif
c -- repeat
               go to 23013
23014          continue
            else
  500          continue
               if (i .eq. 1) then
c at end of line, check for continuation
                  do 23019 j=bp,2,-1
                     token(toksiz+bp+1-j) = pbbuf(j)
23019             continue
                  toksiz = toksiz + bp - 1
                  call getlin
                  if (pbbuf(bp) .eq. 0) then
                     bp = bp -1
                     i = bp
                     go to 500
                  endif
                  qte=char(token(1))
                  call inform('Missing quote ('//qte//')')
                  lnbt = 0
                  call putbak(13)
                  go to 23001
               endif

               if (pbbuf(i) .eq. token(1)) then
c probably the end but watch out for doubled quote
                  if (pbbuf(i-1) .ne. token(1)) go to 501
                  i = i - 1
               endif
               i = i - 1
               go to 500
  501          continue
            endif
            do 23021 j=bp,i,-1
               token(toksiz+bp+1-j) = pbbuf(j)
23021       continue
            toksiz = toksiz + 1 + bp - i
            bp = i - 1
            lnbt = 0
            go to 23001

c WHITE
23023       go to 23003
c -- case 5
23024    continue

            do 23025 i=bp,1,-1
               if (type(pbbuf(i)) .ne. 5 ) go to 23026
23025       continue
23026       continue
            do 23027 j=bp,i+1,-1
               token(2+bp-j) = pbbuf(j)
23027       continue
c If --pretty, only return size 1 tokens
            toksiz = min(1 + bp-i, maxws)
            bp = i
c avoid setting lnbt on white
            go to 23001

c DIGIT
23029       go to 23003
c -- case 3
23030    continue

            do 23031 i=bp,1,-1
               if (type(pbbuf(i)) .ne. 3) go to 23032
23031       continue
23032       continue

            do 23033 j=bp,i+1,-1
               token(2+bp-j) = pbbuf(j)
23033       continue

            toksiz = 1 + bp-i
            bp = i

c peek ahead for Hollerith
            if (pbbuf(bp) .eq. 72 .or. pbbuf(bp) .eq. 104) then
               if (toksiz .gt. 2) call oops('Hollerith count too large.'
     &               )
               n = toktoi(token,toksiz)
               if (n .ge. bp - 1) call oops('Hollerith constant error.')
               toksiz = toksiz + 1
               token(toksiz) = pbbuf(bp)
               do 23035 i = 1, n
                  token(toksiz+i) = pbbuf(bp - i)
23035          continue
               toksiz = toksiz+n
               gettok = 34
               bp = bp - (n+1)

c peek ahead for real number
            elseif (pbbuf(bp) .eq. 46) then
               do 23037 i=bp-1,1,-1
                  if (type(pbbuf(i)) .ne. 2) go to 23038
23037          continue
23038          continue
               if (i .ne. bp-1 .and. i .gt. 1) then
                  if (pbbuf(i) .ne. 46) call parreal(token,toksiz)
c else this is a logical operator
               else
                  call parreal(token,toksiz)
               endif
c look for exponent (d,D,e,E)  [ 4e4 ]
            elseif (pbbuf(bp) .eq. 68 .or. pbbuf(bp) .eq. 100 .or. pbbuf
     &         (bp) .eq. 69 .or. pbbuf(bp) .eq. 101) then
               call parreal(token,toksiz)
            endif
            lnbt = 0
            go to 23001

c NUMBER
23039       go to 23003
c -- case 14
23040    continue

            toksiz = pbbuf(bp)
            bp = bp - 1

            do 23041 j=bp,bp-toksiz+1,-1
               token(1+bp-j) = pbbuf(j)
23041       continue
            bp = bp - toksiz

c peek ahead for Hollerith
            if (pbbuf(bp) .eq. 72 .or. pbbuf(bp) .eq. 104) then
               if (toksiz .gt. 2) call oops('Hollerith count too large.'
     &               )
               n = toktoi(token,toksiz)
               if (n .ge. bp - 1) call oops('Hollerith constant error.')
               toksiz = toksiz + 1
               token(toksiz) = pbbuf(bp)
               do 23043 i = 1, n
                  token(toksiz+i) = pbbuf(bp - i)
23043          continue
               toksiz = toksiz+n
               gettok = 34
               bp = bp - (n+1)
            endif
            lnbt = 0
            go to 23001

c COMMENT
23045       go to 23003
c -- case 1
23046    continue

            if (bp .eq. 0) then
c whole line a comment
               comchar = token(1)
               token(1) = 6
               gettok = 6
               toksiz = lcbuf + 1
               do 23047 i = 2, lcbuf
                  token(i) = ichar(cbuf(i:i))
23047          continue
               token(toksiz) = 13
               lnbt = 0
               if (toksiz .gt. col72 + 1) then
                  if (isompdir(cbuf) .eq. 1) then
                     call inform("c$omp line too long.")
                     call inform(cbuf(1:lcbuf))
                  endif
               endif

            else
               comchar = token(1)
               token(1) = cichar
               do 23049 j=bp,2,-1
                  if (pbbuf(j) .eq. 13) go to 23050
23049          continue
23050          continue

c                    if (bckeep & (cp==0)) then
               if (bckeep) then
                  toksiz = 1
c Do not include the NEWLINE in the token
                  do 23051 i = bp, j+1, -1
                     toksiz = toksiz + 1
                     token(toksiz) = pbbuf(i)
23051             continue

                  if (cp .ne. 0) then
c If in a macro, then revert to mppl comment
c                    token(1) = POUND
c                    token(toksiz+1) = NEWLINE
c                    toksiz = toksiz+1
                     toksiz = zpakchrz(msg, len(msg), token, toksiz)
                     call wrline(stdout,msg,toksiz)
                  endif
               endif

c If in a macro, do not consume the NEWLINE else do
               if (cp .ne. 0) then
                  bp = j
               else
                  bp = j-1
               endif

c check ahead for possible continuation
               if (bp .eq. 0) then
                  call getlin
                  call chkcont
               endif
            endif

c ignore comments in macros
            if (cp.ne.0) go to 23000
            go to 23001

c PERIOD
c Look for logical operators (.or. .and.) and real numbers (.123)
23053       go to 23003
c -- case 46
23054    continue

            if (type(pbbuf(bp)) .eq. 3) then
               call parreal(token,toksiz)
               gettok = 3
            else
               do 23055 i=bp,1,-1
                  if (type(pbbuf(i)) .ne. 2) go to 23056
23055          continue
23056          continue
               if (i .ne. bp .and. i .gt. 1) then
                  if (pbbuf(i) .eq. 46) then
                     do 23057 j=bp,i,-1
                        token(2+bp-j) = pbbuf(j)
23057                continue
                     toksiz = 2 + bp - i
                     bp = i - 1
                     gettok = 16
                  endif
               endif
            endif
            lnbt = 0
            go to 23001

c Recognize multi character operators
c Recognize /, //, *, **
23059       go to 23003
c -- case 47
c -- case 42
23060    continue

            if (bp .gt. 1) then
               if (pbbuf(bp) .eq. gettok) then
                  token(2) = gettok
                  gettok = 15
                  toksiz = 2
                  bp = bp - 1
               endif
            endif
c can't continue on SLASH because of data statement
            if (gettok .eq. 47) then
               lnbt = 0
            else
               lnbt = 1
            endif
            go to 23001

c BSLASH
23061       go to 23003
c -- case 92
23062    continue

            lnbt = 1
            go to 23000

c ESCAPE
23063       go to 23003
c -- case 127
23064    continue

            token(2) = pbbuf(bp)
            toksiz = 2
            bp = bp - 1
            lnbt = 0
            go to 23001

c COL1C
23065       go to 23003
c -- case 6
23066    continue

            do 23067 j = bp, 1, -1
               token(2 + bp - j) = pbbuf(j)
               if (pbbuf(j) .eq. 13) go to 11
23067       continue
            call oops('Comment line token damaged. Internal error.')
   11       continue
            toksiz = 2 + bp - j
            bp = j - 1
            lnbt = 0
            go to 23001

c AMPERSAND
23069       go to 23003
c -- case 38
23070    continue

            if (optlang .eq. 5) then
               lnbt = 1
c look for token after white space
               do 23071 i=bp,1,-1
                  if (type(pbbuf(i)) .ne. 5 ) go to 23072
23071          continue
23072          continue
               if (pbbuf(i) .eq. 13) bp = i
               go to 23000

            else
               lnbt = contu(gettok)
               go to 23001
            endif

c remark: otherwise is a single letter token, returns as itself
23073       go to 23003
c -- case (default)
23074    continue

            lnbt = contu(gettok)
            go to 23001
c -- dispatch area for select
23075    go to 23003
23002    continue
         if ( i23002 .eq. 1) go to 23046
         if ( i23002 .eq. 2) go to 23006
         if ( i23002 .eq. 3) go to 23030
         if ( i23002 .eq. 5) go to 23024
         if ( i23002 .eq. 6) go to 23066
         if ( i23002 .eq. 13) go to 23004
         if ( i23002 .eq. 14) go to 23040
         if ( i23002 .eq. 34) go to 23012
         if ( i23002 .eq. 38) go to 23070
         if ( i23002 .eq. 42) go to 23060
         if ( i23002 .eq. 46) go to 23054
         if ( i23002 .eq. 47) go to 23060
         if ( i23002 .eq. 92) go to 23062
         if ( i23002 .eq. 127) go to 23064
         go to 23074
23003    continue
c endselect
c -- repeat
      go to 23000
23001 continue

c debug - print token
c      do i=1,toksiz
c         msg(i:i) = char(token(i))
c      enddo
c      print *, msg(1:toksiz)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c
c Parse the remainder of a real number
c bp is pointing to the decimal point
c
      subroutine parreal(token,ltoken)
cProlog


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer token(*), ltoken

c look for period
      if (pbbuf(bp) .eq. 46) then
         ltoken = ltoken + 1
         token(ltoken) = pbbuf(bp)
         bp = bp - 1
      endif

c collect digits
23000 continue
         if (type(pbbuf(bp)) .ne. 3) go to 23001
         ltoken = ltoken + 1
         token(ltoken) = pbbuf(bp)
         bp = bp - 1
c -- repeat
      go to 23000
23001 continue

c look for exponent (d,D,e,E)
      if (pbbuf(bp) .eq. 68 .or. pbbuf(bp) .eq. 100 .or. pbbuf(bp) .eq. 
     &   69 .or. pbbuf(bp) .eq. 101) then
         ltoken = ltoken + 1
         token(ltoken) = pbbuf(bp)
         bp = bp - 1

c look for + or =
         if (pbbuf(bp) .eq. 43 .or. pbbuf(bp) .eq. 45) then
            ltoken = ltoken + 1
            token(ltoken) = pbbuf(bp)
            bp = bp - 1
         endif

c collect exponent
23002    continue
            if (type(pbbuf(bp)) .ne. 3) go to 23003
            ltoken = ltoken + 1
            token(ltoken) = pbbuf(bp)
            bp = bp - 1
c -- repeat
         go to 23002
23003    continue
      endif

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------
c
c Check for continuation
c return YES if this line is continued, else NO
c
      subroutine chkcont
cProlog


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer i

      if (pbbuf(bp) .eq. 0) then
c delete NULL marker
         bp = bp - 1

         if (hnl .eq. 1) then
            if (optpretty .eq. 1) then
c remove all leading white space since we're controlling indention
               do 23000 i=bp,1,-1
                  if (type(pbbuf(i)) .ne. 5 ) go to 23001
23000          continue
23001          continue
               bp = i
            endif
c insert linefeed
            bp = bp + 1
            pbbuf(bp) = 10
         endif
         lnbt = 1
      elseif (lnbt .eq. 1 .and. pbbuf(bp) .ne. 28) then
         if (hnl .eq. 1) then
            if (optpretty .eq. 1) then
c remove all leading white space since we're controlling indention
               do 23002 i=bp,1,-1
                  if (type(pbbuf(i)) .ne. 5 ) go to 23003
23002          continue
23003          continue
               bp = i
            elseif (optlang .eq. 2) then
c remove leading space similar to conventional continuation
               do 23004 i=1,6
                  if (type(pbbuf(bp)) .ne. 5 ) go to 23005
                  bp = bp - 1
23004          continue
23005          continue
            endif
c insert linefeed
            bp = bp + 1
            pbbuf(bp) = 10
         endif
      endif

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function gnbtok(token,ltoken)
cProlog
      integer gnbtok,token(1),ltoken
      integer gtok
      external gtok

      gnbtok = gtok(token,ltoken)
c while(gnbtok == 5)
23000 if (gnbtok .eq. 5) then
         gnbtok = gtok(token,ltoken)
         go to 23000
      endif
c endwhile

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function gnbtokw(token,ltoken,wswid)
cProlog
      integer gnbtokw,token(1),ltoken,wswid
      integer gtok
      external gtok

      wswid = 0
      gnbtokw = gtok(token,ltoken)
c while(gnbtokw == 5)
23000 if (gnbtokw .eq. 5) then
         wswid = wswid + ltoken
         gnbtokw = gtok(token,ltoken)
         go to 23000
      endif
c endwhile

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function gtok(token,ltoken)
cProlog
      integer gtok
      integer token(1),ltoken
      integer lookup,addr,supchar
      integer gettok,peek,putdef
      external lookup,gettok,peek,putdef
      integer t


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer ap,argstk(500),callst(200),nlb,plev(200)
      common /cargs/ ap,argstk,callst,nlb,plev


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer lexwarn,realsize,intsize,wordsize
      integer inlev,finerr,nodblquo,yyline(5)
      common /ctlvars/ lexwarn,realsize,intsize,wordsize
      common /ctlvars/ inlev,finerr,nodblquo,yyline


c return with next token after resolving macro definitions
c -- for([t=gettok(token,ltoken), t <> 28 , t=gettok(token,ltoken)])
      t=gettok(token,ltoken)
      go to 23000
23001 continue
         t=gettok(token,ltoken)
23000    if (.not.( t .ne. 28 )) go to 23002

c if token is alphanumeric, see if it is a macro and expand it if
c we are not inside square brackets
         if (t .eq. 4 ) then
cno lookup if inside brackets
            if (nlb .gt. 0) go to 100
            addr = lookup(ltoken,token)
            if (addr .eq. -1) go to 100

            call getsup(addr, supchar)
            t = peek()
            if (supchar .ne. 0 .and. t .eq. supchar) goto 100

c token is a macro ; set up stack frame
            cp = cp + 1
            if (cp .gt. 200) then
               cp = 200
               call oops('Macro error, call stack overflow.')
            endif
            mline(cp) = yyline(inlev)
            mlev(cp) = inlev
            callst(cp) = ap

c start stack frame
            call push(ep,argstk,ap)

c stack definition
            if (putdef(addr,evalst,ep,50000) .ne. 0) then
               call oops('Evaluation stack overflow.')
            endif

            call push(ep,argstk,ap)

c stack name
            call putarg(token,ltoken)
            call push(ep,argstk,ap)

c add parenthesis if they are not already on the way
            if (t .ne. 40) then
c push back parens
               call putbak(41)
               call putbak(40)
            else
23003          continue
                  t=gettok(token,ltoken)
                  if (t .eq. 40) go to 23004
c -- repeat
               go to 23003
23004          continue
               call putbak(40)
            endif
            plev(cp) = 0
c start collecting arguments
            go to 23001

         elseif (t.eq.91) then
            nlb = nlb + 1
cstrip one level of []
            if (nlb .eq. 1) go to 23001

         elseif (t.eq.93) then
            nlb = nlb - 1
            if (nlb .lt. 0) then
               call inform('Extraneous right square bracket.')
               nlb = 0
            endif
            if (nlb .eq. 0) go to 23001
         endif
  100    continue

c not in a macro, this is the token we want
         if (cp .eq. 0)
     & then
            gtok = t
            return
         endif

c if inside square brackets just copy token
         if (nlb .gt. 0) then
            call putarg(token,ltoken)

c argument collection, look for commas, but not inside parens
         elseif (t.eq.40) then
            if (plev(cp) .gt. 0) call putarg(token,ltoken)
            plev(cp) = plev(cp) + 1

         elseif (t.eq.41) then
            plev(cp) = plev(cp) - 1
            if (plev(cp) .gt. 0 ) then
               call putarg(token,ltoken)

cend of argument list found !
            else
c evaluate the macro and its arguments
               argstk(ap) = ep
               call eval(argstk,callst(cp),ap-1)
cpop evaluation stack
               ap = callst(cp)
               ep = argstk(ap)
               cp = cp - 1
            endif

         elseif (t .eq. 44 .and. plev(cp) .eq. 1) then
cnew arg
            call push(ep,argstk,ap)

         else
cjust stack it
            call putarg(token,ltoken)
         endif
c -- repeat
      go to 23001
c end of gettok call loop
23002 continue
c endfor

c arrive here if end-of-file, check for screwed up [] or parens
      if (cp .ne. 0 ) call oops( 
     &      'Unexpected end of file in macro arguments--missing ) or ] ?
     &')

      if (nlb .gt. 0) call oops(
     &      'Missing right square bracket, at end-of-file.')

      gtok = 28
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine init
cProlog


      character dirsep
      character*(256) incldirs(200)
      integer inclnum
      common /idircom/ inclnum
      common /idircoc/ dirsep, incldirs


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer i

c default include directory separator (a slash)
      dirsep = '/'
      inclnum = 0

c type table and switch tables
      call setsen
      do 23000 i=0,127
         type(i) = i
         alphan(i) = 0
         contu(i) = 0
         col1(i) = .false.
23000 continue
      do 23002 i=48,57
         type(i) = 3
         alphan(i) = 1
23002 continue
      do 23004 i=65,90
         type(i) = 2
         alphan(i) = 1
23004 continue
      do 23006 i=97,122
         type(i) = 2
         alphan(i) = 1
23006 continue
      type(32) = 5
      type(9) = 5
      type(95) = 2
      type(39) = 34
      type(33) = 1
      type(35) = 1
      type(127 ) = 127

      alphan(95) = 1
c characters which cause a line to be continued
      contu(43) = 1
      contu(45) = 1
      contu(42) = 1
c can't do it on SLASH because of data statement
      contu(40) = 1
      contu(92) = 1
      contu(61) = 1
      contu(44) = 1
      contu(124) = 1
      contu(38) = 1
      contu(126) = 1
      contu(60) = 1
      contu(62) = 1
c characters which are a comment in column 1
c 'c'
      col1(99) = .true.
c 'C'
      col1(67) = .true.
      col1(42) = .true.
cXXX        col1(BANG) = .true.
c column 6 continuation conventions
      col6 = .true.

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine init1
cProlog

      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6

      integer i

      if (optpretty .eq. 1) then
         maxws = 1
      else
         maxws = 20000+1
      endif

      if (optlang .eq. 2) then
c           if (optpretty == YES) type(SEMICOLON) = NEWLINE
         type(59) = 13
         cchar = 'c'
         optrel = 2
      elseif (optlang .eq. 3) then
         cchar = '!'
         if (optpretty .eq. 1 .and. colcom .eq. -1) colcom = 40
      elseif (optlang .eq. 5) then
         cchar = '!'
         if (optpretty .eq. 1 .and. colcom .eq. -1) colcom = 40
c        reset mppl continuation and comment defaults
c         type(POUND) = POUND  # need to recognize POUND for mppl.std
         do 23000 i=0,127
            contu(i) = 0
            col1(i) = .false.
23000    continue
         col6 = .false.
      endif

      if (optpretty .eq. 0) hnl = 1
      cichar = ichar(cchar)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      integer function initio(fil)
cProlog
      character*(*) fil
      integer il, ierr
      integer lnb
      external lnb


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer lexwarn,realsize,intsize,wordsize
      integer inlev,finerr,nodblquo,yyline(5)
      common /ctlvars/ lexwarn,realsize,intsize,wordsize
      common /ctlvars/ inlev,finerr,nodblquo,yyline


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


c initialize input and output buffers
      character*(256) tmpfil

      inlev = 1
      filnam(inlev) = fil
      call setmod(63,1)
      yyline(inlev) = 0
coutput line empty
      lastpc = 0
cpush back buffer empty
      bp = 0
      icontinue = 0
   10 continue

c -- for([il = len(filnam(inlev)), il > 0, il=il-1])
      il = len(filnam(inlev))
      go to 23000
23001 continue
         il=il-1
23000    if (.not.( il .gt. 0)) go to 23002
         if (' ' .ne. filnam(inlev)(il:il)) go to 20
c -- repeat
      go to 23001
23002 continue
c endfor
   20 continue

      if ( verbose) then
         write(stderr,101) filnam(inlev)(1:il)
  101    format('   Preprocessing ',a)
      endif

      tmpfil = filnam(inlev)
      call filopn(iusin(inlev),ierr,lnb(tmpfil),tmpfil)
      if (ierr .ne. 0 ) go to 200
      initio = 0
      return

  200 continue
      write(stderr,100) filnam(inlev)(1:il)
  100 format('  Could not open input file ',a)

      call warn(
     &   '  ******** Will proceed if others can be opened **********')

      initio = 1
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c return the current value of modnam
      subroutine domodn
cProlog
      integer lnb
      external lnb


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      call cpbstr(modname(0)(1:lnb(modname(0))))

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c convert n into a string in token, length to ltoken
      subroutine itotok(n,token,ltoken)
cProlog
      integer n,token(1),ltoken
      character*16 target
      integer i,j

      if (n .eq. 0) then
         ltoken = 1
         token(1) = 48
         return
      endif

      write(target,100) n
  100 format(i16)

      do 23000 i=1,16
         if ( target(i:i) .ne. ' ') go to 23001
23000 continue
23001 continue

      i = i - 1
      ltoken = 16 - i
      do 23002 j = 1, ltoken
         token(j) = ichar(target(i+j:i+j))
23002 continue

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function labgen(n)
cProlog
      integer labgen,n


      integer labnxt,laborg
      common /labg/labnxt,laborg


      labgen = labnxt
      labnxt = labnxt + n

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine reset
cProlog


      integer labnxt,laborg
      common /labg/labnxt,laborg


      labnxt = laborg

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine setmod(name,lname)
cProlog
      integer name(1),lname,i


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      modname(ifacelev) = ' '
      do 23000 i=1,lname
         modname(ifacelev)(i:i) = char(name(i))
23000 continue

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine setorg(n)
cProlog
      integer n


      integer labnxt,laborg
      common /labg/labnxt,laborg


      laborg = n

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine oops(msg)
cProlog
      character*(*) msg

      call warn(msg)
      call finish(1)

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine warn(msg)
cProlog
      character*(*) msg


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      call inform(msg)

c attempt to recover to next line
      call outlin
      bp = 1

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine inform(msg)
cProlog
      character*(*) msg
      integer igoof,ilev,jlev,a1,a2,j,il, ip, jl,lnb,k
      external lnb
      character*56 mname


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer ap,argstk(500),callst(200),nlb,plev(200)
      common /cargs/ ap,argstk,callst,nlb,plev


      integer lexwarn,realsize,intsize,wordsize
      integer inlev,finerr,nodblquo,yyline(5)
      common /ctlvars/ lexwarn,realsize,intsize,wordsize
      common /ctlvars/ inlev,finerr,nodblquo,yyline


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      finerr = finerr+1
      write(stderr,90) modname(0),msg
      ilev = inlev
      if (pbbuf(bp) .eq. 28) ilev = ilev + 1
      jlev = ilev
c while(ilev > 0)
23000 if (ilev .gt. 0) then
         igoof = yyline(ilev)
c -- for([il = 1, filnam(ilev)(il:il) <> ' ', il=il+1])
         il = 1
         go to 23002
23003    continue
            il=il+1
23002       if (.not.( filnam(ilev)(il:il) .ne. ' ')) go to 23004
            if (il .eq. len(filnam(ilev))) go to 23004
c -- repeat
         go to 23003
23004    continue
c endfor

         if (ilev .eq. jlev) then
            write(stderr,100) igoof,filnam(ilev)(1:il)
            do 23005 ip = 1,cp
               a1 = argstk(callst(ip)+1)
               a2 = argstk(callst(ip)+2) - 1
               jl = min(a2-a1+1,56)
               do 23007 j=1,jl
                  mname(j:j)=char(evalst(a1-1+j))
23007          continue
               if (mname.eq.'Errprint') go to 23006
               if (mname.eq.'Infoprint') go to 23006
               k=lnb(filnam(mlev(ip)))
               if (ip .eq. 1) then
                  write(stderr,103)  mname(1:jl),mline(ip),filnam(mlev(
     &               ip))(1:k)
               else
                  write(stderr,104)  mname(1:jl),mline(ip),filnam(mlev(
     &               ip))(1:k)
               endif
23005       continue
23006       continue
         else
            write(stderr,101) igoof-1,filnam(ilev)(1:il)
         endif

         ilev = ilev - 1
         go to 23000
      endif
c endwhile

      call outlin
      return

   90 format(' mppl error : module = ',a/1x,a)
  100 format(' Near line no. ',i5,' in file ',a)
  101 format(' (Included from line no. ',i5,' in file ',a,')')
  103 format(' ERROR OCCURRED DURING MACRO/KEYWORD PROCESSING:'/ 
     &   ' Error in  ',a,' which began on line ',i5,' of file ',a)
  104 format(' which called  ',a,' on line ',i5,' of file ',a)

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c put out the statement 'n continue'

      subroutine outcnt(n, indent, iblock)
cProlog
      integer n, indent, iblock


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      if (optpretty .eq. 1) then
         if (lastpc .gt. 0) call inform(
     &         'Label not permitted on this kind of statement.')
      else
         lastpc = 0
      endif

      call puti5(n)
      call outtab(indent, iblock)
      call putqs('continue')
      call outlin

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c put the statment 'go to n' into the output buffer, call outlin

      subroutine outgo(n)
cProlog
      integer n

      call putqs('go to ')
      call puti5(n)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c if a label is already present, output a continue on it
c otherwise do nothing
c used to handle labels on statements that generate their own label
c like while, block do

      subroutine outifl
cProlog


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      if (lastpc .ne. 0) then
         call outtab(0,0)
         call putqs('continue')
         call outlin
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c output one line
      subroutine outlin
cProlog


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      character*(200) msg
      integer zpakchrz, n
      external zpakchrz

      n = zpakchrz(msg, len(msg), outbuf,lastpc)
      call wrline(stdout,msg,n)
      lastpc = 0
      icontinue = 0
      nonblk = 0

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c get past statement field
c if already there, do nothing

      subroutine outtab(indent, iblock)
cProlog


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer indent, iblock
      integer newpc, i

c        if (lastpc > 5)
c           return

      if (optpretty .eq. 1) then
         newpc = 6+(iflevel + level + sellev + ifacelev+ icontinue)*
     &      indlev
      else
c indent = 0, uses indpc
         newpc = max(indent,indpc) + iblock*indlev + icontinue*indlev
      endif

      do 23000 i=lastpc+1,newpc
         outbuf(i) = 32
23000 continue

      lastpc = max(lastpc,newpc)
      conpc = newpc

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c Add some spaces into the output

      subroutine outfil(width)
cProlog


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins

cinclude_cstat

      integer newpc, i, width

      newpc = lastpc + width
      do 23000 i=lastpc+1,newpc
         outbuf(i) = 32
23000 continue
      lastpc = newpc

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine pbstr(in,n)
cProlog

c push back in

      integer in(1)
      integer n
      integer i,bpnew


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      bpnew = bp + n
      if (bpnew .gt. 50000) call oops('Macro processor error in pbstr:'
     &      // ' too many characters pushed back')

      do 23000 i = 1, n
         pbbuf(bp + i) = in(n + 1 - i)
23000 continue

      bp = bpnew

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function peek()
cProlog

c return next non-blank character
c only checks in this line

      integer peek


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer i

      do 23000 i=bp,2,-1
         if (type(pbbuf(i)) .ne. 5) go to 23001
23000 continue
23001 continue

      peek = pbbuf(i)
      return

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine prolog
cProlog

c insert a statement Prolog after each module card


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      if (ifacelev .eq. 0 .and. optmacro .eq. 1) then
         call putbak(13)
         call cpbstr('      Prolog')
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine push(ep,argstk,ap)
cProlog
      integer ep,argstk(500),ap

      if (ap .ge. 500) call oops(
     &      'Macro processor error: argument stack overflow.')
      argstk(ap) = ep
      ap = ap + 1

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine putarg(cout,n)
cProlog
      integer i,n
      integer cout(1)


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      if (ep + n .gt. 50000) call oops(
     &      'Macro processor error: evaluation stack overflow.')

      do 23000 i=1,n
         evalst(ep - 1 + i ) = cout(i)
23000 continue
      ep = ep + n

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine putbak(c)
cProlog
      integer c


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      bp = bp + 1
      if (bp .gt. 50000) call oops('Macro processor error: '// 
     &      'too many characters pushed back.')
      pbbuf(bp) = c

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine putchr(cout,n)
cProlog
      integer i,n, nc, idone
      integer cout(1)
      integer zpakchrz, k
      character*(200) msg


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      if ( lastpc + n .le. col72 ) then
         do 23000 i=1,n
            outbuf(lastpc + i) = cout(i)
23000    continue
         lastpc = lastpc + n
         nonblk = 1
         return
      endif

c doesn't fit
c blast out existing line, set up for continuation
c if characters have been written past conpc that are nonblank
      if (lastpc .gt. conpc .and. nonblk .eq. 1) call conlin

      call outtab(0,0)

c watch out for extremely long tokens
      nc = 0
23002 continue
         if ( lastpc + n - nc .le. col72) then
            do 23004 i=1,n-nc
               outbuf(lastpc + i) = cout(nc + i)
23004       continue
            lastpc = lastpc + n - nc
            nonblk = 1
            return
         endif
         if (optlang .eq. 2) then
            idone = col72 - lastpc
            do 23006 i=1,idone
               outbuf(lastpc + i) = cout(nc + i)
23006       continue
            k = zpakchrz(msg,col72, outbuf,col72)
            call wrline(stdout,msg,k)
            lastpc = 6
         elseif (optlang .eq. 3) then
            idone = col72 - lastpc - 1
            do 23008 i=1,idone
               outbuf(lastpc + i) = cout(nc + i)
23008       continue
            outbuf(col72) = 38
            k = zpakchrz(msg,col72, outbuf,col72)
            call wrline(stdout,msg,k)
            lastpc = 0
            call outtab(0,0)
            lastpc = lastpc + 1
            outbuf(lastpc) = 38
         elseif (optlang .eq. 5) then
            idone = col72 - lastpc - 1
            do 23010 i=1,idone
               outbuf(lastpc + i) = cout(nc + i)
23010       continue
            outbuf(col72) = 38
            k = zpakchrz(msg,col72, outbuf,col72)
            call wrline(stdout,msg,k)
            lastpc = 0
            call outtab(0,0)
            lastpc = lastpc + 1
            outbuf(lastpc) = 38
         else
            call oops("Unexpected value of oplang")
         endif
         nc = nc + idone
c -- repeat
      go to 23002

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c continue the current line

      subroutine conlin
cProlog


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      integer i

      if (optlang .eq. 2) then
         call outlin
         do 23000 i=1,5
            outbuf(i) = 32
23000    continue
         outbuf(6) = 38
         lastpc = 6
      elseif (optlang .eq. 3) then
         outbuf(lastpc + 1) = 32
         outbuf(lastpc + 2) = 38
         lastpc = lastpc + 2
         call outlin
      elseif (optlang .eq. 5) then
         outbuf(lastpc + 1) = 38
         lastpc = lastpc + 1
         call outlin
      else
         call oops("Unexpected value for optlang - conlin")
      endif
      conpc = lastpc

      icontinue = 1
      if (optpretty .eq. 1) call outtab(0,0)

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine puti5(n)
cProlog
c adds the character representation of n to the current output line
c used only for label fields...right adjust in 5 characters

      integer n, m, i
      integer lb(5)

      m = n
      lb(5) = mod(m,10) + 48
      m = m/10
      do 23000 i=4,1,-1
         if ( m .eq. 0) then
            lb(i) = 32
         else
            lb(i) = mod(m,10 ) + 48
            m = m/10
         endif
23000 continue

      call putchr(lb,5)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine putlab(token,ltoken)
cProlog
      integer token(1),ltoken, i


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      do 23000 i=1,ltoken
         outbuf(i) = token(i)
23000 continue

      do 23002 i=ltoken+1,6
         outbuf(i) = 32
23002 continue

      lastpc = 6

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c PUTQS -

      subroutine putqs(str)
cProlog
      character*(*) str
      integer i,n
      integer qs(80)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      n = len(str)
      do 23000 i=1,n
         qs(i) = ichar(str(i:i))
23000 continue

      call putchr(qs,n)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sbrks
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer t,gnbtok,toktoi
      external gnbtok,toktoi
      integer nlev

c look for digit after the next
      t = gnbtok(token,ltoken)
      if ( t .eq. 3) then
         nlev = toktoi(token,ltoken) - 1
         t = gnbtok(token,ltoken)
      else
         nlev = 0
      endif

      if (level-nlev .le. 0) then
         call warn('Illegal -break- statement, level wrong')
      else
         call outtab(0,0)
         call outgo(labb(level-nlev))
         useb(level-nlev) = 1
c shouldn't be anything else here
         if (t .eq. 1) then
            call endcomment(1, 0, token, ltoken, 0)
         elseif (t .eq. 13) then
            call outlin
         else
            call warn('Illegal syntax in -break- statement.')
         endif
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sbrks90
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer t,gtok,toktoi,wswid
      external gtok,toktoi
      integer nlev

      wswid = 0
c look for digit after the next
      t = gtok(token,ltoken)
      if (t .eq. 5) then
         wswid = ltoken
         t = gtok(token,ltoken)
      endif

      if (t .eq. 3) then
         nlev = toktoi(token,ltoken) - 1
         t = gtok(token,ltoken)
      else
         nlev = 0
      endif

      if (level-nlev .le. 0) then
         call warn('Illegal -break- statement, level wrong')
      else
         call outtab(0,0)
         if (nlev .eq. 0) then
            call putqs('exit')
         else
            useb(level-nlev) = 1
            call outgo(labb(level-nlev))
         endif

         if (t .eq. 5) then
            wswid = ltoken
            t = gtok(token,ltoken)
         endif

c shouldn't be anything else here
         if (t .eq. 1) then
            call endcomment(1, 0, token, ltoken, wswid)
         elseif (t .eq. 13) then
            call outlin
         else
            call warn('Illegal syntax in -break- statement.')
         endif
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sdo
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer labgen,gnbtok,toktoi
      external labgen,gnbtok,toktoi
      integer t, n

c expand a do statement -- statement might have a label
      doindent(level+1) = indpc

c peek ahead
      t = gnbtok(token,ltoken)
      if (t .eq. 3) then
cOld style, user supplied label for the end
c get past statement field
         call outtab(0,0)
c XXX - put out 'do '
         call putqs('do ')
         level = level + 1
         dotype(level) = 1
         labb(level) = labgen(1)
         labn(level) = toktoi(token,ltoken)
         useb(level) = 0
         call putchr(token,ltoken)
         call eatup(0)
      elseif (t .eq. 13 .or. t .eq. 1) then
         if (t .eq. 1) call endcomment(1, 0, token, ltoken, 0)
         call outifl
         n = labgen(1)
ctarget for repeat, until
         call outcnt(n,0,0)
         level = level + 1
         dotype(level) = 3
         labn(level) = n
         labb(level) = labgen(1)
         useb(level) = 0
c new style do var=... or do for var=...
      else
         if ( t .eq. 127 ) then
            if (token(2) .eq. 90) then
               t=gnbtok(token,ltoken)
            elseif (token(2) .eq. 78) then
               call swhile
               return
            else
               call warn(' Syntax error in do statement.')
               return
            endif
         endif
         call outtab(0,0)
cput out 'do '
         call putqs('do ')
         level = level + 1
         dotype(level) = 2
         labn(level) = labgen(1)
         labb(level) = labgen(1)
         useb(level) = 0
         call puti5(labn(level))
         code(1) = 32
         call putchr(code,1)
         call putchr(token,ltoken)
         call eatup(0)
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sdo90
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer labgen,gnbtok,toktoi
      external labgen,gnbtok,toktoi
      integer t

c expand a do statement -- statement might have a label
      doindent(level+1) = indpc
c peek ahead
      t = gnbtok(token,ltoken)
      if (t .eq. 3) then
cOld style, user supplied label for the end
c get past statement field
         call outtab(0,0)
cput out 'do '
         call putqs('do ')
         level = level + 1
         dotype(level) = 1
         labb(level) = labgen(1)
         labn(level) = toktoi(token,ltoken)
         useb(level) = 0
         usen(level) = 0
         call putchr(token,ltoken)
         call eatup(0)
      elseif (t .eq. 13 .or. t .eq. 1) then
         call outtab(0,0)
cput out 'do '
         call putqs('do')
         if (t .eq. 1) then
            call endcomment(1, 0, token, ltoken, 0)
         else
            call outlin
         endif
         level = level + 1
         dotype(level) = 6
         labn(level) = labgen(1)
         labb(level) = labgen(1)
         usen(level) = 0
         useb(level) = 0
c new style do var=... or do for var=...
      else
         if (t .eq. 127 ) then
            if (token(2) .eq. 90) then
               t=gnbtok(token,ltoken)
            elseif (token(2) .eq. 103) then
               call swhile90
               return
            else
               call warn(' Syntax error in do statement.')
               return
            endif
         endif
         call outtab(0,0)
cput out 'do '
         call putqs('do ')
         level = level + 1
         dotype(level) = 5
         labn(level) = labgen(1)
         labb(level) = labgen(1)
         usen(level) = 0
         useb(level) = 0
         call putchr(token,ltoken)
         call eatup(0)
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sedo
cProlog

c process enddo or labeled target of traditional do


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer gnbtok,lnb
      external gnbtok,lnb
      integer it,nlev,lab,i,nforiter,oldpc

      if (level .eq. 0) then
         call warn('Unmatched end of do block.')
         return
      endif
      oldpc = doindent(level)

      it = dotype(level)
      if (it .eq. 2) then
         call outifl
         level = level - 1
         call outcnt(labn(level+1),0,0)
         if (useb(level+1) .eq. 1) call outcnt(labb(level+1),0,0)
         call spit(1)
      elseif (it .eq. 1) then
c dofile has just processed the labeled statement on a traditional do
c put out the continue statements for breaks
c have to watch out for the case of multiple old-style dos with the
c SAME target  i.e. do 200 i= 1,100 ; do 200 j=1,100 ; .... 200 continue
csave level for later use
         nlev = level
         lab = labn(level)
         level = level - 1
c get the new level of indentation
c while(labn(level) == lab)
23000    if (labn(level) .eq. lab) then
cthis is sure to stop (labn(0)=0)
            level = level - 1
            go to 23000
         endif
c endwhile
         do 23002 i=nlev,level+1,-1
            if (useb(i) .eq. 1) call outcnt(labb(i),doindent(i),0)
23002    continue

      elseif (it .eq. 3) then
         call outifl
         call putqs('c -- repeat')
         call outlin
         level = level - 1
         call outtab(oldpc,0)
         call outgo(labn(level+1))
         call spit(2)
         if (useb(level+1) .eq. 1) call outcnt(labb(level+1),oldpc,0)

      elseif (it .eq. 4) then
         call sewhile

      elseif (it .eq. 5) then
         if (usen(level) .eq. 1) call outcnt(labn(level),oldpc,1)
         level = level - 1
         call outtab(oldpc,0)
         call putqs('enddo')
         call spit(2)
         if (useb(level+1) .eq. 1) call outcnt(labb(level+1),oldpc,0)

      elseif (it .eq. 6) then
         if (usen(level) .eq. 1) call outcnt(labn(level),0,1)
         level = level - 1
         call outtab(oldpc,0)
         call putqs('enddo')
         call spit(2)
         if (useb(level+1) .eq. 1) call outcnt(labb(level+1),oldpc,0)

      elseif (it .eq. 8) then
         if (usen(level) .eq. 1) call outcnt(labn(level),0,1)
c Add the for loop increment
         call outtab(0,1)
         nforiter = lnb(foriter(level))
         call putqs(foriter(level)(:nforiter))
         call outlin
         level = level - 1
         call outtab(0,0)
         call putqs('enddo')
         call spit(2)
         if (useb(level+1) .eq. 1) call outcnt(labb(level+1),0,0)

      elseif (it .eq. 7) then
         call sewhile90

      elseif (it .eq. 9) then
         level = level - 1
         call outtab(oldpc,0)
         call putqs('enddo')
         call eatup(0)
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sefor
cProlog

      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      call sedo
      call putqs(cchar//' endfor')
      call outlin

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine selif
cProlog
c process elseif statement
c call docond to output condition & translate logical operators
c elseif (condition) must be followed by then; we supply even if missing

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer stoken(20000), sltoken, i
      integer t,gtok,gnbtok,gnbtokw,docond,wswid
      external gtok,gnbtok,gnbtokw,docond

      if (iflevel .eq. 0) then
         call warn('Elseif not inside if block.')
         return
      endif

      iflevel = iflevel - 1
      call outtab(0,0)
c XXX add space between 'else if'
      call putqs('elseif')
      if (docond() .ne. 0) return
      iflevel = iflevel + 1

c 1) if () # then
c 2) if () #           --> if () then #
c 3) if () \n          --> if () then \n
c 4) if () then
c 5) if () \r
c 6) if () \r then

      t = gnbtokw(token,ltoken,wswid)
      if (t .eq. 1) then
c save comment, look at next token
         do 23000 i=1,ltoken
            stoken(i) = token(i)
23000    continue
         sltoken = ltoken
         t = gnbtokw(token, ltoken, wswid)
         if (t .eq. 10) then
            t = gnbtokw(token, ltoken, wswid)
         endif
         if (t .eq. 127 .and. token(2) .eq. 85) then
            call endcomment(1, 1, stoken, sltoken, wswid)
            call outtab(0,0)
            call putqs(' then')
            call spit(2)
         else
            call pbstr(token,ltoken)
            call putqs(' then')
            call endcomment(2, 0, stoken, sltoken, wswid)
         endif

      elseif (t .eq. 10) then
         t = gnbtokw(token, ltoken, wswid)
         if (t .eq. 127 .and. token(2) .eq. 85) then
            call putqs(' then')
            call spit(2)
         else
            call pbstr(token,ltoken)
            call putqs(' then')
         endif
      elseif (t .eq. 13) then
         call putqs(' then')
         call outlin
      elseif (t .eq. 127 .and. token(2).eq.85) then
         call putqs(' then')
         call spit(2)
      else
         call warn('Syntax error in elseif statement.')
         call spit(2)
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine selse
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer gtok,wswid
      external gtok
      integer t

      if (iflevel .eq. 0) then
         call warn('Unmatched else.')
         return
      endif

      wswid = 0
      t = gtok(token,ltoken)
      if (t .eq. 5) then
         wswid = ltoken
         t = gtok(token,ltoken)
      endif

c check for 'else if' which is treated as elseif
      if (t .eq. 127 ) then
         if (token(2) .eq. 81) then
            call selif
            return
         endif
      endif

      iflevel = iflevel - 1
      call outtab(0,0)
      call putqs('else')

      if (t .eq. 1) then
         call endcomment(1, 0, token, ltoken, wswid)
      elseif (t .eq. 13) then
         call outlin
      else
         call outlin
         call warn('Syntax error in else statement')
      endif

      iflevel = iflevel + 1

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sendif
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      if (iflevel .eq. 0) then
         call warn('Unmatched endif.')
         return
      endif

      iflevel = iflevel - 1
      call outtab(0,0)
      call putqs('endif')
      call spit(2)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine setopt(optstr)
cProlog

c processes all the options

      character*(*) optstr
      integer c1,i, n, ind1
      integer i23000, i23005, i23023, i23051, i23059
      integer evalone,ierr
      external evalone


      logical ireset,rreset
      common /irset/ ireset,rreset


      character dirsep
      character*(256) incldirs(200)
      integer inclnum
      common /idircom/ inclnum
      common /idircoc/ dirsep, incldirs


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer icompile
      common /icom/icompile


      integer lexwarn,realsize,intsize,wordsize
      integer inlev,finerr,nodblquo,yyline(5)
      common /ctlvars/ lexwarn,realsize,intsize,wordsize
      common /ctlvars/ inlev,finerr,nodblquo,yyline


      character*(256) std,basis,sys,system
      common /fname/ std,basis,sys,system


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


c is passed the option string with the leading - removed

      if (optstr .eq. "-nolang") then
         optlang = 1
         optpretty = 0
      elseif (optstr .eq. "-langf77") then
         optlang = 2
         cchar = 'c'
      elseif (optstr .eq. "-langf90") then
         optlang = 3
      elseif (optstr .eq. "-isf77") then
         optlang = 4
      elseif (optstr .eq. "-isf90") then
         optlang = 5
      elseif (optstr .eq. "-macro") then
         optmacro = 1
      elseif (optstr .eq. "-nomacro") then
         optmacro = 0
      elseif (optstr .eq. "-nonumeric") then
         optnum = 0
         basis = " "
      elseif (optstr .eq. "-pretty") then
         optpretty = 1
      elseif (optstr .eq. "-nopretty") then
         optpretty = 0
      elseif (optstr .eq. "-relationalf77") then
         optrel = 2
      elseif (optstr .eq. "-relationalf90") then
         optrel = 3

      elseif (optstr .eq. "-honour-newlines") then
         hnl = 1
      elseif (optstr .eq. "-honor-newlines") then
         hnl = 1
      elseif (optstr .eq. "hnl") then
         hnl = 1

      elseif (optstr .eq. "-allocate") then
         optalloc = 1

      elseif (optstr(1:11) .eq. "-linelength") then
         col72 = evalone(optstr(12:),ierr)

      elseif (optstr(1:25) .eq. "-continuation-indentation") then
         indlev = evalone(optstr(26:),ierr)
      elseif (optstr(1:2) .eq. "ci") then
         indlev = evalone(optstr(3:),ierr)

      elseif (optstr(1:20) .eq. "-comment-indentation") then
         colcom = evalone(optstr(21:),ierr)
      elseif (optstr(1:4) .eq. "comi") then
         colcom = evalone(optstr(5:),ierr)

      else
         c1 = ichar(optstr(1:1))
c select
         i23000= c1
            go to 23000
c -- case 48- 57
23002    continue

            n = c1 - 48
            do 23003 i=2,len(optstr)
               c1 = ichar(optstr(i:i))
c select
               i23005= c1
                  go to 23005
c -- case 48- 57
23007          continue

                  n = n*10+c1-48
23008             go to 23006
c -- case 32
23009          continue
                  go to 23004
23010             go to 23006
c -- case (default)
23011          continue
                  go to 900
c -- dispatch area for select
23012          go to 23006
23005          continue
               if ( i23005 .eq. 32) go to 23009
               if ( i23005 .ge. 48 .and. i23005 .le. 57) go to 23007
               go to 23011
23006          continue
c endselect
23003       continue
23004       continue
            call setorg(n)
23013       go to 23001
c -- case 119
23014    continue
c w
            lexwarn = 1
23015       go to 23001
c -- case 98
23016    continue
cb
            bckeep = .false.
23017       go to 23001
c -- case 99
23018    continue
cc
            do 23019 i=0,127
               col1(i) = .false.
23019       continue
            do 23021 i=2,len(optstr)
               c1 = ichar(optstr(i:i))
c select
               i23023=c1
                  go to 23023
c -- case 32
23025          continue

                  go to 23022
23026             go to 23024
c -- case (default)
23027          continue

                  col1(c1) = .true.
c -- dispatch area for select
23028          go to 23024
23023          continue
               if ( i23023 .eq. 32) go to 23025
               go to 23027
23024          continue
c endselect
23021       continue
23022       continue
23029       go to 23001
c -- case 67
23030    continue
cC
            call cnstal('COMPILER',optstr(2:))
23031       go to 23001
c -- case 100
23032    continue
c d
            nodblquo = 1
23033       go to 23001
c -- case 68
23034    continue
c D
            ind1 = index(optstr, '=')
            if (ind1 .eq. 0) then
               ind1 = index(optstr,' ')
               call cnstal(optstr(2:ind1-1),' ')
            elseif (len(optstr) .eq. ind1) then
               call cnstal(optstr(2:ind1-1), ' ')
            else
               call cnstal(optstr(2:ind1-1), optstr(ind1+1:))
            endif
23035       go to 23001
c -- case 102
23036    continue
cf
c free form input: no col1 convention, no col6 convention
            do 23037 i=0,127
               col1(i) = .false.
23037       continue
            col6 = .false.
23039       go to 23001
c -- case 73
23040    continue
cI
            inclnum = inclnum + 1
            incldirs(inclnum) = optstr(2:)
23041       go to 23001
c -- case 108
23042    continue
cl
            if (optstr(2:) .eq. " ") then
               col72 = 80
            else
               col72 = evalone(optstr(2:),ierr)
            endif
23043       go to 23001
c -- case 109
23044    continue
cm
            basis=" "
23045       go to 23001
c -- case 77
23046    continue
cM
            call cnstal('MACHINE',optstr(2:))
23047       go to 23001
c -- case 116
23048    continue
ct
            system=optstr(2:)
23049       go to 23001
c -- case 105
23050    continue
ci
            intsize = evalone(optstr(2:),ierr)
c select
            i23051= intsize
               go to 23051
c -- case 2
c -- case 4
c -- case 8
23053       continue
               ireset = .true.
23054          go to 23052
c -- case (default)
23055       continue
               goto 900
c -- dispatch area for select
23056       go to 23052
23051       continue
            i23051=i23051-1
            if (i23051.lt. 1 .or. i23051.gt.7) go to 23055
            go to (23053,23055,23053,23055,23055,23055,23053), i23051
23052       continue
c endselect
23057       go to 23001
c -- case 114
23058    continue
cr
            realsize = evalone(optstr(2:),ierr)
c select
            i23059= realsize
               go to 23059
c -- case 4
c -- case 8
c -- case 16
23061       continue
               rreset = .true.
23062          go to 23060
c -- case (default)
23063       continue
               goto 900
c -- dispatch area for select
23064       go to 23060
23059       continue
            if ( i23059 .eq. 4) go to 23061
            if ( i23059 .eq. 8) go to 23061
            if ( i23059 .eq. 16) go to 23061
            go to 23063
23060       continue
c endselect
23065       go to 23001
c -- case 117
23066    continue
cu
            call notsen
23067       go to 23001
c -- case 118
23068    continue
cv
            verbose=.not.verbose
23069       go to 23001
c -- case (default)
23070    continue

            go to 900
c -- dispatch area for select
23071    go to 23001
23000    continue
         if ( i23000 .ge. 48 .and. i23000 .le. 57) go to 23002
         if ( i23000 .eq. 67) go to 23030
         if ( i23000 .eq. 68) go to 23034
         if ( i23000 .eq. 73) go to 23040
         if ( i23000 .eq. 77) go to 23046
         if ( i23000 .eq. 98) go to 23016
         if ( i23000 .eq. 99) go to 23018
         if ( i23000 .eq. 100) go to 23032
         if ( i23000 .eq. 102) go to 23036
         if ( i23000 .eq. 105) go to 23050
         if ( i23000 .eq. 108) go to 23042
         if ( i23000 .eq. 109) go to 23044
         if ( i23000 .eq. 114) go to 23058
         if ( i23000 .eq. 116) go to 23048
         if ( i23000 .eq. 117) go to 23066
         if ( i23000 .eq. 118) go to 23068
         if ( i23000 .eq. 119) go to 23014
         go to 23070
23001    continue
c endselect
      endif
      return
c bad options
  900 continue
      write(stderr,100) optstr
  100 format(' Error in option....'/a)
      call finish(1)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sewhile
cProlog
c do endwhile statement
c put out go to L,endif

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer oldpc
      oldpc = doindent(level)
      call outifl
      call outtab(oldpc,0)
      call outgo(labn(level))
      call outlin
      level = level - 1
      call outtab(0,0)
      call putqs('endif')
      call outlin
      call putqs('c endwhile')
      call spit(2)
      if (useb(level+1) .eq. 1) call outcnt(labb(level+1),0,0)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c do endwhile statement

      subroutine sewhile90
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer oldpc
      oldpc = doindent(level)
      if (usen(level) .eq. 1) call outcnt(labn(level),oldpc,1)
      level = level - 1
      call outtab(oldpc,0)
      call putqs('enddo')
      call spit(2)
      if (useb(level+1) .eq. 1) call outcnt(labb(level+1),oldpc,0)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c process if statement
c call docond to output condition & translate logical operators
c if (condition) may be followed by
c a. then -- put out as is
c b. some other statement
c     We check on the next line if nothing follow the cond.
c Note that return(value) has to be handled correctly.

      subroutine sif
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins

      integer t,gnbtok,gtok,docond,wswid
      external gnbtok,gtok,docond

      call outtab(0,0)
      call putqs('if')
      if (docond() .ne. 0) return

      wswid = 0
      t = 0
23000 continue
         if (t .eq. 0) t = gtok(token,ltoken)
         if (t .eq. 1) then
            call endcomment(1, 1, token, ltoken, wswid)
            wswid = 0
            t = 0
            if (icontinue .eq. 1) then
c --langf90
               if (optpretty .eq. 1) then
                  call outtab(0,0)
                  t = gnbtok(token,ltoken)
                  icontinue = 0
               else
                  icontinue = 0
                  t = gtok(token,ltoken)
                  if (t .eq. 5) call outtab(ltoken,0)
               endif
            endif
            go to 23000
         elseif (t .eq. 10) then
            wswid = 0
            if (nonblk .eq. 1) call conlin
         elseif (t .eq. 13) then
            wswid = 0
            if (hnl .eq. 1) call conlin
         elseif (t .eq. 5) then
            wswid = ltoken
         else
            go to 23001
         endif
         t = 0
c -- repeat
      go to 23000
23001 continue
      call outfil(wswid)

      iflevel = iflevel + 1
      icontinue = 0
      if (t .ne. 127 ) then
         call outtab(0,0)
         call putchr(token,ltoken)
         call eatup(0)
         iflevel = iflevel - 1
         return
      else
         t = token(2)
      endif
      if (t .eq. 85) then
         call putqs('then')
         call spit(2)
      else
         if (t .eq. 97) then
            if (modfun .eq. 1) then
               call doret(.true.)
            else
               call doret(.false.)
            endif
         elseif (t .eq. 75) then
            call sbrks
         elseif (t .eq. 76) then
            call snext
         elseif (t .eq. 100) then
            call sbrks90
         elseif (t .eq. 101) then
            call snext90
c?? something wrong
         else
            call putchr(token,ltoken)
            call eatup(0)
         endif
         iflevel = iflevel - 1
      endif
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine snext
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer t,gnbtok,toktoi
      external gnbtok,toktoi
      integer nlev

c look for digit after the next
      t = gnbtok(token,ltoken)
      if ( t .eq. 3) then
         nlev = toktoi(token,ltoken) - 1
         t = gnbtok(token,ltoken)
      else
         nlev = 0
      endif

      if (level-nlev .le. 0) then
         call warn('Illegal -next- statement, level wrong')
      else
         call outtab(0,0)
         call outgo(labn(level-nlev))
c shouldn't be anything else here
         if (t .eq. 1) then
            call endcomment(1, 0, token, ltoken, 0)
         elseif (t .eq. 13) then
            call outlin
         else
            call warn('Illegal syntax in -next- statement.')
         endif
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine snext90
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer t,gtok,toktoi,wswid
      external gtok,toktoi
      integer nlev

      wswid = 0
c look for digit after the next
      t = gtok(token,ltoken)
      if (t .eq. 5) then
         wswid = ltoken
         t = gtok(token,ltoken)
      endif

      if ( t .eq. 3) then
         nlev = toktoi(token,ltoken) - 1
         t = gtok(token,ltoken)
      else
         nlev = 0
      endif

      if (level-nlev .le. 0) then
         call warn('Illegal -next- statement, level wrong')
      else
         call outtab(0,0)
c next and break beyond the inner loop requires labels
c as well as next from a for loop which must do the increment step.
         if (nlev .eq. 0 .and. dotype(level) .ne. 8) then
            call putqs('cycle')
         else
            usen(level-nlev) = 1
            call outgo(labn(level-nlev))
         endif

         if (t .eq. 5) then
            wswid = ltoken
            t = gtok(token,ltoken)
         endif

c shouldn't be anything else here
         if (t .eq. 1) then
            call endcomment(1, 0, token, ltoken, wswid)
         elseif (t .eq. 13) then
            call outlin
         else
            call warn('Illegal syntax in -next- statement.')
         endif
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c delete to end of line, inclusive
c but preserve comments

      subroutine spit(ipos)
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins
c lastpc
      integer ipos, wswid, t
      integer gtok
      external gtok

      wswid = 0
23000 continue
         t = gtok(token,ltoken)
         if (t .eq. 1) then
            call endcomment(ipos, 0, token, ltoken, wswid)
            go to 23001
         elseif (t .eq. 5) then
            wswid = ltoken
         elseif (t .eq. 13) then
            if (lastpc .ne. 0) call outlin
            go to 23001
         endif
c -- repeat
      go to 23000
23001 continue
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine suntil
cProlog
c end block do with until(condition)
c translates to: if (.not.(condition)) go to L ; enddo

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer docond, oldpc
      external docond
      if (level .eq. 0) then
         call warn('Extraneous until statement.')
         return
      elseif (dotype(level) .ne. 3) then
         call warn('Until statement in wrong kind of do loop.')
         return
      endif
      oldpc = doindent(level)
      call outifl
      level = level - 1
      call outtab(oldpc,1)
      call putqs('if ( .not.')
      if (docond() .ne. 0) return
      code(1) = 32
      code(2) = 41
      call putchr(code,2)
      call outgo(labn(level+1))
      call spit(2)
      if (useb(level+1) .eq. 1) call outcnt(labb(level+1),oldpc,0)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c end block do with until(condition)
c translates to: if (condition) exit; enddo

      subroutine suntil90
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer oldpc
      integer docond
      external docond

      if (level .eq. 0) then
         call warn('Extraneous until statement.')
         return
      elseif (dotype(level) .ne. 6) then
         call warn('Until statement in wrong kind of do loop.')
         return
      endif

      oldpc = doindent(level)

      if (usen(level) .eq. 1) call outcnt(labn(level),oldpc,1)
      call outtab(0,1)
      call putqs('if')
      if (docond() .ne. 0) return
      call putqs('exit')
      call spit(2)
      level = level - 1
      call outtab(0,0)
      call putqs('enddo')
      call outlin
      if (useb(level+1) .eq. 1) call outcnt(labb(level+1),oldpc,0)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine swhile
cProlog
c translate while(cond) to
c       continue   (in case this statement was labeled)
c L     if (cond) then
c

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer docond,n, labgen
      external docond, labgen
c
      call outifl
      n = labgen(2)
      call puti5(n)
      call outtab(0,0)
      call putqs('if')
      if (docond() .ne. 0) return
      call putqs(' then')
      level = level + 1
      labn(level) = n
      labb(level) = n + 1
      useb(level) = 0
      dotype(level) = 4
      call spit(2)
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c translate while(cond) to
c       do while (cond)


      subroutine swhile90
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer n
      integer docond, labgen, getdef
      external docond, labgen, getdef

      doindent(level+1) = indpc
      n = labgen(2)
      call outtab(0,0)
      call putqs('do while')
      if (docond() .ne. 0) return
      call spit(1)
      level = level + 1
      labn(level) = n
      labb(level) = n + 1
      usen(level) = 0
      useb(level) = 0
      foriter(level) = " "
      if (getdef('mppl_foriter', foriter(level)) .ge. 0) then
c do not reuse macro with next loop
         call undef('mppl_foriter')
         dotype(level) = 8
      else
         dotype(level) = 7
      endif
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      integer function toktoi(token,toksiz)
cProlog
c convert a token to an integer
      integer token(1),toksiz,i,j,k,isgn
      toktoi = 0
      isgn = 1
      do 23000 j=1,toksiz
         if (token(j) .ne. 32) go to 23001
23000 continue
23001 continue
      if (token(j) .eq. 45) then
         isgn = -1
         k=j + 1
         do 23002 j=k,toksiz
            if (token(j) .ne. 32) go to 23003
23002    continue
23003    continue
      endif
      do 23004 i=j,toksiz
         toktoi = toktoi*10 + token(i) - 48
23004 continue
      toktoi = toktoi*isgn
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine sselect
cProlog
c sselect - generate code for beginning of select statement
      integer lab, t
      integer labgen, gnbtok

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      lab = labgen(2)
      if (sellast + 3 .gt. 500) call oops('select table overflow.')
      call outifl
      call putqs('c select')
      call outlin
c Innn=(e)
      call outtab(0,0)
      call selvar(lab)
      t = gnbtok(token,ltoken)
      if (t .eq. 13) then
         call warn('select statement syntax error.')
         return
      endif
      if (t .ne. 61) then
         code(1) = 61
         call putchr(code,1)
      endif
      call putchr(token,ltoken)
      call eatup(0)
      sellev = sellev + 1
      if (sellev .gt. 10) call oops(
     &      'select statements nested too deeply.')
      sellab(sellev) = lab
c the triplets in selstak represent lower, upper bounds + label
c in the FIRST one however, at seltop,
c selstak(seltop) = previous seltop
c selstak(seltop+1) = number of cases in this select so far
c selstak(seltop+2) = label for default case, 0 if no default stat yet.
      selstak(sellast) = seltop
      selstak(sellast+1) = 0
      selstak(sellast+2) = 0
      seltop = sellast
      sellast = sellast + 3
      call outtab(0,0)
c goto L
      call outgo(lab)
      call outlin
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c sselect - generate code for beginning of select statement

      subroutine sselect90
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer t
      integer gnbtok
      external gnbtok

      if (sellast + 3 .gt. 500) call oops('select table overflow.')
c XXX - ?
c Innn=(e)
      call outtab(0,0)
      t = gnbtok(token,ltoken)
      if (t .eq. 13) then
         call warn('select statement syntax error.')
         return
      endif
      call putqs('select case (')
      call putchr(token,ltoken)
      code(1) = 41
      call eatup(1)
      sellev = sellev + 1
      if (sellev .gt. 10) call oops(
     &      'select statements nested too deeply.')
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine scase(td)
cProlog
c case or default statement
c td is SCASE or SDEFAULT
      integer td
      integer t, l, lb, ub, i, j, lab, junk
      integer caslab, labgen, gnbtok

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      if (sellev .le. 0) then
         call warn('illegal case or default.')
         return
      endif
      call outifl
      lab = sellab(sellev)

      if (selstak(seltop+1) .gt. 0) then
c in case the previous statement was a break or other goto, label this one
         i = labgen(1)
         call puti5(i)
         call outtab(0,0)
c terminate previous case
         call outgo(lab+1)
         call outlin
      endif

      l = labgen(1)
      if (td .eq. 87) then
c case n[,n]... : ...
c while(caslab(lb, t) <> 13)
23000    if (caslab(lb, t) .ne. 13) then
            ub = lb
            if (t .eq. 45) junk = caslab(ub, t)
            if (lb .gt. ub) then
               call warn('illegal range in case label.')
               return
            endif

            if (sellast + 3 .gt. 500) call oops('select table overflow.'
     &            )

c -- for([i = seltop + 3, i < sellast, i = i + 3])
            i = seltop + 3
            go to 23002
23003       continue
               i = i + 3
23002          if (.not.( i .lt. sellast)) go to 23004
               if (lb .le. selstak(i)) then
                  go to 23004
               elseif (lb .le. selstak(i+1)) then
                  call warn('duplicate case label.')
                  return
               endif
c -- repeat
            go to 23003
23004       continue
c endfor

            if (i .lt. sellast .and. ub .ge. selstak(i)) then
               call warn('duplicate case label.')
               return
            endif

c insert new entry
c -- for([j = sellast, j > i, j = j - 1])
            j = sellast
            go to 23005
23006       continue
               j = j - 1
23005          if (.not.( j .gt. i)) go to 23007
               selstak(j+2) = selstak(j-1)
c -- repeat
            go to 23006
23007       continue
c endfor

            selstak(i) = lb
            selstak(i+1) = ub
            selstak(i+2) = l
            selstak(seltop+1) = selstak(seltop+1) + 1
            sellast = sellast + 3
            call putqs('c -- case ')
            call outnum(lb)
            if (ub .gt. lb) then
               call putqs('- ')
               call outnum(ub)
            endif

            call outlin

            if (t .eq. 58) then
               go to 23001
            elseif (t .ne. 44) then
               call warn('illegal case syntax.')
               return
            endif
            go to 23000
         endif
c endwhile
23001    continue

c default : ...
      else
         call putqs('c -- case (default)')
         call outlin
         t = gnbtok(token, ltoken)
         if (selstak(seltop+2) .ne. 0) then
            call warn('multiple defaults in select statement.')
            return
         endif
         selstak(seltop+2) = l
      endif

      if (t .ne. 58) call inform(
     &      'missing colon in case or default label.')

      sellev = sellev - 1
      call outcnt(l,0,0)
      sellev = sellev + 1

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c caslab - get one case label
      function caslab(n, t)
cProlog
      integer caslab
      integer n, t
      integer s
      integer gnbtok, toktoi

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      t = gnbtok(token, ltoken)
      if (t .eq. 13) then
         caslab = t
         return
      endif
      if (t .eq. 45) then
         s = -1
      else
         s = +1
      endif
      if (t .eq. 45 .or. t .eq. 43) t = gnbtok(token, ltoken)
      if (t .ne. 3) then
         call inform('invalid case label.')
         t = 58
         n = 9999
         caslab = 0
         return
      endif
      n = s*toktoi(token,ltoken)
c get next token (should be comma or colon)
      t = gnbtok(token, ltoken)
      caslab = 0
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c case or default statement
c td is SCASE or SDEFAULT

      subroutine scase90(td)
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins

      integer td
      integer t, junk, wswid
      integer caslab90, gnbtok, gtok
      external gnbtok, gtok

      if (sellev .le. 0) then
         call warn('illegal case or default.')
         return
      endif
      if (td .eq. 107) then
c case n[,n]... : ...
         sellev = sellev - 1
         call outtab(0,0)
         sellev = sellev + 1
         call putqs('case (')
c while(caslab90(t) <> 13)
23000    if (caslab90(t) .ne. 13) then
            if (t .eq. 45) then
               call putqs(':')
               junk = caslab90(t)
            endif
            if (t .eq. 58) then
               go to 23001
            elseif (t .ne. 44) then
               call warn('illegal case syntax.')
               return
            endif
            call putqs(',')
            go to 23000
         endif
c endwhile
23001    continue
         call putqs(')')
c default : ...
      else
         sellev = sellev - 1
         call outtab(0,0)
         sellev = sellev + 1
         call putqs('case default')
         t = gnbtok(token, ltoken)
      endif

      if (t .ne. 58) then
         call inform('missing colon in case or default label.')
      endif

c The statment may be on the same line or the next
      t = gtok(token, ltoken)
      if (t .eq. 5) then
         wswid = ltoken
         t = gtok(token, ltoken)
      else
         wswid = 0
      endif
      if (t .eq. 1) then
         call endcomment(2, 0, token, ltoken, wswid)
      else
         if (t .ne. 13) then
            call pbstr(token,ltoken)
c Any statement immediately following will be indented a line
            indflg = -1
         endif
         call outlin
      endif
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c caslab - get one case label

      function caslab90(t)
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      integer caslab90
      integer t
      integer s
      integer gnbtok

      t = gnbtok(token, ltoken)
      if (t .eq. 13) then
         caslab90 = t
         return
      endif
      if (t .eq. 45) then
         s = -1
      else
         s = +1
      endif
      if (t .eq. 45 .or. t .eq. 43) t = gnbtok(token, ltoken)
      if (t .ne. 3 .and. t .ne. 4) then
         call inform('invalid case label.')
         t = 58
         caslab90 = 0
         return
      endif
      call putchr(token,ltoken)
c get next token (should be comma or colon)
      t = gnbtok(token, ltoken)
      caslab90 = 0
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine seselect
cProlog
c seselect - finish off select statement; generate dispatch code
      integer lab
      integer lb, ub, n, i, j
      integer labgen
      external labgen

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)

      if (sellev .le.0) then
         call warn('Unmatched endselect.')
         return
      endif
      lab = sellab(sellev)
      lb = selstak(seltop+3)
      ub = selstak(sellast-2)
      n = selstak(seltop+1)
      sellev = sellev - 1
      call outifl
      call putqs('c -- dispatch area for select')
      call outlin
      if ( n .gt. 0 ) then
c in case the previous statement was a break or other goto, label this one
         i = labgen(1)
         call puti5(i)
         call outtab(0,0)
c terminate last case
         call outgo(lab+1)
         call outlin
      endif
c default default label
      if (selstak(seltop+2) .eq. 0) selstak(seltop+2) = lab + 1
c L   continue
      call outcnt(lab,0,0)
      if (n .ge. 3 .and. ub - lb + 1 .lt. 3 * n ) then
c output branch table
         if (lb .ne. 1) then
c L  Innn=Innn-lb+1
            call outtab(0,0)
            call selvar(lab)
            code(1) = 61
            call putchr(code,1)
            call selvar(lab)
            if (lb .lt. 1) then
               code(1) = 43
               call putchr(code,1)
            endif
            call outnum(-lb + 1)
            call outlin
         endif
c  if (Innn.lt.1.or.Innn.gt.ub-lb+1)goto default
         call outtab(0,0)
         call putqs('if (')
         call selvar(lab)
         call putqs('.lt. 1 .or. ')
         call selvar(lab)
         call putqs('.gt.')
         call outnum(ub - lb + 1)
         call putqs(') go to ')
         call puti5(selstak(seltop+2))
         call outlin

c go to (....), Innnn
         call outtab(0,0)
         call putqs('go to (')

         j = lb
c -- for([i = seltop + 3, i < sellast, i = i + 3])
         i = seltop + 3
         go to 23000
23001    continue
            i = i + 3
23000       if (.not.( i .lt. sellast)) go to 23002
c -- for([ , j < selstak(i), j = j + 1])

            go to 23003
23004       continue
               j = j + 1
c fill in vacancies
23003          if (.not.( j .lt. selstak(i))) go to 23005
               call outnum(selstak(seltop+2))
               code(1) = 44
               call putchr(code,1)
c -- repeat
            go to 23004
23005       continue
c endfor

c -- for([j = selstak(i+1) - selstak(i), j >= 0, j = j - 1])
            j = selstak(i+1) - selstak(i)
            go to 23006
23007       continue
               j = j - 1
23006          if (.not.( j .ge. 0)) go to 23008
c fill in range
               call outnum(selstak(i+2))
               if ((i .lt. sellast - 3) .or. (j .gt. 0)) then
                  code(1) = 44
                  call putchr(code,1)
               endif
c -- repeat
            go to 23007
23008       continue
c endfor
            j = selstak(i+1) + 1
c -- repeat
         go to 23001
23002    continue
c endfor

         call putqs('), ')
         call selvar(lab)
         call outlin

      elseif (n .gt. 0) then
c output linear search form
c -- for([i = seltop + 3, i < sellast, i = i + 3])
         i = seltop + 3
         go to 23009
23010    continue
            i = i + 3
23009       if (.not.( i .lt. sellast)) go to 23011
c if (Innn
            call outtab(0,0)
            call putqs('if ( ')
            call selvar(lab)
            if (selstak(i) .eq. selstak(i+1)) then
c   .eq....
               call putqs(' .eq. ')
               call outnum(selstak(i))
            else
c   .ge.lb.and.Innn.le.ub
               call putqs(' .ge. ')
               call outnum(selstak(i))
               call putqs(' .and. ')
               call selvar(lab)
               call putqs(' .le. ')
               call outnum(selstak(i+1))
            endif
c ) go to ....
            call putqs(') ')
            call outgo(selstak(i+2))
            call outlin
c -- repeat
         go to 23010
23011    continue
c endfor

         if (lab + 1 .ne. selstak(seltop+2)) then
            call outtab(0,0)
            call outgo(selstak(seltop+2))
            call outlin
         endif
      endif

c L+1  continue
      call outcnt(lab+1,0,0)
      call putqs('c endselect')

c pop select stack
      sellast = seltop
      seltop = selstak(seltop)

      call spit(2)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c seselect90 -

      subroutine seselect90
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      if (sellev .le.0) then
         call warn('Unmatched endselect.')
         return
      endif

      sellev = sellev - 1
      call outtab(0,0)
      call putqs('end select')

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine selvar(lab)
cProlog
      integer lab

      call putqs('i')
      call outnum(lab)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c 'do' pretty print

      subroutine sdop
cProlog


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer t, wswid
      integer gnbtokw, toktoi
      external gnbtokw, toktoi

      doindent(level+1) = indpc

c peek ahead
      t = gnbtokw(token,ltoken,wswid)
      if (t .eq. 3) then
cOld style, user supplied label for the end
c get past statement field
         call outtab(0,0)
         call putqs('do')
         level = level + 1
         dotype(level) = 1
         labn(level) = toktoi(token,ltoken)
         useb(level) = 0
         call outfil(wswid)
         call putchr(token,ltoken)
         call eatup(0)
      else
         call outtab(0,0)
         call putqs('do')
         level = level + 1
         dotype(level) = 9
         if (t .eq. 1) then
            call endcomment(2, 0, token, ltoken, 0)
         elseif (t .eq. 13) then
            call outlin
         else
            call outfil(wswid)
            call putchr(token,ltoken)
            call eatup(0)
         endif
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c 'select' pretty print

      subroutine sselectp
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer t, wswid
      integer gnbtokw
      external gnbtokw

      if (sellast + 3 .gt. 500) call oops('select table overflow.')
      call outtab(0,0)
      call putqs('select')
      t = gnbtokw(token,ltoken,wswid)
      call outfil(wswid)
      if (t .eq. 127 .and. token(2) .eq. 113) then
         call putqs('case')
      else
         call putchr(token,ltoken)
      endif

      call eatup(0)
      sellev = sellev + 1
      if (sellev .gt. 10) call oops(
     &      'select statements nested too deeply.')
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c 'case' pretty print

      subroutine scasep
cProlog

      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      if (sellev .le. 0) then
         call warn('illegal case or default.')
         return
      endif
      sellev = sellev - 1
      call outtab(0,0)
      call putqs('case')
      call eatup(0)
      sellev = sellev + 1

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine outnum(n)
cProlog
      integer n,i
      character*16 msg
      common /onc/ msg

      write(msg,100) n
  100 format(i16)

c -- for([i = 1 , i<16 & msg(i:i) = ' ', i=i+1])
      i = 1
      go to 23000
23001 continue
         i=i+1
23000    if (.not.( i.lt.16 .and. msg(i:i) .eq. ' ')) go to 23002
c -- repeat
      go to 23001
23002 continue
c endfor

      call putqs(msg(i:16))

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c DOMATH - this subroutine evaluates an expression

      subroutine domath(argstk,i,j)
cProlog
      integer evalone
      external evalone
      integer argstk(1),i,j
      integer ival, ierr
      integer jrem,jq,a1,a2
      integer i0,pos,k
      character*(125) str


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      ierr = 0
      ival = 0
      pos = 1

c if there are too many arguments
      if (i+2 .ne.j) then
         call warn('Too many arguments to Evaluate macro')
         ierr=1

      else
         i0 = 48
         a1=argstk(i+2)
         a2=argstk(i+3)
         if (a2-a1 .gt. 125) then
            call warn('Too long an argument to Evaluate macro')
            ierr = 1
            return
         endif

c if the string is not null, create the character string
         if (a1.ne.a2) then
c -- for([k=a1,k<a2,k=k+1])
            k=a1
            go to 23000
23001       continue
               k=k+1
23000          if (.not.(k.lt.a2)) go to 23002
               str(pos:pos) = char(evalst(k))
               pos=pos+1
c -- repeat
            go to 23001
23002       continue
c endfor
            str(pos: )=' '
c evaluate the string
            ival=evalone(str,ierr)
         endif
      endif

      if (ierr.eq.1) then
c an error occurred
         call pbstr(evalst(a1),a2-a1)

c put the anwswer back
      else
         jrem=abs(ival)
         if (jrem.eq.0) then
            call putbak(i0)
            return
         endif

c while(jrem>0)
23003    if (jrem.gt.0) then
            jq=jrem/10
            call putbak(i0+jrem-jq*10)
            jrem=jq
            go to 23003
         endif
c endwhile
         if (ival.lt.0) then
            call putbak(45)
         endif
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function evalone(str, ierr)
cProlog
c
c   This fuction returns the value of a vlaid infix integer
c   expression. If it cannot be evaluated, it returns 0
c   and ierr = ERR
c
c   extended by
c   Cathleen Benedetti
c   10-13-86
c
c   VARIABLES
c
c

      character*(8) act1
      integer act2, move, stray,stop, numray,numtop
      common /ceo/ act1(26)
      common /ceo1/ act2(26,8), move(26,4)
      common /ceo2/ stray(125), stop, numray(125), numtop


c function name
      integer evalone
c input string
      character*(*) str
      integer ierr, i23005, i23014
c delimits current token in str
      integer i,j,
c length of str
     &  lstr,
c type of current token
     &  tok,
c value of integer token
     &  term,
c value of expression
     &  total
c index to act1,act2
      integer index,
c current state,next state
     &  state,nstate,
c temporary variable
     &  t
c current action
      character*(1) action,
c next input
     &  nxtin
c
c functions
      integer evaltokn,stlength
c functions
      external evaltokn,stlength
c
c   ***********initialize**************
c
      total = 0
      ierr = 0
      action = 'e'
      stop = 0
      numtop = 0
      state = 2
      call evpush(state,stray,stop,ierr)
      lstr = stlength(str)

c -- for([i=1, (ierr = 0 & action <>'a'), i = j+1])
      i=1
      go to 23000
23001 continue
         i = j+1
23000    if (.not.( (ierr .eq. 0 .and. action .ne.'a'))) go to 23002
         tok = evaltokn(str,i,j,ierr,lstr)
         if (ierr .eq. 0) then
            call getin(tok,i,j,str,term,nxtin,index)
            action = 'e'
c while(ierr=0 & action .ne. 'a' & action .ne. 's')
23003       if (ierr.eq.0 .and. action .ne. 'a' .and. action .ne. 's')
     &          then
               state=stray(stop)
               action=act1(state)(index:index)
               nstate=act2(state,index)
c select
               i23005= ichar(action)
                  go to 23005
c -- case 101
23007          continue
c error
                  ierr = 1
23008             go to 23006
c -- case 97
23009          continue
caccept
                  call evpop(total, numray, numtop, ierr)
23010             go to 23006
c -- case 115
23011          continue
cshift
                  if (nxtin .eq. '#') then
                     call evpush (term, numray, numtop, ierr)
                  else
                     t = ichar(nxtin)
                     call evpush(t,numray,numtop,ierr)
                  endif
                  call evpush (nstate, stray, stop, ierr)
23012             go to 23006
c -- case 114
23013          continue
c reduce
                  call operate (ierr, nstate, stray, stop, numray, 
     &               numtop)
c select
                  i23014= nstate
                     go to 23014
c -- case 2
c -- case 3
c -- case 4
c -- case 5
c -- case 6
c -- case 7
23016             continue
                     state = move(stray(stop),1)
23017                go to 23015
c -- case 8
c -- case 9
c -- case 10
23018             continue
                     state = move(stray(stop),2)
23019                go to 23015
c -- case 11
c -- case 12
c -- case 13
23020             continue
                     state = move(stray(stop),3)
23021                go to 23015
c -- case 14
c -- case 15
23022             continue
                     state = move(stray(stop),4)
23023                go to 23015
c -- case (default)
23024             continue
                     call oops('evalsub1:bad input state')
c -- dispatch area for select
23025             go to 23015
23014             continue
                  i23014=i23014-1
                  if (i23014.lt. 1 .or. i23014.gt.14) go to 23024
                  go to (23016,23016,23016,23016,23016,23016,23018,23018
     &               ,23018,23020,23020,23020,23022,23022), i23014
23015             continue
c endselect
                  call evpush (state,stray, stop, ierr)
23026             go to 23006
c -- case (default)
23027          continue

                  call oops ('evalsub1: bad input action')
c -- dispatch area for select
23028          go to 23006
23005          continue
               if ( i23005 .eq. 97) go to 23009
               if ( i23005 .eq. 101) go to 23007
               if ( i23005 .eq. 114) go to 23013
               if ( i23005 .eq. 115) go to 23011
               go to 23027
23006          continue
c endselect
               go to 23003
            endif
c endwhile
         endif
c -- repeat
      go to 23001
23002 continue
c endfor

      evalone = total
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      function evaltokn(str, i, j, ierr,lstr)
cProlog
c
c  this function determines whether the current token
c  invalid, eof, an operator, an integer, or a variable
c
c  str      input string
c  i,j      position of token in strinteger evaltok
c  c1       integer value of current character
c
c
      integer evaltokn
      character*(*) str
      integer i,j,lstr, c1, ierr

      ierr = 0
c while((str(i:i)=' ') & (i<=lstr))
23000 if ((str(i:i).eq.' ') .and. (i.le.lstr)) then
         i=i+1
         go to 23000
      endif
c endwhile

      if (i.gt.lstr) then
c it's the end of the string
         evaltokn = 101
         return
      endif

c  (, ), *, +, -, /
      c1 = ichar(str(i:i))
      if (c1.eq.40.or.c1.eq.41.or.c1.eq.42.or.c1.eq.43.or.c1.eq.45.or.c1
     &   .eq.47) then
         j=i
c it's an operator
         evaltokn = 100
         return
      elseif (c1 .ge. 48 .and. c1 .le. 57) then
c integer
c -- for([j = i + 1, j <= lstr, j = j + 1])
         j = i + 1
         go to 23002
23003    continue
            j = j + 1
23002       if (.not.( j .le. lstr)) go to 23004
            c1 = ichar(str(j:j))
            if (c1 .lt. 48 .or. c1 .gt. 57) go to 23004
c -- repeat
         go to 23003
23004    continue
c endfor
         j = j -1
c it's an integer
         evaltokn = 102
         return

      else
         ierr = 1
c it's a bad character
         evaltokn = 101
         return
      endif

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c STLENGTH - this fucntion returns the length of str

      function stlength(str)
cProlog
      integer stlength,temp
      character*(*) str

      temp=len(str)

c while(str(temp:temp) = ' ')
23000 if (str(temp:temp) .eq. ' ') then
         temp=temp-1
         go to 23000
      endif
c endwhile

      stlength = temp
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine getin(tok,i,j,str,term,nxtin,index)
cProlog

c type of token
      integer tok,
c delimiters
     &  i,j,
c value of integer token
     &  term,
c index to arrays
     &  index,
c temporary variable
     &  t
c input string
      character*(*) str
c next input
      character*(1) nxtin
      integer i23000

c select
      i23000= tok
         go to 23000
c -- case 101
23002 continue

         index=7
         nxtin = '$'
23003    go to 23001
c -- case 100
23004 continue

         index=ichar(str(i:i))-39
         nxtin=str(i:i)
23005    go to 23001
c -- case 102
23006 continue

         index=5
         nxtin = '#'
         t=i
         term=0
c while(t<=j)
23007    if (t.le.j) then
            term=term*10+(ichar(str(t:t))-48)
            t=t+1
            go to 23007
         endif
c endwhile
23009    go to 23001
c -- case (default)
23010 continue
         call oops('getin:impossible token from evaltok')
c -- dispatch area for select
23011 go to 23001
23000 continue
      i23000=i23000-99
      if (i23000.lt. 1 .or. i23000.gt.3) go to 23010
      go to (23004,23002,23006), i23000
23001 continue
c endselect

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c OPERATE - this subroutine performs a reduction

      subroutine operate(ierr, p, stray, stop, numray, numtop)
cProlog
      integer stray(1), numray(1), stop, numtop, ierr, k, i23004, i23014
      integer first, second, temp, p, optr, value

c p corresponds to the production being used
      if (p.lt.2 .or. p.gt.15) then
         ierr = 1

      elseif (p.eq.2 .or. p.eq.8 .or. p.eq.11 .or. p.eq.14) then
         call evpop(temp, stray, stop, ierr)

      elseif (p.eq.15) then
         do 23000 k=1,3
            call evpop(temp, stray, stop, ierr)
23000    continue
         call evpop(temp, numray, numtop, ierr)
         call evpop(value, numray, numtop,ierr)
         call evpop(temp, numray, numtop, ierr)
         call evpush (value,numray,numtop,ierr)

      else
         do 23002 k=1,2
            call evpop(temp, stray, stop, ierr)
23002    continue
         call evpop(second, numray, numtop, ierr)
         call evpop(optr, numray, numtop, ierr)
         if (p.eq.3) then
            value = -second
         else
            call evpop(temp, stray, stop, ierr)
            call evpop(first, numray, numtop, ierr)
            if (p .ne. 5 .and. p .ne. 7) then
c select
               i23004= optr
                  go to 23004
c -- case 42
23006          continue
                  value = first * second
23007             go to 23005
c -- case 43
23008          continue
                  value = first + second
23009             go to 23005
c -- case 45
23010          continue
                  value = first - second
23011             go to 23005
c -- case 47
23012          continue
                  if (second .ne.0) then
                     value = first/second
                  else
                     call oops('evaluate macro--divide by zero')
                  endif
c -- dispatch area for select
23013          go to 23005
23004          continue
               i23004=i23004-41
               if (i23004.lt. 1 .or. i23004.gt.6) go to 23005
               go to (23006,23008,23005,23010,23005,23012), i23004
23005          continue
c endselect
            else
               call evpop(temp, numray, numtop, ierr)
               call evpop (temp, stray, stop, ierr)
c select
               i23014= optr
                  go to 23014
c -- case 45
23016          continue
                  value = -first - second
23017             go to 23015
c -- case 43
23018          continue
                  value = -first + second
c -- dispatch area for select
23019          go to 23015
23014          continue
               if ( i23014 .eq. 43) go to 23018
               if ( i23014 .eq. 45) go to 23016
23015          continue
c endselect
            endif
         endif

         call evpush (value, numray,numtop, ierr)
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c EVPOP - this function pops an integer off an integer stack

      subroutine evpop(item, stack, top, ierr)
cProlog
      integer item, stack, top, ierr
      dimension stack(1)

      item = 0
      if (top.gt.0) then
         item = stack(top)
         top = top - 1

      else
         ierr = 1
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c EVPUSH - this subroutine pushes an integer onto an integer stack

      subroutine evpush(item, stack, top, ierr)
cProlog
      integer item, stack, top, ierr
      dimension stack(1)

      if (top .lt. 125 ) then
         top = top + 1
         stack(top) = item
      else
         ierr = 1
      endif

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c DOUNDF - take definition out of table

      subroutine doundf(argstk,i,j)
cProlog
      integer a1,a2,argstk(1),i,j, ilo, ihi, lname, k


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      if (j .ne. i+2) call oops('wrong number of arguments in undefine')

      a1=argstk(i+2)
      a2=argstk(i+3)

      do 23000 ilo = a1, a2-1
         if (type(evalst(ilo)).ne.5) go to 23001
23000 continue
23001 continue

      do 23002 ihi = a2-1, a1, -1
         if (type(evalst(ihi)).ne.5) go to 23003
23002 continue
23003 continue
      lname = ihi -ilo + 1

      if (lname .le.0) call oops(' Empty name in Undefine macro.')

      do 23004 k = ilo, ihi
         if (alphan(evalst(k)).ne.1) call oops(
     &         ' Name not alphanumeric in Undefine macro.')
23004 continue

      call unstal(evalst(ilo),lname)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      subroutine dodmp(argstk,i,j)
cProlog
      integer i,j,argstk(1)
      integer k,a1,a2
      integer dumpdf
      external dumpdf, dumpa
      character*80 out


      integer cp
      integer ep
      integer mline(200),mlev(200)

      common /cmacro/ cp,ep,mline,mlev

      integer evalst(50000)
      common /cmacrc/ evalst



      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      do 23000 k = i + 2 ,j
         a1 = argstk(k)
         a2 = argstk(k+1)
         if (a2.eq.a1) then
            call dumpa
         else
c while(type(evalst(a1)) == 5 & a1 <a2)
23002       if (type(evalst(a1)) .eq. 5 .and. a1 .lt.a2) then
               a1 = a1 + 1
               go to 23002
            endif
c endwhile
c while(type(evalst(a2-1)) == 5 & a1 <a2)
23004       if (type(evalst(a2-1)) .eq. 5 .and. a1 .lt.a2) then
               a2 = a2 - 1
               go to 23004
            endif
c endwhile
            if ( a2-a1 .le.0) call warn('Dumpdef: empty name')
            if ( dumpdf(a2-a1,evalst(a1)) .ne. 0) then
               do 23006 i=1, a2-a1
                  out(i:i) = char(evalst(a1-1+i))
23006          continue
               call warn('Dumpdef: name not defined')
               call rem(out,a2-a1)
            endif
         endif
23000 continue

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c Set either RealSize or IntSize definition.  Called whenever "realsize"
c or "intsize" is changed, to change the corresponding macros.

      subroutine setsize(name,val)
cProlog
      character *(*)name
      integer val
      integer i23000
      character *1 s1
      character *2 s2

c select
      i23000= val
         go to 23000
c -- case 2
c -- case 4
c -- case 8
23002 continue

         write(s1,'(i1)') val
         call cnstal(name,s1)
23003    go to 23001
c -- case 16
23004 continue

         write(s2,'(i2)') val
         call cnstal(name,s2)
23005    go to 23001
c -- case (default)
23006 continue

         call oops("Attempted to write bad size in setsize")
c -- dispatch area for select
23007 go to 23001
23000 continue
      if ( i23000 .eq. 2) go to 23002
      if ( i23000 .eq. 4) go to 23002
      if ( i23000 .eq. 8) go to 23002
      if ( i23000 .eq. 16) go to 23004
      go to 23006
23001 continue
c endselect

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c Query the hash table and find the current definition of WORDSIZE
c Fatal error if WORDSIZE is not defined

      subroutine getwdsz(ws)
cProlog
      integer ws,getdef
      external getdef
      character *8 wsstr

      if (getdef('WORDSIZE',wsstr) .gt. 0)then
         read(wsstr,'(i2)') ws
         return
      endif

      call oops("WORDSIZE is not defined!")
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c Read a definition from the hash table, leaving the string value in "val".
c Val must be long enough to store the definition value.  Return value is
c length of "val", or -1 if "def" was not in the table.

      integer function getdef(def,val)
cProlog
      character *(*) def, val
      integer lookup,defp
      integer i, readdef, lnb
      external lookup,readdef, lnb
      integer idef(32),lidef,lval

c Measure the length of "def" and convert it to integer form
      lidef = lnb(def)
      do 23000 i=1,lidef
         idef(i) = ichar(def(i:i))
23000 continue

      defp = lookup(lidef,idef)
      if (defp .eq. -1) then
         getdef = -1
         return
      endif

      lval = readdef(defp,val)

      getdef = lval
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

      integer function lnb(s)
cProlog
      character *(*) s
      integer i

      do 23000 i=len(s),1,-1
         if (s(i:i) .ne. ' ') go to 23001
23000 continue
23001 continue

      lnb = i
      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c Remove a definition from the hash table.

      subroutine undef(def)
cProlog
      character *(*) def
      integer i
      integer idef(32),lidef
      integer lnb
      external lnb

c Measure the length of "def" and convert it to integer form
      lidef = lnb(def)
      do 23000 i=1,lidef
         idef(i) = ichar(def(i:i))
23000 continue

      call unstal(idef, lidef)

      return
      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------

c Write a comment at the end of a line
c Do not break the line

      subroutine endcomment(ipos, icont, stoken, sltoken, wswid)
cProlog


      integer iusin(5),lnbt
      integer lcbuf
      common /cfile/iusin,lcbuf,lnbt

      character*(256) filnam(5)
      common /cfilec/filnam
      logical firstfil, verbose
      common /cfiled/firstfil, verbose

      character*200 cbuf
      character*200 cbsav(5)
      integer lcsav(5)
      common /cbsav1/ lcsav
      common /cbsav2/ cbsav
      common /cinbuf/cbuf

      integer outbuf(50000),lastpc,conpc,col72,colcom,icontinue,maxws
      integer cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, 
     &   optalloc,optnum,nonblk
      logical bckeep
      character cchar
      common /cputc/outbuf,lastpc,conpc,col72,colcom,icontinue,maxws, 
     &   cichar,optlang,optmacro,optpretty,indlev,optrel,hnl, optalloc,
     &   optnum,nonblk,bckeep
      common /cputcc/ cchar

      integer bp,pblinl(5)
      common /cdef/bp,pblinl

      integer pbbuf(50000),pblins(50000,5)
      common /cdefc/pbbuf,pblins


      integer type(0:127)
      integer alphan(0:127)
      integer contu(0:127)
      integer comchar
      logical col1(0:127)
      logical col6
      common /ctype/type,alphan,contu,comchar,col1,col6


      integer indpc,indflg
      integer level,iflevel,ifacelev
      integer dotype(0:10), doindent(0:10)
      integer labn(0:10),labb(0:10)
      integer usen(0:10),useb(0:10)
      integer token(20000),ltoken,code(2)
      integer modtype(0:10), modfun
      character*(130) foriter(0:10)
      common /stat1/indpc,indflg,level,iflevel,ifacelev
      common /stat1/dotype,doindent,labn,labb,usen,useb,token,ltoken,
     &   code, modtype,modfun
      character*32 modname(0:10)
      common /stat2/ modname, foriter
      integer seltop
      integer sellast
      integer selstak
      integer sellev
      integer sellab
      common /csel/ sellev,seltop, sellast, selstak(500),sellab(10)


      integer stdin,stdout,stderr
      common /stdunits/ stdin,stdout,stderr


      integer ipos, icont, stoken(*), sltoken, wswid
      integer i, toksiz, wswid0
      character*(200) msg
      integer zpakchrz, gtok
      external zpakchrz, gtok

      if (optlang .eq. 2) then
         if (ipos .eq. 1) then
c write comment out immediately, before statement
            toksiz = zpakchrz(msg, len(msg), stoken, sltoken)
            call wrline(stdout,msg,toksiz)
         else
c put current line, then the comment
            call outlin
            call putchr(stoken, sltoken)
         endif
         if (lastpc .gt. 0) call outlin
         if (icont .eq. 1) then
            do 23000 i=1,5
               outbuf(i) = 32
23000       continue
            outbuf(6) = 38
            lastpc = 6
         endif

      elseif (optlang .eq. 3 .or. optlang .eq. 5) then
c for f90, keep comment at the end of the line
c do not continue comments across lines
         if (icont .eq. 1) then
            call putqs(" &")
            wswid0 = max(0, wswid-2)
         else
            wswid0 = wswid
         endif

         if (colcom .eq. -1) then
            call outfil(wswid0)
         elseif (colcom .gt. lastpc) then
            call outfil(colcom - lastpc - 1)
         endif

         do 23002 i=1,sltoken
            outbuf(lastpc + i) = stoken(i)
23002    continue
         lastpc = lastpc + sltoken
         call outlin
         if (icont .eq. 1) then
            icontinue = 1
         endif

      else
         call outfil(wswid)
         code(1) = comchar
c actual comment character used
         call putchr(code,1)
         call putchr(stoken, sltoken)
         call outlin
      endif

      end

c--------------------------------------------------------------------------
c--------------------------------------------------------------------------
