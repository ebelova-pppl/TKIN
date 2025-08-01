         
         
       
        
         
        
       
       
       
        
        
        
       
            
          
      module param
      parameter( nid= 120 ,  njd= 60 , nkd=64 , nijkd= nid*njd*nkd, ni= 
     &nid +4,  nim= ni -1, nim2= ni -2, nim3= ni -3, nim4= ni -4, nj= 
     &njd +4, njm= nj -1, njm2= nj -2, njm3= nj -3, njm4= nj -4, nij= ni
     &*nj,  k2= 2, k3= 3, k3m= 2, nk= nkd +4, nkm= nk -1, nkm2= nk -2, 
     &nkm3= nk -3, nkm4= nk -4,  nijk= ni*nj*nk, nstyp= 21,  nvtyp= 22, 
     & npx= 100 ,  nspx= 1 ,  nvec= 1,  nvecm= nvec -1, nvecp= nvec +1 )
     & 
      end
      module cm_par 
      real :: beta,     dq,  dqi,  dr,  dri,  ds,  dsi,  gamma,  odgamma
     &,  rpi,  rpi2,  rpid180,  q1in,  q2in,  r1in,  r2in, s1in, s2in, 
     &ri1in,  ri2in,    rj1in,  rj2in, rk1in, rk2in, aw1,  aw2,  aw3,  
     &aw4,  t,  tdgammam,  rhoc,  pcut,  vpert,  zbdzeff 
      integer :: its,  idrun,  ialign,      idum,  np 
      end
      module cm_cmvec 
      real, allocatable, dimension(:,:,:) :: sh3,  sh3w,  sh3i,  t12hder
     &,  t13hder, t21hder, t23hder, t31hder, t32hder, v1h,v2h,v3h,  v1hi
     &,v2hi,v3hi,  v1h2,v2h2,v3h2,  v1h2i,v2h2i,v3h2i,  v1h2d1,v2h2d1,v3
     &h2d1 
      end
      module cm_fevec 
      use param
      real :: a1styp(2,3,nstyp),  a2styp(2,3,nstyp),  d(-2:2,3),  h3sum,
     &  q(ni),  qlo,  qhi,  qlen,  r(nj),  rlo,  rhi,  rlen,  s(nk),  
     &slo,  shi,  slen,  sins(nk),  coss(nk),  sin2s(nk),  cos2s(nk),  
     &wq(ni),  wr(nj), ws(nk)
      integer :: ibstyp(2,3,nstyp),  isvtyp(3,nvtyp),  na(3),  nam(3),  
     &nam2(3),  nam3(3),  nam4(3) 
      end
      module cm_fld 
      real, allocatable, dimension(:,:,:) :: sp,  sp1,   srhob1,  srhoq,
     &  srhoq0,  spi0,  spsi,  sphi,  rmask,  sres,  sg,  v1b1,v2b1,v3b1
     &,  v1a,v2a,v3a,  v1cur,v2cur,v3cur,  v1curi,v2curi,v3curi,  v1
     &curi0,v2curi0,v3curi0,  v1e,v2e,v3e,  v1mo1,v2mo1,v3mo1,  v1ub,v2
     &ub,v3ub,  v1vhypa,v2vhypa,v3vhypa 
      end
      module cm_part 
      real, allocatable, dimension(:) :: pv1x1,pv2x1,pv3x1,  pv1v1,pv2v1
     &,pv3v1,  pspphi,  psr,  psmu,  psw,  psp 
      end
      module cm_spec 
      real, dimension(1 ) :: dennsp,  qnsp,   qsp,  tratsp,  vthplsp,  
     &vthprsp,  wnsp,   wsp,  wdqsp 
      integer :: nsp,  mfsp(1 ),  mlsp(0:1 ),  npsp(1 ) 
      character*20 cisp(1 ) 
      end
      module cm_pgrid 
      use param
      parameter( nxd= 71 , nyd= 71 , nx= nxd +4, ny= nyd +4, nxm2= nx -2
     &, nym2= ny -2, nxy= nx*ny, nixy= ni*nx*ny )
      real :: dx, dy, dxi, dyi, x(nx), y(ny), xlen, ylen, xlo, xhi, ylo,
     & yhi, x0, y0, rxy(nx,ny), sxy(nx,ny), cosp(nx,ny), sinp(nx,ny) 
      integer :: ibp(nx,ny) 
      end
      module cm_out 
      real :: en,  enb,  enk,  enp 
      integer :: ienout,   isfldout 
      end
      module cm_tk 
      real :: psi0,  u0,  u1,  fwall,  p0,  pmax,  p1,  r0,  rm,  rhoc1,
     &  alpha,  alphai,  rmir,  bt,  hi0,  omegai0,  ti0,  ni0,  dz_gs, 
     & dr_gs,  rci,  riphi,  alphah1,  alphah2,  omega1,  omega2,  bv,  
     &q0,  zb,  zeff 
      integer :: nz,  nr,  nzd2p1,  iaxis,  jaxis 
      end
      module cm_df 
      real :: v0,  vstar,  deltav,  lambda0,  deltal,  rl0,  rdl,  etaf,
     &  deltala,  lambda0a,  b0,  pminus,  pplus 
      integer :: aav,  bbv,  betav 
      end
      module MPI_module
       include "mpif.h" 
      integer :: ierr, myid, nprocs
      integer, parameter :: master=0
      real :: tstart, tend
       end module MPI_module 
      program hyb
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_out
      use cm_part 
      use cm_spec
      call MPI_init(ierr)
      call inpar
      call init
      call savedump(0)
      call finalize
      call MPI_finalize(ierr)
      print *, 'stop 999'
      end
      subroutine inpar
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_out
      use cm_spec
      namelist /hin/ idrun, isbc, ivbc_b, ivbc_v, beta, gamma, vpert, 
     &q1in, r1in, s1in, q2in, r2in, s2in, ri1in, rj1in, rk1in, ri2in, 
     &rj2in, rk2in, nsp, ialign, cisp, dennsp, npsp, qnsp, tratsp, 
     &vthplsp, wnsp
      idrun=0
      beta=.5
      gamma=1.6666666666667
      vpert=0.
      q1in=0.
      r1in=0.
      s1in=0.
      q2in=1.
      r2in=1.
      s2in=1.
      ri1in=3.
      rj1in=3.
      rk1in=3.
      ri2in=-2.
      rj2in=-2.
      rk2in=-2.
      nsp=1
      ialign=1
      cisp='hydrogen$'
      dennsp=1.
      npsp=1
      qnsp=1.
      tratsp=1.
      vthplsp=0.01
      wnsp=1.
      open(11,file='hmin.i',status='old')
      read(11,hin)
      close(11)
       
      if( ri1in.lt.+1 .or. rj1in.lt.+1 .or. rk1in.lt.+1 .or. ri2in.gt.-1
     & .or. rj2in.gt.-1 .or. rk2in.gt.-1 ) then
         print *, 
     &'Error:  ri1in<+1 | rj1in<+1 | rk1in<+1 | ri2in>-1 | rj2in>-1 | rk
     &2in>-1  in  inpar  > exit'
         call exit(0)
      endif
      ibstyp= reshape( (/ 1,1,1,1,1,1,  2,2,2,2,2,2,  2,2,9,9,2,2,  2,2,
     &10,9,2,2,  2,2,11,9,2,2,  2,2,10,10,2,2,  2,2,3,3,2,2,  2,2,4,4,2,
     &2,  2,2,7,11,2,2,  2,2,8,11,2,2,  2,2,7,10,2,2,  2,2,8,10,2,2,  2,
     &2,7,9,2,2,  2,2,8,9,2,2,  5,5,5,5,5,5,  9,9,9,9,2,2,  6,6,6,6,2,2,
     &  9,9,10,10,2,2,  10,10,9,9,2,2,  11,11,12,12,2,2,  13,13,13,13,2,
     &2 /),  (/ 2,3,nstyp /) )
      isvtyp= reshape( (/ 1,1,1,  2,2,2,  13,12,14,  13,14,14,  7,8,7,  
     &8,7,8,  9,12,10,  11,10,12,  13,12,14,  11,14,12,  5,6,5,  3,4,3, 
     & 4,3,4,  4,3,3,  6,6,6,  11,14,14,  11,12,12,  19,18,16,  16,16,16
     &,  21,21,21,  17,17,17,  16,16,20 /),  (/ 3,nvtyp /) )
      return 
      end
      subroutine init
      call dfcon
      call dfgrid
      call dfgrid_p
      call dfvec
      call alloc_fld
      call dfh
      call fromh
      call dfspec
      call initfld
      call alloc_part
      call initpart
      return 
      end
      subroutine dfcon
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_out
      rpid4= atan2(1.,1.) 
      rpi= 4.*rpid4 
      rpi2= rpi*2.
      rpid180= rpi/180.
      its= 0 
      t= 0. 
      odgamma= 1./gamma
      tdgammam= 2./(gamma-1.)
      ienout= 0 
      isfldout= 0
      a1styp= 0.
      a2styp= 0.
      idum= -1
      call random_init(.true., .true.) 
      return 
      end
      subroutine dfgrid
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      if( r1in.eq.0. ) then
          
         if( rj1in.ne.1 .or. rj2in.ne.-3 ) then
            print *, 'Error:  rj1in<>1 | rj2in<>-3 in  dfgrid  > exit'
            call exit(0)
         endif
      else
          
         if( rj1in.ne.3 .or. rj2in.ne.-3 ) then
            print *, 'Error:  rj1in<>3 | rj2in<>-3 in  dfgrid  > exit'
            call exit(0)
         endif
      endif
      ri2abs= ni +1 +ri2in
      rj2abs= nj +1 +rj2in
      rk2abs= nk +1 +rk2in
      dq= (q2in -q1in)/(ri2abs -ri1in)
      dr= (r2in -r1in)/(rj2abs -rj1in)
      ds= (s2in -s1in)/(rk2abs -rk1in)
      dqi= 1./dq
      dri= 1./dr
      dsi= 1./ds
      do 23000 i= 1,ni
         q(i)= q1in +(i-ri1in)*dq
23000 continue
      if( ibstyp(1,1,16 ).eq.2 ) then
         qlo= .5* ( q(2) + q(3) )
      else
         qlo= q(3)
      endif
      if( ibstyp(2,1,16 ).eq.2 ) then
         qhi= .5* ( q(nim2) + q(nim) )
      else
         qhi= q(nim2)
      endif
      qlen= qhi - qlo
      do 23002 j= 1,nj
         r(j)= r1in +(j-rj1in)*dr
23002 continue
      rlo= r(int(rj1in))
      rhi= r(njm2)
      rlen= rhi - rlo
      do 23004 k= 1,nk
         s(k)= s1in +(k-rk1in)*ds
23004 continue
      slo= s1in +(2.5 -rk1in)*ds
      shi= s1in +(nkm2 + .5 -rk1in)*ds
      slen= shi - slo
      oddq12= 1./(12.*dq)
      d(-2,1)= oddq12
      d(-1,1)= -8.*oddq12
      d( 0,1)= 0.
      d( 1,1)= 8.*oddq12
      d( 2,1)= -oddq12
      oddr12= 1./(12.*dr)
      d(-2,2)= oddr12
      d(-1,2)= -8.*oddr12
      d( 0,2)= 0.
      d( 1,2)= 8.*oddr12
      d( 2,2)= -oddr12
      odds12= 1./(12.*ds)
      d(-2,3)= odds12
      d(-1,3)= -8.*odds12
      d( 0,3)= 0.
      d( 1,3)= 8.*odds12
      d( 2,3)= -odds12
      aw1= 17./48.
      aw2= 59./48. 
      aw3= 43./48.
      aw4= 49./48.
      if( ibstyp(1,1,16 ).eq.2 ) then
         do 23006 i= 3,nim2
            wq(i)= 1.
23006    continue
      else
         if( nid .gt. 7 ) then
            wq(3)= aw1
            wq(4)= aw2
            wq(5)= aw3
            wq(6)= aw4
            do 23008 i= 7,nim4-2
               wq(i)= 1.
23008       continue
            wq(nim4-1)= aw4
            wq(nim4)= aw3
            wq(nim3)= aw2
            wq(nim2)= aw1
         elseif( nid .gt. 1 ) then
            wq(3)= .5
            do 23010 i= 4,nim3
               wq(i)= 1.
23010       continue
            wq(nim2)= .5
         else
            wq(3)= 1.
         endif
      endif
      wq(1)= 0.
      wq(2)= 0.
      wq(nim)= 0.
      wq(ni )= 0.
      if( ibstyp(1,2,16 ).eq.2 ) then
         do 23012 j= 3,njm2
            wr(j)= 1.
23012    continue
      else
         if( njd .gt. 7 ) then
            wr(3)= aw1
            wr(4)= aw2
            wr(5)= aw3
            wr(6)= aw4
            do 23014 j= 7,njm4-2
               wr(j)= 1.
23014       continue
            wr(njm4-1)= aw4
            wr(njm4)= aw3
            wr(njm3)= aw2
            wr(njm2)= aw1
         elseif( njd .gt. 1 ) then
            wr(3)= .5
            do 23016 j= 4,njm3
               wr(j)= 1.
23016       continue
            wr(njm2)= .5
         else
            wr(3)= 1.
         endif
      endif
      wr(1)= 0.
      wr(2)= 0.
      wr(njm)= 0.
      wr(nj )= 0.
      if( r1in.eq.0. ) then
         wr(1)= aw1
         wr(2)= aw2
         wr(3)= aw3
         wr(4)= aw4
         wr(5)= 1.
         wr(6)= 1.
      endif
      if( ibstyp(1,3,16 ).eq.2 ) then
         do 23018 k= 3,nkm2
            ws(k)= 1.
23018    continue
      else
         if( nkd .gt. 7 ) then
            ws(3)= aw1
            ws(4)= aw2
            ws(5)= aw3
            ws(6)= aw4
            do 23020 k= 7,nkm4-2
               ws(k)= 1.
23020       continue
            ws(nkm4-1)= aw4
            ws(nkm4)= aw3
            ws(nkm3)= aw2
            ws(nkm2)= aw1
         elseif( nkd .gt. 1 ) then
            ws(3)= .5
            do 23022 k= 4,nkm3
               ws(k)= 1.
23022       continue
            ws(nkm2)= .5
         else
            ws(3)= 1.
         endif
      endif
      ws(1)= 0.
      ws(2)= 0.
      ws(nkm)= 0.
      ws(nk )= 0.
      do 23024 k= k3,nkm2
         sins(k)= sin( s(k) )
         coss(k)= cos( s(k) )
         sin2s(k)= sin( 2*s(k) )
         cos2s(k)= cos( 2*s(k) )
23024 continue
      return 
      end
      subroutine dfgrid_p
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_pgrid
      xlen= 2.*rhi
      dx= xlen/(nxd-1)
      dxi= 1./dx
      xlo= 0.
      xhi= xlo + xlen
      do 23000 j= 1,nx
         x(j)= xlo + (j-3)*dx 
23000 continue
      ylen= 2.*rhi
      dy= ylen/(nyd-1)
      dyi= 1./dy
      ylo= 0.
      yhi= ylo + ylen
      do 23002 k= 1,ny
         y(k)= ylo + (k-3)*dy
23002 continue
      x0= 0.5*xlen
      y0= 0.5*ylen
      do 23004 k= 1,ny
         do 23006 j= 1,nx
            rxy(j,k)= sqrt( (x(j)-x0)**2 + (y(k)-y0)**2 )
            ibp(j,k)= 1
            if( rxy(j,k).lt.rlo .or. rxy(j,k).gt.rhi ) ibp(j,k)= 0
            if( rxy(j,k).eq.0. ) then
               sxy(j,k)= 0.
            else
               sxy(j,k)= acos( (x(j)-x0)/rxy(j,k)/1.0000000000001 )
               if( (y(k)-y0).lt.0. ) sxy(j,k)= rpi2 - sxy(j,k)
            endif
            cosp(j,k)= cos( sxy(j,k) )
            sinp(j,k)= sin( sxy(j,k) )
23006    continue
23004 continue
      return 
      end
      subroutine alloc_fld
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      allocate( sp(ni,nj,nk), sp1(ni,nj,nk), srhob1(ni,nj,nk) )
      allocate( srhoq(ni,nj,nk), srhoq0(ni,nj,nk), spi0(ni,nj,nk) )
      allocate( spsi(ni,nj,nk), sphi(ni,nj,nk), rmask(ni,nj,nk), sres(ni
     &,nj,nk), sg(ni,nj,nk) )
      allocate( v1b1(ni,nj,nk),v2b1(ni,nj,nk),v3b1(ni,nj,nk), v1a(ni,nj,
     &nk),v2a(ni,nj,nk),v3a(ni,nj,nk), v1cur(ni,nj,nk),v2cur(ni,nj,nk),
     &v3cur(ni,nj,nk) )
      allocate( v1e(ni,nj,nk),v2e(ni,nj,nk),v3e(ni,nj,nk), v1mo1(ni,nj,
     &nk),v2mo1(ni,nj,nk),v3mo1(ni,nj,nk), v1ub(ni,nj,nk),v2ub(ni,nj,nk)
     &,v3ub(ni,nj,nk) )
      allocate( v1curi(ni,nj,nk),v2curi(ni,nj,nk),v3curi(ni,nj,nk), 
     &v1curi0(ni,nj,nk),v2curi0(ni,nj,nk),v3curi0(ni,nj,nk) )
      allocate( v1vhypa(ni,nj,nk),v2vhypa(ni,nj,nk),v3vhypa(ni,nj,nk) )
      allocate( v1h(ni,nj,nk),v2h(ni,nj,nk),v3h(ni,nj,nk) )
      allocate( v1hi(ni,nj,nk),v2hi(ni,nj,nk),v3hi(ni,nj,nk), v1h2(ni,nj
     &,nk),v2h2(ni,nj,nk),v3h2(ni,nj,nk), v1h2i(ni,nj,nk),v2h2i(ni,nj,nk
     &),v3h2i(ni,nj,nk) )
      allocate( v1h2d1(ni,nj,nk),v2h2d1(ni,nj,nk),v3h2d1(ni,nj,nk) )
      allocate( sh3(ni,nj,nk), sh3w(ni,nj,nk), sh3i(ni,nj,nk) )
      allocate( t12hder(ni,nj,nk),t13hder(ni,nj,nk),t21hder(ni,nj,nk),
     &t23hder(ni,nj,nk),t31hder(ni,nj,nk),t32hder(ni,nj,nk) )
      return 
      end
      subroutine alloc_part
      use param
      use cm_par
      use cm_part 
      allocate( pv1x1(np),pv2x1(np),pv3x1(np), pv1v1(np),pv2v1(np),pv3v1
     &(np) )
      allocate( pspphi(np), psr(np), psmu(np), psw(np), psp(np) )
      return 
      end
      subroutine dfvec
      use param
      use cm_cmvec
      use cm_fevec
      nam4(1)= nim4 
      nam4(2)= njm4 
      nam4(3)= nkm4
      nam3(1)= nim3 
      nam3(2)= njm3 
      nam3(3)= nkm3
      nam2(1)= nim2 
      nam2(2)= njm2 
      nam2(3)= nkm2
      nam(1)= nim 
      nam(2)= njm 
      nam(3)= nkm
      na(1)= ni 
      na(2)= nj 
      na(3)= nk
      return 
      end
      subroutine dfh
      use param
      use cm_cmvec
      use cm_fevec
      v1h = 1. 
      v2h = 1. 
      v3h = 1. 
       
      do 23000 k= 1,nk
         do 23002 j= 1,nj
            do 23004 i= 1,ni
               v3h(i,j,k)= r(j) + 1.e-10
23004       continue
23002    continue
23000 continue
      return 
      end
      subroutine fromh
      use param
      use cm_cmvec
      use cm_fevec
      v1hi = 1./ v1h
      v2hi = 1./v2h
      v3hi = 1./v3h 
      v1h2 = v2h * v3h
      v2h2 = v1h * v3h
      v3h2 = v1h * v2h
      sh3 = v1h * v1h2
      do 23000 k= 1,nk
         do 23002 j= 1,nj
            do 23004 i= 1,ni
               sh3w(i,j,k)= sh3(i,j,k)*wq(i)*wr(j)*ws(k)
23004       continue
23002    continue
23000 continue
      call sum_rs( h3sum, sh3 )
      v1h2i = 1./ v1h2
      v2h2i = 1./v2h2
      v3h2i = 1./v3h2 
      sh3i = v1hi * v1h2i
      v1h2d1 = v1h2 * v1hi
      v2h2d1 = v2h2 * v2hi
      v3h2d1 = v3h2 * v3hi
      call der_ss( t12hder, v1h, 2 )
      call der_ss( t13hder, v1h, 3 )
      call der_ss( t21hder, v2h, 1 )
      call der_ss( t23hder, v2h, 3 )
      call der_ss( t31hder, v3h, 1 )
      call der_ss( t32hder, v3h, 2 )
      t12hder = t12hder * v3h2i
      t13hder = t13hder * v2h2i
      t21hder = t21hder * v3h2i
      t23hder = t23hder * v1h2i
      t31hder = t31hder * v2h2i
      t32hder = t32hder * v1h2i
      return 
      end
      subroutine dfspec
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_spec
      use cm_pgrid
      do 23000 isp= 1,nsp
         wdqsp(isp) = wnsp(isp)/qnsp(isp)
         qsp(isp)= dennsp(isp) * qnsp(isp) * h3sum / npsp(isp)
         isum= 0
         do 23002 i= 3,nim2
            do 23004 j=3,nx-2
               do 23006 k=3,ny-2
                  isum= isum + ibp(j,k)
23006          continue
23004       continue
23002    continue
         qsp(isp)= qsp(isp) * isum / h3sum
         wsp(isp)= qsp(isp)*wnsp(isp) / qnsp(isp)
         vthprsp(isp)= sqrt( tratsp(isp) ) * vthplsp(isp)
23000 continue
      np= 0
      mlsp(0)= 0
      do 23008 isp= 1,nsp 
         mfsp(isp)= mlsp(isp-1) + 1
         mlsp(isp)= mfsp(isp) + npsp(isp) - 1
         np= np + npsp(isp)
23008 continue
      return 
      end
      subroutine initfld 
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_spec
      use cm_tk
      parameter( eps=1.e-10 )
      real, dimension(ni,nj,nk) :: stmp,stmpa,svel 
      real, dimension(ni,nj,nk) :: v1tmp,v2tmp,v3tmp,v1 tmpa,v2 tmpa,v3 
     &tmpa
      call MPI_comm_rank(MPI_comm_world, myid, ierr)
      call MPI_comm_size(MPI_comm_world, nprocs, ierr)
      if( myid.eq.master ) print *, 'master proc=',myid, 'nprocs=',
     &nprocs
      rm= 0.00 
      bt= -1.00 
      rhoc= 0.10 
      p1= 0.5*rhoc/(1.-rhoc) 
      if( rm.eq.0. ) p1= 0.
      omegai0= 0.
      ti0= vthplsp(1)**2/2.
      ni0= dennsp(1)
      nz= 2*(ni-1) + 1
      nr= 2*(nj-1) + 1
      tstart= MPI_wtime()
      call gs_tk 
      tend= MPI_wtime()
      if( myid.eq.master ) print *, 'GS solver time =', (tend-tstart)/60
     &, 'min'
      if(bt.eq.0.) then
         v1a = 0.
         v2a = 0.
      endif
      v1tmp = v1b1
      v2tmp = v2b1
      v3tmp = v3b1 
      call curl_vv( v1b1,v2b1,v3b1, v1a,v2a,v3a, 22 )
      if(bt.eq.0.) v3b1 = 0.
      if(bt.eq.0.) then
         stmp = v3curi0 + v3cur
         call curl_vv( v1tmp,v2tmp,v3tmp, v1b1,v2b1,v3b1, 21 )
         stmp = v3tmp - stmp
         call maxabs_rs( rcur, stmp )
         if( myid.eq.master ) print *, 'J-Ji-Jb=',rcur
         v1cur = 0.
         v2cur = 0.
      else
         call curl_vv( v1tmp,v2tmp,v3tmp, v1b1,v2b1,v3b1, 21 )
         v1cur = v1cur + v1curi0
         v2cur = v2cur + v2curi0
         v3cur = v3cur + v3curi0 
         v1tmp = v1cur - v1tmp
         v2tmp = v2cur - v2tmp
         v3tmp = v3cur - v3tmp 
         call maxabs1_rs( rcur1, v1tmp )
         call maxabs1_rs( rcur2, v2tmp )
         call maxabs1_rs( rcur3, v3tmp )
         call maxabs1_rs( rcurt, v3cur )
         if( myid.eq.master ) print *, 'Jb+Ji-curl(B)=', rcur1, rcur2, 
     &rcur3
         if( myid.eq.master ) print *, '|J_pol|=', maxval(sqrt(v1cur**2+
     &v2cur**2)),'  J_phi=', rcurt
          
         open(unit=13,file='j_tot.dat')
         write(13,9) nid, njd
         write(13,10) (q(i),i=3,nim2),(r(j),j=3,njm2)
         write(13,10) ((v1cur(i,j,k3),i=3,nim2),j=3,njm2) 
         write(13,10) ((v2cur(i,j,k3),i=3,nim2),j=3,njm2)
         write(13,10) ((v3cur(i,j,k3),i=3,nim2),j=3,njm2)
         close(13)
      endif
      sp1 = 0.
      pcut= 0.0
      if( beta.ne.0. ) then
         pcut= 0.0001
         if( rm.eq.0. ) sp = sp + pcut
         do 23000 k= 1,nk
            do 23002 j= 1,nj
               do 23004 i= 1,ni
                  if( sp(i,j,k) .gt. 0. ) then
                     sp1(i,j,k)= ( sp(i,j,k) )**odgamma
                  endif
23004          continue
23002       continue
23000    continue
         pmax=0.
         do 23006 k= 1,nk
            do 23008 j= 1,nj
               do 23010 i= 1,ni
                  pmax=amax1(pmax,sp(i,j,k))
23010          continue
23008       continue
23006    continue
         call fix_s( sp1, sp1, 16 )
      endif
      v1e = 0. 
      v2e = 0. 
      v3e = 0. 
      if( myid.eq.master ) print *, 'rhoc=', rhoc
      stmp= srhob1 - 0.9*rhoc
      sres= 1/stmp
      v1ub = 0. 
      v2ub = 0. 
      v3ub = 0. 
      v1mo1 = 0. 
      v2mo1 = 0. 
      v3mo1 = 0. 
      do 23012 k= 1,nk
         do 23014 j= 1,nj
            do 23016 i= 1,ni
               if( spsi(i,j,k) .lt. u1 ) then
                  xx= (spsi(i,j,k)-u1)/(psi0-u1)
                  v1mo1(i,j,k)= vpert*(0.5-ran2(idum))*xx**2
                  v2mo1(i,j,k)= vpert*(0.5-ran2(idum))*xx**2
               endif
23016       continue
23014    continue
23012 continue
      v1mo1 = v1mo1 * srhob1 
      v2mo1 = v2mo1 * srhob1 
      v3mo1 = v3mo1 * srhob1 
      rpi0= 3.*wdqsp(1)*vthplsp(1)**2/2.
      spi0 = srhoq0 * rpi0
      stmpa = v3curi0 * v3curi0
      stmp = srhoq0 + eps
      stmpa = stmpa / stmp
      spi0 = spi0 + stmpa
      v1tmp = v1cur - v1curi0
      v2tmp = v2cur - v2curi0
      v3tmp = v3cur - v3curi0 
      call cros_vvv( v1tmp,v2tmp,v3tmp, v1tmp,v2tmp,v3tmp, v1b1,v2b1,v3
     &b1 ) 
      call div_vvv( v1tmpa,v2tmpa,v3tmpa, v1ub,v2ub,v3ub, v1mo1,v2mo1,v3
     &mo1, 1 ) 
      v1tmp = v1tmp - v1tmpa
      v2tmp = v2tmp - v2tmpa
      v3tmp = v3tmp - v3tmpa 
      call grad_vs( v1tmpa,v2tmpa,v3tmpa, sp, 1 ) 
      v1tmp = v1tmp - v1tmpa
      v2tmp = v2tmp - v2tmpa
      v3tmp = v3tmp - v3tmpa 
      call maxabs_rs( f1, v1tmp )
      call maxabs_rs( f2, v2tmp )
      call maxabs_rs( f3, v3tmp )
      if( myid.eq.master ) print *, 'F= -div(v*mom)-grad(p)+(J-Ji)xB=',
     &f1,f2,f3
   10 format(5es12.3)
    9 format(2i4)
      return 
      end
      subroutine gs_tk
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_spec
      use cm_tk
      use cm_df
      real apsi(nz,nr),rad(nr)
      real spt(nz,nr),spp(nz,nr)
      real jz(nz,nr), jr(nz,nr), jphi(nz,nr)
      real sf(nz,nr), deni(nz,nr)
      real hi(nz,nr),dhi(nz,nr),hhp1(nz,nr),hhp2(nz,nr)
      real, dimension(ni,nj,nk) :: stmp 
      real, dimension(ni,nj,nk) :: v1tmp,v2tmp,v3tmp
      real rii(3), rib(3)
      dr_gs= ( r(nj)-r(1) )/(nr-1) 
      rci= r(1)
      rc= r(nj)
      do 23000 j=1,nr
         rad(j)= rci + (j-1)*dr_gs
23000 continue
      zc= 0.5*( q(ni)-q(1) )
      dz_gs= 2*zc/(nz-1)
      nzd2p1= nz/2+1
      v0= vthplsp(1)
      call gssolve_tk(apsi,zc,rc,rad) 
      call pdpdpsi(spt,spp,apsi,rad)
      pmax= 0.
      psimax= 0.
      do 23002 j= 1,nr
         do 23004 i=1,nz
            pmax= amax1(pmax,spt(i,j))
            psimax= amax1(psimax,apsi(i,j))
23004    continue
23002 continue
      if(abs(bt).gt.0.) then
         call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,apsi,rad)
      endif
      call j_i(sf,jz,jr,jphi,deni,apsi,hi,rad) 
      do 23006 k= 1,nk
         do 23008 j= 1,nj
            do 23010 i= 1,ni
               ii= 2*i
               if( ibstyp(1,1,16 ).ne.2 ) ii= 2*(i-1)+1
               jj= 2*j
               if( ibstyp(1,2,16 ).ne.2 ) jj= 2*(j-1)+1
               sp(i,j,k)= spt(ii,jj) 
               v3cur(i,j,k)= -r(j)*spp(ii,jj) 
               stmp(i,j,k)= apsi(ii,jj) 
               srhoq0(i,j,k) = deni(ii,jj) 
               v1curi0(i,j,k)= jz(ii,jj)
               v2curi0(i,j,k)= jr(ii,jj)
               v3curi0(i,j,k)= jphi(ii,jj) 
               sg(i,j,k)= sf(ii,jj) 
23010       continue
23008    continue
23006 continue
      call grad_vs( v1tmp,v2tmp,v3tmp, sg, 1 )
      v1tmp = v1tmp / v3h 
      v2tmp = v2tmp / v3h 
      v3tmp = v3tmp / v3h 
      call maxabs_rs(djz,v1curi0-v2tmp)
      call maxabs_rs(ajz,v1curi0)
      call maxabs_rs(djr,v2curi0+v1tmp)
      call maxabs_rs(ajr,v2curi0)
      spsi = stmp
      v3a = stmp / v3h
      v3a = - v3a
      call grad_vs( v1tmp,v2tmp,v3tmp, stmp, 1 )
      v1tmp = v1tmp / v3h 
      v2tmp = v2tmp / v3h 
      v3tmp = v3tmp / v3h 
      v1b1 = - v2tmp
      v2b1 = v1tmp
      call fix_v( v1b1,v2b1,v3b1, v1b1,v2b1,v3b1, 22 )
      if( bt.ne.0. ) then
         do 23012 k= 1,nk
            do 23014 j= 1,nj
               do 23016 i= 1,ni
                  ii= 2*i 
                  if( ibstyp(1,1,16 ).ne.2 ) ii= 2*(i-1)+1
                  jj= 2*j
                  if( ibstyp(1,2,16 ).ne.2 ) jj= 2*(j-1)+1
                  v3b1(i,j,k)= (hi(ii,jj) + sf(ii,jj))/r(j) 
                  v1cur(i,j,k)= -dhi(ii,jj)*v1b1(i,j,k) 
                  v2cur(i,j,k)= -dhi(ii,jj)*v2b1(i,j,k) 
                  v3cur(i,j,k)= v3cur(i,j,k) - (hi(ii,jj) + sf(ii,jj))*
     &dhi(ii,jj)/r(j)
23016          continue
23014       continue
23012    continue
         do 23018 j= 1,nj
            v2a(1,j,k3)= 0.
23018    continue
         do 23020 i= 2,7 
            do 23022 j= 1,nj
               v2a(i,j,k3)= 0.5*(v3b1(1,j,k3) + v3b1(i,j,k3))
23022       continue
            do 23024 ii= 2,i-1
               do 23026 j= 1,nj
                  v2a(i,j,k3)= v2a(i,j,k3) + v3b1(ii,j,k3)
23026          continue
23024       continue
23020    continue
         do 23028 i= 8,ni 
            do 23030 j= 1,nj
               v2a(i,j,k3)= aw1*(v3b1(1,j,k3) + v3b1(i, j,k3)) +aw2*(
     &v3b1(2,j,k3) + v3b1(i-1,j,k3)) +aw3*(v3b1(3,j,k3) + v3b1(i-2,j,k3)
     &) +aw4*(v3b1(4,j,k3) + v3b1(i-3,j,k3))
23030       continue
            do 23032 ii= 5,i-4
               do 23034 j= 1,nj
                  v2a(i,j,k3)= v2a(i,j,k3) + v3b1(ii,j,k3)
23034          continue
23032       continue
23028    continue
         do 23036 j= 1,nj
            do 23038 i= 1,ni
               v2a(i,j,k3)= v2a(i,j,k3)*dq
               do 23040 k= 1,nk
                  v2a(i,j,k)= v2a(i,j,k3)
                  v1a(i,j,k)= 0. 
23040          continue
23038       continue
23036    continue
         call fix_v( v1b1,v2b1,v3b1, v1b1,v2b1,v3b1, 22 )
         call fix_v( v1cur,v2cur,v3cur, v1cur,v2cur,v3cur, 21 )
      endif
      do 23042 k= 1,nk
         do 23044 j= 1,nj
            do 23046 i= 1,ni
               u2= u1*1.0
               x= (spsi(i,j,k)-u2)/(psi0-u2)
               if( x.gt.0. .and. abs(q(i)) .lt. 29.5) then
                  srhob1(i,j,k)= ( exp(-1.5*(1-x)**1.5) - exp(-1.5) ) /(
     &1 - exp(-1.5))
                  srhob1(i,j,k)= (1-ni0)*srhob1(i,j,k)
                  srhob1(i,j,k)= (1-rhoc)*srhob1(i,j,k) + rhoc
               else
                  srhob1(i,j,k)= rhoc
               endif
23046       continue
23044    continue
23042 continue
      fwall= 2.0 
      wpsi= fwall*psimax 
      do 23048 k= 1,nk
         do 23050 j= 1,nj
            do 23052 i= 1,ni
               if( stmp(i,j,k).lt.wpsi ) then
                  rmask(i,j,k)= 1.
               else
                  rmask(i,j,k)= 0.
               endif
23052       continue
23050    continue
23048 continue
      rii= 0.
      rib= 0.
      do 23054 i= 1,ni
         do 23056 j= 1,nj
            rii(3)= rii(3) + r(j)*v3curi0(i,j,k3)
            rib(3)= rib(3) + r(j)*v3cur(i,j,k3)
23056    continue
23054 continue
      if( myid.eq.master ) print *, 'I_i_phi/I_phi= ', rii(3)/(rib(3)+
     &rii(3))
      rii(1)= maxval(abs(v1curi0(:,:,k3)))
      rib(1)= maxval(abs(v1cur(:,:,k3)))
      rii(2)= maxval(abs(v2curi0(:,:,k3)))
      rib(2)= maxval(abs(v2cur(:,:,k3)))
      rii(3)= maxval(abs(v3curi0(:,:,k3)))
      rib(3)= maxval(abs(v3cur(:,:,k3)))
      if( myid.eq.master ) print *, 'J_i_max/J_b_max(z,r,phi)= ', rii/
     &rib
      zbdzeff= zb/zeff
      psum= 0.
      bsum= 0.
      vsum= 0.
      do 23058 i= 1,ni
         do 23060 j= 1,nj
            if( sp(i,j,k3).gt.0.00*p0 ) then
               psum= psum + sp(i,j,k3)*r(j)
               bsum= bsum + (v1b1(i,j,k3)**2 + v2b1(i,j,k3)**2 + v3b1(i,
     &j,k3)**2)*r(j)
               vsum= vsum + 1.*r(j)
            endif
23060    continue
23058 continue
      beta_av= 2.*psum/bsum
      qq0= -2*(hi(iaxis,jaxis) + sf(iaxis,jaxis)) /( r0*( r0**2*spp(
     &iaxis,jaxis) + hi(iaxis,jaxis)*dhi(iaxis,jaxis) + sf(iaxis,jaxis)*
     &dhi(iaxis,jaxis) - r0*jphi(iaxis,jaxis) ) )
      if( myid.eq.master ) print *, ' '
      if( myid.eq.master ) print *, 'R0=',r0,'   alphah1=',alphah1,
     &'   alphah2=',alphah2,'   psi0=',psi0
      if( myid.eq.master ) print *, 'q0=',qq0, '    <beta>=',beta_av
      if( myid.eq.master ) print *, ' '
      rsep= 0.
      zsep= 0.
      do 23062 i= 1,ni
         do 23064 j= 1,nj
            if( spsi(i,j,k3).lt.0.0 ) then
               if( q(i) .gt. zsep ) zsep= q(i)
               if( r(j) .gt. rsep ) rsep= r(j)
            endif
23064    continue
23062 continue
      if( myid.eq.master ) print *, ' '
      if( myid.eq.master ) print *, 'Rs=',rsep,' Zs=',zsep
      if( myid.eq.master ) print *, ' '
      j3= 3
      open(unit=15,file='psi.dat')
      write(15,*) 'equilibrium'
      write(15,9) nid, (njm2-j3+1)
      write(15,10) (q(i),i=3,nim2),(r(j),j=j3,njm2)
      write(15,10) ((spsi(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((sp(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((v1b1(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((v2b1(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((v3b1(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ( (srhoq0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((v3curi0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ( (srhob1(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((v1cur(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((v2cur(i,j,k3),i=3,nim2),j=j3,njm2)
      write(15,10) ((v3cur(i,j,k3),i=3,nim2),j=j3,njm2)
   10 format(5f12.5)
    9 format(2i4)
      close(15)
      open(unit=16,file='ji.dat')
      write(16,9) nid, (njm2-j3+1)
      write(16,10) ( (srhoq0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((v1curi0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((v2curi0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((v3curi0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((sg(i,j,k3),i=3,nim2),j=j3,njm2)
      close(16)
      return 
      end
      subroutine gssolve_tk(u,zc,rc,rad)
      use MPI_module
      use cm_tk
      parameter( eps=1.e-3 )
      real, dimension(nz,nr) :: u, g, uold
      real, dimension(nz,nr) :: p, pp, jz, jr, jphi
      real, dimension(nz,nr) :: sf, deni
      real, dimension(nz,nr) :: hi, dhi, hhp1, hhp2, psie
      real :: rad(nr)
      integer :: index(2)
      real r2,dz,dr,err
      real dminn1,dmax1,dmin2,dmax2,t,kappa,f,r,q,omega
      psi(r,z)= 4.*( 1. - (r/rs)**2 - (z/zs)**2 )*(r/rs)**2
      namelist /tk/ e, psi0, u1, p0, xs, riphi, bv, q0, rmir
      open(11,file='tk.i',status='old')
      read(11,tk)
      close(11)
      zb= 1. 
      zeff= 1.2 
      r0= 0.6*(rad(1)+rad(nr)) 
      rs= rc*xs 
      zs= rs*e 
      hi0= bt*21.2 
      u1rel= u1
      u1= u1rel*psi0 
      riphi0= riphi
      dz= dz_gs
      dr= dr_gs
      jphi = 0.
      sf = 0.
      do 23000 i=1,nz
         z= -zc + (i-1)*dz
         do 23002 j=1,nr
            r= rad(j)
            u(i,j)= psi0*psi(r,z)
            if(u(i,j).gt.0.) u(i,j)= 0.
23002    continue
23000 continue
      alphah1= 0.
      alphah2= 0.
      call pdpdpsi(p,pp,u,rad)
      call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,u,rad)
      sum= 0.
      do 23004 i=1,nz
         do 23006 j=1,nr
            sum= sum -rad(j)*pp(i,j) -(hi(i,j)+sf(i,j))*dhi(i,j)/rad(j) 
     &+jphi(i,j)
23006    continue
23004 continue
      riphi= sum*dr*dz 
      if( riphi0.gt.0.0 ) riphi= riphi0
      if( myid.eq.master ) print *, 'I_phi=',riphi
      call psi_tk_bc(psie,zc,rc,rad) 
      psie0=0.
      do 23008 i=1,nz
         do 23010 j=1,nr
            if(u(i,j).ge.0.) psie0=amin1( psie0, psie(i,j) )
23010    continue
23008 continue
      do 23012 i=1,nz
         do 23014 j=1,nr
            psie(i,j)= psie(i,j)-psie0
23014    continue
23012 continue
      u = psie
      do 23016 i=2,nz-1
         z= -zc + (i-1)*dz
         do 23018 j=2,nr-1
            r= rad(j)
            u(i,j)= psi0*psi(r,z)
            if(u(i,j).gt.0.) u(i,j)= psie(i,j)
23018    continue
23016 continue
      dminn1= ( 2./dz*sin(3.1415927*dz/(4.*zc)) )**2
      dmax1= ( 2./dz*cos(3.1415927*dz/(4.*zc)) )**2
      dmin2= ( 4./rc )**2
      dmax2= ( 2./dr*cos(3.1415927*dr/(2.*rc)) )**2
      t= sqrt( (dmax1-dminn1)/(dmax1+dmin2)*(dmax2-dmin2)/(dmax2+dminn1)
     & )
      kappa= (dmax1-dminn1)/(dmax2+dminn1)*dmax2/dmax1
      f= (kappa-t)/(kappa+t)
      r= ( dmax1-dmax2+(dmax1+dmax2)*f )/(2.*dmax1*dmax2)
      q= r + (1-f)/dmax1
      omega= sqrt( (1+t)/(1-t) )
      omega1= (1+f*omega)/(q*omega+r)
      omega2= (1-f*omega)/(q*omega-r)
      omega1= omega1*dz**2
      omega2= omega2*dr**2
      alphai= - omegai0*psi0/ti0
      q0m= q0
      dq0= q0m/5.
      do 23020 ia= 1,20
         uold= u
         do 23022 l=1,500
            u0= minval(u) 
            index=minloc(u) 
            iaxis= index(1)
            jaxis= index(2)
            r0= rad(jaxis)
            psi0= u0
            u1= u1rel*psi0
            call pdpdpsi(p,pp,u,rad)
            call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,u,rad) 
            pp0= pp(iaxis,jaxis)
            hh1= hhp1(iaxis,jaxis)
            hh2= hhp2(iaxis,jaxis)
            sf0= sf(iaxis,jaxis)
            sfhp0= sf0*dhi(iaxis,jaxis) - r0*jphi(iaxis,jaxis)
            sum= 0.
            sump= 0.
            sumh1= 0.
            sumh2= 0.
            sumji= 0.
            sumgi= 0.
            do 23024 i=1,nz
               do 23026 j=1,nr
                  sum= sum - rad(j)*pp(i,j) - (hi(i,j)+sf(i,j))*dhi(i,j)
     &/rad(j) + jphi(i,j)
                  sump= sump - rad(j)*pp(i,j)
                  sumh1= sumh1 - hhp1(i,j)/rad(j)
                  sumh2= sumh2 - hhp2(i,j)/rad(j)
                  sumji= sumji + jphi(i,j) - sf(i,j)*dhi(i,j)/rad(j)
23026          continue
23024       continue
            if(l.eq.1) then
               if( myid.eq.master ) print *, 'I_phi(calcul)=', sum*dr*dz
               sum0= riphi/(dr*dz)
            endif
            r2pp0= r0**2*pp0 + sfhp0
            d0= hh1*rf2 - hh2*rf1
            if( d0.eq.0. ) then
               if( hh2.eq.0. ) then
                  rt1= r2pp0/hh1
                  rt2= 2*hi0/(q0*r0*hh1)
                  alphah1= -rt1 + .5*rf1*rt2**2 - sign(1.,rt2) *sqrt( (
     &rt1-.5*rf1*rt2**2)**2 + rt2**2 - rt1**2 )
               else
                  rt1= 2*hi0/(q0*r0) + r2pp0
                  rt2= hh1*sumh2 - hh2*sumh1
                  alphah1= - (hh2*(sum0-sump-sumji) + sumh2*rt1)/rt2
               endif
            else
               a10= (hh2 - rf2*r2pp0)/d0
               a20= (-hh1 + rf1*r2pp0)/d0
               aq= (0.5*q0*r0/hi0)**2*(-hh2*sumh1 + hh1*sumh2)/d0
               bq= (rf2*sumh1 - rf1*sumh2)/d0
               cq= -(sum0-sump-sumji) + a10*sumh1 + a20*sumh2
               x1= -0.5*bq/aq - sign(0.5,aq)*sqrt(bq**2-4*aq*cq)/aq
               x2= (0.5*q0*r0*x1/hi0)**2
               alphah1= (rf2*x1 - hh2*x2)/d0 + a10
            endif
            alphah2= (sum0 - sump - sumji - alphah1*sumh1)/sumh2
            call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,u,rad) 
            do 23028 j=1,nr
               r2= rad(j)**2
               do 23030 i=1,nz
                  g(i,j)= - r2*pp(i,j) - (hi(i,j) + sf(i,j))*dhi(i,j) + 
     &rad(j)*jphi(i,j)
23030          continue
23028       continue
            call adi_tk(u,g,nz,nr,zc,rad,err,omega1,omega2)
            if( err.lt.eps*abs(psi0) ) goto 1
23022    continue
         if( myid.eq.master ) print *, 'no convergence in gssolve'
         if( myid.eq.master ) print *, 'err=',err,'   abs(psi0)=',abs(
     &psi0)
         stop
    1    continue
         if( myid.eq.master ) print *, '# of iterations=', l, '    err='
     &,err
         qq0= -2*(hi(iaxis,jaxis) + sf(iaxis,jaxis)) /( r0*( r0**2*pp(
     &iaxis,jaxis) + hi(iaxis,jaxis)*dhi(iaxis,jaxis) + sf(iaxis,jaxis)*
     &dhi(iaxis,jaxis) - r0*jphi(iaxis,jaxis) ) )
         if( myid.eq.master ) print *, '**** q0=',qq0
         err1= maxval(abs(u-uold))
         if( myid.eq.master ) print *, '*** err1=', err1
         if( myid.eq.master ) print *, ''
         if( err1.lt.eps*abs(psi0) ) goto 2
23020 continue
      if( myid.eq.master ) print *, 'no convergence in gssolve -ia'
      if( myid.eq.master ) print *, 'err1=', err1
      stop
    2 continue
      return 
      end
      subroutine pdpdpsi(p,pp,psi,rad)
      use cm_tk
      real psi(nz,nr), p(nz,nr), pp(nz,nr), rad(nr)
      real psis, sigma
      real d, k0
      do 23000 i=1,nz
         do 23002 j=1,nr
            p(i,j)= 0.
            pp(i,j)= 0.
23002    continue
23000 continue
      iprofile= 2 
c select
      i23004= iprofile 
         go to 23004
c -- case 1
23006 continue
23007    go to 23005
c -- case 2
23008 continue
         pw= 0.00 
         zc= dz_gs*(nz-1)/2.
         do 23009 i=1,nz
            z= -zc + (i-1)*dz_gs
            do 23011 j=1,nr
               x= (psi(i,j)-u1)/(psi0-u1)
               if( x.gt.0. ) then
                  pdx= 0.25
                  p(i,j)= 0.5*( 1 + tanh((x-0.73)/pdx)) + 0.1*x**1.5
                  pnorm= 0.5*( 1 + tanh( 0.27/pdx)) + 0.1
                  p(i,j)= p0*p(i,j)/pnorm
                  pp(i,j)= ((1-(tanh((x-0.73)/pdx))**2)/(2*pdx) + 0.15*x
     &**0.5 )/(psi0-u1)
                  pp(i,j)= p0*pp(i,j)/pnorm
               else
                  p(i,j)= p0*pw
                  pp(i,j)= 0.
               endif
23011       continue
23009    continue
23013    go to 23005
c -- case 3
23014 continue
         pw= 0.07
         xw= (pw/1.6)**(5./6)
         bx= ( 1.6*1.2*xw**0.2-0.6*xw-0.9*xw**2)/( 1.6*xw**1.2-0.3*xw**2
     &-0.3*xw**3 )
         do 23015 i=1,nz
            do 23017 j=1,nr
               x= (psi(i,j)-u1)/(psi0-u1)
               if( x .gt. xw) then
                  p(i,j)= p0*( 1.6*x**1.2 - 0.3*x**2 - 0.3*x**3 )
                  pp(i,j)= p0*( 1.6*1.2*x**0.2 - 0.6*x - 0.9*x**2)/(psi0
     &-u1)
               else
                  p(i,j)= p0*( 1.6*xw**1.2 - 0.3*xw**2 - 0.3*xw**3 ) *
     &exp(bx*(x-xw))
                  pp(i,j)= p0*( 1.6*1.2*xw**0.2 - 0.6*xw - 0.9*xw**2)/(
     &psi0-u1) *exp(bx*(x-xw))
               endif
23017       continue
23015    continue
23019    go to 23005
c -- case (default)
23020 continue
         do 23021 i=1,nz
            do 23023 j=1,nr
               if( psi(i,j).lt.0. ) then
                  p(i,j)= p0*psi(i,j)/psi0
                  pp(i,j)= p0/psi0
               endif
23023       continue
23021    continue
c -- dispatch area for select
23025 go to 23005
23004 continue
      if(i23004.lt. 1 .or. i23004.gt.3) go to 23020
      go to (23006,23008,23014), i23004
23005 continue
c endselect
      return 
      end
      subroutine hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,psi,rad)
      use cm_tk
      parameter(a1=-2.21166,a2=21.1309,a3=-68.6066,a4=102.536)
      parameter(a5=-72.4683,a6=19.4894)
      real hi(nz,nr), dhi(nz,nr), psi(nz,nr), rad(nr)
      real hhp1(nz,nr), hhp2(nz,nr)
      real sigma, rf1, rf2
      f2(x)= x**2
      fp2(x)= 2*x
      f1(x)= 5*x**4 - 4*x**5
      fp1(x)= 20*x**3 - 20*x**4
       
      if( rm.ne.0. ) then
         print *, 'Error:  rm<>0. in  hdhdpsi  > exit'
         call exit(0)
      endif
      hi = 0.
      dhi = 0.
      hhp1 = 0.
      hhp2 = 0.
      zc= dz_gs*(nz-1)/2.
      do 23000 i=1,nz
         z= -zc + (i-1)*dz_gs
         do 23002 j=1,nr
            x= (psi(i,j)-u1)/(psi0-u1)
            if( x.gt.0. ) then
               hi(i,j) = hi0*sqrt( 1 + alphah1*f1(x) + alphah2*f2(x) )
               dhi(i,j)= 0.5*hi0**2*( alphah1*fp1(x) + alphah2*fp2(x) ) 
     &/(hi(i,j)*(psi0 - u1))
               hhp1(i,j)= 0.5*hi0**2*fp1(x)/(psi0 - u1)
               hhp2(i,j)= 0.5*hi0**2*fp2(x)/(psi0 - u1)
            else
               hi(i,j)= hi0*sqrt( 1 + alphah1*f1(0.) + alphah2*f2(0.) )
               dhi(i,j)= 0.
            endif
23002    continue
23000 continue
      rf1= f1(1.)
      rf2= f2(1.)
      return 
      end
      subroutine j_phii(jphi,deni,u,rad) 
      use cm_tk
      real jphi(nz,nr), u(nz,nr), deni(nz,nr), rad(nr)
      return 
      do 23000 i= 1,nz
         do 23002 j=1,nr
            rr= omegai0*( u0-u(i,j) + 0.5*omegai0*(rad(j)**2-r0**2) )/
     &ti0
            deni(i,j)= ni0*exp(rr)
            jphi(i,j)= rad(j)*omegai0*deni(i,j)
23002    continue
23000 continue
      return 
      end
      module local_flds
      real :: bl,vdg2l,vdg3l,vdc2l,vdc3l,a22l,a23l,bcbl 
      real :: psil, rbpl, re2pl, re3pl, rl 
      end module local_flds
      subroutine j_i(sf,jz,jr,jphi,deni,u,hi,rad) 
      use MPI_module
      use local_flds
      use cm_tk
      use cm_df
      parameter( eps=1.e-3 )
      real, dimension(nz,nr) :: sf, jz, jr, jphi, deni
      real, dimension(nz,nr) :: u, hi, r2d
      real, dimension(nz,nr) :: j1, j2, j3, p11, p22, p33
      real, dimension(nz,nr) :: btot, bcurlb, a22, a23
      real, dimension(nz,nr) :: vdg2, vdg3, vdc2, vdc3
      real, dimension(nz,nr) :: bz, br, bp, vz, vr, vp, tmp
      real, dimension(nz,nr) :: e2r, e2p, e3z, e3r, e3p
      real, dimension(nzd2p1, 4) :: vmomi
      real :: rad(nr), vmom(0:3)
      integer :: stat(MPI_status_size), sender
      vstar= 0.24*v0
      deltav= v0/20.
      lambda0= 0.6
      deltal= 0.3
      rl0= 0. 
      rdl= 0. 
      etaf= 0. 
      lambda0a= 0.
      deltala= 0.2 
      b0= 1.
      pminus= -psi0*0.1
      betav= 5
      vmax= v0+2*deltav
      do 23000 i= 1,nz
         r2d(i,:)= rad(:)
23000 continue
      call der_r(tmp, u)
      bz= -tmp/r2d 
      call der_z(tmp, u)
      br= tmp/r2d 
      bp= (hi + sf)/r2d 
      btot= sqrt( bz**2 + br**2 + bp**2 )
      bz= bz/btot 
      br= br/btot 
      bp= bp/btot 
      call curl_gs(vz,vr,vp,bz,br,bp,r2d) 
      bcurlb= (bz*vz + br*vr + bp*vp)/btot 
      tmp= sqrt( 1. - bz**2 )
      e2r= bp/tmp
      e2p= -br/tmp
      e3z= -tmp
      e3r= br*bz/tmp
      e3p= bp*bz/tmp
      vdc2= ( e2r*vr + e2p*vp )/btot 
      vdc3= ( e3z*vz + e3r*vr + e3p*vp )/btot
      call der_z(vz, btot) 
      call der_r(vr, btot)
      vdg2= -( e3z*vz + e3r*vr )/btot 
      vdg3= e2r*vr/btot
      a22= -( bz*vz + br*vr )/btot 
      a23= -bcurlb*btot
      call der_z(vz, bz) 
      call der_r(vr, bz)
      a22= a22 + 2*(e3z*vz + e3r*vr)/tmp
      a23= a23 - 2*e2r*vr/tmp
      jb=maxval( maxloc(rad, u((nz+1)/2,:) <= u1 ) )
      rb=maxval(rad, u((nz+1)/2,:) <= u1 )
      jb=jb+1
      rb=rad(jb)
      hb= hi((nz+1)/2,jb) + sf((nz+1)/2,jb) 
      vmom= 0.
      vmomi= 0.
      if(nprocs .lt. 2) then
         print *,'nprocs=',nprocs, '  in j_i'
         stop 1111
          call MPI_abort(MPI_comm_world, 1, ierr) 
      endif
      if(myid .gt. master) then
         do 23002 j= myid, nr, nprocs-1 
            do 23004 i=1,nz/2+1
               bl= btot(i,j)
               vdg2l= vdg2(i,j) 
               vdg3l= vdg3(i,j)
               vdc2l= vdc2(i,j) 
               vdc3l= vdc3(i,j)
               a22l= a22(i,j)
               a23l= a23(i,j)
               bcbl= bcurlb(i,j)
               psil= u(i,j)
               rbpl= rad(j)*bp(i,j)
               re2pl= rad(j)*e2p(i,j)
               re3pl= rad(j)*e3p(i,j)
               rl= rad(j)
               call vmoments(vmom) 
               vmomi(i,1:4)=vmom(0:3)
23004       continue
            call MPI_send(vmomi, nzd2p1*4, MPI_double_precision, master 
     &, j, MPI_comm_world, ierr)
23002    continue
      endif
      if(myid .eq. master) then
         do 23006 j= 1,nr
            call MPI_recv(vmomi, nzd2p1*4, MPI_double_precision , 
     &MPI_any_source , MPI_any_tag, MPI_comm_world, stat, ierr)
            jrecv= stat(MPI_tag)
            deni(1:nzd2p1,jrecv)= vmomi(:,1)
            j1(1:nzd2p1,jrecv) = vmomi(:,2)
            j2(1:nzd2p1,jrecv) = vmomi(:,3)
            j3(1:nzd2p1,jrecv) = vmomi(:,4)
23006    continue
         do 23008 i= 1, nz/2+1
            deni(nz-i+1,:)= deni(i,:) 
            j1(nz-i+1,:)= j1(i,:)
            j2(nz-i+1,:)= -j2(i,:)
            j3(nz-i+1,:)= j3(i,:)
23008    continue
      endif
      call MPI_bcast( deni, nz*nr, MPI_double_precision, master , 
     &MPI_comm_world, ierr)
      call MPI_bcast( j1, nz*nr, MPI_double_precision, master , 
     &MPI_comm_world, ierr)
      call MPI_bcast( j2, nz*nr, MPI_double_precision, master , 
     &MPI_comm_world, ierr)
      call MPI_bcast( j3, nz*nr, MPI_double_precision, master , 
     &MPI_comm_world, ierr)
      jz = j1*bz + j3*e3z
      jr = j1*br + j2*e2r + j3*e3r
      jphi= j1*bp + j2*e2p + j3*e3p
      call der_z(vz, jr)
      call der_r(vr, jz)
      tmp= r2d*(vr - vz) 
      zc= dz_gs*(nz-1)/2.
      sf= 0. 
      do 23010 l= 1,200
         call adi_tk(sf,tmp,nz,nr,zc,rad,err,omega1,omega2)
         if( err.lt.eps*maxval(abs(sf)) ) goto 1
23010 continue
      if( myid.eq.master ) print *, 'no convergence in j_i', ',   err=',
     &err
      stop
    1 continue
      if( myid.eq.master ) print *, '==============================='
      if( myid.eq.master ) print *, '# of interations in j_i=', l, 
     &'    err=',err
      if( myid.eq.master ) print *, '==============================='
      deni_max= maxval(deni)
      fdeni= ni0/deni_max
      deni= deni*fdeni
      jz = jz*fdeni
      jr = jr*fdeni
      jphi= jphi*fdeni
      sf = sf*fdeni
      return 
      end
      subroutine vmoments(vmom)
      use MPI_module
      use local_flds
      use cm_tk
      use cm_df
      parameter( eps=5.e-2, lmax=25 )
      real, dimension(4) :: ss, os, st, ost, sum, f1d
      real :: vmom(0:3)
      vmax= v0+2*deltav 
      vmom= 0.
      if( vmax .lt. (psil+pminus)/rl ) then
         return 
      endif
      ax= -vmax
      bx= vmax
      os= -1.e30
      ss= 0. 
      f1d= 0.
      st= 0.5*(bx-ax)*( ss + f1d )
      ost= st
      it= 1
      do 23000 l= 2,lmax
         tnm= it
         del= (bx-ax)/tnm
         x= ax + 0.5*del
         sum= 0.
         do 23002 ll= 1,it
            call int_2d(f1d, x)
            sum= sum + f1d
            x= x + del
23002    continue
         st= 0.5*(st + (bx-ax)*sum/tnm)
         it= 2*it
         ss= (4*st - ost)/3.
         if( count( abs(ss-os) .lt. eps*(abs(os)+0.0000000000001) ).eq.4
     & ) goto 100
         os= ss
         ost=st
23000 continue
      print *, 'too many steps, vmoments', '  myid=',myid
      stop 1111
       call MPI_abort(MPI_comm_world, 2, ierr) 
  100 continue
      vmom= ss
      return 
      end
      subroutine int_2d( f1d, v3 )
      use MPI_module
      use local_flds
      use cm_tk
      use cm_df
      parameter( eps=5.e-2, lmax=25 )
      real, dimension(4) :: ss, os, st, ost, sum, f1d, f2d
      real v3
      vmax= v0+2*deltav
      f1d= 0.
      if( abs(v3).ge.vmax ) then
         return 
      endif
      v2max= sqrt( vmax**2 - v3**2 ) 
      v2min= -v2max
      ax= v2min
      bx= v2max
      os= -1.e30
      ss= 0. 
      f2d= 0.
      st= 0.5*(bx-ax)*( ss + f2d )
      ost= st
      it= 1
      do 23000 l= 2,lmax
         tnm= it
         del= (bx-ax)/tnm
         x= ax + 0.5*del
         sum= 0.
         do 23002 ll= 1,it
            call int_1d(f2d, x, v3)
            sum= sum + f2d
            x= x + del
23002    continue
         st= 0.5*(st + (bx-ax)*sum/tnm)
         it= 2*it
         ss= (4*st - ost)/3.
         if( count( abs(ss-os) .lt. eps*(abs(os)+0.0000000000001) ).eq.4
     & ) goto 100
         os= ss
         ost=st
23000 continue
      print *, 'too many steps, int_2d', '  myid=',myid
      print *, 'maxval(abs(os))=',maxval(abs(os))
      stop 1111
       call MPI_abort(MPI_comm_world, 3, ierr) 
  100 continue
      f1d= ss
      return 
      end
      subroutine int_1d( f2d, v2, v3 )
      use MPI_module
      use local_flds
      use cm_tk
      use cm_df
      parameter( eps=3.e-3, lmax=20 )
      real, dimension(4) :: f2d
      real, dimension(2) :: ss, os, st, ost, sum, dss
      real, dimension(2) :: df, vec
      real v1, v2, v3
      real mu, mu0, lambda, pphi, v, func
      step(xa)= 0.5 + sign(0.5,xa)
      ff1(xa)= exp( -step(xa-v0)*(xa-v0)**2/deltav**2 )
      f1(xa)= ff1(xa)/(xa**3 + vstar**3) *(1./(r0*xa-psi0-pminus)**betav
     &)
      fl0(ya)= lambda0*( 1 - rl0*(1-ya/v0)*step(v0-ya) ) 
      fdl(ya)= deltal*( 1 + rdl*(1-ya/v0)*step(v0-ya) ) 
      f20(xa,ya)= exp( -( (xa - fl0(ya))/fdl(ya) )**2 )
      f2a(xa) = exp( -( (xa -lambda0a)/deltala )**2 )
      f2(xa,ya)= f20(xa,ya) + etaf*f2a(xa)
      f3(xa)= step(xa-pminus)*(xa-pminus)**betav
      vmax= v0+2*deltav
      f2d= 0.
      if( (v2**2+v3**2) .ge. vmax**2 ) then
         return 
      endif
      v1max= sqrt( vmax**2 - v2**2 - v3**2 ) 
      v1min= -v1max
      pphi_min1= pminus
      v1mm= (psil + pphi_min1 - v2*re2pl - v3*re3pl)/rbpl
      if( rbpl .gt. 0. ) then
         v1min= max( v1min, v1mm ) 
      else
         v1max= min( v1max, v1mm ) 
      endif
      if( v1min .ge. v1max ) then
         return 
      endif
      vperp2= v2**2 + v3**2
      mu0= 0.5*vperp2/bl
      dmu2= ( (v2**2 - v3**2)*a23l - 2*v2*v3*a22l )/(2*bl)**2
      vec(1)= 1.
      ax= v1min
      bx= v1max
      os= -1.e30
      v1= ax
      vpl2= v1**2
      v= sqrt( vpl2 + vperp2 ) 
      vd2= mu0*vdg2l + vpl2*vdc2l
      vd3= mu0*vdg3l + vpl2*vdc3l
      mu= 0.5*( (v2-vd2)**2 + (v3-vd3)**2 )/bl + v1*dmu2
      mu= mu*( 1 - v1*bcbl )
      lambda= 1.
      if( v .gt. 1.e-4 ) lambda= 2*mu*b0/v**2
      pphi= -psil + v1*rbpl + v2*re2pl + v3*re3pl 
      func= f1(v)*f2(lambda,v)*f3(pphi)*v0**3
      vec(2)= v1
      ss= vec*func
      v1= bx
      vpl2= v1**2
      v= sqrt( vpl2 + vperp2 ) 
      vd2= mu0*vdg2l + vpl2*vdc2l
      vd3= mu0*vdg3l + vpl2*vdc3l
      mu= 0.5*( (v2-vd2)**2 + (v3-vd3)**2 )/bl + v1*dmu2
      mu= mu*( 1 - v1*bcbl )
      lambda= 1.
      if( v .gt. 1.e-4 ) lambda= 2*mu*b0/v**2
      pphi= -psil + v1*rbpl + v2*re2pl + v3*re3pl 
      func= f1(v)*f2(lambda,v)*f3(pphi)*v0**3
      vec(2)= v1
      df= vec*func
      st= 0.5*(bx-ax)*( ss + df )
      ost= st
      it= 1
      do 23000 l= 2,lmax
         tnm= it
         del= (bx-ax)/tnm
         x= ax + 0.5*del 
         sum= 0.
         do 23002 ll= 1,it
            v1= x
            vpl2= v1**2
            v= sqrt( vpl2 + vperp2 ) 
            vd2= mu0*vdg2l + vpl2*vdc2l
            vd3= mu0*vdg3l + vpl2*vdc3l
            mu= 0.5*( (v2-vd2)**2 + (v3-vd3)**2 )/bl + v1*dmu2
            mu= mu*( 1 - v1*bcbl )
            lambda= 1.
            if( v .gt. 1.e-4 ) lambda= 2*mu*b0/v**2
            pphi= -psil + v1*rbpl + v2*re2pl + v3*re3pl 
            func= f1(v)*f2(lambda,v)*f3(pphi)*v0**3
            vec(2)= v1
            df= vec*func
            sum= sum + df
            x= x + del
23002    continue
         st= 0.5*(st + (bx-ax)*sum/tnm)
         it= 2*it
         ss= (4*st - ost)/3.
         if( count( abs(ss-os) .lt. eps*(abs(os)+0.0000000000001) ).eq.2
     & ) goto 100
         if( func .lt. 1.e-7 ) goto 100 
         dss=ss-os
         os= ss
         ost=st
23000 continue
      print *, 'too many steps, int_1d, os=',os, '   ss-os',dss
      print *, 'f2=',lambda,f2(lambda,v),func
      stop 1111
       call MPI_abort(MPI_comm_world, 4, ierr) 
  100 continue
      f2d(1)= ss(1) 
      f2d(2)= ss(2) 
      f2d(3)= v2*ss(1) 
      f2d(4)= v3*ss(1) 
      contains
      function erf(x) result(errorf)
      real, intent(in) :: x
      real :: errorf, erfcc, z, t
      z=abs(x)
      t=1./(1+0.5*z)
      erfcc=t*exp( -z**2 -1.26551223 +t*( 1.00002368 + t*( .37409196 + t
     &*( .09678418 + t*( -.18628806 + t*( .27886807 + t*( -1.13520398 + 
     &t*( 1.48851587 + t*( -.82215223 + t*0.17087277 ) ) ) ) ) ) ) ) )
      errorf= 1.-erfcc
      if(x < 0.) errorf=-errorf
      end function erf
      end
      subroutine psi_tk_bc(psie,zc,rc,rad)
      use cm_tk
      real psie(nz,nr), psiv(nz,nr), rad(nr)
      dz= 2*zc/(nz-1)
      betap= 1.0
      rli= 0.7
      riphi_c=66.
      bvert0= riphi_c/r0*( log(8.*r0/zc) - 1.5 + betap + 0.5*rli )
      bvert= bv*bvert0/(4.*3.1415927)
      ivert= 2 
c select
      i23000= ivert 
         go to 23000
c -- case 1
23002 continue
         bda= 0.25*(1-rmir)/zc**2
         a= 0.5*bvert
         b= bda*a
         do 23003 i= 1,nz 
            z= -zc + (i-1)*dz
            do 23005 j= 1,nr
               r2= ( rad(j) )**2
               psiv(i,j)= ( a + b*(r2 - 4*z**2) )*r2
23005       continue
23003    continue
23007    go to 23001
c -- case 2
23008 continue
         a= 0.5*bvert
         bda= 0.25*(1-rmir)/zc**2
         bvert1=1.80*bvert0/(4.*3.1415927)
         b= bda*0.5*bvert1
         do 23009 i= 1,nz 
            z= -zc + (i-1)*dz
            do 23011 j= 1,nr
               r2= ( rad(j) )**2
               psiv(i,j)= ( a + b*(r2 - 4*z**2) )*r2
23011       continue
23009    continue
23013    go to 23001
c -- case 3
23014 continue
         a= 0.5*(rmir + 1)
         ak= 3.1415927/zc
         ua= 0.5*bvert*rc**2
         do 23015 i= 1,nz
            z= -zc + (i-1)*dz
            psir0= ua/( 0.5*a*rc**2 - (a-1)*rc/ak*BESSI1(ak*rc) )
            do 23017 j= 1,nr
               rr= rad(j)
               psiv(i,j)= psir0 *( 0.5*a*rr**2 - (a-1)*rr/ak*BESSI1(ak*
     &rr)*cos(ak*z) )
23017       continue
23015    continue
23019    go to 23001
c -- case (default)
23020 continue
         bda= 0.25*(1-rmir)/zc**2
         a= 0.5*bvert
         b= bda*a
         do 23021 i= 1,nz
            z= -zc + (i-1)*dz
            do 23023 j= 1,nr
               r2= ( rad(j) )**2
               psiv(i,j)= ( a + b*(r2 - 4*z**2) )*r2
23023       continue
23021    continue
c -- dispatch area for select
23025 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23020
      go to (23002,23008,23014), i23000
23001 continue
c endselect
      rax= r0
      rax= 23.0
      do 23026 i= 1,nz 
         z= -zc + (i-1)*dz
         do 23028 j= 1,nr
            r2= ( rad(j) )**2
            rrz= sqrt( (rax+rad(j))**2 + z**2 )
            rkk= sqrt( 4.*rax*rad(j) )/rrz
            rkk1= sqrt( 1. - rkk**2 )
            if( rkk.eq.1. ) then
               psie(i,j)= -1.
            else
               ekk= CEL(rkk1,1.,1.,1.) 
               eek= CEL(rkk1,1.,1.,rkk1**2) 
               psie(i,j)= - 2*riphi*rrz*( (1-0.5*rkk**2)*ekk-eek )
               psie(i,j)= psie(i,j)/(4.*3.1415927)
            endif
23028    continue
23026 continue
      za= zc/5.0
      do 23030 i= 1,nz
         z= -zc + (i-1)*dz
         do 23032 j= 1,nr
            r2= ( rad(j) )**2
            psie_c= psie(i,j)
            rrz= sqrt( (rax+rad(j))**2 + (z-za)**2 )
            rkk= sqrt( 4.*rax*rad(j) )/rrz
            rkk1= sqrt( 1. - rkk**2 )
            if( rkk.eq.1. ) then
               psie_r= -1.
            else
               ekk= CEL(rkk1,1.,1.,1.) 
               eek= CEL(rkk1,1.,1.,rkk1**2) 
               psie_r= - 2*riphi*rrz*( (1-0.5*rkk**2)*ekk-eek )
               psie_r= psie_r/(4.*3.1415927)
            endif
            rrz= sqrt( (rax+rad(j))**2 + (z+za)**2 )
            rkk= sqrt( 4.*rax*rad(j) )/rrz
            rkk1= sqrt( 1. - rkk**2 )
            if( rkk.eq.1. ) then
               psie_l= -1.
            else
               ekk= CEL(rkk1,1.,1.,1.) 
               eek= CEL(rkk1,1.,1.,rkk1**2) 
               psie_l= - 2*riphi*rrz*( (1-0.5*rkk**2)*ekk-eek )
               psie_l= psie_l/(4.*3.1415927)
            endif
            psie(i,j)= 0.4*psie_c + 0.30*psie_r + 0.30*psie_l
            psie(i,j)= psie(i,j) + psiv(i,j)
23032    continue
23030 continue
      rtmp= psie(1,1)
      do 23034 i= 1,nz
         psie(i,1)= rtmp + 0.3*( psie(i,1) - rtmp )
23034 continue
      icenter= nz/2 + 1
      psie_c= psie(icenter,1)
      do 23036 i= 1,nz
         do 23038 j= 1,nr
            psie(i,j)= psie(i,j) - psie_c
23038    continue
23036 continue
      return 
      end
      subroutine adi_tk(u,g,nz,nr,zc,rad,err,omega1,omega2)
      real u(nz,nr), uu(nz,nr), g(nz,nr)
      real rad(nr), bz(nz), br(nr)
      real a1(nz),a2(nz),a3(nz)
      real aa1(nr),aa2(nr),aa3(nr)
      real dz,dr,omega1,omega2
      real fm,fp
      err= 0.
      dz= 2*zc/(nz-1)
      dr= (rad(nr)-rad(1))/(nr-1)
      do 23000 i=1,nz
         do 23002 j=1,nr
            uu(i,j)= u(i,j)
23002    continue
23000 continue
      dzdr2= (dz/dr)**2
      do 23004 j=2,nr-1
         do 23006 i=2,nz-1 
            ii= i-1
            a1(ii)= 1.
            a2(ii)= -2.-omega1
            a3(ii)= 1.
23006    continue
         fm= dzdr2*rad(j)/(rad(j)-0.5*dr)
         fp= dzdr2*rad(j)/(rad(j)+0.5*dr)
         do 23008 i=2,nz-1
            ii= i-1
            bz(ii)= - fm*u(i,j-1) + (fm+fp-omega1)*u(i,j) - fp*u(i,j+1) 
     &+ dz**2*g(i,j)
23008    continue
         bz(1)= bz(1) - u(1,j) 
         bz(nz-2)= bz(nz-2) - u(nz,j)
         call THOMAS(3,a1,a2,a3,bz,nz-2) 
         do 23010 i=2,nz-1
            ii= i-1
            uu(i,j)= bz(ii)
23010    continue
23004 continue
      drdz2= 1./dzdr2
      do 23012 i=2,nz-1 
         do 23014 j=2,nr-1
            jj= j-1
            br(jj)= - drdz2*uu(i-1,j) + ( 2.*drdz2-omega2 )*uu(i,j) - 
     &drdz2*uu(i+1,j) + dr**2*g(i,j)
23014    continue
         br(1)= br(1) - uu(i,1)*rad(2)/( rad(2)-0.5*dr )
         br(nr-2)= br(nr-2) - uu(i,nr)*rad(nr-1)/( rad(nr-1)+0.5*dr )
         do 23016 j=2,nr-1
            aa1(j-1)= rad(j)/( rad(j)-0.5*dr )
            aa3(j-1)= rad(j)/( rad(j)+0.5*dr )
            aa2(j-1)= ( - aa1(j-1) - aa3(j-1) - omega2 )
23016    continue
         call THOMAS(3,aa1,aa2,aa3,br,nr-2) 
         do 23018 j=2,nr-1
            utmp= br(j-1)
            err= amax1( err, abs(u(i,j)-utmp) )
            u(i,j)= utmp
23018    continue
23012 continue
      return 
      end
      subroutine der_z( ao, ai )
      use cm_tk
      real ao(nz,nr), ai(nz,nr), atmp(nz,nr)
      real ddz(-2:2)
      atmp= 0.
      nzm= nz-1
      nzm2= nz-2
      oddz12= 1./(12.*dz_gs)
      ddz(-2)= oddz12
      ddz(-1)= -8.*oddz12
      ddz(0) = 0.
      ddz(1) = 8.*oddz12
      ddz(2) = -oddz12
      do 23000 j=1,nr 
         do 23002 i= 3,nzm2
            atmp(i,j)= ddz(-1)*ai(i-1,j) +ddz( 1)*ai(i+1,j) +ddz(-2)*ai(
     &i-2,j) +ddz( 2)*ai(i+2,j)
23002    continue
23000 continue
      atmp(2,:)= ( ai(3,:) - ai(1,:) )/(2*dz_gs) 
      atmp(nzm,:)= ( ai(nz,:) - ai(nzm2,:) )/(2*dz_gs)
      atmp(1,:)= ( ai(2,:) - ai(1,:) )/dz_gs
      atmp(nz,:)= ( ai(nz,:) - ai(nzm,:) )/dz_gs
      ao= atmp
      return 
      end
      subroutine der_r( ao, ai )
      use cm_tk
      real ao(nz,nr), ai(nz,nr), atmp(nz,nr)
      real ddr(-2:2)
      atmp= 0.
      nrm= nr-1
      nrm2= nr-2
      oddrr12= 1./(12.*dr_gs)
      ddr(-2)= oddrr12
      ddr(-1)= -8.*oddrr12
      ddr(0) = 0.
      ddr(1) = 8.*oddrr12
      ddr(2) = -oddrr12
      do 23000 i= 1,nz
         do 23002 j=3,nrm2
            atmp(i,j)= ddr(-1)*ai(i,j-1) +ddr( 1)*ai(i,j+1) +ddr(-2)*ai(
     &i,j-2) +ddr( 2)*ai(i,j+2)
23002    continue
23000 continue
      atmp(:,2)= ( ai(:,3) - ai(:,1) )/(2*dr_gs) 
      atmp(:,nrm)= ( ai(:,nr) - ai(:,nrm2) )/(2*dr_gs)
      atmp(:,1)= ( ai(:,2) - ai(:,1) )/dr_gs
      atmp(:,nr)= ( ai(:,nr) - ai(:,nrm) )/dr_gs
      ao= atmp
      return 
      end
      subroutine curl_gs(voz,vor,vophi,viz,vir,viphi,r2d)
      use cm_tk
      real voz(nz,nr), vor(nz,nr), vophi(nz,nr)
      real viz(nz,nr), vir(nz,nr), viphi(nz,nr)
      real tmp(nz,nr), r2d(nz,nr)
      tmp= r2d*viphi 
      call der_r(voz, tmp)
      voz= voz/r2d
      call der_z(vor, viphi) 
      vor= -vor
      call der_z(vophi, vir) 
      call der_r(tmp, viz)
      vophi= vophi - tmp
      return 
      end
      subroutine initpart 
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_part 
      use cm_spec
      use cm_pgrid
      use cm_tk
      use cm_df
      dimension sppsi(ni,nx,ny) 
      real, dimension(ni,nj,nk) :: stmp,sden
      parameter( frmin= 0.30, frmax= 1.00, fzmin= 0.7, fzmax= 0.7 )
      parameter(nrg=200, nzg=400, nvg=300)
      dimension rrd(nrg), zd(nzg,nj), vd(ni,nj,nvg)
      dimension psen(np)
      rmin= frmin*rhi
      rmax= frmax*rhi
      zmin= fzmin*qlo
      zmax= fzmax*qhi
      rmin2= rmin**2
      rmax2= rmax**2
      call maptop_s( sppsi, spsi ) 
      call nstx_load( sden, vd, nvg )
      call inversed( rrd, zd, nrg, nzg, sden )
      dd1d= 1./(nrg-1)
      dd2d= 1./(nzg-1)
      dvd= 1./(nvg-1)
      do 23000 isp = 1, nsp
         tsp= vthplsp(isp)**2/2.
         mm= 0
         do 23002 m = mfsp(isp), mlsp(isp) 
             
  300       continue
            d1dm= ran2(idum) 
            rj= d1dm/dd1d + 1.
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            rm2= fjj*rrd(jj) + fjp*rrd(jp)
            rrm= sqrt(rm2)
            d2dm= ran2(idum) 
            ri= d2dm/dd2d + 1.
            rj= ( rrm - r(1) )*dri + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            zzm= fjj*( fii*zd(ii,jj) + fip*zd(ip,jj) ) + fjp*( fii*zd(ii
     &,jp) + fip*zd(ip,jp) )
            sm= rpi2*ran2(idum) 
            pv1x1(m)= zzm
            pv2x1(m)= x0 + rrm*cos(sm)
            pv3x1(m)= y0 + rrm*sin(sm)
            ri= ( pv1x1(m) -q(1) )*dqi + 1.
            rj= ( pv2x1(m) -x(1) )*dxi + 1.
            rk= ( pv3x1(m) -y(1) )*dyi + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            kk= rk
            kp= kk + 1
            fkp= rk - kk
            fkk= 1. - fkp
            psim = ( fkk*  ( fjj*( fii*sppsi(ii,jj,kk) +fip*sppsi(ip,jj,
     &kk) ) + fjp*( fii*sppsi(ii,jp,kk) +fip*sppsi(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*sppsi(ii,jj,kp) +fip*sppsi(ip,jj,kp) ) + fjp*( fii*
     &sppsi(ii,jp,kp) +fip*sppsi(ip,jp,kp) ) ) )
            vdm= ran2(idum) 
            rk= vdm/dvd + 1.
            ri= ( zzm - q(1) )*dqi + 1.
            rj= ( rrm - r(1) )*dri + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            kk= rk
            kp= kk + 1
            fkp= rk - kk
            fkk= 1. - fkp
            vm= fkk*( fjj*( fii*vd(ii,jj,kk) + fip*vd(ip,jj,kk) ) + fjp*
     &( fii*vd(ii,jp,kk) + fip*vd(ip,jp,kk) ) ) + fkp*( fjj*( fii*vd(ii,
     &jj,kp) + fip*vd(ip,jj,kp) ) + fjp*( fii*vd(ii,jp,kp) + fip*vd(ip,
     &jp,kp) ) )
            vmax= v0+2*deltav
            vpsi= (psim+pminus)/rrm
            vmin= max(0., vpsi)
            if( vmin.ge.vmax ) goto 300
            if( vm.le.vmin .or. vm.ge.vmax ) goto 300
            dvphi= ran2(idum) 
            betavp= betav+1
            if( (-vm) .le. vpsi ) then
               vphim= vpsi + (vm-vpsi)*dvphi**(1./betavp)
            else
               vphim= vpsi + ( (1-dvphi)*(-vm-vpsi)**betavp + dvphi*(vm-
     &vpsi)**betavp )**(1./betavp)
            endif
            if( abs(vphim).gt.vm ) stop 8811
            pspphi(m)= rrm*vphim - psim 
            psen(m)= 0.5*vm**2 
            psr(m)= rrm 
            vp2= vm**2 - vphim**2 
            vp= 0.
            if( vp2.gt.0. ) vp= sqrt(vp2)
            thetam= rpi2*ran2(idum)
            vzm= vp*cos(thetam)
            vrm= vp*sin(thetam)
            pv1v1(m)= vzm 
            pv2v1(m)= vrm*cos(sm) - vphim*sin(sm) 
            pv3v1(m)= vrm*sin(sm) + vphim*cos(sm) 
            vzampl= 0.
            if(psim .lt. psi0/4.) vzampl= vpert *((psim-psi0/4.)/psi0)/6
     &.*cos(sm)
            psw(m)= 0.
            psp(m)= 1.
23002    continue
23000 continue
      if( myid.eq.master ) print *, '***************************'
      if( myid.eq.master ) print *, 'min/max(zm)=',minval(pv1x1(1:np)),
     &maxval(pv1x1(1:np))
      if( myid.eq.master ) print *, '***************************'
      call p_weight2 
      wsum= 0.
      do 23004 m= 1,np
         wsum= wsum + psp(m)
23004 continue
      do 23006 m= 1,np
         psp(m)= psp(m)*np/wsum
23006 continue
      stmp = srhoq0 * sh3w
      call sum_rs( qtot, stmp )
      qtot= qtot*dr*ds/dx/dy 
      if( myid.eq.master ) print *, qsp(1),qtot/np
      do 23008 isp= 1,nsp
         qsp(isp)= qtot/np
         wsp(isp)= qsp(isp)*wdqsp(isp)
23008 continue
      call frompart
      rmax0= 0.
      rmax1= 0.
      do 23010 k= 1,nk
         do 23012 j= 1,nj
            do 23014 i= 1,ni
               if( srhoq0(i,j,k) .gt. rmax0 ) rmax0= srhoq0(i,j,k)
               if( srhoq(i,j,k) .gt. rmax1 ) rmax1= srhoq(i,j,k)
23014       continue
23012    continue
23010 continue
      if( myid.eq.master ) print *, 
     &'========Check initial loading========='
      if( myid.eq.master ) print *, 'rmax0=', rmax0,'   rmax1=',rmax1
      if( myid.eq.master ) print *, 'delta_n=', maxval(abs(srhoq0(:,:,3)
     &-srhoq(:,:,3)))
      do 23016 isp= 1,nsp
         qsp(isp)= qsp(isp)*rmax0/rmax1
         wsp(isp)= qsp(isp)*wdqsp(isp)
23016 continue
      call frompart
      if( myid.eq.master ) print *, 'delta_n=', maxval(abs(srhoq0(:,:,3)
     &-srhoq(:,:,3)))
      if( myid.eq.master ) print *, 
     &'========================================'
      j3=3
      if( ibstyp(1,2,16 ).eq.7 ) j3=1 
      open(unit=15,file='rhoi.dat')
      write(15,*) 'rhoi'
      write(15,9) nid, njm2-j3+1
      write(15,10) (q(i),i=3,nim2),(r(j),j=j3,njm2)
      write(15,10) (( srhoq(i,j,6),i=3,nim2),j=j3,njm2)
      write(15,10) ((v3curi(i,j,6),i=3,nim2),j=j3,njm2)
      write(15,10) ((v1curi(i,j,6),i=3,nim2),j=j3,njm2)
      write(15,10) ((v2curi(i,j,6),i=3,nim2),j=j3,njm2)
    9 format(2i5)
   10 format(5es12.3)
      close(15)
      if(myid .eq. master) then
         open( unit=18,file='en.dat') 
         write(18,*) np
         write(18,'(5es12.3)') (psen(m), m=1,np)
         close(18)
         do 23018 m= 1,np 
            if( psen(m).eq.0. ) then
               psen(m)= 1.
            else
               psen(m)= psmu(m)*b0/psen(m)
            endif
23018    continue
         open( unit=19,file='ff.dat') 
         write(19,*) np
         write(19,'(5es12.3)') (psen(m), m=1,np),(pspphi(m), m=1,np) ,(
     &psr(m), m=1,np)
         close(19)
         open( unit=20,file='psp.dat') 
         write(20,*) np
         write(20,'(5es12.3)') (psp(m), m=1,np)
         close(20)
      endif
      return 
      end
      subroutine p_weight2 
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_part 
      use cm_spec
      use cm_pgrid
      use cm_tk
      use cm_df
      real, dimension(ni,nj,nk) :: stmp1, stmp3
      real, dimension(ni,nj,nk) :: v1d1,v2d1,v3d1,v1d2,v2d2,v3d2,v1tmp,
     &v2tmp,v3tmp
      real, allocatable, dimension(:,:,:) :: sptmp1,sptmp3
      real, allocatable, dimension(:,:,:) :: vp1b,vp2b,vp3b
      real, allocatable, dimension(:,:,:) :: vp1d1,vp2d1,vp3d1
      real, allocatable, dimension(:,:,:) :: vp1d2,vp2d2,vp3d2
      step(xa)= 0.5 + sign(0.5,xa)
      ff1(xa)= exp( -step(xa-v0)*(xa-v0)**2/deltav**2 )
      f1(xa)= ff1(xa)/(xa**3 + vstar**3) *(1./(r0*xa-psi0-pminus)**betav
     &)
      fl0(ya)= lambda0*( 1 - rl0*(1-ya/v0)*step(v0-ya) ) 
      fdl(ya)= deltal*( 1 + rdl*(1-ya/v0)*step(v0-ya) ) 
      f20(xa,ya)= exp( -( (xa - fl0(ya))/fdl(ya) )**2 )
      f2a(xa) = exp( -( (xa -lambda0a)/deltala )**2 )
      f2(xa,ya)= f20(xa,ya) + etaf*f2a(xa)
      f3(xa)= step(xa-pminus)*(xa-pminus)**betav
      allocate( sptmp1(ni,nx,ny), sptmp3(ni,nx,ny) )
      allocate( vp1b(ni,nx,ny), vp2b(ni,nx,ny), vp3b(ni,nx,ny) )
      allocate( vp1d1(ni,nx,ny), vp2d1(ni,nx,ny), vp3d1(ni,nx,ny) )
      allocate( vp1d2(ni,nx,ny), vp2d2(ni,nx,ny), vp3d2(ni,nx,ny) )
      stmp3= sqrt( v1b1**2 + v2b1**2 + v3b1**2 ) 
      v1d1 = v1b1 / stmp3 
      v2d1 = v2b1 / stmp3 
      v3d1 = v3b1 / stmp3 
      call curl_vv( v1d2,v2d2,v3d2, v1d1,v2d1,v3d1, 21 ) 
      stmp1 = v1d1 * v1d2 + v2d1 * v2d2 + v3d1 * v3d2 
      v1d1 = v1d1 * stmp1 
      v2d1 = v2d1 * stmp1 
      v3d1 = v3d1 * stmp1 
      v1d2 = v1d2 - v1d1
      v2d2 = v2d2 - v2d1
      v3d2 = v3d2 - v3d1 
      v1d2 = v1d2 / stmp3 
      v2d2 = v2d2 / stmp3 
      v3d2 = v3d2 / stmp3 
      stmp1 = stmp1 / stmp3
      call grad_vs( v1d1,v2d1,v3d1, stmp3, 18 )
      call cros_vvv( v1tmp,v2tmp,v3tmp, v1b1,v2b1,v3b1, v1d1,v2d1,v3d1 )
      v1d1 = v1tmp / stmp3 
      v2d1 = v2tmp / stmp3 
      v3d1 = v3tmp / stmp3 
      v1d1 = v1d1 / stmp3 
      v2d1 = v2d1 / stmp3 
      v3d1 = v3d1 / stmp3 
      call maptop_s( sptmp1, stmp1 )
      call maptop_v( vp1b, vp2b, vp3b, v1b1,v2b1,v3b1 )
      call maptop_v( vp1d1, vp2d1, vp3d1, v1d1,v2d1,v3d1 )
      call maptop_v( vp1d2, vp2d2, vp3d2, v1d2,v2d2,v3d2 )
      rlo2= rlo**2
      rhi2= rhi**2
      mout= 0
      do 23000 isp= 1, nsp
         do 23002 m= mfsp(isp), mlsp(isp)
            ri= ( pv1x1(m) -q(1) )*dqi + 1.
            rj= ( pv2x1(m) -x(1) )*dxi + 1.
            rk= ( pv3x1(m) -y(1) )*dyi + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            kk= rk
            kp= kk + 1
            fkp= rk - kk
            fkk= 1. - fkp
            bz = ( fkk*  ( fjj*( fii*vp1b(ii,jj,kk) +fip*vp1b(ip,jj,kk) 
     &) + fjp*( fii*vp1b(ii,jp,kk) +fip*vp1b(ip,jp,kk) ) ) + fkp*  ( fjj
     &*( fii*vp1b(ii,jj,kp) +fip*vp1b(ip,jj,kp) ) + fjp*( fii*vp1b(ii,jp
     &,kp) +fip*vp1b(ip,jp,kp) ) ) )
            bx = ( fkk*  ( fjj*( fii*vp2b(ii,jj,kk) +fip*vp2b(ip,jj,kk) 
     &) + fjp*( fii*vp2b(ii,jp,kk) +fip*vp2b(ip,jp,kk) ) ) + fkp*  ( fjj
     &*( fii*vp2b(ii,jj,kp) +fip*vp2b(ip,jj,kp) ) + fjp*( fii*vp2b(ii,jp
     &,kp) +fip*vp2b(ip,jp,kp) ) ) )
            by = ( fkk*  ( fjj*( fii*vp3b(ii,jj,kk) +fip*vp3b(ip,jj,kk) 
     &) + fjp*( fii*vp3b(ii,jp,kk) +fip*vp3b(ip,jp,kk) ) ) + fkp*  ( fjj
     &*( fii*vp3b(ii,jj,kp) +fip*vp3b(ip,jj,kp) ) + fjp*( fii*vp3b(ii,jp
     &,kp) +fip*vp3b(ip,jp,kp) ) ) )
            v11m = ( fkk*  ( fjj*( fii*vp1d1(ii,jj,kk) +fip*vp1d1(ip,jj,
     &kk) ) + fjp*( fii*vp1d1(ii,jp,kk) +fip*vp1d1(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp1d1(ii,jj,kp) +fip*vp1d1(ip,jj,kp) ) + fjp*( fii*
     &vp1d1(ii,jp,kp) +fip*vp1d1(ip,jp,kp) ) ) )
            v21m = ( fkk*  ( fjj*( fii*vp2d1(ii,jj,kk) +fip*vp2d1(ip,jj,
     &kk) ) + fjp*( fii*vp2d1(ii,jp,kk) +fip*vp2d1(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp2d1(ii,jj,kp) +fip*vp2d1(ip,jj,kp) ) + fjp*( fii*
     &vp2d1(ii,jp,kp) +fip*vp2d1(ip,jp,kp) ) ) )
            v31m = ( fkk*  ( fjj*( fii*vp3d1(ii,jj,kk) +fip*vp3d1(ip,jj,
     &kk) ) + fjp*( fii*vp3d1(ii,jp,kk) +fip*vp3d1(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp3d1(ii,jj,kp) +fip*vp3d1(ip,jj,kp) ) + fjp*( fii*
     &vp3d1(ii,jp,kp) +fip*vp3d1(ip,jp,kp) ) ) )
            v12m = ( fkk*  ( fjj*( fii*vp1d2(ii,jj,kk) +fip*vp1d2(ip,jj,
     &kk) ) + fjp*( fii*vp1d2(ii,jp,kk) +fip*vp1d2(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp1d2(ii,jj,kp) +fip*vp1d2(ip,jj,kp) ) + fjp*( fii*
     &vp1d2(ii,jp,kp) +fip*vp1d2(ip,jp,kp) ) ) )
            v22m = ( fkk*  ( fjj*( fii*vp2d2(ii,jj,kk) +fip*vp2d2(ip,jj,
     &kk) ) + fjp*( fii*vp2d2(ii,jp,kk) +fip*vp2d2(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp2d2(ii,jj,kp) +fip*vp2d2(ip,jj,kp) ) + fjp*( fii*
     &vp2d2(ii,jp,kp) +fip*vp2d2(ip,jp,kp) ) ) )
            v32m = ( fkk*  ( fjj*( fii*vp3d2(ii,jj,kk) +fip*vp3d2(ip,jj,
     &kk) ) + fjp*( fii*vp3d2(ii,jp,kk) +fip*vp3d2(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp3d2(ii,jj,kp) +fip*vp3d2(ip,jj,kp) ) + fjp*( fii*
     &vp3d2(ii,jp,kp) +fip*vp3d2(ip,jp,kp) ) ) )
            v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2 
            bm= sqrt(bz**2 + bx**2 + by**2) 
            b1m= bz/bm
            b2m= bx/bm
            b3m= by/bm
            vpl= pv1v1(m)*b1m + pv2v1(m)*b2m + pv3v1(m)*b3m
            vpl2= vpl**2
            psmu(m)= 0.5*(v2 - vpl2)/bm 
            vd1= psmu(m)*v11m + vpl2*v12m
            vd2= psmu(m)*v21m + vpl2*v22m
            vd3= psmu(m)*v31m + vpl2*v32m
            vv2= (pv1v1(m)-vd1)**2 + (pv2v1(m)-vd2)**2 + (pv3v1(m)-vd3)*
     &*2 
            psmu(m)= 0.5*(vv2 - vpl2)/bm 
            v1m= pv1v1(m)-vpl*b1m 
            v2m= pv2v1(m)-vpl*b2m
            v3m= pv3v1(m)-vpl*b3m
            rho1= (b2m*v3m-b3m*v2m)/bm 
            rho2= (b3m*v1m-b1m*v3m)/bm
            rho3= (b1m*v2m-b2m*v1m)/bm
            rm2= (pv2x1(m) - rho2 - x0)**2 + (pv3x1(m) - rho3 - y0)**2
            if( rm2 .lt. rlo2 .or. rm2 .gt. rhi2 ) then
               rho2= 0.
               rho3= 0.
               mout= mout+1
            endif
            ri= ( pv1x1(m) -rho1 -q(1) )*dqi + 1. 
            rj= ( pv2x1(m) -rho2 -x(1) )*dxi + 1. 
            rk= ( pv3x1(m) -rho3 -y(1) )*dyi + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            kk= rk
            kp= kk + 1
            fkp= rk - kk
            fkk= 1. - fkp
            bh1 = ( fkk*  ( fjj*( fii*vp1b(ii,jj,kk) +fip*vp1b(ip,jj,kk)
     & ) + fjp*( fii*vp1b(ii,jp,kk) +fip*vp1b(ip,jp,kk) ) ) + fkp*  ( 
     &fjj*( fii*vp1b(ii,jj,kp) +fip*vp1b(ip,jj,kp) ) + fjp*( fii*vp1b(ii
     &,jp,kp) +fip*vp1b(ip,jp,kp) ) ) )
            bh2 = ( fkk*  ( fjj*( fii*vp2b(ii,jj,kk) +fip*vp2b(ip,jj,kk)
     & ) + fjp*( fii*vp2b(ii,jp,kk) +fip*vp2b(ip,jp,kk) ) ) + fkp*  ( 
     &fjj*( fii*vp2b(ii,jj,kp) +fip*vp2b(ip,jj,kp) ) + fjp*( fii*vp2b(ii
     &,jp,kp) +fip*vp2b(ip,jp,kp) ) ) )
            bh3 = ( fkk*  ( fjj*( fii*vp3b(ii,jj,kk) +fip*vp3b(ip,jj,kk)
     & ) + fjp*( fii*vp3b(ii,jp,kk) +fip*vp3b(ip,jp,kk) ) ) + fkp*  ( 
     &fjj*( fii*vp3b(ii,jj,kp) +fip*vp3b(ip,jj,kp) ) + fjp*( fii*vp3b(ii
     &,jp,kp) +fip*vp3b(ip,jp,kp) ) ) )
            bcbm = ( fkk*  ( fjj*( fii*sptmp1(ii,jj,kk) +fip*sptmp1(ip,
     &jj,kk) ) + fjp*( fii*sptmp1(ii,jp,kk) +fip*sptmp1(ip,jp,kk) ) ) + 
     &fkp*  ( fjj*( fii*sptmp1(ii,jj,kp) +fip*sptmp1(ip,jj,kp) ) + fjp*(
     & fii*sptmp1(ii,jp,kp) +fip*sptmp1(ip,jp,kp) ) ) )
            bm_gc= sqrt(bh1**2 + bh2**2 + bh3**2) 
            b1m= bh1/bm_gc
            b2m= bh2/bm_gc
            b3m= bh3/bm_gc
            vdotb= (v1m*b1m+v2m*b2m+v3m*b3m)
            rmu2= 0.5*vpl*vdotb/bm_gc
            psmu(m)= psmu(m) - rmu2
            psmu(m)= psmu(m)*( 1 - 0.5*vpl*bcbm ) 
  400       continue
23002    continue
23000 continue
      if( myid.eq.master ) print *, 'N-out=',mout
      do 23004 m= 1,np
         v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2 
         vm= sqrt(v2)
         rlambda= 1.
         if( v2 .gt. 0. ) rlambda= 2*psmu(m)*b0/v2
         psp(m)= f2(rlambda,vm)
23004 continue
      deallocate( sptmp1, sptmp3 )
      deallocate( vp1b, vp2b, vp3b )
      deallocate( vp1d1, vp2d1, vp3d1 )
      deallocate( vp1d2, vp2d2, vp3d2 )
      return 
      end
      subroutine p_weight 
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_part 
      use cm_spec
      use cm_pgrid
      use cm_tk
      use cm_df
      real, dimension(ni,nj,nk) :: stmp1, stmp3
      real, dimension(ni,nj,nk) :: v1d1,v2d1,v3d1,v1d2,v2d2,v3d2,v1tmp,
     &v2tmp,v3tmp
      real, allocatable, dimension(:,:,:) :: sptmp1,sptmp3
      real, allocatable, dimension(:,:,:) :: vp1b,vp2b,vp3b
      real, allocatable, dimension(:,:,:) :: vp1d1,vp2d1,vp3d1
      real, allocatable, dimension(:,:,:) :: vp1d2,vp2d2,vp3d2
      step(xa)= 0.5 + sign(0.5,xa)
      ff1(xa)= exp( -step(xa-v0)*(xa-v0)**2/deltav**2 )
      f1(xa)= ff1(xa)/(xa**3 + vstar**3) *(1./(r0*xa-psi0-pminus)**betav
     &)
      fl0(ya)= lambda0*( 1 - rl0*(1-ya/v0)*step(v0-ya) ) 
      fdl(ya)= deltal*( 1 + rdl*(1-ya/v0)*step(v0-ya) ) 
      f20(xa,ya)= exp( -( (xa - fl0(ya))/fdl(ya) )**2 )
      f2a(xa) = exp( -( (xa -lambda0a)/deltala )**2 )
      f2(xa,ya)= f20(xa,ya) + etaf*f2a(xa)
      f3(xa)= step(xa-pminus)*(xa-pminus)**betav
      allocate( sptmp1(ni,nx,ny), sptmp3(ni,nx,ny) )
      allocate( vp1b(ni,nx,ny), vp2b(ni,nx,ny), vp3b(ni,nx,ny) )
      allocate( vp1d1(ni,nx,ny), vp2d1(ni,nx,ny), vp3d1(ni,nx,ny) )
      allocate( vp1d2(ni,nx,ny), vp2d2(ni,nx,ny), vp3d2(ni,nx,ny) )
      stmp3= sqrt( v1b1**2 + v2b1**2 + v3b1**2 ) 
      v1d1 = v1b1 / stmp3 
      v2d1 = v2b1 / stmp3 
      v3d1 = v3b1 / stmp3 
      call curl_vv( v1d2,v2d2,v3d2, v1d1,v2d1,v3d1, 21 ) 
      stmp1 = v1d1 * v1d2 + v2d1 * v2d2 + v3d1 * v3d2 
      v1d1 = v1d1 * stmp1 
      v2d1 = v2d1 * stmp1 
      v3d1 = v3d1 * stmp1 
      v1d2 = v1d2 - v1d1
      v2d2 = v2d2 - v2d1
      v3d2 = v3d2 - v3d1 
      v1d2 = v1d2 / stmp3 
      v2d2 = v2d2 / stmp3 
      v3d2 = v3d2 / stmp3 
      stmp1 = stmp1 / stmp3
      call grad_vs( v1d1,v2d1,v3d1, stmp3, 18 )
      call cros_vvv( v1tmp,v2tmp,v3tmp, v1b1,v2b1,v3b1, v1d1,v2d1,v3d1 )
      v1d1 = v1tmp / stmp3 
      v2d1 = v2tmp / stmp3 
      v3d1 = v3tmp / stmp3 
      v1d1 = v1d1 / stmp3 
      v2d1 = v2d1 / stmp3 
      v3d1 = v3d1 / stmp3 
      call maptop_s( sptmp1, stmp1 )
      call maptop_v( vp1b, vp2b, vp3b, v1b1,v2b1,v3b1 )
      call maptop_v( vp1d1, vp2d1, vp3d1, v1d1,v2d1,v3d1 )
      call maptop_v( vp1d2, vp2d2, vp3d2, v1d2,v2d2,v3d2 )
      rlo2= rlo**2
      rhi2= rhi**2
      do 23000 isp= 1, nsp
         do 23002 m= mfsp(isp), mlsp(isp)
            ri= ( pv1x1(m) -q(1) )*dqi + 1.
            rj= ( pv2x1(m) -x(1) )*dxi + 1.
            rk= ( pv3x1(m) -y(1) )*dyi + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            kk= rk
            kp= kk + 1
            fkp= rk - kk
            fkk= 1. - fkp
            bz = ( fkk*  ( fjj*( fii*vp1b(ii,jj,kk) +fip*vp1b(ip,jj,kk) 
     &) + fjp*( fii*vp1b(ii,jp,kk) +fip*vp1b(ip,jp,kk) ) ) + fkp*  ( fjj
     &*( fii*vp1b(ii,jj,kp) +fip*vp1b(ip,jj,kp) ) + fjp*( fii*vp1b(ii,jp
     &,kp) +fip*vp1b(ip,jp,kp) ) ) )
            bx = ( fkk*  ( fjj*( fii*vp2b(ii,jj,kk) +fip*vp2b(ip,jj,kk) 
     &) + fjp*( fii*vp2b(ii,jp,kk) +fip*vp2b(ip,jp,kk) ) ) + fkp*  ( fjj
     &*( fii*vp2b(ii,jj,kp) +fip*vp2b(ip,jj,kp) ) + fjp*( fii*vp2b(ii,jp
     &,kp) +fip*vp2b(ip,jp,kp) ) ) )
            by = ( fkk*  ( fjj*( fii*vp3b(ii,jj,kk) +fip*vp3b(ip,jj,kk) 
     &) + fjp*( fii*vp3b(ii,jp,kk) +fip*vp3b(ip,jp,kk) ) ) + fkp*  ( fjj
     &*( fii*vp3b(ii,jj,kp) +fip*vp3b(ip,jj,kp) ) + fjp*( fii*vp3b(ii,jp
     &,kp) +fip*vp3b(ip,jp,kp) ) ) )
            v11m = ( fkk*  ( fjj*( fii*vp1d1(ii,jj,kk) +fip*vp1d1(ip,jj,
     &kk) ) + fjp*( fii*vp1d1(ii,jp,kk) +fip*vp1d1(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp1d1(ii,jj,kp) +fip*vp1d1(ip,jj,kp) ) + fjp*( fii*
     &vp1d1(ii,jp,kp) +fip*vp1d1(ip,jp,kp) ) ) )
            v21m = ( fkk*  ( fjj*( fii*vp2d1(ii,jj,kk) +fip*vp2d1(ip,jj,
     &kk) ) + fjp*( fii*vp2d1(ii,jp,kk) +fip*vp2d1(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp2d1(ii,jj,kp) +fip*vp2d1(ip,jj,kp) ) + fjp*( fii*
     &vp2d1(ii,jp,kp) +fip*vp2d1(ip,jp,kp) ) ) )
            v31m = ( fkk*  ( fjj*( fii*vp3d1(ii,jj,kk) +fip*vp3d1(ip,jj,
     &kk) ) + fjp*( fii*vp3d1(ii,jp,kk) +fip*vp3d1(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp3d1(ii,jj,kp) +fip*vp3d1(ip,jj,kp) ) + fjp*( fii*
     &vp3d1(ii,jp,kp) +fip*vp3d1(ip,jp,kp) ) ) )
            v12m = ( fkk*  ( fjj*( fii*vp1d2(ii,jj,kk) +fip*vp1d2(ip,jj,
     &kk) ) + fjp*( fii*vp1d2(ii,jp,kk) +fip*vp1d2(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp1d2(ii,jj,kp) +fip*vp1d2(ip,jj,kp) ) + fjp*( fii*
     &vp1d2(ii,jp,kp) +fip*vp1d2(ip,jp,kp) ) ) )
            v22m = ( fkk*  ( fjj*( fii*vp2d2(ii,jj,kk) +fip*vp2d2(ip,jj,
     &kk) ) + fjp*( fii*vp2d2(ii,jp,kk) +fip*vp2d2(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp2d2(ii,jj,kp) +fip*vp2d2(ip,jj,kp) ) + fjp*( fii*
     &vp2d2(ii,jp,kp) +fip*vp2d2(ip,jp,kp) ) ) )
            v32m = ( fkk*  ( fjj*( fii*vp3d2(ii,jj,kk) +fip*vp3d2(ip,jj,
     &kk) ) + fjp*( fii*vp3d2(ii,jp,kk) +fip*vp3d2(ip,jp,kk) ) ) + fkp* 
     & ( fjj*( fii*vp3d2(ii,jj,kp) +fip*vp3d2(ip,jj,kp) ) + fjp*( fii*
     &vp3d2(ii,jp,kp) +fip*vp3d2(ip,jp,kp) ) ) )
            v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2 
            bm= sqrt(bz**2 + bx**2 + by**2) 
            b1m= bz/bm
            b2m= bx/bm
            b3m= by/bm
            vpl= pv1v1(m)*b1m + pv2v1(m)*b2m + pv3v1(m)*b3m
            vpl2= vpl**2
            psmu(m)= 0.5*(v2 - vpl2)/bm 
            vd1= psmu(m)*v11m + vpl2*v12m
            vd2= psmu(m)*v21m + vpl2*v22m
            vd3= psmu(m)*v31m + vpl2*v32m
            vv2= (pv1v1(m)-vd1)**2 + (pv2v1(m)-vd2)**2 + (pv3v1(m)-vd3)*
     &*2 
            psmu(m)= 0.5*(vv2 - vpl2)/bm 
            v1m= pv1v1(m)-vpl*b1m 
            v2m= pv2v1(m)-vpl*b2m
            v3m= pv3v1(m)-vpl*b3m
            rho1= (b2m*v3m-b3m*v2m)/bm 
            rho2= (b3m*v1m-b1m*v3m)/bm
            rho3= (b1m*v2m-b2m*v1m)/bm
            rm2= (pv2x1(m) - rho2 - x0)**2 + (pv3x1(m) - rho3 - y0)**2
            if( rm2 .lt. rlo2 .or. rm2 .gt. rhi2 ) then
               rho2= 0.
               rho3= 0.
            endif
            ri= ( pv1x1(m) -rho1 -q(1) )*dqi + 1. 
            rj= ( pv2x1(m) -rho2 -x(1) )*dxi + 1. 
            rk= ( pv3x1(m) -rho3 -y(1) )*dyi + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            kk= rk
            kp= kk + 1
            fkp= rk - kk
            fkk= 1. - fkp
            bh1 = ( fkk*  ( fjj*( fii*vp1b(ii,jj,kk) +fip*vp1b(ip,jj,kk)
     & ) + fjp*( fii*vp1b(ii,jp,kk) +fip*vp1b(ip,jp,kk) ) ) + fkp*  ( 
     &fjj*( fii*vp1b(ii,jj,kp) +fip*vp1b(ip,jj,kp) ) + fjp*( fii*vp1b(ii
     &,jp,kp) +fip*vp1b(ip,jp,kp) ) ) )
            bh2 = ( fkk*  ( fjj*( fii*vp2b(ii,jj,kk) +fip*vp2b(ip,jj,kk)
     & ) + fjp*( fii*vp2b(ii,jp,kk) +fip*vp2b(ip,jp,kk) ) ) + fkp*  ( 
     &fjj*( fii*vp2b(ii,jj,kp) +fip*vp2b(ip,jj,kp) ) + fjp*( fii*vp2b(ii
     &,jp,kp) +fip*vp2b(ip,jp,kp) ) ) )
            bh3 = ( fkk*  ( fjj*( fii*vp3b(ii,jj,kk) +fip*vp3b(ip,jj,kk)
     & ) + fjp*( fii*vp3b(ii,jp,kk) +fip*vp3b(ip,jp,kk) ) ) + fkp*  ( 
     &fjj*( fii*vp3b(ii,jj,kp) +fip*vp3b(ip,jj,kp) ) + fjp*( fii*vp3b(ii
     &,jp,kp) +fip*vp3b(ip,jp,kp) ) ) )
            bcbm = ( fkk*  ( fjj*( fii*sptmp1(ii,jj,kk) +fip*sptmp1(ip,
     &jj,kk) ) + fjp*( fii*sptmp1(ii,jp,kk) +fip*sptmp1(ip,jp,kk) ) ) + 
     &fkp*  ( fjj*( fii*sptmp1(ii,jj,kp) +fip*sptmp1(ip,jj,kp) ) + fjp*(
     & fii*sptmp1(ii,jp,kp) +fip*sptmp1(ip,jp,kp) ) ) )
            bm_gc= sqrt(bh1**2 + bh2**2 + bh3**2) 
            b1m= bh1/bm_gc
            b2m= bh2/bm_gc
            b3m= bh3/bm_gc
            vdotb= (v1m*b1m+v2m*b2m+v3m*b3m)
            rmu2= 0.5*vpl*vdotb/bm_gc
            psmu(m)= psmu(m) - rmu2
            psmu(m)= psmu(m)*( 1 - 0.5*vpl*bcbm ) 
  400       continue
23002    continue
23000 continue
      do 23004 m= 1,np
         v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2 
         vm= sqrt(v2)
         rlambda= 1.
         if( v2 .gt. 0. ) rlambda= 2*psmu(m)*b0/v2
         psp(m)= ff1(vm)*f2(rlambda,vm)*f3(pspphi(m)) *(1./(r0*vm-psi0-
     &pminus)**betav)
         psw(m)= 0. 
23004 continue
      deallocate( sptmp1, sptmp3 )
      deallocate( vp1b, vp2b, vp3b )
      deallocate( vp1d1, vp2d1, vp3d1 )
      deallocate( vp1d2, vp2d2, vp3d2 )
      return 
      end
      subroutine inversed(rrd, zd, nrg, nzg, sden) 
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      real, dimension(ni,nj,nk) :: sden
      dimension d1d(nj), d2d(ni,nj), rrd(nrg), zd(nzg,nj)
      dimension dsum(nj)
      do 23000 j= 1,nj 
         dsum(j)= 0.
         do 23002 i= 1,ni
            dsum(j)= dsum(j) + wq(i)*sden(i,j,k3)*sh3(i,j,k3)
23002    continue
23000 continue
      j3=3
      if( ibstyp(1,2,16 ).eq.7 ) j3=1 
      d1d(1:3)= 0.
      do 23004 jj= j3+1,njm2
         if( jj.lt.j3+4 ) then
            d1d(jj)= 0.5*( dsum(j3) + dsum(jj) )
            do 23006 j= j3+1,jj-1
               d1d(jj)= d1d(jj) + dsum(j)
23006       continue
         elseif( jj.lt.j3+7 ) then
            d1d(jj)= 0.5*dsum(jj)
            do 23008 j= j3,jj-1
               d1d(jj)= d1d(jj) + wr(j)*dsum(j)
23008       continue
         else
            d1d(jj)= aw1*dsum(jj) + aw2*dsum(jj-1) + aw3*dsum(jj-2) + 
     &aw4*dsum(jj-3)
            do 23010 j= j3,jj-4
               d1d(jj)= d1d(jj) + wr(j)*dsum(j)
23010       continue
         endif
23004 continue
      do 23012 j= j3,njm2
         d1d(j)= d1d(j)/d1d(njm2)
23012 continue
      d1d(njm)= 1.
      d1d(nj)= 1.
      eps= 0.1/nrg
      jmin= 1
      do 23014 j= 1,nj
         if( d1d(j).gt.eps ) goto 80
         jmin= j
23014 continue
   80 continue
      jmax= nj
      do 23016 j= 1,nj
         if( 1-d1d(nj+1-j).gt.eps ) goto 90
         jmax= nj+1-j
23016 continue
   90 continue
      if( myid.eq.master ) print *, ''
      if( myid.eq.master ) print *, 'jmin,jmax=',jmin,jmax,r(jmin),r(
     &jmax)
      rtmp= 0.5
      if( ibstyp(1,1,16 ).eq.2 ) rtmp= 1.
      d2d = 0.
      if( ibstyp(1,1,16 ).eq.2 ) then
         d2d(3,1:nj)= 0.5*sden(3,1:nj,k3)
      endif
      do 23018 ii= 4,nim2
         if( ii.lt.7) then
            do 23020 j= j3,njm2
               d2d(ii,j)= rtmp*sden(3,j,k3) + 0.5*sden(ii,j,k3)
               do 23022 i= 4,ii-1
                  d2d(ii,j)= d2d(ii,j) + sden(i,j,k3)
23022          continue
23020       continue
         elseif( ii.lt.10 ) then
            do 23024 j= j3,njm2
               d2d(ii,j)= 0.5*sden(ii,j,k3)
               do 23026 i= 3,ii-1
                  d2d(ii,j)= d2d(ii,j) + wq(i)*sden(i,j,k3)
23026          continue
23024       continue
         else
            do 23028 j= j3,njm2
               d2d(ii,j)= aw1*sden(ii,j,k3) + aw2*sden(ii-1,j,k3) + aw3*
     &sden(ii-2,j,k3) + aw4*sden(ii-3,j,k3)
               do 23030 i= 3,ii-4
                  d2d(ii,j)= d2d(ii,j) + wq(i)*sden(i,j,k3)
23030          continue
23028       continue
         endif
23018 continue
      do 23032 j= j3,njm2
         sum= 0.
         do 23034 i= 1,ni
            sum= sum + wq(i)*sden(i,j,k3)
23034    continue
         do 23036 i= 3,nim2
            if( sum.eq.0. ) then
               d2d(i,j)= 0.
            else
               d2d(i,j)= d2d(i,j)/sum
            endif
23036    continue
23032 continue
      do 23038 i= 1,ni
         do 23040 j= 1,jmin
            d2d(i,j)= 0.
23040    continue
         do 23042 j= jmax,nj
            d2d(i,j)= 0.
23042    continue
23038 continue
      if( ibstyp(1,1,16 ).eq.2 .and. ibstyp(2,1,16 ).eq.2 ) then
         do 23044 j= jmin+1,jmax-1
            d2d(2,j)= - d2d(3,j)
            d2d(nim,j)= 2. - d2d(nim2,j)
            d2d(ni, j)= 1.
23044    continue
      else
         do 23046 j= jmin+1,jmax-1
            d2d(nim,j)= 1.
            d2d(ni, j)= 1.
23046    continue
      endif
      jjp= 2
      do 23048 j= 2,nrg-1
         d1dt= real(j-1)/(nrg-1)
         do 23050 jj= jjp,njm2
            if( d1d(jj).gt.d1dt ) then
               jjp= jj
               goto 100
            endif
23050    continue
         print *, 'd1dt>d1d(j) in tine inversed'
  100    continue
         rrd(j)= ( (d1d(jjp)-d1dt)*r(jjp-1)**2 + (d1dt-d1d(jjp-1))*r(jjp
     &)**2 )/( d1d(jjp)-d1d(jjp-1) )
23048 continue
      rrd(1)= (r(jmin))**2
      rrd(nrg)= (r(jmax))**2
      do 23052 j= jmin+1,jmax-1
         iip= 3
         do 23054 i= 1,nzg
            d2dt= max( real(i-1)/(nzg-1), 3.0e-4 )
            d2dt= min( d2dt, 1-3.0e-4 )
            do 23056 ii= iip,nim
               if( d2d(ii,j).gt.d2dt ) then
                  iip= ii
                  goto 200
               endif
23056       continue
            print *, 'd2dt>d2d(i,j) in tine inversed'
            stop 1122
  200       continue
            zd(i,j)= ( (d2d(iip,j)-d2dt)*q(iip-1) + (d2dt-d2d(iip-1,j))*
     &q(iip) )/( d2d(iip,j)-d2d(iip-1,j) )
23054    continue
23052 continue
      do 23058 i= 1,nzg
         do 23060 j= 1,jmin
            zd(i,j)= zd(i,jmin+1)
23060    continue
         do 23062 j= jmax,nj
            zd(i,j)= zd(i,jmax-1)
23062    continue
23058 continue
      return 
      end
      subroutine inversed_o(rrd, zd, nrg, nzg)
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      dimension d1d(nj), d2d(ni,nj), rrd(nrg), zd(nzg,nj)
      dimension wwr(nj), wwq(ni)
       
      if( ibstyp(1,1,16 ).ne.2 .or. ibstyp(2,1,16 ).ne.2 ) then
         print *, 
     &'Error:  ibstyp(1,1,16 )<>2 | ibstyp(2,1,16 )<>2 in  inversed  > e
     &xit'
         call exit(0)
      endif
      do 23000 j= 1,nj
         wwr(j)= 1.
23000 continue
      do 23002 jj= 1,njm2
         if( jj.lt.8 ) then
            wwr(1)= 0.5
            wwr(jj)= 0.5
         else
            wwr(1)= aw1
            wwr(2)= aw2
            wwr(3)= aw3
            wwr(4)= aw4
            wwr(jj-3)= aw4
            wwr(jj-2)= aw3
            wwr(jj-1)= aw2
            wwr(jj) = aw1
         endif
         if( jj.eq.1 ) wwr(1)= 0.
         sumj= 0.
         do 23004 j= 1,jj
            sum= 0.
            do 23006 i= 1,ni
               sum= sum + wq(i)*srhoq0(i,j,k3)*v3h(i,j,k3)
23006       continue
            sumj= sumj + sum*wwr(j)
23004    continue
         d1d(jj)= sumj
         do 23008 j= 1,nj
            wwr(j)= 1.
23008    continue
23002 continue
      do 23010 j= 1,njm2
         d1d(j)= d1d(j)/d1d(njm2)
23010 continue
      d1d(njm)= 1.
      d1d(nj)= 1.
      do 23012 i= 1,ni
         wwq(i)= 1.
23012 continue
      do 23014 ii= 3,nim2
         if( ii.lt.7 ) then
            wwq(ii)= 0.5
         else
            wwq(ii-3)= aw4
            wwq(ii-2)= aw3
            wwq(ii-1)= aw2
            wwq(ii) = aw1
         endif
         do 23016 j= 1,njm2
            d2d(ii,j)= 0.
            do 23018 i= 3,ii
               d2d(ii,j)= d2d(ii,j) + wwq(i)*srhoq0(i,j,k3)
23018       continue
23016    continue
         do 23020 i= 1,ni
            wwq(i)= 1.
23020    continue
23014 continue
      do 23022 j= 1,njm2
         sum= 0.
         do 23024 i= 1,ni
            sum= sum + wq(i)*srhoq0(i,j,k3)
23024    continue
         do 23026 i= 3,nim2
            d2d(i,j)= d2d(i,j)/sum
23026    continue
23022 continue
      do 23028 i= 3,nim2
         d2d(i,njm)= 0.
         d2d(i,nj)= 0.
23028 continue
      do 23030 j= 1,nj
         d2d(2,j)= - d2d(3,j)
         d2d(nim,j)= 2. - d2d(nim2,j)
23030 continue
      jjp= 2
      do 23032 j= 2,nrg-1
         d1dt= real(j-1)/(nrg-1)
         do 23034 jj= jjp,njm2
            if( d1d(jj).gt.d1dt ) then
               jjp= jj
               goto 100
            endif
23034    continue
         print *, 'd1dt>d1d(j) in tine inversed'
  100    continue
         rrd(j)= ( (d1d(jjp)-d1dt)*r(jjp-1)**2 + (d1dt-d1d(jjp-1))*r(jjp
     &)**2 )/( d1d(jjp)-d1d(jjp-1) )
23032 continue
      rrd(1)= 0.
      rrd(nrg)= r(njm2)**2
      do 23036 j= 1,njm2
         iip= 3
         do 23038 i= 2,nzg-1
            d2dt= real(i-1)/(nzg-1)
            do 23040 ii= iip,nim
               if( d2d(ii,j).gt.d2dt ) then
                  iip= ii
                  goto 200
               endif
23040       continue
            print *, 'd2dt>d2d(i,j) in tine inversed'
  200       continue
            zd(i,j)= ( (d2d(iip,j)-d2dt)*q(iip-1) + (d2dt-d2d(iip-1,j))*
     &q(iip) )/( d2d(iip,j)-d2d(iip-1,j) )
23038    continue
         zd(1,j)= qlo
         zd(nzg,j)= qhi
23036 continue
      return 
      end
      subroutine nstx_load(sden, vd, nvg)
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_tk
      use cm_df
      real, dimension(ni,nj,nk) :: sden
      dimension den(ni,nj), v1d(nvg), vd(ni,nj,nvg)
      real,dimension(:),allocatable :: v,vint
      step(xa)= 0.5 + sign(0.5,xa)
      ff1(xa)= exp( -step(xa-v0)*(xa-v0)**2/deltav**2 )
      f1(xa)= ff1(xa)/(xa**3 + vstar**3) *(1./(r0*xa-psi0-pminus)**betav
     &)
      fl0(ya)= lambda0*( 1 - rl0*(1-ya/v0)*step(v0-ya) ) 
      fdl(ya)= deltal*( 1 + rdl*(1-ya/v0)*step(v0-ya) ) 
      f20(xa,ya)= exp( -( (xa - fl0(ya))/fdl(ya) )**2 )
      f2a(xa) = exp( -( (xa -lambda0a)/deltala )**2 )
      f2(xa,ya)= f20(xa,ya) + etaf*f2a(xa)
      f3(xa)= step(xa-pminus)*(xa-pminus)**betav
      pf(xa)= f1(xa)*xa*( step( xa-vpsi)*( xa-vpsi)**(betav+1) - step(-
     &xa-vpsi)*(-xa-vpsi)**(betav+1) )
      den= 0.
      v1d= 0.
      vd= 0.
      nv= 2*nvg
      allocate(v(nv),vint(nv))
      vmax= v0+2*deltav
      dv= vmax/(nv-1)
      v= (/ (dv*i, i= 0,nv-1) /) 
      vint= 0.
      v1d= (/ (real(i), i= 0,nvg-1) /)
      dvd= 1./(nvg-1)
      v1d= v1d*dvd 
      do 23000 i= 1,ni
         do 23002 j= 1,nj
            vpsi= (spsi(i,j,k3) + pminus)/r(j)
            if( srhoq0(i,j,k3).eq.0. .or. vmax.le.vpsi ) goto 100
            vint(1)= 0.
            vint(2)= 0.5*( pf(v(1)) + pf(v(2)) )
            vint(3)= vint(2) + 0.5*( pf(v(2)) + pf(v(3)) )
            vint(4)= vint(3) + 0.5*( pf(v(3)) + pf(v(4)) )
            sum0= aw1*pf(v(1)) +aw2*pf(v(2)) +aw3*pf(v(3)) +aw4*pf(v(4))
            vint(5)= sum0 + 0.5*pf(v(5))
            vint(6)= vint(5) + 0.5*( pf(v(5)) + pf(v(6)) )
            vint(7)= vint(6) + 0.5*( pf(v(6)) + pf(v(7)) )
            sumv= aw4*pf(v(5)) +aw3*pf(v(6)) +aw2*pf(v(7)) +aw1*pf(v(8))
            vint(8)= sum0 + sumv
            do 23004 iv= 9,nv
               sumvt= aw4*pf(v(iv-3)) + aw3*pf(v(iv-2)) + aw2*pf(v(iv-1)
     &) + aw1*pf(v(iv))
               vint(iv)= vint(iv-1) - sumv + pf(v(iv-4)) + sumvt
               sumv= sumvt
23004       continue
            den(i,j)= vint(nv)*dv*r(j)**betav/(betav+1)
            vint0= vint(nv)
            if( vint0.eq.0. ) goto 100
            vint= vint/vint0
            vmin= max(0.,vpsi)
            vd(i,j,1)= vmin
            vd(i,j,nvg)= vmax
            ivp= 2
            do 23006 l= 2,nvg-1
               do 23008 iv= ivp,nv
                  if( vint(iv) .gt. v1d(l) ) then
                     ivp= iv
                     goto 200
                  endif
23008          continue
               print *,'v1d(l)>vint(iv) in tine nstx_load'
  200          continue
               vd(i,j,l)= ( v(ivp-1)*(vint(ivp)-v1d(l)) + v(ivp)*(v1d(l)
     &-vint(ivp-1)) ) /(vint(ivp)-vint(ivp-1))
23006       continue
  100       continue
23002    continue
23000 continue
      denmax= maxval(den)
      do 23010 k= 1,nk
         sden(:,:,k)= den/denmax
23010 continue
      deallocate(v,vint)
      return 
      end
      subroutine savedump(isavdmp)
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_out
      use cm_part 
      use cm_spec
      use cm_tk
      use cm_df
      character *20 csd
      if( isavdmp.eq.0 ) then
         csd = 'start.d'
      elseif( isavdmp.eq.1 ) then
         isave= 0
         isave= isave + 1
         stop 33
      elseif(isavdmp.eq.2) then
         idump= 0
         idump= idump + 1
         stop 33
      else
         if( myid.eq.master ) print *, 'Improper isavdmp value (',
     &isavdmp,') in savedump'
         stop 1234
      endif
      isave= 0
      idump= 0
      open( unit=26,file=csd,status='unknown',form='unformatted' )
      ifhout= 0
      ivfldout= 0
      ibfldout= 0
      itiout= 0
      i3dbout= 0
      iptrout= 0
      denki0= 0.
      if(myid .eq. master) then
         write(26) idrun,its,t,3 ,ni,nj,nk ,16 ,22 ,18 ,.true. ,.true. ,
     &.false. ,nsp,np,beta,gamma,ialign,denki0 ,pcut,rhoc,omegai0 ,idump
     &, isave, ienout, ifhout, isfldout, ivfldout, ibfldout ,itiout, 
     &i3dbout, iptrout
         write(26) q, r, s, v1h,v2h,v3h
         write(26) sp1, v1b1,v2b1,v3b1
         write(26) srhob1, v1mo1,v2mo1,v3mo1
         write(26) v1a,v2a,v3a
         write(26) rmask,sres,v1b1,v2b1,v3b1,v1ub,v2ub,v3ub,v1a,v2a,v3a 
         write(26) srhob1,sp1 
         write(26) v0,vstar,deltav,aav,bbv,r0,psi0 ,pminus,betav,b0,
     &lambda0,deltal,rl0,rdl ,etaf,deltala,lambda0a
         do 23000 isp= 1,nsp
            write(26) cisp(isp),dennsp(isp),npsp(isp),qnsp(isp), tratsp(
     &isp),vthplsp(isp),wnsp(isp),wdqsp(isp), wsp(isp),qsp(isp)
23000    continue
         write(26) srhoq0, v1curi0,v2curi0,v3curi0, spi0
         write(26) (pv1v1(m), m= 1,np)
         write(26) (pv2v1(m), m= 1,np)
         write(26) (pv3v1(m), m= 1,np)
         write(26) (pv1x1(m), m= 1,np)
         write(26) (pv2x1(m), m= 1,np)
         write(26) (pv3x1(m), m= 1,np)
         write(26) (psw(m), m= 1,np)
         write(26) (psp(m), m= 1,np)
      endif
      close(26)
      return 
      end
      subroutine finalize
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_part 
      deallocate( sp, sp1, srhob1)
      deallocate( srhoq, srhoq0, spi0)
      deallocate( spsi, sphi, rmask, sres, sg)
      deallocate( v1b1,v2b1,v3b1, v1a,v2a,v3a, v1cur,v2cur,v3cur)
      deallocate( v1e,v2e,v3e, v1mo1,v2mo1,v3mo1, v1ub,v2ub,v3ub)
      deallocate( v1curi,v2curi,v3curi, v1curi0,v2curi0,v3curi0)
      deallocate( v1vhypa,v2vhypa,v3vhypa)
      deallocate( v1h,v2h,v3h)
      deallocate( v1hi,v2hi,v3hi, v1h2,v2h2,v3h2, v1h2i,v2h2i,v3h2i)
      deallocate( v1h2d1,v2h2d1,v3h2d1)
      deallocate( sh3, sh3w, sh3i)
      deallocate( t12hder,t13hder,t21hder,t23hder,t31hder,t32hder)
      deallocate( pv1x1,pv2x1,pv3x1, pv1v1,pv2v1,pv3v1)
      deallocate( pspphi, psr, psmu, psw, psp)
      return 
      end
      subroutine der_ss( so , si , ider )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so, si, stmp 
       
      if( ider.lt.1 .or. ider.gt.3 ) then
         print *, 'Error:  ider<1 | ider>3  in  der_ss  > exit'
         call exit(0)
      endif
      if( 3 .eq.2 .and. ider.eq.3 ) then
         so = 0.
         return 
      endif
      stmp = 0.
      if( ider.eq.1 ) then
         do 23000 k= k3,nkm2
            do 23002 j= 3,njm2
               do 23004 i= 3,nim2
                  stmp(i,j,k)= d(-1,1)*si(i-1,j,k) +d( 1,1)*si(i+1,j,k) 
     &+d(-2,1)*si(i-2,j,k) +d( 2,1)*si(i+2,j,k)
23004          continue
23002       continue
23000    continue
      elseif( ider.eq.2 ) then
         do 23006 k= k3,nkm2
            do 23008 j= 3,njm2
               do 23010 i= 3,nim2
                  stmp(i,j,k)= d(-1,2)*si(i,j-1,k) +d( 1,2)*si(i,j+1,k) 
     &+d(-2,2)*si(i,j-2,k) +d( 2,2)*si(i,j+2,k)
23010          continue
23008       continue
23006    continue
      elseif( ider.eq.3 ) then
         do 23012 k= k3,nkm2
            do 23014 j= 3,njm2
               do 23016 i= 3,nim2
                  stmp(i,j,k)= d(-1,3)*si(i,j,k-1) +d( 1,3)*si(i,j,k+1) 
     &+d(-2,3)*si(i,j,k-2) +d( 2,3)*si(i,j,k+2)
23016          continue
23014       continue
23012    continue
      endif
      so = stmp
      return 
      end
      subroutine maptop_v( vp1o, vp2o, vp3o, v1ia,v2ia,v3ia )
      use param
      use cm_cmvec
      use cm_fevec
      use cm_pgrid
      real, dimension(ni,nj,nk) :: v1ia,v2ia,v3ia,v1tmp,v2tmp,v3tmp 
      dimension vp1o(ni,nx,ny),vp2o(ni,nx,ny),vp3o(ni,nx,ny)
      do 23000 k= 1,nk
         do 23002 j= 1,nj
            do 23004 i= 1,ni
               v2tmp(i,j,k)= v2ia(i,j,k)*coss(k) - v3ia(i,j,k)*sins(k)
               v3tmp(i,j,k)= v2ia(i,j,k)*sins(k) + v3ia(i,j,k)*coss(k)
23004       continue
23002    continue
23000 continue
      if( ibstyp(1,2,16 ).eq.7 ) then
         call fixo_s( v2tmp, v2tmp, 2 )
         call fixo_s( v3tmp, v3tmp, 2 )
      endif
      call maptop_s( vp1o, v1ia )
      call maptop_s( vp2o, v2tmp )
      call maptop_s( vp3o, v3tmp )
      return 
      end
      subroutine maptop_s( spo, si )
      use MPI_module
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_pgrid
      real, dimension(ni,nj,nk) :: si 
      dimension spo(ni,nx,ny)
      integer ibp1(nx,ny)
      spo = 0.
      ibp1= ibp
      do 23000 k= 1,ny
         do 23002 j= 1,nx
            rj= ( rxy(j,k) - r(1) )*dri + 1.
            rk= ( sxy(j,k) - s(1) )*dsi + 1.
             
            jj= nint( rj )
            jm= jj - 1
            jp= jj + 1
            if( rj .gt. jj ) then
               fj= rj - jj
               fjm5= fj - .5
               fjp5= fj + .5
               fjm= fjm5 * fjm5
               fjp= fjp5 * fjp5
            else
               fj= jj - rj
               fjm5= fj - .5
               fjp5= fj + .5
               fjm= fjp5 * fjp5
               fjp= fjm5 * fjm5
            endif
            fjj= 1.5 - 2. * fj * fj
             
            kk= nint( rk )
            km= kk - 1
            kp= kk + 1
            if( rk .gt. kk ) then
               fk= rk - kk
               fkm5= fk - .5
               fkp5= fk + .5
               fkm= fkm5 * fkm5
               fkp= fkp5 * fkp5
            else
               fk= kk - rk
               fkm5= fk - .5
               fkp5= fk + .5
               fkm= fkp5 * fkp5
               fkp= fkm5 * fkm5
            endif
            fkk= 1.5 - 2. * fk * fk
            if( jj.gt.1 .and. jj.lt.nj ) then
               ibp1(j,k)= 1
               do 23004 i= 1,ni
                  spo(i,j,k) = .25 * ( fkm*( fjm*si(i,jm,km) +fjj*si(i,
     &jj,km) +fjp*si(i,jp,km) ) + fkk*( fjm*si(i,jm,kk) +fjj*si(i,jj,kk)
     & +fjp*si(i,jp,kk) ) + fkp*( fjm*si(i,jm,kp) +fjj*si(i,jj,kp) +fjp*
     &si(i,jp,kp) ) ) 
23004          continue
            endif
            if( jj.eq.1 .and. ibstyp(1,2,16 ).eq.7 ) then
               do 23006 kk= 3,nkm2
                  do 23008 i= 1,ni
                     spo(i,j,k)= spo(i,j,k) + 0.5*( fjj*si(i,1,kk) + (
     &fjp+fjm)*si(i,2,kk) )/nkd
23008             continue
23006          continue
            endif
23002    continue
23000 continue
      do 23010 k= 2,ny-1
         do 23012 j= 2,nx-1
            if( ibp1(j,k).eq.1 ) goto 200
            isum= ibp1(j-1,k)+ibp1(j+1,k)+ibp1(j,k-1)+ibp1(j,k+1)
            if( isum.eq.0 ) goto 200
            do 23014 i= 1,ni
               spo(i,j,k)= ( spo(i,j-1,k)*ibp1(j-1,k) + spo(i,j+1,k)*
     &ibp1(j+1,k) + spo(i,j,k-1)*ibp1(j,k-1) + spo(i,j,k+1)*ibp1(j,k+1) 
     &)/isum
23014       continue
  200       continue
23012    continue
23010 continue
      return 
      end
      subroutine maptof_v( v1o,v2o,v3o, vp1i, vp2i, vp3i )
      use param
      use cm_cmvec
      use cm_fevec
      use cm_pgrid
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o,v1tmp,v2tmp,v3tmp 
      dimension vp1i(ni,nx,ny),vp2i(ni,nx,ny),vp3i(ni,nx,ny)
      call maptof_s( v1o, vp1i ) 
      call maptof_vxy( v2o, v3o, vp2i, vp3i ) 
      return 
      end
      subroutine maptof_s( so, spa )
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_pgrid
      real, dimension(ni,nj,nk) :: so 
      dimension spa(ni,nx,ny)
      parameter( nkk=100 )
      dimension tmp(ni,nkk), cost(nkk), sint(nkk)
      do 23000 k= 1,nkk
         cost(k)= cos( rpi2*(k-1)/nkk )
         sint(k)= sin( rpi2*(k-1)/nkk )
23000 continue
      so = 0.
      tmp = 0.
      j3=3
      if( ibstyp(1,2,16 ).eq.7 ) j3=1 
      do 23002 j= j3,njm2
         do 23004 k= 1,nkk
            xx= r(j)*cost(k) + 0.5*(xlo+xhi)
            yy= r(j)*sint(k) + 0.5*(ylo+yhi)
            rj= ( xx - x(1) )*dxi + 1.
            rk= ( yy - y(1) )*dyi + 1.
             
            jj= nint( rj )
            jm= jj - 1
            jp= jj + 1
            if( rj .gt. jj ) then
               fj= rj - jj
               fjm5= fj - .5
               fjp5= fj + .5
               fjm= fjm5 * fjm5
               fjp= fjp5 * fjp5
            else
               fj= jj - rj
               fjm5= fj - .5
               fjp5= fj + .5
               fjm= fjp5 * fjp5
               fjp= fjm5 * fjm5
            endif
            fjj= 1.5 - 2. * fj * fj
             
            kk= nint( rk )
            km= kk - 1
            kp= kk + 1
            if( rk .gt. kk ) then
               fk= rk - kk
               fkm5= fk - .5
               fkp5= fk + .5
               fkm= fkm5 * fkm5
               fkp= fkp5 * fkp5
            else
               fk= kk - rk
               fkm5= fk - .5
               fkp5= fk + .5
               fkm= fkp5 * fkp5
               fkp= fkm5 * fkm5
            endif
            fkk= 1.5 - 2. * fk * fk
            do 23006 i= 1,ni
               tmp(i,k) = .25 * ( fkm*( fjm*spa(i,jm,km) +fjj*spa(i,jj,
     &km) +fjp*spa(i,jp,km) ) + fkk*( fjm*spa(i,jm,kk) +fjj*spa(i,jj,kk)
     & +fjp*spa(i,jp,kk) ) + fkp*( fjm*spa(i,jm,kp) +fjj*spa(i,jj,kp) +
     &fjp*spa(i,jp,kp) ) ) 
23006       continue
23004    continue
         do 23008 i= 1,ni
            rsum= 0.
            do 23010 k= 1,nkk
               rsum= rsum + tmp(i,k)
23010       continue
            do 23012 k= 3,nkm2
               so(i,j,k)= rsum/nkk
23012       continue
23008    continue
23002 continue
      return 
      end
      subroutine maptof_vxy( v2o, v3o, vp2i, vp3i )
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_pgrid
      real, dimension(ni,nj,nk) :: v2o,v3o 
      dimension vp2i(ni,nx,ny) 
      dimension vp3i(ni,nx,ny)
      parameter( nkk=100 )
      dimension tmpx(ni,nkk), tmpy(ni,nkk), cost(nkk), sint(nkk)
      dimension tmpr(ni,nkk), tmps(ni,nkk)
      do 23000 k= 1,nkk
         cost(k)= cos( rpi2*(k-1)/nkk )
         sint(k)= sin( rpi2*(k-1)/nkk )
23000 continue
      v2o = 0.
      v3o = 0.
      tmpr = 0.
      tmps = 0.
      j3=3
      if( ibstyp(1,2,16 ).eq.7 ) j3=1 
      do 23002 j= j3,njm2
         do 23004 k= 1,nkk
            xx= r(j)*cost(k) + 0.5*(xlo+xhi)
            yy= r(j)*sint(k) + 0.5*(ylo+yhi)
            rj= ( xx - x(1) )*dxi + 1.
            rk= ( yy - y(1) )*dyi + 1.
             
            jj= nint( rj )
            jm= jj - 1
            jp= jj + 1
            if( rj .gt. jj ) then
               fj= rj - jj
               fjm5= fj - .5
               fjp5= fj + .5
               fjm= fjm5 * fjm5
               fjp= fjp5 * fjp5
            else
               fj= jj - rj
               fjm5= fj - .5
               fjp5= fj + .5
               fjm= fjp5 * fjp5
               fjp= fjm5 * fjm5
            endif
            fjj= 1.5 - 2. * fj * fj
             
            kk= nint( rk )
            km= kk - 1
            kp= kk + 1
            if( rk .gt. kk ) then
               fk= rk - kk
               fkm5= fk - .5
               fkp5= fk + .5
               fkm= fkm5 * fkm5
               fkp= fkp5 * fkp5
            else
               fk= kk - rk
               fkm5= fk - .5
               fkp5= fk + .5
               fkm= fkp5 * fkp5
               fkp= fkm5 * fkm5
            endif
            fkk= 1.5 - 2. * fk * fk
            do 23006 i= 1,ni
               tmpx(i,k) = .25 * ( fkm*( fjm*vp2i(i,jm,km) +fjj*vp2i(i,
     &jj,km) +fjp*vp2i(i,jp,km) ) + fkk*( fjm*vp2i(i,jm,kk) +fjj*vp2i(i,
     &jj,kk) +fjp*vp2i(i,jp,kk) ) + fkp*( fjm*vp2i(i,jm,kp) +fjj*vp2i(i,
     &jj,kp) +fjp*vp2i(i,jp,kp) ) ) 
               tmpy(i,k) = .25 * ( fkm*( fjm*vp3i(i,jm,km) +fjj*vp3i(i,
     &jj,km) +fjp*vp3i(i,jp,km) ) + fkk*( fjm*vp3i(i,jm,kk) +fjj*vp3i(i,
     &jj,kk) +fjp*vp3i(i,jp,kk) ) + fkp*( fjm*vp3i(i,jm,kp) +fjj*vp3i(i,
     &jj,kp) +fjp*vp3i(i,jp,kp) ) ) 
               tmpr(i,k)= tmpx(i,k)*cost(k) + tmpy(i,k)*sint(k)
               tmps(i,k)= -tmpx(i,k)*sint(k) + tmpy(i,k)*cost(k)
23006       continue
23004    continue
         do 23008 i= 1,ni
            sumr= 0.
            sums= 0.
            do 23010 k= 1,nkk
               sumr= sumr + tmpr(i,k)
               sums= sums + tmps(i,k)
23010       continue
            do 23012 k= 3,nkm2
               v2o(i,j,k)= sumr/nkk
               v3o(i,j,k)= sums/nkk
23012       continue
23008    continue
23002 continue
      return 
      end
      subroutine frompart
      use param
      use cm_cmvec
      use cm_fevec
      use cm_par
      use cm_fld 
      use cm_part 
      use cm_spec
      use cm_pgrid
      real, dimension(ni,nj,nk) :: stmp 
      real, dimension(ni,nj,nk) :: v1tmp,v2tmp,v3tmp
      real, allocatable, dimension(:,:,:) :: sprho,sprhot
      real, allocatable, dimension(:,:,:) :: vp1cur, vp2cur, vp3cur
      real, allocatable, dimension(:,:,:) :: vp1curt,vp2curt,vp3curt
      allocate( sprho(ni,nx,ny), sprhot(ni,nx,ny) )
      allocate( vp1cur(ni,nx,ny), vp2cur(ni,nx,ny), vp3cur(ni,nx,ny) )
      allocate( vp1curt(ni,nx,ny), vp2curt(ni,nx,ny), vp3curt(ni,nx,ny) 
     &)
      sprho = 0.
      vp1cur = 0.
      vp2cur = 0.
      vp3cur = 0.
      do 23000 isp= 1, nsp
         sprhot = 0.
         vp1curt = 0.
         vp2curt = 0.
         vp3curt = 0.
         do 23002 m= mfsp(isp),mlsp(isp)
            ri= ( pv1x1(m) -q(1) )*dqi + 1.
            rj= ( pv2x1(m) -x(1) )*dxi + 1.
            rk= ( pv3x1(m) -y(1) )*dyi + 1.
            ii= ri
            ip= ii + 1
            fip= ri - ii
            fii= 1. - fip
            jj= rj
            jp= jj + 1
            fjp= rj - jj
            fjj= 1. - fjp
            kk= rk
            kp= kk + 1
            fkp= rk - kk
            fkk= 1. - fkp
            wqsp= psp(m)*qsp(isp)
            wv1c= wqsp*pv1v1(m)
            wv2c= wqsp*pv2v1(m)
            wv3c= wqsp*pv3v1(m)
            sprhot(ii,jj,kk)= sprhot(ii,jj,kk) + fkk* fjj * fii * wqsp
            sprhot(ip,jj,kk)= sprhot(ip,jj,kk) + fkk* fjj * fip * wqsp
            sprhot(ii,jp,kk)= sprhot(ii,jp,kk) + fkk* fjp * fii * wqsp
            sprhot(ip,jp,kk)= sprhot(ip,jp,kk) + fkk* fjp * fip * wqsp
            sprhot(ii,jj,kp)= sprhot(ii,jj,kp) + fkp* fjj * fii * wqsp
            sprhot(ip,jj,kp)= sprhot(ip,jj,kp) + fkp* fjj * fip * wqsp
            sprhot(ii,jp,kp)= sprhot(ii,jp,kp) + fkp* fjp * fii * wqsp
            sprhot(ip,jp,kp)= sprhot(ip,jp,kp) + fkp* fjp * fip * wqsp
            vp1curt(ii,jj,kk)= vp1curt(ii,jj,kk) + fkk* fjj * fii * wv1c
            vp1curt(ip,jj,kk)= vp1curt(ip,jj,kk) + fkk* fjj * fip * wv1c
            vp1curt(ii,jp,kk)= vp1curt(ii,jp,kk) + fkk* fjp * fii * wv1c
            vp1curt(ip,jp,kk)= vp1curt(ip,jp,kk) + fkk* fjp * fip * wv1c
            vp1curt(ii,jj,kp)= vp1curt(ii,jj,kp) + fkp* fjj * fii * wv1c
            vp1curt(ip,jj,kp)= vp1curt(ip,jj,kp) + fkp* fjj * fip * wv1c
            vp1curt(ii,jp,kp)= vp1curt(ii,jp,kp) + fkp* fjp * fii * wv1c
            vp1curt(ip,jp,kp)= vp1curt(ip,jp,kp) + fkp* fjp * fip * wv1c
            vp2curt(ii,jj,kk)= vp2curt(ii,jj,kk) + fkk* fjj * fii * wv2c
            vp2curt(ip,jj,kk)= vp2curt(ip,jj,kk) + fkk* fjj * fip * wv2c
            vp2curt(ii,jp,kk)= vp2curt(ii,jp,kk) + fkk* fjp * fii * wv2c
            vp2curt(ip,jp,kk)= vp2curt(ip,jp,kk) + fkk* fjp * fip * wv2c
            vp2curt(ii,jj,kp)= vp2curt(ii,jj,kp) + fkp* fjj * fii * wv2c
            vp2curt(ip,jj,kp)= vp2curt(ip,jj,kp) + fkp* fjj * fip * wv2c
            vp2curt(ii,jp,kp)= vp2curt(ii,jp,kp) + fkp* fjp * fii * wv2c
            vp2curt(ip,jp,kp)= vp2curt(ip,jp,kp) + fkp* fjp * fip * wv2c
            vp3curt(ii,jj,kk)= vp3curt(ii,jj,kk) + fkk* fjj * fii * wv3c
            vp3curt(ip,jj,kk)= vp3curt(ip,jj,kk) + fkk* fjj * fip * wv3c
            vp3curt(ii,jp,kk)= vp3curt(ii,jp,kk) + fkk* fjp * fii * wv3c
            vp3curt(ip,jp,kk)= vp3curt(ip,jp,kk) + fkk* fjp * fip * wv3c
            vp3curt(ii,jj,kp)= vp3curt(ii,jj,kp) + fkp* fjj * fii * wv3c
            vp3curt(ip,jj,kp)= vp3curt(ip,jj,kp) + fkp* fjj * fip * wv3c
            vp3curt(ii,jp,kp)= vp3curt(ii,jp,kp) + fkp* fjp * fii * wv3c
            vp3curt(ip,jp,kp)= vp3curt(ip,jp,kp) + fkp* fjp * fip * wv3c
23002    continue
         do 23004 k= 1,ny
            do 23006 j= 1,nx
               do 23008 i= 1,ni
                  sprho(i,j,k)= sprho(i,j,k) + sprhot(i,j,k)
                  vp1cur(i,j,k)= vp1cur(i,j,k) + vp1curt(i,j,k)
                  vp2cur(i,j,k)= vp2cur(i,j,k) + vp2curt(i,j,k)
                  vp3cur(i,j,k)= vp3cur(i,j,k) + vp3curt(i,j,k)
23008          continue
23006       continue
23004    continue
23000 continue
      call maptof_s( srhoq, sprho )
      call maptof_v( v1curi,v2curi,v3curi, vp1cur, vp2cur, vp3cur )
      call fix_s( srhoq, srhoq, 16 )
      call fix_v( v1curi,v2curi,v3curi, v1curi,v2curi,v3curi, 18 )
      do 23010 k= k3,nkm2
         do 23012 j= 3,njm2
            do 23014 i= 3,nim2
               if( srhoq(i,j,k).lt.0. ) goto 3333
23014       continue
23012    continue
23010 continue
      goto 3334
 3333 print *, '3333; srhoq<0; i,j,k,srhoq= '
      print *, '             ',i,j,k,srhoq(i,j,k)
 3334 continue
      deallocate( sprho, sprhot )
      deallocate( vp1cur, vp2cur, vp3cur )
      deallocate( vp1curt, vp2curt, vp3curt )
      return 
      end
      subroutine cros_vvv( v1o,v2o,v3o, v1ia,v2ia,v3ia, v1ib,v2ib,v3ib )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: stmp 
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o,v1tmp,v2tmp,v3tmp,v1ia,
     &v2ia,v3ia,v1ib,v2ib,v3ib
      v1tmp = v2ia * v3ib
      stmp = v3ia * v2ib
      v1tmp = v1tmp - stmp
      v2tmp = v3ia * v1ib
      stmp = v1ia * v3ib
      v2tmp = v2tmp - stmp
      v3tmp = v1ia * v2ib
      stmp = v2ia * v1ib
      v3tmp = v3tmp - stmp
      v1o = v1tmp
      v2o = v2tmp
      v3o = v3tmp 
      return 
      end
      subroutine curl_vv( v1o,v2o,v3o, v1i,v2i,v3i, ivtyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: stmp 
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o,v1tmp,v2tmp,v3tmp,v1i,v2i
     &,v3i
      v1tmp = v1i * v1h
      v2tmp = v2i * v2h
      v3tmp = v3i * v3h
      call der_ss( v1o, v3tmp, 2 )
      call der_ss( stmp , v2tmp, 3 )
      v1o = v1o - stmp
      v1o = v1o * v1h2i
      call der_ss( v2o, v1tmp, 3 )
      call der_ss( stmp , v3tmp, 1 )
      v2o = v2o - stmp
      v2o = v2o * v2h2i
      call der_ss( v3o, v2tmp, 1 )
      call der_ss( stmp , v1tmp, 2 )
      v3o = v3o - stmp
      v3o = v3o * v3h2i
      call fix_v( v1o,v2o,v3o, v1o,v2o,v3o, ivtyp )
      return 
      end
      subroutine grad_vs( v1o,v2o,v3o, si, ivtyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: si 
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o
      call der_ss( v1o, si, 1)
      v1o = v1o * v1hi
      call der_ss( v2o, si, 2)
      v2o = v2o * v2hi
      call der_ss( v3o, si, 3)
      v3o = v3o * v3hi
      call fix_v( v1o,v2o,v3o, v1o,v2o,v3o, ivtyp )
      return 
      end
      subroutine div_sv( so, v1i,v2i,v3i, istyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so, stmp 
      real, dimension(ni,nj,nk) :: v1i,v2i,v3i
      so = v1i * v1h2
      call der_ss( so , so, 1 )
      stmp = v2i * v2h2
      call der_ss( stmp , stmp, 2 )
      so = so + stmp
      stmp = v3i * v3h2
      call der_ss( stmp , stmp, 3 )
      so = so + stmp
      so = so * sh3i
      call fix_s( so, so, istyp )
      return 
      end
      subroutine div_svs( so, v1i,v2i,v3i, si, istyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so 
      real, dimension(ni,nj,nk) :: v1tmp,v2tmp,v3tmp,v1i,v2i,v3i
      v1tmp = v1i * si 
      v2tmp = v2i * si 
      v3tmp = v3i * si 
      call div_sv( so, v1tmp,v2tmp,v3tmp, istyp )
      return 
      end
      subroutine div_vvv( v1o,v2o,v3o, v1ia,v2ia,v3ia, v1ib,v2ib,v3ib, 
     &ivtyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: stmp 
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o,v1tmpa,v2tmpa,v3tmpa,
     &v1tmpb,v2tmpb,v3tmpb,v1ia,v2ia,v3ia,v1ib,v2ib,v3ib
      v1tmpa = v1ia * v1h2
      v2tmpa = v2ia * v2h2
      v3tmpa = v3ia * v3h2 
      stmp = v1ib * v1tmpa
      call der_ss( v1tmpb , stmp, 1 )
      stmp = v1ib * v2tmpa
      call der_ss( stmp , stmp, 2 )
      v1tmpb = v1tmpb + stmp
      stmp = v1ib * v3tmpa
      call der_ss( stmp , stmp, 3 )
      v1tmpb = v1tmpb + stmp
      v1tmpb = v1tmpb * sh3i
      stmp = v2ib * v1tmpa
      call der_ss( v2tmpb , stmp, 1 )
      stmp = v2ib * v2tmpa
      call der_ss( stmp , stmp, 2 )
      v2tmpb = v2tmpb + stmp
      stmp = v2ib * v3tmpa
      call der_ss( stmp , stmp, 3 )
      v2tmpb = v2tmpb + stmp
      v2tmpb = v2tmpb * sh3i
      stmp = v3ib * v1tmpa
      call der_ss( v3tmpb , stmp, 1 )
      stmp = v3ib * v2tmpa
      call der_ss( stmp , stmp, 2 )
      v3tmpb = v3tmpb + stmp
      stmp = v3ib * v3tmpa
      call der_ss( stmp , stmp, 3 )
      v3tmpb = v3tmpb + stmp
      v3tmpb = v3tmpb * sh3i
      v1tmpa = v1ia * v1ib
      v2tmpa = v2ia * v2ib
      v3tmpa = v3ia * v3ib 
      v1tmpb= v1tmpb +v1ia* ( v2ib*t12hder +v3ib*t13hder ) -v2tmpa*t21
     &hder -v3tmpa*t31hder
      v2tmpb= v2tmpb +v2ia* ( v1ib*t21hder +v3ib*t23hder ) -v1tmpa*t12
     &hder -v3tmpa*t32hder
      v3tmpb= v3tmpb +v3ia* ( v1ib*t31hder +v2ib*t32hder ) -v1tmpa*t13
     &hder -v2tmpa*t23hder
      call fix_v( v1tmpb,v2tmpb,v3tmpb, v1tmpb,v2tmpb,v3tmpb, ivtyp )
      v1o = v1tmpb
      v2o = v2tmpb
      v3o = v3tmpb 
      return 
      end
      subroutine lap_ss( so, si, istyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so, stmp1, stmp2, si
      call der_ss( stmp1 , si , 1 )
      stmp1 = stmp1 * v1h2d1
      call der_ss( stmp1 , stmp1 , 1 )
      call der_ss( stmp2 , si , 2 )
      stmp2 = stmp2 * v2h2d1
      call der_ss( stmp2 , stmp2 , 2 )
      stmp1 = stmp1 + stmp2
      call der_ss( stmp2 , si , 3 )
      stmp2 = stmp2 * v3h2d1
      call der_ss( stmp2 , stmp2 , 3 )
      stmp1 = stmp1 + stmp2
      stmp1 = stmp1 * sh3i
      so = stmp1
      call fix_s( so, so, istyp )
      return 
      end
      subroutine add_vvvr( v1o,v2o,v3o, v1ia,v2ia,v3ia, v1ib,v2ib,v3ib, 
     &rval )
      use param
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o,v1ia,v2ia,v3ia,v1ib,v2ib,
     &v3ib
      do 23000 k= 1,nk
         do 23002 j= 1,nj
            do 23004 i= 1,ni
               v1o(i,j,k)= v1ia(i,j,k) + v1ib(i,j,k)*rval
               v2o(i,j,k)= v2ia(i,j,k) + v2ib(i,j,k)*rval
               v3o(i,j,k)= v3ia(i,j,k) + v3ib(i,j,k)*rval
23004       continue
23002    continue
23000 continue
      return 
      end
      subroutine mul_vvvr( v1o,v2o,v3o, v1ia,v2ia,v3ia, v1ib,v2ib,v3ib, 
     &rval )
      use param
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o,v1ia,v2ia,v3ia,v1ib,v2ib,
     &v3ib
      do 23000 k= 1,nk
         do 23002 j= 1,nj
            do 23004 i= 1,ni
               v1o(i,j,k)= v1ia(i,j,k) * v1ib(i,j,k) * rval
               v2o(i,j,k)= v2ia(i,j,k) * v2ib(i,j,k) * rval
               v3o(i,j,k)= v3ia(i,j,k) * v3ib(i,j,k) * rval
23004       continue
23002    continue
23000 continue
      return 
      end
      subroutine swi_ss( s1, s2 )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: s1,s2
      real, dimension(ni,nj,nk) :: stmp
      stmp= s2
      s2= s1
      s1= stmp
      return 
      end
      subroutine swi_vv( v1ia,v2ia,v3ia, v1ib,v2ib,v3ib )
      use param
      real, dimension(ni,nj,nk) :: v1ia,v2ia,v3ia,v1ib,v2ib,v3ib
      call swi_ss( v1ia, v1ib )
      call swi_ss( v2ia, v2ib )
      call swi_ss( v3ia, v3ib )
      return 
      end
      subroutine swi_aa( a1, a2, nel )
      dimension a1(nel), a2(nel)
      dimension atmp(nel)
      atmp= a2
      a2= a1
      a1= atmp
      return 
      end
      subroutine swi_pvpv( pv1ia,pv2ia,pv3ia, pv1ib,pv2ib,pv3ib, nel )
      use param
      use cm_par
      dimension pv1ia(np),pv2ia(np),pv3ia(np),pv1ib(np),pv2ib(np),pv3ib(
     &np)
      call swi_aa( pv1ia, pv1ib, nel )
      call swi_aa( pv2ia, pv2ib, nel )
      call swi_aa( pv3ia, pv3ib, nel )
      return 
      end
      subroutine fix_v( v1o,v2o,v3o, v1i,v2i,v3i, ivtyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: v1o,v2o,v3o,v1i,v2i,v3i
      call fix_s( v1o, v1i, isvtyp(1,ivtyp) )
      call fix_s( v2o, v2i, isvtyp(2,ivtyp) )
      call fix_s( v3o, v3i, isvtyp(3,ivtyp) )
      return 
      end
      subroutine fix_s( so, si, istyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so,si
       
      if( istyp.lt.1 .or. istyp.gt.nstyp ) then
         print *, 'Error:  istyp<1 | istyp>nstyp in  fix_s  > exit'
         call exit(0)
      endif
      so = si
      if( istyp.eq.0 ) then
         return 
      endif
      do 23000 idim= 1,3 
c select
         i23002= ibstyp(1,idim,istyp) 
            go to 23002
c -- case 1
23004    continue
23005       go to 23003
c -- case 2
23006    continue
            call sfx_ss( so, si, idim, 2, nam2(idim), 1)
            call sfx_ss( so, si, idim, 1, nam3(idim), 1)
23007       go to 23003
c -- case 3
23008    continue
            call sfx_ss( so, si, idim, 2, 4, 1)
            call sfx_ss( so, si, idim, 1, 5, 1)
23009       go to 23003
c -- case 4
23010    continue
            call sfx_ss( so, si, idim, 2, 4, -1)
            call sfx_ss( so, si, idim, 1, 5, -1)
23011       go to 23003
c -- case 5
23012    continue
            call sfx_sr( so, idim, 2, a1styp(1,idim,istyp) )
            call sfx_sr( so, idim, 1, a2styp(1,idim,istyp) )
23013       go to 23003
c -- case 6
23014    continue
            call sfx_sr( so, idim, 3, 0. )
            call extrap_slp( so, idim, 3, 1)
23015       go to 23003
c -- case 7
23016    continue
            call fixo_s( so, si, idim )
23017       go to 23003
c -- case 8
23018    continue
            call fixo_v( so, si, idim )
23019       go to 23003
c -- case 9
23020    continue
            call extrap_s( so, idim, 2, 1, 1)
23021       go to 23003
c -- case 10
23022    continue
            call sfx_sr( so, idim, 3, 0. )
            call extrap_s( so, idim, 2, 1, 1)
23023       go to 23003
c -- case 11
23024    continue
            call extrap_slp( so, idim, 3, 1)
23025       go to 23003
c -- case 12
23026    continue
            call extrap_rslp( so, idim, 3, 1)
23027       go to 23003
c -- case 13
23028    continue
            call extrap4_s( so, idim, 2, 1)
            call extrap4_s( so, idim, 1, 1)
c -- dispatch area for select
23029    go to 23003
23002    continue
         if(i23002.lt. 1 .or. i23002.gt.13) go to 23003
         go to (23004,23006,23008,23010,23012,23014,23016,23018,23020,
     &23022,23024,23026,23028), i23002
23003    continue
c endselect
c select
         i23030= ibstyp(2,idim,istyp) 
            go to 23030
c -- case 1
23032    continue
            go to 23001
23033       go to 23031
c -- case 2
23034    continue
            call sfx_ss( so, si, idim, nam(idim), 3, 1)
            call sfx_ss( so, si, idim, na(idim), 4, 1)
23035       go to 23031
c -- case 3
23036    continue
            call sfx_ss( so, si, idim, nam(idim), nam3(idim), 1)
            call sfx_ss( so, si, idim, na(idim), nam4(idim), 1)
23037       go to 23031
c -- case 4
23038    continue
            call sfx_ss( so, si, idim, nam(idim), nam3(idim), -1)
            call sfx_ss( so, si, idim, na(idim), nam4(idim), -1)
23039       go to 23031
c -- case 5
23040    continue
            call sfx_sr( so, idim, nam(idim), a1styp(1,idim,istyp) )
            call sfx_sr( so, idim, na(idim), a2styp(1,idim,istyp) )
23041       go to 23031
c -- case 6
23042    continue
            call sfx_sr( so, idim, nam2(idim), 0. )
            call extrap_slp( so, idim, nam2(idim), -1)
23043       go to 23031
c -- case 7
23044    continue
            print *, 'this option is not defined for high end of dim.'
            stop 772
23045       go to 23031
c -- case 8
23046    continue
            print *, 'this option is not defined for high end of dim.'
            stop 882
23047       go to 23031
c -- case 9
23048    continue
            call extrap_s( so, idim, nam(idim), na(idim), -1)
23049       go to 23031
c -- case 10
23050    continue
            call sfx_sr( so, idim, nam2(idim), 0. )
            call extrap_s( so, idim, nam(idim), na(idim), -1)
23051       go to 23031
c -- case 11
23052    continue
            call extrap_slp( so, idim, nam2(idim), -1)
23053       go to 23031
c -- case 12
23054    continue
            call extrap_rslp( so, idim, nam2(idim), -1)
23055       go to 23031
c -- case 13
23056    continue
            call extrap4_s( so, idim, nam(idim), -1)
            call extrap4_s( so, idim, na(idim), -1)
c -- dispatch area for select
23057    go to 23031
23030    continue
         if(i23030.lt. 1 .or. i23030.gt.13) go to 23031
         go to (23032,23034,23036,23038,23040,23042,23044,23046,23048,
     &23050,23052,23054,23056), i23030
23031    continue
c endselect
23000 continue
23001 continue
      return 
      end
      subroutine sfx_ss( so, si, idim, io, ii, ipm )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so,si
       
      if( idim.lt.1 .or. idim.gt.3 ) then
         print *, 'Error:  idim<1 | idim>3  in  sfx_ss  > exit'
         call exit(0)
      endif
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         so(io,3:njm2,k3:nkm2)= ipm*si(ii,3:njm2,k3:nkm2)
23003    go to 23001
c -- case 2
23004 continue
         so(1:ni,io,k3:nkm2)= ipm*si(1:ni,ii,k3:nkm2)
23005    go to 23001
c -- case 3
23006 continue
         so(1:ni,1:nj,io)= ipm*si(1:ni,1:nj,ii)
c -- dispatch area for select
23007 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23004,23006), i23000
23001 continue
c endselect
      return 
      end
      subroutine sfx_sr( sa, idim, io, rval )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
       
      if( idim.lt.1 .or. idim.gt.3 ) then
         print *, 'Error:  idim<1 | idim>3  in  sfx_sr  > exit'
         call exit(0)
      endif
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         sa(io,3:njm2,k3:nkm2)= rval
23003    go to 23001
c -- case 2
23004 continue
         sa(1:ni,io,k3:nkm2)= rval
23005    go to 23001
c -- case 3
23006 continue
         sa(1:ni,1:nj,io)= rval
c -- dispatch area for select
23007 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23004,23006), i23000
23001 continue
c endselect
      return 
      end
      subroutine sfx_slp( sa, ilh, idim, istyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
      parameter (odsev=1./7.)
       
      if( idim.lt.1 .or. idim.gt.3 .or. ilh.lt.1 .or. ilh.gt.2 ) then
         print *, 
     &'Error:  idim<1 | idim>3 | ilh<1 | ilh>2  in  sfx_sr  > exit'
         call exit(0)
      endif
      if( ilh.eq.1 ) then
         io1= 2
         io2= 1
         ii1= 3
         ii2= 4
         slpdx= a1styp(ilh,idim,istyp)
         slpdx6= slpdx*6.
      else
         io1= nam(idim)
         io2= na(idim)
         ii1= nam2(idim)
         ii2= nam3(idim)
         slpdx= -a1styp(ilh,idim,istyp)
         slpdx6= slpdx*6.
      endif
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         sa(io1,3:njm2,k3:nkm2)= ( 8.*sa(ii1,3:njm2,k3:nkm2) -sa(ii2,3:
     &njm2,k3:nkm2) -slpdx6 )*odsev
         sa(io2,3:njm2,k3:nkm2)= sa(io1,3:njm2,k3:nkm2) -slpdx
23003    go to 23001
c -- case 2
23004 continue
         sa(1:ni,io1,k3:nkm2)= ( 8.*sa(1:ni,ii1,k3:nkm2) -sa(1:ni,ii2,k3
     &:nkm2) -slpdx6 )*odsev
         sa(1:ni,io2,k3:nkm2)= sa(1:ni,io1,k3:nkm2) -slpdx
23005    go to 23001
c -- case 3
23006 continue
         sa(1:ni,1:nj,io1)= ( 8.*sa(1:ni,1:nj,ii1) -sa(1:ni,1:nj,ii2) -
     &slpdx6 )*odsev
         sa(1:ni,1:nj,io2)= sa(1:ni,1:nj,io1) -slpdx
c -- dispatch area for select
23007 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23004,23006), i23000
23001 continue
c endselect
      return 
      end
      subroutine fixo_s( so, si, idim )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so,si
      dimension sum1(ni), sum2(ni), suma(ni), sumb(ni)
       
      if( idim.ne.2 ) then
         print *, 'Error:  idim<>2 in  fixo_s  > exit'
         call exit(0)
      endif
       
      if( rlo.gt.0. ) then
         print *, 'Error:  rlo>0. in  fixo_s  > exit'
         call exit(0)
      endif
      do 23000 i= 1,ni
         sum1(i)= 0.
         sum2(i)= 0.
         suma(i)= 0.
         sumb(i)= 0.
23000 continue
       
      do 23002 k= k3,nkm2
         do 23004 i= 1,ni
            sum1(i)= sum1(i) + 12./13.*( 3*si(i,3,k)-8./3.*si(i,4,k) +3.
     &/4.*si(i,5,k) )/nkd
            suma(i)= suma(i) + (3*si(i,3,k)-2*si(i,4,k)+0.5*si(i,5,k)) *
     &coss(k)/nkd
            sumb(i)= sumb(i) + (3*si(i,3,k)-2*si(i,4,k)+0.5*si(i,5,k)) *
     &sins(k)/nkd
23004    continue
23002 continue
      do 23006 k= k3,nkm2
         do 23008 i= 1,ni
            sum2(i)= sum2(i) + ( 0.25*sum1(i)+1.5*si(i,3,k)-si(i,4,k) +0
     &.25*si(i,5,k) )/nkd
23008    continue
23006 continue
      do 23010 k= k3,nkm2
         do 23012 i= 1,ni
            so(i,1,k)= sum1(i)
            so(i,2,k)= sum2(i) + suma(i)*coss(k) + sumb(i)*sins(k)
23012    continue
23010 continue
      return 
      end
      subroutine fixo_v( so, si, idim )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: so,si
      dimension sum2(ni),suma1(ni),sumb1(ni)
      dimension suma2(ni),sumb2(ni),sumaa2(ni),sumbb2(ni)
       
      if( idim.ne.2 ) then
         print *, 'Error:  idim<>2 in  fixo_v  > exit'
         call exit(0)
      endif
       
      if( rlo.gt.0. ) then
         print *, 'Error:  rlo>0. in  fixo_v  > exit'
         call exit(0)
      endif
      do 23000 i=1,ni
         sum2(i)= 0.
         suma1(i)= 0.
         sumb1(i)= 0.
         suma2(i)= 0.
         sumb2(i)= 0.
         sumaa2(i)= 0.
         sumbb2(i)= 0.
23000 continue
       
      do 23002 k= k3,nkm2
         do 23004 i= 1,ni
            sum2(i)= sum2(i) + ( 1.5*si(i,3,k) - si(i,4,k) + si(i,5,k)/4
     & )/nkd
            suma1(i)= suma1(i) + 12/13.*( 3*si(i,3,k)-8/3.*si(i,4,k) + 3
     &/4.*si(i,5,k) )*coss(k)/nkd
            sumb1(i)= sumb1(i) + 12/13.*( 3*si(i,3,k)-8/3.*si(i,4,k) + 3
     &/4.*si(i,5,k) )*sins(k)/nkd
            sumaa2(i)= sumaa2(i) + ( 1.5*si(i,3,k) - si(i,4,k) + si(i,5,
     &k)/4 )*cos2s(k)/nkd
            sumbb2(i)= sumbb2(i) + ( 1.5*si(i,3,k) - si(i,4,k) + si(i,5,
     &k)/4 )*sin2s(k)/nkd
23004    continue
23002 continue
      do 23006 k= k3,nkm2
         do 23008 i= 1,ni
            suma2(i)= suma2(i) + ( 0.25*suma1(i) + (1.5*si(i,3,k)-si(i,4
     &,k)+0.25*si(i,5,k))*coss(k) )/nkd
            sumb2(i)= sumb2(i) + ( 0.25*sumb1(i) + (1.5*si(i,3,k)-si(i,4
     &,k)+0.25*si(i,5,k))*sins(k) )/nkd
23008    continue
23006 continue
      do 23010 k= k3,nkm2
         do 23012 i= 1,ni
            so(i,1,k)= 2*suma1(i)*coss(k) + 2*sumb1(i)*sins(k)
            so(i,2,k)= sum2(i) + 2*suma2(i)*coss(k) + 2*sumb2(i)*sins(k)
     & + 2*sumaa2(i)*cos2s(k) + 2*sumbb2(i)*sin2s(k)
23012    continue
23010 continue
      return 
      end
      subroutine extrap_s( sa, idim, i2, i1, ipm )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
      dimension f2(3),f3(4)
       
      if( idim.lt.1 .or. idim.gt.3 ) then
         print *, 'Error:  idim<1 | idim>3  in  extrap_s  > exit'
         call exit(0)
      endif
       
      if( na(idim) .lt. 6 ) then
         print *, 'Error:  na(idim) < 6 in  extrap_s  > exit'
         call exit(0)
      endif
      f2(1)= 6.
      f2(2)= -8.
      f2(3)= 3.
      f3(1)= 4.
      f3(2)= -6.
      f3(3)= 4.
      f3(4)= -1.
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         do 23003 k= k3,nkm2
            do 23005 j= 3,njm2
               sa(i1,j,k)= f2(1)*sa(i1+ipm*2,j,k) +f2(2)*sa(i1+ipm*3,j,k
     &) +f2(3)*sa(i1+ipm*4,j,k)
               sa(i2,j,k)= f3(1)*sa(i2+ipm, j,k) +f3(2)*sa(i2+ipm*2,j,k)
     & +f3(3)*sa(i2+ipm*3,j,k) +f3(4)*sa(i2+ipm*4,j,k)
23005       continue
23003    continue
23007    go to 23001
c -- case 2
23008 continue
         do 23009 k= k3,nkm2
            do 23011 i= 1,ni
               sa(i,i1,k)= f2(1)*sa(i,i1+ipm*2,k) +f2(2)*sa(i,i1+ipm*3,k
     &) +f2(3)*sa(i,i1+ipm*4,k)
               sa(i,i2,k)= f3(1)*sa(i,i2+ipm, k) +f3(2)*sa(i,i2+ipm*2,k)
     & +f3(3)*sa(i,i2+ipm*3,k) +f3(4)*sa(i,i2+ipm*4,k)
23011       continue
23009    continue
23013    go to 23001
c -- case 3
23014 continue
         do 23015 j= 1,nj
            do 23017 i= 1,ni
               sa(i,j,i1)= f2(1)*sa(i,j,i1+ipm*2) +f2(2)*sa(i,j,i1+ipm*3
     &) +f2(3)*sa(i,j,i1+ipm*4)
               sa(i,j,i2)= f3(1)*sa(i,j,i2+ipm ) +f3(2)*sa(i,j,i2+ipm*2)
     & +f3(3)*sa(i,j,i2+ipm*3) +f3(4)*sa(i,j,i2+ipm*4)
23017       continue
23015    continue
c -- dispatch area for select
23019 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23008,23014), i23000
23001 continue
c endselect
      return 
      end
      subroutine extrap4_s( sa, idim, ii, ipm )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
      dimension fa(5)
       
      if( idim.lt.1 .or. idim.gt.3 ) then
         print *, 'Error:  idim<1 | idim>3  in  extrap_s  > exit'
         call exit(0)
      endif
       
      if( na(idim) .lt. 7 ) then
         print *, 'Error:  na(idim) < 7 in  extrap_s  > exit'
         call exit(0)
      endif
      fa(1)= 5.
      fa(2)= -10.
      fa(3)= 10.
      fa(4)= -5.
      fa(5)= 1.
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         do 23003 k= k3,nkm2
            do 23005 j= 3,njm2
               sa(ii,j,k)= fa(1)*sa(ii+ipm, j,k) +fa(2)*sa(ii+ipm*2,j,k)
     & +fa(3)*sa(ii+ipm*3,j,k) +fa(4)*sa(ii+ipm*4,j,k) +fa(5)*sa(ii+ipm*
     &5,j,k)
23005       continue
23003    continue
23007    go to 23001
c -- case 2
23008 continue
         do 23009 k= k3,nkm2
            do 23011 i= 1,ni
               sa(i,ii,k)= fa(1)*sa(i,ii+ipm, k) +fa(2)*sa(i,ii+ipm*2,k)
     & +fa(3)*sa(i,ii+ipm*3,k) +fa(4)*sa(i,ii+ipm*4,k) +fa(5)*sa(i,ii+
     &ipm*5,k)
23011       continue
23009    continue
23013    go to 23001
c -- case 3
23014 continue
         do 23015 j= 1,nj
            do 23017 i= 1,ni
               sa(i,j,ii)= fa(1)*sa(i,j,ii+ipm ) +fa(2)*sa(i,j,ii+ipm*2)
     & +fa(3)*sa(i,j,ii+ipm*3) +fa(4)*sa(i,j,ii+ipm*4) +fa(5)*sa(i,j,ii+
     &ipm*5)
23017       continue
23015    continue
c -- dispatch area for select
23019 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23008,23014), i23000
23001 continue
c endselect
      return 
      end
      subroutine extrap_slp( sa, idim, ii, ipm )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
      dimension f3(4)
       
      if( idim.lt.1 .or. idim.gt.3 ) then
         print *, 'Error:  idim<1 | idim>3  in  extrap_slp  > exit'
         call exit(0)
      endif
       
      if( na(idim) .lt. 6 ) then
         print *, 'Error:  na(idim) < 6 in  extrap_slp  > exit'
         call exit(0)
      endif
      f3(1)= 4.
      f3(2)= -6.
      f3(3)= 4.
      f3(4)= -1.
      r1= d(-2*ipm,idim)
      r2= d(-ipm, idim)
      r3= d( 0, idim)
      r4= d( ipm, idim)
      r5= d( 2*ipm,idim)
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         do 23003 k= k3,nkm2
            do 23005 j= 3,njm2
               sa(ii-ipm,j,k)= - ( (r3+r1*f3(2))*sa(ii, j, k) +(r4+r1*f3
     &(3))*sa(ii+ipm, j,k) +(r5+r1*f3(4))*sa(ii+ipm*2,j,k) ) / (r2+r1*f3
     &(1))
               sa(ii-ipm*2,j,k)= f3(1)*sa(ii-ipm,j,k) + f3(2)*sa(ii ,j,k
     &) + f3(3)*sa(ii+ipm,j,k) + f3(4)*sa(ii+ipm*2,j,k)
23005       continue
23003    continue
23007    go to 23001
c -- case 2
23008 continue
         do 23009 k= k3,nkm2
            do 23011 i= 1,ni
               sa(i,ii-ipm,k)= - ( (r3+r1*f3(2))*sa(i,ii,k) +(r4+r1*f3(3
     &))*sa(i,ii+ipm, k) +(r5+r1*f3(4))*sa(i,ii+ipm*2,k) ) / (r2+r1*f3(1
     &))
               sa(i,ii-ipm*2,k)= f3(1)*sa(i,ii-ipm,k) + f3(2)*sa(i,ii ,k
     &) + f3(3)*sa(i,ii+ipm,k) + f3(4)*sa(i,ii+ipm*2,k)
23011       continue
23009    continue
23013    go to 23001
c -- case 3
23014 continue
         do 23015 j= 1,nj
            do 23017 i= 1,ni
               sa(i,j,ii-ipm)= - ( (r3+r1*f3(2))*sa(i,j,ii) +(r4+r1*f3(3
     &))*sa(i,j,ii+ipm) +(r5+r1*f3(4))*sa(i,j,ii+ipm*2) ) / (r2+r1*f3(1)
     &)
               sa(i,j,ii-ipm*2)= f3(1)*sa(i,j,ii-ipm) + f3(2)*sa(i,j,ii 
     &) + f3(3)*sa(i,j,ii+ipm) + f3(4)*sa(i,j,ii+ipm*2)
23017       continue
23015    continue
c -- dispatch area for select
23019 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23008,23014), i23000
23001 continue
c endselect
      return 
      end
      subroutine extrap_rslp( sa, idim, ii, ipm ) 
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
      dimension f3(4)
       
      if( idim.lt.1 .or. idim.gt.3 ) then
         print *, 'Error:  idim<1 | idim>3  in  extrap_rslp  > exit'
         call exit(0)
      endif
       
      if( na(idim) .lt. 6 ) then
         print *, 'Error:  na(idim) < 6 in  extrap_rslp  > exit'
         call exit(0)
      endif
      f3(1)= 4.
      f3(2)= -6.
      f3(3)= 4.
      f3(4)= -1.
      r1= d(-2*ipm,idim)
      r2= d(-ipm, idim)
      r3= d( 0, idim)
      r4= d( ipm, idim)
      r5= d( 2*ipm,idim)
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         do 23003 k= k3,nkm2
            do 23005 j= 3,njm2
               sa(ii-ipm,j,k)= - ( (r3+r1*f3(2))*sa(ii, j, k) +(r4+r1*f3
     &(3))*sa(ii+ipm, j,k) +(r5+r1*f3(4))*sa(ii+ipm*2,j,k) ) / (r2+r1*f3
     &(1))
               sa(ii-ipm*2,j,k)= f3(1)*sa(ii-ipm,j,k) + f3(2)*sa(ii ,j,k
     &) + f3(3)*sa(ii+ipm,j,k) + f3(4)*sa(ii+ipm*2,j,k)
23005       continue
23003    continue
23007    go to 23001
c -- case 2
23008 continue
          
         if( r(ii-ipm*2).eq.0. ) then
            print *, 'Error:  r(ii-ipm*2)=0. in  extrap_rslp  > exit'
            call exit(0)
         endif
         do 23009 k= k3,nkm2
            do 23011 i= 1,ni
               sa(i,ii-ipm,k)= -( (r3+r1*f3(2))*sa(i,ii,k)*r(ii) +(r4+r1
     &*f3(3))*sa(i,ii+ipm, k)*r(ii+ipm) +(r5+r1*f3(4))*sa(i,ii+ipm*2,k)*
     &r(ii+ipm*2) ) / ( (r2+r1*f3(1))*r(ii-ipm) )
               sa(i,ii-ipm*2,k)= ( f3(1)*sa(i,ii-ipm,k)*r(ii-ipm) + f3(2
     &)*sa(i,ii ,k)*r(ii) + f3(3)*sa(i,ii+ipm,k)*r(ii+ipm) + f3(4)*sa(i,
     &ii+ipm*2,k)*r(ii+ipm*2) ) /r(ii-ipm*2)
23011       continue
23009    continue
23013    go to 23001
c -- case 3
23014 continue
         do 23015 j= 1,nj
            do 23017 i= 1,ni
               sa(i,j,ii-ipm)= - ( (r3+r1*f3(2))*sa(i,j,ii) +(r4+r1*f3(3
     &))*sa(i,j,ii+ipm) +(r5+r1*f3(4))*sa(i,j,ii+ipm*2) ) / (r2+r1*f3(1)
     &)
               sa(i,j,ii-ipm*2)= f3(1)*sa(i,j,ii-ipm) + f3(2)*sa(i,j,ii 
     &) + f3(3)*sa(i,j,ii+ipm) + f3(4)*sa(i,j,ii+ipm*2)
23017       continue
23015    continue
c -- dispatch area for select
23019 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23008,23014), i23000
23001 continue
c endselect
      return 
      end
      subroutine dfix_s( sa, istyp )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
       
      if( istyp.lt.1 .or. istyp.gt.nstyp ) then
         print *, 'Error:  istyp<1 | istyp>nstyp in  dfix_s  > exit'
         call exit(0)
      endif
      do 23000 idim= 1,3 
c select
         i23002= ibstyp(1,idim,istyp) 
            go to 23002
c -- case 1
23004    continue
23005       go to 23003
c -- case 2
23006    continue
            call dfx_s( sa, idim, 3, nam(idim), 1)
23007       go to 23003
c -- case 3
23008    continue
            call dfx_s( sa, idim, 4, 2, 1)
            call dfx_s( sa, idim, 3, 3, 1)
23009       go to 23003
c -- case 4
23010    continue
            call dfx_s( sa, idim, 4, 2, -1)
            call sfx_sr( sa, idim, 3, 0. )
23011       go to 23003
c -- case 5
23012    continue
            print *, 'istyp=5 not supported in dfix_s'
            stop 8171
23013       go to 23003
c -- case 6
23014    continue
            print *, 'istyp=6 not supported in dfix_s'
            stop 8172
23015       go to 23003
c -- case (default)
23016    continue
            print *, 'istyp>6 not supported in dfix_s'
            stop 8172
c -- dispatch area for select
23017    go to 23003
23002    continue
         if(i23002.lt. 1 .or. i23002.gt.6) go to 23016
         go to (23004,23006,23008,23010,23012,23014), i23002
23003    continue
c endselect
c select
         i23018= ibstyp(2,idim,istyp) 
            go to 23018
c -- case 1
23020    continue
23021       go to 23019
c -- case 2
23022    continue
            call dfx_s( sa, idim, nam2(idim), 2, 1)
23023       go to 23019
c -- case 3
23024    continue
            call dfx_s( sa, idim, nam3(idim), nam(idim), 1)
            call dfx_s( sa, idim, nam2(idim), nam2(idim), 1)
23025       go to 23019
c -- case 4
23026    continue
            call dfx_s( sa, idim, nam3(idim), nam(idim), -1)
            call sfx_sr( sa, idim, nam2(idim), 0. )
23027       go to 23019
c -- case 5
23028    continue
            print *, 'istyp=5 not supported in dfix_s'
            stop 8173
23029       go to 23019
c -- case 6
23030    continue
            print *, 'istyp=6 not supported in dfix_s'
            stop 8174
23031       go to 23019
c -- case (default)
23032    continue
            print *, 'istyp>6 not supported in dfix_s'
            stop 8174
c -- dispatch area for select
23033    go to 23019
23018    continue
         if(i23018.lt. 1 .or. i23018.gt.6) go to 23032
         go to (23020,23022,23024,23026,23028,23030), i23018
23019    continue
c endselect
23000 continue
      return 
      end
      subroutine dfx_s( sa, idim, io, ii, ipm )
      use param
      use cm_cmvec
      use cm_fevec
      real, dimension(ni,nj,nk) :: sa
       
      if( idim.lt.1 .or. idim.gt.3 ) then
         print *, 'Error:  idim<1 | idim>3  in  dfx_s  > exit'
         call exit(0)
      endif
c select
      i23000= idim 
         go to 23000
c -- case 1
23002 continue
         if( ipm.eq.-1 ) then
            sa(io,:,:)= sa(io,:,:) -sa(ii,:,:)
         else
            sa(io,:,:)= sa(io,:,:) +sa(ii,:,:)
         endif
23003    go to 23001
c -- case 2
23004 continue
         if( ipm.eq.-1 ) then
            sa(3:nim2,io,:)= sa(3:nim2,io,:) -sa(3:nim2,ii,:)
         else
            sa(3:nim2,io,:)= sa(3:nim2,io,:) +sa(3:nim2,ii,:)
         endif
23005    go to 23001
c -- case 3
23006 continue
         if( ipm.eq.-1 ) then
            sa(3:nim2,3:njm2,io)= sa(3:nim2,3:njm2,io) -sa(3:nim2,3:njm2
     &,ii)
         else
            sa(3:nim2,3:njm2,io)= sa(3:nim2,3:njm2,io) +sa(3:nim2,3:njm2
     &,ii)
         endif
c -- dispatch area for select
23007 go to 23001
23000 continue
      if(i23000.lt. 1 .or. i23000.gt.3) go to 23001
      go to (23002,23004,23006), i23000
23001 continue
c endselect
      return 
      end
      subroutine maxabs_rs(rmax,s)
      use param
      real, dimension(ni,nj,nk) :: s
      rmax= 0.
      do 23000 k= k3,nkm2
         do 23002 j= 3,njm2
            do 23004 i= 3,nim2
               if( abs(s(i,j,k)).gt.rmax ) rmax= abs(s(i,j,k))
23004       continue
23002    continue
23000 continue
      return 
      end
      subroutine maxabs1_rs(rmax,s)
      use param
      real, dimension(ni,nj,nk) :: s
      rmax= 0.
      k= k3
      do 23000 i= 7,ni-5
         do 23002 j= 7,nj-5
            if( abs(s(i,j,k)).gt.rmax ) rmax= abs(s(i,j,k))
23002    continue
23000 continue
      return 
      end
      subroutine sum_rs(rsum,s)
      use param
      use cm_par
      real, dimension(ni,nj,nk) :: s
      rsum= 0.
      do 23000 k= k3,nkm2
         do 23002 j= 3,njm2
            do 23004 i= 3,nim2
               rsum= rsum + s(i,j,k)
23004       continue
23002    continue
23000 continue
      if( r1in.eq.0. ) then
         do 23006 k= k3,nkm2
            do 23008 i= 3,nim2
               rsum= rsum + s(i,2,k)
23008       continue
23006    continue
      endif
      return 
      end
      FUNCTION RAN1(IDUM)
      DIMENSION RRN(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RMM2=7.4373773E-6)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      SAVE IX1,IX2,IX3,RRN
      DATA IFF /0/ 
      IF (IDUM.LT.0.OR.IFF.EQ.0) THEN
      IFF=1
      IX1=MOD(IC1-IDUM,M1)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IX1,M2)
      IX1=MOD(IA1*IX1+IC1,M1)
      IX3=MOD(IX1,M3)
      DO 11 J=1,97
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      RRN(J)=(REAL(IX1)+REAL(IX2)*RMM2)*RM1
   11 CONTINUE
      IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
       
      if( J.GT.97.OR.J.LT.1 ) then
         print *, 'Error: J.GT.97.OR.J.LT.1 in  RAN1 > exit'
         call exit(0)
      endif
      RAN1=RRN(J)
      RRN(J)=(REAL(IX1)+REAL(IX2)*RMM2)*RM1
      RETURN
      END
      FUNCTION ran2(idum)
      implicit none
      integer, parameter :: k4b=selected_int_kind(9)
      integer(k4b), intent(inout) :: idum
      real :: ran2
      call RANDOM_NUMBER(ran2)
      END FUNCTION ran2
      FUNCTION ran(idum)
      implicit none
      integer, parameter :: k4b=selected_int_kind(9)
      integer(k4b), intent(inout) :: idum
      real :: ran
      integer(k4b), parameter :: ia=16807,im=2147483647,iq=127773,ir=
     &2836
      real*4, save :: am
      integer(k4b), save :: ix=-1,iy=-1,k
      if(idum .le. 0 .or. iy .lt. 0 ) then
         am= nearest(1.0,-1.0)/im
         iy= ior(ieor(888889999,abs(idum)),1)
         ix= ieor(777755555,abs(idum))
         idum= abs(idum)+1
      endif
      ix= ieor(ix,ishft(ix,13))
      ix= ieor(ix,ishft(ix,-17))
      ix= ieor(ix,ishft(ix,5))
      k= iy/iq
      iy= ia*(iy-k*iq)-ir*k
      if( iy.lt.0 ) iy= iy+im
      ran= dble( am*ior(iand(im,ieor(ix,iy)),1))
      END FUNCTION ran
      FUNCTION BESSJ0(X)
      REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6, S1,S2,S3
     &,S4,S5,S6
      DATA P1,P2,P3,P4,P5/1.0,-.1098628627E-2,.2734510407E-4, -.
     &2073370639E-5,.2093887211E-6/, Q1,Q2,Q3,Q4,Q5/-.1562499995E-1, .
     &1430488765E-3,-.6911147651E-5,.7621095161E-6,-.934945152E-7/
      DATA R1,R2,R3,R4,R5,R6/57568490574.0,-13362590354.0,651619640.70, 
     &-11214424.180,77392.330170,-184.90524560/, S1,S2,S3,S4,S5,S6/
     &57568490411.0,1029532985.0, 9494680.7180,59272.648530,267.85327120
     &,1.0/
      IF(ABS(X).LT.8.)THEN
      Y=X**2
      BESSJ0=(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6))))) /(S1+Y*(S2+Y*(S3+Y*(
     &S4+Y*(S5+Y*S6)))))
      ELSE
      AX=ABS(X)
      Z=8./AX
      Y=Z**2
      XX=AX-.785398164
      BESSJ0=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y *P5)))
     &)-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
      ENDIF
      RETURN
      END
      FUNCTION BESSJ1(X)
      REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6, S1,S2,S3
     &,S4,S5,S6
      DATA R1,R2,R3,R4,R5,R6/72362614232.0,-7895059235.0,242396853.10, -
     &2972611.4390,15704.482600,-30.160366060/, S1,S2,S3,S4,S5,S6/
     &144725228442.0,2300535178.0, 18583304.740,99447.433940,376.
     &99913970,1.0/
      DATA P1,P2,P3,P4,P5/1.0,.183105E-2,-.3516396496E-4,.2457520174E-5,
     & -.240337019E-6/, Q1,Q2,Q3,Q4,Q5/.046874999950,-.2002690873E-3, .
     &8449199096E-5,-.88228987E-6,.105787412E-6/
      IF(ABS(X).LT.8.)THEN
      Y=X**2
      BESSJ1=X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6))))) /(S1+Y*(S2+Y*(S3+Y*
     &(S4+Y*(S5+Y*S6)))))
      ELSE
      AX=ABS(X)
      Z=8./AX
      Y=Z**2
      XX=AX-2.356194491
      BESSJ1=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y *P5)))
     &)-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5))))) *SIGN(1.,X)
      ENDIF
      RETURN
      END
      FUNCTION BESSJ(N,X)
      PARAMETER (IACC=40,BIGNO=1.E10,BIGNI=1.E-10)
      IF(N.LT.2) print *, 'bad argument N in BESSJ'
      AX=ABS(X)
      IF(AX.EQ.0.)THEN
      BESSJ=0.
      ELSE IF(AX.GT.REAL(N))THEN
      TOX=2./AX
      BJM=BESSJ0(AX)
      BJ=BESSJ1(AX)
      DO 11 J=1,N-1
      BJP=J*TOX*BJ-BJM
      BJM=BJ
      BJ=BJP
   11 CONTINUE
      BESSJ=BJ
      ELSE
      TOX=2./AX
      M=2.*((N+INT(SQRT(REAL(IACC*N))))/2)
      BESSJ=0.
      JSUM=0
      SUM=0.
      BJP=0.
      BJ=1.
      DO 12 J=M,1,-1
      BJM=J*TOX*BJ-BJP
      BJP=BJ
      BJ=BJM
      IF(ABS(BJ).GT.BIGNO)THEN
      BJ=BJ*BIGNI
      BJP=BJP*BIGNI
      BESSJ=BESSJ*BIGNI
      SUM=SUM*BIGNI
      ENDIF
      IF(JSUM.NE.0)SUM=SUM+BJ
      JSUM=1-JSUM
      IF(J.EQ.N)BESSJ=BJP
   12 CONTINUE
      SUM=2.*SUM-BJ
      BESSJ=BESSJ/SUM
      ENDIF
      IF(X.LT.0..AND.MOD(N,2).EQ.1)BESSJ=-BESSJ
      RETURN
      END
      FUNCTION BESSI1(X)
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7, Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0, 0.
     &15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1, -0.
     &362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1, -0.2895312D-1,0.
     &1787654D-1,-0.420059D-2/
      IF (ABS(X).LT.3.75) THEN
      Y=(X/3.75)**2
      BESSI1=X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
      AX=ABS(X)
      Y=3.75/AX
      BESSI1=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+ Y*(Q5+Y*(Q6+Y*(Q7
     &+Y*(Q8+Y*Q9))))))))
      IF(X.LT.0.) BESSI1=-BESSI1
      ENDIF
      RETURN
      END
      SUBROUTINE THOMAS(KKK,A,B,C,R,N)
      DIMENSION A(N),B(N),C(N),R(N)
      IF(KKK.EQ.2)GO TO 45
      C(1)=C(1)/B(1)
      DO 40 I=2,N
      IM=I-1
      C(I)=C(I)/(B(I)-A(I)*C(IM))
   40 CONTINUE
      IF(KKK.EQ.1)RETURN
   45 CONTINUE
      R(1)=R(1)/B(1)
      DO 41 I=2,N
      IM=I-1
      R(I)=(R(I)-A(I)*R(IM))/(B(I)-A(I)*C(IM))
   41 CONTINUE
      DO 50 I=1,N-1
      J=N-I
      JP=J+1
      R(J)=R(J)-C(J)*R(JP)
   50 CONTINUE
      RETURN
      END
      FUNCTION CEL(QQC,PP,AA,BB)
      PARAMETER (CA=.0003, PI02=1.5707963268)
      IF(QQC.EQ.0.) THEN
      WRITE(6,*) 'failure in CEL'
      STOP 1122
      ENDIF
      QC=ABS(QQC)
      A=AA
      B=BB
      P=PP
      E=QC
      EM=1.
      IF(P.GT.0.) THEN
      P=SQRT(P)
      B=B/P
      ELSE
      F=QC*QC
      Q=1.-F
      G=1.-P
      F=F-P
      Q=Q*(B-A*P)
      P=SQRT(F/G)
      A=(A-B)/G
      B=-Q/(G*G*P)+A*P
      ENDIF
    1 F=A
      A=A+B/P
      G=E/P
      B=B+F*G
      B=B+B
      P=G+P
      G=EM
      EM=QC+EM
      IF(ABS(G-QC).GT.G*CA) THEN
      QC=SQRT(E)
      QC=QC+QC
      E=QC*EM
      GO TO 1
      ENDIF
      CEL=PI02*(B+A*EM)/(EM*(EM+P))
      RETURN
      END
