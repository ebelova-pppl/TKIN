#     Revision Date 0110
! start up code for tk (nstx) runs, no frc stuff.
# Rev 02/03: moved to SP, use CMPLR=f90 and dbl precision.
!            added MPI option with nprocs>=2.

      define CMPLR  f90                # f77 or f90.

      define DIMEN   3                 # Is code 2D or 3D?

      define NID     120                ! Number of data i positions.
      define NJD     60                ! Number of data j positions.
      define NKD     64                ! Number of data k positions --
                                        !  ---if DIMEN=3.
      define LBULK T                   # Step bulk momentum and density.
      define LMHD  F                   # mhd (no particles) option.
      define LCYL  T                   # 3D cylindrical geometry, where
                                       #  q -> z ; r -> r ; s -> phi.
      define LMPI  T                   # =T use MPI calls.

      define NPX 100                   # Maximum number of particles-
                                    ! -not used anymore 12/2015.
      define NSPX 1                    # Maximum number of species.

      define LIN     T                 # Linearized mhd.
      define LDF     T                 # =T use df scheme.
      define LPGRID  F                 # Different grid for particles.

      ifelse(LCYL,T,[
        ifelse(LMHD,F,[ifelse(DIMEN,3,[
          define LPGRID T
          define NXD     71           # For 3D cylinder define particle 
          define NYD     71           ! grid (z=q, x, y): NIDxNXDxNYD.
        ],)],)
      ],)

      define MODE   0                # s-mode number (=N if none).

      define NCHARFLD 24               ! # of characters in field names
      define PART_VEL_CHECK T          # Check particle steps to see
                                       !  if they are too big?
      define NTERP 2                   # Interpolation for particle
                                       !weighting, linear (2), quad (3).
      define SORDER 4             # Use forth (4) or second (2) order
                                  ! accuracy in space (diff & int & bc).
      
      define LOAD   1         # =0 particles are loaded uniformly (in x)
                              ! =1 nonuniform loading (inverse density).
      define LDENST F         # =T uniform density, =F density profile.
      define PARTONGRID F     # Place particles on grid points.
      define lRAN1 T          # Use ran1 routine for random numbers.

      define Hq_1    TRUE                 # Is v1h = 1 ? (TRUE for True)
      define Hr_1    TRUE                 # Is v2h = 1 ? (TRUE for True)
      ifelse(LCYL,T,[
        define Hs_1    FALSE        
        define HALL_1  FALSE
      ],[
        define Hs_1    TRUE               # Is v3h = 1 ?
        define HALL_1  TRUE               # Are all h = 1 ?
      ])

      define SBC_no  1         # 1 is do nothing
      define VBC_no  1

      ifelse(LCYL,T,[
        define SBC     16       #(9)(13) bc for cylindrical geometry
        define VBC_v   18       #(7)(3) for V
        define VBC_b   22       #(9)(4) for B
        define VBC_e   19       #(10)(4) for E
        define VBC_j   21       # for J
        define VBC_a   20       #(4)(4) for A
      ],[
        define SBC   2         # if not cylinder, then periodic bc
        define VBC_v 2
        define VBC_b 2
        define VBC_e 2
        define VBC_j 2
        define VBC_a 2
      ])
                             #  NOTE: ran() does not work with gnu compiler.
                             ! rand works better than ran1
      ifelse(lRAN1,T,[define RAN ran2(idum)],[define RAN rand()])
c     ifelse(lRAN1,T,[define RAN ran1(idum)],[define RAN rand()])
      define WORDLENGTH 8
      define ONEPLUS 1.0000000000001
      define ZERO    0.0000000000001
      define NVEC  1

# CDIR macro to include Cray compiler directive macros.

      define(CDIR,[Immediate(cdir$$ $1)])


# V macro (to expand vector):

      define(V,[v1[]$1,v2[]$1,v3[]$1])

# PV macro (to expand particle vector):

      define(PV,[pv1[]$1,pv2[]$1,pv3[]$1])

# PVx macro (to expand particle position vector):

      ifelse(DIMEN,3,[
        define(PVx,[pv1[]$1,pv2[]$1,pv3[]$1])
      ],[
        define(PVx,[pv1[]$1,pv2[]$1])
      ])

# PARAM macro:

      define(PARAM,[
        use param
      ])
      module param
        parameter(
     &    nid= NID,    # Num of data grid pts in 1st (i) dimension.
     &    njd= NJD, ifelse(DIMEN,3,nkd=NKD,nkd=1),
     &    nijkd= nid*njd*nkd,
     &    ni= nid +4,     # Total dimension size.
     &    nim= ni -1, nim2= ni -2, nim3= ni -3, nim4= ni -4,
     &    nj= njd +4, njm= nj -1, njm2= nj -2, njm3= nj -3,
     &      njm4= nj -4,
     &    nij= ni*nj,
          ifelse(DIMEN,3,[
     &      k2= 2, k3= 3, k3m= 2, nk= nkd +4, nkm= nk -1,
     &      nkm2= nk -2, nkm3= nk -3, nkm4= nk -4,
          ],[
     &      k2=1, k3=1, k3m=1, nk=1, nkm=1, nkm2=1, nkm3=1, nkm4=1,
          ])
     &    nijk= ni*nj*nk,
     &    nstyp= 21,        # Max number of scalar bc types.
     &    nvtyp= 22,        # Max number of vector bc types.
     &    npx= NPX,        # Max number of particles.
     &    nspx= NSPX,      # Max number of species.
     &    nvec= NVEC,      # Vector length for vector operations.
     &    nvecm= nvec -1, nvecp= nvec +1 ) # nvec - 1, and nvec + 1.
      end module param

# COMMON_PARAM

      define(COMMON_PARAM,[
        use cm_par
      ])
      module cm_par     # Various parameters.
        real ::
     &    beta,              # Plasma beta.  Normalization for straight
                             !  coordinates with constant V(b) and sp:
                             !  If V(b)=1, sp will be beta/2, and the
                             !  average sp energy will be beta.
     &    dq,                # Grid step size for 1st (i) dimension.
     &    dqi,               # 1./dq.
     &    dr,                # Grid step size for 2nd (j) dimension.
     &    dri,               # 1./dr.
     &    ds,                # Grid step size for 3rd (k) dimension.
     &    dsi,               # 1./ds.
     &    gamma,             # Ratio of specific heats.
     &    odgamma,           # 1./gamma.
     &    rpi,               # pi.
     &    rpi2,              # pi*2.
     &    rpid180,           # pi/180 (for convers. of degrees to rads)
     &    q1in,              # Value of q at i=ri1in.
     &    q2in,              # Value of q at i corresponding to ri2in.
     &    r1in,              # Value of r at j=rj1in.
     &    r2in,
     &    s1in,
     &    s2in,
     &    ri1in,             # See def of q1in.
     &    ri2in,             # ri2in is entered as a negative number,
                      ! and counts backwards from ni.  ri2in=-1, -2,
                      ! and -3 indicate i=ni, ni-1, and ni-2.
     &    rj1in,              # See def of r1in.
     &    rj2in,
     &    rk1in,
     &    rk2in,
     &    aw1,               # Coefficients needed for integration.
     &    aw2,               !
     &    aw3,               !
     &    aw4,               !
     &    t,                 # Time.
     &    tdgammam,          # 2./(gamma-1.).
     &    rhoc,              # Cutoff density.
     &    pcut,              # Cutoff pressure.
     &    vpert,             # velocity perturbation.
     &    zbdzeff            # Beam ion to plasma charge ratio Zb/Zeff.
        integer ::
     &    its,               # Time step index.
     &    idrun,             # Run identifier.
     &    ialign,            # ialign=1 to align particle parallel
                             ! velocity with q direction, 2 for r
                             ! direction, and 3 for s direction.
                             ! Otherwise, will be alignd with local
                             ! magnetic field.
     &    idum,              # Seed for random number generator.
     &    np                 # Total number of particles in run.
      end module cm_par

# COMMON_CMVEC

      define(COMMON_CMVEC,[
        use cm_cmvec
      ])
      module cm_cmvec     # Quantities needed for vector operations.
        real, allocatable, dimension(:,:,:) ::
     &   sh3,                 # Prod of all 3 comp of h.
     &   sh3w,                # Prod of all 3 comp of h * weights
     &   sh3i,                # Inverse of prod of all 3 comp of h.
     &   t12hder,             # tijhder= d(h_i)/d(q_j) / (h_i * h_j)
     &   t13hder,
     &   t21hder,
     &   t23hder,
     &   t31hder,
     &   t32hder,
     &   V(h),                # Vector h (scale factors).
     &   V(hi),               # 1./h.
     &   V(h2),               # Each comp = prod of other 2.
     &   V(h2i),              # 1./h2.
     &   V(h2d1)              # h2/h.
      end module cm_cmvec

# COMMON_FEVEC

      define(COMMON_FEVEC,[
        use cm_fevec
      ])
      module cm_fevec       # Quantities needed for
                                  !  vector operations.
        use param
        real ::
     &   a1styp(2,3,nstyp),       # 1st const associated w/ scal typ.
     &   a2styp(2,3,nstyp),       # 2nd const associated w/ scal typ.
     &   d(-2:2,3),               # Coefficients for derivatives.
     &   h3sum,                   # sum of Jacobian, prop to system vol
     &   q(ni),                   # 1st coordinate positions.
     &   qlo,                     # q at 2 1/2 position.
     &   qhi,                     # q at ni - 2 + 1/2 position.
     &   qlen,                    # qhi - qlo.
     &   r(nj),                   # 2nd coordinate positions.
     &   rlo,                     # r_min.
     &   rhi,                     # r_max.
     &   rlen,                    # rhi - rlo.
     &   s(nk),                   # 3rd coordinate positions.
     &   slo,                     # s at 2 1/2 position.
     &   shi,                     # s at ni - 2 + 1/2 position.
     &   slen,                    # shi - slo.
     &   sins(nk),                # sin(s(k)).
     &   coss(nk),                # cos(s(k)).
     &   sin2s(nk),               # sin(2*s(k)).
     &   cos2s(nk),               # cos(2*s(k)).
     &   wq(ni),                  # Weights for integration, q direct.
     &   wr(nj),
     &   ws(nk) 
        integer ::
     &   ibstyp(2,3,nstyp),       # Boundary cond (bc) type.
     &   isvtyp(3,nvtyp),         # Scal typ for each dimen of vec typ.
     &   na(3),                   # = ni,nj,nk.
     &   nam(3),                  # na -1
     &   nam2(3),                 # na -2
     &   nam3(3),                 # na -3
     &   nam4(3)                  # na -4
      end module cm_fevec

# COMMON_VEC

      define(COMMON_VEC,[
        COMMON_CMVEC
        COMMON_FEVEC
      ])

# COMMON_FLD

      define(COMMON_FLD,[
        use cm_fld             # Field module.
      ])
      module cm_fld            # global field data.
        real, allocatable, dimension(:,:,:) ::
     &    sp,               # Pressure = sp1**gamma.
     &    sp1,              # Pressure variable at time 1.
                                 !  =pressure**(1/gamma).
     &    srhob1,           # Bulk plasma mass density at time 1.
     &    srhoq,            # Particle ion charge density.
     &    srhoq0,           # Zero order ion charge density.
     &    spi0,             # Zero order ion stress tensor.
     &    spsi,             # Poloidal flux.
     &    sphi,             # Toroidal flux (normalized).
     &    rmask,            # =1 inside pc shell, =0 outside.
     &    sres,             # Normalized resistivity profile.
     &    sg,               # poloidal stream function.
     &    V(b1),            # Vector mag. field at time 1.
     &    V(a),             # Vector potential.
     &    V(cur),           # Vector current.
     &    V(curi),          # Ion current.
     &    V(curi0),         # Zero order ion current.
     &    V(e),             # Vector electric field.
     &    V(mo1),           # Vector momentum at time 1.
     &    V(ub),            # Vector bulk fluid velocity.
     &    V(vhypa)          # Vector field modified for hyp. term
      end module cm_fld

# COMMON_PART

      define(COMMON_PART,[
        use cm_part              # Particle module.
      ])
      module cm_part             # global arrays in tkin.
        real, allocatable, dimension(:) ::
     &    PV(x1),                # Vector position, time 1.
     &    PV(v1),                # Vector velocity, time 1.
     &    pspphi,                # p_phi (for loading).
     &    psr,                   # initial R.
     &    psmu,                  # mu + corrections.
     &    psw,                   # Weight.
     &    psp                    # Another weight.
      end module cm_part

# COMMON_SPEC

      define(COMMON_SPEC,[
        use cm_spec
      ])
      module cm_spec      # Species dependent quantities.
        real, dimension(NSPX) ::
     &    dennsp,             # Dens. of species (normal. to nucleons).
     &    qnsp,               # Charge of real species particle (norm-
                              !  alized to H).
     &    qsp,                # Charge of ion species microparticle.
     &    tratsp,             # T-perp_background/T-par.
     &    vthplsp,            # Thermal velocities of species -parallel.
     &    vthprsp,            # " " -perp.
     &    wnsp,               # Weight of real species particle (norm-
                              !  alized to H).
     &    wsp,                # Weight of ion species microparticle.
     &    wdqsp               # Mass/charge ratio for ion species.
       integer ::
     &    nsp,                # Number of species actually used.
     &    mfsp(NSPX),         # First particle index for each species.
     &    mlsp(0:NSPX),       # Last particle index for each species.
     &    npsp(NSPX)          # Number of particles of each species.
       character*20
     &   cisp(NSPX)       # Species label.
       end module cm_spec 

# COMMON_PGRID
      ifelse(LPGRID,T,[
        define(COMMON_PGRID,[
          use cm_pgrid
        ])
        module cm_pgrid       # Various particle grid parameters.
          use param
          parameter(
     &      nxd= NXD, # Num of data grid pts in x dir. on particle grid.
     &      nyd= NYD,
     &      nx= nxd +4,     # Total dimension size.
     &      ny= nyd +4,
     &      nxm2= nx -2,
     &      nym2= ny -2,
     &      nxy= nx*ny,
     &      nixy= ni*nx*ny )

          real ::
     &      dx,                 # Grid step size for x (j) dimension.
     &      dy,                 # Grid step size for y (k) dimension.
     &      dxi,                # 1./dx
     &      dyi,                # 1./dy
     &      x(nx),              # x coordinate positions.
     &      y(ny),              # y coordinate positions.
     &      xlen,               # Length in x direction.
     &      ylen,               # Length in y direction.
     &      xlo,                # xlo=0.
     &      xhi,                # xhi= 2*rlen.
     &      ylo,                !
     &      yhi,                !
     &      x0,                 # Center x coord.
     &      y0,                 # Center y coord.
     &      rxy(nx,ny),         # Distance from the center to (x_j,y_k).
     &      sxy(nx,ny),         # Polar angle for (x_j,y_k).
     &      cosp(nx,ny),        # cos( sxy(j,k) )
     &      sinp(nx,ny)         # sin( sxy(j,k) )
          integer ::
     &      ibp(nx,ny)          # =1 inside (=0 outside) cylinder.
        end module cm_pgrid
      ],[
        define(COMMON_PGRID,[])
      ])

# COMMON_OUT

      define(COMMON_OUT,[
        use cm_out
      ])
      module cm_out      # Output data.
        real ::
     &    en,                 # Total energy.
     &    enb,                # Magnetic energy.
     &    enk,                # Kinetic energy.
     &    enp                 # Pressure energy.
        integer ::
     &    ienout,             # Count of how many times energy terms
                              !  have been output.
     &    isfldout            # Count of how many times field
                              !  quantities have been output.
      end module cm_out

# COMMON_FRC

      define(COMMON_FRC,[
        use cm_tk
      ])
      module cm_tk      # TK parameters.
        real ::
     &    psi0,               # Poloidal flux at magnetic axis.
     &    u0,                 # same as above.
     &    u1,                 # Flux at the plasma boundary.
     &    fwall,              # =1 wall at r=Rc; <1 cond. shell inside.
     &    p0,                 # Bulk pressure profile parameter.
     &    pmax,               # Max pressure.
     &    p1,                 # Pressure parameter ~rhoc (rotation).
     &    r0,                 # Radius of magnetic null point (m. axis).
     &    rm,                 # Max number for bulk rotation =Vo/Va.
     &    rhoc1,              # Calculated cutoff density (for rm<>0).
     &    alpha,              # Bulk rotation parameter.
     &    alphai,             # Particle ion rotation parameter.
     &    rmir,               # Mirror ratio =B(edge)/B(0).
     &    bt,                 # Ratio of B_phi on magnetic axis to B0.
     &    hi0,                # hi=B_phi*r  value on magn. axis.
     &    omegai0,            # Fast ion rotation frequency.
     &    ti0,                # Fast ion temperature=const.
     &    ni0,                # Fast ion density at magnetic null.
     &    dz_gs,              # dz for GS-grid.
     &    dr_gs,              # dr for GS-grid.
     &    rci,                # Ri radius of internal wall.
     &    riphi,              # Total toroidal current.
     &    alphah1,            # B_phi profile parameter #1.
     &    alphah2,            # B_phi profile parameter #2.
     &    omega1,             # Parameter for ADI solver.
     &    omega2,             # Parameter for ADI solver.
     &    bv,                 # Vertical field factor.
     &    q0,                 # Tokamak 'q' on the axis.
     &    zb,                 # Beam ion normalized charge Z.
     &    zeff                # Thermal plasma effective charge Z_eff.
        integer ::
     &    nz,                 # Number of grid points in z-direction.
     &    nr,                 # Number of grid points in r-direction.
     &    nzd2p1,             # nz/2+1
     &    iaxis,              # Magn. axis(null) i-position on GS grid.
     &    jaxis               #                  j-position.
      end module cm_tk

# COMMON_DF

      define(COMMON_DF,[
        use cm_df
      ])
      module cm_df      # Distribution function parameters.
        real ::
     &    v0,                 # for F1(v)
     &    vstar,              #
     &    deltav,             #
     &    lambda0,            # for F2(lambda)
     &    deltal,             # 
     &    rl0,                #
     &    rdl,                #
     &    etaf,               # for F2_add
     &    deltala,            #
     &    lambda0a,           #
     &    b0,                 # B_tor at axis.
     &    pminus,             # p_phi_min.
     &    pplus               # p_phi_max.
        integer ::
     &    aav,                #
     &    bbv,                #
     &    betav               # for F3(p_phi)
      end module cm_df


# DIM_s and DIM_v macros (to dimension vectors and scalars):

      define(IJK,[ni,nj,nk])

      define(DIM_s,[real, dimension(IJK) :: $1[]REST_DIM_s($-)])
      define(REST_DIM_s,[ifelse($1,,,[,$1[]REST_DIM_s($-)])])

      define(DIM_v,[real, dimension(IJK) :: v1$1,v2$1,v3$1[]REST_DIM_v($-)])
      define(REST_DIM_v,[ifelse($1,,,[,v1$1,v2$1,v3$1[]REST_DIM_v($-)])])

# DIM_pv macro (to dimension particle vector):

      define(DIM_pv,[dimension pv1$1(np),pv2$1(np),pv3$1(np)REST_DIM_pv($-)])
      define(REST_DIM_pv,[ifelse($1,,,[,pv1$1(np),pv2$1(np),pv3$1(np)REST_DIM_pv($-)])])

# DIM_sp and DIM_vp macros (to dimension vectors and scalars on
! particle (q,x,y) grid):
      ifelse(LPGRID,T,[
        define(DIM_sp,[dimension $1(ni,nx,ny)])
        define(DIM_vp,[dimension vp1$1(ni,nx,ny),vp2$1(ni,nx,ny),vp3$1(ni,nx,ny)])
      ],[
        define(DIM_sp,[])
        define(DIM_vp,[])
      ])

# Machine (MACH) dependant macros:

      define(SUM_rs,[
        call sum_rs($1,$2)
      ])

# Compiler (CMPLR) dependant macros:

# Array operations:

      ifelse(CMPLR,f90,[
        define(SET_ar,[
          $1 = $2
        ])
        define(SET_aa,[
          $1 = $2
        ])
        define(NEG_aa,[
          $1 = -$2
        ])
        define(INV_aa,[
          $1 = 1./$2
        ])
        define(ADD_aar,[
          $1 = $2 + $3
        ])
        define(ADD_aaa,[
          $1 = $2 + $3
        ])
        define(ADD_aaar,[
          $1 = $2 + $3*$4
        ])
        define(SUB_aaa,[
          $1 = $2 - $3
        ])
        define(AV_aaa,[
          $1 = .5*( $2 + $3 )
        ])
        define(MUL_aar,[
          $1 = $2 * $3
        ])
        define(MUL_aaa,[
          $1 = $2 * $3
        ])
        define(MUL_aaar,[
          $1 = $2 * $3 * $4
        ])
        define(MUL_aaaa,[
          $1 = $2 * $3 * $4
        ])
        define(DIVI_aaa,[
          $1 = $2 / $3
        ])
        define(POW_aar,[
          $1 = $2 ** $3
        ])
      ],[
        ifelse(MACH,cray1,[
          define(SET_ar,[
            do ia= 1,$3
              $1(ia)= $2
            enddo ia
          ])
          define(SET_aa,[
            do ia= 1,$3
              $1(ia)= $2(ia)
            enddo ia
          ])
          define(NEG_aa,[
            do ia= 1,$3
              $1(ia)= -$2(ia)
            enddo ia
          ])
          define(INV_aa,[
            do ia= 1,$3
              $1(ia) = 1./$2(ia)
            enddo ia
          ])
          define(ADD_aar,[
            do ia= 1,$4
              $1(ia)= $2(ia) + $3
            enddo ia
          ])
          define(ADD_aaa,[
            do ia= 1,$4
              $1(ia)= $2(ia) + $3(ia)
            enddo ia
          ])
          define(ADD_aaar,[
            do ia= 1,$5
              $1(ia)= $2(ia) + $3(ia)*$4
            enddo ia
          ])
          define(SUB_aaa,[
            do ia= 1,$4
              $1(ia)= $2(ia) - $3(ia)
            enddo ia
          ])
          define(AV_aaa,[
            do ia= 1,$4
              $1(ia)= .5*( $2(ia) + $3(ia) )
            enddo ia
          ])
          define(MUL_aar,[
            do ia= 1,$4
              $1(ia)= $2(ia) * $3
            enddo ia
          ])
          define(MUL_aaa,[
            do ia= 1,$4
              $1(ia)= $2(ia) * $3(ia)
            enddo ia
          ])
          define(MUL_aaar,[
            do ia= 1,$5
              $1(ia)= $2(ia) * $3(ia) * $4
            enddo ia
          ])
          define(MUL_aaaa,[
            do ia= 1,$5
              $1(ia)= $2(ia) * $3(ia) * $4(ia)
            enddo ia
          ])
          define(DIVI_aaa,[
            do ia= 1,$4
              $1(ia)= $2(ia) / $3(ia)
            enddo ia
          ])
          define(POW_aar,[
            do ia= 1,$4
              $1(ia)= $2(ia) ** $3
            enddo ia
          ])
        ],[
          define(SET_ar,[
            call set_ar($1,$2,$3)
          ])
          define(SET_aa,[
            call set_aa($1,$2,$3)
          ])
          define(NEG_aa,[
            call neg_aa($1,$2,$3)
          ])
          define(INV_aa,[
            call inv_aa($1,$2,$3)
          ])
          define(ADD_aar,[
            call add_aar($1,$2,$3,$4)
          ])
          define(ADD_aaa,[
            call add_aaa($1,$2,$3,$4)
          ])
          define(ADD_aaar,[
            call add_aaar($1,$2,$3,$4,$5)
          ])
          define(SUB_aaa,[
            call sub_aaa($1,$2,$3,$4)
          ])
          define(AV_aaa,[
            call av_aaa($1,$2,$3,$4)
          ])
          define(MUL_aar,[
            call mul_aar($1,$2,$3,$4)
          ])
          define(MUL_aaa,[
            call mul_aaa($1,$2,$3,$4)
          ])
          define(MUL_aaar,[
            call mul_aaar($1,$2,$3,$4,$5)
          ])
          define(MUL_aaaa,[
            call mul_aaaa($1,$2,$3,$4,$5)
          ])
          define(DIVI_aaa,[
            call divi_aaa($1,$2,$3,$4)
          ])
          define(POW_aar,[
            call pow_aar($1,$2,$3,$4)
          ])
        ])
      ])

# Simple vector operations:

      ifelse(CMPLR,f90,[
        define(SET_vr,[
          $1 = $4
          $2 = $4
          $3 = $4
        ])
        define(SET_vv,[
          $1 = $4
          $2 = $5
          $3 = $6
        ])
        define(INV_vv,[
          $1 = 1./$4
          $2 = 1./$5
          $3 = 1./$6
        ])
        define(ADD_vvv,[
          $1 = $4 + $7
          $2 = $5 + $8
          $3 = $6 + $9
        ])
        define(SUB_vvv,[
          $1 = $4 - $7
          $2 = $5 - $8
          $3 = $6 - $9
        ])
        define(MUL_vvr,[
          $1 = $4 * $7
          $2 = $5 * $7
          $3 = $6 * $7
        ])
        define(MUL_vvs,[
          $1 = $4 * $7
          $2 = $5 * $7
          $3 = $6 * $7
        ])
        define(MUL_vvsr,[
          $1 = $4 * $7 * $8
          $2 = $5 * $7 * $8
          $3 = $6 * $7 * $8
        ])
        define(MUL_vvv,[
          $1 = $4 * $7
          $2 = $5 * $8
          $3 = $6 * $9
        ])
        define(DOT_svv,[
          $1 = $2 * $5  + $3 * $6  + $4 * $7
        ])
        define(DIVI_vvs,[
          $1 = $4 / $7
          $2 = $5 / $7
          $3 = $6 / $7
        ])
      ],[
        define(SET_vr,[
          call set_vr($1,$2,$3,$4)
        ])
        define(SET_vv,[
          call set_vv($1,$2,$3,$4,$5,$6)
        ])
        define(INV_vv,[
          call inv_vv($1,$2,$3,$4,$5,$6)
        ])
        define(ADD_vvv,[
          call add_vvv($1,$2,$3,$4,$5,$6,$7,$8,$9)
        ])
        define(SUB_vvv,[
          call sub_vvv($1,$2,$3,$4,$5,$6,$7,$8,$9)
        ])
        define(MUL_vvr,[
          call mul_vvr($1,$2,$3,$4,$5,$6,$7)
        ])
        define(MUL_vvs,[
          call mul_vvs($1,$2,$3,$4,$5,$6,$7)
        ])
        define(MUL_vvsr,[
          call mul_vvsr($1,$2,$3,$4,$5,$6,$7,$8)
        ])
        define(MUL_vvv,[
          call mul_vvv($1,$2,$3,$4,$5,$6,$7,$8,$9)
        ])
        define(DOT_svv,[
          call dot_svv($1,$2,$3,$4,$5,$6,$7)
        ])
        define(DIVI_vvs,[
          call divi_vvs($1,$2,$3,$4,$5,$6,$7)
        ])
      ])

# SDO and SENDDO macros:

      ifelse(DIMEN,2,[
        define(SDO,[
          k= 1
          do j= 1,nj
            do i= 1,ni
        ])
        define(SDO_dat,[
          k= 1
          do j= 3,njm2
            do i= 3,nim2
        ])
        define(SDO_2,[
          k= 1
          do j= 2,njm2
            do i= 2,nim2
        ])
        define(SDO_exp,[
          k= 1
          do j= 1,nje
            do i= 1,nie
        ])
        define(SENDDO,[
            enddo i
          enddo j
        ])
      ],[
        define(SDO,[
          do k= 1,nk
            do j= 1,nj
              do i= 1,ni
        ])
        define(SDO_dat,[
          do k= k3,nkm2
            do j= 3,njm2
              do i= 3,nim2
        ])
        define(SDO_2,[
          do k= k3m,nkm2
            do j= 2,njm2
              do i= 2,nim2
        ])
        define(SDO_exp,[
          do k= 1,nke
            do j= 1,nje
              do i= 1,nie
        ])
        define(SENDDO,[
              enddo i
            enddo j
          enddo k
        ])
      ])

# Define for LPGRID=T.

      ifelse(LPGRID,T,[
        define(SDOP,[
          do k= 1,ny
            do j= 1,nx
              do i= 1,ni
        ])
        define(SDOP_dat,[
          do k= k3,nym2
            do j= 3,nxm2
              do i= 3,nim2
        ])
      ],)

# IMPLICIT_TYPES macro:

      define(IMPLICIT_TYPES,[
        implicit logical (l)
        implicit complex (z)
        implicit character*30 (c)
      ])

# ERROR_EXIT($1,$2) macro:          # The argument $1 is a condition
                                    !  for an error.  $2 is the name
      define(ERROR_EXIT,[           !  of the routine.
        if( $1 ) then                             
          print *, 'Error: $1 in $2 > exit'
          call exit(0)
        endif
      ])

# RETURN_END macro:

      define(RETURN_END,[
        return
        end
      ])

# MPI macros:

      ifelse(LMPI,T,[
        define(MPI_print,[if( myid=master ) print *,])
        define(MPI_ABORT,[ call MPI_abort(MPI_comm_world, $1, ierr) ])
        define(MPI_BARRIER,[ call MPI_barrier($1, ierr) ])
      ],[
        define(MPI_print,[print *,])
        define(MPI_ABORT,[])
        define(MPI_BARRIER,[])
      ])



c######################################################################
      module MPI_module
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Contains MPI stuff, including myid.

    [ include "mpif.h" ]
      integer :: ierr, myid, nprocs
      integer, parameter :: master=0
      real :: tstart, tend

    [ end module MPI_module ]

c######################################################################
c######################################################################
c######################################################################
      program hyb
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    
      use MPI_module

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD ; COMMON_OUT
      COMMON_PART ; COMMON_SPEC

      call MPI_init(ierr)

      call inpar
      call init
      call savedump(0)
      call finalize
      
      call MPI_finalize(ierr)

      print *, 'stop 999'
      end program
c######################################################################
      subroutine inpar
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Read run-time parameters.

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_OUT ; COMMON_SPEC

# Run-time parameter namelist:

      namelist /hin/ idrun,
     &    isbc,  ivbc_b, ivbc_v,
     &    beta,  gamma, vpert,
     &    q1in,  r1in,  s1in,
     &    q2in,  r2in,  s2in,
     &    ri1in, rj1in, rk1in,
     &    ri2in, rj2in, rk2in,
     &    nsp,
     &    ialign,

     &    cisp,
     &    dennsp,
     &    npsp,
     &    qnsp,
     &    tratsp,
     &    vthplsp,
     &    wnsp

# Initialize commons (modules) - these are defaults.

      idrun=0
      beta=.5;    gamma=1.6666666666667
      vpert=0.
      q1in=0.;    r1in=0.;    s1in=0.
      q2in=1.;    r2in=1.;    s2in=1.
      ri1in=3.;   rj1in=3.;   rk1in=3.
      ri2in=-2.;  rj2in=-2.;  rk2in=-2.
      nsp=1;      ialign=1

      cisp='hydrogen$'
      dennsp=1.
      npsp=1
      qnsp=1.
      tratsp=1.
      vthplsp=0.01
      wnsp=1.

# Read namelist (default values are above).

      open(11,file='hmin.i',status='old')
      read(11,hin)
      close(11)

      ERROR_EXIT( ri1in<+1 | rj1in<+1 | rk1in<+1 |
     &            ri2in>-1 | rj2in>-1 | rk2in>-1 , inpar )


# Defines scalar and vector boundary condition (bc) types.

      ibstyp= reshape(
     &         (/ 1,1,1,1,1,1,             # do nothing
     &            2,2,2,2,2,2,             # periodic for now
     &            2,2,9,9,2,2,             #3 per + cont/cont + per
     &            2,2,10,9,2,2,            #4 per +  odd/cont + per
     &            2,2,11,9,2,2,            #5 per + even/cont + per
     &            2,2,10,10,2,2,           #6 per +  odd/odd  + per
     &            2,2,3,3,2,2,             #7 periodic + even + periodic
     &            2,2,4,4,2,2,             #8 periodic + odd + periodic
     &            2,2,7,11,2,2,            #9  per + scalar/even + per
     &            2,2,8,11,2,2,            #10 per + vector/even + per
     &            2,2,7,10,2,2,            #11 per + scalar/odd + per
     &            2,2,8,10,2,2,            #12 per + vector/odd + per
     &            2,2,7,9,2,2,             #13 per + scalar/cont + per
     &            2,2,8,9,2,2,             #14 per + vector/cont + per
     &            5,5,5,5,5,5,             #15 zero value.
     &            9,9,9,9,2,2,             #16 cont + cont + per
     &            6,6,6,6,2,2,             #17 zero + zero + per
     &            9,9,10,10,2,2,           #18 cont + odd  + per
     &            10,10,9,9,2,2,           #19 odd  + cont + per.
     &            11,11,12,12,2,2,         #20 even + even* + per
     &            13,13,13,13,2,2 /),      #21 con4 + con4 + per
     &          (/ 2,3,nstyp /) )

      isvtyp= reshape(
     &         (/ 1,1,1,                  #1 do nothing
     &            2,2,2,                  #2 periodic
     &            13,12,14,              #3 for V
     &            13,14,14,              #4 for B,E,J,A
     &            7,8,7,                 #5 for B and V
     &            8,7,8,                 #6 for E and J
     &            9,12,10,               #7 for V in cyl. geometry
     &            11,10,12,              #8 for E in cyl. geometry
     &            13,12,14,              #9 for B in cyl. geometry
     &            11,14,12,              #10 for E in cyl. geometry
     &            5,6,5,                 #11 for V if Ri>0 ==========
     &            3,4,3,                 #12 for B
     &            4,3,4,                 #13 for E and J
     &            4,3,3,                 #14 for A
     &            6,6,6,                 #15 for V_pert.
     &            11,14,14,              #16 for A in cylinder.
     &            11,12,12,              #17 for V_pert.
     &            19,18,16,              #18 for V 
     &            16,16,16,              #19 for A,E
     &            21,21,21,              #20 for A
     &            17,17,17,              #21 for J
     &            16,16,20 /),           #22 for B
     &         (/ 3,nvtyp /) )


      RETURN_END
c######################################################################
      subroutine init
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Initialize stuff.

      call dfcon

      call dfgrid

      ifelse(LPGRID,T,[call dfgrid_p],)

      call dfvec

      call alloc_fld

      call dfh

      call fromh

      ifelse(LMHD,F,call dfspec,)

      call initfld

      ifelse(LMHD,F,[
        call alloc_part
        call initpart
      ],)


      RETURN_END

c######################################################################
      subroutine dfcon
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Define miscellaneous quantities.

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_OUT

      rpid4= atan2(1.,1.)      # pi/4.
      rpi= 4.*rpid4            # pi.
      rpi2= rpi*2.
      rpid180= rpi/180.

      its= 0                  # time step counter.
      t= 0.                   # initial time.

      odgamma= 1./gamma
      tdgammam= 2./(gamma-1.)

      ienout= 0               # Counts how many times energy terms
                              !  have been output.

      isfldout= 0

      a1styp= 0.
      a2styp= 0.

      ifelse(lRAN1,T,[
        idum= -1
        call random_init(.true., .true.)  # needed for ran2

c       print *,'using RAN2() function',ran2(idum),ran2(idum),ran2(idum)
c       print *,'using RAN1() function',ran1(idum),ran1(idum)
c       print *,'using RAN() function',ran(idum),ran(idum)
      ],)

      RETURN_END
c######################################################################
      subroutine dfgrid
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate grid associated quantities.

      PARAM ; COMMON_VEC ; COMMON_PARAM

# Define grid coordinate values.

      ifelse(LCYL,T,[
        if( r1in=0. ) then
          ERROR_EXIT( rj1in<>1 | rj2in<>-3, dfgrid )
        else
          ERROR_EXIT( rj1in<>3 | rj2in<>-3, dfgrid )
        endif
      ],)

      ri2abs= ni +1 +ri2in
      rj2abs= nj +1 +rj2in
      rk2abs= nk +1 +rk2in

      dq= (q2in -q1in)/(ri2abs -ri1in)
      dr= (r2in -r1in)/(rj2abs -rj1in)
      ds= (s2in -s1in)/(rk2abs -rk1in)

      dqi= 1./dq
      dri= 1./dr
      dsi= 1./ds

#q
      do i= 1,ni
        q(i)= q1in +(i-ri1in)*dq
      enddo i
      if( ibstyp(1,1,SBC)=2 ) then
        qlo= .5* ( q(2) + q(3) )
      else
        qlo= q(3)
      endif
      if( ibstyp(2,1,SBC)=2 ) then
        qhi= .5* ( q(nim2) + q(nim) )
      else
        qhi= q(nim2)
      endif
c     qlo= q1in +(2.5       -ri1in)*dq
c     qhi= q1in +(nim2 + .5 -ri1in)*dq
      qlen= qhi - qlo

#r
      do j= 1,nj
        r(j)= r1in +(j-rj1in)*dr
      enddo j
      ifelse(LCYL,T,[
        rlo= r(int(rj1in))
        rhi= r(njm2)
      ],[
        if( ibstyp(1,2,SBC)=2 ) then
          rlo= .5* ( r(2) + r(3) )
        else
          rlo= r(3)
        endif
        if( ibstyp(2,2,SBC)=2 ) then
          rhi= .5* ( r(njm2) + r(njm) )
        else
          rhi= r(njm2)
        endif
c       rlo= r1in +(2.5       -rj1in)*dr
c       rhi= r1in +(njm2 + .5 -rj1in)*dr
      ])
      rlen= rhi - rlo
        

      ifelse(DIMEN,3,[
#s - always periodic.
        do k= 1,nk
          s(k)= s1in +(k-rk1in)*ds
        enddo k
        slo= s1in +(2.5       -rk1in)*ds
        shi= s1in +(nkm2 + .5 -rk1in)*ds
        slen= shi - slo
      ],[
        s(1)= s1in
        slo=  s1in
        shi=  s1in
        slen= 0.
      ])

# Define coefficients for calculation of derivatives.

      oddq12= 1./(12.*dq)
      d(-2,1)=  oddq12
      d(-1,1)= -8.*oddq12
      d( 0,1)=  0.
      d( 1,1)=  8.*oddq12
      d( 2,1)= -oddq12

      oddr12= 1./(12.*dr)
      d(-2,2)=  oddr12
      d(-1,2)= -8.*oddr12
      d( 0,2)=  0.
      d( 1,2)=  8.*oddr12
      d( 2,2)= -oddr12

      odds12= 1./(12.*ds)
      d(-2,3)=  odds12
      d(-1,3)= -8.*odds12
      d( 0,3)=  0.
      d( 1,3)=  8.*odds12
      d( 2,3)= -odds12

# Define weights for high order integration.  We use eq. 4.1.14 in
!  Numerical Recipes unless the boundary is periodic.
      ifelse(SORDER,4,[
        aw1= 17./48.
        aw2= 59./48.         # Coefficients needed for weights.
        aw3= 43./48.
        aw4= 49./48.
      ],[
        aw1= 0.5
        aw2= 1.
        aw3= 1.
        aw4= 1.
      ])
# DEFW macro.  For wq, the weight in the q direction, the arguments
!  of this macro are $1=q, $2=i, and $3=1.
      define(DEFW,[
        if( ibstyp(1,$3,SBC)=2 ) then       # Periodic bc's.
          do $2= 3,n[]$2[]m2
            w[]$1($2)= 1.
          enddo
        else                                 # Non-periodic bc's.
          if( n[]$2[]d > 7 ) then
            w[]$1(3)= aw1
            w[]$1(4)= aw2
            w[]$1(5)= aw3
            w[]$1(6)= aw4
            do $2= 7,n[]$2[]m4-2
              w[]$1($2)= 1.
            enddo $2
            w[]$1(n[]$2[]m4-1)= aw4
            w[]$1(n[]$2[]m4)= aw3
            w[]$1(n[]$2[]m3)= aw2
            w[]$1(n[]$2[]m2)= aw1
          else if( n[]$2[]d > 1 ) then
            w[]$1(3)= .5
            do $2= 4,n[]$2[]m3
              w[]$1($2)= 1.
            enddo $2
            w[]$1(n[]$2[]m2)= .5
          else
            w[]$1(3)= 1.
          endif
        endif
        w[]$1(1)= 0.
        w[]$1(2)= 0.
        w[]$1(n[]$2[]m)= 0.
        w[]$1(n[]$2  )= 0.
      ])

      DEFW(q,i,1)
      DEFW(r,j,2)
      ifelse(LCYL,T,[
        if( r1in=0. ) then
          ifelse(SORDER,4,[
            wr(1)= aw1
            wr(2)= aw2
            wr(3)= aw3
            wr(4)= aw4
            wr(5)= 1.
            wr(6)= 1.
          ],[
            wr(1)= 0.
            wr(2)= aw1
            wr(3)= aw2
            wr(4)= aw3
            wr(5)= aw4
            wr(6)= 1.
          ])
        endif
      ],)
      ifelse(DIMEN,3,[
        DEFW(s,k,3)
      ],[
        ws(1)= 1.
      ])


# Calculate sin(phi) and cos(phi) for cylindrical geometry.

      ifelse(LCYL,T,[
        do k= k3,nkm2
          sins(k)= sin( s(k) )
          coss(k)= cos( s(k) )
          sin2s(k)= sin( 2*s(k) )
          cos2s(k)= cos( 2*s(k) )
        enddo
      ],)

      RETURN_END
c######################################################################
    ifelse(LPGRID,T,[
      subroutine dfgrid_p
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate particle grid associated quantities( needed for LCYL=T,
!  DIMEN=3 & LMHD=F, when fields are done on a cylindrical grid and
!  particles are done on rectangular coord. grid).

      PARAM ; COMMON_VEC ; COMMON_PARAM
      COMMON_PGRID

# Define grid coordinate values.
      xlen= 2.*rhi
      dx= xlen/(nxd-1)
      dxi= 1./dx
      xlo= 0.
      xhi= xlo + xlen
      do j= 1,nx
        x(j)= xlo + (j-3)*dx       # defined such that x(3)=0
      enddo j                      ! and x(nxm2)=xlen.

      ylen= 2.*rhi
      dy= ylen/(nyd-1)
      dyi= 1./dy
      ylo= 0.
      yhi= ylo + ylen
      do k= 1,ny
        y(k)= ylo + (k-3)*dy
      enddo k

      x0= 0.5*xlen
      y0= 0.5*ylen
      do k= 1,ny
        do j= 1,nx
          rxy(j,k)= sqrt( (x(j)-x0)**2 + (y(k)-y0)**2 )

          ibp(j,k)= 1
          if( rxy(j,k)<rlo | rxy(j,k)>rhi ) ibp(j,k)= 0

          if( rxy(j,k)=0. ) then
            sxy(j,k)= 0.
          else
            sxy(j,k)= acos( (x(j)-x0)/rxy(j,k)/ONEPLUS )
            if( (y(k)-y0)<0. ) sxy(j,k)= rpi2 - sxy(j,k)
          endif
          cosp(j,k)= cos( sxy(j,k) )
          sinp(j,k)= sin( sxy(j,k) )

        enddo j
      enddo k

      RETURN_END
    ],)
c######################################################################
      subroutine alloc_fld
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Allocate all fld grid scalars and vectors.

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD

      define(ALLOC_fld,[allocate( $1(IJK)R_ALLOC_fld($-) )])
      define(R_ALLOC_fld,[ifelse($1,,,[,$1(IJK)R_ALLOC_fld($-)])])

# Allocate all scalar arrays from COMMON_FLD.

      ALLOC_fld( sp, sp1, srhob1)
      ALLOC_fld( srhoq, srhoq0, spi0)
      ALLOC_fld( spsi, sphi, rmask, sres, sg)

# Allocate all vector arrays from COMMON_FLD.

      ALLOC_fld( V(b1), V(a), V(cur))
      ALLOC_fld( V(e), V(mo1), V(ub))
      ALLOC_fld( V(curi), V(curi0))
      ALLOC_fld( V(vhypa))

# Allocate geometry arrays from COMMON_CMVEC.

      ALLOC_fld( V(h))
      ALLOC_fld( V(hi), V(h2), V(h2i))
      ALLOC_fld( V(h2d1))
      ALLOC_fld( sh3, sh3w, sh3i)
      ALLOC_fld( t12hder,t13hder,t21hder,t23hder,t31hder,t32hder)

      RETURN_END
c######################################################################
      subroutine alloc_part
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Allocate particle arrays if not MHD. Using np.

      PARAM ; COMMON_PARAM ; COMMON_PART

      define(ALLOC_part,[allocate( $1(np)R_ALLOC_part($-) )])
      define(R_ALLOC_part,[ifelse($1,,,[,$1(np)R_ALLOC_part($-)])])

      ifelse(LMHD,F,[
        ALLOC_part( PV(x1), PV(v1))
        ALLOC_part( pspphi, psr, psmu, psw, psp)
      ],)

      RETURN_END
c######################################################################
      subroutine dfvec
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Defines quantities needed for vector operations.

      PARAM ; COMMON_VEC

      nam4(1)= nim4 ; nam4(2)= njm4 ; nam4(3)= nkm4
      nam3(1)= nim3 ; nam3(2)= njm3 ; nam3(3)= nkm3
      nam2(1)= nim2 ; nam2(2)= njm2 ; nam2(3)= nkm2
      nam(1)=  nim  ; nam(2)=  njm  ; nam(3)=  nkm
      na(1)=   ni   ; na(2)=   nj   ; na(3)=   nk

      RETURN_END
c######################################################################
      subroutine dfh
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Define h.  This is the sub. in which the coordinate system is
!  defined.

      PARAM ; COMMON_VEC

      SET_vr( V(h), 1. )             # straight coordinates for now

      ifelse(LCYL,T,[                # cylindrical geometry
        SDO                          ! s -> phi
          v3h(i,j,k)= r(j) + 1.e-10
        SENDDO
      ],)

      RETURN_END
c######################################################################
      subroutine fromh
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Define quantities calculated automatically from h.  We do not need
!  to change anything here when we change our coordinate system.
!  Rather change sub. dfh.

      PARAM ; COMMON_VEC

      INV_vv( V(hi), V(h) )         # 1./h

      MUL_aaa( v1h2, v2h, v3h, nijk )   # h2= prod of other 2 comp's
      MUL_aaa( v2h2, v1h, v3h, nijk )
      MUL_aaa( v3h2, v1h, v2h, nijk )

      MUL_aaa( sh3, v1h, v1h2, nijk )    # prod of all 3 comp's

      SDO                            # sh3 * weights for integration.
        sh3w(i,j,k)= sh3(i,j,k)*wq(i)*wr(j)*ws(k)
      SENDDO

      SUM_rs( h3sum, sh3 )    # sum of Jacobian, prop to system vol

      INV_vv( V(h2i), V(h2) )         # 1./h2

      MUL_aaa( sh3i, v1hi, v1h2i, nijk )  # inv of prod of all 3 compts

      MUL_aaa( v1h2d1, v1h2, v1hi, nijk )   # h2d1= prod of h2 and hi.
      MUL_aaa( v2h2d1, v2h2, v2hi, nijk )
      MUL_aaa( v3h2d1, v3h2, v3hi, nijk )

# tensor tijhder= dh_i/dq_j / (h_i * h_j ).
!  But here, let tijhder just be the derivative (without dividing by
!  (h_i * h_j) -- We will do that in a few lines.  Use sub. der_ss.

      call der_ss( t12hder, v1h, 2 )
      call der_ss( t13hder, v1h, 3 )
      call der_ss( t21hder, v2h, 1 )
      call der_ss( t23hder, v2h, 3 )
      call der_ss( t31hder, v3h, 1 )
      call der_ss( t32hder, v3h, 2 )

# tensor tijhder= dh_i/dq_j / (h_i * h_j )
!  Here we complete the definition of tijhder.

      MUL_aaa( t12hder, t12hder, v3h2i, nijk )
      MUL_aaa( t13hder, t13hder, v2h2i, nijk )
      MUL_aaa( t21hder, t21hder, v3h2i, nijk )
      MUL_aaa( t23hder, t23hder, v1h2i, nijk )
      MUL_aaa( t31hder, t31hder, v2h2i, nijk )
      MUL_aaa( t32hder, t32hder, v1h2i, nijk )

      RETURN_END
c######################################################################
      subroutine dfspec
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Define species quantities.

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_SPEC
      COMMON_PGRID

      do isp= 1,nsp

#  Define new species quantity, mass/charge
        wdqsp(isp) = wnsp(isp)/qnsp(isp)

# Define macro-particle charge and weight.

        qsp(isp)= dennsp(isp) * qnsp(isp) * h3sum / npsp(isp)
        ifelse(LPGRID,T,[
          isum= 0
          do i= 3,nim2
            do j=3,nx-2
              do k=3,ny-2
                isum= isum + ibp(j,k)
              enddo k
            enddo j
          enddo i
          qsp(isp)= qsp(isp) * isum / h3sum
        ],)
        wsp(isp)= qsp(isp)*wnsp(isp) / qnsp(isp)

# Define perpendicular thermal velocity.

        vthprsp(isp)= sqrt( tratsp(isp) ) * vthplsp(isp)


      enddo isp

# Define total # of particles = np, and first and last particle
!  index for each species.

      np= 0
      mlsp(0)= 0
      do isp= 1,nsp                       # This loop not vectorizable.
        mfsp(isp)= mlsp(isp-1) + 1
        mlsp(isp)= mfsp(isp) + npsp(isp) - 1
        np= np + npsp(isp)
      enddo isp

      RETURN_END
c######################################################################
      subroutine initfld   # tk
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Initializes the time stepped fields.

      use MPI_module

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD
      COMMON_SPEC ; COMMON_FRC
      parameter( eps=1.e-10 )
      DIM_s(stmp,stmpa,svel) ; DIM_v(tmp, tmpa)

      call MPI_comm_rank(MPI_comm_world, myid, ierr)
      call MPI_comm_size(MPI_comm_world, nprocs, ierr)

      MPI_print 'master proc=',myid, 'nprocs=',nprocs

# Define parameters.

      rm= 0.00                             # Max number rm=Vo/Va.
      bt= -1.00                             # Toroidal m.field (1/B0)
      rhoc= 0.10                           # Cutoff density for hybrid
                                           ! and rotating mhd.
      p1= 0.5*rhoc/(1.-rhoc)               # Assumed that pmax~0.5
      if( rm=0. ) p1= 0.

      ifelse(LMHD,T,[
        omegai0= 0.
        ti0= 1.
        ni0= 0.
      ],[
        omegai0= 0.
        ti0= vthplsp(1)**2/2.
        ni0= dennsp(1)
      ])

# Get tk equilibrium.

      nz= 2*(ni-1) + 1
      nr= 2*(nj-1) + 1

      tstart= MPI_wtime()
      
      call gs_tk               # Calculate V(a), V(b), sp, srhob1 etc.

      tend= MPI_wtime()
      MPI_print 'GS solver time =', (tend-tstart)/60, 'min'

# Initialize vector potential.

      if(bt=0.) then
        SET_ar( v1a, 0., nijk )
        SET_ar( v2a, 0., nijk )
      endif

# Initialize magnetic field.

      SET_vv( V(tmp), V(b1) )
      call curl_vv( V(b1), V(a), VBC_b )
      if(bt=0.) SET_ar( v3b1, 0., nijk )

# Initialize bulk current. Check that J= Ji + Jb.

      if(bt=0.) then
        ADD_aaa( stmp, v3curi0, v3cur, nijk )   # here v3cur=-r*dp/dpsi
        call curl_vv( V(tmp), V(b1), VBC_j )
        SUB_aaa( stmp, v3tmp, stmp, nijk )
        call maxabs_rs( rcur, stmp )
        MPI_print 'J-Ji-Jb=',rcur
        SET_ar( v1cur, 0., nijk )
        SET_ar( v2cur, 0., nijk )
      else
        call curl_vv( V(tmp), V(b1), VBC_j )
        ADD_vvv( V(cur), V(cur), V(curi0) )        # Jb+Ji
        SUB_vvv( V(tmp), V(cur), V(tmp) )
        call maxabs1_rs( rcur1, v1tmp )
        call maxabs1_rs( rcur2, v2tmp )
        call maxabs1_rs( rcur3, v3tmp )
        call maxabs1_rs( rcurt, v3cur )
        MPI_print 'Jb+Ji-curl(B)=', rcur1, rcur2, rcur3
        MPI_print '|J_pol|=', maxval(sqrt(v1cur**2+v2cur**2)),'  J_phi=', rcurt
       ifelse(1,2,[
        open(unit=13,file='j_check.dat')
        write(13,9) nid, njd
        write(13,10) (q(i),i=3,nim2),(r(j),j=3,njm2)

        write(13,10) ((v1tmp(i,j,k3),i=3,nim2),j=3,njm2)    # J-curl(B)
        write(13,10) ((v2tmp(i,j,k3),i=3,nim2),j=3,njm2)
        write(13,10) ((v3tmp(i,j,k3),i=3,nim2),j=3,njm2)

        write(13,10) ((v1curi0(i,j,k3),i=3,nim2),j=3,njm2)  # J_i
        write(13,10) ((v2curi0(i,j,k3),i=3,nim2),j=3,njm2)
        write(13,10) ((v3curi0(i,j,k3),i=3,nim2),j=3,njm2)

        write(13,10) ((v1cur(i,j,k3),i=3,nim2),j=3,njm2)    # J_total
        write(13,10) ((v2cur(i,j,k3),i=3,nim2),j=3,njm2)
        write(13,10) ((v3cur(i,j,k3),i=3,nim2),j=3,njm2)
        close(13)
       ],)
       ifelse(1,1,[              # Saved total current for Luca.
        open(unit=13,file='j_tot.dat')
        write(13,9) nid, njd
        write(13,10) (q(i),i=3,nim2),(r(j),j=3,njm2)

        write(13,10) ((v1cur(i,j,k3),i=3,nim2),j=3,njm2)    # J_total
        write(13,10) ((v2cur(i,j,k3),i=3,nim2),j=3,njm2)
        write(13,10) ((v3cur(i,j,k3),i=3,nim2),j=3,njm2)
        close(13)
       ],)
      endif

# Initialize pressure variable, sp1=(sp)^(1/gamma).

      SET_ar( sp1, 0., nijk)
      pcut= 0.0

      if( beta<>0. ) then
        pcut= 0.0001
        if( rm=0. ) ADD_aar( sp, sp, pcut, nijk )
        SDO
          if( sp(i,j,k) > 0. ) then
            sp1(i,j,k)= ( sp(i,j,k) )**odgamma
          endif
        SENDDO

        pmax=0.
        SDO
          pmax=amax1(pmax,sp(i,j,k))
        SENDDO

        call fix_s( sp1, sp1, SBC )
      endif

# Initialize electric field.

      ifelse(LBULK,T,[
        SET_vr( V(e), 0. )
      ],[
        call grad_vs( V(e), sp, VBC_no )             # grad(p)
        ADD_aar( stmp, srhoq0, eps, nijk )
        DIVI_vvs( V(e), V(e), stmp )
        call fix_v( V(e), V(e), VBC_e )
      ])

# Initialize bulk density.

      ifelse(LDENST,T,[
        SET_ar( srhob1, 1., nijk)                 # rho= const.
        rhoc= 0.2
      ],[
      # Now density profile is set in gs_tk.
      ])

      MPI_print 'rhoc=', rhoc

# Initialize resistivity profile ~1/srhob. For LBULK=F done in initpart.
 
      ifelse(LBULK,T,[
        stmp= srhob1 - 0.9*rhoc
        sres= 1/stmp
c       INV_aa( sres, srhob1, nijk )
c       SET_ar( sres, 1., nijk )
      ],)

# Initialize momentum and velocity.

      SET_vr( V(ub), 0. )
      SET_vr( V(mo1), 0. )
      ifelse(LBULK,T,[
c       nmode=0 
        SDO
          if( spsi(i,j,k) < u1 ) then
            xx= (spsi(i,j,k)-u1)/(psi0-u1)
            v1mo1(i,j,k)= vpert*(0.5-RAN)*xx**2
            v2mo1(i,j,k)= vpert*(0.5-RAN)*xx**2

c           theta= acos( q(i)/sqrt(q(i)**2+(r0-r(j))**2)/ONEPLUS )
c           if( (r(j)-r0)<0. ) theta= rpi2 - theta
c           v1mo1(i,j,k)= vpert*xx**2*(1-xx)*cos(nmode*s(k)+2*theta)
c           v2mo1(i,j,k)= vpert*xx**2*(1-xx)*sin(nmode*s(k)+2*theta)

c           v1mo1(i,j,k)= vpert*xx**2*(1-xx)*cos(nmode*s(k))
c    &                                      *sign(1.,q(i))
c           v2mo1(i,j,k)= vpert*xx**2*(1-xx)*cos(nmode*s(k))
c    &                                      *sign(1.,(r0-r(j)))
c           v3mo1(i,j,k)= - vpert*xx**2*(1-xx)
c    &                           *sin(nmode*s(k))*q(i)/qlen
          endif
        SENDDO

        MUL_vvs( V(mo1), V(mo1), srhob1 )
      ],)


# Initialize particle density, current and pressure (stress).
! Density and J_phi are calculated now in gs.


      ifelse(LBULK,F,[                         # for hybrid scheme.
        call curl_vv( V(b1), V(a), VBC_b )
c       call curl_vv( V(curi0), V(b1), VBC_j )      # J_i=J at t=0.
c       SET_ar( v3curi0, 0., nijk )                 # J_i=0.
      ],)

# Calculate V_e0 (for V_e0<>0).
      ifelse(LBULK,F,[
        SUB_vvv( V(ub), V(curi0), V(cur) )
        ADD_aar( stmp, srhoq0, eps, nijk )
        DIVI_vvs( V(ub), V(ub), stmp )            # Ve=(Ji-J)/ne.
        call fix_v( V(ub), V(ub), VBC_v )
      ],)

      ifelse(LMHD,T,,[
        ifelse(LBULK,T,[
          rpi0= 3.*wdqsp(1)*vthplsp(1)**2/2.
          MUL_aar( spi0, srhoq0, rpi0, nijk )         # p_i= 3 T_0*n_i.
        ],[
          MUL_aar( spi0, sp, 3., nijk )
        ])
        MUL_aaa( stmpa, v3curi0, v3curi0, nijk )
        ADD_aar( stmp, srhoq0, eps, nijk )
        DIVI_aaa( stmpa, stmpa, stmp, nijk )        # n_i <ui>^2
        ADD_aaa( spi0, spi0, stmpa, nijk )          # p_i= p_i+n_i<ui>^2

      ])

# Check equilibrium.

      ifelse(LBULK,T,[
        SUB_vvv( V(tmp), V(cur), V(curi0) )
        call cros_vvv( V(tmp), V(tmp), V(b1) )            # (J-Ji)xB
        call div_vvv( V(tmpa), V(ub), V(mo1), VBC_no )    # div(v mom)
        SUB_vvv( V(tmp), V(tmp), V(tmpa) )
        call grad_vs( V(tmpa), sp, VBC_no )               # grad(p)
        SUB_vvv( V(tmp), V(tmp), V(tmpa) )
      ],[
        call cros_vvv( V(tmp), V(curi0), V(b1) )         # JixB
        call grad_vs( V(tmpa), sp, VBC_no )              # -grad(p)
        SUB_vvv( V(tmp), V(tmp), V(tmpa) )
        MUL_vvs( V(tmpa), V(e), srhoq0 )                 # +ni*E
        ADD_vvv( V(tmp), V(tmp), V(tmpa) )
        SDO_dat                                          # +rho*V^2/r
          v2tmp(i,j,k)= v2tmp(i,j,k)
     &                + ( v3curi0(i,j,k)**2/(srhoq0(i,j,k)+eps) )/r(j)
        SENDDO
      ])
      call maxabs_rs( f1, v1tmp )
      call maxabs_rs( f2, v2tmp )
      call maxabs_rs( f3, v3tmp )
      MPI_print 'F= -div(v*mom)-grad(p)+(J-Ji)xB=',f1,f2,f3
      ifelse(1,2,[
        open(unit=13,file='f_check.dat')
        write(13,9) nid, njd
        write(13,10) (q(i),i=3,nim2),(r(j),j=3,njm2)
        write(13,10) ((v1tmp(i,j,k3),i=3,nim2),j=3,njm2)       # F_total
        write(13,10) ((v2tmp(i,j,k3),i=3,nim2),j=3,njm2)
        write(13,10) ((v3tmp(i,j,k3),i=3,nim2),j=3,njm2)

        write(13,10) ((v1tmpa(i,j,k3),i=3,nim2),j=3,njm2)     # grad(P)
        write(13,10) ((v2tmpa(i,j,k3),i=3,nim2),j=3,njm2)
        write(13,10) ((v3tmpa(i,j,k3),i=3,nim2),j=3,njm2)
        close(13)
      ],)
 10   format(5es12.3)
  9   format(2i4)


      RETURN_END
c######################################################################
# Define profiles for gs and etc.
# Toroidal flux (vs poloidal flux) profile.

      define(TPFLUX,[
        tflux(x)= (1-x)
        tfluxp(x)= -1.
      ])

c######################################################################
      subroutine gs_tk
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Solves GS equation to get 2D equilibrium (ie V(b1) and sp, etc).
! Assumes B= grad(phi) x grad(psi) + hi*grad(phi),  where hi=B_phi*r .

      use MPI_module

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD ; COMMON_SPEC
      COMMON_FRC ; COMMON_DF

      real apsi(nz,nr),rad(nr)
      real spt(nz,nr),spp(nz,nr)
      real jz(nz,nr), jr(nz,nr), jphi(nz,nr)
      real sf(nz,nr), deni(nz,nr)
      real hi(nz,nr),dhi(nz,nr),hhp1(nz,nr),hhp2(nz,nr)
      DIM_s(stmp) ; DIM_v(tmp)
      real rii(3), rib(3)

c     TPFLUX

      dr_gs= ( r(nj)-r(1) )/(nr-1)       # solving GS on extended grid.
      rci= r(1)
      rc= r(nj)
      do j=1,nr
        rad(j)= rci + (j-1)*dr_gs
      enddo j
      zc= 0.5*( q(ni)-q(1) )
      dz_gs= 2*zc/(nz-1)
      nzd2p1= nz/2+1

# Distribution function parameters.

      v0= vthplsp(1)

# Solve G-S eqn.

      call gssolve_tk(apsi,zc,rc,rad)           # for tk and ST

# Calculate bulk pressure.

      call pdpdpsi(spt,spp,apsi,rad)

      pmax= 0.
      psimax= 0.
      do j= 1,nr
        do i=1,nz
          pmax= amax1(pmax,spt(i,j))
          psimax= amax1(psimax,apsi(i,j))
        enddo i
      enddo j

# Calculate hi= B_phi*r (toroidal field).

      if(abs(bt)>0.) then
        call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,apsi,rad)
      endif

# Calculate fast ion current, density and stream function.

      call j_i(sf,jz,jr,jphi,deni,apsi,hi,rad)         ! new!

c     call j_phii(jphi,deni,apsi,rad)
c     ifelse(LBULK,F,[
c       ti0= pmax/ni0
c       omegai0= - alphai*ti0/psi0
c       MPI_print 'ti0=',ti0,'  vth=',sqrt(2*ti0),'  omegai0=',omegai0
c       do isp= 1,nsp
c         vthplsp(isp)= sqrt( 2.*ti0 )
c         vthprsp(isp)= sqrt(tratsp(isp))*vthplsp(isp)
c       enddo isp
c     ],)
 
      do k= 1,nk
        do j= 1,nj
          do i= 1,ni
            ii= 2*i
            if( ibstyp(1,1,SBC)<>2 ) ii= 2*(i-1)+1
            jj= 2*j
            if( ibstyp(1,2,SBC)<>2 ) jj= 2*(j-1)+1
            sp(i,j,k)= spt(ii,jj)                        # p_b
            v3cur(i,j,k)= -r(j)*spp(ii,jj)               # J_phi_b
            stmp(i,j,k)= apsi(ii,jj)                     # psi
            ifelse(LBULK,T,[
              srhoq0(i,j,k) = deni(ii,jj)                 # n_i
              v1curi0(i,j,k)= jz(ii,jj)
              v2curi0(i,j,k)= jr(ii,jj)
              v3curi0(i,j,k)= jphi(ii,jj)                 # J_phi_i
              sg(i,j,k)= sf(ii,jj)                        # stream-f
            ],[
              srhoq0(i,j,k) = sp(i,j,k)/ti0
            ])
          enddo i
        enddo j
      enddo k

# Check for sg (G=GC stream function) and div(J).
      ifelse(LMHD,T,,[
        call grad_vs( V(tmp), sg, VBC_no )
        DIVI_vvs( V(tmp),V(tmp),v3h )
        call maxabs_rs(djz,v1curi0-v2tmp)
        call maxabs_rs(ajz,v1curi0)
        call maxabs_rs(djr,v2curi0+v1tmp)
        call maxabs_rs(ajr,v2curi0)
c       MPI_print '|djz/jz|=',djz/ajz,'  |djr/jr|=',djr/ajr

c       call div_sv(v1tmp, V(curi0), SBC )
c       call maxabs_rs(divj, v1tmp)
c       MPI_print 'divj=',divj,'  |j_pol|=',sqrt(ajz**2+ajr**2)
      ])
# end check.


      SET_aa( spsi, stmp, nijk )
      ifelse(LBULK,F,[
c       ADD_aar( srhoq0, srhoq0, rhoc, nijk )      # rho_0=rho_0+rhoc
c       SUB_aaa( v3cur, v3cur, v3curi0, nijk )
      ],)
     

# Calculate As.

      DIVI_aaa( v3a, stmp, v3h, nijk )           # divide by r(j).
      NEG_aa( v3a, v3a, nijk )                   # As= - 1/r*Psi

# Calculate Bz and Br.

      call grad_vs( V(tmp), stmp, VBC_no )
      DIVI_vvs( V(tmp),V(tmp),v3h )             # divide by r(j).
      NEG_aa( v1b1, v2tmp, nijk )               # B_z=-(dpsi/dr)/r
      SET_aa( v2b1, v1tmp, nijk )               # B_r= (dpsi/dz)/r
      call fix_v( V(b1), V(b1), VBC_b )

# For bt<>0 calculate B_phi, J_phi+, J_pol and A_pol.
! Note that here V(cur) is bulk current, not a total current.

      if( bt<>0. ) then

        SDO                       # extended grid
          ii= 2*i                                 # periodic bc
          if( ibstyp(1,1,SBC)<>2 ) ii= 2*(i-1)+1
          jj= 2*j
          if( ibstyp(1,2,SBC)<>2 ) jj= 2*(j-1)+1
c         v3b1(i,j,k)= hi(ii,jj)/r(j)               # B_phi= hi/r
          v3b1(i,j,k)= (hi(ii,jj) + sf(ii,jj))/r(j) # B_phi=(H+G)/r
          v1cur(i,j,k)= -dhi(ii,jj)*v1b1(i,j,k)     # J_z
          v2cur(i,j,k)= -dhi(ii,jj)*v2b1(i,j,k)     # J_r
c         v3cur(i,j,k)= v3cur(i,j,k) - hi(ii,jj)*dhi(ii,jj)/r(j)
          v3cur(i,j,k)= v3cur(i,j,k) 
     &                 - (hi(ii,jj) + sf(ii,jj))*dhi(ii,jj)/r(j)
        SENDDO

c       call fix_v( V(b1), V(b1), VBC_b )
c       call fix_v( V(cur), V(cur), VBC_j )

                                             # Integrate for A_r.
        do j= 1,nj
          v2a(1,j,k3)= 0.
        enddo j

        do i= 2,7                                    # i<=7
          do j= 1,nj
            v2a(i,j,k3)= 0.5*(v3b1(1,j,k3) + v3b1(i,j,k3))
          enddo j
          do ii= 2,i-1
            do j= 1,nj
              v2a(i,j,k3)= v2a(i,j,k3) + v3b1(ii,j,k3)
            enddo j
          enddo ii
        enddo i

        do i= 8,ni                                   # i>7
          do j= 1,nj
            v2a(i,j,k3)=  aw1*(v3b1(1,j,k3) + v3b1(i,  j,k3))
     &                   +aw2*(v3b1(2,j,k3) + v3b1(i-1,j,k3))
     &                   +aw3*(v3b1(3,j,k3) + v3b1(i-2,j,k3))
     &                   +aw4*(v3b1(4,j,k3) + v3b1(i-3,j,k3))
          enddo j
          do ii= 5,i-4
            do j= 1,nj
              v2a(i,j,k3)= v2a(i,j,k3) + v3b1(ii,j,k3)
            enddo j
          enddo ii
        enddo i

        do j= 1,nj
          do i= 1,ni
            v2a(i,j,k3)= v2a(i,j,k3)*dq
            do k= 1,nk
              v2a(i,j,k)= v2a(i,j,k3)
              v1a(i,j,k)= 0.                          # Az=0.
            enddo k
          enddo i
        enddo j

        call fix_v( V(b1), V(b1), VBC_b )
        call fix_v( V(cur), V(cur), VBC_j )
c       call fix_v( V(a), V(a), VBC_a )
            
      endif

# Calculate density (for psi.dat).

      ifelse(LDENST,T,[
        SET_ar( srhob1, 1., nijk)                 # rho= const.
      ],[
# New density profile for 141711_470.
! New: normalize srhob1 to (1-ni0).
         SDO
          u2= u1*1.0
          x= (spsi(i,j,k)-u2)/(psi0-u2)
c         if( x>0. & abs(q(i)) < 29.5) then
          if( x>0.) then
            srhob1(i,j,k)= ( exp(-1.5*(1-x)**1.5) - exp(-1.5) )
     &                     /(1 - exp(-1.5))
c           srhob1(i,j,k)= (1-ni0)*srhob1(i,j,k)
            srhob1(i,j,k)= (1-rhoc)*srhob1(i,j,k) + rhoc
          else
            srhob1(i,j,k)= rhoc
          endif
        SENDDO
      ])
      

# Place perf. conducting shell inside at psi=wpsi and r=rwall*Rc;
!  r1wall<r<r2wall  and  |z|<zwall.

      fwall= 2.0                        # u1<=0 flux hole.
c     wpsi= fwall*u1                 # c. shell
      wpsi= fwall*psimax             # no shell
      SDO
        if( stmp(i,j,k)<wpsi ) then
          rmask(i,j,k)= 1.
        else
          rmask(i,j,k)= 0.
        endif
      SENDDO
c     MPI_print 'fwall=',fwall,'  wpsi=',wpsi


# Calculate  I_i/I_total  ratio.
  
      ifelse(LMHD,T,,[
        rii= 0.
        rib= 0.
        do i= 1,ni
          do j= 1,nj
            rii(3)= rii(3) + r(j)*v3curi0(i,j,k3)
            rib(3)= rib(3) + r(j)*v3cur(i,j,k3)
          enddo j
        enddo i
        MPI_print 'I_i_phi/I_phi= ', rii(3)/(rib(3)+rii(3))
        rii(1)= maxval(abs(v1curi0(:,:,k3)))
        rib(1)= maxval(abs(v1cur(:,:,k3)))
        rii(2)= maxval(abs(v2curi0(:,:,k3)))
        rib(2)= maxval(abs(v2cur(:,:,k3)))
        rii(3)= maxval(abs(v3curi0(:,:,k3)))
        rib(3)= maxval(abs(v3cur(:,:,k3)))
        MPI_print 'J_i_max/J_b_max(z,r,phi)= ', rii/rib
      ])

# Magnetic field normalization.

c     iz0= 0.5*(ni+1)
c     MPI_print 'Bz(0,Ri)=',v1b1(iz0,3,k3)
c     MPI_print 'Bz(0,Rc)=',v1b1(iz0,njm2,k3)

# Calculate Zb/Zeff ratio (used in frompart).
   
      zbdzeff= zb/zeff

# Calculate q0 and <beta>=2*<p>/<B^2>.
      psum= 0.
      bsum= 0.
      vsum= 0.
      do i= 1,ni
        do j= 1,nj
          if( sp(i,j,k3)>0.00*p0 ) then
            psum= psum + sp(i,j,k3)*r(j)
            bsum= bsum + (v1b1(i,j,k3)**2 + v2b1(i,j,k3)**2
     &                                    + v3b1(i,j,k3)**2)*r(j)
            vsum= vsum + 1.*r(j)
          endif
        enddo j
      enddo i
      beta_av= 2.*psum/bsum
c     qq0= -2*hi(iaxis,jaxis)/( r0*( r0**2*spp(iaxis,jaxis)
c    &                      + hi(iaxis,jaxis)*dhi(iaxis,jaxis) ) )
      qq0= -2*(hi(iaxis,jaxis) + sf(iaxis,jaxis))
     &       /( r0*( r0**2*spp(iaxis,jaxis)
     &              + hi(iaxis,jaxis)*dhi(iaxis,jaxis)
     &              + sf(iaxis,jaxis)*dhi(iaxis,jaxis)
     &              - r0*jphi(iaxis,jaxis) ) )
      MPI_print ' '
      MPI_print 'R0=',r0,'   alphah1=',alphah1,'   alphah2=',alphah2,'   psi0=',psi0
      MPI_print 'q0=',qq0, '    <beta>=',beta_av
c     MPI_print '<p>=',psum/vsum, '   beta_t=',2*psum/vsum,'   <B^2>=',bsum/vsum
      MPI_print ' '

# Calculate Zs and Rs.

      rsep= 0.
      zsep= 0.
      do i= 1,ni
        do j= 1,nj
c         if( sp(i,j,k3)>0.001 ) then
          if( spsi(i,j,k3)<0.0 ) then
            if( q(i) > zsep ) zsep= q(i)
            if( r(j) > rsep ) rsep= r(j)
          endif
        enddo j
      enddo i
      MPI_print ' '
      MPI_print 'Rs=',rsep,' Zs=',zsep
      MPI_print ' '

# Save equilibrium solution.
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
 10   format(5f12.5)
  9   format(2i4)

      close(15)

      open(unit=16,file='ji.dat')
      write(16,9) nid, (njm2-j3+1)
      write(16,10) ( (srhoq0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((v1curi0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((v2curi0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((v3curi0(i,j,k3),i=3,nim2),j=j3,njm2)
      write(16,10) ((sg(i,j,k3),i=3,nim2),j=j3,njm2)
      close(16)

c     call save_ncdf(ni,nj,nk,q,r,spsi)

      RETURN_END
c######################################################################
      subroutine gssolve_tk(u,zc,rc,rad)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Solve G-S equation:   [d^2/dz^2 + r*d/dr(1/r*d/dr)] u = g(u)
! in a cylinder ( -zc<z<zc & ri<r<rc ), where 
! g= -r^2*dp/dpsi - (hi+sf)*dhi/dpsi + r*Jphi.
! BC are:  u(z,ri)=psi1(z), u(z,rc)=psi2(z) and u(+-zc,r)=psi3(r).

      use MPI_module

      COMMON_FRC
      parameter( eps=1.e-3 )
      real, dimension(nz,nr) :: u, g, uold
      real, dimension(nz,nr) :: p, pp, jz, jr, jphi
      real, dimension(nz,nr) :: sf, deni
      real, dimension(nz,nr) :: hi, dhi, hhp1, hhp2, psie
      real :: rad(nr)
      integer :: index(2)

      real r2,dz,dr,err
      real dminn1,dmax1,dmin2,dmax2,t,kappa,f,r,q,omega
 
      define PI 3.1415927

                                             # Hills vortex solution.
      psi(r,z)= 4.*( 1. - (r/rs)**2 - (z/zs)**2 )*(r/rs)**2 


# Equilibrium parameters.

      namelist /tk/ e,
      psi0,  u1,  p0,  xs,
      riphi, bv, q0, rmir

      open(11,file='tk.i',status='old')
      read(11,tk)
      close(11)

      zb= 1.                         # Beam ion and plasma eff. charge.
      zeff= 1.2                      # for 141711_470

      r0= 0.6*(rad(1)+rad(nr))              # Magnetic axis radius.

      rs= rc*xs                             # Separatrix radius at z=0.
      zs= rs*e                              # Separatrix z-length.
c     hi0= bt*r0                            # hi value on magn. axis.
c     hi0= bt*0.5*(rad(1)+rad(nr))          # hi value at major radius.
      hi0= bt*22.6                          # TMP for shot 141711_470.

      u1rel= u1
      u1= u1rel*psi0                           # u1<0 - flux hole.
      riphi0= riphi

c     dz= 2*zc/(nz-1)
c     dr= (rad(nr)-rad(1))/(nr-1)
      dz= dz_gs
      dr= dr_gs


# Calculate total current I_phi (for BC).
! However, total I_phi which is used in iterations is from 'tk.i'.

      SET_ar( jphi, 0., nz*nr )
      SET_ar( sf, 0., nz*nr )

      do i=1,nz
        z= -zc + (i-1)*dz
        do j=1,nr
          r= rad(j)
          u(i,j)= psi0*psi(r,z)
          if(u(i,j)>0.) u(i,j)= 0.
        enddo j
      enddo i

      alphah1= 0.
      alphah2= 0.
      call pdpdpsi(p,pp,u,rad)
      call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,u,rad)
c     call j_phii(jphi,deni,u,rad)                    # J_phi_i.
c     call j_i(sf,jz,jr,jphi,deni,u,hi,rad)           # J_i & G
   
      sum= 0.
      do i=1,nz
        do j=1,nr
          sum= sum -rad(j)*pp(i,j) -(hi(i,j)+sf(i,j))*dhi(i,j)/rad(j)
     &             +jphi(i,j)
        enddo
      enddo
      riphi= sum*dr*dz                           # Total current.
      if( riphi0>0.0 ) riphi= riphi0
      MPI_print 'I_phi=',riphi

# Initialize u and apply bc.

      call psi_tk_bc(psie,zc,rc,rad)            # for tk

      psie0=0.
      do i=1,nz
        do j=1,nr
          if(u(i,j)>=0.) psie0=amin1( psie0, psie(i,j) )
        enddo
      enddo
      do i=1,nz
        do j=1,nr
          psie(i,j)= psie(i,j)-psie0
        enddo
      enddo

      call read_psi_bc(psie,u,zc,rc,rad)              # read input from L

      SET_aa(u, psie, nz*nr)                            # set BC.
 
c     do i=2,nz-1                                       ! done in read_psi_bc
c       z= -zc + (i-1)*dz
c       do j=2,nr-1
c         r= rad(j)
c         u(i,j)= psi0*psi(r,z)
c         if(u(i,j)>0.) u(i,j)= psie(i,j)
c       enddo j
c     enddo i

# Calculate omega1 and omega2 (from Samarskii & Gulin book).

      dminn1= ( 2./dz*sin(PI*dz/(4.*zc)) )**2
      dmax1= ( 2./dz*cos(PI*dz/(4.*zc)) )**2
      dmin2= ( 4./rc )**2
      dmax2= ( 2./dr*cos(PI*dr/(2.*rc)) )**2

      t= sqrt( (dmax1-dminn1)/(dmax1+dmin2)*(dmax2-dmin2)/(dmax2+dminn1) )
      kappa= (dmax1-dminn1)/(dmax2+dminn1)*dmax2/dmax1
      f= (kappa-t)/(kappa+t)
      r= ( dmax1-dmax2+(dmax1+dmax2)*f )/(2.*dmax1*dmax2)
      q= r + (1-f)/dmax1
      omega= sqrt( (1+t)/(1-t) )
      omega1= (1+f*omega)/(q*omega+r)
      omega2= (1-f*omega)/(q*omega-r)

      omega1= omega1*dz**2
      omega2= omega2*dr**2
c     omega1= 0.02816
c     omega2= 0.01982
c     omega1= 0.031
c     omega2= 0.02
c     print *,'omega1,2=',omega1,omega2

# Set parameters for LBULK=F case. Here alphai is defined from
! initial guess of omegai0, psi0 and ti0; final values are calculated
! in gs.

      alphai= - omegai0*psi0/ti0

# Rotation parameter: alpha= rho*omega^2/(2*P).
c     alpha= 2.*(rm/rs)**2
c     alpha1= 0.0
c     na= 4
c     if( rm=0. ) na= 0
c     do ia= 1,na+1
c       if( na>0 ) alpha1= alpha*(ia-1)/na


      q0m= q0
      dq0= q0m/5.

      do ia= 1,20

        uold= u
c       qt= q0m/2 + dq0
c       q0= min(qt,q0m)

# Do iterations until converges.

        do l=1,500

          u0= minval(u)                            # Calculate u0=psi0
          index=minloc(u)                          ! and r0.
          iaxis= index(1)
          jaxis= index(2)
          r0= rad(jaxis)

          psi0= u0
c         hi0= bt*r0
          u1= u1rel*psi0

                                                 # Calculate RHS.
          call pdpdpsi(p,pp,u,rad)
          call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,u,rad)     # hi=B_phi*R
c         call j_phii(jphi,deni,u,rad)                     # J_phi_i.

                                  # B_phi profile parameters #1 and 2.
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
          do i=1,nz
            do j=1,nr
              sum= sum - rad(j)*pp(i,j) 
     &                 - (hi(i,j)+sf(i,j))*dhi(i,j)/rad(j)
     &                 + jphi(i,j)
              sump= sump - rad(j)*pp(i,j)
              sumh1= sumh1 - hhp1(i,j)/rad(j)
              sumh2= sumh2 - hhp2(i,j)/rad(j)
              sumji= sumji + jphi(i,j) - sf(i,j)*dhi(i,j)/rad(j)
            enddo
          enddo
          if(l=1) then
            MPI_print 'I_phi(calcul)=', sum*dr*dz
            sum0= riphi/(dr*dz)
          endif

c         r2pp0= r0**2*pp0
          r2pp0= r0**2*pp0 + sfhp0
c         r2pp0= r0**2*pp0 + sfhp0 + 2*sf0/(r0*q0)
          
          d0= hh1*rf2 - hh2*rf1
          if( d0=0. ) then
            if( hh2=0. ) then
              rt1= r2pp0/hh1 
              rt2= 2*hi0/(q0*r0*hh1)
              alphah1= -rt1 + .5*rf1*rt2**2 - sign(1.,rt2)
     &              *sqrt( (rt1-.5*rf1*rt2**2)**2 + rt2**2 - rt1**2 )
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


          call hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,u,rad)       # Update h.


          do j=1,nr
            r2= rad(j)**2
            do i=1,nz
              g(i,j)= - r2*pp(i,j) - (hi(i,j) + sf(i,j))*dhi(i,j) 
     &                + rad(j)*jphi(i,j)
            enddo i
          enddo j

                                                        # Solve for u.
          call adi_tk(u,g,nz,nr,zc,rad,err,omega1,omega2)

c         MPI_print '# of iterations=', l, '    err=',err
          if( err<eps*abs(psi0) ) goto 1

        enddo l
        MPI_print 'no convergence in gssolve'
        MPI_print 'err=',err,'   abs(psi0)=',abs(psi0)
        stop
 1      continue
        MPI_print '# of iterations=', l, '    err=',err
        qq0= -2*(hi(iaxis,jaxis) + sf(iaxis,jaxis))
     &          /( r0*( r0**2*pp(iaxis,jaxis)
     &                  + hi(iaxis,jaxis)*dhi(iaxis,jaxis)
     &                  + sf(iaxis,jaxis)*dhi(iaxis,jaxis)
     &                  - r0*jphi(iaxis,jaxis) ) )
        MPI_print '**** q0=',qq0


        call j_i(sf,jz,jr,jphi,deni,u,hi,rad)      # update J_i and G
        err1= maxval(abs(u-uold))
        MPI_print '*** err1=', err1
        MPI_print ''
        if( err1<eps*abs(psi0) ) goto 2


      enddo ia
      MPI_print 'no convergence in gssolve -ia'
      MPI_print 'err1=', err1
      stop

 2    continue

      return
      end

c######################################################################
      subroutine pdpdpsi(p,pp,psi,rad)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate p(psi) and dp/dpsi(psi).

      COMMON_FRC
      real psi(nz,nr), p(nz,nr), pp(nz,nr), rad(nr)
      real psis, sigma
      real d, k0

c     WPROFILE
c     h(x)= 0.5*( 1. + tanh(a*x+c*x**3+b) )
c     hh(x)= 0.5*(a+3*c*x**2)/(cosh(a*x+c*x**3+b))**2


c     TPFLUX                  # defines toroidal flux in terms of psi

      do i=1,nz
        do j=1,nr
          p(i,j)= 0.
          pp(i,j)= 0.
        enddo j
      enddo i

      iprofile= 2                         # Pressure profile index.


      select( iprofile )

      case 1:                # profile: p= p0*(1-tflux)**sigma

c       sigma= 4.0
c       pw= 0.00             # adds p=const
c       do i=1,nz
c         do j=1,nr

c           x= (psi(i,j)-u1)/(psi0-u1)
c           if( x>0. ) then
c             p(i,j)= p0*(1-tflux(x))**sigma + p0*pw
c             pp(i,j)= -p0*sigma*tfluxp(x)*(1-tflux(x))**(sigma-1)
c    &                                                      /(psi0-u1)
c           else
c             p(i,j)= p0*pw
c             pp(i,j)= 0.
c           endif

c         enddo j
c       enddo i

      case 2:                         # profile: p= p0*f(x)
                                      ! x - normalized poloidal flux
                                      # for 141711_470

        pw= 0.00             # adds p=const
        zc= dz_gs*(nz-1)/2.
        do i=1,nz
          z= -zc + (i-1)*dz_gs
          do j=1,nr
            x= (psi(i,j)-u1)/(psi0-u1)
            if( x>0. ) then
c           if( x>0. & abs(z) < 27.5) then

              pdx= 0.25
              p(i,j)= 0.5*( 1 + tanh((x-0.73)/pdx)) + 0.1*x**1.5
              pnorm=  0.5*( 1 + tanh(    0.27/pdx)) + 0.1
              p(i,j)= p0*p(i,j)/pnorm
              pp(i,j)= ((1-(tanh((x-0.73)/pdx))**2)/(2*pdx) + 0.15*x**0.5 )/(psi0-u1)
              pp(i,j)= p0*pp(i,j)/pnorm

c             p(i,j)= p0*x**1.2*exp(-3.5*(1-x)**1.6) + p0*pw
c             pp(i,j)= p0*(1.2 + 5.6*x*(1-x)**0.6)*x**0.2*exp(-3.5*(1-x)**1.6)
c    &                   /(psi0-u1)
            else
              p(i,j)= p0*pw
              pp(i,j)= 0.
c             p(i,j)= p0*pw*exp(0.5*x/pw)
c             pp(i,j)= 0.5*p0*exp(0.5*x/pw)/(psi0-u1)
            endif

          enddo j
        enddo i

      case 3:                  # profile: p= p0*f(x)
                               ! x - normalized poloidal flux

c       pw= 0.01               # change profile for p<p0*pw; pw>0.
        pw= 0.07
        xw= (pw/1.6)**(5./6)
        bx= ( 1.6*1.2*xw**0.2-0.6*xw-0.9*xw**2)/( 1.6*xw**1.2-0.3*xw**2-0.3*xw**3 )
        do i=1,nz
          do j=1,nr

            x= (psi(i,j)-u1)/(psi0-u1)
            if( x > xw) then
              p(i,j)= p0*( 1.6*x**1.2 - 0.3*x**2 - 0.3*x**3 )
              pp(i,j)= p0*( 1.6*1.2*x**0.2 - 0.6*x  - 0.9*x**2)/(psi0-u1)
            else
              p(i,j)= p0*( 1.6*xw**1.2 - 0.3*xw**2 - 0.3*xw**3 )
     &                   *exp(bx*(x-xw))
              pp(i,j)= p0*( 1.6*1.2*xw**0.2 - 0.6*xw  - 0.9*xw**2)/(psi0-u1)
     &                   *exp(bx*(x-xw))
            endif

          enddo j
        enddo i

      default:                         # Linear: p=p0*(psi/psi0)*H(psi)
                                       ! for Hill''s vortex solution.
        do i=1,nz
          do j=1,nr
            if( psi(i,j)<0. ) then
              p(i,j)= p0*psi(i,j)/psi0
              pp(i,j)= p0/psi0
            endif
          enddo j
        enddo i

      endselect

      return
      end

c######################################################################
      subroutine hdhdpsi(hi,dhi,hhp1,hhp2,rf1,rf2,psi,rad)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate hi(psi) and d(hi)/d(psi). Done for mhd without rotation.
# Note: fp1(1)<>0 is assumed.

      COMMON_FRC
      real hi(nz,nr), dhi(nz,nr), psi(nz,nr), rad(nr)
      real hhp1(nz,nr), hhp2(nz,nr)
      real sigma, rf1, rf2

c     f1(x)=  1.7*x**2 -0.7*x**3
c     fp1(x)= 3.4*x - 2.1*x**2
c     f2(x)= x**2
c     fp2(x)= 2*x
                                                # for 141711_470
      f1(x)=  5*x**4  - 4*x**5 
      fp1(x)= 20*x**3 - 20*x**4
      f2(x)=  x**5
      fp2(x)= 5*x**4

     
      ERROR_EXIT( rm<>0., hdhdpsi )

      SET_ar( hi, 0., nz*nr )
      SET_ar( dhi, 0., nz*nr )
      SET_ar( hhp1, 0., nz*nr )
      SET_ar( hhp2, 0., nz*nr )
 

      zc= dz_gs*(nz-1)/2.
      do i=1,nz
        z= -zc + (i-1)*dz_gs
        do j=1,nr
          x= (psi(i,j)-u1)/(psi0-u1)
          if( x>0. ) then
c         if( x>0. & abs(z)<29.5 ) then
            hi(i,j) = hi0*sqrt( 1 + alphah1*f1(x) + alphah2*f2(x) )
            dhi(i,j)= 0.5*hi0**2*( alphah1*fp1(x) + alphah2*fp2(x) )
     &                   /(hi(i,j)*(psi0 - u1))
            hhp1(i,j)= 0.5*hi0**2*fp1(x)/(psi0 - u1)
            hhp2(i,j)= 0.5*hi0**2*fp2(x)/(psi0 - u1)
          else
            hi(i,j)= hi0*sqrt( 1 + alphah1*f1(0.) + alphah2*f2(0.) )
            dhi(i,j)= 0.
          endif
        enddo j
      enddo i
      rf1= f1(1.)
      rf2= f2(1.)


      return
      end

c######################################################################
      subroutine j_phii(jphi,deni,u,rad)           # frc - not used
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate fast ion current and density for LMHD=F & LBULK=T case.
! This is for expRR distribution.

      COMMON_FRC
      real jphi(nz,nr), u(nz,nr), deni(nz,nr), rad(nr)

      ifelse(LMHD,T,[return],)
      return

      do i= 1,nz
        do j=1,nr
          rr= omegai0*( u0-u(i,j) + 0.5*omegai0*(rad(j)**2-r0**2) )/ti0
          deni(i,j)= ni0*exp(rr)
          jphi(i,j)= rad(j)*omegai0*deni(i,j)
        enddo j
      enddo i

      return
      end



c######################################################################
      module local_flds
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Contains all local fld parameters needed for velocity integration.

      real :: bl,vdg2l,vdg3l,vdc2l,vdc3l,a22l,a23l,bcbl   # for mu
      real :: psil, rbpl, re2pl, re3pl, rl                # for p_phi

      [end module local_flds]

c######################################################################
      subroutine j_i(sf,jz,jr,jphi,deni,u,hi,rad)   # in P variables
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate fast ion current and density for LMHD=F & LBULK=T case.
! Also, poloidal stream function for GS solver.
! j1, j2, j3, p11, p22, p33 are fluid moments of d.f,
! sf=G stream function, and u -> psi.

      use MPI_module
      use local_flds

      COMMON_FRC ; COMMON_DF
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

      ifelse(LMHD,T,[return],)


# Define F0 parameters.

      vstar= 0.24*v0
      deltav= v0/20.

      lambda0= 0.6
      deltal= 0.3
      rl0= 0.                       # parameter for lambda0(v), 0<=rl0<=1.
      rdl= 0.                       # parameter for deltal(v), 0<=rdl.
      etaf= 0.                      # parameter for F2_add: F2=f2 + etaf*f2_add.
      lambda0a= 0.
      deltala= 0.2                  # parameter for F2_add.
      b0= 1.

c     pplus= -psi0*2.0
c     pminus= psi0*0.1
      pminus= -psi0*0.1
      betav= 3.0

      vmax= v0+2*deltav

c=========================

# Calculate B and etc.

      do i= 1,nz
          r2d(i,:)= rad(:)
      enddo i

      call der_r(tmp, u)
      bz= -tmp/r2d                              # Bz= -1/R*dpsi/dr
      call der_z(tmp, u)
      br= tmp/r2d                               # Br= 1/R*dpsi/dz

      bp= (hi + sf)/r2d                         # h= H+G
c     bp= hi/r2d
      btot= sqrt( bz**2 + br**2 + bp**2 )

      bz= bz/btot                                     # bz
      br= br/btot                                     # br
      bp= bp/btot                                     # bphi

      call curl_gs(vz,vr,vp,bz,br,bp,r2d)             # curl(b)
 
      bcurlb= (bz*vz + br*vr + bp*vp)/btot            # b*curl(b)/B


# Calculate ^e2, ^e3 and vdg and vdc.

      tmp= sqrt( 1. - bz**2 )
      e2r= bp/tmp
      e2p= -br/tmp
      e3z= -tmp
      e3r= br*bz/tmp
      e3p= bp*bz/tmp

      vdc2= ( e2r*vr + e2p*vp )/btot              # curvature drift
      vdc3= ( e3z*vz + e3r*vr + e3p*vp )/btot

      call der_z(vz, btot)                        # grad(B)
      call der_r(vr, btot)

      vdg2= -( e3z*vz + e3r*vr )/btot             # grad(B) drift
      vdg3= e2r*vr/btot

# Calculate a22 and a23.

      a22= -( bz*vz + br*vr )/btot                # div(b)
      a23= -bcurlb*btot

      call der_z(vz, bz)                          # grad(bz)
      call der_r(vr, bz)

      a22= a22 + 2*(e3z*vz + e3r*vr)/tmp
      a23= a23 - 2*e2r*vr/tmp

# Calculate position of the plasma boundary u=u1 and pminus, pplus.

      jb=maxval( maxloc(rad, u((nz+1)/2,:) <= u1 ) )
      rb=maxval(rad, u((nz+1)/2,:) <= u1 )
      jb=jb+1
      rb=rad(jb)

      hb= hi((nz+1)/2,jb) + sf((nz+1)/2,jb)         # h(psi=0)
c     xim=b0/btot((nz+1)/2,jb)
c     pminus= hb*sqrt(xim*(xim-lambda0))/b0
c     pplus= r0*sqrt(1-lambda0/xim)
      
# Start MPI part.


      vmom= 0.
      vmomi= 0.

      if(nprocs < 2) then
        print *,'nprocs=',nprocs, '  in j_i'
        stop 1111
        MPI_ABORT(1)
      endif

      if(myid > master) then               # Slaves calculate moments of F0.

        do j= myid, nr, nprocs-1              # do j=1,nr

          do i=1,nz/2+1
            bl= btot(i,j)
            vdg2l= vdg2(i,j) ; vdg3l= vdg3(i,j)
            vdc2l= vdc2(i,j) ; vdc3l= vdc3(i,j)
            a22l= a22(i,j)
            a23l= a23(i,j)
            bcbl= bcurlb(i,j)
            psil= u(i,j)
            rbpl= rad(j)*bp(i,j)
            re2pl= rad(j)*e2p(i,j)
            re3pl= rad(j)*e3p(i,j)
            rl= rad(j)
            call vmoments(vmom)             # Calculate 3D integrals
            vmomi(i,1:4)=vmom(0:3)
c           deni(i,j)= vmom(0)
c           j1(i,j)= vmom(1)
c           j2(i,j)= vmom(2)
c           j3(i,j)= vmom(3)
          enddo i
          call MPI_send(vmomi, nzd2p1*4, MPI_double_precision, master
     &                                      , j, MPI_comm_world, ierr)

        enddo j
      endif

      if(myid = master) then
                                              ! master process work
        do j= 1,nr
          call MPI_recv(vmomi, nzd2p1*4, MPI_double_precision
     &        , MPI_any_source , MPI_any_tag, MPI_comm_world, stat, ierr)
          jrecv= stat(MPI_tag)
          deni(1:nzd2p1,jrecv)= vmomi(:,1)
          j1(1:nzd2p1,jrecv)  = vmomi(:,2)
          j2(1:nzd2p1,jrecv)  = vmomi(:,3)
          j3(1:nzd2p1,jrecv)  = vmomi(:,4)
        enddo j

        do i= 1, nz/2+1
          deni(nz-i+1,:)= deni(i,:)     # use symmetry rel to nz/2+1
          j1(nz-i+1,:)=  j1(i,:)
          j2(nz-i+1,:)= -j2(i,:)
          j3(nz-i+1,:)=  j3(i,:)
        enddo i

      endif                                   ! end master work

      call MPI_bcast( deni, nz*nr, MPI_double_precision, master
     &              , MPI_comm_world, ierr)
      call MPI_bcast( j1, nz*nr, MPI_double_precision, master
     &              , MPI_comm_world, ierr)
      call MPI_bcast( j2, nz*nr, MPI_double_precision, master
     &              , MPI_comm_world, ierr)
      call MPI_bcast( j3, nz*nr, MPI_double_precision, master
     &              , MPI_comm_world, ierr)

# End MPI part.


# Calculate Ji in (z,r,phi), where J= j1*b + j2*e2 + j3*e3.
# Multiply j1 by (1-Zb/Zeff) coefficient. 9/08/09
! Not needed in equilibrium calculations. 30/3/11

c     j1= (1-zb/zeff)*j1

      jz  = j1*bz          + j3*e3z
      jr  = j1*br + j2*e2r + j3*e3r
      jphi= j1*bp + j2*e2p + j3*e3p

c=========================check
c     sf= r2d*u**3
c     where( sf > 0. ) sf=0.
c     deni= -sf
c     call der_r(jz, sf)
c     jz= jz/r2d
c     call der_z(jr, sf)
c     jr= -jr/r2d
c================================

# Calculate sf=G.

      call der_z(vz, jr)
      call der_r(vr, jz)
      tmp= r2d*(vr - vz)                     # RHS
      zc= dz_gs*(nz-1)/2.
      sf= 0.                                 # BC
      do l= 1,200
        call adi_tk(sf,tmp,nz,nr,zc,rad,err,omega1,omega2)
        if( err<eps*maxval(abs(sf)) ) goto 1
      enddo l
      MPI_print 'no convergence in j_i', ',   err=',err
      stop
 1    continue
      MPI_print '==============================='
      MPI_print '# of interations in j_i=', l, '    err=',err
      MPI_print '==============================='

# Normalize everything so that max(ni)=ni0.

      deni_max= maxval(deni)
c     MPI_print 'maxval(deni)=',deni_max
      fdeni= ni0/deni_max

      deni= deni*fdeni
      jz  =   jz*fdeni
      jr  =   jr*fdeni
      jphi= jphi*fdeni
      sf  =   sf*fdeni


      return
      end
c######################################################################
      subroutine vmoments(vmom)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate 1, v1, v2, v3 - moments of distribution function
!  F0=F0(epsilon,lambda,pphi) in 3D velocity space dv1*dv2*dv3.

      use MPI_module
      use local_flds
      COMMON_FRC ; COMMON_DF

      parameter( eps=5.e-2, lmax=25 )
      real, dimension(4) :: ss, os, st, ost, sum, f1d
      real :: vmom(0:3)



c===============================================
c     vmom(0)= 1.  ; vmom(1:3)= 1.e-6  ;  return
c===============================================

      vmax= v0+2*deltav                    # v3 integration limits.

# Calculate 3D integrals.

      vmom= 0.
      if( vmax < (psil+pminus)/rl ) return
      ax= -vmax
      bx= vmax
                              # inline 1d integration routine qsimp(NR)
         os= -1.e30
c        call int_2d(ss,  ax)
c        call int_2d(f1d, bx)
         ss= 0.                         # F0=0 at boundaries.
         f1d= 0.
         st= 0.5*(bx-ax)*( ss + f1d )
         ost= st
         it= 1
         do l= 2,lmax
           tnm= it
           del= (bx-ax)/tnm
           x= ax + 0.5*del
           sum= 0.
           do ll= 1,it
             call int_2d(f1d, x)
             sum= sum + f1d
             x= x + del
           enddo ll
           st= 0.5*(st + (bx-ax)*sum/tnm)
           it= 2*it
           ss= (4*st - ost)/3.
           if( count( abs(ss-os) < eps*(abs(os)+ZERO) )=4 ) goto 100
           os= ss
           ost=st
         enddo l
         print *, 'too many steps, vmoments', '  myid=',myid
         stop 1111
         MPI_ABORT(2)

 100  continue
      vmom= ss

      return
      end
c######################################################################
      subroutine int_2d( f1d, v3 )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculates integrals with respect to v1 and v2 (for each v3).

      use MPI_module
      use local_flds

      COMMON_FRC ; COMMON_DF
      parameter( eps=5.e-2, lmax=25 )
      real, dimension(4) :: ss, os, st, ost, sum, f1d, f2d
      real v3


      vmax= v0+2*deltav
      f1d= 0.
      if( abs(v3)>=vmax ) return

      v2max= sqrt( vmax**2 - v3**2 )               # integration limits
      v2min= -v2max

      ax= v2min
      bx= v2max

# 1d integration routine qsimp(NR).

      os= -1.e30
c     call int_1d(ss,  ax, v3)           # Calculates f2d(v2,v3).
c     call int_1d(f2d, bx, v3)
      ss= 0.                         # F0=0 at boundaries.
      f2d= 0.
      st= 0.5*(bx-ax)*( ss + f2d )
      ost= st
      it= 1
      do l= 2,lmax
        tnm= it
        del= (bx-ax)/tnm
        x= ax + 0.5*del
        sum= 0.
        do ll= 1,it
          call int_1d(f2d, x, v3)
          sum= sum + f2d
          x= x + del
        enddo ll
        st= 0.5*(st + (bx-ax)*sum/tnm)
        it= 2*it
        ss= (4*st - ost)/3.
        if( count( abs(ss-os) < eps*(abs(os)+ZERO) )=4 ) goto 100
        os= ss
        ost=st
      enddo l
      print *, 'too many steps, int_2d', '  myid=',myid
      print *, 'maxval(abs(os))=',maxval(abs(os))
      stop 1111
      MPI_ABORT(3)

 100  continue
      f1d= ss

      return
      end
c######################################################################
      subroutine int_1d( f2d, v2, v3 )
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculates integrals with respect to v1 (for each v2 and v3).
! Distribution function is defined here by F0 and DFF0 macros.
# New F2 09/2019.

      use MPI_module
      use local_flds
      COMMON_FRC ; COMMON_DF

      parameter( eps=1.e-3, lmax=20 )
      real, dimension(4) :: f2d
      real, dimension(2) :: ss, os, st, ost, sum
      real, dimension(2) :: df, vec
      real v1, v2, v3
      real mu, mu0, lambda, pphi, v, func


# Define F0=f1*f2*f3.

      define(F0,[
        step(xa)= 0.5 + sign(0.5,xa)
        ff1(xa)= exp( -step(xa-v0)*(xa-v0)**2/deltav**2 )
        f1(xa)= ff1(xa)/(xa**3 + vstar**3)
     &          *(1./(r0*xa-psi0-pminus)**betav)

cc      f2(xa)= exp( -( (xa-lambda0)/deltal )**2)
        fl0(ya)= lambda0*( 1 - rl0*(1-ya/v0)*step(v0-ya) )      ! lambda0(v)
        fdl(ya)=  deltal*( 1 + rdl*(1-ya/v0)*step(v0-ya) )      ! dl(v)

        f20(xa,ya)= exp( -( (xa - fl0(ya))/fdl(ya) )**2 )
        f2a(xa)   = exp( -( (xa -lambda0a)/deltala )**2 )
        f2(xa,ya)= f20(xa,ya) + etaf*f2a(xa)

        f3(xa)= step(xa-pminus)*(xa-pminus)**betav
      ])


      F0                                       # dont forget to call


      vmax= v0+2*deltav
      f2d= 0.
      if( (v2**2+v3**2) >= vmax**2 ) return

      v1max= sqrt( vmax**2 - v2**2 - v3**2 )       # integration limits
      v1min= -v1max
c     pphi_min1= -u1 + sqrt(v2**2 + v3**2)*pminus
      pphi_min1= pminus
      v1mm= (psil + pphi_min1 - v2*re2pl - v3*re3pl)/rbpl
      if( rbpl > 0. ) then
        v1min= max( v1min, v1mm )              ! v1>v1mm
      else
        v1max= min( v1max, v1mm )              ! v1<v1mm
      endif

      if( v1min >= v1max ) return

# Macro for F0 calculation.

      vperp2= v2**2 + v3**2
      mu0= 0.5*vperp2/bl
      dmu2= ( (v2**2 - v3**2)*a23l - 2*v2*v3*a22l )/(2*bl)**2
      vec(1)= 1.

      define(DFF0,[
        v1= $2
        vpl2= v1**2
        v= sqrt( vpl2 + vperp2 )                     # agrument of f1

        vd2= mu0*vdg2l + vpl2*vdc2l
        vd3= mu0*vdg3l + vpl2*vdc3l
        mu= 0.5*( (v2-vd2)**2 + (v3-vd3)**2 )/bl + v1*dmu2
        mu= mu*( 1 - v1*bcbl )
c       mu= mu0
        lambda= 1.
c       if( v > 0. ) lambda= 2*mu*b0/v**2            # argument of f2
        if( v > 1.e-4 ) lambda= 2*mu*b0/v**2

c       pphi_min= -u1 + v*pminus
c       pphi_max= -psi0 + v*pplus
        pphi= -psil + v1*rbpl + v2*re2pl + v3*re3pl  # argument of f3

        func= f1(v)*f2(lambda,v)*f3(pphi)*v0**3

c       arg2= ((lambda - fl0(v))/fdl(v) )**2
c       if( arg2 > 10. ) then
c         func= 0.
c         print *,'arg2=',arg2
c       endif

        vec(2)= v1
        $1= vec*func
      ])


      ax= v1min
      bx= v1max

# 1d integration routine qsimp(NR).

      os= -1.e30
      DFF0(ss,ax)                       # Calculates f0(v1,v2,v3).
      DFF0(df,bx)
      st= 0.5*(bx-ax)*( ss + df )
      ost= st
      it= 1
      do l= 2,lmax
        tnm= it
        del= (bx-ax)/tnm
        x= ax + 0.5*del                      # v1 value
        sum= 0.
        do ll= 1,it
          DFF0(df,x)
          sum= sum + df
          x= x + del
        enddo ll
        st= 0.5*(st + (bx-ax)*sum/tnm)
        it= 2*it
        ss= (4*st - ost)/3.
        if( count( abs(ss-os) < eps*(abs(os)+ZERO) )=2 ) goto 100
        os= ss
        ost=st
      enddo l
      print *, 'too many steps, int_1d', '   myid=',myid
      stop 1111
      MPI_ABORT(4)

 100  continue
      f2d(1)= ss(1)              # Int[f0]dv1
      f2d(2)= ss(2)              # Int[v1*f0]dv1
      f2d(3)= v2*ss(1)           # Int[v2*f0]dv1
      f2d(4)= v3*ss(1)           # Int[v3*f0]dv1

#======================================================================
      contains
#======================================================================
# Internal function Erf- error function, for F2 normalization.

[     function erf(x)    result(errorf)
        real, intent(in) :: x
        real :: errorf, erfcc, z, t
        z=abs(x)
        t=1./(1+0.5*z)
        erfcc=t*exp( -z**2 -1.26551223 +t*( 1.00002368 + t*( .37409196
     &           + t*( .09678418 + t*( -.18628806 + t*( .27886807
     &           + t*( -1.13520398 + t*( 1.48851587 + t*( -.82215223
     &           + t*0.17087277 ) ) ) ) ) ) ) ) )
        errorf= 1.-erfcc
        if(x < 0.) errorf=-errorf
      end function erf
]


c     return                  !cannot use with f90 contains
      end
c######################################################################
      subroutine psi_tk_bc(psie,zc,rc,rad)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Flux at the walls r=Ri & Rc and at the edge z=+-Zc.
! For uniform vertical field Bv and current loop at r=r0.
! Bv= -|bv|.
# New version with all parameters decoupled (ie bv, riphi and rmir);
! for now, need to specify riphi_c and estimated bv and r0 by hand.

      COMMON_FRC
      real psie(nz,nr), psiv(nz,nr), rad(nr)

      dz= 2*zc/(nz-1)
      betap= 1.0
      rli= 0.7
      riphi_c=66.
      bvert0= riphi_c/r0*( log(8.*r0/zc) - 1.5 + betap + 0.5*rli )
      bvert= bv*bvert0/(4.*PI)


      ivert= 2                     # external field solution index.

      select( ivert )

      case 1:                     # psi= ( a + b*(r**2-4*z**2) )*r**2

        bda= 0.25*(1-rmir)/zc**2
        a= 0.5*bvert
        b= bda*a
        do i= 1,nz                                      # r=Rc
          z= -zc + (i-1)*dz
          do j= 1,nr
            r2= ( rad(j) )**2
            psiv(i,j)= ( a + b*(r2 - 4*z**2) )*r2
          enddo j
        enddo i

      case 2:                     # psi= f*psi1 + (1-f)*psi3

        a= 0.5*bvert
        bda= 0.25*(1-rmir)/zc**2
c       b= bda*a
        bvert1=1.80*bvert0/(4.*PI)
        b= bda*0.5*bvert1
        do i= 1,nz                                      # r=Rc
          z= -zc + (i-1)*dz
          do j= 1,nr
            r2= ( rad(j) )**2
            psiv(i,j)= ( a + b*(r2 - 4*z**2) )*r2
          enddo j
        enddo i

c       fv= 0.0                       # not used.
c       rmir3= 0.5
c       aa= 0.5*(rmir3 + 1)
c       ak= 1.0*PI/zc
c       ua= 0.5*bvert*rc**2
c       psir0= ua/( 0.5*aa*rc**2 - (aa-1)*rc/ak*BESSI1(ak*rc) )
c       do i= 1,nz
c         z= -zc + (i-1)*dz
c         do j= 1,nr
c           rr= rad(j)
c           psiv3= psir0
c    &            *( 0.5*aa*rr**2 - (aa-1)*rr/ak*BESSI1(ak*rr)*cos(ak*z) )
c           psiv1= psiv(i,j)
c           psiv(i,j)= (1-fv)*psiv1 + fv*psiv3
c         enddo j
c       enddo i

      case 3:                     # psi~ r*BESSI1(a*r)*cos(a*z)

        a= 0.5*(rmir + 1)
        ak= PI/zc
        ua= 0.5*bvert*rc**2
        do i= 1,nz
          z= -zc + (i-1)*dz
          psir0= ua/( 0.5*a*rc**2 - (a-1)*rc/ak*BESSI1(ak*rc) )
          do j= 1,nr
            rr= rad(j)
            psiv(i,j)= psir0
     &            *( 0.5*a*rr**2 - (a-1)*rr/ak*BESSI1(ak*rr)*cos(ak*z) )
          enddo j
        enddo i


      default:            # psiv= ( a + b*(r**2-4*z**2) )*r**2

        bda= 0.25*(1-rmir)/zc**2
        a= 0.5*bvert
        b= bda*a
        do i= 1,nz 
          z= -zc + (i-1)*dz
          do j= 1,nr
            r2= ( rad(j) )**2
            psiv(i,j)= ( a + b*(r2 - 4*z**2) )*r2
          enddo j
        enddo i

      endselect



# Add plasma current to external field.
      rax= r0
      rax= 23.

      do i= 1,nz                                      # r=Rc
        z= -zc + (i-1)*dz
        do j= 1,nr
          r2= ( rad(j) )**2
          rrz= sqrt( (rax+rad(j))**2 + z**2 )
          rkk= sqrt( 4.*rax*rad(j) )/rrz
          rkk1= sqrt( 1. - rkk**2 )
          if( rkk=1. ) then
            psie(i,j)= -1.
          else
            ekk= CEL(rkk1,1.,1.,1.)              # elliptic K(rkk)
            eek= CEL(rkk1,1.,1.,rkk1**2)         # elliptic E(rkk)
            psie(i,j)= - 2*riphi*rrz*( (1-0.5*rkk**2)*ekk-eek )
            psie(i,j)= psie(i,j)/(4.*PI)
c           psie(i,j)= psie(i,j) + psiv(i,j)
          endif
        enddo j
      enddo i

! Add a couple of extra current loops at r=rax and z=+/-za.

      za= zc/5.0
c     rax= 23.
      do i= 1,nz
        z= -zc + (i-1)*dz
        do j= 1,nr
          r2= ( rad(j) )**2
          psie_c= psie(i,j)

          rrz= sqrt( (rax+rad(j))**2 + (z-za)**2 )
          rkk= sqrt( 4.*rax*rad(j) )/rrz
          rkk1= sqrt( 1. - rkk**2 )
          if( rkk=1. ) then
            psie_r= -1.
          else
            ekk= CEL(rkk1,1.,1.,1.)              # elliptic K(rkk)
            eek= CEL(rkk1,1.,1.,rkk1**2)         # elliptic E(rkk)
            psie_r= - 2*riphi*rrz*( (1-0.5*rkk**2)*ekk-eek )
            psie_r= psie_r/(4.*PI)
          endif

          rrz= sqrt( (rax+rad(j))**2 + (z+za)**2 )
          rkk= sqrt( 4.*rax*rad(j) )/rrz
          rkk1= sqrt( 1. - rkk**2 )
          if( rkk=1. ) then
            psie_l= -1.
          else
            ekk= CEL(rkk1,1.,1.,1.)              # elliptic K(rkk)
            eek= CEL(rkk1,1.,1.,rkk1**2)         # elliptic E(rkk)
            psie_l= - 2*riphi*rrz*( (1-0.5*rkk**2)*ekk-eek )
            psie_l= psie_l/(4.*PI)
          endif

          psie(i,j)= 0.4*psie_c + 0.30*psie_r + 0.30*psie_l 
          psie(i,j)= psie(i,j) + psiv(i,j)
        enddo j
      enddo i

# Modify bc at R=Ri.

      rtmp= psie(1,1)
      do i= 1,nz
        psie(i,1)= rtmp + 0.3*( psie(i,1) - rtmp )
      enddo i

# Make sure psie=0. at R=Ri, z=0.

      icenter= nz/2 + 1
      psie_c= psie(icenter,1)
      do i= 1,nz
        do j= 1,nr
          psie(i,j)= psie(i,j) - psie_c
        enddo j
      enddo i

c     print *,'psie at  R=Ri',psie(1,1),psie(nz,1)

      return
      end

######################################################################
      subroutine read_psi_bc(psie,u,zc,rc,rad)
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Reads flux at the walls r=Ri & Rc and at the edge z=+-Zc from file.

      use MPI_module

      COMMON_FRC
      real psie(nz,nr),u(nz,nr),rad(nr)
      real psi_lo(nz),psi_hi(nz)
      real psi_le(nr),psi_ri(nr)


      open(91,file='psi_bc.dat',status='old')
      read(91,*) mynz,mynr
      read(91,*) z_lo,z_hi,r_lo,r_hi
      close(91)

      if(nz<>mynz | nr<>mynr) then
        print *,'mynz<>nz or mynr<>nr in read_psi_bc'
        print *,'mynz,mynr=',mynz,mynr
        print *,'nz,nr=    ',nz,nr
        stop 1112
      endif

      MPI_print 'Reading psi BCs from file=psi_bc.dat'
      MPI_print 'z range=',z_lo,z_hi,'  vs zc=',zc
      MPI_print 'r range=',r_lo,r_hi,'  vs rci,rc=',rci,rc

      open(91,file='psi_bc.dat',status='old')
      read(91,*) mynz,mynr
      read(91,*) z_lo,z_hi,r_lo,r_hi
      read(91,*) psi_lo
      read(91,*) psi_hi
      read(91,*) psi_le
      read(91,*) psi_ri
      close(91)

# Change BCs.
      do i= 1,nz
        psie(i,1) = psi_lo(i)
        psie(i,nr)= psi_hi(i)
      enddo i

      do j= 1,nr
        psie(1,j) = psi_le(j)
        psie(nz,j)= psi_ri(j)
      enddo j

# Generate initial values of psie consistent with BC.
      do i= 2,nz-1
        j0= 1
        do j= 2,nr-1
          if( u(i,j) < 0. ) then
            psie(i,j)= u(i,j)
            j0= j+1
          else
            if(j0 > 1) then
              psie(i,j)= psi_hi(i)*(rad(j)**2 - rad(j0)**2)/(rc**2 - rad(j0)**2)
            else
              psie(i,j)= psi_lo(i) + (psi_hi(i)-psi_lo(i))*(rad(j)**2-rad(1)**2)
     &                                                    /(rc**2 - rad(1)**2)
            endif
          endif
        enddo j
      enddo i

      return
      end

c######################################################################
      subroutine adi_tk(u,g,nz,nr,zc,rad,err,omega1,omega2)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Solve equation:   [d^2/dz^2 + r*d/dr(1/r*d/dr)] u = g(u)  in a
! region=(-zc<z<zc, ri<r<rc) using ADI with omega_z != omega_r.
# For tor mhd BC are: u(z,r)=psie(z,r) at all boundaries.

      real u(nz,nr), uu(nz,nr), g(nz,nr)
      real rad(nr), bz(nz), br(nr)
      real a1(nz),a2(nz),a3(nz)
      real aa1(nr),aa2(nr),aa3(nr)
      real dz,dr,omega1,omega2
      real fm,fp

      err= 0.
      dz= 2*zc/(nz-1)
      dr= (rad(nr)-rad(1))/(nr-1)

      do i=1,nz
        do j=1,nr
          uu(i,j)= u(i,j)
        enddo j
      enddo i


# Sweep in z direction. Note bc.

      dzdr2= (dz/dr)**2

      do j=2,nr-1

        do i=2,nz-1             # Define diagonals of 3-diag. matrix A. 
          ii= i-1
          a1(ii)= 1.
          a2(ii)= -2.-omega1
          a3(ii)= 1.
        enddo i
                                           # Calculate RHS of A*y=bz.
        fm= dzdr2*rad(j)/(rad(j)-0.5*dr)
        fp= dzdr2*rad(j)/(rad(j)+0.5*dr)
        do i=2,nz-1
          ii= i-1
          bz(ii)= - fm*u(i,j-1) + (fm+fp-omega1)*u(i,j) - fp*u(i,j+1)
     &                                                 + dz**2*g(i,j)
        enddo i
        bz(1)= bz(1) - u(1,j)                     # BC at z=+-Zc.
        bz(nz-2)= bz(nz-2) - u(nz,j)

        call THOMAS(3,a1,a2,a3,bz,nz-2)       # solve A*y=bz; y -> bz.

        do i=2,nz-1
          ii= i-1
          uu(i,j)= bz(ii)
        enddo i

      enddo j


# Sweep in r direction.

      drdz2= 1./dzdr2

      do i=2,nz-1                         # Calculate RHS of A*y=br.
     
        do j=2,nr-1
          jj= j-1
          br(jj)= - drdz2*uu(i-1,j) + ( 2.*drdz2-omega2 )*uu(i,j)
     &             - drdz2*uu(i+1,j) + dr**2*g(i,j)
        enddo j

                                                 # BC at r=R_i & R_c.   
        br(1)= br(1) - uu(i,1)*rad(2)/( rad(2)-0.5*dr )
        br(nr-2)= br(nr-2) - uu(i,nr)*rad(nr-1)/( rad(nr-1)+0.5*dr )

                               # Define diagonals of 3-diag. matrix A.
        do j=2,nr-1         
          aa1(j-1)= rad(j)/( rad(j)-0.5*dr )
          aa3(j-1)= rad(j)/( rad(j)+0.5*dr )
          aa2(j-1)= ( - aa1(j-1) - aa3(j-1) - omega2 )
        enddo j

        call THOMAS(3,aa1,aa2,aa3,br,nr-2)     # solve A*y=br; y -> br.

        do j=2,nr-1
          utmp= br(j-1)
          err= amax1( err, abs(u(i,j)-utmp) )
          u(i,j)= utmp
        enddo j

      enddo i

      return
      end
c######################################################################
      subroutine der_z( ao, ai )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# ao= first derivative of ai with respect to z (GS-grid).

      COMMON_FRC
      real ao(nz,nr), ai(nz,nr), atmp(nz,nr)
      real ddz(-2:2)

      atmp= 0.
      nzm= nz-1
      nzm2= nz-2

      oddz12= 1./(12.*dz_gs)
      ddz(-2)= oddz12
      ddz(-1)= -8.*oddz12
      ddz(0) = 0.
      ddz(1) =  8.*oddz12
      ddz(2) = -oddz12

      do j=1,nr                                       # 4th-order
        do i= 3,nzm2
          atmp(i,j)= ddz(-1)*ai(i-1,j)
     &              +ddz( 1)*ai(i+1,j)
     &              +ddz(-2)*ai(i-2,j)
     &              +ddz( 2)*ai(i+2,j)
        enddo i
      enddo j
      atmp(2,:)= ( ai(3,:) - ai(1,:) )/(2*dz_gs)       # 2d-order
      atmp(nzm,:)= ( ai(nz,:) - ai(nzm2,:) )/(2*dz_gs)
      atmp(1,:)= ( ai(2,:) - ai(1,:) )/dz_gs
      atmp(nz,:)= ( ai(nz,:) - ai(nzm,:) )/dz_gs

      ao= atmp

      return
      end
c######################################################################
      subroutine der_r( ao, ai )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# ao= first derivative of ai with respect to r (GS-grid).

      COMMON_FRC
      real ao(nz,nr), ai(nz,nr), atmp(nz,nr)
      real ddr(-2:2)

      atmp= 0.
      nrm= nr-1
      nrm2= nr-2

      oddrr12= 1./(12.*dr_gs)
      ddr(-2)= oddrr12
      ddr(-1)= -8.*oddrr12
      ddr(0) = 0.
      ddr(1) =  8.*oddrr12
      ddr(2) = -oddrr12

      do i= 1,nz
        do j=3,nrm2
          atmp(i,j)= ddr(-1)*ai(i,j-1)
     &              +ddr( 1)*ai(i,j+1)
     &              +ddr(-2)*ai(i,j-2)
     &              +ddr( 2)*ai(i,j+2)
        enddo j
      enddo i
      atmp(:,2)= ( ai(:,3) - ai(:,1) )/(2*dr_gs)       # 2d-order
      atmp(:,nrm)= ( ai(:,nr) - ai(:,nrm2) )/(2*dr_gs)
      atmp(:,1)= ( ai(:,2) - ai(:,1) )/dr_gs
      atmp(:,nr)= ( ai(:,nr) - ai(:,nrm) )/dr_gs

      ao= atmp

      return
      end
c######################################################################
      subroutine curl_gs(voz,vor,vophi,viz,vir,viphi,r2d)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculates curl of vi on GS-grid: vo=curl(vi) (assuming d/d phi=0).

      COMMON_FRC
      real voz(nz,nr), vor(nz,nr), vophi(nz,nr)
      real viz(nz,nr), vir(nz,nr), viphi(nz,nr)
      real tmp(nz,nr), r2d(nz,nr)

      tmp= r2d*viphi               # vz
      call der_r(voz, tmp)
      voz= voz/r2d

      call der_z(vor, viphi)       # vr
      vor= -vor

      call der_z(vophi, vir)       # vphi
      call der_r(tmp, viz)
      vophi= vophi - tmp

      return
      end
c######################################################################
# macros for initp. Linear interpolation.

          define(TERP_SETUP,[
            $1[]$1= r[]$1
            $1[]p= $1[]$1 + 1
            f[]$1[]p= r[]$1 - $1[]$1
            f[]$1[]$1= 1. - f[]$1[]p
          ])

          define(TERP_k,[ ( fjj*( fii*$1(ii,jj,$2)
     &                           +fip*$1(ip,jj,$2) )
     &                    + fjp*( fii*$1(ii,jp,$2)
     &                           +fip*$1(ip,jp,$2) ) ) ])

          define(INTERP,[
            ifelse(DIMEN,3,[
              $1 =  (
     &            fkk*
     &                TERP_k($2,kk)
     &          + fkp*
     &                TERP_k($2,kp)
     &                            )
            ],[
              $1 = TERP_k($2,1)
            ])
          ])

c######################################################################
    ifelse(LPGRID,T,[
      subroutine initpart  # tk
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Initialize phase space for particles.

      use MPI_module

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD ; COMMON_PART
      COMMON_SPEC ; COMMON_PGRID 

      COMMON_FRC ; COMMON_DF

      DIM_sp(sppsi) ; DIM_s(stmp,sden)
      parameter( frmin= 0.30, frmax= 1.00, fzmin= 0.7, fzmax= 0.7 )
      parameter(nrg=200, nzg=400, nvg=300)
      dimension rrd(nrg), zd(nzg,nj), vd(ni,nj,nvg)
      dimension psen(np)

c     return

      rmin= frmin*rhi
      rmax= frmax*rhi
      zmin= fzmin*qlo
      zmax= fzmax*qhi
      rmin2= rmin**2
      rmax2= rmax**2
      call maptop_s( sppsi, spsi )      # Map psi into particle grid.

      ifelse(LOAD,1,[
        call nstx_load( sden, vd, nvg )
        call inversed( rrd, zd, nrg, nzg, sden )
c       call inversed( rrd, zd, nrg, nzg, srhoq0 )
        dd1d= 1./(nrg-1)
        dd2d= 1./(nzg-1)
        dvd=  1./(nvg-1)
      ],)

# LPGRID=T.   Load particles inside a 3D cylinder.
# Loop over species:

      do isp = 1, nsp
        tsp= vthplsp(isp)**2/2.

        mm= 0
        do m = mfsp(isp), mlsp(isp)            # Loop over particles.

          ifelse(LOAD,0,[
# Load marker particles uniformly inside rmin<r<rmax & zmin<z<zmax.

            ifelse(PARTONGRID,T,[
 200          mm= mm + 1
              i= mod(mm-1,nid) + 3
              jm= (mm-1)/nid
              j= mod(jm,nxd) + 3
              km= jm/nxd
              k= mod(km,nyd) + 3
              if( ibp(j,k)=0 ) goto 200
              pv1x1(m) = q(i)
              pv2x1(m) = x(j)
              pv3x1(m) = y(k)
              rm2= (pv2x1(m)-x0)**2 + (pv3x1(m)-y0)**2
            ],[

c             pv1x1(m) = RAN*qlen + qlo
              pv1x1(m) = RAN*(zmax-zmin) + zmin
 100          continue
              pv2x1(m) = RAN*xlen + xlo
              pv3x1(m) = RAN*ylen + ylo
              rm2= ( pv2x1(m)-x0 )**2 + ( pv3x1(m)-y0 )**2
c             if( rm2 > rlen**2 ) goto 100
              if( rmin2 > rm2 | rmax2 < rm2 ) goto 100
            ])

# Slowing-down distribution for NSTX.

            vmax= v0+2*deltav
            vm= vstar*( ((vmax/vstar)**3+1)**RAN - 1. )**(1./3)
c           vm= vmax*(RAN)**(1./3)
            th1= rpi*RAN
            th2= rpi2*RAN
            pv1v1(m)= vm*cos(th1)                    # v_z
            pv2v1(m)= vm*sin(th1)*cos(th2)           # v_x
            pv3v1(m)= vm*sin(th1)*sin(th2)           # v_y

            rrm= sqrt(rm2)                                    # r
            rrm= rrm + 1.e-10
            cosm= (pv2x1(m)-x0)/rrm
            sinm= (pv3x1(m)-y0)/rrm

            vs0= omegai0*rrm                            # V_phi0= w*r
            vsm= - pv2v1(m)*sinm + pv3v1(m)*cosm        # V_phi

# Interpolate psi to particle location.
            ri= ( pv1x1(m) -q(1) )*dqi + 1.
            rj= ( pv2x1(m) -x(1) )*dxi + 1.
            rk= ( pv3x1(m) -y(1) )*dyi + 1.
            TERP_SETUP(i)
            TERP_SETUP(j)
            TERP_SETUP(k)
            INTERP(psim,sppsi)

# Calculate p_phi for NSTX loading.
            pspphi(m)= rrm*vsm - psim
            psen(m)= 0.5*vm**2                        # save E
            psr(m)= rrm                               # save R

            psw(m)= 0.
            psp(m)= 1.               # defined in p_weight


          ],[         # LOAD=1
# Load marker particles as physical particles with P^= F(t=0)
! or P^= F_marker(t=0).


 300        continue
            d1dm= RAN                              # find r_m
            rj= d1dm/dd1d + 1.
            TERP_SETUP(j)
            rm2= fjj*rrd(jj) + fjp*rrd(jp)
            rrm= sqrt(rm2)

            d2dm= RAN                              # find z_m
            ri= d2dm/dd2d + 1.
            rj= ( rrm - r(1) )*dri + 1.
            TERP_SETUP(i)
            TERP_SETUP(j)
            zzm=  fjj*( fii*zd(ii,jj) + fip*zd(ip,jj) )
     &          + fjp*( fii*zd(ii,jp) + fip*zd(ip,jp) )

            sm= rpi2*RAN                           # find phi_m

            pv1x1(m)= zzm
            pv2x1(m)= x0 + rrm*cos(sm)
            pv3x1(m)= y0 + rrm*sin(sm)

# For test particles.
c           if( m<10 ) then
c             pv1x1(m)= 0.                        # z=0.
c             rrm= 0.2*rhi + 0.6*rhi*m/9          # 0.2<r/Rc<0.8
c             pv2x1(m)= x0 + rrm*cos(sm)
c             pv3x1(m)= y0 + rrm*sin(sm)
c           endif

# Interpolate psi to particle location.
            ri= ( pv1x1(m) -q(1) )*dqi + 1.
            rj= ( pv2x1(m) -x(1) )*dxi + 1.
            rk= ( pv3x1(m) -y(1) )*dyi + 1.

            TERP_SETUP(i)
            TERP_SETUP(j)
            TERP_SETUP(k)

            INTERP(psim,sppsi)

# Load velocity distribution.

            ifelse(1,1,[
                            # Load P^=f1(v)*f3(pphi)*v using vd(i,j,l)
                            !  for v. Assumes f3=(pphi-pminus)^betav.
              vdm= RAN                                    # find v_m
              rk= vdm/dvd + 1.
              ri= ( zzm - q(1) )*dqi + 1.
              rj= ( rrm - r(1) )*dri + 1.
              TERP_SETUP(i)
              TERP_SETUP(j)
              TERP_SETUP(k)
              vm=  fkk*( fjj*( fii*vd(ii,jj,kk) + fip*vd(ip,jj,kk) )
     &                 + fjp*( fii*vd(ii,jp,kk) + fip*vd(ip,jp,kk) ) )
     &           + fkp*( fjj*( fii*vd(ii,jj,kp) + fip*vd(ip,jj,kp) )
     &                 + fjp*( fii*vd(ii,jp,kp) + fip*vd(ip,jp,kp) ) )

              vmax= v0+2*deltav
              vpsi= (psim+pminus)/rrm
              vmin= max(0., vpsi)
              if( vmin>=vmax ) goto 300
              if( vm<=vmin | vm>=vmax ) goto 300

              dvphi= RAN                                 # find v_phi
              betavp= betav+1
              if( (-vm) <= vpsi ) then
                vphim= vpsi + (vm-vpsi)*dvphi**(1./betavp)
              else
                vphim= vpsi + ( (1-dvphi)*(-vm-vpsi)**betavp 
     &                       + dvphi*(vm-vpsi)**betavp  )**(1./betavp)
              endif
              if( abs(vphim)>vm ) stop 8811
              pspphi(m)= rrm*vphim - psim              # calculate p_phi
              psen(m)= 0.5*vm**2                        # save E
              psr(m)= rrm                               # save R


              vp2= vm**2 - vphim**2                       # find vr & vz
              vp= 0.
              if( vp2>0. ) vp= sqrt(vp2)
              thetam= rpi2*RAN
              vzm= vp*cos(thetam)
              vrm= vp*sin(thetam)
              
              pv1v1(m)= vzm                              # vz
              pv2v1(m)= vrm*cos(sm) - vphim*sin(sm)      # vx
              pv3v1(m)= vrm*sin(sm) + vphim*cos(sm)      # vy

            ],[
                              # Load Maxwellian distribution: exp(RR).
              vparl= vthplsp(isp)*sqrt(-log(RAN+ZERO))
              th1= rpi2*RAN
              vperp= vthprsp(isp)*sqrt(-log(RAN+ZERO))
              th2= rpi2*RAN
              if( ialign=1 ) then
                pv1v1(m)= vparl*cos(th1)
                pv2v1(m)= vperp*cos(th2)
                pv3v1(m)= vperp*sin(th2)
              else
                print *, 'Other align options not yet supported.'
                stop 2854
              endif
              vs0= omegai0*rrm                            # V_phi0= w*r
              pv2v1(m)= pv2v1(m) - vs0*sin(sm)
              pv3v1(m)= pv3v1(m) + vs0*cos(sm)

            ])

# Set initial perturbation and weights. Now done in p_weights.

            vzampl= 0.
            if(psim < psi0/4.) vzampl= vpert
     &                              *((psim-psi0/4.)/psi0)/6.*cos(sm)
c           if(psim < psi0/4.) vzampl= vpert*(RAN-0.5)/3.

            ifelse(LDF,T,[
c             psw(m)= pv1v1(m)*vzampl/tsp              # Vz*V0z/(T/m)
              psw(m)= 0.
              psp(m)= 1.
            ],[
              psw(m)= 1.
              psp(m)= 1.
c             pv1v1(m)= pv1v1(m) + vzampl
            ])

          ])

c         nm= m/100000
c         if( nm*100000=m ) then
c           MPI_print m
c         endif
        enddo m

      enddo isp

      MPI_print '***************************'
      MPI_print 'min/max(zm)=',minval(pv1x1(1:np)),maxval(pv1x1(1:np))
      MPI_print '***************************'

# Calculate particle weights for NSTX loading with LOAD=0 or LOAD=1.

      ifelse(LOAD,0,[
        call p_weight
      ],[
        call p_weight2       # LOAD=1
      ])

# Normalize particle weights.

      wsum= 0.
      do m= 1,np
        wsum= wsum + psp(m)
      enddo m
      do m= 1,np
        psp(m)= psp(m)*np/wsum
      enddo m

# Normalize qsp and wsp.
      MUL_aaa( stmp, srhoq0, sh3w, nijk)      # Multiply by Jacobian
      SUM_rs( qtot, stmp )                    # Sum at all grid points.
      qtot= qtot*dr*ds/dx/dy                  # Total ion charge.
c     MPI_print qsp(1),qtot/np
      do isp= 1,nsp
        qsp(isp)= qtot/np
        wsp(isp)= qsp(isp)*wdqsp(isp)
      enddo isp

# Check initial loading.

      call frompart

      rmax0= 0.
      rmax1= 0.
      SDO
        if( srhoq0(i,j,k) > rmax0 ) rmax0= srhoq0(i,j,k)
        if( srhoq(i,j,k) > rmax1 ) rmax1= srhoq(i,j,k)
      SENDDO

      MPI_print '========Check initial loading========='
      MPI_print 'rmax0=', rmax0,'   rmax1=',rmax1
      MPI_print 'delta_n=', maxval(abs(srhoq0(:,:,3)-srhoq(:,:,3)))

      do isp= 1,nsp
        qsp(isp)= qsp(isp)*rmax0/rmax1
        wsp(isp)= qsp(isp)*wdqsp(isp)
      enddo isp

      call frompart

      MPI_print 'delta_n=', maxval(abs(srhoq0(:,:,3)-srhoq(:,:,3)))
      MPI_print '========================================'


# For hybrid scheme make sure srhoq<>0; if LOAD=1 have to do it
! after particles are loaded.

      ifelse(LBULK,F,[
        SDO
          if( srhoq0(i,j,k) < rhoc ) srhoq0(i,j,k)= rhoc
        SENDDO
      ],)

# Initialize resistivity profile ~1/srhoq. For LBULK=T done in initfld.

      ifelse(LBULK,F,[
c       INV_aa( sres, srhoq0, nijk )
c       MUL_aar( sres, sres, ni0, nijk )
        SET_ar( sres, 1., nijk )
      ],)

      j3=3
      if( ibstyp(1,2,SBC)=7 ) j3=1       # cyl. geometry with Ri=0.

# Save equilibrium densities.
      open(unit=15,file='rhoi.dat')
      write(15,*) 'rhoi'
      write(15,9) nid, njm2-j3+1
      write(15,10) (q(i),i=3,nim2),(r(j),j=j3,njm2)

      write(15,10) (( srhoq(i,j,6),i=3,nim2),j=j3,njm2)
      write(15,10) ((v3curi(i,j,6),i=3,nim2),j=j3,njm2)
      write(15,10) ((v1curi(i,j,6),i=3,nim2),j=j3,njm2)
      write(15,10) ((v2curi(i,j,6),i=3,nim2),j=j3,njm2)
  9   format(2i5)
 10   format(5es12.3)

      close(15)

# Save particle data.

      if(myid = master) then

        open( unit=18,file='en.dat')          # energy
        write(18,*) np
        write(18,'(5es12.3)') (psen(m), m=1,np)
        close(18)

        do m= 1,np                            # use psen to save lambda
          if( psen(m)=0. ) then
            psen(m)= 1.
          else
            psen(m)= psmu(m)*b0/psen(m)
          endif
        enddo m
        open( unit=19,file='ff.dat')          # Lambda and pphi and R.
        write(19,*) np
        write(19,'(5es12.3)') (psen(m), m=1,np),(pspphi(m), m=1,np)
     &                       ,(psr(m), m=1,np)
        close(19)

        open( unit=20,file='psp.dat')         # Equilibrium weight
        write(20,*) np
        write(20,'(5es12.3)') (psp(m), m=1,np)
        close(20)
      endif

      RETURN_END
    ],)
c######################################################################
   ifelse(LPGRID,T,[
      subroutine p_weight2                       # NSTX for LOAD=1
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate equilibrium weights (psp) for particles assuming
! P=F1(v)*F3(pphi) and psp(m)=F/P=F2(lambda).
! Need psmu(m) here.

      use MPI_module

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD ; COMMON_PART
      COMMON_SPEC ; COMMON_PGRID 
      COMMON_FRC ; COMMON_DF

# Mu calculation stuff (E=0 assumed).
      DIM_s(stmp1, stmp3)
      DIM_v(d1,d2,tmp)
      real, allocatable, dimension(:,:,:) :: sptmp1,sptmp3
      real, allocatable, dimension(:,:,:) :: vp1b,vp2b,vp3b
      real, allocatable, dimension(:,:,:) :: vp1d1,vp2d1,vp3d1
      real, allocatable, dimension(:,:,:) :: vp1d2,vp2d2,vp3d2

      F0                # this is macro for F0=f1(v)*f2(lam,v)*f3(pphi).

      allocate( sptmp1(ni,nx,ny), sptmp3(ni,nx,ny) )
      allocate( vp1b(ni,nx,ny), vp2b(ni,nx,ny), vp3b(ni,nx,ny) )
      allocate( vp1d1(ni,nx,ny), vp2d1(ni,nx,ny), vp3d1(ni,nx,ny) )
      allocate( vp1d2(ni,nx,ny), vp2d2(ni,nx,ny), vp3d2(ni,nx,ny) )

# Calculate psmu(m) as in hybm_test.p.
! Calculate V_d= (mu/m)* b x grad(B)/B + U^2* curl(b)_perp/B, and
!  stmp1= b*curl(b)/B, stmp3=|B|.

      stmp3= sqrt( v1b1**2 + v2b1**2 + v3b1**2 )          # B

      DIVI_vvs( V(d1), V(b1), stmp3 )           # b=B/B
      call curl_vv( V(d2), V(d1), VBC_j)        # =curl(b)
      DOT_svv( stmp1, V(d1), V(d2) )
      MUL_vvs( V(d1), V(d1), stmp1 )
      SUB_vvv( V(d2), V(d2), V(d1) )            # =curl(b)_perp
      DIVI_vvs( V(d2), V(d2), stmp3 )

      DIVI_aaa( stmp1, stmp1, stmp3, nijk )     # =b*curl(b)/B

      call grad_vs( V(d1), stmp3, VBC_v )
      call cros_vvv( V(tmp), V(b1), V(d1) )
      DIVI_vvs( V(d1), V(tmp), stmp3 )
      DIVI_vvs( V(d1), V(d1), stmp3 )           # =Bxgrad(B)/(B^2)

# Map all to particle grid.

      call maptop_s( sptmp1, stmp1 )
      call maptop_v( vp1b, vp2b, vp3b, V(b1) )
      call maptop_v( vp1d1, vp2d1, vp3d1, V(d1) )
      call maptop_v( vp1d2, vp2d2, vp3d2, V(d2) )

      rlo2= rlo**2
      rhi2= rhi**2
      mout= 0

# Loop over species:

      do isp= 1, nsp

# Loop over particles of a given species.


        do m= mfsp(isp), mlsp(isp)


# Interpolate fields to particle locations.
                                     # Interpolate to particle grid.
          ri= ( pv1x1(m) -q(1) )*dqi + 1.
          rj= ( pv2x1(m) -x(1) )*dxi + 1.
          rk= ( pv3x1(m) -y(1) )*dyi + 1.

          TERP_SETUP(i)
          TERP_SETUP(j)
          TERP_SETUP(k)

          INTERP(bz,vp1b)           # B
          INTERP(bx,vp2b)
          INTERP(by,vp3b)

          INTERP(v11m,vp1d1)        # drift velocities
          INTERP(v21m,vp2d1)
          INTERP(v31m,vp3d1)
          INTERP(v12m,vp1d2)
          INTERP(v22m,vp2d2)
          INTERP(v32m,vp3d2)

# Calculate mu + corrections.

          v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2     # V^2

          bm= sqrt(bz**2 + bx**2 + by**2)                 # B
          b1m= bz/bm
          b2m= bx/bm
          b3m= by/bm

          vpl= pv1v1(m)*b1m + pv2v1(m)*b2m + pv3v1(m)*b3m
          vpl2= vpl**2
          psmu(m)= 0.5*(v2 - vpl2)/bm                      # mu_0

c         if(1=1) goto 400        # for mu=mu0

          vd1= psmu(m)*v11m + vpl2*v12m
          vd2= psmu(m)*v21m + vpl2*v22m
          vd3= psmu(m)*v31m + vpl2*v32m

          vv2= (pv1v1(m)-vd1)**2 + (pv2v1(m)-vd2)**2
     &                           + (pv3v1(m)-vd3)**2       #(v-vd)^2
          psmu(m)= 0.5*(vv2 - vpl2)/bm                     # mu_1

          v1m= pv1v1(m)-vpl*b1m                     # v_perp
          v2m= pv2v1(m)-vpl*b2m
          v3m= pv3v1(m)-vpl*b3m

          rho1= (b2m*v3m-b3m*v2m)/bm                # Larmor radius
          rho2= (b3m*v1m-b1m*v3m)/bm
          rho3= (b1m*v2m-b2m*v1m)/bm

                                        # Check if gc is still inside.
          rm2= (pv2x1(m) - rho2 - x0)**2 + (pv3x1(m) - rho3 - y0)**2
          if( rm2 < rlo2 | rm2 > rhi2 ) then
            rho2= 0.
            rho3= 0.
            mout= mout+1
          endif

          ri= ( pv1x1(m) -rho1 -q(1) )*dqi + 1.     # X=x-rho -
          rj= ( pv2x1(m) -rho2 -x(1) )*dxi + 1.     ! guiding center
          rk= ( pv3x1(m) -rho3 -y(1) )*dyi + 1.

          TERP_SETUP(i)
          TERP_SETUP(j)
          TERP_SETUP(k)
          INTERP(bh1,vp1b)
          INTERP(bh2,vp2b)
          INTERP(bh3,vp3b)
          INTERP(bcbm,sptmp1)

          bm_gc= sqrt(bh1**2 + bh2**2 + bh3**2)              # B
          b1m= bh1/bm_gc
          b2m= bh2/bm_gc
          b3m= bh3/bm_gc
                                                             # mu_2
          vdotb= (v1m*b1m+v2m*b2m+v3m*b3m)
          rmu2= 0.5*vpl*vdotb/bm_gc
          psmu(m)= psmu(m) - rmu2
          psmu(m)= psmu(m)*( 1 - 0.5*vpl*bcbm )              # mu_3

 400      continue

        enddo m

      enddo isp
      MPI_print 'N-out=',mout

# Calculate weights.

      do m= 1,np

        v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2            # V^2
        vm= sqrt(v2)
        rlambda= 1.
        if( v2 > 0. ) rlambda= 2*psmu(m)*b0/v2
                         # Loading with P=F1*F3 marker distribution.
        psp(m)= f2(rlambda,vm)
c       psp(m)= 1.

      enddo m

      deallocate( sptmp1, sptmp3 )
      deallocate( vp1b,  vp2b,  vp3b )
      deallocate( vp1d1, vp2d1, vp3d1 )
      deallocate( vp1d2, vp2d2, vp3d2 )

      RETURN_END
c######################################################################
      subroutine p_weight                       # NSTX for LOAD=0
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate equilibrium weights (psp) for particles assuming
! F0=F0(epsilon,lambda,pphi) and loading markers with slowing-down df.
! Need psmu(m) here, pspphi(m) is calculated in initpart.

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD ; COMMON_PART
      COMMON_SPEC ; COMMON_PGRID 
      COMMON_FRC ; COMMON_DF

# Mu calculation stuff (E=0 assumed).
      DIM_s(stmp1, stmp3)
      DIM_v(d1,d2,tmp)
      real, allocatable, dimension(:,:,:) :: sptmp1,sptmp3
      real, allocatable, dimension(:,:,:) :: vp1b,vp2b,vp3b
      real, allocatable, dimension(:,:,:) :: vp1d1,vp2d1,vp3d1
      real, allocatable, dimension(:,:,:) :: vp1d2,vp2d2,vp3d2

      F0                # this is macro for F0=f1(v)*f2(lam)*f3(pphi).

      allocate( sptmp1(ni,nx,ny), sptmp3(ni,nx,ny) )
      allocate( vp1b(ni,nx,ny),  vp2b(ni,nx,ny),  vp3b(ni,nx,ny) )
      allocate( vp1d1(ni,nx,ny), vp2d1(ni,nx,ny), vp3d1(ni,nx,ny) )
      allocate( vp1d2(ni,nx,ny), vp2d2(ni,nx,ny), vp3d2(ni,nx,ny) )



# Calculate psmu(m) as in hybm_test.p.
! Calculate V_d= (mu/m)* b x grad(B)/B + U^2* curl(b)_perp/B, and
!  stmp1= b*curl(b)/B, stmp3=|B|.

      stmp3= sqrt( v1b1**2 + v2b1**2 + v3b1**2 )          # B

      DIVI_vvs( V(d1), V(b1), stmp3 )           # b=B/B
      call curl_vv( V(d2), V(d1), VBC_j)        # =curl(b)
      DOT_svv( stmp1, V(d1), V(d2) )
      MUL_vvs( V(d1), V(d1), stmp1 )
      SUB_vvv( V(d2), V(d2), V(d1) )            # =curl(b)_perp
      DIVI_vvs( V(d2), V(d2), stmp3 )

      DIVI_aaa( stmp1, stmp1, stmp3, nijk )     # =b*curl(b)/B

      call grad_vs( V(d1), stmp3, VBC_v )
      call cros_vvv( V(tmp), V(b1), V(d1) )
      DIVI_vvs( V(d1), V(tmp), stmp3 )
      DIVI_vvs( V(d1), V(d1), stmp3 )           # =Bxgrad(B)/(B^2)

# Map all to particle grid.

      call maptop_s( sptmp1, stmp1 )
      call maptop_v( vp1b, vp2b, vp3b, V(b1) )
      call maptop_v( vp1d1, vp2d1, vp3d1, V(d1) )
      call maptop_v( vp1d2, vp2d2, vp3d2, V(d2) )

      rlo2= rlo**2
      rhi2= rhi**2

# Loop over species:

      do isp= 1, nsp

# Loop over particles of a given species.


        do m= mfsp(isp), mlsp(isp)


# Interpolate fields to particle locations.
                                     # Interpolate to particle grid.
          ri= ( pv1x1(m) -q(1) )*dqi + 1.
          rj= ( pv2x1(m) -x(1) )*dxi + 1.
          rk= ( pv3x1(m) -y(1) )*dyi + 1.

          TERP_SETUP(i)
          TERP_SETUP(j)
          TERP_SETUP(k)

          INTERP(bz,vp1b)           # B
          INTERP(bx,vp2b)
          INTERP(by,vp3b)

          INTERP(v11m,vp1d1)        # drift velocities
          INTERP(v21m,vp2d1)
          INTERP(v31m,vp3d1)
          INTERP(v12m,vp1d2)
          INTERP(v22m,vp2d2)
          INTERP(v32m,vp3d2)

# Calculate mu + corrections.

          v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2     # V^2

          bm= sqrt(bz**2 + bx**2 + by**2)                 # B
          b1m= bz/bm
          b2m= bx/bm
          b3m= by/bm

          vpl= pv1v1(m)*b1m + pv2v1(m)*b2m + pv3v1(m)*b3m
          vpl2= vpl**2
          psmu(m)= 0.5*(v2 - vpl2)/bm                      # mu_0

c         if(1=1) goto 400        # for mu=mu0

          vd1= psmu(m)*v11m + vpl2*v12m
          vd2= psmu(m)*v21m + vpl2*v22m
          vd3= psmu(m)*v31m + vpl2*v32m

          vv2= (pv1v1(m)-vd1)**2 + (pv2v1(m)-vd2)**2
     &                           + (pv3v1(m)-vd3)**2       #(v-vd)^2
          psmu(m)= 0.5*(vv2 - vpl2)/bm                     # mu_1

          v1m= pv1v1(m)-vpl*b1m                     # v_perp
          v2m= pv2v1(m)-vpl*b2m
          v3m= pv3v1(m)-vpl*b3m

          rho1= (b2m*v3m-b3m*v2m)/bm                # Larmor radius
          rho2= (b3m*v1m-b1m*v3m)/bm
          rho3= (b1m*v2m-b2m*v1m)/bm

                                        # Check if gc is still inside.
          rm2= (pv2x1(m) - rho2 - x0)**2 + (pv3x1(m) - rho3 - y0)**2
          if( rm2 < rlo2 | rm2 > rhi2 ) then
            rho2= 0.
            rho3= 0.
          endif

          ri= ( pv1x1(m) -rho1 -q(1) )*dqi + 1.     # X=x-rho -
          rj= ( pv2x1(m) -rho2 -x(1) )*dxi + 1.     ! guiding center
          rk= ( pv3x1(m) -rho3 -y(1) )*dyi + 1.

          TERP_SETUP(i)
          TERP_SETUP(j)
          TERP_SETUP(k)
          INTERP(bh1,vp1b)
          INTERP(bh2,vp2b)
          INTERP(bh3,vp3b)
          INTERP(bcbm,sptmp1)

          bm_gc= sqrt(bh1**2 + bh2**2 + bh3**2)              # B
          b1m= bh1/bm_gc
          b2m= bh2/bm_gc
          b3m= bh3/bm_gc
                                                             # mu_2
          vdotb= (v1m*b1m+v2m*b2m+v3m*b3m)
          rmu2= 0.5*vpl*vdotb/bm_gc
          psmu(m)= psmu(m) - rmu2
          psmu(m)= psmu(m)*( 1 - 0.5*vpl*bcbm )              # mu_3

 400      continue

        enddo m

      enddo isp

# Calculate weights.

      do m= 1,np

        v2= pv1v1(m)**2 + pv2v1(m)**2 + pv3v1(m)**2            # V^2
        vm= sqrt(v2)
        rlambda= 1.
        if( v2 > 0. ) rlambda= 2*psmu(m)*b0/v2

                                                    # Uniform loading.
c       psp(m)=  f1(vm)*f2(rlambda,vm)*f3(pspphi(m))

                         # Loading with slowing down distr. in energy.
        psp(m)= ff1(vm)*f2(rlambda,vm)*f3(pspphi(m))
     &                 *(1./(r0*vm-psi0-pminus)**betav)
c       psp(m)= 1.

        psw(m)= 0.                         # no initial perturbation.

      enddo m

      deallocate( sptmp1, sptmp3 )
      deallocate( vp1b,  vp2b,  vp3b )
      deallocate( vp1d1, vp2d1, vp3d1 )
      deallocate( vp1d2, vp2d2, vp3d2 )

      RETURN_END
    ],)
c######################################################################
    ifelse(LPGRID,T,[
      subroutine inversed(rrd, zd, nrg, nzg, sden)            #0302
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Inverse 2D marker density sden (on cylindrical grid, phi independent).

      use MPI_module

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD
      DIM_s(sden)
      dimension d1d(nj), d2d(ni,nj), rrd(nrg), zd(nzg,nj)
      dimension dsum(nj)

# Calculate 1D particle density integral d1d(r).

      do j= 1,nj                         # integrate in z.
        dsum(j)= 0.
        do i= 1,ni
          dsum(j)= dsum(j) + wq(i)*sden(i,j,k3)*sh3(i,j,k3)
        enddo i
      enddo j

      j3=3
      if( ibstyp(1,2,SBC)=7 ) j3=1       # cyl. geometry with Ri=0.

      d1d(1:3)= 0.

      do jj= j3+1,njm2
        if( jj<j3+4 ) then

          d1d(jj)= 0.5*( dsum(j3) + dsum(jj) )
          do j= j3+1,jj-1
            d1d(jj)= d1d(jj) + dsum(j)
          enddo j

        else if( jj<j3+7 ) then

          d1d(jj)= 0.5*dsum(jj)
          do j= j3,jj-1
            d1d(jj)= d1d(jj) + wr(j)*dsum(j)
          enddo j

        else

          d1d(jj)= aw1*dsum(jj) + aw2*dsum(jj-1) + aw3*dsum(jj-2)
     &           + aw4*dsum(jj-3)
          do j= j3,jj-4
            d1d(jj)= d1d(jj) + wr(j)*dsum(j)
          enddo j

        endif
      enddo jj

      do j= j3,njm2
        d1d(j)= d1d(j)/d1d(njm2)
      enddo j
      d1d(njm)= 1.
      d1d(nj)= 1.

# Calculate range of j=[jmin,jmax] where particles will be loaded.

      eps= 0.1/nrg
      jmin= 1
      do j= 1,nj
        if( d1d(j)>eps ) goto 80
        jmin= j
      enddo j
 80   continue
      jmax= nj
      do j= 1,nj
        if( 1-d1d(nj+1-j)>eps ) goto 90
        jmax= nj+1-j
      enddo j
 90   continue
      MPI_print ''
      MPI_print 'jmin,jmax=',jmin,jmax,r(jmin),r(jmax)

# Calculate 2D particle density integral d2d(z,r).
! Assumed that qlo=q(2.5) and qhi=q(nim2+0.5) for periodic bc, and
!  qlo=q(3), qhi=q(nim2) for other bc.

      rtmp= 0.5
      if( ibstyp(1,1,SBC)=2 ) rtmp= 1.

      SET_ar( d2d, 0., ni*nj )

      if( ibstyp(1,1,SBC)=2 ) then
        d2d(3,1:nj)= 0.5*sden(3,1:nj,k3)
      endif

      do ii= 4,nim2

        if( ii<7) then

          do j= j3,njm2
            d2d(ii,j)= rtmp*sden(3,j,k3) + 0.5*sden(ii,j,k3)
            do i= 4,ii-1
              d2d(ii,j)= d2d(ii,j) + sden(i,j,k3)
            enddo i
          enddo j

        else if( ii<10 ) then

          do j= j3,njm2
            d2d(ii,j)= 0.5*sden(ii,j,k3)
            do i= 3,ii-1
              d2d(ii,j)= d2d(ii,j) + wq(i)*sden(i,j,k3)
            enddo i
          enddo j

        else

          do j= j3,njm2
            d2d(ii,j)= aw1*sden(ii,j,k3) + aw2*sden(ii-1,j,k3)
     &               + aw3*sden(ii-2,j,k3) + aw4*sden(ii-3,j,k3)
            do i= 3,ii-4
              d2d(ii,j)= d2d(ii,j) + wq(i)*sden(i,j,k3)
            enddo i
          enddo j

        endif

      enddo ii

      do j= j3,njm2
        sum= 0.
        do i= 1,ni
          sum= sum + wq(i)*sden(i,j,k3)
        enddo i
        do i= 3,nim2
          if( sum=0. ) then
            d2d(i,j)= 0.
          else
            d2d(i,j)= d2d(i,j)/sum
          endif
        enddo i
      enddo j

# Make d2d=0 for j<=jmin or j>=jmax.

      do i= 1,ni
        do j= 1,jmin
          d2d(i,j)= 0.
        enddo j
        do j= jmax,nj
          d2d(i,j)= 0.
        enddo j
      enddo i


# Calculate d2d(2,j) and d2d(nim,j) using d2d(2.5,j)=0 and
!  d2d(njm2+0.5,j)=1 (for periodic bc).

      if( ibstyp(1,1,SBC)=2 & ibstyp(2,1,SBC)=2 ) then
        do j= jmin+1,jmax-1
          d2d(2,j)= - d2d(3,j)
          d2d(nim,j)= 2. - d2d(nim2,j)
          d2d(ni, j)= 1.
        enddo j
      else
        do j= jmin+1,jmax-1
          d2d(nim,j)= 1.
          d2d(ni, j)= 1.
        enddo j
      endif

# Inverse d1d(r^2) to get r^2=rrd(j), assuming rmin=r(jmin) and
!  rmax=r(jmax).

      jjp= 2
      do j= 2,nrg-1
        d1dt= real(j-1)/(nrg-1)
        do jj= jjp,njm2
          if( d1d(jj)>d1dt ) then
            jjp= jj
            goto 100
          endif
        enddo jj
        print *, 'd1dt>d1d(j) in tine inversed'
 100    continue
        rrd(j)= ( (d1d(jjp)-d1dt)*r(jjp-1)**2
     &           + (d1dt-d1d(jjp-1))*r(jjp)**2 )/( d1d(jjp)-d1d(jjp-1) )
      enddo j

      rrd(1)= (r(jmin))**2
      rrd(nrg)= (r(jmax))**2

# Inverse d2d(z,r) to get z=zd(i=1:nzg,j=1:nj).
! For j such that sden=0 set zd to be same as at jmin or jmax.
! 02/2017 modified to make loading symmetric in z.

      do j= jmin+1,jmax-1

        iip= 3
        do i= 1,nzg
          d2dt= max( real(i-1)/(nzg-1), 3.0e-4 )
          d2dt= min( d2dt, 1-3.0e-4 )
          do ii= iip,nim
            if( d2d(ii,j)>d2dt ) then
              iip= ii
              goto 200
            endif
          enddo ii
          print *, 'd2dt>d2d(i,j) in tine inversed'
          stop 1122
 200      continue
          zd(i,j)= ( (d2d(iip,j)-d2dt)*q(iip-1)
     &       + (d2dt-d2d(iip-1,j))*q(iip) )/( d2d(iip,j)-d2d(iip-1,j) )
        enddo i

      enddo j

      do i= 1,nzg
        do j= 1,jmin
          zd(i,j)= zd(i,jmin+1)
        enddo j
        do j= jmax,nj
          zd(i,j)= zd(i,jmax-1)
        enddo j
      enddo i

c     jjc=nj/1.5
c     MPI_print ' '
c     MPI_print 'zd(1,j)=', zd(1,jjc),zd(2,jjc),zd(3,jjc)
c     MPI_print 'zd(nzg,j)=', zd(nzg,jjc),zd(nzg-1,jjc),zd(nzg-2,jjc)

      RETURN_END
c######################################################################
      subroutine inversed_o(rrd, zd, nrg, nzg)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Inverse particle density for Ri=0 and periodic bc in q-dir.

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD
      dimension d1d(nj), d2d(ni,nj), rrd(nrg), zd(nzg,nj)
      dimension wwr(nj), wwq(ni)

      ERROR_EXIT( ibstyp(1,1,SBC)<>2 | ibstyp(2,1,SBC)<>2, inversed )

# Calculate 1D particle density integral d1d(r).      

      do j= 1,nj
        wwr(j)= 1.
      enddo j

      do jj= 1,njm2

        if( jj<8 ) then
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
          wwr(jj)  = aw1
        endif
        if( jj=1 ) wwr(1)= 0.

        sumj= 0.
        do j= 1,jj
          sum= 0.
          do i= 1,ni
            sum= sum + wq(i)*srhoq0(i,j,k3)*v3h(i,j,k3)
          enddo i
          sumj= sumj + sum*wwr(j)
        enddo j
        d1d(jj)= sumj

        do j= 1,nj
          wwr(j)= 1.
        enddo j

      enddo jj
 
      do j= 1,njm2
        d1d(j)= d1d(j)/d1d(njm2)
      enddo j
      d1d(njm)= 1.
      d1d(nj)= 1.

# Calculate 2D particle density integral d2d(z,r).
! Assumed that qlo=q(2.5) and qhi=q(nim2+0.5).

      do i= 1,ni
        wwq(i)= 1.
      enddo i

      do ii= 3,nim2
        
        if( ii<7 ) then
          wwq(ii)= 0.5
        else
          wwq(ii-3)= aw4
          wwq(ii-2)= aw3
          wwq(ii-1)= aw2
          wwq(ii)  = aw1
        endif
        do j= 1,njm2
          d2d(ii,j)= 0.
          do i= 3,ii
            d2d(ii,j)= d2d(ii,j) + wwq(i)*srhoq0(i,j,k3)
          enddo i
        enddo j
        do i= 1,ni
          wwq(i)= 1.
        enddo i

      enddo ii

      do j= 1,njm2
        sum= 0.
        do i= 1,ni
          sum= sum + wq(i)*srhoq0(i,j,k3)
        enddo i
        do i= 3,nim2
          d2d(i,j)= d2d(i,j)/sum
        enddo i
      enddo j

      do i= 3,nim2
        d2d(i,njm)= 0.
        d2d(i,nj)= 0.
      enddo i

# Calculate d2d(2,j) and d2d(nim,j) using d2d(2.5,j)=0 and 
!  d2d(njm2+0.5,j)=1.

      do j= 1,nj
        d2d(2,j)= - d2d(3,j)
        d2d(nim,j)= 2. - d2d(nim2,j)
      enddo j

# Inverse d1d(r^2) to get r^2=rrd(j).

      jjp= 2
      do j= 2,nrg-1
        d1dt= real(j-1)/(nrg-1)
        do jj= jjp,njm2
          if( d1d(jj)>d1dt ) then
            jjp= jj
            goto 100
          endif
        enddo jj
        print *, 'd1dt>d1d(j) in tine inversed'
 100    continue
        rrd(j)= ( (d1d(jjp)-d1dt)*r(jjp-1)**2  
     &           + (d1dt-d1d(jjp-1))*r(jjp)**2 )/( d1d(jjp)-d1d(jjp-1) )
      enddo j

      rrd(1)= 0.
      rrd(nrg)= r(njm2)**2

# Inverse d2d(z,r) to get z=zd(i=1:nzg,j=1:nj).

      do j= 1,njm2
     
        iip= 3
        do i= 2,nzg-1
          d2dt= real(i-1)/(nzg-1)
          do ii= iip,nim
            if( d2d(ii,j)>d2dt ) then
              iip= ii
              goto 200
            endif
          enddo ii
          print *, 'd2dt>d2d(i,j) in tine inversed'
 200      continue
          zd(i,j)= ( (d2d(iip,j)-d2dt)*q(iip-1) 
     &       + (d2dt-d2d(iip-1,j))*q(iip) )/( d2d(iip,j)-d2d(iip-1,j) )
        enddo i
        zd(1,j)= qlo
        zd(nzg,j)= qhi

      enddo j
        

      RETURN_END
    ],)
c######################################################################
    ifelse(LPGRID,T,[
      subroutine nstx_load(sden, vd, nvg)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Calculate marker density and vd array which inverts |v| distribution.
! Markers are loaded with P=f1(v)*v*f3(pphi).

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD
      COMMON_FRC ; COMMON_DF

      DIM_s(sden)
      dimension den(ni,nj), v1d(nvg), vd(ni,nj,nvg)
      real,dimension(:),allocatable :: v,vint


      F0                                       # macro for f1,f2,f3
      pf(xa)= f1(xa)*xa*( step( xa-vpsi)*( xa-vpsi)**(betav+1)
     &                  - step(-xa-vpsi)*(-xa-vpsi)**(betav+1) )

# At each i,j calculate den(i,j) and vd(i,j,l).

      den= 0.
      v1d= 0.
      vd= 0.

      nv= 2*nvg
      allocate(v(nv),vint(nv))
      vmax= v0+2*deltav
      dv= vmax/(nv-1)
      v= (/ (dv*i, i= 0,nv-1) /)             # array of v=[0,vmax]
      vint= 0.

      v1d= (/ (real(i), i= 0,nvg-1) /)
      dvd= 1./(nvg-1)
      v1d= v1d*dvd                   # uniform probability distr.=[0,1]

      do i= 1,ni
        do j= 1,nj
          vpsi= (spsi(i,j,k3) + pminus)/r(j)
          if( srhoq0(i,j,k3)=0. | vmax<=vpsi ) goto 100
                                     # Calculate int from 0 to v(iv).
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
          do iv= 9,nv
            sumvt= aw4*pf(v(iv-3)) + aw3*pf(v(iv-2)) + aw2*pf(v(iv-1)) 
     &           + aw1*pf(v(iv))
            vint(iv)= vint(iv-1) - sumv + pf(v(iv-4)) + sumvt
            sumv= sumvt
          enddo iv
                                                # Marker density
          den(i,j)= vint(nv)*dv*r(j)**betav/(betav+1)
          vint0= vint(nv)
          if( vint0=0. ) goto 100 
          vint= vint/vint0
          
                                                # Invert vint -> vd
          vmin= max(0.,vpsi)
          vd(i,j,1)= vmin
          vd(i,j,nvg)= vmax

          ivp= 2
          do l= 2,nvg-1
            do iv= ivp,nv
              if( vint(iv) > v1d(l) ) then
                ivp= iv
                goto 200
              endif
            enddo iv
            print *,'v1d(l)>vint(iv) in tine nstx_load'
 200        continue
            vd(i,j,l)= ( v(ivp-1)*(vint(ivp)-v1d(l)) 
     &                 + v(ivp)*(v1d(l)-vint(ivp-1)) )
     &                                       /(vint(ivp)-vint(ivp-1))
          enddo l

 100      continue
        enddo j
      enddo i

      denmax= maxval(den)
      do k= 1,nk
        sden(:,:,k)= den/denmax
      enddo k

      deallocate(v,vint)

      RETURN_END
    ],)
c######################################################################
      subroutine savedump(isavdmp)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      use MPI_module

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD ; COMMON_OUT
      COMMON_PART ; COMMON_SPEC
      COMMON_FRC ; COMMON_DF
      character *20 csd

      if( isavdmp=0 ) then       # Writing initial startup file.
        csd = 'start.d'
      elseif( isavdmp=1 ) then   # Save to numbered file.

        isave= 0
        isave= isave + 1
c        call set_fname(idrun,isave,csd)
         stop 33

      elseif(isavdmp=2)         # isavdmp=2 : Dump writes over old file.

        idump= 0
        idump= idump + 1
c        call set_fname(idrun,-1,csd)
         stop 33

      else
        MPI_print 'Improper isavdmp value (',isavdmp,') in savedump'
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

      if(myid = master) then

        write(26) idrun,its,t,DIMEN,ni,nj,nk
     &     ,SBC,VBC_b,VBC_v,Hq_1,Hr_1,Hs_1
     &     ,nsp,np,beta,gamma,ialign,denki0
     &     ,pcut,rhoc,omegai0
     &    ,idump, isave, ienout, ifhout, isfldout, ivfldout, ibfldout
     &    ,itiout, i3dbout, iptrout
        write(26) q, r, s, V(h)
        write(26) sp1, V(b1)
        write(26) srhob1, V(mo1)
        write(26) V(a)
        write(26) rmask,sres,V(b1),V(ub),V(a)    # Save V(b0) and V(ub0).
        write(26) srhob1,sp1                   # Save srhob0, sp0.

        ifelse(LMHD,F,[
          write(26) v0,vstar,deltav,aav,bbv,r0,psi0     # Save df param.
     &             ,pminus,betav,b0,lambda0,deltal,rl0,rdl
     &             ,etaf,deltala,lambda0a
          do isp= 1,nsp
            write(26) cisp(isp),dennsp(isp),npsp(isp),qnsp(isp),
     &              tratsp(isp),vthplsp(isp),wnsp(isp),wdqsp(isp),
     &              wsp(isp),qsp(isp)
          enddo isp

          write(26) srhoq0, V(curi0), spi0

          write(26) (pv1v1(m), m= 1,np)
          write(26) (pv2v1(m), m= 1,np)
          write(26) (pv3v1(m), m= 1,np)
          write(26) (pv1x1(m), m= 1,np)
          write(26) (pv2x1(m), m= 1,np)
          ifelse(DIMEN,3,[
            write(26) (pv3x1(m), m= 1,np)
          ],)
          write(26)  (psw(m), m= 1,np)
          write(26)  (psp(m), m= 1,np)
        ],)
      endif

      close(26)

      RETURN_END
c######################################################################
      subroutine finalize
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD
      COMMON_PART

# Deallocate all fld-grid arrays.

      deallocate( sp, sp1, srhob1)
      deallocate( srhoq, srhoq0, spi0)
      deallocate( spsi, sphi, rmask, sres, sg)

      deallocate( V(b1), V(a), V(cur))
      deallocate( V(e), V(mo1), V(ub))
      deallocate( V(curi), V(curi0))
      deallocate( V(vhypa))

# Deallocate geometry arrays from COMMON_CMVEC.

      deallocate( V(h))
      deallocate( V(hi), V(h2), V(h2i))
      deallocate( V(h2d1))
      deallocate( sh3, sh3w, sh3i)
      deallocate( t12hder,t13hder,t21hder,t23hder,t31hder,t32hder)

# Deallocate particle arrays.

      ifelse(LMHD,F,[
        deallocate( PV(x1), PV(v1))
        deallocate( pspphi, psr, psmu, psw, psp)
      ],)

      RETURN_END
c######################################################################
      subroutine der_ss( so , si , ider )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# so= first derivative of si with respect to the ider-th coordinate.

      PARAM ; COMMON_VEC ; DIM_s( so, si, stmp )

      ERROR_EXIT( ider<1 | ider>3 , der_ss )

      if ( DIMEN=2 & ider=3 ) then
        SET_ar(so,0.,nijk)
        return
      endif

      SET_ar(stmp,0.,nijk)

        if( ider=1 ) then
          SDO_dat
            stmp(i,j,k)= d(-1,1)*si(i-1,j,k)
     &                  +d( 1,1)*si(i+1,j,k)
     &                  +d(-2,1)*si(i-2,j,k)
     &                  +d( 2,1)*si(i+2,j,k)
          SENDDO
        else if( ider=2 ) then
          SDO_dat
            stmp(i,j,k)= d(-1,2)*si(i,j-1,k)
     &                  +d( 1,2)*si(i,j+1,k)
     &                  +d(-2,2)*si(i,j-2,k)
     &                  +d( 2,2)*si(i,j+2,k)
          SENDDO
        else if( ider=3 ) then
          SDO_dat
            stmp(i,j,k)= d(-1,3)*si(i,j,k-1)
     &                  +d( 1,3)*si(i,j,k+1)
     &                  +d(-2,3)*si(i,j,k-2)
     &                  +d( 2,3)*si(i,j,k+2)
          SENDDO
        endif
        SET_aa(so,stmp,nijk)


      RETURN_END
c######################################################################
  ifelse(LPGRID,T,[
      subroutine maptop_v( vp1o, vp2o, vp3o, V(ia) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Map field-grid vector V(ia) into particle-grid vector VP(o).
# Dont forget to change the directions, ie (Vr,Vs) -> (Vx,Vy).

      PARAM ; COMMON_VEC ; COMMON_PGRID
      DIM_v(ia,tmp) ; DIM_vp(o)

# Calculate x and y-components of V(ia), then map to particle grid.

      SDO
        v2tmp(i,j,k)= v2ia(i,j,k)*coss(k) - v3ia(i,j,k)*sins(k)
        v3tmp(i,j,k)= v2ia(i,j,k)*sins(k) + v3ia(i,j,k)*coss(k)
      SENDDO

      if( ibstyp(1,2,SBC)=7 ) then       # cyl. geometry with Ri=0.
        call fixo_s( v2tmp, v2tmp, 2 )
        call fixo_s( v3tmp, v3tmp, 2 )
      endif

      call maptop_s( vp1o, v1ia )
      call maptop_s( vp2o, v2tmp )
      call maptop_s( vp3o, v3tmp )

      RETURN_END
c######################################################################
# Setup for quadratic splines interpolation (3 point) of fields.
! To be used in maptop and maptof (interpolates in 2D ie in j & k).

            define(TERP_SETUP2,[          # Here, $1 will be j, or k.
              $1[]$1= nint( r[]$1 )
              $1[]m= $1[]$1 - 1
              $1[]p= $1[]$1 + 1
              if( r[]$1 > $1[]$1 ) then
                f[]$1= r[]$1 - $1[]$1
                f[]$1[]m5= f[]$1 - .5
                f[]$1[]p5= f[]$1 + .5
                f[]$1[]m= f[]$1[]m5 * f[]$1[]m5
                f[]$1[]p= f[]$1[]p5 * f[]$1[]p5
              else
                f[]$1= $1[]$1 - r[]$1
                f[]$1[]m5= f[]$1 - .5
                f[]$1[]p5= f[]$1 + .5
                f[]$1[]m= f[]$1[]p5 * f[]$1[]p5
                f[]$1[]p= f[]$1[]m5 * f[]$1[]m5
              endif
              f[]$1[]$1= 1.5 - 2. * f[]$1 * f[]$1
            ])

            define(INTERP2,[
              $1 = .25 * ( fkm*( fjm*$2(i,jm,km)
     &                          +fjj*$2(i,jj,km)
     &                          +fjp*$2(i,jp,km) )
     &                   + fkk*( fjm*$2(i,jm,kk)
     &                          +fjj*$2(i,jj,kk)
     &                          +fjp*$2(i,jp,kk) )
     &                   + fkp*( fjm*$2(i,jm,kp)
     &                          +fjj*$2(i,jj,kp)
     &                          +fjp*$2(i,jp,kp) ) )   ])

c######################################################################
      subroutine maptop_s( spo, si )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Map field grid array  si into particle grid array  spo.

      use MPI_module
      PARAM ; COMMON_VEC; COMMON_PARAM ; COMMON_PGRID
      DIM_s(si) ; DIM_sp(spo)
      integer ibp1(nx,ny)

      SET_ar( spo, 0., nixy )
      ibp1= ibp

      do k= 1,ny
        do j= 1,nx

          rj= ( rxy(j,k) - r(1) )*dri + 1.
          rk= ( sxy(j,k) - s(1) )*dsi + 1.
          TERP_SETUP2(j)
          TERP_SETUP2(k)

          if( jj>1 & jj<nj ) then
            ibp1(j,k)= 1
            do i= 1,ni
              INTERP2(spo(i,j,k),si)
            enddo i
          endif

          if( jj=1 & ibstyp(1,2,SBC)=7 ) then     # cylinder with Ri=0.
            do kk= 3,nkm2
              do i= 1,ni
                spo(i,j,k)= spo(i,j,k)
     &              + 0.5*( fjj*si(i,1,kk) + (fjp+fjm)*si(i,2,kk) )/nkd
              enddo i
            enddo kk
          endif

        enddo j
      enddo k


# Fix values at close to the boundary ( rxy<Ri | rxy>Rc ) points:
!  extrapolate values from inside.

      do k= 2,ny-1
        do j= 2,nx-1

          if( ibp1(j,k)=1 ) goto 200
          isum= ibp1(j-1,k)+ibp1(j+1,k)+ibp1(j,k-1)+ibp1(j,k+1)
          if( isum=0 ) goto 200
          do i= 1,ni
            spo(i,j,k)= (  spo(i,j-1,k)*ibp1(j-1,k)
     &                   + spo(i,j+1,k)*ibp1(j+1,k)
     &                   + spo(i,j,k-1)*ibp1(j,k-1)
     &                   + spo(i,j,k+1)*ibp1(j,k+1) )/isum
          enddo i

 200      continue
        enddo j
      enddo k

      RETURN_END
c######################################################################
      subroutine maptof_v( V(o), vp1i, vp2i, vp3i )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Map particle-grid vector VP(i) into field-grid vector V(o).
# Dont forget to change the directions, ie (Vx,Vy) -> (Vr,Vs).

      PARAM ; COMMON_VEC ; COMMON_PGRID
      DIM_v(o,tmp) ; DIM_vp(i)

      call maptof_s( v1o, vp1i )              # Map q-component.
      call maptof_vxy( v2o, v3o, vp2i, vp3i ) # Map x & y components.

      RETURN_END
c######################################################################
      subroutine maptof_s( so, spa )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Map particle grid array  spa into field grid array  so.
! Done only for m=0 or m=1 harmonics.

      PARAM ; COMMON_VEC; COMMON_PARAM ; COMMON_PGRID
      DIM_s(so) ; DIM_sp(spa)
      parameter( nkk=100 )
      dimension tmp(ni,nkk), cost(nkk), sint(nkk)

      ifelse(MODE,N,[
        print *, 'cannot use MODE=N in maptof_s'
        stop 4321
      ],)

      do k= 1,nkk
        cost(k)= cos( rpi2*(k-1)/nkk )
        sint(k)= sin( rpi2*(k-1)/nkk )
      enddo k

      SET_ar( so, 0., nijk )
      SET_ar( tmp, 0., ni*nkk )

      j3=3
      if( ibstyp(1,2,SBC)=7 ) j3=1        # cylinder with Ri=0

      do j= j3,njm2

        do k= 1,nkk
          xx= r(j)*cost(k) + 0.5*(xlo+xhi)
          yy= r(j)*sint(k) + 0.5*(ylo+yhi)
          rj= ( xx - x(1) )*dxi + 1.
          rk= ( yy - y(1) )*dyi + 1.

          TERP_SETUP2(j)
          TERP_SETUP2(k)
          do i= 1,ni
            INTERP2(tmp(i,k),spa)
          enddo i
        enddo k
      
        ifelse(MODE,0,[
          do i= 1,ni
            rsum= 0.
            do k= 1,nkk
              rsum= rsum + tmp(i,k)
            enddo k
            do k= 3,nkm2
              so(i,j,k)= rsum/nkk
            enddo k
          enddo i
        ],[
          do i= 1,ni
            rsumc= 0.
            rsums= 0.
            do k= 1,nkk
              rsumc= rsumc + tmp(i,k)*cost(k)
              rsums= rsums + tmp(i,k)*sint(k)
            enddo k
            do k= 3,nkm2
              so(i,j,k)= 2.*( rsumc*coss(k) + rsums*sins(k) )/nkk
            enddo k
          enddo i
        ])

      enddo j

      RETURN_END
c######################################################################
      subroutine maptof_vxy( v2o, v3o, vp2i, vp3i )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Map particle grid Vx and Vy arrays into field grid arrays Vr and Vs.
! Done only for m=0 or m=1 harmonics.

      PARAM ; COMMON_VEC; COMMON_PARAM ; COMMON_PGRID
      DIM_s(v2o,v3o) ; DIM_sp(vp2i) ; DIM_sp(vp3i)
      parameter( nkk=100 )
      dimension tmpx(ni,nkk), tmpy(ni,nkk), cost(nkk), sint(nkk)
      dimension tmpr(ni,nkk), tmps(ni,nkk)

      ifelse(MODE,N,[
        print *, 'cannot use MODE=N in maptof_vxy'
        stop 4321
      ],)

      do k= 1,nkk
        cost(k)= cos( rpi2*(k-1)/nkk )
        sint(k)= sin( rpi2*(k-1)/nkk )
      enddo k

      SET_ar( v2o, 0., nijk )
      SET_ar( v3o, 0., nijk )
      SET_ar( tmpr, 0., ni*nkk )
      SET_ar( tmps, 0., ni*nkk )

      j3=3
      if( ibstyp(1,2,SBC)=7 ) j3=1        # cylinder with Ri=0

      do j= j3,njm2

        do k= 1,nkk
          xx= r(j)*cost(k) + 0.5*(xlo+xhi)
          yy= r(j)*sint(k) + 0.5*(ylo+yhi)
          rj= ( xx - x(1) )*dxi + 1.
          rk= ( yy - y(1) )*dyi + 1.

          TERP_SETUP2(j)
          TERP_SETUP2(k)
          do i= 1,ni
            INTERP2(tmpx(i,k),vp2i)
            INTERP2(tmpy(i,k),vp3i)
            tmpr(i,k)=  tmpx(i,k)*cost(k) + tmpy(i,k)*sint(k)
            tmps(i,k)= -tmpx(i,k)*sint(k) + tmpy(i,k)*cost(k)
          enddo i
        enddo k
      
        ifelse(MODE,0,[
          do i= 1,ni
            sumr= 0.
            sums= 0.
            do k= 1,nkk
              sumr= sumr + tmpr(i,k)
              sums= sums + tmps(i,k)
            enddo k
            do k= 3,nkm2
              v2o(i,j,k)= sumr/nkk
              v3o(i,j,k)= sums/nkk
            enddo k
          enddo i
        ],[
          do i= 1,ni
            sumrc= 0.
            sumrs= 0.
            sumsc= 0.
            sumss= 0.
            do k= 1,nkk
              sumrc= sumrc + tmpr(i,k)*cost(k)
              sumrs= sumrs + tmpr(i,k)*sint(k)
              sumsc= sumsc + tmps(i,k)*cost(k)
              sumss= sumss + tmps(i,k)*sint(k)
            enddo k
            do k= 3,nkm2
              v2o(i,j,k)= 2.*( sumrc*coss(k) + sumrs*sins(k) )/nkk
              v3o(i,j,k)= 2.*( sumsc*coss(k) + sumss*sins(k) )/nkk
            enddo k
          enddo i
        ])

      enddo j

      RETURN_END
c######################################################################
      subroutine frompart
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Get quantities which depend on the particles, that is, the
!  ion charge and current densities.
# In this sub. density and current are accumulated on particle grid.

      PARAM ; COMMON_VEC ; COMMON_PARAM ; COMMON_FLD
      COMMON_PART ; COMMON_SPEC ; COMMON_PGRID

      DIM_s(stmp) ; DIM_v(tmp)
      real, allocatable, dimension(:,:,:) :: sprho,sprhot
      real, allocatable, dimension(:,:,:) :: vp1cur, vp2cur, vp3cur
      real, allocatable, dimension(:,:,:) :: vp1curt,vp2curt,vp3curt


      allocate( sprho(ni,nx,ny), sprhot(ni,nx,ny) )
      allocate( vp1cur(ni,nx,ny),  vp2cur(ni,nx,ny),  vp3cur(ni,nx,ny) )
      allocate( vp1curt(ni,nx,ny), vp2curt(ni,nx,ny), vp3curt(ni,nx,ny) )

# Set total ion charge and current density arrays to zero.

      SET_ar( sprho, 0., nixy ) 
      SET_ar( vp1cur, 0., nixy )
      SET_ar( vp2cur, 0., nixy )
      SET_ar( vp3cur, 0., nixy )

#  First, loop over species:

      do isp= 1, nsp

# Set temporary arrays to zero.
      SET_ar( sprhot,  0., nixy )
      SET_ar( vp1curt, 0., nixy )
      SET_ar( vp2curt, 0., nixy )
      SET_ar( vp3curt, 0., nixy )

# Accumulate densities.

# Loop over particles.

        do m= mfsp(isp),mlsp(isp)


# Collect terms needed for accumulation step.
!  First, get weights for accumulation step.
!   Note: TERP_SETUP was defined in sub. initpart.

          ri= ( pv1x1(m) -q(1) )*dqi + 1.
          rj= ( pv2x1(m) -x(1) )*dxi + 1.
          ifelse(DIMEN,3,[rk= ( pv3x1(m) -y(1) )*dyi + 1.],)

          TERP_SETUP(i)
          TERP_SETUP(j)
          ifelse(DIMEN,3,[TERP_SETUP(k)], )

# This section defines linear spline accumulation.
          define(ACCUM_k,[
            $1(ii,jj,$3)= $1(ii,jj,$3) + $4 fjj * fii * $2
            $1(ip,jj,$3)= $1(ip,jj,$3) + $4 fjj * fip * $2
            $1(ii,jp,$3)= $1(ii,jp,$3) + $4 fjp * fii * $2
            $1(ip,jp,$3)= $1(ip,jp,$3) + $4 fjp * fip * $2
          ])
          define (ACCUM,[
            ifelse(DIMEN,3,[
              ACCUM_k($1,$2,kk,fkk*)
              ACCUM_k($1,$2,kp,fkp*)
            ],[
              ACCUM_k($1,$2,1,)
            ])
          ])

# Calculate zero order densities.

          wqsp= psp(m)*qsp(isp)
          wv1c= wqsp*pv1v1(m)
          wv2c= wqsp*pv2v1(m)
          wv3c= wqsp*pv3v1(m)

          ACCUM(sprhot,  wqsp)
          ACCUM(vp1curt, wv1c)
          ACCUM(vp2curt, wv2c)
          ACCUM(vp3curt, wv3c)

        enddo m

# Now sum up species density arrays into total density arrays.
        SDOP
           sprho(i,j,k)=  sprho(i,j,k) +  sprhot(i,j,k)
          vp1cur(i,j,k)= vp1cur(i,j,k) + vp1curt(i,j,k)
          vp2cur(i,j,k)= vp2cur(i,j,k) + vp2curt(i,j,k)
          vp3cur(i,j,k)= vp3cur(i,j,k) + vp3curt(i,j,k)
        SENDDO

      enddo isp


# Fix up the effect of density accumulation overflow into the buffer
!  zone. Need different sub for LPGRID=T.

c     call dfix_sp( sprho,  SBC )
c     call dfix_vp( vp1cur, vp2cur, vp3cur, VBC_v )
c
c     if( ismoo>0 ) then
c       call smoo_sp( sprho, sprho, SBC )
c       call smoo_vp(vp1cur,vp2cur,vp3cur, vp1cur,vp2cur,vp3cur,VBC_v)
c       call dfix_sp( sprho,  SBC )
c       call dfix_vp( vp1cur, vp2cur, vp3cur, VBC_v )
c     endif


# Map particle grid arrays back to field grid arrays, ie to srhoq
!  and V(curi).

      call maptof_s( srhoq, sprho )
      call maptof_v( V(curi), vp1cur, vp2cur, vp3cur )


# Fix bc of field arrays.

      call fix_s( srhoq, srhoq, SBC )
      call fix_v( V(curi), V(curi), VBC_v )

c     if( ismoo>0 ) then
c       call smoo_s( srhoq, srhoq, SBC )
c       call smoo_v( V(curi), V(curi), VBC_v )
c     endif

# 9/09 Modify fast ion parallel current with (1-Zb/Zeff) coefficient.
! 3/11 not needed for equilibrium calculations.

c     DOT_svv( stmp, V(b1), V(b1) )        # B^2
c     stmp= sqrt( stmp )                   # |B|
c     DIVI_vvs( V(tmp), V(b1), stmp )      # b^
c     DOT_svv( stmp, V(curi), V(tmp) )     # J_pll
c     MUL_aar( stmp, stmp, zbdzeff )
c     MUL_vvs( V(tmp), V(tmp), stmp )      # (zb/zeff)*J_pll*b^
c     SUB_vvv( V(curi), V(curi), V(tmp) )  # J_i - (zb/zeff)*J_pll*b^

# Check density.

      SDO_dat
        if( srhoq(i,j,k)<0. ) goto 3333
      SENDDO
      goto 3334
3333  print *, '3333; srhoq<0; i,j,k,srhoq= '
      print *, '             ',i,j,k,srhoq(i,j,k)
3334   continue

      deallocate( sprho, sprhot )
      deallocate( vp1cur,  vp2cur,  vp3cur )
      deallocate( vp1curt, vp2curt, vp3curt )

      RETURN_END
  ],)
c######################################################################
      subroutine cros_vvv( V(o), V(ia), V(ib) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(ia) x V(ib)   -- vector cross product.
      PARAM ; COMMON_VEC ; DIM_s(stmp) ; DIM_v(o,tmp,ia,ib)

      MUL_aaa( v1tmp, v2ia, v3ib, nijk )
      MUL_aaa(  stmp, v3ia, v2ib, nijk )
      SUB_aaa( v1tmp, v1tmp, stmp,  nijk )

      MUL_aaa( v2tmp, v3ia, v1ib, nijk )
      MUL_aaa(  stmp, v1ia, v3ib, nijk )
      SUB_aaa( v2tmp, v2tmp, stmp,  nijk )

      MUL_aaa( v3tmp, v1ia, v2ib, nijk )
      MUL_aaa(  stmp, v2ia, v1ib, nijk )
      SUB_aaa( v3tmp, v3tmp, stmp,  nijk )

      SET_vv( V(o), V(tmp) )

      RETURN_END
c######################################################################
      subroutine curl_vv( V(o), V(i), ivtyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= curl( V(i) ).

      PARAM ; COMMON_VEC ; DIM_s(stmp) ; DIM_v(o,tmp,i)

      MUL_aaa( v1tmp, v1i, v1h, nijk )
      MUL_aaa( v2tmp, v2i, v2h, nijk )
      MUL_aaa( v3tmp, v3i, v3h, nijk )

      call der_ss( v1o, v3tmp, 2 )
      call der_ss(  stmp , v2tmp, 3 )
      SUB_aaa( v1o, v1o, stmp,  nijk )
      MUL_aaa( v1o, v1o, v1h2i, nijk )

      call der_ss( v2o, v1tmp, 3 )
      call der_ss(  stmp , v3tmp, 1 )
      SUB_aaa( v2o, v2o, stmp,  nijk )
      MUL_aaa( v2o, v2o, v2h2i, nijk )

      call der_ss( v3o, v2tmp, 1 )
      call der_ss(  stmp , v1tmp, 2 )
      SUB_aaa( v3o, v3o, stmp,  nijk )
      MUL_aaa( v3o, v3o, v3h2i, nijk )

      call fix_v( V(o), V(o), ivtyp )

      RETURN_END
c######################################################################
      subroutine grad_vs( V(o), si, ivtyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= grad( si ).

      PARAM ; COMMON_VEC ; DIM_s(si) ; DIM_v(o)

      call der_ss( v1o, si, 1)
      MUL_aaa( v1o, v1o, v1hi, nijk )

      call der_ss( v2o, si, 2)
      MUL_aaa( v2o, v2o, v2hi, nijk )

      call der_ss( v3o, si, 3)
      MUL_aaa( v3o, v3o, v3hi, nijk )

      call fix_v( V(o), V(o), ivtyp )

      RETURN_END
c######################################################################
      subroutine div_sv( so, V(i), istyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# so= div( V(i) ).

      PARAM ; COMMON_VEC ; DIM_s(so, stmp) ; DIM_v(i)

      MUL_aaa( so, v1i, v1h2, nijk )
      call der_ss( so , so, 1 )

      MUL_aaa( stmp, v2i, v2h2, nijk )
      call der_ss( stmp , stmp, 2 )

      ADD_aaa( so, so, stmp, nijk )

      MUL_aaa( stmp, v3i, v3h2, nijk )
      call der_ss( stmp , stmp, 3 )

      ADD_aaa( so, so, stmp, nijk )

      MUL_aaa( so, so, sh3i, nijk )

      call fix_s( so, so, istyp )

      RETURN_END
c######################################################################
      subroutine div_svs( so, V(i), si, istyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# so= div( V(i)*si ).

      PARAM ; COMMON_VEC ; DIM_s(so) ; DIM_v(tmp,i)

      MUL_vvs( V(tmp), V(i), si )

      call div_sv( so, V(tmp), istyp )

      RETURN_END
c######################################################################
      subroutine div_vvv( V(o), V(ia), V(ib), ivtyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= div( V(ia) V(ib) ).

      PARAM ; COMMON_VEC
      DIM_s(stmp) ; DIM_v(o,tmpa,tmpb,ia,ib)

# Get first term:

      MUL_vvv( V(tmpa), V(ia), V(h2) )
      define(TERM_1,[
        MUL_aaa( stmp, v[]$1[]ib, v1tmpa, nijk )
        call der_ss( v[]$1[]tmpb , stmp, 1 )

        MUL_aaa( stmp, v[]$1[]ib, v2tmpa, nijk )
        call der_ss( stmp , stmp, 2 )
        ADD_aaa( v[]$1[]tmpb, v[]$1[]tmpb, stmp, nijk)

        MUL_aaa( stmp, v[]$1[]ib, v3tmpa, nijk )
        call der_ss( stmp , stmp, 3 )
        ADD_aaa( v[]$1[]tmpb, v[]$1[]tmpb, stmp, nijk)

        MUL_aaa( v[]$1[]tmpb, v[]$1[]tmpb, sh3i, nijk )
                    ])
      TERM_1(1)
      TERM_1(2)
      TERM_1(3)

# Get second and third term (this term = 0 for straight coordinates):

      MUL_vvv( V(tmpa), V(ia), V(ib) )
      ifelse(CMPLR,f90,[
        define(TERM_23,[
            v[]$1[]tmpb=  v[]$1[]tmpb   +v[]$1[]ia*
     &        (  v[]$2[]ib*t[]$1[]$2[]hder
     &          +v[]$3[]ib*t[]$1[]$3[]hder )
     &       -v[]$2[]tmpa*t[]$2[]$1[]hder
     &       -v[]$3[]tmpa*t[]$3[]$1[]hder
        ])
        TERM_23(1,2,3)
        TERM_23(2,1,3)
        TERM_23(3,1,2)
      ],[
        SDO_dat
          define(TERM_23,[
           v[]$1[]tmpb(i,j,k)=  v[]$1[]tmpb(i,j,k)   +v[]$1[]ia(i,j,k)*
     &        (  v[]$2[]ib(i,j,k)*t[]$1[]$2[]hder(i,j,k)
     &          +v[]$3[]ib(i,j,k)*t[]$1[]$3[]hder(i,j,k) )
     &       -v[]$2[]tmpa(i,j,k)*t[]$2[]$1[]hder(i,j,k)
     &       -v[]$3[]tmpa(i,j,k)*t[]$3[]$1[]hder(i,j,k)
          ])
          TERM_23(1,2,3)
          TERM_23(2,1,3)
          TERM_23(3,1,2)
        SENDDO
      ])

      call fix_v( V(tmpb), V(tmpb), ivtyp )

      SET_vv( V(o), V(tmpb) )

      RETURN_END
c######################################################################
      subroutine lap_ss( so, si, istyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# so= Laplacian( si ).

      PARAM ; COMMON_VEC ; DIM_s(so, stmp1, stmp2, si)

      call der_ss( stmp1 , si , 1 )
      MUL_aaa( stmp1, stmp1, v1h2d1, nijk )
      call der_ss( stmp1 , stmp1 , 1 )

      call der_ss( stmp2 , si , 2 )
      MUL_aaa( stmp2, stmp2, v2h2d1, nijk )
      call der_ss( stmp2 , stmp2 , 2 )
      ADD_aaa( stmp1, stmp1, stmp2, nijk )

      call der_ss( stmp2 , si , 3 )
      MUL_aaa( stmp2, stmp2, v3h2d1, nijk )
      call der_ss( stmp2 , stmp2 , 3 )
      ADD_aaa( stmp1, stmp1, stmp2, nijk )

      MUL_aaa( stmp1, stmp1, sh3i, nijk )

      SET_aa( so, stmp1, nijk )

      call fix_s( so, so, istyp )

      RETURN_END
c######################################################################
      subroutine add_vvvr( V(o), V(ia), V(ib), rval )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(ia) + V(ib)*rval.   This is not a macro, ADD_vvvr, because
!  it has 10 arguments and the dollar substitutions only go to 9.

      PARAM ; DIM_v(o,ia,ib)

      SDO
        v1o(i,j,k)= v1ia(i,j,k) + v1ib(i,j,k)*rval
        v2o(i,j,k)= v2ia(i,j,k) + v2ib(i,j,k)*rval
        v3o(i,j,k)= v3ia(i,j,k) + v3ib(i,j,k)*rval
      SENDDO

      RETURN_END
c######################################################################
      subroutine mul_vvvr( V(o), V(ia), V(ib), rval )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(ia)*V(ib)*rval.   This is not a macro, MUL_vvvr, because
!  it has 10 arguments and the dollar substitutions only go to 9.

      PARAM ; DIM_v(o,ia,ib)

      SDO
        v1o(i,j,k)= v1ia(i,j,k) * v1ib(i,j,k) * rval
        v2o(i,j,k)= v2ia(i,j,k) * v2ib(i,j,k) * rval
        v3o(i,j,k)= v3ia(i,j,k) * v3ib(i,j,k) * rval
      SENDDO

      RETURN_END
c######################################################################
      subroutine swi_ss( s1, s2 )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Switch two scalar quantities, that is, s1=s2 and s2=s1.

      PARAM ; COMMON_VEC ; DIM_s(s1,s2)

      ifelse(CMPLR,f90,[
        DIM_s(stmp)
        stmp= s2
        s2=   s1
        s1=   stmp
      ],[
        SDO
          stmp= s2(i,j,k)
          s2(i,j,k)= s1(i,j,k)
          s1(i,j,k)= stmp
        SENDDO
      ])

      RETURN_END
c######################################################################
      subroutine swi_vv( V(ia), V(ib) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Switch two vector quantities, that is, V(ia)=V(ib) and V(ib)=V(ia).

      PARAM ; DIM_v(ia,ib)

      call swi_ss( v1ia, v1ib )
      call swi_ss( v2ia, v2ib )
      call swi_ss( v3ia, v3ib )

      RETURN_END
c######################################################################
      subroutine swi_aa( a1, a2, nel )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Switch two arrays, that is, a1=a2 and a2=a1.

      dimension a1(nel), a2(nel)

      ifelse(CMPLR,f90,[
# *** Caution: This dimension may not work on the cm2. ***
        dimension atmp(nel)
        atmp= a2
        a2=   a1
        a1=   atmp
      ],[
        do k=1,nel
          atmp= a2(k)
          a2(k)= a1(k)
          a1(k)= atmp
        enddo k
      ])

      RETURN_END
c######################################################################
      subroutine swi_pvpv( PV(ia), PV(ib), nel )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Switch two particle vector quantities, that is, V(ia)=V(ib) 
!  and V(ib)=V(ia).  *** Caution: Dont use for particle position. ***

      PARAM ; COMMON_PARAM ; DIM_pv(ia,ib)

      call swi_aa( pv1ia, pv1ib, nel )
      call swi_aa( pv2ia, pv2ib, nel )
      call swi_aa( pv3ia, pv3ib, nel )

      RETURN_END
c######################################################################
      subroutine fix_v( V(o), V(i), ivtyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Fix boundaries of a vector quantity.  V(o) will normally be the same
!  as V(i).

      PARAM ; COMMON_VEC ; DIM_v(o,i)

      call fix_s( v1o, v1i, isvtyp(1,ivtyp) )
      call fix_s( v2o, v2i, isvtyp(2,ivtyp) )
      call fix_s( v3o, v3i, isvtyp(3,ivtyp) )

      RETURN_END
c######################################################################
      subroutine fix_s( so, si, istyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Fix boundaries of a scalar quantity.  so will normally be the same
!  as si.

      PARAM ; COMMON_VEC ; DIM_s(so,si)

      ERROR_EXIT( istyp<1 | istyp>nstyp, fix_s )

      SET_aa( so, si, nijk )

      if( istyp=0 ) return

      do idim= 1,DIMEN

        select( ibstyp(1,idim,istyp) )     # ilh=1 for low end of dim.
          case 1:                          # Dont do anything.

          case 2:                          # Periodic bc.

            ifelse(NJD,1,[
              if( idim=2 ) then
                call sfx_ss( so, si, idim, 2, 3, 1)
                call sfx_ss( so, si, idim, 1, 3, 1)
              else
                call sfx_ss( so, si, idim, 2, nam2(idim), 1)
                call sfx_ss( so, si, idim, 1, nam3(idim), 1)
              endif
            ],[
              call sfx_ss( so, si, idim, 2, nam2(idim), 1)
              call sfx_ss( so, si, idim, 1, nam3(idim), 1)
            ])

          case 3:                          # Positive reflection.

            call sfx_ss( so, si, idim, 2, 4, 1)
            call sfx_ss( so, si, idim, 1, 5, 1)

          case 4:                          # Negative reflection.

            call sfx_ss( so, si, idim, 2, 4, -1)
            call sfx_ss( so, si, idim, 1, 5, -1)
c           if( ismoo>0 ) then
c             call sfx_sr( so, idim, 3, 0. )
c           endif

          case 5:                          # Constant value.

            call sfx_sr( so, idim, 2, a1styp(1,idim,istyp) )
            call sfx_sr( so, idim, 1, a2styp(1,idim,istyp) )

          case 6:                          # Use extrapolation with
                                           !  so(3)=0. & d(so)dn=0.
          ifelse(SORDER,4,[
              call sfx_sr( so, idim, 3, 0. )
              call extrap_slp( so, idim, 3, 1)
            ],[
              call sfx_sr( so, idim, 3, 0. )
              call sfx_ss( so, si, idim, 2, 4, 1)
              call sfx_ss( so, si, idim, 1, 5, 1)
            ])

                                           # Cylindrical geometry.
          case 7:                          ! r=0, scalar bc.
            call fixo_s( so, si, idim )

          case 8:                          ! r=0, vector bc.
            call fixo_v( so, si, idim )
 

          case 9:                          # Use extrapolation to 
                                            ! calculate values at 1 & 2.
            call extrap_s( so, idim, 2, 1, 1)

          case 10:                          ! same with so(3)=0.
            call sfx_sr( so, idim, 3, 0. )
            ifelse(SORDER,4,[
              call extrap_s( so, idim, 2, 1, 1)
            ],[
              call sfx_ss( so, si, idim, 2, 4, -1)
              call sfx_ss( so, si, idim, 1, 5, -1)
            ])
 
          case 11:                          ! same with d(so)dn=0.
            ifelse(SORDER,4,[
              call extrap_slp( so, idim, 3, 1)
            ],[
              call sfx_ss( so, si, idim, 2, 4, 1)
              call sfx_ss( so, si, idim, 1, 5, 1)
            ])

          case 12:                          ! same with d(r*so)dn=0.
            ifelse(SORDER,4,[
              call extrap_rslp( so, idim, 3, 1)
            ],[
              print *, '#12 in fix_s not done yet'
              stop
            ])

          case 13:                          ! 4th order extrapolation.
            call extrap4_s( so, idim, 2, 1)
            call extrap4_s( so, idim, 1, 1)

        endselect



        select( ibstyp(2,idim,istyp) )    # ilh=2 for high end of dim.
          case 1:
            break
          case 2:
            ifelse(NJD,1,[
              if( idim=2 ) then
                call sfx_ss( so, si, idim, 4, 3, 1)
                call sfx_ss( so, si, idim, 5, 3, 1)
              else
                call sfx_ss( so, si, idim, nam(idim), 3, 1)
                call sfx_ss( so, si, idim, na(idim),  4, 1)
              endif
            ],[
              call sfx_ss( so, si, idim, nam(idim), 3, 1)
              call sfx_ss( so, si, idim, na(idim),  4, 1)
            ])
          case 3:
            call sfx_ss( so, si, idim, nam(idim), nam3(idim), 1)
            call sfx_ss( so, si, idim, na(idim),  nam4(idim), 1)
          case 4:
            call sfx_ss( so, si, idim, nam(idim), nam3(idim), -1)
            call sfx_ss( so, si, idim, na(idim),  nam4(idim), -1)
c           if( ismoo>0 ) then
c             call sfx_sr( so, idim, nam2(idim), 0. )
c           endif
          case 5:
            call sfx_sr( so, idim, nam(idim), a1styp(1,idim,istyp) )
            call sfx_sr( so, idim, na(idim),  a2styp(1,idim,istyp) )
          case 6:
            ifelse(SORDER,4,[
              call sfx_sr( so, idim, nam2(idim), 0. )
              call extrap_slp( so, idim, nam2(idim), -1)
            ],[
              call sfx_sr( so, idim, nam2(idim), 0. )
              call sfx_ss( so, si, idim, nam(idim), nam3(idim), 1)
              call sfx_ss( so, si, idim, na(idim), nam4(idim), 1)
            ])
          case 7:                     
            print *, 'this option is not defined for high end of dim.'
            stop 772
          case 8:
            print *, 'this option is not defined for high end of dim.'
            stop 882
          case 9:
            call extrap_s( so, idim, nam(idim), na(idim), -1)
          case 10:
            call sfx_sr( so, idim, nam2(idim), 0. )
            ifelse(SORDER,4,[
              call extrap_s( so, idim, nam(idim), na(idim), -1)
            ],[
              call sfx_ss( so, si, idim, nam(idim), nam3(idim), -1)
              call sfx_ss( so, si, idim, na(idim), nam4(idim), -1)
            ])
          case 11:
            ifelse(SORDER,4,[
              call extrap_slp( so, idim, nam2(idim), -1)
            ],[
              call sfx_ss( so, si, idim, nam(idim), nam3(idim), 1)
              call sfx_ss( so, si, idim, na(idim), nam4(idim), 1)
            ])
          case 12:
            ifelse(SORDER,4,[
              call extrap_rslp( so, idim, nam2(idim), -1)
            ],[
              print *, '#12 in fix_s not done yet'
              stop
            ])
          case 13:
            call extrap4_s( so, idim, nam(idim), -1)
            call extrap4_s( so, idim, na(idim), -1)

        endselect
      enddo idim

      RETURN_END
c######################################################################
      subroutine sfx_ss( so, si, idim, io, ii, ipm )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Sets the scalar value associated with index io of dimension idim
!  to the scalar value associated with index ii.  If ipm=-1, the
!  negative of the scalar value associated with index ii is used.
!  so will normally be the same as si.

      PARAM ; COMMON_VEC ; DIM_s(so,si)

      ERROR_EXIT( idim<1 | idim>DIMEN , sfx_ss )

      ifelse(CMPLR,f90,[
        select( idim )
        case 1:
          so(io,3:njm2,k3:nkm2)= ipm*si(ii,3:njm2,k3:nkm2)
        case 2:
          so(1:ni,io,k3:nkm2)= ipm*si(1:ni,ii,k3:nkm2)
        ifelse(DIMEN,3,[
          case 3:
            so(1:ni,1:nj,io)= ipm*si(1:ni,1:nj,ii)
        ],)
        endselect
      ],[
        select( idim )
        case 1:
          if( ipm=-1 ) then
            do k= k3,nkm2
              do j= 3,njm2
                so(io,j,k)= -si(ii,j,k)
              enddo j
            enddo k
          else
            do k= k3,nkm2
              do j= 3,njm2
                so(io,j,k)=  si(ii,j,k)
              enddo j
            enddo k
          endif
        case 2:
          if( ipm=-1 ) then
            do k= k3,nkm2
              do i= 1,ni
                so(i,io,k)= -si(i,ii,k)
              enddo i
            enddo k
          else
            do k= k3,nkm2
              do i= 1,ni
                so(i,io,k)=  si(i,ii,k)
              enddo i
            enddo k
          endif
        ifelse(DIMEN,3,[
          case 3:
            if( ipm=-1 ) then
              do j= 1,nj
                do i= 1,ni
                  so(i,j,io)= -si(i,j,ii)
                enddo i
              enddo j
            else
              do j= 1,nj
                do i= 1,ni
                  so(i,j,io)=  si(i,j,ii)
                enddo i
              enddo j
            endif
        ],)
        endselect
      ])

      RETURN_END
c######################################################################
      subroutine sfx_sr( sa, idim, io, rval )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Sets the scalar value associated with index io of dimension idim
!  to value rval.

      PARAM ; COMMON_VEC ; DIM_s(sa)

      ERROR_EXIT( idim<1 | idim>DIMEN , sfx_sr )

      ifelse(CMPLR,f90,[
        select( idim )
        case 1:
          sa(io,3:njm2,k3:nkm2)= rval
        case 2:
          sa(1:ni,io,k3:nkm2)= rval
        ifelse(DIMEN,3,[
          case 3:
            sa(1:ni,1:nj,io)= rval
        ],)
        endselect
      ],[
        select( idim )
        case 1:
          do k= k3,nkm2
            do j= 3,njm2
              sa(io,j,k)=  rval
            enddo j
          enddo k
        case 2:
          do k= k3,nkm2
            do i= 1,ni
              sa(i,io,k)=  rval
            enddo i
          enddo k
        ifelse(DIMEN,3,[
          case 3:
            do j= 1,nj
              do i= 1,ni
                sa(i,j,io)=  rval
              enddo i
            enddo j
        ],)
        endselect
      ])

      RETURN_END
c######################################################################
      subroutine sfx_slp( sa, ilh, idim, istyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Fixes the boundary at one end of a dimension using the assumption
!  that the slope at the boundary is a constant.  ilh indicates the
!  low or high end of the dimension.

      PARAM ; COMMON_VEC ; DIM_s(sa)
      parameter (odsev=1./7.)

      ERROR_EXIT( idim<1 | idim>DIMEN | ilh<1 | ilh>2 , sfx_sr )

      if( ilh=1 ) then
        io1= 2
        io2= 1
        ii1= 3
        ii2= 4
        slpdx=  a1styp(ilh,idim,istyp)
        slpdx6= slpdx*6.

      else
        io1= nam(idim)
        io2= na(idim)
        ii1= nam2(idim)
        ii2= nam3(idim)
        slpdx= -a1styp(ilh,idim,istyp)
        slpdx6= slpdx*6.
      endif

      ifelse(CMPLR,f90,[
          select( idim )
          case 1:
            sa(io1,3:njm2,k3:nkm2)= ( 8.*sa(ii1,3:njm2,k3:nkm2)
     &             -sa(ii2,3:njm2,k3:nkm2) -slpdx6 )*odsev
            sa(io2,3:njm2,k3:nkm2)= sa(io1,3:njm2,k3:nkm2) -slpdx
          case 2:
            sa(1:ni,io1,k3:nkm2)= ( 8.*sa(1:ni,ii1,k3:nkm2)
     &             -sa(1:ni,ii2,k3:nkm2) -slpdx6 )*odsev
            sa(1:ni,io2,k3:nkm2)= sa(1:ni,io1,k3:nkm2) -slpdx
          ifelse(DIMEN,3,[
            case 3:
              sa(1:ni,1:nj,io1)= ( 8.*sa(1:ni,1:nj,ii1)
     &               -sa(1:ni,1:nj,ii2) -slpdx6 )*odsev
              sa(1:ni,1:nj,io2)= sa(1:ni,1:nj,io1) -slpdx
          ],)
          endselect
      ],[
        select( idim )
        case 1:
          do k= k3,nkm2
            do j= 3,njm2
              sa(io1,j,k)= (  8.*sa(ii1,j,k)
     &                        -sa(ii2,j,k) -slpdx6  )*odsev
              sa(io2,j,k)= sa(io1,j,k) -slpdx
            enddo j
          enddo k
        case 2:
          do k= k3,nkm2
            do i= 1,ni
              sa(i,io1,k)= (  8.*sa(i,ii1,k)
     &                        -sa(i,ii2,k) -slpdx6  )*odsev
              sa(i,io2,k)= sa(i,io1,k) -slpdx
            enddo i
          enddo k
        ifelse(DIMEN,3,[
          case 3:
            do j= 1,nj
              do i= 1,ni
                sa(i,j,io1)= (  8.*sa(i,j,ii1)
     &                          -sa(i,j,ii2) -slpdx6  )*odsev
                sa(i,j,io2)= sa(i,j,io1) -slpdx
              enddo i
            enddo j
        ],)
        endselect
      ])

      RETURN_END
c######################################################################
      subroutine fixo_s( so, si, idim )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Fix value at the origin (cylindrical geometry) with scalar type bc
! using 4-point (3d order accurate) extrapolation for j=2 value and
! 3-point (2d order accurate) extrapolation for j=1 value, when not =0.
# For 2d order scheme, SORDER=2, value at j=1 is not used, and value at
! j=2 (ie r=0.) is calculated with 1st order accuracy.

      PARAM ; COMMON_VEC ; DIM_s(so,si)
      dimension sum1(ni), sum2(ni), suma(ni), sumb(ni)

      ifelse(LCYL,T,,[
        print *, 'wrong bc in fixo_s' 
        stop 771
      ])
      ERROR_EXIT( idim<>2, fixo_s )
      ERROR_EXIT( rlo>0., fixo_s )

      do i= 1,ni
        sum1(i)= 0.
        sum2(i)= 0.
        suma(i)= 0.
        sumb(i)= 0.
      enddo i

      ifelse(SORDER,4,,[                # for 2d order scheme use
                                        ! linear extrap. for m=0 mode.
        do k= k3,nkm2
          do i= 1,ni
            sum2(i)= sum2(i) + ( 2.*si(i,3,k) - si(i,4,k) )/nkd
          enddo i
        enddo k
        do k= k3,nkm2
          do i= 1,ni
            so(i,2,k)= sum2(i)
            so(i,1,k)= sum2(i)
          enddo i
        enddo k
       
        return

      ])


      ifelse(DIMEN,3,[                  # 4th order scheme.

        do k= k3,nkm2  
          do i= 1,ni
            sum1(i)= sum1(i) + 12./13.*( 3*si(i,3,k)-8./3.*si(i,4,k)
     &                                         +3./4.*si(i,5,k) )/nkd
            suma(i)= suma(i) + (3*si(i,3,k)-2*si(i,4,k)+0.5*si(i,5,k))
     &                                                    *coss(k)/nkd
            sumb(i)= sumb(i) + (3*si(i,3,k)-2*si(i,4,k)+0.5*si(i,5,k))
     &                                                    *sins(k)/nkd
          enddo i
        enddo k
        do k= k3,nkm2 
          do i= 1,ni
            sum2(i)= sum2(i) + ( 0.25*sum1(i)+1.5*si(i,3,k)-si(i,4,k)
     &                                           +0.25*si(i,5,k) )/nkd
          enddo i
        enddo k
        do k= k3,nkm2
          do i= 1,ni
            so(i,1,k)= sum1(i)
            so(i,2,k)= sum2(i) + suma(i)*coss(k) + sumb(i)*sins(k)
          enddo i
        enddo k

      ],[
        do i= 1,ni

          so(i,1,1)= 12./13.*( 3*si(i,3,1)-8./3.*si(i,4,1)
     &                                    +3./4.*si(i,5,1) )
          so(i,2,1)= 0.25*so(i,1,1)+1.5*si(i,3,1)-si(i,4,1)
     &                                           +0.25*si(i,5,1)

        enddo i
      ])

      RETURN_END
c######################################################################
      subroutine fixo_v( so, si, idim )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Fix value at the origin (cylindrical geometry) with vector type bc
! using 4-point (3d order accurate) extrapolation for j=2 value and
! 3-point (2d order accurate) extrapolation for j=1 value, when not =0.
# For 2d order scheme, SORDER=2, value at j=1 is not used, and value at
! j=2 (ie r=0.) is calculated with 1st order accuracy.

      PARAM ; COMMON_VEC ; DIM_s(so,si)
      dimension sum2(ni),suma1(ni),sumb1(ni)
      dimension suma2(ni),sumb2(ni),sumaa2(ni),sumbb2(ni)

      ifelse(LCYL,T,,[
        print *, 'wrong bc in fixo_v'
        stop 881
      ])
      ERROR_EXIT( idim<>2, fixo_v )
      ERROR_EXIT( rlo>0., fixo_v )

      do i=1,ni
        sum2(i)=  0.
        suma1(i)= 0.
        sumb1(i)= 0.
        suma2(i)= 0.
        sumb2(i)= 0.
        sumaa2(i)= 0.
        sumbb2(i)= 0.
      enddo i
 
      ifelse(SORDER,4,,[               # for 2d order scheme use
                                       ! linear extrap. for |m|=1 mode.
        ifelse(DIMEN,3,[
          do k= k3,nkm2
            do i= 1,ni
              suma2(i)= suma2(i) + (2*si(i,3,k)-si(i,4,k))*coss(k)/nkd
              sumb2(i)= sumb2(i) + (2*si(i,3,k)-si(i,4,k))*sins(k)/nkd
            enddo i
          enddo k
          do k= k3,nkm2
            do i= 1,ni
              so(i,1,k)= 0.
              so(i,2,k)= 2*suma2(i)*coss(k) + 2*sumb2(i)*sins(k)
            enddo i
          enddo k
        ],[
          do i= 1,ni
            so(i,1,1)= 0.
            so(i,2,1)= 0.
          enddo i
        ])
      
        return

      ])


      ifelse(DIMEN,3,[                 # 4th order scheme.

        do k= k3,nkm2 
          do i= 1,ni
                                                     # 0th harmonic
            sum2(i)= sum2(i) + ( 1.5*si(i,3,k) - si(i,4,k)
     &                                         + si(i,5,k)/4 )/nkd

                                                     # 1st harmonic
            suma1(i)= suma1(i) + 12/13.*( 3*si(i,3,k)-8/3.*si(i,4,k)
     &                                  + 3/4.*si(i,5,k) )*coss(k)/nkd
            sumb1(i)= sumb1(i) + 12/13.*( 3*si(i,3,k)-8/3.*si(i,4,k)
     &                                  + 3/4.*si(i,5,k) )*sins(k)/nkd

                                                     # 2d harmonic
            sumaa2(i)= sumaa2(i) + ( 1.5*si(i,3,k) - si(i,4,k)
     &                                    + si(i,5,k)/4 )*cos2s(k)/nkd
            sumbb2(i)= sumbb2(i) + ( 1.5*si(i,3,k) - si(i,4,k)
     &                                    + si(i,5,k)/4 )*sin2s(k)/nkd
          enddo i
        enddo k
        do k= k3,nkm2
          do i= 1,ni

            suma2(i)= suma2(i) + ( 0.25*suma1(i) 
     &             + (1.5*si(i,3,k)-si(i,4,k)+0.25*si(i,5,k))*coss(k) 
     &                                                           )/nkd
            sumb2(i)= sumb2(i) + ( 0.25*sumb1(i) 
     &             + (1.5*si(i,3,k)-si(i,4,k)+0.25*si(i,5,k))*sins(k)
     &                                                           )/nkd

          enddo i
        enddo k
        do k= k3,nkm2
          do i= 1,ni

            so(i,1,k)= 2*suma1(i)*coss(k) + 2*sumb1(i)*sins(k)
            so(i,2,k)=  sum2(i) 
     &                 + 2*suma2(i)*coss(k) + 2*sumb2(i)*sins(k)
     &                 + 2*sumaa2(i)*cos2s(k) + 2*sumbb2(i)*sin2s(k)

          enddo i
        enddo k

      ],[
        do i= 1,ni
          so(i,1,1)= 0.
          so(i,2,1)= 1.5*si(i,3,1) - si(i,4,1) + si(i,5,1)/4
        enddo
      ])

      RETURN_END
c######################################################################
      subroutine extrap_s( sa, idim, i2, i1, ipm )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Extrapolate value of sa with index i2 and i1 of dimension idim 
! using 3d (for i2) and 2d (for i1) order extrapolation;
! ipm=1 for forward extrapolation, ipm=-1 for backward extrapolation.

      PARAM ; COMMON_VEC ; DIM_s(sa)
      dimension f2(3),f3(4)

      ERROR_EXIT( idim<1 | idim>DIMEN , extrap_s )
      ERROR_EXIT( na(idim) < 6, extrap_s )

      f2(1)=  6.
      f2(2)= -8.
      f2(3)=  3.

      ifelse(SORDER,4,[
        f3(1)=  4.
        f3(2)= -6.
        f3(3)=  4.
        f3(4)= -1.
      ],[
        f3(1)=  2.
        f3(2)= -1.
        f3(3)=  0.
        f3(4)=  0.       
      ])
      
      select( idim )

      case 1:
        do k= k3,nkm2
          do j= 3,njm2
            sa(i1,j,k)=  f2(1)*sa(i1+ipm*2,j,k)
     &                  +f2(2)*sa(i1+ipm*3,j,k)
     &                  +f2(3)*sa(i1+ipm*4,j,k)

            sa(i2,j,k)=  f3(1)*sa(i2+ipm,  j,k)
     &                  +f3(2)*sa(i2+ipm*2,j,k) 
     &                  +f3(3)*sa(i2+ipm*3,j,k)
     &                  +f3(4)*sa(i2+ipm*4,j,k)
          enddo j
        enddo k

      case 2:
        do k= k3,nkm2
          do i= 1,ni
            sa(i,i1,k)=  f2(1)*sa(i,i1+ipm*2,k)
     &                  +f2(2)*sa(i,i1+ipm*3,k)
     &                  +f2(3)*sa(i,i1+ipm*4,k)

            sa(i,i2,k)=  f3(1)*sa(i,i2+ipm,  k)
     &                  +f3(2)*sa(i,i2+ipm*2,k) 
     &                  +f3(3)*sa(i,i2+ipm*3,k)
     &                  +f3(4)*sa(i,i2+ipm*4,k)
          enddo i
        enddo k

      ifelse(DIMEN,3,[
        case 3:
          do j= 1,nj
            do i= 1,ni
              sa(i,j,i1)=  f2(1)*sa(i,j,i1+ipm*2)
     &                    +f2(2)*sa(i,j,i1+ipm*3)
     &                    +f2(3)*sa(i,j,i1+ipm*4)

              sa(i,j,i2)=  f3(1)*sa(i,j,i2+ipm  )
     &                    +f3(2)*sa(i,j,i2+ipm*2)
     &                    +f3(3)*sa(i,j,i2+ipm*3)
     &                    +f3(4)*sa(i,j,i2+ipm*4)

            enddo i
          enddo j
      ],)
      endselect


      RETURN_END
c######################################################################
      subroutine extrap4_s( sa, idim, ii, ipm )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Extrapolate value of sa with index ii of dimension idim using 4th
! (or 2d) order extrapolation; ipm=1 for forward extrapolation,
! ipm=-1 for backward extrapolation.

      PARAM ; COMMON_VEC ; DIM_s(sa)
      dimension fa(5)

      ERROR_EXIT( idim<1 | idim>DIMEN , extrap_s )
      ERROR_EXIT( na(idim) < 7, extrap_s )

      ifelse(SORDER,4,[
        fa(1)=  5.
        fa(2)= -10.
        fa(3)=  10.
        fa(4)= -5.
        fa(5)=  1.
      ],)
      
      select( idim )

      case 1:
        do k= k3,nkm2
          do j= 3,njm2
            sa(ii,j,k)=  fa(1)*sa(ii+ipm,  j,k)
     &                  +fa(2)*sa(ii+ipm*2,j,k) 
     &                  +fa(3)*sa(ii+ipm*3,j,k)
     &                  +fa(4)*sa(ii+ipm*4,j,k)
     &                  +fa(5)*sa(ii+ipm*5,j,k)
          enddo j
        enddo k

      case 2:
        do k= k3,nkm2
          do i= 1,ni
            sa(i,ii,k)=  fa(1)*sa(i,ii+ipm,  k)
     &                  +fa(2)*sa(i,ii+ipm*2,k)
     &                  +fa(3)*sa(i,ii+ipm*3,k)
     &                  +fa(4)*sa(i,ii+ipm*4,k)
     &                  +fa(5)*sa(i,ii+ipm*5,k)
          enddo i
        enddo k

      ifelse(DIMEN,3,[
        case 3:
          do j= 1,nj
            do i= 1,ni
              sa(i,j,ii)=  fa(1)*sa(i,j,ii+ipm  )
     &                    +fa(2)*sa(i,j,ii+ipm*2)
     &                    +fa(3)*sa(i,j,ii+ipm*3)
     &                    +fa(4)*sa(i,j,ii+ipm*4)
     &                    +fa(5)*sa(i,j,ii+ipm*5)
            enddo i
          enddo j
      ],)
      endselect


      RETURN_END
c######################################################################
      subroutine extrap_slp( sa, idim, ii, ipm )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Extrapolate values of sa with index (ii-ipm) and (ii-ipm*2) of 
! dimension idim using 3d order extrapolation with additional
! condition of zero slope at boundary point ii=3 (for ipm=1) and 
! ii=nam2(idim) (for ipm=-1).

      PARAM ; COMMON_VEC ; DIM_s(sa)
      dimension f3(4)

      ERROR_EXIT( idim<1 | idim>DIMEN , extrap_slp )
      ERROR_EXIT( na(idim) < 6, extrap_slp )

      f3(1)=  4.
      f3(2)= -6.
      f3(3)=  4.
      f3(4)= -1.
      
      r1= d(-2*ipm,idim)
      r2= d(-ipm,  idim)
      r3= d( 0,    idim)
      r4= d( ipm,  idim)
      r5= d( 2*ipm,idim)
      
      select( idim )

      case 1:
        do k= k3,nkm2
          do j= 3,njm2
            sa(ii-ipm,j,k)= - (  (r3+r1*f3(2))*sa(ii, j, k)
     &                          +(r4+r1*f3(3))*sa(ii+ipm,  j,k)
     &                          +(r5+r1*f3(4))*sa(ii+ipm*2,j,k) )
     &                         / (r2+r1*f3(1))

            sa(ii-ipm*2,j,k)=  f3(1)*sa(ii-ipm,j,k) 
     &                       + f3(2)*sa(ii    ,j,k)
     &                       + f3(3)*sa(ii+ipm,j,k)
     &                       + f3(4)*sa(ii+ipm*2,j,k)
          enddo j
        enddo k

      case 2:
        do k= k3,nkm2
          do i= 1,ni
            sa(i,ii-ipm,k)= - (  (r3+r1*f3(2))*sa(i,ii,k) 
     &                          +(r4+r1*f3(3))*sa(i,ii+ipm,  k)
     &                          +(r5+r1*f3(4))*sa(i,ii+ipm*2,k) )
     &                         / (r2+r1*f3(1))

            sa(i,ii-ipm*2,k)=  f3(1)*sa(i,ii-ipm,k) 
     &                       + f3(2)*sa(i,ii    ,k)
     &                       + f3(3)*sa(i,ii+ipm,k)
     &                       + f3(4)*sa(i,ii+ipm*2,k)
          enddo i
        enddo k

      ifelse(DIMEN,3,[
        case 3:
          do j= 1,nj
            do i= 1,ni
              sa(i,j,ii-ipm)= - (  (r3+r1*f3(2))*sa(i,j,ii)
     &                            +(r4+r1*f3(3))*sa(i,j,ii+ipm)
     &                            +(r5+r1*f3(4))*sa(i,j,ii+ipm*2) )
     &                           / (r2+r1*f3(1))

              sa(i,j,ii-ipm*2)=  f3(1)*sa(i,j,ii-ipm) 
     &                         + f3(2)*sa(i,j,ii    )
     &                         + f3(3)*sa(i,j,ii+ipm)
     &                         + f3(4)*sa(i,j,ii+ipm*2)
            enddo i
          enddo j
      ],)
      endselect

      RETURN_END

c######################################################################
      subroutine extrap_rslp( sa, idim, ii, ipm )       # for B
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Extrapolate values of sa with index (ii-ipm) and (ii-ipm*2) of
! dimension idim using 3d order extrapolation with additional
! condition of d(r*sa)/dn=0 at boundary point ii=3 (for ipm=1) and
! ii=nam2(idim) (for ipm=-1).

      PARAM ; COMMON_VEC ; DIM_s(sa)
      dimension f3(4)

      ERROR_EXIT( idim<1 | idim>DIMEN , extrap_rslp )
      ERROR_EXIT( na(idim) < 6, extrap_rslp )

      f3(1)=  4.
      f3(2)= -6.
      f3(3)=  4.
      f3(4)= -1.

      r1= d(-2*ipm,idim)
      r2= d(-ipm,  idim)
      r3= d( 0,    idim)
      r4= d( ipm,  idim)
      r5= d( 2*ipm,idim)

      select( idim )

      case 1:                              # same as extrap_slp
        do k= k3,nkm2
          do j= 3,njm2
            sa(ii-ipm,j,k)= - (  (r3+r1*f3(2))*sa(ii, j, k)
     &                          +(r4+r1*f3(3))*sa(ii+ipm,  j,k)
     &                          +(r5+r1*f3(4))*sa(ii+ipm*2,j,k) )
     &                         / (r2+r1*f3(1))

            sa(ii-ipm*2,j,k)=  f3(1)*sa(ii-ipm,j,k)
     &                       + f3(2)*sa(ii    ,j,k)
     &                       + f3(3)*sa(ii+ipm,j,k)
     &                       + f3(4)*sa(ii+ipm*2,j,k)
          enddo j
        enddo k

      case 2:                             # sa -> r*sa -> sa/r
        ERROR_EXIT( r(ii-ipm*2)=0., extrap_rslp )
        do k= k3,nkm2
          do i= 1,ni
            sa(i,ii-ipm,k)= -( (r3+r1*f3(2))*sa(i,ii,k)*r(ii)
     &                  +(r4+r1*f3(3))*sa(i,ii+ipm,  k)*r(ii+ipm)
     &                  +(r5+r1*f3(4))*sa(i,ii+ipm*2,k)*r(ii+ipm*2) )
     &                                / ( (r2+r1*f3(1))*r(ii-ipm) )

            sa(i,ii-ipm*2,k)= ( f3(1)*sa(i,ii-ipm,k)*r(ii-ipm)
     &                        + f3(2)*sa(i,ii    ,k)*r(ii)
     &                        + f3(3)*sa(i,ii+ipm,k)*r(ii+ipm)
     &                        + f3(4)*sa(i,ii+ipm*2,k)*r(ii+ipm*2) )
     &                                                /r(ii-ipm*2)
          enddo i
        enddo k

      ifelse(DIMEN,3,[
        case 3:                           # same as extrap_slp
          do j= 1,nj
            do i= 1,ni
              sa(i,j,ii-ipm)= - (  (r3+r1*f3(2))*sa(i,j,ii)
     &                            +(r4+r1*f3(3))*sa(i,j,ii+ipm)
     &                            +(r5+r1*f3(4))*sa(i,j,ii+ipm*2) )
     &                           / (r2+r1*f3(1))

              sa(i,j,ii-ipm*2)=  f3(1)*sa(i,j,ii-ipm)
     &                         + f3(2)*sa(i,j,ii    )
     &                         + f3(3)*sa(i,j,ii+ipm)
     &                         + f3(4)*sa(i,j,ii+ipm*2)
            enddo i
          enddo j
      ],)
      endselect

      RETURN_END
c######################################################################
      subroutine dfix_s( sa, istyp )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Fix up the effect of density accumulation overflow into the buffer
!  zone.

      PARAM ; COMMON_VEC ; DIM_s(sa)

      ERROR_EXIT( istyp<1 | istyp>nstyp, dfix_s )

      do idim= 1,DIMEN

        select( ibstyp(1,idim,istyp) )     # ilh=1 for low end of dim.
          case 1:                          # Dont do anything.

          case 2:                          # Periodic bc.

            call dfx_s( sa, idim, 3, nam(idim), 1)

          case 3:                          # Positive reflection.

            call dfx_s( sa, idim, 4, 2,  1)
            call dfx_s( sa, idim, 3, 3,  1)

          case 4:                          # Negative reflection.

            call dfx_s( sa, idim, 4, 2, -1)
            call sfx_sr( sa, idim, 3, 0. )

          case 5:                          # Constant value.

            print *, 'istyp=5 not supported in dfix_s'
            stop 8171

          case 6:                          # Constant slope

            print *, 'istyp=6 not supported in dfix_s'
            stop 8172

          default:

            print *, 'istyp>6 not supported in dfix_s'
            stop 8172

        endselect

        select( ibstyp(2,idim,istyp) )     # ilh=2 for high end of dim.
          case 1:
          case 2:
            call dfx_s( sa, idim, nam2(idim), 2, 1)
          case 3:
            call dfx_s( sa, idim, nam3(idim), nam(idim),  1)
            call dfx_s( sa, idim, nam2(idim), nam2(idim),  1)
          case 4:
            call dfx_s( sa, idim, nam3(idim), nam(idim), -1)
            call sfx_sr( sa, idim, nam2(idim), 0. )
          case 5:
            print *, 'istyp=5 not supported in dfix_s'
            stop 8173
          case 6:
            print *, 'istyp=6 not supported in dfix_s'
            stop 8174
          default:
            print *, 'istyp>6 not supported in dfix_s'
            stop 8174
        endselect
      enddo idim

      RETURN_END
c######################################################################
      subroutine dfx_s( sa, idim, io, ii, ipm )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Adds the scalar value associated with index ii of dimension idim
!  to the scalar value associated with index io.  If ipm=-1, the
!  negative of the scalar value associated with index ii is used.
# Unlike sfx_s, this routine has some of the indices varying over
!  their whole length, because densities can be accumulated in the
!  corner buffer regions (i AND j in the buffer zone).

      PARAM ; COMMON_VEC ; DIM_s(sa)

      ERROR_EXIT( idim<1 | idim>DIMEN , dfx_s )

      ifelse(CMPLR,f90,[
        select( idim )
        case 1:
          if( ipm=-1 ) then
            sa(io,:,:)=  sa(io,:,:) -sa(ii,:,:)
          else
            sa(io,:,:)=  sa(io,:,:) +sa(ii,:,:)
          endif
        case 2:
          if( ipm=-1 ) then
            sa(3:nim2,io,:)=  sa(3:nim2,io,:) -sa(3:nim2,ii,:)
          else
            sa(3:nim2,io,:)=  sa(3:nim2,io,:) +sa(3:nim2,ii,:)
          endif
        ifelse(DIMEN,3,[
          case 3:
            if( ipm=-1 ) then
              sa(3:nim2,3:njm2,io)=  sa(3:nim2,3:njm2,io)
     &                              -sa(3:nim2,3:njm2,ii)
            else
              sa(3:nim2,3:njm2,io)=  sa(3:nim2,3:njm2,io)
     &                              +sa(3:nim2,3:njm2,ii)
            endif
        ],)
        endselect
      ],[
        select( idim )
        case 1:
          if( ipm=-1 ) then
            do k= 1,nk
              do j= 1,nj
                sa(io,j,k)=  sa(io,j,k) -sa(ii,j,k)
              enddo j
            enddo k
          else
            do k= 1,nk
              do j= 1,nj
                sa(io,j,k)=  sa(io,j,k) +sa(ii,j,k)
              enddo j
            enddo k
          endif
        case 2:
          if( ipm=-1 ) then
            do k= 1,nk
              do i= 3,nim2
                sa(i,io,k)= sa(i,io,k) -sa(i,ii,k)
              enddo i
            enddo k
          else
            do k= 1,nk
              do i= 3,nim2
                sa(i,io,k)= sa(i,io,k) +sa(i,ii,k)
              enddo i
            enddo k
          endif
        ifelse(DIMEN,3,[
          case 3:
            if( ipm=-1 ) then
              do j= 3,njm2
                do i= 3,nim2
                  sa(i,j,io)= sa(i,j,io) -sa(i,j,ii)
                enddo i
              enddo j
            else
              do j= 3,njm2
                do i= 3,nim2
                  sa(i,j,io)= sa(i,j,io) +sa(i,j,ii)
                enddo i
              enddo j
            endif
        ],)
        endselect
      ])

      RETURN_END

c######################################################################
      subroutine maxabs_rs(rmax,s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# rmax= maximum of scalar array abs(s)

      PARAM ; DIM_s(s)

        rmax= 0.
        SDO_dat
          if( abs(s(i,j,k))>rmax ) rmax= abs(s(i,j,k))
        SENDDO

      RETURN_END
c######################################################################
      subroutine maxabs1_rs(rmax,s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# rmax= maximum of scalar array abs(s) on smaller region for curl(curl(A))

      PARAM ; DIM_s(s)

        rmax= 0.
        k= k3
        do i= 7,ni-5
          do j= 7,nj-5
            if( abs(s(i,j,k))>rmax ) rmax= abs(s(i,j,k))
          enddo j
        enddo i
      RETURN_END
c######################################################################
      subroutine sum_rs(rsum,s)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# rsum = sum of scalar array s

      PARAM ; COMMON_PARAM ; DIM_s(s)

        rsum= 0.
        SDO_dat
          rsum= rsum + s(i,j,k)
        SENDDO

      ifelse(LCYL,T,[
        if( r1in=0. ) then
          do k= k3,nkm2
            do i= 3,nim2
              rsum= rsum +  s(i,2,k)
            enddo i
          enddo k
        endif
      ],)

      RETURN_END

##############    f77   Routines:  ###################################
##############    f77   Routines:  ###################################
##############    f77   Routines:  ###################################

      ifelse(CMPLR,f77,[

c######################################################################
      subroutine set_vr( V(o), rval )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= r.

      PARAM ; COMMON_VEC ; DIM_v(o)

      SET_ar( v1o, rval, nijk )
      SET_ar( v2o, rval, nijk )
      SET_ar( v3o, rval, nijk )

      RETURN_END
c######################################################################
      subroutine set_vv( V(o), V(i) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(i).

      PARAM ; COMMON_VEC ; DIM_v(o,i)

      SET_aa( v1o, v1i, nijk )
      SET_aa( v2o, v2i, nijk )
      SET_aa( v3o, v3i, nijk )

      RETURN_END
c######################################################################
      subroutine set_pvpv( PV(o), PV(i) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# PV(o)= PV(i).  *** CAUTION: Dont use for position vector. ***

      PARAM ; COMMON_PARAM ; DIM_pv(o,i)

      SET_aa( pv1o, pv1i, np )
      SET_aa( pv2o, pv2i, np )
      SET_aa( pv3o, pv3i, np )

      RETURN_END
c######################################################################
      subroutine inv_vv( V(o), V(i) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= 1./V(i).

      PARAM ; COMMON_VEC ; DIM_v(o,i)

      INV_aa( v1o, v1i, nijk )
      INV_aa( v2o, v2i, nijk )
      INV_aa( v3o, v3i, nijk )

      RETURN_END
c######################################################################
      subroutine neg_vv( V(o), V(i) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= -V(i).

      PARAM ; COMMON_VEC ; DIM_v(o,i)

      NEG_aa( v1o, v1i, nijk )
      NEG_aa( v2o, v2i, nijk )
      NEG_aa( v3o, v3i, nijk )

      RETURN_END
c######################################################################
      subroutine add_vvv( V(o), V(ia), V(ib) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(ia) + V(ib).

      PARAM ; DIM_v(o,ia,ib)

      SDO
        v1o(i,j,k)= v1ia(i,j,k) + v1ib(i,j,k)
        v2o(i,j,k)= v2ia(i,j,k) + v2ib(i,j,k)
        v3o(i,j,k)= v3ia(i,j,k) + v3ib(i,j,k)
      SENDDO

      RETURN_END
c######################################################################
      subroutine sub_vvv( V(o), V(ia), V(ib) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(ia) - V(ib).

      PARAM ; DIM_v(o,ia,ib)

      SDO
        v1o(i,j,k)= v1ia(i,j,k) - v1ib(i,j,k)
        v2o(i,j,k)= v2ia(i,j,k) - v2ib(i,j,k)
        v3o(i,j,k)= v3ia(i,j,k) - v3ib(i,j,k)
      SENDDO

      RETURN_END
c######################################################################
      subroutine mul_vvr( V(o), V(i), rval )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(i)*rval.
      PARAM ; DIM_v(o,i)

      MUL_aar( v1o, v1i, rval, nijk )
      MUL_aar( v2o, v2i, rval, nijk )
      MUL_aar( v3o, v3i, rval, nijk )

      RETURN_END
c######################################################################
      subroutine mul_vvs( V(o), V(i), si )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(i)*si.
      PARAM ; DIM_s(si) ; DIM_v(o,i)

      MUL_aaa( v1o, v1i, si, nijk )
      MUL_aaa( v2o, v2i, si, nijk )
      MUL_aaa( v3o, v3i, si, nijk )

      RETURN_END
c######################################################################
      subroutine mul_vvsr( V(o), V(i), si, rval )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(i)*si*rval.
      PARAM ; DIM_s(si) ; DIM_v(o,i)

      MUL_aaar( v1o, v1i, si, rval, nijk )
      MUL_aaar( v2o, v2i, si, rval, nijk )
      MUL_aaar( v3o, v3i, si, rval, nijk )

      RETURN_END
c######################################################################
      subroutine divi_vvs( V(o), V(i), si )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# V(o)= V(i)/si.
      PARAM ; DIM_s(si) ; DIM_v(o,i)

      DIVI_aaa( v1o, v1i, si, nijk )
      DIVI_aaa( v2o, v2i, si, nijk )
      DIVI_aaa( v3o, v3i, si, nijk )

      RETURN_END
c######################################################################
      subroutine mul_vvv( V(o), V(ia), V(ib) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# Each component of V(o) = product of the same component of V(ia) and
!  V(ib).

      PARAM ; DIM_v(o,ia,ib)

      SDO
        v1o(i,j,k)= v1ia(i,j,k)*v1ib(i,j,k)
        v2o(i,j,k)= v2ia(i,j,k)*v2ib(i,j,k)
        v3o(i,j,k)= v3ia(i,j,k)*v3ib(i,j,k)
      SENDDO

      RETURN_END
c######################################################################
      subroutine dot_svv( so, V(ia), V(ib) )
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
# so= V(ia) dot V(ib).

      PARAM ; DIM_s(so) ; DIM_v(ia,ib)

      SDO
        so(i,j,k)= v1ia(i,j,k)*v1ib(i,j,k)
     &         +   v2ia(i,j,k)*v2ib(i,j,k)
     &         +   v3ia(i,j,k)*v3ib(i,j,k)
      SENDDO

      RETURN_END

      ifelse(CMPLR,f90,,[
c######################################################################
      subroutine set_ar(ao,rval,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel)

      do i= 1,nel
        ao(i)= rval
      enddo i

      RETURN_END
c######################################################################
      subroutine set_aa(ao,ai,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai(nel)

      do i= 1,nel
        ao(i)= ai(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine neg_aa(ao,ai,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai(nel)

      do i= 1,nel
        ao(i)= -ai(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine inv_aa(ao,ai,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai(nel)

      do i= 1,nel
        ao(i)= 1./ai(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine add_aar(ao,ai,rval,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai(nel)

      do i= 1,nel
        ao(i)= ai(i) + rval
      enddo i

      RETURN_END
c######################################################################
      subroutine add_aaa(ao,ai1,ai2,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel)

      do i= 1,nel
        ao(i)= ai1(i) + ai2(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine add_aaar(ao,ai1,ai2,rval,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel)

      do i= 1,nel
        ao(i)= ai1(i) + ai2(i)*rval
      enddo i

      RETURN_END
c######################################################################
      subroutine sub_aaa(ao,ai1,ai2,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel)

      do i= 1,nel
        ao(i)= ai1(i) - ai2(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine av_aaa(ao,ai1,ai2,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel)

      do i= 1,nel
        ao(i)= .5* ( ai1(i) + ai2(i) )
      enddo i

      RETURN_END
c######################################################################
      subroutine mul_aar(ao,ai,rval,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai(nel)

      do i= 1,nel
        ao(i)= ai(i) * rval
      enddo i

      RETURN_END
c######################################################################
      subroutine mul_aaa(ao,ai1,ai2,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel)

      do i= 1,nel
        ao(i)= ai1(i) * ai2(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine mul_aaar(ao,ai1,ai2,rval,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel)

      do i= 1,nel
        ao(i)= ai1(i) * ai2(i) * rval
      enddo i

      RETURN_END
c######################################################################
      subroutine mul_aaaa(ao,ai1,ai2,ai3,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel),ai3(nel)

      do i= 1,nel
        ao(i)= ai1(i) * ai2(i) * ai3(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine divi_aaa(ao,ai1,ai2,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai1(nel),ai2(nel)

      do i= 1,nel
        ao(i)= ai1(i) / ai2(i)
      enddo i

      RETURN_END
c######################################################################
      subroutine pow_aar(ao,ai,rval,nel)
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      dimension ao(nel),ai(nel)

      do i= 1,nel
        ao(i)= ai(i)**rval
      enddo i

      RETURN_END
      ])

      ])

      ifelse(lRAN1,T,[
c**********************************************************************
      FUNCTION RAN1(IDUM)
c**********************************************************************
      DIMENSION RRN(97)
      PARAMETER (M1=259200,IA1=7141,IC1=54773,RM1=3.8580247E-6)
      PARAMETER (M2=134456,IA2=8121,IC2=28411,RMM2=7.4373773E-6)
      PARAMETER (M3=243000,IA3=4561,IC3=51349)
      SAVE IX1,IX2,IX3,RRN
      DATA IFF /0/                          # SAVEd automatically
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
11      CONTINUE
        IDUM=1
      ENDIF
      IX1=MOD(IA1*IX1+IC1,M1)
      IX2=MOD(IA2*IX2+IC2,M2)
      IX3=MOD(IA3*IX3+IC3,M3)
      J=1+(97*IX3)/M3
      ERROR_EXIT(J.GT.97.OR.J.LT.1, RAN1)
c     IF(J.GT.97.OR.J.LT.1)PAUSE
      RAN1=RRN(J)
      RRN(J)=(REAL(IX1)+REAL(IX2)*RMM2)*RM1
      RETURN
      END
c**********************************************************************
      FUNCTION ran2(idum)
c**********************************************************************
      implicit none
      integer, parameter :: k4b=selected_int_kind(9)
      integer(k4b), intent(inout) :: idum
      real :: ran2

      call RANDOM_NUMBER(ran2)

      END FUNCTION ran2
c**********************************************************************
      FUNCTION ran(idum)
c**********************************************************************
      implicit none
      integer, parameter :: k4b=selected_int_kind(9)
      integer(k4b), intent(inout) :: idum
      real :: ran
      integer(k4b), parameter :: ia=16807,im=2147483647,iq=127773,ir=2836
      real*4, save :: am
      integer(k4b), save :: ix=-1,iy=-1,k
     
      if(idum <= 0 | iy < 0 ) then
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
      if( iy<0 ) iy= iy+im
      ran= dble( am*ior(iand(im,ieor(ix,iy)),1))
      END FUNCTION ran
      ],)
c**********************************************************************
      FUNCTION BESSJ0(X)
c**********************************************************************
      REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6,
     *    S1,S2,S3,S4,S5,S6
      DATA P1,P2,P3,P4,P5/1.0,-.1098628627E-2,.2734510407E-4,
     *    -.2073370639E-5,.2093887211E-6/, Q1,Q2,Q3,Q4,Q5/-.1562499995E-
     *1,
     *    .1430488765E-3,-.6911147651E-5,.7621095161E-6,-.934945152E-7/
      DATA R1,R2,R3,R4,R5,R6/57568490574.0,-13362590354.0,651619640.70,
     *
     *    -11214424.180,77392.330170,-184.90524560/,
     *    S1,S2,S3,S4,S5,S6/57568490411.0,1029532985.0,
     *    9494680.7180,59272.648530,267.85327120,1.0/
      IF(ABS(X).LT.8.)THEN
        Y=X**2
        BESSJ0=(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6)))))
     *      /(S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
        AX=ABS(X)
        Z=8./AX
        Y=Z**2
        XX=AX-.785398164
        BESSJ0=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y
     *      *P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
      ENDIF
      RETURN
      END
c**********************************************************************
      FUNCTION BESSJ1(X)
c**********************************************************************
      REAL*8 Y,P1,P2,P3,P4,P5,Q1,Q2,Q3,Q4,Q5,R1,R2,R3,R4,R5,R6,
     *    S1,S2,S3,S4,S5,S6
      DATA R1,R2,R3,R4,R5,R6/72362614232.0,-7895059235.0,242396853.10
     *,
     *    -2972611.4390,15704.482600,-30.160366060/,
     *    S1,S2,S3,S4,S5,S6/144725228442.0,2300535178.0,
     *    18583304.740,99447.433940,376.99913970,1.0/
      DATA P1,P2,P3,P4,P5/1.0,.183105E-2,-.3516396496E-4,.2457520174E-5
     *,
     *    -.240337019E-6/, Q1,Q2,Q3,Q4,Q5/.046874999950,-.2002690873E-3
     *,
     *    .8449199096E-5,-.88228987E-6,.105787412E-6/
      IF(ABS(X).LT.8.)THEN
        Y=X**2
        BESSJ1=X*(R1+Y*(R2+Y*(R3+Y*(R4+Y*(R5+Y*R6)))))
     *      /(S1+Y*(S2+Y*(S3+Y*(S4+Y*(S5+Y*S6)))))
      ELSE
        AX=ABS(X)
        Z=8./AX
        Y=Z**2
        XX=AX-2.356194491
        BESSJ1=SQRT(.636619772/AX)*(COS(XX)*(P1+Y*(P2+Y*(P3+Y*(P4+Y
     *      *P5))))-Z*SIN(XX)*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+Y*Q5)))))
     *      *SIGN(1.,X)
      ENDIF
      RETURN
      END
c**********************************************************************
      FUNCTION BESSJ(N,X)
c**********************************************************************
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
 11     CONTINUE
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
 12     CONTINUE
        SUM=2.*SUM-BJ
        BESSJ=BESSJ/SUM
      ENDIF
      IF(X.LT.0..AND.MOD(N,2).EQ.1)BESSJ=-BESSJ
      RETURN
      END

c**********************************************************************
      FUNCTION BESSI1(X)
c**********************************************************************
      REAL*8 Y,P1,P2,P3,P4,P5,P6,P7,
     *     Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9
      DATA P1,P2,P3,P4,P5,P6,P7/0.5D0,0.87890594D0,0.51498869D0,
     *     0.15084934D0,0.2658733D-1,0.301532D-2,0.32411D-3/
      DATA Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9/0.39894228D0,-0.3988024D-1,
     *     -0.362018D-2,0.163801D-2,-0.1031555D-1,0.2282967D-1,
     *     -0.2895312D-1,0.1787654D-1,-0.420059D-2/
      IF (ABS(X).LT.3.75) THEN
          Y=(X/3.75)**2
          BESSI1=X*(P1+Y*(P2+Y*(P3+Y*(P4+Y*(P5+Y*(P6+Y*P7))))))
      ELSE
          AX=ABS(X)
          Y=3.75/AX
          BESSI1=(EXP(AX)/SQRT(AX))*(Q1+Y*(Q2+Y*(Q3+Y*(Q4+
     *         Y*(Q5+Y*(Q6+Y*(Q7+Y*(Q8+Y*Q9))))))))
          IF(X.LT.0.) BESSI1=-BESSI1
      ENDIF
      RETURN
      END

c######################################################################
C THOMAS ALGORITHM FOR TRIDIAGONAL MATRICES  -  SYSTEM FORTRAN-77
C A, B, AND C = LHS COLUMNS, DIMENSIONED (=>N) IN CALLING PROGRAM
C R = RIGHT-HAND-SIDE, DIMENSIONED (=>N) IN CALLING PROGRAM 
C N = # OF EQUATIONS             
C SOLUTION RETURNS IN R WHEN KKK => 2
C KKK = 1 PERFORMS LU DECOMPOSITION - ORIGINAL A,B,C ARRAYS ARE MODIFIED!!
C KKK = 2 PERFORMS BACK SUBSTITUTION - ASSUMES LU DECOMPOSITION ALREADY DONE
C KKK = 3 PERFORMS OPTIONS 1 AND 2
C
      SUBROUTINE THOMAS(KKK,A,B,C,R,N)            
      DIMENSION A(N),B(N),C(N),R(N)          
C                                                                               
C  STEP 1 - TRIANGULARIZE MATRIX A USING DOOLITTLE METHOD
C
      IF(KKK.EQ.2)GO TO 45
      C(1)=C(1)/B(1)           
      DO 40 I=2,N              
      IM=I-1            
      C(I)=C(I)/(B(I)-A(I)*C(IM))            
   40 CONTINUE
      IF(KKK.EQ.1)RETURN
C                
C STEP 2 - MODIFY LOAD VECTOR R 
C
   45 CONTINUE
      R(1)=R(1)/B(1)           
      DO 41 I=2,N              
      IM=I-1            
      R(I)=(R(I)-A(I)*R(IM))/(B(I)-A(I)*C(IM))      
   41 CONTINUE
C
C BACK SUBSTITUTE SOLUTION INTO R VECTOR
C                
      DO 50 I=1,N-1            
      J=N-I             
      JP=J+1            
      R(J)=R(J)-C(J)*R(JP)            
   50 CONTINUE          
      RETURN            
      END


c**********************************************************************
      FUNCTION CEL(QQC,PP,AA,BB)
c**********************************************************************
c General complete elliptic integral (Num. Recipes p.187), used to
c calculate K(k) and E(k), where k=sqrt(1-QQC**2).

      PARAMETER (CA=.0003, PI02=1.5707963268)
c The desired accuracy is the square of CA.
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
 1    F=A
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

