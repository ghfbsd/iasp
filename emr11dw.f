      subroutine emdlv(r,vp,vs)
c        Velocity model subroutine for (geophysical) Mars model from
c        Rivoldini et al. (2011) with Dreibus and Wanke compositional model.
c
c     Reference:
c        Rivoldini, A. et al. (2011). Geodesy constraints on the interior
c        structure and composition of Mars.  Icarus, 213, 451-472.
c
      parameter (re=3389.50)
      call vr11dw(re-r,vp,vs,rho)
      end

      subroutine emdld(dmax,n,cpr,name)
      parameter (re=3389.50,nd=3,eqdmax=300.)
      dimension cpr(nd),rd(nd)
      character*(*) name
      character*20 modnam
      logical first
      data rd/1658.5, 85., 0./
      data modnam/'r11dw'/, first/.true./
      if (first) then
	 first = .false.
	 do i=1,nd
	    rd(i) = re-rd(i)
         enddo
      endif
      n=nd
      do i=1,nd
         cpr(i)=rd(i)
      enddo
      name=modnam
      dmax=eqdmax
      return
      end

      subroutine vr11dw(pz,pp,ps,prho)
      parameter (nz=119)
      real z(nz),rho(nz),vp(nz),vs(nz)
      real interp

      data (z(i),rho(i),vp(i),vs(i),i=1,nz) /
     +      0.0,  2.700,  7.893,  4.550,
     +     85.0,  2.700,  7.893,  4.550,
     +     85.0,  2.710,  8.045,  4.574,
     +    105.0,  3.480,  8.045,  4.569,
     +    125.0,  3.490,  8.045,  4.564,
     +    145.0,  3.490,  8.043,  4.558,
     +    165.0,  3.490,  8.040,  4.553,
     +    185.0,  3.490,  8.037,  4.547,
     +    205.0,  3.490,  8.035,  4.542,
     +    225.0,  3.490,  8.032,  4.536,
     +    245.0,  3.490,  8.028,  4.530,
     +    265.0,  3.490,  8.025,  4.522,
     +    285.0,  3.500,  8.021,  4.513,
     +    305.0,  3.500,  8.018,  4.504,
     +    325.0,  3.500,  8.014,  4.496,
     +    345.0,  3.500,  8.011,  4.487,
     +    365.0,  3.500,  8.007,  4.478,
     +    385.0,  3.500,  8.003,  4.467,
     +    405.0,  3.500,  8.000,  4.457,
     +    425.0,  3.500,  7.997,  4.446,
     +    445.0,  3.510,  7.998,  4.436,
     +    465.0,  3.510,  8.000,  4.425,
     +    485.0,  3.510,  8.002,  4.418,
     +    505.0,  3.510,  8.005,  4.411,
     +    525.0,  3.510,  8.007,  4.404,
     +    545.0,  3.520,  8.016,  4.397,
     +    565.0,  3.520,  8.037,  4.390,
     +    585.0,  3.520,  8.058,  4.393,
     +    605.0,  3.520,  8.080,  4.398,
     +    625.0,  3.530,  8.101,  4.403,
     +    645.0,  3.530,  8.123,  4.408,
     +    665.0,  3.540,  8.144,  4.413,
     +    685.0,  3.540,  8.165,  4.418,
     +    705.0,  3.550,  8.187,  4.423,
     +    725.0,  3.550,  8.208,  4.427,
     +    745.0,  3.560,  8.229,  4.431,
     +    765.0,  3.570,  8.221,  4.436,
     +    785.0,  3.570,  8.205,  4.441,
     +    805.0,  3.580,  8.221,  4.444,
     +    825.0,  3.580,  8.237,  4.445,
     +    845.0,  3.590,  8.253,  4.447,
     +    865.0,  3.590,  8.270,  4.428,
     +    885.0,  3.600,  8.286,  4.435,
     +    905.0,  3.600,  8.302,  4.441,
     +    925.0,  3.610,  8.318,  4.448,
     +    945.0,  3.610,  8.335,  4.454,
     +    965.0,  3.620,  8.351,  4.460,
     +    985.0,  3.630,  8.385,  4.465,
     +   1005.0,  3.640,  8.432,  4.471,
     +   1025.0,  3.650,  8.479,  4.476,
     +   1045.0,  3.660,  8.526,  4.481,
     +   1065.0,  3.670,  8.573,  4.507,
     +   1085.0,  3.690,  8.620,  4.534,
     +   1105.0,  3.700,  9.084,  4.561,
     +   1125.0,  3.710,  9.115,  4.588,
     +   1145.0,  3.720,  9.146,  4.616,
     +   1165.0,  3.740,  9.178,  4.643,
     +   1185.0,  3.770,  9.209,  4.775,
     +   1205.0,  3.820,  9.240,  4.954,
     +   1225.0,  3.860,  9.271,  4.975,
     +   1245.0,  3.880,  9.303,  4.997,
     +   1265.0,  3.890,  9.334,  5.018,
     +   1285.0,  3.910,  9.343,  5.039,
     +   1305.0,  3.930,  9.343,  5.061,
     +   1325.0,  3.940,  9.347,  5.082,
     +   1345.0,  3.960,  9.360,  5.103,
     +   1365.0,  3.970,  9.372,  5.124,
     +   1385.0,  3.990,  9.385,  5.128,
     +   1405.0,  4.010,  9.397,  5.132,
     +   1425.0,  4.020,  9.410,  5.136,
     +   1445.0,  4.030,  9.422,  5.140,
     +   1465.0,  4.050,  9.435,  5.144,
     +   1485.0,  4.050,  9.447,  5.148,
     +   1505.0,  4.050,  9.460,  5.152,
     +   1525.0,  4.060,  9.468,  5.156,
     +   1545.0,  4.060,  9.474,  5.159,
     +   1565.0,  4.070,  9.479,  5.162,
     +   1585.0,  4.070,  9.484,  5.165,
     +   1605.0,  4.080,  9.485,  5.167,
     +   1625.0,  4.080,  9.486,  5.170,
     +   1645.0,  4.090,  9.486,  5.173,
     +   1658.5,  4.120,  9.487,  5.175,
     +   1658.5,  6.030,  5.004,  0.000,
     +   1659.0,  6.040,  5.004,  0.000,
     +   1709.0,  6.070,  5.036,  0.000,
     +   1759.0,  6.100,  5.065,  0.000,
     +   1809.0,  6.130,  5.094,  0.000,
     +   1859.0,  6.160,  5.121,  0.000,
     +   1909.0,  6.190,  5.147,  0.000,
     +   1959.0,  6.220,  5.172,  0.000,
     +   2009.0,  6.250,  5.196,  0.000,
     +   2059.0,  6.280,  5.220,  0.000,
     +   2109.0,  6.300,  5.242,  0.000,
     +   2159.0,  6.330,  5.263,  0.000,
     +   2209.0,  6.350,  5.283,  0.000,
     +   2259.0,  6.370,  5.302,  0.000,
     +   2309.0,  6.400,  5.320,  0.000,
     +   2359.0,  6.410,  5.337,  0.000,
     +   2409.0,  6.430,  5.353,  0.000,
     +   2459.0,  6.450,  5.369,  0.000,
     +   2509.0,  6.460,  5.383,  0.000,
     +   2559.0,  6.480,  5.397,  0.000,
     +   2609.0,  6.490,  5.410,  0.000,
     +   2659.0,  6.510,  5.422,  0.000,
     +   2709.0,  6.520,  5.433,  0.000,
     +   2759.0,  6.530,  5.443,  0.000,
     +   2809.0,  6.530,  5.453,  0.000,
     +   2859.0,  6.540,  5.461,  0.000,
     +   2909.0,  6.550,  5.469,  0.000,
     +   2959.0,  6.560,  5.476,  0.000,
     +   3009.0,  6.560,  5.483,  0.000,
     +   3059.0,  6.560,  5.488,  0.000,
     +   3109.0,  6.570,  5.493,  0.000,
     +   3159.0,  6.570,  5.497,  0.000,
     +   3209.0,  6.570,  5.500,  0.000,
     +   3259.0,  6.570,  5.502,  0.000,
     +   3309.0,  6.570,  5.504,  0.000,
     +   3359.0,  6.570,  5.505,  0.000,
     +   3389.5,  6.570,  5.505,  0.000/

      pp = interp(z,vp,nz,1,pz,err)
      ps = interp(z,vs,nz,1,pz,err)
      prho = interp(z,rho,nz,1,pz,err)
      end

      real function interp(xa,ya,n,npts,x,err)
C    Function to interpolate between tabulated values.  The function
C       returns the interpolated result, plus an error.  A number of
C       points to either side of the desired value is given as an argument.
C       This value should be as small as feasible, since large values cause
C       more variation in the interpolated result.
      real xa(n), ya(n), x, value, err

C     Run through tables and bracket value requested.  Interpolate
C        with a more limited number of points.
      if (xa(1) .lt. xa(n)) then
	 ilo = 1
	 ihi = n
      else
	 ilo = n
	 ihi = 1
      endif
10    continue
	 i = (ihi + ilo)/2
	 if (x .eq. xa(i)) then
	    interp = ya(i)
	    err = 0.0
	    return
	 endif
	 if (x .lt. xa(i)) then
	    ihi = i
	 else
	    ilo = i
	 endif
      if (abs(ihi - ilo) .gt. 1) go to 10
      if (ihi .lt. ilo) then
	 i = ihi
	 ihi = ilo
	 ilo = i
      endif

C     Have desired value bracketed.  Adjust bounds and interpolate.
      j = max(1,ilo-npts+1)
      k = min(n,ihi+npts-1)
      if (k-j+1 .ne. 2*npts) then
	 if (j .eq. 1) then
	    k=min(j+2*npts-1,n)
	 else
	    j=max(1,k-2*npts+1)
	 endif
      endif
      call polint(xa(j),ya(j),k-j+1,x,value,err)
      interp = value
      return
      end

      SUBROUTINE POLINT(XA,YA,N,X,Y,DY)
      PARAMETER (NMAX=25) 
      DIMENSION XA(N),YA(N),C(NMAX),D(NMAX)
      IF (N .GT. NMAX) STOP '**POLINT:  TOO MUCH DATA.'
      NS=1
      DIF=ABS(X-XA(1))
      DO 11 I=1,N 
        DIFT=ABS(X-XA(I))
        IF (DIFT.LT.DIF) THEN
          NS=I
          DIF=DIFT
        ENDIF
        C(I)=YA(I)
        D(I)=YA(I)
11    CONTINUE
      Y=YA(NS)
      NS=NS-1
      DO 13 M=1,N-1
        DO 12 I=1,N-M
          HO=XA(I)-X
          HP=XA(I+M)-X
          W=C(I+1)-D(I)
          DEN=HO-HP
          IF(DEN.EQ.0.)STOP '**POLINT:  IMPOSSIBLE!'
          DEN=W/DEN
          D(I)=HP*DEN
          C(I)=HO*DEN
12      CONTINUE
        IF (2*NS.LT.N-M)THEN
          DY=C(NS+1)
        ELSE
          DY=D(NS)
          NS=NS-1
        ENDIF
        Y=Y+DY
13    CONTINUE
      RETURN
      END
