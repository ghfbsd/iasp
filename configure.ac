dnl Configure input for iasp-grh - G. Helffrich/U. Bristol April 21 2008
AC_INIT(limits.inc.in)
AC_CONFIG_AUX_DIR(utils)
dnl AC_PROG_CC(cc gcc)
AC_PROG_F77(gfortran g77 f77)
dnl AC_PROG_RANLIB
AC_PROG_INSTALL
AC_F77_LIBRARY_LDFLAGS
AC_CANONICAL_HOST
JSEG=40 JBRN=150 JOUT=3350 NOPT=1
case $host in
   *-apple-darwin*) # Override default Darwin mandir
      test "$mandir" = '${prefix}/man' && mandir='${prefix}/share/man'
      ;;
esac
dnl Have to undo damage done by F77_LIBRARY_LDFLAGS on MacOS systems
KLUDGE= KEEP=
for L in $ac_cv_flibs $ac_cv_f77_libs ; do
   case $L in
   -l*) ;;
   -L*) KEEP=1 ;;
   *) ;;
   esac
   test -z "${KEEP}" || KLUDGE="${KLUDGE} $L"
done
sym=neither
test -z "$ac_cv_flibs" || sym=ac_cv_flibs
test -z "$ac_cv_f77_libs"  || sym=ac_cv_f77_libs
: ${sym}="${KLUDGE}" ; FLIBS="${KLUDGE}"
dnl Set default CFLAGS, FFLAGS to what we want
KEEP=
for L in $FFLAGS ; do
   case $L in
   -g* | -O*) ;;
   *) KEEP="${KEEP} $L"
   ;;
   esac
done
FFLAGS=`echo "${KEEP}"`

AC_SUBST(ADD_FFLAGS)
AC_ARG_VAR(BINDIR,[ttimes program location])
AC_ARG_VAR(LIBDIR,[tpttsub.o library routine location])

dnl C library routine checks
dnl AC_CHECK_LIB(SystemStubs,err$LDBLStub)
dnl AC_CHECK_FUNC(snprintf,,[ADD_CFLAGS="${ADD_CFLAGS} -DDONTHAVE_SNPRINTF"])
dnl AC_CHECK_FUNC(reallocf,,[ADD_CFLAGS="${ADD_CFLAGS} -DDONTHAVE_REALLOCF"])

LIBS="${LDFLAGS} ${LIBS}"

AC_LANG_PUSH(Fortran 77)
SV_FFLAGS="${FFLAGS}" F77BC=
dnl Check for useful Fortran options
for F in fbounds-check C unavailable-option ; do
   FFLAGS="-c -$F"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[break])
done
test "$F" = "unavailable-option" || F77BC="-$F"
dnl Check Fortran for brief debug info availability
FFLAGS="-g -feliminate-unused-debug-symbols"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[FDEBUG=" -feliminate-unused-debug-symbols"],[FDEBUG=])
dnl Check for need for Fortran equivalence/common table size options
rm -f conftest.$ac_objext conftest.$ac_ext
cat >conftest.$ac_ext <<_ACEOF
      program main
      end
_ACEOF
F77MISC=
for F in Nq300 Nn2000 std=legacy ; do
   FFLAGS="-c -$F"
AC_TRY_COMPILE([],[
      integer array(2)
      array(1)=1
      array(2)=2
],[F77MISC="${F77MISC} -$F"])
done
rm -f conftest.$ac_objext conftest.$ac_ext
AC_LANG_POP(Fortran 77)
FFLAGS="${SV_FFLAGS} ${F77MISC}"

test ${enable_debug-no} = yes && \
    ADD_FFLAGS="${ADD_FFLAGS} -g${FDEBUG}"
test ${enable_opt-yes} = yes && \
    ADD_FFLAGS="${ADD_FFLAGS} ${FOPT}"
test ${enable_range_check-no} = yes && ADD_FFLAGS="${ADD_FFLAGS} ${F77BC}"

AC_CONFIG_FILES(limits.inc)
AC_SUBST(JSEG)
AC_SUBST(JBRN)
AC_SUBST(JOUT)
AC_SUBST(NOPT)

dnl Optional compilation defines
AC_ARG_ENABLE(core,  
   [  --enable-core           adds PmKP m=3,4 and SmKS m=3,4,5],
   [JSEG=`expr $JSEG + 5` JOUT=`expr $JOUT + 450` NOPT=`expr $NOPT + 4`]
)
AC_ARG_ENABLE(depth,  
   [  --disable-depth         suppress depth phases for most phases (smaller tables)],
   [JSEG=`expr $JSEG - 10` JOUT=`expr $JOUT - 750` NOPT=`expr $NOPT - 1`]
)
AC_ARG_ENABLE(debug,  
   [
  --enable-debug          compiles programs with debugging enabled]
)
AC_ARG_ENABLE(range_check,  
   [  --enable-range-check    checks that array references are within bounds]
)
AC_ARG_ENABLE(opt,  
   [  --disable-opt           compiles programs with optimizing disabled]
)

test "${BINDIR}" = NONE && BINDIR=$bindir
test -z "${LIBDIR}" && LIBDIR=$libdir

AC_OUTPUT(Makefile)
echo "Configured with Fortran compiler" $F77
echo "Configured with Fortran compiler flags" $FFLAGS $ADD_FFLAGS
echo "Programs will be installed in:" ${prefix}/bin
echo "Libraries will be installed in:" ${prefix}/lib
echo "man pages will be installed in:" ${datarootdir}/man
