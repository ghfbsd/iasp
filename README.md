# Buland and Kennett Tau-P

The Buland and Kennett tau-p routines are a suite of programs that calculate
seismic wave travel times through a radial earth model specified as velocity
as a function of depth.
They use the methods described in Buland and Chapman (1983), BSSA 73, 1271-1302
to calculate tables of tau to quickly determine all arrivals at a particular
range for an earthquake at a particular depth.
The original package provided a command line interface to the tables, and
tables for one model, *iasp91* by Kennett and Engdahl (1991), GJI 105,
429-465.

The package is useful and versatile, but lacked tables for other wavespeed
models and a simple, well-documented subroutine interface to the tables so
that programs could take advantage of the rapid calculations the method
affords.
Consequently, I extended the package to include other earth models,
defined a simple subroutine interface to it, and built a configuration-based
compilation method that makes the program, subroutines and tables easy to
build and install.

The general changes are:

* do automatic source configuration;
* remove Fortran language constructs that prevent it from compiling
   on the GCC Fortran compilers;
* to write table formats that are self describing (in size) and do
   not require recompilation of previously-built travel time tables if
   the table storage size changes;
* adds additional phases to the set that is computed in the tables,
   viz. depth phases for ScS and PcP and diffracted phases for PKP branches;
* adds a subroutine-callable interface to the tables;
* adds the capability to define models for planets lacking an inner core;
* adds tables for different models:
   ek137, iasp91, ak135, fakeprem, fakeprem20s, sp6, kghj, pemc (Earth), and
   ss97a and r11dw (Mars);
* adds a routine to extract slowness-range-travel time for each phase;
* optionally, under configuration control, adds more depth phases
   and core phases.


To configure and install:

```
./configure
make
make install
make clean
```

This will build a default set of travel time tables for all models.  To build
a smaller set of tables by eliminating the depth phase arrivals for core
reflections and multiple core phases, use the `--disable-depth` option:
```
./configure --disable-depth
```
To enable generation of multiple core bounce phases (PmKP m=3,4 and SmKS
m=3,4,5), configure with `--enable-core` option:
```
./configure --enable-core
```
This can be used in conjunction with `--disable-depth`.

Default install location for travel time tables and linkable subroutine code
is `/usr/local/lib`, and for the ttimes program is `/usr/local/bin`.  See
```
./configure --help=short
```
for directions explaining how to change this.

```
                                 TAU
   
   The files in this anonymous FTP implement the travel time computation
algorithm described by Buland and Chapman (1983) in "The Computation of
Seismic Travel Times", BSSA, v. 73, pp. 1271-1302, for the IASPEI phase
set derived from the IASPEI travel-time model developed by Brian Kennett.
This phase set currently includes:  P, Pdiff, PKP, PKiKP, pP, pPdiff,
pPKP, pPKiKP, sP, sPdiff, sPKP, sPKiKP, PP, P'P', S, Sdiff, SKS, pS,
pSdiff, pSKS, sS, sSdiff, sSKS, SS, S'S', PS, PKS, SP, SKP, SKiKP, PcP,
PcS, ScP, ScS, PKKP, PKKS, SKKP, and SKKS.  The IASPEI model was
developed specifically as a replacement for the Jeffreys-Bullen model for
earthquake location work.  It incorporates a PEM core, a lower mantle 
developed by Ken Toy, and an upper mantle constructed by Brian Kennett.
The phase set was chosen by requiring that the phases be well observed in
some distance range and that they be useful either for earthquake
location or for studies of earth structure.

   This implementation of the travel-time algorithm includes all
refracted arrivals of each phase listed, but only the partial
reflections specifically noted in the phase list.  All total internal
reflections (except at the free surface) have been supressed as they are
almost always tertiary arrivals.  This policy does result in no PS and
SP arrivals in the distance range of about 25-50 degrees.  Phase naming
is derived directly from the model layer in which the phase turns and is
sometimes at odds with various seismological conventions.  In particular,
the lack of a low velocity zone in the upper mantle of the IASPEI model
results in Pn and Sn extending out to about 25 degrees.  Even without the
total reflections, the upper mantle discontinuities and triplications
will typically result in two to four P and S arrivals each in the
distance range of about 17-30 degrees.

   The tables are generated by a two step process which involves running
first program REMODL and then program SETBRN.  REMODL generates the
model by calling routines in EMIASP91.F.  In principle any model may be
used.  However, a range of IASPEI models has actually been tested.
REMODL examines the model and its implications, makes up a best guess at
how to sample slowness, and performs all of the raw integrals.  It writes
its results into the REMODL.HED and REMODL.TBL files.  REMODL1.LIS and
REMODL2.LIS show some of what goes on inside of REMODL and are useful
when REMODL is being converted to a different computer.  SETBRN takes the
results of REMODL from (REMODL.HED AND REMODL.TBL) and constructs the
specific branches desired by the user.  While some attempt has been
made to make SETBRN ``programmable'', care must be taken that phases
added are within the assumptions made by the interpolation routines in
LIBTAU.F.  SETBRN produces the tau-p tables IASP91.HED AND IASP91.TBL.
The access to these tables has been cleaned up so that they are quite
machine independent.  SETBRN also produces internal listing files
SETBRN1.LIS, SETBRN2.LIS, and SETBRN3.LIS.  REMODL and SETBRN (and the
routines they call) use the include file LIMITS.INC.  It is assumed that
the include file is in the directory in which compilation is being done.
These programs are linked with the library LIBTAU.A (which should be
constructed from the source in LIBTAU.F).  REMODL and SETBRN also require
machine dependent routines from either LIBVAX.A or LIBSUN.A (which should
be constructed from the source in LIBVAX.F or LIBSUN.F) depending on the
computer being used.  Note that REMODL and SETBRN are not actually needed
to simply invoke the tau-p tables once generated.  They are only used for
setting up the tables the first time.

   To invoke the travel times, a handful of routines also in the LIBTAU
library need to be called.  An example of how this is done is given in
the program TTIMES.  Note that a different include file, TTLIM.INC is
needed.  Playing with the print flags can give you a feeling for what
TTIMES does, but one of them generates a lot of output (into file
TTIM1.LIS).  The tau interpolation is usually only needed for debugging.
However, the range summary can be very useful as it shows the distance
range over which each branch of each phase will appear.  The BRNSET
routine can be used interactively (as in TTIMES) or hardwired for a
specific set of phases.  It allows the user to select only the phases
of immediate interest.  When running TTIMES, note that at each distance,
all possible arrivals from all branches are given in time order.  The
travel-time is given twice (in seconds and in minutes and seconds).  The
numbers at the end of each line are various derivatives (travel-time
with respect to distance, travel-time with respect to depth and the
second derivative of travel-time with respect to distance).

                                           Ray Buland

P.S. UNIX manual pages generated by Brian Kennett are provided in the file 
     TAU.MAN.

LOCAL CHANGES:

   A subroutine called tptt (code: tptt.f; binary tpttsub.o) is available
to return travel time information (time, dt/ddelta, dt/ddepth, ddelta/dp)
from these tables.  See the code for documentation on use.  (G. Helffrich
Carnegie/DTM 28 Aug. 1991)

   Include files limits.inc and libtau.inc were rationalized so that
   dimensioning dependencies were made explicit.  All declarations changed
   to use parameter values set in limits.inc and libtau.inc.
      G. Helffrich/UB 27 Apr. 1993

   libsun and libvax changed to add evget subroutine.
   ttimes changed to take module name from environment variable if asked.
      G. Helffrich/UB 27 Apr. 1993

   emiasp91.f changed to flip data statements, making modification of parts
   of model possible.
      G. Helffrich/UB 5 June 1994
   
   Discontinuity size in remodl.f changed so that these aren't declared where
   the mismatch is minor between velocities.  This causes trouble with the
   iasp91 model and has been returned to its original size.
      G. Helffrich/UB 5 June 1994
   
   Table writing rearranged so that table size can be checked by ttimes.
   This is to ensure that tables that are too large or even a different
   size may be read in correctly by the program.
      G. Helffrich/UB 13 August 1995

   Modified libtau.f so that PKPbc is also diffracted around the inner core.
      G. Helffrich/UB 3 Dec. 1996
   
   Added code to check for table overflows of various sorts in remodl and
   setbrn.  These mostly result in pause statements executed with cryptic
   comments.
      G. Helffrich/UB 20 Mar. 1998
   
   ***WARNING: This is not a backwards-compatible change.  If you make this
   change, you have to either rebuild all your .hed files, or keep an old
   version of ttimes around capable of reading the old table format. ***
   Modified .hed format so that the ttimes program and friends can determine
   whether the table it will read in is too big to fit.  This permits changes
   to the size of built-in tables without having to change previous versions
   of the tables that ttimes reads in.
      G. Helffrich/UB 29 Mar. 1998
   
   Checked for overflow of temporary storage in fitspl (libtau.f).  Will
   stop with an error message if number of branches exceeds bounds.
      G. Helffrich/UB 1 Aug. 1998

   remodl.f: Fixed bug in findrng which caused division by zero if any value
   in the y table was equal to y(i) in inverse iteration.  Basically caused by
   fact that xmax is real*4 and function arguments are real*8 - so you can get
   no change in xmax for a change in its x value.
      G. Helffrich/UB 6 Aug. 2003

   remodl.f: Added pSKP and sSKP to tabulated phases.
      G. Helffrich/UB 20 Aug. 2005

   Makefile:  Tweak to prevent non-fatal build error messages and to install
   with default model in ttimes.
      G. Helffrich/UB 11 Jan. 2006

   Makefile:  Tweak to fix builds on fast machines -- some models skipped.  Fix
   install-all-models and behavior of simple 'install' with no previous models
   built.
      G. Helffrich/UB 3 Dec. 2006

   tptt.f:  New routine tpttsv to return velocities at source.
      G. Helffrich/UB 13 Oct. 2007

   Makefile.in, configure.ac, utils/install-sh, utils/version, ttimes.f:  Make
   configure-driven. (version 2)
      G. Helffrich/UB 21 Apr. 2008

   Makefile.in, configure.ac, utils/config.guess, utils/config.sub: Version 3
      G. Helffrich/UB 3 Jun. 2008
   1) Improve Mac installation defaults to be more intelligent about choosing
      the man page locations.
   2) Add gtt91 man page (not very useful); edit ttimes man page to be
      consistent about name of program.
   3) Add uninstall option (suggested by Arthur Snoke).
   4) Add range in km option (sugg. by Art Snoke) and terse option.
   5) Add take-off option (sugg. by Art Snoke).

   limits.com, setbrn.f, ttimes.f, configure.ac/configure:  Version 4
      G. Helffrich/UB 7 Jun. 2008
   1) Add PcS, sScS, pScS, sPcP, pPcP phases -- increase limits.inc to
      permit.
   2) Include model name in even terse output (suggested by Arthur Snoke).
   3) Eliminate odd configure complaint (reported by Arthur Snoke).
   4) Correct conversion factors for slowness in km; fix output labels.

   Makefile.in:  Version 5
      G. Helffrich/UB 10 Jun. 2008
   1) Create all directories in $(MANDIR) path (reported by Arthur Snoke).

   Makefile.in:  Version 6
      G. Helffrich/UB 21 Jan. 2010
   1) ttimes.1, ttimes91.1 - fix typos, document behavior.
      G. Helffrich/UB 21 Jan. 2010
   2) ttimes.f, libtau.f, libvax.f, setbrn.f, remodl.f:  Eliminate dasign,
      assign, retrns use; use Fortran constructs instead.  Add die() to
      libvax.f
   3) configure.ac, configure:  Add configure options to eliminate depth
      phases and add extra core phases.  Automatic generation of limits.inc
   4) tptt.f:  Document use of subroutine interface to tables better and the
      tpmod and phgen routines.

   Makefile.in:  Version 7
      G. Helffrich/UB 2 Feb. 2010
   1) libtau.f - enlarge temporary branch table, and check for small return
      value array.

   Makefile.in:  Version 8
      G. Helffrich/UB 15 June 2013
   1) libtau.f - lengthen msg variable.
   2) remodl.f, setbrn.f - Allow for planetary models that lack an inner core.
      Many hard-wired offsets into phase name tables and breakpoint tables
      were changed to positions derived from model characteristics.
   3) Add emss97a.f - Mars model of Sohl and Spohn (1997).

   Makefile.in:  Version 9
      G. Helffrich/ELSI 13 June 2017
   1) libtau.f - Sanity check whether tables file exists before trying to read
      it; report if not.
   2) remodl.f, setbrn.f - Allow for planetary models where the core is a low
      velocity zone for both P and S waves.
   3) Add emr11dw.f - Mars model of Rivoldini et al. (2011) with Dreibus and
      Wanke composition.

   Makefile.in:  Version 10
      G. Helffrich/ELSI 09 Apr. 2020
   1) libtau.f - TXTB: New externally callable routine to return slowness,
      range and travel time for specific branch.
   2) emr11dw.f, emss97a.f, emak135.f - Eliminate compilation niggles.
   3) tpttsub.3.in - New file to document subroutine interface to tau-p tables.
   4) ttimes.1.in - New file to document command line interface to tau-p tables.

   Makefile.in:  Version 11
      G. Helffrich/ELSI 17 Nov. 2020
   1) Add ek137 model by Kennett (2020)
   2) ttimes.1.in - Add ek137 to model list.
```
