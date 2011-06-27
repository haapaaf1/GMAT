/* mgram_M05.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Table of constant values */

static doublereal c_b2 = 0.;

/* Subroutine */ int mgram_m05__(integer *intyear, integer *intmonth, integer 
	*intday, integer *inthour, integer *intmin, doublereal *rsec, 
	doublereal *rlat, doublereal *rlon, doublereal *rhgt, doublereal *
	temp, doublereal *pres, doublereal *dens, doublereal *denslo, 
	doublereal *denshi, doublereal *densp, doublereal *ewwind, doublereal 
	*ewpert, doublereal *nswind, doublereal *nspert, doublereal *vwpert, 
	doublereal *hrho, doublereal *hpres, doublereal *als, doublereal *sza,
	 doublereal *sunlat, doublereal *sunlon, doublereal *owlt, doublereal 
	*marsau, doublereal *tlocal, integer *isload, char *inputfl, ftnlen 
	inputfl_len)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static doublereal csec, fsec, clat, chgt;
    static integer iday;
    static doublereal flat, fhgt, clon, rhod, flon;
    static integer iert, iutc;
    static doublereal rhou, rhov, rhow, profnear;
    extern /* Subroutine */ int setup_m05__(doublereal *, doublereal *, 
	    doublereal *, integer *, integer *, integer *, integer *, integer 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, char *, 
	    integer *, integer *, doublereal *, integer *, integer *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *, 
	    ftnlen);
    static doublereal pertstep;
    static integer iustdout, i__, j;
    static doublereal fdlat, fdhgt, fdlon;
    static integer iyear, lonew, nprof, ihour;
    static doublereal dellat, delhgt, fdtime, dellon, corlim;
    static integer imonth, nmonte, maxnum, iulist, nr1, eof;
    static doublereal hgtasfc, deltime;
    static integer iupdate;
    static doublereal proffar, corlmin;
    extern /* Subroutine */ int datastep_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     integer *, doublereal *, integer *, integer *, doublereal *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *), randinit_m05__(integer *, 
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     integer *, integer *);
    static integer iminute;
    static doublereal day0, denstot;
    static integer numwave;

/*      DATA INPUTFL/'inputstd0.txt'/ */
    iustdout = 6;
    chgt = *rhgt;
    clat = *rlat;
    clon = *rlon;
    iyear = *intyear;
    imonth = *intmonth;
    iday = *intday;
    ihour = *inthour;
    iminute = *intmin;
    csec = *rsec;
/* ...  Setup_M05 information for start of run                            MGRM194 */
    setup_m05__(&chgt, &clat, &clon, &iyear, &imonth, &iday, &ihour, &iminute,
	     &csec, &day0, &rhod, &rhou, &rhov, &rhow, &delhgt, &dellat, &
	    dellon, &deltime, &maxnum, &nr1, &nmonte, &c_b2, &c_b2, &c_b2, &
	    lonew, inputfl, &iustdout, &iulist, &hgtasfc, &iert, &iutc, &
	    corlmin, &profnear, &proffar, &nprof, isload, (ftnlen)60);
/* ...  Save initial position, date, and position displacement values     MGRM200 */
    fhgt = chgt;
    flat = clat;
    flon = clon;
    fsec = csec;
    fdhgt = delhgt;
    fdlat = dellat;
    fdlon = dellon;
    fdtime = deltime;
/* ...  Initialize total perturbation step                                MGRM209 */
    pertstep = 0.;
    iupdate = 0;
/* ...  Step through number of Monte Carlo runs                           MGRM212 */
    i__1 = nmonte;
    for (j = 1; j <= i__1; ++j) {
/* ...  Initialize number for wave coefficients                           MGRM214 */
	numwave = 0;
/* ...  Re-initialize random number, position and time for each run       MGRM216 */
	if (j > 1) {
	    randinit_m05__(&j, &nr1, &rhod, &rhou, &rhov, &rhow, &iulist, &
		    iustdout);
	    chgt = fhgt;
	    clat = flat;
	    clon = flon;
	    csec = fsec;
	    delhgt = fdhgt;
	    dellat = fdlat;
	    dellon = fdlon;
	    deltime = fdtime;
/* ...      Re-initialize total perturbation step                         MGRM227 */
	    pertstep = 0.;
	    iupdate = 0;
	}
/* ...  Step through max Number of points for each Monte Carlo run        MGRM231 */
	i__2 = maxnum;
	for (i__ = 0; i__ <= i__2; ++i__) {
/*                                                                       MGRM233 */
	    datastep_m05__(&i__, &chgt, &clat, &clon, &csec, &day0, &rhod, &
		    rhou, &rhov, &rhow, &eof, &delhgt, &dellat, &dellon, &
		    deltime, temp, pres, denslo, dens, denshi, densp, ewwind, 
		    ewpert, nswind, nspert, vwpert, hrho, hpres, &c_b2, &c_b2,
		     &c_b2, &c_b2, &c_b2, &lonew, &corlim, &denstot, &numwave,
		     &hgtasfc, &iert, &iutc, &pertstep, &corlmin, &iupdate, 
		    als, sza, owlt, sunlat, sunlon, marsau, tlocal, &profnear,
		     &proffar, &nprof);
/*       Go to next Monte Carlo run if EOF=1                             MGRM283 */
	    if (eof == 1) {
		goto L910;
	    }
/* L900: */
	}
L910:
	;
    }
/*      STOP ' Normal Termination'                                        MGRM287 */
    return 0;
} /* mgram_m05__ */

#ifdef __cplusplus
	}
#endif
