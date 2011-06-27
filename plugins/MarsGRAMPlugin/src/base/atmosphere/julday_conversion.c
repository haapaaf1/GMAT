/* julday_conversion.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Subroutine */ int caltojul_(integer *iy, integer *im, integer *id, integer 
	*ihour, integer *imin, doublereal *sec, doublereal *xjd)
{
    static integer a, b;
    static doublereal d__;
    static integer m, y;

/*                                                                       CTOJ  2 */
/*     Compute Julian day (Real*8) by method of Meeus, Astronomical      CTOJ  3 */
/*       Algorithms, 2nd Edition, 1998, page 61. Inputs are year iY,     CTOJ  4 */
/*       month iM, day of month iD, and time of day in hours, minutes,   CTOJ  5 */
/*       and seconds (all integer except seconds).  Output is Real*8     CTOJ  6 */
/*       Julian day, xJD.                                                CTOJ  7 */
/*                                                                       CTOJ  8 */
    y = *iy;
    m = *im;
/* ...  Consider Jan or Feb as if months 13 and 14 of previous year       CTOJ 14 */
    if (*im <= 2) {
	y = *iy - 1;
	m = *im + 12;
    }
/* ...  Compute day of month plus fractional part                         CTOJ 19 */
    d__ = *id + *ihour / 24. + *imin / 1440. + *sec / 86400.;
    a = (integer) (y / 100.);
    b = 2 - a + (integer) (a / 4.);
/* ...  Compute Julian day with fractional part                           CTOJ 23 */
    *xjd = (integer) ((y + 4716) * 365.25) + (integer) ((m + 1) * 30.6001) + 
	    d__ + b - 1524.5;
    return 0;
} /* caltojul_ */

/* ---------------------------------------------------------------------- CTOJ 28 */
/* Subroutine */ int jultocal_(doublereal *jd, integer *year, integer *month, 
	integer *day, doublereal *dayfrac)
{
    /* Builtin functions */
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static integer a, b, c__, d__, e;
    static doublereal f;
    static integer alpha, z__;
    static doublereal jdp;

/* ...    Compute year, month, day of month, and fraction of day          JCAL  2 */
/*       from Julian day JD.  Algorithm from Meeus, Astronomical         JCAL  3 */
/*       Algorithms, 2nd edition, 1998, page 63                          JCAL  4 */
/* ...    Add 0.5 to Julian day                                           JCAL  8 */
    jdp = *jd + .5;
/* ...    Get integer part and fractional part of JD+0.5                  JCAL 10 */
    z__ = (integer) jdp;
    f = jdp - z__;
/* ...    Compute parameter A                                             JCAL 13 */
    if (z__ < 2299161) {
	a = z__;
    } else {
	alpha = (integer) ((z__ - 1867216.25) / 36524.25);
	a = z__ + 1 + alpha - (integer) (alpha / 4.);
    }
/* ...    Compute parameters B, C, D, and E                               JCAL 20 */
    b = a + 1524;
    c__ = (integer) ((b - 122.1) / 365.25);
    d__ = (integer) (c__ * 365.25);
    e = (integer) ((b - d__) / 30.6001);
/* ...    Get integer day of month and fractional day from parameters     JCAL 25 */
/*         B, D, E, and F                                                JCAL 26 */
    *dayfrac = b - d__ - (integer) (e * 30.6001) + f;
    *day = (integer) (*dayfrac);
    *dayfrac -= *day;
/* ...    Get month from parameter E                                      JCAL 30 */
    if (e < 2) {
	s_stop(" Bad month parameter E: too small", (ftnlen)33);
    } else if (e < 14) {
	*month = e - 1;
    } else if (e < 16) {
	*month = e - 13;
    } else {
	s_stop(" Bad month parameter E: too large", (ftnlen)33);
    }
/* ...    Get year from parameter C                                       JCAL 40 */
    if (*month > 2) {
	*year = c__ - 4716;
    } else {
	*year = c__ - 4715;
    }
    return 0;
} /* jultocal_ */

#ifdef __cplusplus
	}
#endif
