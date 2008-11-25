/* irit13ts.f -- translated by f2c (version 19990311).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;
static integer c__4 = 4;
static real c_b20 = (float)100.;
static real c_b21 = (float)1e3;

/* Test program for IRIT13 program. You can also get the F peak */
/* density and heights with the COMMON block */
/* 	common/block1/hmf2,xnmf2,hmf1 */
/* IRIS13 contains other COMMON blocks that may be helpful. */

/* Changes: */
/* 	9/16/98 jf now input in IRIT13 */

/* Main program */ MAIN__()
{
    /* Initialized data */

    static logical jf[17] = { TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,
	    TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_,TRUE_ };

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(), s_rsle(cilist *), e_rsle();
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static real tecb;
    static integer jmag, mmdd;
    static real tect, hour;
    static integer i__, j;
    extern /* Subroutine */ int irit13_(real *, real *, integer *, logical *, 
	    integer *, integer *, real *, real *, real *, real *, real *, 
	    real *);
    static integer iy;
    static real tec, xla, xlo;

    /* Fortran I/O blocks */
    static cilist io___2 = { 0, 6, 0, 0, 0 };
    static cilist io___3 = { 0, 5, 0, 0, 0 };
    static cilist io___8 = { 0, 6, 0, 0, 0 };
    static cilist io___16 = { 0, 6, 0, 0, 0 };



/*  select various options and choices for IRI-95 */

    jf[1] = FALSE_;
/* no temperatures */
    jf[2] = FALSE_;
/* test   jf(4)=.false.           ! Gulyaeva-B0 */
/* no ion composition */
    jf[4] = FALSE_;
/*       jf(12)=.false.          ! konsol output to file (unit=12) */
/* 	type*,'iyear(yyyy),mmdd,hour(UT+25),jmag' */
/* URSI-88 for foF2 */
    s_wsle(&io___2);
    do_lio(&c__9, &c__1, "iyear(yyyy),mmdd,hour(UT+25),jmag", (ftnlen)33);
    e_wsle();
    s_rsle(&io___3);
    do_lio(&c__3, &c__1, (char *)&iy, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&mmdd, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&hour, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&jmag, (ftnlen)sizeof(integer));
    e_rsle();
/* 	type*,'latitude, longitude, TEC/m-2, TEC-bottomside/%,', */
    s_wsle(&io___8);
    do_lio(&c__9, &c__1, "latitude, longitude, TEC/m-2, TEC-bottomside/%,", (
	    ftnlen)47);
    do_lio(&c__9, &c__1, " TEC-topside/%", (ftnlen)14);
    e_wsle();
    for (i__ = 1; i__ <= 5; ++i__) {
	xla = (i__ - 1) * (float)30. - (float)60.;
	for (j = 1; j <= 4; ++j) {
	    xlo = (j - 1) * (float)90.;
	    irit13_(&xla, &xlo, &jmag, jf, &iy, &mmdd, &hour, &c_b20, &c_b21, 
		    &tec, &tecb, &tect);
	    s_wsle(&io___16);
	    do_lio(&c__4, &c__1, (char *)&xla, (ftnlen)sizeof(real));
	    do_lio(&c__4, &c__1, (char *)&xlo, (ftnlen)sizeof(real));
	    do_lio(&c__4, &c__1, (char *)&tec, (ftnlen)sizeof(real));
	    do_lio(&c__4, &c__1, (char *)&tecb, (ftnlen)sizeof(real));
	    do_lio(&c__4, &c__1, (char *)&tect, (ftnlen)sizeof(real));
	    e_wsle();
/* L2: */
	}
/* L1: */
    }
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */

#ifdef __cplusplus
	}
#endif
