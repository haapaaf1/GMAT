/* readalb.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Table of constant values */

static integer c__5 = 5;
static integer c__1 = 1;
static integer c__65884 = 65884;
static integer c__3 = 3;
static integer c__9 = 9;

/* Main program */ int CreateAlbFile()
{
    /* Format strings */
    static char fmt_110[] = "(a14,2f9.3)";

    /* System generated locals */
    integer i__1;
    olist o__1;

    /* Builtin functions */
    integer f_open(olist *), s_rsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_rsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(), 
	    s_wsue(cilist *), do_uio(integer *, char *, ftnlen), e_wsue(), 
	    s_wsle(cilist *), e_wsle();

    /* Local variables */
    static doublereal alat, alon;
    static integer ilat, jlon;
    static doublereal albnorth, albsouth, albedo[65884]	/* was [182][362] */, 
	    alb, alborth;
    static integer numread;
    static doublereal steplat, steplon;

    /* Fortran I/O blocks */
    static cilist io___6 = { 1, 21, 1, 0, 0 };
    static cilist io___14 = { 0, 6, 0, fmt_110, 0 };
    static cilist io___15 = { 0, 22, 0, 0, 0 };
    static cilist io___16 = { 0, 6, 0, 0, 0 };


    o__1.oerr = 0;
    o__1.ounit = 21;
    o__1.ofnmlen = 11;
    o__1.ofnm = "albedo1.txt";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 22;
    o__1.ofnmlen = 11;
    o__1.ofnm = "albedo1.bin";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    steplat = 1.;
    steplon = 1.;
    alborth = 0.;
    albsouth = 0.;
    numread = 0;
L50:
    i__1 = s_rsle(&io___6);
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&alat, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&alon, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&alb, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = e_rsle();
L100001:
    if (i__1 < 0) {
	goto L99;
    }
    if (i__1 > 0) {
	goto L50;
    }
    jlon = (integer) (alon / steplon + 1.);
    ilat = (integer) ((alat + 90.) / steplat + 1.);
    if (jlon < 1 || jlon > 360) {
	s_stop(" Bad lon", (ftnlen)8);
    }
    if (ilat < 1 || ilat > 180) {
	s_stop(" Bad lat", (ftnlen)8);
    }
    albedo[ilat + jlon * 182] = alb;
/* ...    Set albedo near 0 longitude                                     RALB 18 */
    if (jlon == 360) {
	albedo[ilat + 65702] = alb;
    }
    if (jlon == 1) {
	albedo[ilat] = alb;
    }
    ++numread;
    goto L50;
L99:
    for (jlon = 1; jlon <= 360; ++jlon) {
	albnorth += albedo[jlon * 182 + 180] / 360.;
	albsouth += albedo[jlon * 182 + 1] / 360.;
/* L100: */
    }
    s_wsfe(&io___14);
    do_fio(&c__1, " albedo S,N = ", (ftnlen)14);
    do_fio(&c__1, (char *)&albsouth, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&albnorth, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* ...    Set polar (90N and 90 S) albedo values                          RALB 34 */
    for (jlon = 0; jlon <= 361; ++jlon) {
	albedo[jlon * 182] = albsouth;
	albedo[jlon * 182 + 181] = albnorth;
/* L115: */
    }
    s_wsue(&io___15);
    do_uio(&c__65884, (char *)&albedo[0], (ftnlen)sizeof(doublereal));
    e_wsue();
    s_wsle(&io___16);
    do_lio(&c__3, &c__1, (char *)&numread, (ftnlen)sizeof(integer));
    do_lio(&c__9, &c__1, " data points processed", (ftnlen)22);
    e_wsle();
    return 0;
} /* MAIN__ */

#ifdef __cplusplus
	}
#endif
