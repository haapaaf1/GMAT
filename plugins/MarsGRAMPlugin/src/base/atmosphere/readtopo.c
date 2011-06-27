/* readtopo.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Table of constant values */

static integer c__1 = 1;
static integer c_b30 = 261364;
static integer c__3 = 3;
static integer c__9 = 9;

/* Main program */ int CreateMOLADataFile()
{
    /* Format strings */
    static char fmt_10[] = "(2f8.1,2f12.2,f10.2,i6)";
    static char fmt_110[] = "(a24,f13.5,f11.5)";

    /* System generated locals */
    integer i__1;
    olist o__1;

    /* Builtin functions */
    integer f_open(olist *), s_rsfe(cilist *), do_fio(integer *, char *, 
	    ftnlen), e_rsfe();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_wsfe(cilist *), e_wsfe(), s_wsue(cilist *), do_uio(integer *, 
	    char *, ftnlen), e_wsue(), s_wsle(cilist *), do_lio(integer *, 
	    integer *, char *, ftnlen), e_wsle();

    /* Local variables */
    static doublereal alat, prad;
    static integer ilat;
    static doublereal alon;
    static integer jlon;
    static doublereal topo, topomola[261364]	/* was [362][722] */, 
	    areonorth, areosouth, toponorth, toposouth, areoid, areorad[
	    261364]	/* was [362][722] */;
    static integer num, numread;
    static doublereal steplat, steplon;

    /* Fortran I/O blocks */
    static cilist io___8 = { 1, 21, 1, fmt_10, 0 };
    static cilist io___19 = { 0, 6, 0, fmt_110, 0 };
    static cilist io___20 = { 0, 6, 0, fmt_110, 0 };
    static cilist io___21 = { 0, 22, 0, 0, 0 };
    static cilist io___22 = { 0, 6, 0, 0, 0 };


/* ...    Parameter (nlat = 181)                                          RTOP  5 */
/* ...    Parameter (nlon = 361)                                          RTOP  6 */
    o__1.oerr = 0;
    o__1.ounit = 21;
    o__1.ofnmlen = 12;
    o__1.ofnm = "MOLATOPH.TXT";
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
    o__1.oerr = 0;
    o__1.ounit = 22;
    o__1.ofnmlen = 12;
    o__1.ofnm = "molatoph.bin";
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    steplat = .5;
    steplon = .5;
    areonorth = 0.;
    areosouth = 0.;
    toponorth = 0.;
    toposouth = 0.;
    numread = 0;
/* L10: */
L50:
    i__1 = s_rsfe(&io___8);
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_fio(&c__1, (char *)&alon, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_fio(&c__1, (char *)&alat, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_fio(&c__1, (char *)&prad, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_fio(&c__1, (char *)&areoid, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_fio(&c__1, (char *)&topo, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = do_fio(&c__1, (char *)&num, (ftnlen)sizeof(integer));
    if (i__1 != 0) {
	goto L100001;
    }
    i__1 = e_rsfe();
L100001:
    if (i__1 < 0) {
	goto L99;
    }
    if (i__1 > 0) {
	goto L50;
    }
/* ...    Convert to West longitude                                       RTOP 21 */
    alon = 360. - alon;
    jlon = (integer) (alon / steplon + 1.);
    ilat = (integer) ((alat + 90.) / steplat + 1.);
    if (jlon < 1 || jlon > 720) {
	s_stop(" Bad lon", (ftnlen)8);
    }
    if (ilat < 1 || ilat > 360) {
	s_stop(" Bad lat", (ftnlen)8);
    }
    areorad[ilat + jlon * 362] = areoid / 1e3;
    topomola[ilat + jlon * 362] = topo / 1e3;
/* ...    Set areoid and topo height values near 0 longitude              RTOP 29 */
    if (jlon == 720) {
	areorad[ilat] = areoid / 1e3;
	topomola[ilat] = topo / 1e3;
    }
    if (jlon == 1) {
	areorad[ilat + 261002] = areoid / 1e3;
	topomola[ilat + 261002] = topo / 1e3;
    }
    ++numread;
    goto L50;
L99:
    for (jlon = 1; jlon <= 720; ++jlon) {
	areonorth += areorad[jlon * 362 + 360] / 720.;
	areosouth += areorad[jlon * 362 + 1] / 720.;
	toponorth += topomola[jlon * 362 + 360] / 720.;
	toposouth += topomola[jlon * 362 + 1] / 720.;
/* L100: */
    }
    s_wsfe(&io___19);
    do_fio(&c__1, " areoid, topoheight S =", (ftnlen)23);
    do_fio(&c__1, (char *)&areosouth, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&toposouth, (ftnlen)sizeof(doublereal));
    e_wsfe();
    s_wsfe(&io___20);
    do_fio(&c__1, " areoid, topoheight N =", (ftnlen)23);
    do_fio(&c__1, (char *)&areonorth, (ftnlen)sizeof(doublereal));
    do_fio(&c__1, (char *)&toponorth, (ftnlen)sizeof(doublereal));
    e_wsfe();
/* ...    Set polar areoid and topo height values                         RTOP 49 */
    for (jlon = 0; jlon <= 721; ++jlon) {
	areorad[jlon * 362] = areosouth;
	topomola[jlon * 362] = toposouth;
	areorad[jlon * 362 + 361] = areonorth;
	topomola[jlon * 362 + 361] = toponorth;
/* L115: */
    }
    s_wsue(&io___21);
    do_uio(&c_b30, (char *)&areorad[0], (ftnlen)sizeof(doublereal));
    do_uio(&c_b30, (char *)&topomola[0], (ftnlen)sizeof(doublereal));
    e_wsue();
    s_wsle(&io___22);
    do_lio(&c__3, &c__1, (char *)&numread, (ftnlen)sizeof(integer));
    do_lio(&c__9, &c__1, " data points processed", (ftnlen)22);
    e_wsle();
    return 0;
} /* MAIN__ */

#ifdef __cplusplus
	}
#endif
