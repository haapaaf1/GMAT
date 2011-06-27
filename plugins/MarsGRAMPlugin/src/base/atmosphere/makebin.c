/* makebin.f -- translated by f2c (version 20000704).
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
static integer c__4 = 4;
static integer c__3 = 3;
static integer c__5 = 5;
static integer c_b530 = 147600;

/* ------ Program makebin ------------------------------------------------MKBN  1 */
/*                                                                       MKBN  2 */
/* ...    Makes binary files from ASCII versions of NASA Ames Mars Global MKBN  3 */
/*       Circulation Model (MGCM) and University of Michigan Mars        MKBN  4 */
/*       Thermospheric Global Circulation Model (MTGCM) data, for fast   MKBN  5 */
/*       reading on user system                                          MKBN  6 */
/* Main program */ int CreateMGRAMBinaryFiles()
{
    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();

    /* Local variables */
    extern /* Subroutine */ int mtgcmbin_(char *, ftnlen), mgcmbin_(char *, 
	    ftnlen), surfbin_(char *, ftnlen), dustbin_(char *, ftnlen);

    /* Fortran I/O blocks */
    static cilist io___1 = { 0, 6, 0, 0, 0 };
    static cilist io___2 = { 0, 6, 0, 0, 0 };
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 6, 0, 0, 0 };


    s_wsle(&io___1);
    do_lio(&c__9, &c__1, " Reading MGCM surface data", (ftnlen)26);
    e_wsle();
    surfbin_("1", (ftnlen)1);
    s_wsle(&io___2);
    do_lio(&c__9, &c__1, " Reading MGCM -5 to 80 km data", (ftnlen)30);
    e_wsle();
    mgcmbin_("1", (ftnlen)1);
    s_wsle(&io___3);
    do_lio(&c__9, &c__1, " Reading MTGCM 80 to 240 km data", (ftnlen)32);
    e_wsle();
    mtgcmbin_("1", (ftnlen)1);
    s_wsle(&io___4);
    do_lio(&c__9, &c__1, " Reading TES dust data", (ftnlen)22);
    e_wsle();
    dustbin_("1", (ftnlen)1);
    return 0;
} /* MAIN__ */

/* -----------------------------------------------------------------------MKBN 18 */
/* Subroutine */ int surfbin_(char *version, ftnlen version_len)
{
    /* Initialized data */

    static char dust[2*5+1] = "031030y1y2";

    /* Format strings */
    static char fmt_10[] = "(a)";

    /* System generated locals */
    address a__1[4];
    integer i__1, i__2[4], i__3, i__4, i__5;
    char ch__1[12];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_wsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_wsle(), s_rsfe(cilist *), do_fio(integer *, 
	    char *, ftnlen), e_rsfe(), s_rsle(cilist *), e_rsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_wsue(cilist *), do_uio(integer *, char *, ftnlen), e_wsue(), 
	    f_clos(cllist *);

    /* Local variables */
    static integer lsea, jlon;
    static doublereal xlat, ylat;
    static integer ilatstep, i__, k, m;
    static char dummy[2];
    static integer ls, lastls, lat, ils, lon, ier0, lat1, lat2;
    static doublereal tsa0, tsa1;
    static integer lon1, lon2;
    static doublereal tsa2, usa0, usa1, usa2, vsa0, vsa1, vsa2, tsp1, tsp2, 
	    usp1, usp2, vsp1, vsp2;

    /* Fortran I/O blocks */
    static cilist io___10 = { 0, 6, 0, 0, 0 };
    static cilist io___11 = { 0, 23, 0, fmt_10, 0 };
    static cilist io___13 = { 0, 24, 0, fmt_10, 0 };
    static cilist io___14 = { 0, 25, 0, fmt_10, 0 };
    static cilist io___26 = { 0, 0, 1, 0, 0 };
    static cilist io___35 = { 0, 0, 1, 0, 0 };
    static cilist io___46 = { 0, 0, 0, 0, 0 };
    static cilist io___47 = { 0, 0, 0, 0, 0 };
    static cilist io___48 = { 0, 6, 0, 0, 0 };
    static cilist io___49 = { 0, 6, 0, 0, 0 };


/* ...    Reads ASCII version MGCM surface data and writes binary version SRFB  2 */
/* ...    Set values for ndust=number of dust optical depths, nbl=number  SRFB  6 */
/*       of boundary layer levels, nlat=number of MGCM latitudes, and    SRFB  7 */
/*       nlon= number of MGCM longitudes                                 SRFB  8 */
/* ...    Set parameter for "form=" in binary file Open statement         SRFB 13 */
/* ...    Set character values of dust optical depths used in file names  SRFB 16 */
/* ...    Initialize last Ls processed to zero                            SRFB 18 */
    lastls = 0;
/* ...    Compute latitude step size                                      SRFB 20 */
    ilatstep = 75;
/* ...    Step through all dust optical depths                            SRFB 22 */
    for (m = 1; m <= 5; ++m) {
/* ...      Open ASCII version and binary version data files at 5m and    SRFB 24 */
/*         30m levels                                                    SRFB 25 */
	ier0 = 0;
	o__1.oerr = 1;
	o__1.ounit = 23;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "sfc00";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".txt";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L1;
	}
	o__1.oerr = 1;
	o__1.ounit = 26;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "sfc00";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".bin";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
	goto L2;
L1:
	s_wsle(&io___10);
	do_lio(&c__9, &c__1, " sfc00 files not available for optical depth= ",
		 (ftnlen)46);
	do_lio(&c__9, &c__1, dust + (m - 1 << 1), (ftnlen)2);
	e_wsle();
	ier0 = 1;
L2:
	o__1.oerr = 1;
	o__1.ounit = 24;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "sfc05";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".txt";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
	o__1.oerr = 1;
	o__1.ounit = 25;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "sfc30";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".txt";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
	o__1.oerr = 1;
	o__1.ounit = 27;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "sfc05";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".bin";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
	o__1.oerr = 1;
	o__1.ounit = 28;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "sfc30";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".bin";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
/* ...      Read (and ignore) the header line from the ASCII files        SRFB 43 */
	if (ier0 == 0) {
	    s_rsfe(&io___11);
	    do_fio(&c__1, dummy, (ftnlen)2);
	    e_rsfe();
	}
	s_rsfe(&io___13);
	do_fio(&c__1, dummy, (ftnlen)2);
	e_rsfe();
	s_rsfe(&io___14);
	do_fio(&c__1, dummy, (ftnlen)2);
	e_rsfe();
/* ...      Step through all Ls values                                    SRFB 48 */
	for (lsea = 30; lsea <= 360; lsea += 30) {
	    ls = lsea / 30;
/* ...      Step through all latitudes                                    SRFB 51 */
	    lat1 = -900;
	    lat2 = 900;
	    i__1 = lat2;
	    i__3 = ilatstep;
	    for (lat = lat1; i__3 < 0 ? lat >= i__1 : lat <= i__1; lat += 
		    i__3) {
		xlat = lat / 10.;
		i__ = (lat + 900) / ilatstep + 1;
/* ...      Step through all boundary layer levels                        SRFB 57 */
		for (k = 1; k <= 3; ++k) {
/* ...      Step through all longitudes                                   SRFB 59 */
		    lon1 = 360;
		    lon2 = 9;
		    i__4 = lon2;
		    for (lon = lon1; lon >= i__4; lon += -9) {
/* ...        Read ASCII version tide coefficient amplitudes and phases   SRFB 63 */
/*           for temperature, EW wind, and NS wind                       SRFB 64 */
			if (k == 1) {
			    if (ier0 != 0) {
				goto L50;
			    }
			    io___26.ciunit = k + 22;
			    i__5 = s_rsle(&io___26);
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)
				    sizeof(integer));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&ylat, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__3, &c__1, (char *)&jlon, (
				    ftnlen)sizeof(integer));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsa0, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsa1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsp1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsa2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsp2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = e_rsle();
			    if (i__5 != 0) {
				goto L60;
			    }
			} else {
			    io___35.ciunit = k + 22;
			    i__5 = s_rsle(&io___35);
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)
				    sizeof(integer));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&ylat, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__3, &c__1, (char *)&jlon, (
				    ftnlen)sizeof(integer));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsa0, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsa1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsp1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsa2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&tsp2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&usa0, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&usa1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&usp1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&usa2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&usp2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&vsa0, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&vsa1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&vsp1, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&vsa2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = do_lio(&c__5, &c__1, (char *)&vsp2, (
				    ftnlen)sizeof(doublereal));
			    if (i__5 != 0) {
				goto L60;
			    }
			    i__5 = e_rsle();
			    if (i__5 != 0) {
				goto L60;
			    }
			}
/* ...        Reset value of last Ls processed                            SRFB 72 */
			lastls = ils;
			if (ils != lsea) {
			    s_stop(" Bad surface Ls", (ftnlen)15);
			}
			if (ylat != xlat) {
			    s_stop(" Bad surface Latitude", (ftnlen)21);
			}
			if (jlon != lon) {
			    s_stop(" Bad surface Longitude", (ftnlen)22);
			}
/* ...        Write binary version tide amplitudes and phases for         SRFB 77 */
/*           temperature, EW wind, and NS wind                           SRFB 78 */
			if (k == 1) {
			    io___46.ciunit = k + 25;
			    s_wsue(&io___46);
			    do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				    integer));
			    do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&jlon, (ftnlen)sizeof(
				    integer));
			    do_uio(&c__1, (char *)&tsa0, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsa1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsp1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsa2, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsp2, (ftnlen)sizeof(
				    doublereal));
			    e_wsue();
			} else {
			    io___47.ciunit = k + 25;
			    s_wsue(&io___47);
			    do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				    integer));
			    do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&jlon, (ftnlen)sizeof(
				    integer));
			    do_uio(&c__1, (char *)&tsa0, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsa1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsp1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsa2, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&tsp2, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&usa0, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&usa1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&usp1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&usa2, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&usp2, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&vsa0, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&vsa1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&vsp1, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&vsa2, (ftnlen)sizeof(
				    doublereal));
			    do_uio(&c__1, (char *)&vsp2, (ftnlen)sizeof(
				    doublereal));
			    e_wsue();
			}
L50:
			;
		    }
/* L51: */
		}
/* L52: */
	    }
/* L53: */
	}
	goto L70;
/* ...      Write termination message if premature EOF encountered        SRFB 90 */
L60:
	s_wsle(&io___48);
	do_lio(&c__9, &c__1, " End of surface file at Ls =", (ftnlen)28);
	do_lio(&c__3, &c__1, (char *)&lastls, (ftnlen)sizeof(integer));
	e_wsle();
/* ...      Close all MGCM surface data input files                       SRFB 92 */
L70:
	for (i__ = 23; i__ <= 28; ++i__) {
	    if (i__ != 23 || ier0 == 0) {
		cl__1.cerr = 0;
		cl__1.cunit = i__;
		cl__1.csta = 0;
		f_clos(&cl__1);
	    }
/* L80: */
	}
	goto L100;
/* ...    Write warning message in case all dust optical depth values     SRFB 97 */
/*       are not available for processing                                SRFB 98 */
L99:
	s_wsle(&io___49);
	do_lio(&c__9, &c__1, " Files not available for dust optical depth= ", 
		(ftnlen)45);
	do_lio(&c__9, &c__1, dust + (m - 1 << 1), (ftnlen)2);
	e_wsle();
L100:
	;
    }
    return 0;
} /* surfbin_ */

/* -----------------------------------------------------------------------SRFB104 */
/* Subroutine */ int mgcmbin_(char *version, ftnlen version_len)
{
    /* Initialized data */

    static char dust[2*5+1] = "031030y1y2";

    /* Format strings */
    static char fmt_10[] = "(a)";

    /* System generated locals */
    address a__1[4];
    integer i__1, i__2[4], i__3, i__4;
    char ch__1[12], ch__2[11];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsfe(cilist *), do_fio(integer *, char *, 
	    ftnlen), e_rsfe(), s_rsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_rsle(), s_wsue(cilist *), do_uio(integer *, 
	    char *, ftnlen), e_wsue(), s_wsle(cilist *), e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer f_clos(cllist *);

    /* Local variables */
    static doublereal uzp1, uzp2, vzp1, vzp2;
    static integer lsea, ihgt, khgt, nhgt;
    static doublereal xlat, ylat;
    static integer ilatstep, i__, k, m;
    static char dummy[2];
    static integer ls, lastls, lat, ils;
    static doublereal dza0, dza1, dza2;
    static integer lat1, lat2;
    static doublereal pza0, pza1, pza2, tza0, tza1, tza2, dzp1, dzp2, uza0, 
	    uza1, uza2, vza0, vza1, vza2, pzp1, pzp2, tzp1, tzp2;

    /* Fortran I/O blocks */
    static cilist io___54 = { 0, 22, 0, fmt_10, 0 };
    static cilist io___56 = { 0, 23, 0, fmt_10, 0 };
    static cilist io___66 = { 0, 22, 1, 0, 0 };
    static cilist io___81 = { 0, 24, 0, 0, 0 };
    static cilist io___82 = { 0, 22, 1, 0, 0 };
    static cilist io___87 = { 0, 24, 0, 0, 0 };
    static cilist io___88 = { 0, 6, 0, 0, 0 };
    static cilist io___90 = { 0, 23, 1, 0, 0 };
    static cilist io___101 = { 0, 25, 0, 0, 0 };
    static cilist io___102 = { 0, 6, 0, 0, 0 };
    static cilist io___103 = { 0, 6, 0, 0, 0 };


/* ...    Reads ASCII version MGCM 0-80 km data and writes binary version GCMB  2 */
/* ...    Set values for ndust=number of dust optical depths, nhgt=number GCMB  6 */
/*       of MGCM height levels, and nlat=number of MGCM latitudes        GCMB  7 */
/* ...    Set parameter for "form=" in binary file Open statement         GCMB 10 */
/* ...    Set character values of dust optical depths used in file names  GCMB 13 */
/* ...    Initialize last Ls processed to zero                            GCMB 15 */
    lastls = 0;
/* ...    Compute latitude step size                                      GCMB 17 */
    ilatstep = 75;
/* ...    Step through all dust optical depths                            GCMB 19 */
    for (m = 1; m <= 5; ++m) {
/* ...      Open ASCII version and binary version data files for temp-    GCMB 21 */
/*         erature, density, pressure and for EW and NS wind components  GCMB 22 */
	o__1.oerr = 1;
	o__1.ounit = 22;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "tpdlo";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".txt";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
	o__1.oerr = 1;
	o__1.ounit = 23;
	o__1.ofnmlen = 11;
/* Writing concatenation */
	i__2[0] = 4, a__1[0] = "uvlo";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".txt";
	s_cat(ch__2, a__1, i__2, &c__4, (ftnlen)11);
	o__1.ofnm = ch__2;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
	o__1.oerr = 1;
	o__1.ounit = 24;
	o__1.ofnmlen = 12;
/* Writing concatenation */
	i__2[0] = 5, a__1[0] = "tpdlo";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".bin";
	s_cat(ch__1, a__1, i__2, &c__4, (ftnlen)12);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
	o__1.oerr = 1;
	o__1.ounit = 25;
	o__1.ofnmlen = 11;
/* Writing concatenation */
	i__2[0] = 4, a__1[0] = "uvlo";
	i__2[1] = 2, a__1[1] = dust + (m - 1 << 1);
	i__2[2] = 1, a__1[2] = version;
	i__2[3] = 4, a__1[3] = ".bin";
	s_cat(ch__2, a__1, i__2, &c__4, (ftnlen)11);
	o__1.ofnm = ch__2;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L99;
	}
/* ...      Read (and ignore) the header line from the ASCII files        GCMB 31 */
	s_rsfe(&io___54);
	do_fio(&c__1, dummy, (ftnlen)2);
	e_rsfe();
	s_rsfe(&io___56);
	do_fio(&c__1, dummy, (ftnlen)2);
	e_rsfe();
/* ...      Step through all Ls values                                    GCMB 35 */
	for (lsea = 30; lsea <= 360; lsea += 30) {
	    ls = lsea / 30;
/* ...      Step through all latitudes                                    GCMB 38 */
	    lat1 = -900;
	    lat2 = 900;
	    i__1 = lat2;
	    i__3 = ilatstep;
	    for (lat = lat1; i__3 < 0 ? lat >= i__1 : lat <= i__1; lat += 
		    i__3) {
		xlat = lat / 10.;
		i__ = (lat + 900) / ilatstep + 1;
/* ...      Step through all heights                                      GCMB 44 */
		nhgt = 17;
		if (m > 3) {
		    nhgt = 30;
		}
		for (k = nhgt; k >= 1; --k) {
/* ...        Read ASCII version tide coefficient amplitudes and phases   GCMB 48 */
/*           for temperature, pressure, and density                      GCMB 49 */
		    if (m <= 3) {
			i__4 = s_rsle(&io___66);
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = e_rsle();
			if (i__4 != 0) {
			    goto L55;
			}
/* ...          Write binary version tide amplitudes and phases for       GCMB 53 */
/*             temperature, pressure, and density                        GCMB 54 */
			s_wsue(&io___81);
			do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tzp2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pzp2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dza0, (ftnlen)sizeof(
				doublereal));
			e_wsue();
		    } else {
			i__4 = s_rsle(&io___82);
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = e_rsle();
			if (i__4 != 0) {
			    goto L55;
			}
/* ...          Write binary version tide amplitudes and phases for       GCMB 60 */
/*             temperature, pressure, and density                        GCMB 61 */
			s_wsue(&io___87);
			do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tzp2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dzp2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pza0, (ftnlen)sizeof(
				doublereal));
			e_wsue();
		    }
		    if (ils != lsea) {
			s_wsle(&io___88);
			do_lio(&c__3, &c__1, (char *)&lsea, (ftnlen)sizeof(
				integer));
			do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)sizeof(
				integer));
			do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)sizeof(
				integer));
			do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			e_wsle();
			s_stop(" Bad tpd Ls", (ftnlen)11);
		    }
		    khgt = (k - 1) * 5;
		    if (m > 3) {
			khgt = (k - 14) * 5;
			if (k < 16) {
			    khgt = k - 6;
			}
		    }
		    if (ihgt != khgt) {
			s_stop(" Bad tpd Height", (ftnlen)15);
		    }
		    if (ylat != xlat) {
			s_stop(" Bad tpd Latitude", (ftnlen)17);
		    }
/* ...        Read ASCII version tide coefficient amplitudes and phases   GCMB 76 */
/*           for EW wind and NS wind components                          GCMB 77 */
		    i__4 = s_rsle(&io___90);
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)sizeof(
			    integer));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)sizeof(
			    integer));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&uza0, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&uza1, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&uzp1, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&uza2, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&uzp2, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&vza0, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&vza1, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&vzp1, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&vza2, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = do_lio(&c__5, &c__1, (char *)&vzp2, (ftnlen)sizeof(
			    doublereal));
		    if (i__4 != 0) {
			goto L55;
		    }
		    i__4 = e_rsle();
		    if (i__4 != 0) {
			goto L55;
		    }
/* ...        Reset value of last Ls processed                            GCMB 80 */
		    lastls = ils;
/* ...        Write binary version tide amplitudes and phases for         GCMB 82 */
/*           EW wind and NS wind components                              GCMB 83 */
		    s_wsue(&io___101);
		    do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(integer));
		    do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(integer));
		    do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&uza0, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&uza1, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&uzp1, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&uza2, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&uzp2, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&vza0, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&vza1, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&vzp1, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&vza2, (ftnlen)sizeof(doublereal));
		    do_uio(&c__1, (char *)&vzp2, (ftnlen)sizeof(doublereal));
		    e_wsue();
		    if (ils != lsea) {
			s_stop(" Bad uv Ls", (ftnlen)10);
		    }
		    if (ihgt != khgt) {
			s_stop(" Bad uv Height", (ftnlen)14);
		    }
		    if (ylat != xlat) {
			s_stop(" Bad uv Latitude", (ftnlen)16);
		    }
/* L50: */
		}
/* L51: */
	    }
/* L52: */
	}
	goto L57;
/* ...      Write termination message if premature EOF encountered        GCMB 93 */
L55:
	s_wsle(&io___102);
	do_lio(&c__9, &c__1, " End of file on 0-80 km at Ls=", (ftnlen)30);
	do_lio(&c__3, &c__1, (char *)&lastls, (ftnlen)sizeof(integer));
	e_wsle();
/* ...      Close all MGCM 0-80 km data input files                       GCMB 95 */
L57:
	for (i__ = 22; i__ <= 25; ++i__) {
	    cl__1.cerr = 0;
	    cl__1.cunit = i__;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
/* L60: */
	}
	goto L100;
/* ...    Write warning message in case all dust optical depth values     GCMB100 */
/*       are not available for processing                                GCMB101 */
L99:
	s_wsle(&io___103);
	do_lio(&c__9, &c__1, " Files not available for dust optical depth=", (
		ftnlen)44);
	do_lio(&c__9, &c__1, dust + (m - 1 << 1), (ftnlen)2);
	e_wsle();
L100:
	;
    }
    return 0;
} /* mgcmbin_ */

/* -----------------------------------------------------------------------GCMB107 */
/* Subroutine */ int mtgcmbin_(char *version, ftnlen version_len)
{
    /* Initialized data */

    static char dust[2*5+1] = "031030y1y2";
    static char solact[2*3+1] = "lsmshs";

    /* Format strings */
    static char fmt_10[] = "(a)";

    /* System generated locals */
    address a__1[5];
    integer i__1, i__2, i__3[5], i__4;
    char ch__1[12], ch__2[11];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsfe(cilist *), do_fio(integer *, char *, 
	    ftnlen), e_rsfe(), s_rsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_rsle(), s_wsue(cilist *), do_uio(integer *, 
	    char *, ftnlen), e_wsue(), s_wsle(cilist *), e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer f_clos(cllist *);

    /* Local variables */
    static doublereal uzp1, uzp2, vzp1, vzp2;
    static integer lsea, ilat, ihgt;
    static doublereal ylat;
    static integer i__, k, m, n, nhgtt;
    static char dummy[2];
    static integer ls, lastls, nf10, ils;
    static doublereal dza0, dza1, dza2, pza0, pza1, pza2, tza0, tza1, tza2, 
	    dzp1, dzp2, uza0, uza1, uza2, vza0, vza1, vza2, pzp1, pzp2, tzp1, 
	    tzp2;

    /* Fortran I/O blocks */
    static cilist io___110 = { 0, 22, 0, fmt_10, 0 };
    static cilist io___112 = { 0, 23, 0, fmt_10, 0 };
    static cilist io___118 = { 0, 22, 1, 0, 0 };
    static cilist io___137 = { 0, 24, 0, 0, 0 };
    static cilist io___138 = { 0, 6, 0, 0, 0 };
    static cilist io___139 = { 0, 6, 0, 0, 0 };
    static cilist io___140 = { 0, 23, 1, 0, 0 };
    static cilist io___151 = { 0, 25, 0, 0, 0 };
    static cilist io___152 = { 0, 6, 0, 0, 0 };
    static cilist io___154 = { 0, 6, 0, 0, 0 };


/* ...    Reads ASCII version MTGCM 80-240 km data and writes binary      TGMB  2 */
/*       version                                                         TGMB  3 */
/* ...    Set values for ndust=number of dust optical depths, nhgtt=      TGMB  7 */
/*       number of MTGCM height levels, nlatt=number of MTGCM, and       TGMB  8 */
/*       nf10=number of solar activity levels (Earth F10.7)              TGMB  9 */
/*       latitudes                                                       TGMB 10 */
/* ...    Set parameter for "form=" in binary file Open statement         TGMB 13 */
/* ...    Set character values of dust optical depths used in file names  TGMB 16 */
/* ...    Set characters for solar activity values used in file names     TGMB 18 */
/* ...    Initialize last Ls processed to zero                            TGMB 20 */
    lastls = 0;
/* ...    Step through all dust optical depths and solar activities       TGMB 22 */
    for (m = 1; m <= 5; ++m) {
	nf10 = 2;
	if (m > 3) {
	    nf10 = 3;
	}
	i__1 = nf10;
	for (n = 1; n <= i__1; ++n) {
/* ...      Open ASCII version and binary version data files for temp-    TGMB 27 */
/*         erature, density, pressure and for EW and NS wind components  TGMB 28 */
	    o__1.oerr = 1;
	    o__1.ounit = 22;
	    o__1.ofnmlen = 12;
/* Writing concatenation */
	    i__3[0] = 3, a__1[0] = "tpd";
	    i__3[1] = 2, a__1[1] = solact + (n - 1 << 1);
	    i__3[2] = 2, a__1[2] = dust + (m - 1 << 1);
	    i__3[3] = 1, a__1[3] = version;
	    i__3[4] = 4, a__1[4] = ".txt";
	    s_cat(ch__1, a__1, i__3, &c__5, (ftnlen)12);
	    o__1.ofnm = ch__1;
	    o__1.orl = 0;
	    o__1.osta = "old";
	    o__1.oacc = 0;
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    i__2 = f_open(&o__1);
	    if (i__2 != 0) {
		goto L99;
	    }
	    o__1.oerr = 1;
	    o__1.ounit = 23;
	    o__1.ofnmlen = 11;
/* Writing concatenation */
	    i__3[0] = 2, a__1[0] = "uv";
	    i__3[1] = 2, a__1[1] = solact + (n - 1 << 1);
	    i__3[2] = 2, a__1[2] = dust + (m - 1 << 1);
	    i__3[3] = 1, a__1[3] = version;
	    i__3[4] = 4, a__1[4] = ".txt";
	    s_cat(ch__2, a__1, i__3, &c__5, (ftnlen)11);
	    o__1.ofnm = ch__2;
	    o__1.orl = 0;
	    o__1.osta = "old";
	    o__1.oacc = 0;
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    i__2 = f_open(&o__1);
	    if (i__2 != 0) {
		goto L99;
	    }
	    o__1.oerr = 1;
	    o__1.ounit = 24;
	    o__1.ofnmlen = 12;
/* Writing concatenation */
	    i__3[0] = 3, a__1[0] = "tpd";
	    i__3[1] = 2, a__1[1] = solact + (n - 1 << 1);
	    i__3[2] = 2, a__1[2] = dust + (m - 1 << 1);
	    i__3[3] = 1, a__1[3] = version;
	    i__3[4] = 4, a__1[4] = ".bin";
	    s_cat(ch__1, a__1, i__3, &c__5, (ftnlen)12);
	    o__1.ofnm = ch__1;
	    o__1.orl = 0;
	    o__1.osta = 0;
	    o__1.oacc = 0;
	    o__1.ofm = "unformatted";
	    o__1.oblnk = 0;
	    i__2 = f_open(&o__1);
	    if (i__2 != 0) {
		goto L99;
	    }
	    o__1.oerr = 1;
	    o__1.ounit = 25;
	    o__1.ofnmlen = 11;
/* Writing concatenation */
	    i__3[0] = 2, a__1[0] = "uv";
	    i__3[1] = 2, a__1[1] = solact + (n - 1 << 1);
	    i__3[2] = 2, a__1[2] = dust + (m - 1 << 1);
	    i__3[3] = 1, a__1[3] = version;
	    i__3[4] = 4, a__1[4] = ".bin";
	    s_cat(ch__2, a__1, i__3, &c__5, (ftnlen)11);
	    o__1.ofnm = ch__2;
	    o__1.orl = 0;
	    o__1.osta = 0;
	    o__1.oacc = 0;
	    o__1.ofm = "unformatted";
	    o__1.oblnk = 0;
	    i__2 = f_open(&o__1);
	    if (i__2 != 0) {
		goto L99;
	    }
/* ...      Read (and ignore) the header line from the ASCII files        TGMB 37 */
	    s_rsfe(&io___110);
	    do_fio(&c__1, dummy, (ftnlen)2);
	    e_rsfe();
	    s_rsfe(&io___112);
	    do_fio(&c__1, dummy, (ftnlen)2);
	    e_rsfe();
/* ...      Step through all Ls values                                    TGMB 41 */
	    for (lsea = 30; lsea <= 360; lsea += 30) {
		ls = lsea / 30;
/* ...      Step through all latitudes                                    TGMB 44 */
		for (ilat = 1; ilat <= 36; ++ilat) {
/* ...      Step through all heights                                      TGMB 46 */
		    nhgtt = 19;
		    if (m > 3) {
			nhgtt = 33;
		    }
		    i__2 = nhgtt;
		    for (k = 1; k <= i__2; ++k) {
/* ...        Read ASCII version tide coefficient amplitudes and phases   TGMB 52 */
/*           for temperature, pressure, and density                      TGMB 53 */
			i__4 = s_rsle(&io___118);
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&tzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&pzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&dzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = e_rsle();
			if (i__4 != 0) {
			    goto L55;
			}
/* ...        Write binary version tide amplitudes and phases for         TGMB 56 */
/*           temperature, pressure, and density                          TGMB 57 */
			s_wsue(&io___137);
			do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&tzp2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&pzp2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&dzp2, (ftnlen)sizeof(
				doublereal));
			e_wsue();
			if (ils != lsea) {
			    s_wsle(&io___138);
			    do_lio(&c__9, &c__1, " dust,Ls,hgt,lat=", (ftnlen)
				    17);
			    do_lio(&c__9, &c__1, dust + (m - 1 << 1), (ftnlen)
				    2);
			    do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)sizeof(
				    integer));
			    do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)
				    sizeof(integer));
			    do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)
				    sizeof(doublereal));
			    e_wsle();
			    s_stop(" Bad tpd Ls", (ftnlen)11);
			}
			if (ihgt != (k - 1) * 5 + 80) {
			    s_wsle(&io___139);
			    do_lio(&c__9, &c__1, " sol,dust,ils,k,ihgt,nhgtt="
				    , (ftnlen)27);
			    do_lio(&c__9, &c__1, solact + (n - 1 << 1), (
				    ftnlen)2);
			    do_lio(&c__9, &c__1, dust + (m - 1 << 1), (ftnlen)
				    2);
			    do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)sizeof(
				    integer));
			    do_lio(&c__3, &c__1, (char *)&k, (ftnlen)sizeof(
				    integer));
			    do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)
				    sizeof(integer));
			    do_lio(&c__3, &c__1, (char *)&nhgtt, (ftnlen)
				    sizeof(integer));
			    e_wsle();
			    s_stop(" Bad tpd Height", (ftnlen)15);
			}
/* ...        Read ASCII version tide coefficient amplitudes and phases   TGMB 69 */
/*           for EW wind and NS wind components                          TGMB 70 */
			i__4 = s_rsle(&io___140);
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__3, &c__1, (char *)&ihgt, (ftnlen)
				sizeof(integer));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&uza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&uza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&uzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&uza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&uzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&vza0, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&vza1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&vzp1, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&vza2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = do_lio(&c__5, &c__1, (char *)&vzp2, (ftnlen)
				sizeof(doublereal));
			if (i__4 != 0) {
			    goto L55;
			}
			i__4 = e_rsle();
			if (i__4 != 0) {
			    goto L55;
			}
/* ...        Reset value of last Ls processed                            TGMB 73 */
			lastls = ils;
/* ...        Write binary version tide amplitudes and phases for         TGMB 75 */
/*           EW wind and NS wind components                              TGMB 76 */
			s_wsue(&io___151);
			do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(integer));
			do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&uza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&uza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&uzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&uza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&uzp2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&vza0, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&vza1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&vzp1, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&vza2, (ftnlen)sizeof(
				doublereal));
			do_uio(&c__1, (char *)&vzp2, (ftnlen)sizeof(
				doublereal));
			e_wsue();
			if (ils != lsea) {
			    s_stop(" Bad uv Ls", (ftnlen)10);
			}
			if (ihgt != (k - 1) * 5 + 80) {
			    s_stop(" Bad uv Height", (ftnlen)14);
			}
/* L50: */
		    }
/* L51: */
		}
/* L52: */
	    }
	    goto L57;
/* ...      Write termination message if premature EOF encountered        TGMB 85 */
L55:
	    s_wsle(&io___152);
	    do_lio(&c__9, &c__1, " End of file on 85-240 km at Ls=", (ftnlen)
		    32);
	    do_lio(&c__3, &c__1, (char *)&lastls, (ftnlen)sizeof(integer));
	    e_wsle();
/* ...      Close all MTGCM 80-240 km data input files                    TGMB 87 */
L57:
	    for (i__ = 22; i__ <= 25; ++i__) {
		cl__1.cerr = 0;
		cl__1.cunit = i__;
		cl__1.csta = 0;
		f_clos(&cl__1);
/* L60: */
	    }
	    goto L100;
/* ...    Write warning message in case all dust optical depth values     TGMB 92 */
/*       are not available for processing                                TGMB 93 */
L99:
	    s_wsle(&io___154);
	    do_lio(&c__9, &c__1, " Files not available for dust optical dept\
h=", (ftnlen)44);
	    do_lio(&c__9, &c__1, dust + (m - 1 << 1), (ftnlen)2);
	    e_wsle();
L100:
	    ;
	}
/* L110: */
    }
    return 0;
} /* mtgcmbin_ */

/* -----------------------------------------------------------------------TGMB100 */
/* Subroutine */ int dustbin_(char *version, ftnlen version_len)
{
    /* Format strings */
    static char fmt_10[] = "(a1)";

    /* System generated locals */
    address a__1[3];
    integer i__1, i__2[3];
    doublereal d__1;
    char ch__1[12];
    olist o__1;

    /* Builtin functions */
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsfe(cilist *), do_fio(integer *, char *, 
	    ftnlen), e_rsfe(), s_rsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_rsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_wsle(cilist *), e_wsle(), s_wsue(cilist *), do_uio(integer *, 
	    char *, ftnlen), e_wsue();

    /* Local variables */
    static doublereal xlat, ylat, dust, xlon, ylon, ylat1;
    static integer iyear;
    static char dummy[1];
    static doublereal testau[147600]	/* was [2][72][25][41] */, stepls, 
	    als;
    static integer lat, ils, lon, iyr;
    static doublereal xls, steplat, steplon;

    /* Fortran I/O blocks */
    static cilist io___155 = { 0, 23, 0, fmt_10, 0 };
    static cilist io___168 = { 0, 23, 0, 0, 0 };
    static cilist io___174 = { 0, 6, 0, 0, 0 };
    static cilist io___176 = { 0, 24, 0, 0, 0 };
    static cilist io___177 = { 0, 6, 0, 0, 0 };


/* ...    Reads ASCII TES dust optical depths; write binary version       DSTB  2 */
/* ...    Set values for ntesyr = number of TES years; ntesls = number    DSTB  6 */
/*       of TES Ls's; nlat, nlon = number of TES lats and lons           DSTB  7 */
/* ...    Set parameter for "form=" in binary file Open statement         DSTB 12 */
/* ...    Open input and output files for dust data                       DSTB 15 */
    o__1.oerr = 1;
    o__1.ounit = 23;
    o__1.ofnmlen = 12;
/* Writing concatenation */
    i__2[0] = 7, a__1[0] = "TESdust";
    i__2[1] = 1, a__1[1] = version;
    i__2[2] = 4, a__1[2] = ".txt";
    s_cat(ch__1, a__1, i__2, &c__3, (ftnlen)12);
    o__1.ofnm = ch__1;
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    i__1 = f_open(&o__1);
    if (i__1 != 0) {
	goto L99;
    }
    o__1.oerr = 1;
    o__1.ounit = 24;
    o__1.ofnmlen = 12;
/* Writing concatenation */
    i__2[0] = 7, a__1[0] = "TESdust";
    i__2[1] = 1, a__1[1] = version;
    i__2[2] = 4, a__1[2] = ".bin";
    s_cat(ch__1, a__1, i__2, &c__3, (ftnlen)12);
    o__1.ofnm = ch__1;
    o__1.orl = 0;
    o__1.osta = 0;
    o__1.oacc = 0;
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    i__1 = f_open(&o__1);
    if (i__1 != 0) {
	goto L99;
    }
    s_rsfe(&io___155);
    do_fio(&c__1, dummy, (ftnlen)1);
    e_rsfe();
/* ...    Latitude step, based on number of lats                          DSTB 20 */
    steplat = 7.5;
    ylat1 = -90. - steplat;
/* ...    Longitude step, based on number of longitudes                   DSTB 23 */
    steplon = 9.;
/* ...    Ls step, based on number of Ls's                                DSTB 25 */
    stepls = (float)5.;
/* ...    Step through TES years                                          DSTB 27 */
    for (iyear = 1; iyear <= 2; ++iyear) {
/* ...    Step through Ls values                                          DSTB 29 */
	for (ils = 1; ils <= 72; ++ils) {
	    xls = -stepls / (float)2. + stepls * ils;
/* ...    Step through latitudes                                          DSTB 32 */
	    for (lat = 1; lat <= 25; ++lat) {
		ylat = ylat1 + steplat * lat;
/* ...    Step through longitudes                                         DSTB 35 */
		for (lon = 40; lon >= 1; --lon) {
		    ylon = steplon * lon;
/* ...      Read a line from ASCII format file                            DSTB 38 */
		    s_rsle(&io___168);
		    do_lio(&c__3, &c__1, (char *)&iyr, (ftnlen)sizeof(integer)
			    );
		    do_lio(&c__5, &c__1, (char *)&als, (ftnlen)sizeof(
			    doublereal));
		    do_lio(&c__5, &c__1, (char *)&xlat, (ftnlen)sizeof(
			    doublereal));
		    do_lio(&c__5, &c__1, (char *)&xlon, (ftnlen)sizeof(
			    doublereal));
		    do_lio(&c__5, &c__1, (char *)&dust, (ftnlen)sizeof(
			    doublereal));
		    e_rsle();
/* ...      Ceck input data; stop if error encountered                    DSTB 40 */
		    if (iyear != iyr) {
			s_stop(" Bad TES dust year", (ftnlen)18);
		    }
		    if ((d__1 = xls - als, abs(d__1)) > .01) {
			s_wsle(&io___174);
			do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)sizeof(
				integer));
			do_lio(&c__5, &c__1, (char *)&xls, (ftnlen)sizeof(
				doublereal));
			do_lio(&c__5, &c__1, (char *)&als, (ftnlen)sizeof(
				doublereal));
			e_wsle();
			s_stop(" Bad TES dust Ls", (ftnlen)16);
		    }
		    if ((d__1 = xlat - ylat, abs(d__1)) > .01) {
			s_stop(" Bad TES dust latitude", (ftnlen)22);
		    }
		    if ((d__1 = ylon - xlon, abs(d__1)) > .01) {
			s_stop(" Bad TES dust longitude", (ftnlen)23);
		    }
/* ...      Store dust optical depth value in array                       DSTB 48 */
		    testau[iyear + (ils + (lat + lon * 25) * 72 << 1) - 147] =
			     dust;
/* L40: */
		}
/* ...    Set longitude = 0 value to longitude = 360 value                DSTB 51 */
		testau[iyear + (ils + lat * 72 << 1) - 147] = testau[iyear + (
			ils + (lat + 1000) * 72 << 1) - 147];
/* L50: */
	    }
/* L60: */
	}
/* L70: */
    }
/* ...    Write binary array to output file                               DSTB 56 */
    s_wsue(&io___176);
    do_uio(&c_b530, (char *)&testau[0], (ftnlen)sizeof(doublereal));
    e_wsue();
    goto L100;
L99:
    s_wsle(&io___177);
    do_lio(&c__9, &c__1, " I/O Files not available for TES dust data", (
	    ftnlen)42);
    e_wsle();
L100:
    return 0;
} /* dustbin_ */

#ifdef __cplusplus
	}
#endif
