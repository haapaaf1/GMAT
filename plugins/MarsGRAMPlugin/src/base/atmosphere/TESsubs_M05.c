/* TESsubs_M05.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal dust[3], dzbl[3], zwsfc, f10val[2], f10tes[3];
    char dustc[6], solact[4], tesyr[4], soltes[6];
} mgcmparm_m05__;

#define mgcmparm_m05__1 mgcmparm_m05__

struct {
    doublereal ttsa0[79950]	/* was [3][25][41][13][2] */, ttsa1[79950]	
	    /* was [3][25][41][13][2] */, ttsp1[79950]	/* was [3][25][41][13]
	    [2] */, ttsa2[79950]	/* was [3][25][41][13][2] */, ttsp2[
	    79950]	/* was [3][25][41][13][2] */, tusa0[79950]	/* 
	    was [3][25][41][13][2] */, tusa1[79950]	/* was [3][25][41][13]
	    [2] */, tusp1[79950]	/* was [3][25][41][13][2] */, tusa2[
	    79950]	/* was [3][25][41][13][2] */, tusp2[79950]	/* 
	    was [3][25][41][13][2] */, tvsa0[79950]	/* was [3][25][41][13]
	    [2] */, tvsa1[79950]	/* was [3][25][41][13][2] */, tvsp1[
	    79950]	/* was [3][25][41][13][2] */, tvsa2[79950]	/* 
	    was [3][25][41][13][2] */, tvsp2[79950]	/* was [3][25][41][13]
	    [2] */;
} surftes_m05__;

#define surftes_m05__1 surftes_m05__

struct {
    doublereal ttza0[19500]	/* was [30][25][13][2] */, ttza1[19500]	/* 
	    was [30][25][13][2] */, ttzp1[19500]	/* was [30][25][13][2]
	     */, ttza2[19500]	/* was [30][25][13][2] */, ttzp2[19500]	/* 
	    was [30][25][13][2] */, tdza0[19500]	/* was [30][25][13][2]
	     */, tdza1[19500]	/* was [30][25][13][2] */, tdzp1[19500]	/* 
	    was [30][25][13][2] */, tdza2[19500]	/* was [30][25][13][2]
	     */, tdzp2[19500]	/* was [30][25][13][2] */, tpza0[19500]	/* 
	    was [30][25][13][2] */, tuza0[19500]	/* was [30][25][13][2]
	     */, tuza1[19500]	/* was [30][25][13][2] */, tuzp1[19500]	/* 
	    was [30][25][13][2] */, tuza2[19500]	/* was [30][25][13][2]
	     */, tuzp2[19500]	/* was [30][25][13][2] */, tvza0[19500]	/* 
	    was [30][25][13][2] */, tvza1[19500]	/* was [30][25][13][2]
	     */, tvzp1[19500]	/* was [30][25][13][2] */, tvza2[19500]	/* 
	    was [30][25][13][2] */, tvzp2[19500]	/* was [30][25][13][2]
	     */;
} mgcmtes_m05__;

#define mgcmtes_m05__1 mgcmtes_m05__

struct {
    doublereal ttta0[92664]	/* was [33][36][13][2][3] */, ttta1[92664]	
	    /* was [33][36][13][2][3] */, tttp1[92664]	/* was [33][36][13][2]
	    [3] */, ttta2[92664]	/* was [33][36][13][2][3] */, tttp2[
	    92664]	/* was [33][36][13][2][3] */, tpta0[92664]	/* 
	    was [33][36][13][2][3] */, tpta1[92664]	/* was [33][36][13][2]
	    [3] */, tptp1[92664]	/* was [33][36][13][2][3] */, tpta2[
	    92664]	/* was [33][36][13][2][3] */, tptp2[92664]	/* 
	    was [33][36][13][2][3] */, tdta0[92664]	/* was [33][36][13][2]
	    [3] */, tdta1[92664]	/* was [33][36][13][2][3] */, tdtp1[
	    92664]	/* was [33][36][13][2][3] */, tdta2[92664]	/* 
	    was [33][36][13][2][3] */, tdtp2[92664]	/* was [33][36][13][2]
	    [3] */, tuta0[92664]	/* was [33][36][13][2][3] */, tuta1[
	    92664]	/* was [33][36][13][2][3] */, tutp1[92664]	/* 
	    was [33][36][13][2][3] */, tuta2[92664]	/* was [33][36][13][2]
	    [3] */, tutp2[92664]	/* was [33][36][13][2][3] */, tvta0[
	    92664]	/* was [33][36][13][2][3] */, tvta1[92664]	/* 
	    was [33][36][13][2][3] */, tvtp1[92664]	/* was [33][36][13][2]
	    [3] */, tvta2[92664]	/* was [33][36][13][2][3] */, tvtp2[
	    92664]	/* was [33][36][13][2][3] */, tzfa0[2808]	/* 
	    was [36][13][2][3] */, tzfa1[2808]	/* was [36][13][2][3] */, 
	    tzfp1[2808]	/* was [36][13][2][3] */, tzfa2[2808]	/* was [36][
	    13][2][3] */, tzfp2[2808]	/* was [36][13][2][3] */;
    integer iztop[78]	/* was [13][2][3] */;
} tgcmtes_m05__;

#define tgcmtes_m05__1 tgcmtes_m05__

struct {
    doublereal dlat, dlon, dls, dlatw, dlonw, dlatt, df10, wpolefac, tpolefac;
    integer ilat, jlon, ls, k1st, ilatw, jlonw, ilatt, mf10;
} testerp_m05__;

#define testerp_m05__1 testerp_m05__

struct {
    doublereal offsets[39]	/* was [13][3] */, toffsets[26]	/* was [13][2]
	     */, zoffset, hgtoffset, ofszl;
    integer ibougher;
} tgcmoffset_m05__;

#define tgcmoffset_m05__1 tgcmoffset_m05__

struct {
    doublereal f107, stdl, fmol[9];
} therm_m05__;

#define therm_m05__1 therm_m05__

struct {
    doublereal phgt[100000], plat[100000], plon[100000], ptmp[100000], pprs[
	    100000], pden[100000], puwn[100000], pvwn[100000];
} pterp_m05__;

#define pterp_m05__1 pterp_m05__

/* Table of constant values */

static integer c__5 = 5;
static integer c__1 = 1;
static integer c__6 = 6;
static integer c__3 = 3;
static integer c__9 = 9;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c__30 = 30;

/* Subroutine */ int rdtessrf_m05__(char *gcmdir, char *version, ftnlen 
	gcmdir_len, ftnlen version_len)
{
    /* System generated locals */
    address a__1[5];
    integer i__1[5], i__2, i__3;
    char ch__1[72];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsue(cilist *), do_uio(integer *, char *, 
	    ftnlen), e_rsue();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer f_clos(cllist *);

    /* Local variables */
    static integer lsea, jlon;
    static doublereal xlat, ylat;
    static integer ilatstep, i__, m, k, ls, lendir, lastls, lat, ils, lon;

    /* Fortran I/O blocks */
    static cilist io___12 = { 0, 0, 1, 0, 0 };
    static cilist io___16 = { 0, 0, 1, 0, 0 };


/* ---    Reads NASA Ames Mars General Circulation Model (MGCM) surface   RDTS  2 */
/*       data (in binary format) for TES Mapping Years, and loads into   RDTS  3 */
/*       data arrays for common surfTES                                  RDTS  4 */
/*       GCMDIR is directory name where MGCM data resides                RDTS  5 */
/* ---    Set parameter values for ndust=number of dust optical depths,   RDTS 10 */
/*       ntbl=number of boundary layer levels, nlat=number of MGCM lati- RDTS 11 */
/*       tudes, nlon=number of MGCM longitudes                           RDTS 12 */
/* ---    Set parameter for form= in binary file open statement           RDTS 20 */
/*                                                                       RDTS 41 */
/* ---    Initialize last Ls value processed to 0                         RDTS 42 */
    lastls = 0;
/* ---    Set ilatstep = latitude step size x 10                          RDTS 44 */
    ilatstep = 75;
/* ---    Compute string length for directory name                        RDTS 46 */
    lendir = i_indx(gcmdir, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lendir < 1 || lendir > 60) {
	lendir = 60;
    }
/* ---    Step through all dust optical depths                            RDTS 49 */
    for (m = 1; m <= 2; ++m) {
/* ---      Open surface data files for surface level                     RDTS 51 */
	o__1.oerr = 0;
	o__1.ounit = 33;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "sfc00";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.tesyr + (m - 1 << 1);
	i__1[3] = 1, a__1[3] = version;
	i__1[4] = 4, a__1[4] = ".bin";
	s_cat(ch__1, a__1, i__1, &c__5, (ftnlen)72);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	f_open(&o__1);
/* ---      Open surface data files for 5 meter level above surface       RDTS 54 */
	o__1.oerr = 0;
	o__1.ounit = 34;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "sfc05";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.tesyr + (m - 1 << 1);
	i__1[3] = 1, a__1[3] = version;
	i__1[4] = 4, a__1[4] = ".bin";
	s_cat(ch__1, a__1, i__1, &c__5, (ftnlen)72);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	f_open(&o__1);
/* ---      Open surface data files for 30 meter level above surface      RDTS 57 */
	o__1.oerr = 0;
	o__1.ounit = 35;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "sfc30";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.tesyr + (m - 1 << 1);
	i__1[3] = 1, a__1[3] = version;
	i__1[4] = 4, a__1[4] = ".bin";
	s_cat(ch__1, a__1, i__1, &c__5, (ftnlen)72);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	f_open(&o__1);
/* ---      Step through all Ls values                                    RDTS 60 */
	for (lsea = 30; lsea <= 360; lsea += 30) {
	    ls = lsea / 30;
/* ---      Step through all latitudes                                    RDTS 63 */
	    i__2 = ilatstep;
	    for (lat = -900; i__2 < 0 ? lat >= 900 : lat <= 900; lat += i__2) 
		    {
		xlat = lat / 10.;
		i__ = (lat + 900) / ilatstep + 1;
/* ---      Step through all boundary layer levels                        RDTS 67 */
		for (k = 1; k <= 3; ++k) {
/* ---      Step through all longitudes                                   RDTS 69 */
		    for (lon = 40; lon >= 1; --lon) {
/* ---        Read (binary) tide coefficients for temperature and wind    RDTS 71 */
/*           components at all boundary layer levels                     RDTS 72 */
			if (k == 1) {
			    io___12.ciunit = k + 32;
			    i__3 = s_rsue(&io___12);
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				    integer));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&ylat, (ftnlen)
				    sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&jlon, (ftnlen)
				    sizeof(integer));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsp2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = e_rsue();
			    if (i__3 != 0) {
				goto L99;
			    }
/* ---         Assume surface wind = 0 (no slip condition)                RDTS 77 */
			    surftes_m05__1.tusa0[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tusa1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tusp1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tusa2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tusp2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tvsa0[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tvsa1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tvsp1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tvsa2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surftes_m05__1.tvsp2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			} else {
			    io___16.ciunit = k + 32;
			    i__3 = s_rsue(&io___16);
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				    integer));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&ylat, (ftnlen)
				    sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&jlon, (ftnlen)
				    sizeof(integer));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.ttsp2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tusa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tusa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tusp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tusa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tusp2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tvsa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tvsa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tvsp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tvsa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surftes_m05__1.tvsp2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = e_rsue();
			    if (i__3 != 0) {
				goto L99;
			    }
			}
			if (ils != lsea) {
			    s_stop(" Bad surface Ls", (ftnlen)15);
			}
/* ---        Reset value of last Ls processed                            RDTS 99 */
			lastls = ils;
			if (ylat != xlat) {
			    s_stop(" Bad surface Latitude", (ftnlen)21);
			}
			if (jlon != lon * 9) {
			    s_stop(" Bad surface Longitude", (ftnlen)22);
			}
/* L50: */
		    }
/* L51: */
		}
/* L52: */
	    }
/* L53: */
	}
/* ---      Set all values at Ls=0 to values at Ls=360                    RDTS107 */
	for (k = 1; k <= 3; ++k) {
	    for (i__ = 1; i__ <= 25; ++i__) {
		for (lon = 1; lon <= 40; ++lon) {
		    surftes_m05__1.ttsa0[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.ttsa0[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.ttsa1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.ttsa1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.ttsp1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.ttsp1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.ttsa2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.ttsa2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.ttsp2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.ttsp2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tusa0[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tusa0[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tusa1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tusa1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tusp1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tusp1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tusa2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tusa2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tusp2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tusp2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tvsa0[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tvsa0[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tvsa1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tvsa1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tvsp1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tvsp1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tvsa2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tvsa2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surftes_m05__1.tvsp2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surftes_m05__1.tvsp2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
/* L60: */
		}
/*           Set all values at Lon=0 to values at Lon=360                RDTS127 */
		for (ls = 0; ls <= 12; ++ls) {
		    surftes_m05__1.ttsa0[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.ttsa0[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.ttsa1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.ttsa1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.ttsp1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.ttsp1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.ttsa2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.ttsa2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.ttsp2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.ttsp2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tusa0[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tusa0[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tusa1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tusa1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tusp1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tusp1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tusa2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tusa2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tusp2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tusp2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tvsa0[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tvsa0[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tvsa1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tvsa1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tvsp1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tvsp1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tvsa2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tvsa2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surftes_m05__1.tvsp2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surftes_m05__1.tvsp2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
/* L70: */
		}
/* L80: */
	    }
	}
/* ---      Close input file units                                        RDTS146 */
	cl__1.cerr = 0;
	cl__1.cunit = 33;
	cl__1.csta = 0;
	f_clos(&cl__1);
	cl__1.cerr = 0;
	cl__1.cunit = 34;
	cl__1.csta = 0;
	f_clos(&cl__1);
	cl__1.cerr = 0;
	cl__1.cunit = 35;
	cl__1.csta = 0;
	f_clos(&cl__1);
	goto L100;
/* ---      Terminate if not all Ls values processed                      RDTS151 */
L99:
	if (lastls != 360) {
	    s_stop(" Incomplete TES surface GCM data", (ftnlen)32);
	}
L100:
	;
    }
    return 0;
} /* rdtessrf_m05__ */

/* -----------------------------------------------------------------------RDTS156 */
/* Subroutine */ int rdtesmgcm_m05__(char *gcmdir, char *version, ftnlen 
	gcmdir_len, ftnlen version_len)
{
    /* System generated locals */
    address a__1[5];
    integer i__1[5], i__2, i__3;
    char ch__1[72], ch__2[71];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsue(cilist *), do_uio(integer *, char *, 
	    ftnlen), e_rsue();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer f_clos(cllist *);

    /* Local variables */
    static integer lsea, ihgt, khgt;
    static doublereal xlat, ylat;
    static integer ilatstep, m, i__, k, ls, lendir, lastls, lat, ils;

    /* Fortran I/O blocks */
    static cilist io___27 = { 0, 32, 1, 0, 0 };
    static cilist io___32 = { 0, 33, 1, 0, 0 };


/* ---    Reads NASA Ames Mars General Circulation Model (MGCM) -5 to 80  RDTM  2 */
/*       km data (in binary format) for TES years 1& 2 and loads into    RDTM  3 */
/*       data arrays for common MGCMTES                                  RDTM  4 */
/*       GCMDIR is directory name where MGCM data resides                RDTM  5 */
/* ---    Set parameters for ntesy=number of TES years, nhgt=             RDTM 10 */
/*       number of MGCM heights, nlat=number of MGCM latitudes           RDTM 11 */
/* ---    Set parameter for form= in binary file open statement           RDTM 19 */
/*                                                                       RDTM 36 */
/* ---    Initialize last Ls value processed to 0                         RDTM 37 */
    lastls = 0;
/* ---    Set ilatstep = latitude step size x 10                          RDTM 39 */
    ilatstep = 75;
/* ---    Compute string length for directory name                        RDTM 41 */
    lendir = i_indx(gcmdir, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lendir < 1 || lendir > 60) {
	lendir = 60;
    }
/* ---    Step through all dust optical depths                            RDTM 44 */
    for (m = 1; m <= 2; ++m) {
/* ---      Open MGCM input files for temperature, pressure, and density  RDTM 46 */
	o__1.oerr = 0;
	o__1.ounit = 32;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "tpdlo";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.tesyr + (m - 1 << 1);
	i__1[3] = 1, a__1[3] = version;
	i__1[4] = 4, a__1[4] = ".bin";
	s_cat(ch__1, a__1, i__1, &c__5, (ftnlen)72);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	f_open(&o__1);
/* ---      Open MGCM input files for wind components                     RDTM 49 */
	o__1.oerr = 0;
	o__1.ounit = 33;
	o__1.ofnmlen = lendir + 11;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 4, a__1[1] = "uvlo";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.tesyr + (m - 1 << 1);
	i__1[3] = 1, a__1[3] = version;
	i__1[4] = 4, a__1[4] = ".bin";
	s_cat(ch__2, a__1, i__1, &c__5, (ftnlen)71);
	o__1.ofnm = ch__2;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	f_open(&o__1);
/* ---      Step through all Ls values                                    RDTM 52 */
	for (lsea = 30; lsea <= 360; lsea += 30) {
	    ls = lsea / 30;
/* ---      Step through all latitude grid points                         RDTM 55 */
	    i__2 = ilatstep;
	    for (lat = -900; i__2 < 0 ? lat >= 900 : lat <= 900; lat += i__2) 
		    {
		xlat = lat / 10.;
		i__ = (lat + 900) / ilatstep + 1;
/* ---      Step through all height levels                                RDTM 59 */
		for (k = 30; k >= 1; --k) {
/* ---        Read (binary) tide coefficients for temperature, pressure,  RDTM 61 */
/*           and density                                                 RDTM 62 */
		    i__3 = s_rsue(&io___27);
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(integer)
			    );
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(
			    integer));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
			    doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.ttza0[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.ttza1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.ttzp1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.ttza2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.ttzp2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tdza0[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tdza1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tdzp1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tdza2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tdzp2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tpza0[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = e_rsue();
		    if (i__3 != 0) {
			goto L99;
		    }
		    if (ils != lsea) {
			s_stop(" Bad tpd Ls", (ftnlen)11);
		    }
		    khgt = (k - 14) * 5;
		    if (k < 16) {
			khgt = k - 6;
		    }
		    if (ihgt != khgt) {
			s_stop(" Bad tpd Height", (ftnlen)15);
		    }
		    if (ylat != xlat) {
			s_stop(" Bad tpd Latitude", (ftnlen)17);
		    }
/* ---        Read (binary) tide coefficients for wind components         RDTM 73 */
		    i__3 = s_rsue(&io___32);
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(integer)
			    );
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(
			    integer));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
			    doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tuza0[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tuza1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tuzp1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tuza2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tuzp2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tvza0[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tvza1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tvzp1[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tvza2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmtes_m05__1.tvzp2[k + (
			    i__ + (ls + m * 13) * 25) * 30 - 9781], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = e_rsue();
		    if (i__3 != 0) {
			goto L99;
		    }
		    if (ils != lsea) {
			s_stop(" Bad uv Ls", (ftnlen)10);
		    }
/* ---        Reset value of last Ls processed                            RDTM 79 */
		    lastls = ils;
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
/* ---      Set data for Ls = 0 to data for Ls = 360                      RDTM 86 */
	for (k = 1; k <= 30; ++k) {
	    for (i__ = 1; i__ <= 25; ++i__) {
		mgcmtes_m05__1.ttza0[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.ttza0[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.ttza1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.ttza1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.ttzp1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.ttzp1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.ttza2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.ttza2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.ttzp2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.ttzp2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tdza0[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tdza0[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tdza1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tdza1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tdzp1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tdzp1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tdza2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tdza2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tdzp2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tdzp2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tpza0[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tpza0[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tuza0[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tuza0[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tuza1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tuza1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tuzp1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tuzp1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tuza2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tuza2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tuzp2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tuzp2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tvza0[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tvza0[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tvza1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tvza1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tvzp1[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tvzp1[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tvza2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tvza2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
		mgcmtes_m05__1.tvzp2[k + (i__ + m * 325) * 30 - 9781] = 
			mgcmtes_m05__1.tvzp2[k + (i__ + (m * 13 + 12) * 25) * 
			30 - 9781];
/* L60: */
	    }
/* L61: */
	}
/* ---    Close input files to re-use same unit number for next dust      RDTM112 */
/*       value                                                           RDTM113 */
	cl__1.cerr = 0;
	cl__1.cunit = 32;
	cl__1.csta = 0;
	f_clos(&cl__1);
	cl__1.cerr = 0;
	cl__1.cunit = 33;
	cl__1.csta = 0;
	f_clos(&cl__1);
	goto L100;
/* ---    Terminate if not all Ls values have been processed              RDTM117 */
L99:
	if (lastls != 360) {
	    s_stop(" Incomplete -5 to 80 km MGCM data", (ftnlen)33);
	}
L100:
	;
    }
    return 0;
} /* rdtesmgcm_m05__ */

/* -----------------------------------------------------------------------RDTM122 */
/* Subroutine */ int rdtestgcm_m05__(char *gcmdir, char *version, ftnlen 
	gcmdir_len, ftnlen version_len)
{
    /* System generated locals */
    address a__1[5], a__2[6];
    integer i__1[5], i__2[6], i__3, i__4, i__5, i__6;
    char ch__1[72], ch__2[71];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_rsle(), s_wsle(cilist *), e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_rsue(cilist *), do_uio(integer *, char *, ftnlen), e_rsue(), 
	    f_clos(cllist *);

    /* Local variables */
    static integer lsea, ihgt;
    static doublereal xlat, ylat;
    static integer ilatstep, ilat1, ilat2, i__, n, m, k, ls, lendir, lastls, 
	    lat, ils, iyr, nhgttop;

    /* Fortran I/O blocks */
    static cilist io___45 = { 0, 34, 1, 0, 0 };
    static cilist io___50 = { 0, 6, 0, 0, 0 };
    static cilist io___52 = { 0, 32, 1, 0, 0 };
    static cilist io___54 = { 0, 33, 1, 0, 0 };


/* ---    Reads University of Michigan Mars Thermospheric General Circu-  RDTT  2 */
/*       lation Model (MTGCM) data (in binary format) for TES years 1&2  RDTT  3 */
/*       and loads into dataarrays for common TGCMTES                    RDTT  4 */
/*       GCMDIR is directory name where MTGCM data resides               RDTT  5 */
/* ---    Set parameter values for ntesy = number of TES years,           RDTT 10 */
/*       nhgtt=number of MTGCM heights, nlatt=number of MTGCM latitudes  RDTT 11 */
/* ---    Set parameter for form= in binary file open statement           RDTT 19 */
/*                                                                       RDTT 44 */
/* ---    Initialize last Ls value processed to 0                         RDTT 45 */
    lastls = 0;
/* ---    Set ilatstep = latitude step size x 10                          RDTT 47 */
    ilatstep = 50;
/* ---    Set initial and final latitudes (x 10) for stepping             RDTT 49 */
    ilat1 = ilatstep / 2 - 900;
    ilat2 = 900 - ilatstep / 2;
/* ---    Compute string length for directory name                        RDTT 52 */
    lendir = i_indx(gcmdir, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lendir < 1 || lendir > 60) {
	lendir = 60;
    }
/* ---    Step through all solar activity levels                          RDTT 55 */
    for (n = 1; n <= 3; ++n) {
	o__1.oerr = 0;
	o__1.ounit = 34;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "zfTES";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.soltes + (n - 1 << 1);
	i__1[3] = 1, a__1[3] = version;
	i__1[4] = 4, a__1[4] = ".txt";
	s_cat(ch__1, a__1, i__1, &c__5, (ftnlen)72);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);
/* ---    Step through all dust optical depths                            RDTT 59 */
	for (m = 1; m <= 2; ++m) {
/* ---      Open MTGCM data files for temperature, pressure, and density  RDTT 61 */
	    o__1.oerr = 0;
	    o__1.ounit = 32;
	    o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	    i__2[0] = lendir, a__2[0] = gcmdir;
	    i__2[1] = 3, a__2[1] = "tpd";
	    i__2[2] = 2, a__2[2] = mgcmparm_m05__1.soltes + (n - 1 << 1);
	    i__2[3] = 2, a__2[3] = mgcmparm_m05__1.tesyr + (m - 1 << 1);
	    i__2[4] = 1, a__2[4] = version;
	    i__2[5] = 4, a__2[5] = ".bin";
	    s_cat(ch__1, a__2, i__2, &c__6, (ftnlen)72);
	    o__1.ofnm = ch__1;
	    o__1.orl = 0;
	    o__1.osta = "old";
	    o__1.oacc = 0;
	    o__1.ofm = "unformatted";
	    o__1.oblnk = 0;
	    f_open(&o__1);
/* ---      Open MTGCM data files for wind components                     RDTT 64 */
	    o__1.oerr = 0;
	    o__1.ounit = 33;
	    o__1.ofnmlen = lendir + 11;
/* Writing concatenation */
	    i__2[0] = lendir, a__2[0] = gcmdir;
	    i__2[1] = 2, a__2[1] = "uv";
	    i__2[2] = 2, a__2[2] = mgcmparm_m05__1.soltes + (n - 1 << 1);
	    i__2[3] = 2, a__2[3] = mgcmparm_m05__1.tesyr + (m - 1 << 1);
	    i__2[4] = 1, a__2[4] = version;
	    i__2[5] = 4, a__2[5] = ".bin";
	    s_cat(ch__2, a__2, i__2, &c__6, (ftnlen)71);
	    o__1.ofnm = ch__2;
	    o__1.orl = 0;
	    o__1.osta = "old";
	    o__1.oacc = 0;
	    o__1.ofm = "unformatted";
	    o__1.oblnk = 0;
	    f_open(&o__1);
/* ---      Step through all Ls values                                    RDTT 67 */
	    for (lsea = 30; lsea <= 360; lsea += 30) {
		ls = lsea / 30;
/* ---      Step through all latitudes                                    RDTT 70 */
		i__3 = ilat2;
		i__4 = ilatstep;
		for (lat = ilat1; i__4 < 0 ? lat >= i__3 : lat <= i__3; lat +=
			 i__4) {
		    xlat = lat / 10.;
		    i__ = (lat - ilat1) / ilatstep + 1;
/* ---        Read tide coefficient data for ZF=height of 1.26 nbar level RDTT 74 */
		    i__5 = s_rsle(&io___45);
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__3, &c__1, (char *)&iyr, (ftnlen)sizeof(
			    integer));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)sizeof(
			    integer));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)sizeof(
			    doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmtes_m05__1.tzfa0[
			    i__ + (ls + (m + (n << 1)) * 13) * 36 - 1405], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmtes_m05__1.tzfa1[
			    i__ + (ls + (m + (n << 1)) * 13) * 36 - 1405], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmtes_m05__1.tzfp1[
			    i__ + (ls + (m + (n << 1)) * 13) * 36 - 1405], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmtes_m05__1.tzfa2[
			    i__ + (ls + (m + (n << 1)) * 13) * 36 - 1405], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmtes_m05__1.tzfp2[
			    i__ + (ls + (m + (n << 1)) * 13) * 36 - 1405], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__3, &c__1, (char *)&nhgttop, (ftnlen)
			    sizeof(integer));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = e_rsle();
		    if (i__5 != 0) {
			goto L99;
		    }
		    if (nhgttop != 240) {
			s_wsle(&io___50);
			do_lio(&c__9, &c__1, "iyr,ils,ylat,nhgttop=", (ftnlen)
				21);
			do_lio(&c__3, &c__1, (char *)&iyr, (ftnlen)sizeof(
				integer));
			do_lio(&c__3, &c__1, (char *)&ils, (ftnlen)sizeof(
				integer));
			do_lio(&c__5, &c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			do_lio(&c__3, &c__1, (char *)&nhgttop, (ftnlen)sizeof(
				integer));
			e_wsle();
			s_stop(" Bad nhgttop value", (ftnlen)18);
		    }
		    if (iyr != m) {
			s_stop(" Bad ZF year value", (ftnlen)18);
		    }
		    if (ils != lsea) {
			s_stop(" Bad ZF Ls", (ftnlen)10);
		    }
		    if (ylat != xlat) {
			s_stop(" Bad ZF Latitude", (ftnlen)16);
		    }
		    nhgttop = (nhgttop - 75) / 5;
		    tgcmtes_m05__1.iztop[ls + (m + (n << 1)) * 13 - 39] = 
			    nhgttop;
/* ---      Step through all heights                                      RDTT 83 */
		    i__5 = nhgttop;
		    for (k = 1; k <= i__5; ++k) {
/* ---        Read (binary) tide coefficients for temperature, pressure,  RDTT 85 */
/*           and density                                                 RDTT 86 */
			i__6 = s_rsue(&io___52);
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				integer));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(
				integer));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.ttta0[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.ttta1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tttp1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.ttta2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tttp2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tpta0[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tpta1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tptp1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tpta2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tptp2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tdta0[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tdta1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tdtp1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tdta2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tdtp2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = e_rsue();
			if (i__6 != 0) {
			    goto L99;
			}
			if (ils != lsea) {
			    s_stop(" Bad tpd Ls", (ftnlen)11);
			}
			if (ihgt != (k - 1) * 5 + 80) {
			    s_stop(" Bad tpd Height", (ftnlen)15);
			}
			if (ylat != xlat) {
			    s_stop(" Bad tpd Latitude", (ftnlen)17);
			}
/* ---        Read (binary) tide coefficients for wind components         RDTT 96 */
			i__6 = s_rsue(&io___54);
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				integer));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(
				integer));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tuta0[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tuta1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tutp1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tuta2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tutp2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tvta0[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tvta1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tvtp1[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tvta2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = do_uio(&c__1, (char *)&tgcmtes_m05__1.tvtp2[k 
				+ (i__ + (ls + (m + (n << 1)) * 13) * 36) * 
				33 - 46366], (ftnlen)sizeof(doublereal));
			if (i__6 != 0) {
			    goto L99;
			}
			i__6 = e_rsue();
			if (i__6 != 0) {
			    goto L99;
			}
			if (ils != lsea) {
			    s_stop(" Bad uv Ls", (ftnlen)10);
			}
/* ---        Reset last Ls value processed                               RDTT102 */
			lastls = ils;
			if (ihgt != (k - 1) * 5 + 80) {
			    s_stop(" Bad uv Height", (ftnlen)14);
			}
			if (ylat != xlat) {
			    s_stop(" Bad uv Latitude", (ftnlen)16);
			}
/* L40: */
		    }
/* L50: */
		}
	    }
/* ---      Set all values at Ls=0 to values at Ls=360                    RDTT108 */
	    tgcmtes_m05__1.iztop[(m + (n << 1)) * 13 - 39] = 
		    tgcmtes_m05__1.iztop[(m + (n << 1)) * 13 - 27];
	    for (i__ = 1; i__ <= 36; ++i__) {
		tgcmtes_m05__1.tzfa0[i__ + (m + (n << 1)) * 468 - 1405] = 
			tgcmtes_m05__1.tzfa0[i__ + ((m + (n << 1)) * 13 + 12) 
			* 36 - 1405];
		tgcmtes_m05__1.tzfa1[i__ + (m + (n << 1)) * 468 - 1405] = 
			tgcmtes_m05__1.tzfa1[i__ + ((m + (n << 1)) * 13 + 12) 
			* 36 - 1405];
		tgcmtes_m05__1.tzfp1[i__ + (m + (n << 1)) * 468 - 1405] = 
			tgcmtes_m05__1.tzfp1[i__ + ((m + (n << 1)) * 13 + 12) 
			* 36 - 1405];
		tgcmtes_m05__1.tzfa2[i__ + (m + (n << 1)) * 468 - 1405] = 
			tgcmtes_m05__1.tzfa2[i__ + ((m + (n << 1)) * 13 + 12) 
			* 36 - 1405];
		tgcmtes_m05__1.tzfp2[i__ + (m + (n << 1)) * 468 - 1405] = 
			tgcmtes_m05__1.tzfp2[i__ + ((m + (n << 1)) * 13 + 12) 
			* 36 - 1405];
		i__4 = tgcmtes_m05__1.iztop[(m + (n << 1)) * 13 - 27];
		for (k = 1; k <= i__4; ++k) {
		    tgcmtes_m05__1.ttta0[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.ttta0[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.ttta1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.ttta1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tttp1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tttp1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.ttta2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.ttta2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tttp2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tttp2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tpta0[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tpta0[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tpta1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tpta1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tptp1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tptp1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tpta2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tpta2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tptp2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tptp2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tdta0[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tdta0[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tdta1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tdta1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tdtp1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tdtp1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tdta2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tdta2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tdtp2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tdtp2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tuta0[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tuta0[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tuta1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tuta1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tutp1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tutp1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tuta2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tuta2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tutp2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tutp2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tvta0[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tvta0[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tvta1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tvta1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tvtp1[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tvtp1[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tvta2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tvta2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
		    tgcmtes_m05__1.tvtp2[k + (i__ + (m + (n << 1)) * 468) * 
			    33 - 46366] = tgcmtes_m05__1.tvtp2[k + (i__ + ((m 
			    + (n << 1)) * 13 + 12) * 36) * 33 - 46366];
/* L60: */
		}
	    }
/* ---    Close input file unit numbers                                   RDTT143 */
	    cl__1.cerr = 0;
	    cl__1.cunit = 32;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    cl__1.cerr = 0;
	    cl__1.cunit = 33;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    goto L100;
/* ---    Terminate if not all Ls values processed                        RDTT147 */
L99:
	    if (lastls != 360) {
		s_stop(" Incomplete 80-240  km MTGCM data", (ftnlen)33);
	    }
L100:
	    ;
	}
	cl__1.cerr = 0;
	cl__1.cunit = 34;
	cl__1.csta = 0;
	f_clos(&cl__1);
/* L110: */
    }
    return 0;
} /* rdtestgcm_m05__ */

/* -----------------------------------------------------------------------RDTT154 */
/* Subroutine */ int tesgterp_m05__(integer *khgt, doublereal *time, 
	doublereal *tmgcm, doublereal *pmgcm, doublereal *dmgcm, doublereal *
	umgcm, doublereal *vmgcm, doublereal *tempday, doublereal *presday, 
	doublereal *densday, doublereal *uwndday, doublereal *vwndday, 
	doublereal *tempmax, doublereal *tempmin, doublereal *densmax, 
	doublereal *densmin, integer *mtesy, integer *idaydata)
{
    static doublereal dday[4]	/* was [2][2] */, dmin__[4]	/* was [2][2] 
	    */, dmax__[4]	/* was [2][2] */, tday[4]	/* was [2][2] 
	    */, uday[4]	/* was [2][2] */, vday[4]	/* was [2][2] */, 
	    tmin[4]	/* was [2][2] */, tmax[4]	/* was [2][2] */, 
	    upolefac;
    extern doublereal tidex_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), tidey_m05__(doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static integer i__, l;
    static doublereal dtime, rmgcm;
    static integer itime;
    static doublereal ptime, d0, xtime, ttime, a1, a2, p0, r0[4]	/* 
	    was [2][2] */, t0, u0, p1, p2, v0, dm[4]	/* was [2][2] */, tm[
	    4]	/* was [2][2] */, um[4]	/* was [2][2] */, vm[4]	/* was [2][2] 
	    */, a1d, a2d, p1d, a1t, a2t, p2d, p1t, p2t, polefac;
    extern /* Subroutine */ int twod_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *);

/* ---    Interpolates Ames Mars General Circulation Model (MGCM) data    TGTP  4 */
/*       to a given latitude, time of year (Ls), for a given TES year,   TGTP  5 */
/*       height index (khgt) and time of day (time).                     TGTP  6 */
/*       Some input data is provided by the Common "Interp".             TGTP  7 */
/* ---    Set parameter values for number of heights, latitudes           TGTP  8 */
/* ---    MGCM -5 to 80 km data arrays for interpolation                  TGTP 13 */
/* ---    Establish MGCM values at corners of a 2-dimensional cube in     TGTP 30 */
/*       latitude-Ls space, at the given height index (khgt), and        TGTP 31 */
/*       time of day (time)                                              TGTP 32 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	upolefac = 1.;
	if (testerp_m05__1.ilat == 1) {
	    polefac = i__ - 1.;
	} else if (testerp_m05__1.ilat == 24) {
	    polefac = 2. - i__;
	}
	if (testerp_m05__1.ilatw == 2) {
	    if (i__ == 1) {
		upolefac = testerp_m05__1.wpolefac;
	    }
	} else if (testerp_m05__1.ilatw == 24) {
	    if (i__ == 2) {
		upolefac = testerp_m05__1.wpolefac;
	    }
	}
	for (l = 1; l <= 2; ++l) {
/* ---      Daily mean temperature                                        TGTP 47 */
	    t0 = mgcmtes_m05__1.ttza0[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    tday[i__ + (l << 1) - 3] = t0;
/* ---      Temperature tide amplitudes and phases                        TGTP 50 */
	    a1t = mgcmtes_m05__1.ttza1[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * polefac;
	    p1t = mgcmtes_m05__1.ttzp1[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    a2t = mgcmtes_m05__1.ttza2[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * polefac;
	    p2t = mgcmtes_m05__1.ttzp2[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
/* ---      Temperature at corners of 2-D cube                            TGTP 55 */
	    tm[i__ + (l << 1) - 3] = tidex_m05__(&t0, &a1t, &p1t, &a2t, &p2t, 
		    time);
/* ---      Daily mean density                                            TGTP 57 */
	    d0 = mgcmtes_m05__1.tdza0[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    dday[i__ + (l << 1) - 3] = d0;
/* ---      Density tide amplitudes and phases                            TGTP 60 */
	    a1d = mgcmtes_m05__1.tdza1[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * polefac;
	    p1d = mgcmtes_m05__1.tdzp1[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    a2d = mgcmtes_m05__1.tdza2[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * polefac;
	    p2d = mgcmtes_m05__1.tdzp2[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
/* ---      Density at corners of 2-D cube                                TGTP 65 */
	    dm[i__ + (l << 1) - 3] = tidey_m05__(&d0, &a1d, &p1d, &a2d, &p2d, 
		    time);
/* ---      Daily average pressure P0                                     TGTP 67 */
	    p0 = mgcmtes_m05__1.tpza0[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
/* ---      Gas constant from pressure, density and temperature           TGTP 69 */
	    r0[i__ + (l << 1) - 3] = 190.;
	    if (t0 != 0. && d0 != 0.) {
		r0[i__ + (l << 1) - 3] = p0 / (t0 * d0);
	    }
/* ---      Max and Min temperature and density at corners of 2-D cube    TGTP 72 */
	    tmax[i__ + (l << 1) - 3] = -9999.;
	    tmin[i__ + (l << 1) - 3] = 9999.;
	    dmax__[i__ + (l << 1) - 3] = -9999.;
	    dmin__[i__ + (l << 1) - 3] = 9999.;
	    if (*idaydata > 0) {
		for (itime = 0; itime <= 23; ++itime) {
		    xtime = (real) itime;
		    ttime = tidex_m05__(&t0, &a1t, &p1t, &a2t, &p2t, &xtime);
		    dtime = tidey_m05__(&d0, &a1d, &p1d, &a2d, &p2d, &xtime);
		    ptime = dtime * r0[i__ + (l << 1) - 3] * ttime;
		    if (ttime > tmax[i__ + (l << 1) - 3]) {
			tmax[i__ + (l << 1) - 3] = ttime;
		    }
		    if (ttime < tmin[i__ + (l << 1) - 3]) {
			tmin[i__ + (l << 1) - 3] = ttime;
		    }
		    if (dtime > dmax__[i__ + (l << 1) - 3]) {
			dmax__[i__ + (l << 1) - 3] = dtime;
		    }
		    if (dtime < dmin__[i__ + (l << 1) - 3]) {
			dmin__[i__ + (l << 1) - 3] = dtime;
		    }
/* L50: */
		}
	    }
/* ---      Daily mean EW wind                                            TGTP 89 */
	    u0 = mgcmtes_m05__1.tuza0[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    uday[i__ + (l << 1) - 3] = u0;
/* ---      EW wind tide amplitudes and phases                            TGTP 92 */
	    a1 = mgcmtes_m05__1.tuza1[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * upolefac;
	    p1 = mgcmtes_m05__1.tuzp1[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    a2 = mgcmtes_m05__1.tuza2[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * upolefac;
	    p2 = mgcmtes_m05__1.tuzp2[*khgt + (testerp_m05__1.ilat + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
/* ---      EW wind at corners of 2-D cube                                TGTP 97 */
	    um[i__ + (l << 1) - 3] = tidex_m05__(&u0, &a1, &p1, &a2, &p2, 
		    time);
/* ---      Daily mean NS wind                                            TGTP 99 */
	    v0 = mgcmtes_m05__1.tvza0[*khgt + (testerp_m05__1.ilatw + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    vday[i__ + (l << 1) - 3] = v0;
/* ---      NS wind tide amplitudes and phases                            TGTP102 */
	    a1 = mgcmtes_m05__1.tvza1[*khgt + (testerp_m05__1.ilatw + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * upolefac;
	    p1 = mgcmtes_m05__1.tvzp1[*khgt + (testerp_m05__1.ilatw + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
	    a2 = mgcmtes_m05__1.tvza2[*khgt + (testerp_m05__1.ilatw + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781] * upolefac;
	    p2 = mgcmtes_m05__1.tvzp2[*khgt + (testerp_m05__1.ilatw + i__ - 1 
		    + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 25) * 30 - 
		    9781];
/* ---      NS wind at corners of 2-D cube                                TGTP107 */
	    vm[i__ + (l << 1) - 3] = tidex_m05__(&v0, &a1, &p1, &a2, &p2, 
		    time);
/* L101: */
	}
/* L102: */
    }
/* ---    Use 2-D interpolation to get temperature, pressure, gas         TGTP111 */
/*       constant, EW wind, and NS wind at given latitude, and Ls        TGTP112 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, tm, tmgcm);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, tday, tempday);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, tmax, tempmax);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, tmin, tempmin);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dmax__, densmax);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dmin__, densmin);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dm, dmgcm);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dday, densday);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, r0, &rmgcm);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, um, umgcm);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, uday, uwndday);
    twod_m05__(&testerp_m05__1.dlatw, &testerp_m05__1.dls, vm, vmgcm);
    twod_m05__(&testerp_m05__1.dlatw, &testerp_m05__1.dls, vday, vwndday);
/* ---    Compute pressure from temperature, density, and gas constant    TGTP126 */
    *pmgcm = *dmgcm * rmgcm * *tmgcm;
    *presday = *densday * rmgcm * *tempday;
    return 0;
} /* tesgterp_m05__ */

/* -----------------------------------------------------------------------TGTP131 */
/* Subroutine */ int tessrftrp_m05__(integer *khgt, doublereal *time, 
	doublereal *tmgcm, doublereal *pmgcm, doublereal *dmgcm, doublereal *
	umgcm, doublereal *vmgcm, doublereal *hpres, doublereal *hdens, 
	doublereal *ctopohgt, doublereal *tempday, doublereal *presday, 
	doublereal *densday, doublereal *uwndday, doublereal *vwndday, 
	doublereal *hpres0, doublereal *tempmax, doublereal *tempmin, 
	doublereal *densmax, doublereal *densmin, doublereal *tat5m, integer *
	mtesy, integer *idaydata)
{
    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static doublereal z1st, dmin__[4]	/* was [2][2] */, dmax__[4]	/* 
	    was [2][2] */, tday[8]	/* was [2][2][2] */, uday[8]	/* 
	    was [2][2][2] */, vday[8]	/* was [2][2][2] */, tbar, tmin[8]	
	    /* was [2][2][2] */, tmax[8]	/* was [2][2][2] */, upolefac;
    extern doublereal tidex_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), tidey_m05__(doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), zlogr_m05__(doublereal *, doublereal *, char *, 
	    ftnlen);
    static doublereal tbar0, d1min, d1max, dzk1h, t1min[4]	/* was [2][2] 
	    */, tmin1, t1max[4]	/* was [2][2] */;
    static integer i__, j, l, itime;
    extern /* Subroutine */ int threed_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static doublereal dtime, xtime, ttime, a1, a2, d0, pzk1h, p0, p1, t0, u0, 
	    p2, v0, pzk1h1, dzk1h1, tzero, dzero, pzero, rzero, tmax1, hdmin, 
	    hdens0, hdmax, t1time, tm[8]	/* was [2][2][2] */, um[8]	
	    /* was [2][2][2] */, vm[8]	/* was [2][2][2] */, height;
    static integer k1h;
    static doublereal a1t, a2t, dz0[4]	/* was [2][2] */, dz1[4]	/* 
	    was [2][2] */, p1t, p2t, ts0[8]	/* was [2][2][2] */, tszero, 
	    pz0[4]	/* was [2][2] */, rz0[4]	/* was [2][2] */, tz0[
	    4]	/* was [2][2] */, tz1[4]	/* was [2][2] */, dzh[4]	
	    /* was [2][2] */, polefac, pzh[4]	/* was [2][2] */;
    extern /* Subroutine */ int twod_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal dzh1[4]	/* was [2][2] */, dz1h, d1st, pzh1[4]	/* 
	    was [2][2] */, p1st, t1st;

/* ---    Interpolates Ames Mars General Circulation Model (MGCM) surface TSTP  5 */
/*       data to a given latitude, longitude, time of year (Ls), for a   TSTP  6 */
/*       given TES year (mtesy), height index (khgt) and time of day     TSTP  7 */
/*       (time).  Some input data is provided by the Common "Interp".    TSTP  8 */
/* ---    Set parameter values for number of heights, boundary layer      TSTP  9 */
/*       levels, latitudes, longitudes                                   TSTP 10 */
/* ---    MGCM surface data arrays                                        TSTP 21 */
/* ---    MGCM -5 to 80 km data arrays                                    TSTP 37 */
/* ---    Establish MGCM surface values at corners of a 3-dimensional     TSTP 60 */
/*       cube in latitude-longitude-Ls space, at a given height          TSTP 61 */
/*       index (khgt) and time of day (time)                             TSTP 62 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	upolefac = 1.;
	if (testerp_m05__1.ilat == 1) {
	    polefac = i__ - 1.;
	} else if (testerp_m05__1.ilat == 24) {
	    polefac = 2. - i__;
	}
	if (testerp_m05__1.ilatw == 2) {
	    if (i__ == 1) {
		upolefac = testerp_m05__1.wpolefac;
	    }
	} else if (testerp_m05__1.ilatw == 24) {
	    if (i__ == 2) {
		upolefac = testerp_m05__1.wpolefac;
	    }
	}
	for (j = 1; j <= 2; ++j) {
	    for (l = 1; l <= 2; ++l) {
/* ---      Daily mean temperature at level khgt                          TSTP 78 */
		t0 = surftes_m05__1.ttsa0[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
		tday[i__ + (j + (l << 1) << 1) - 7] = t0;
/* ---      Temperature tide amplitudes and phases                        TSTP 81 */
		a1t = surftes_m05__1.ttsa1[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979] * polefac;
		p1t = surftes_m05__1.ttsp1[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
		a2t = surftes_m05__1.ttsa2[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979] * polefac;
		p2t = surftes_m05__1.ttsp2[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
/* ---      Temperature at corners of 3-D cube                            TSTP 86 */
		tm[i__ + (j + (l << 1) << 1) - 7] = tidex_m05__(&t0, &a1t, &
			p1t, &a2t, &p2t, time);
/* ---      Daily mean temperature at surface                             TSTP 88 */
		ts0[i__ + (j + (l << 1) << 1) - 7] = surftes_m05__1.ttsa0[(
			testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.jlon 
			+ j - 1 + (testerp_m05__1.ls + l - 1 + *mtesy * 13) * 
			41) * 25) * 3 - 39978];
/* ---      Max and Min temperatures at corners of 3-D cube               TSTP 90 */
		tmax[i__ + (j + (l << 1) << 1) - 7] = -9999.;
		tmin[i__ + (j + (l << 1) << 1) - 7] = 9999.;
		if (*idaydata > 0) {
		    for (itime = 0; itime <= 23; ++itime) {
			xtime = (real) itime;
			ttime = tidex_m05__(&t0, &a1t, &p1t, &a2t, &p2t, &
				xtime);
			if (ttime > tmax[i__ + (j + (l << 1) << 1) - 7]) {
			    tmax[i__ + (j + (l << 1) << 1) - 7] = ttime;
			}
			if (ttime < tmin[i__ + (j + (l << 1) << 1) - 7]) {
			    tmin[i__ + (j + (l << 1) << 1) - 7] = ttime;
			}
/* L50: */
		    }
		}
/* ---      Daily mean EW wind at level khgt                              TSTP101 */
		u0 = surftes_m05__1.tusa0[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlonw + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
		uday[i__ + (j + (l << 1) << 1) - 7] = u0;
/* ---      EW wind tide coefficient amplitudes and phases                TSTP104 */
		a1 = surftes_m05__1.tusa1[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlonw + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979] * upolefac;
		p1 = surftes_m05__1.tusp1[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlonw + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
		a2 = surftes_m05__1.tusa2[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlonw + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979] * upolefac;
		p2 = surftes_m05__1.tusp2[*khgt + (testerp_m05__1.ilat + i__ 
			- 1 + (testerp_m05__1.jlonw + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
/* ---      EW wind at corners of 3-D cube                                TSTP109 */
		um[i__ + (j + (l << 1) << 1) - 7] = tidex_m05__(&u0, &a1, &p1,
			 &a2, &p2, time);
/* ---      Daily mean NS wind at level khgt                              TSTP111 */
		v0 = surftes_m05__1.tvsa0[*khgt + (testerp_m05__1.ilatw + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
		vday[i__ + (j + (l << 1) << 1) - 7] = v0;
/* ---      NS wind coefficient amplitudes and phases                     TSTP114 */
		a1 = surftes_m05__1.tvsa1[*khgt + (testerp_m05__1.ilatw + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979] * upolefac;
		p1 = surftes_m05__1.tvsp1[*khgt + (testerp_m05__1.ilatw + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
		a2 = surftes_m05__1.tvsa2[*khgt + (testerp_m05__1.ilatw + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979] * upolefac;
		p2 = surftes_m05__1.tvsp2[*khgt + (testerp_m05__1.ilatw + i__ 
			- 1 + (testerp_m05__1.jlon + j - 1 + (
			testerp_m05__1.ls + l - 1 + *mtesy * 13) * 41) * 25) *
			 3 - 39979];
/* ---      NS wind at corners of 3-D cube                                TSTP119 */
		vm[i__ + (j + (l << 1) << 1) - 7] = tidex_m05__(&v0, &a1, &p1,
			 &a2, &p2, time);
/* L101: */
	    }
/* L102: */
	}
/* L103: */
    }
/* ---    Use 3-D interpolation to get temperature, EW wind, NS wind,     TSTP124 */
/*       and daily mean surface temperature at given latitude,           TSTP125 */
/*       longitude, Ls                                                   TSTP126 */
    threed_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dlon, &
	    testerp_m05__1.dls, tm, tmgcm, &c__0);
    threed_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dlonw, &
	    testerp_m05__1.dls, um, umgcm, &c__0);
    threed_m05__(&testerp_m05__1.dlatw, &testerp_m05__1.dlon, &
	    testerp_m05__1.dls, vm, vmgcm, &c__0);
    threed_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dlon, &
	    testerp_m05__1.dls, ts0, &tszero, &c__0);
    threed_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dlon, &
	    testerp_m05__1.dls, tday, tempday, &c__0);
    threed_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dlon, &
	    testerp_m05__1.dls, tmax, tempmax, &c__0);
    threed_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dlon, &
	    testerp_m05__1.dls, tmin, tempmin, &c__0);
    threed_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dlonw, &
	    testerp_m05__1.dls, uday, uwndday, &c__0);
    threed_m05__(&testerp_m05__1.dlatw, &testerp_m05__1.dlon, &
	    testerp_m05__1.dls, vday, vwndday, &c__0);
/* ---    k1h = height index just below k1st                              TSTP136 */
    k1h = testerp_m05__1.k1st - 1;
    if (k1h < 1) {
	k1h = 1;
    }
/* ---    Establish MGCM values at height levels k1h, k1st and corners of TSTP139 */
/*       a 2-dimensional cube in latitude-Ls, at given time of day       TSTP140 */
/*       (time)                                                          TSTP141 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	if (testerp_m05__1.ilat == 1) {
	    polefac = i__ - 1.;
	} else if (testerp_m05__1.ilat == 24) {
	    polefac = 2. - i__;
	}
	for (l = 1; l <= 2; ++l) {
/* ---      Daily average pressure and density at level k1h               TSTP150 */
	    pzh[i__ + (l << 1) - 3] = mgcmtes_m05__1.tpza0[k1h + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    dzh[i__ + (l << 1) - 3] = mgcmtes_m05__1.tdza0[k1h + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    pzh1[i__ + (l << 1) - 3] = mgcmtes_m05__1.tpza0[k1h + 1 + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    dzh1[i__ + (l << 1) - 3] = mgcmtes_m05__1.tdza0[k1h + 1 + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
/* ---      Density tide coefficient amplitudes and phases                TSTP155 */
	    d0 = mgcmtes_m05__1.tdza0[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    a1 = mgcmtes_m05__1.tdza1[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781] * polefac;
	    p1 = mgcmtes_m05__1.tdzp1[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    a2 = mgcmtes_m05__1.tdza2[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781] * polefac;
	    p2 = mgcmtes_m05__1.tdzp2[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
/* ---      Density values at corners of 2-D cube                         TSTP161 */
	    dz1[i__ + (l << 1) - 3] = tidey_m05__(&d0, &a1, &p1, &a2, &p2, 
		    time);
/* ---      Daily average density at level k1st                           TSTP163 */
	    dz0[i__ + (l << 1) - 3] = d0;
/* ---      Level k1st density at corners of 2-D cube                     TSTP165 */
	    dmax__[i__ + (l << 1) - 3] = -9999.;
	    dmin__[i__ + (l << 1) - 3] = 9999.;
	    if (*idaydata > 0) {
		for (itime = 0; itime <= 23; ++itime) {
		    xtime = (real) itime;
		    dtime = tidey_m05__(&d0, &a1, &p1, &a2, &p2, &xtime);
		    if (dtime > dmax__[i__ + (l << 1) - 3]) {
			dmax__[i__ + (l << 1) - 3] = dtime;
		    }
		    if (dtime < dmin__[i__ + (l << 1) - 3]) {
			dmin__[i__ + (l << 1) - 3] = dtime;
		    }
/* L150: */
		}
	    }
/* ---      Temperature tide coefficient amplitudes and phases            TSTP176 */
	    t0 = mgcmtes_m05__1.ttza0[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    a1 = mgcmtes_m05__1.ttza1[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781] * polefac;
	    p1 = mgcmtes_m05__1.ttzp1[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    a2 = mgcmtes_m05__1.ttza2[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781] * polefac;
	    p2 = mgcmtes_m05__1.ttzp2[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
/* ---      temperature values at corners of 2-D cube                     TSTP182 */
	    tz1[i__ + (l << 1) - 3] = tidex_m05__(&t0, &a1, &p1, &a2, &p2, 
		    time);
/* ---      Level k1st Temperature at corners of 2-D cube                 TSTP184 */
	    t1max[i__ + (l << 1) - 3] = -9999.;
	    t1min[i__ + (l << 1) - 3] = 9999.;
	    if (*idaydata > 0) {
		for (itime = 0; itime <= 23; ++itime) {
		    xtime = (real) itime;
		    t1time = tidex_m05__(&t0, &a1, &p1, &a2, &p2, &xtime);
		    if (t1time > t1max[i__ + (l << 1) - 3]) {
			t1max[i__ + (l << 1) - 3] = t1time;
		    }
		    if (t1time < t1min[i__ + (l << 1) - 3]) {
			t1min[i__ + (l << 1) - 3] = t1time;
		    }
/* L160: */
		}
	    }
/* ---      Daily average temperature at level k1st                       TSTP195 */
	    tz0[i__ + (l << 1) - 3] = t0;
/* ---      Daily average pressure at level k1st                          TSTP197 */
	    p0 = mgcmtes_m05__1.tpza0[testerp_m05__1.k1st + (
		    testerp_m05__1.ilat + i__ - 1 + (testerp_m05__1.ls + l - 
		    1 + *mtesy * 13) * 25) * 30 - 9781];
	    pz0[i__ + (l << 1) - 3] = p0;
/* ---      Gas constant from pressure, density, and temperature          TSTP200 */
	    rz0[i__ + (l << 1) - 3] = 190.;
	    if (t0 != 0. && d0 != 0.) {
		rz0[i__ + (l << 1) - 3] = p0 / (t0 * d0);
	    }
/* L201: */
	}
/* L202: */
    }
/* ---    Do 2-D interpolation on pressure                                TSTP205 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, pzh, &pzk1h);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, pzh1, &pzk1h1);
/* ---    Daily average pressure scale height                             TSTP208 */
    dz1h = 1.;
    if (testerp_m05__1.k1st >= 16) {
	dz1h = 5.;
    }
    *hpres0 = dz1h / zlogr_m05__(&pzk1h, &pzk1h1, "TSTP-01", (ftnlen)7);
/* ---    Do 2-D interpolation on density                                 TSTP212 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dzh, &dzk1h);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dzh1, &dzk1h1);
/* ---    Daily average density scale height                              TSTP215 */
    hdens0 = dz1h / zlogr_m05__(&dzk1h, &dzk1h1, "TSTP-02", (ftnlen)7);
/* ---    Do 2-D interpolation on daily mean temperature                  TSTP217 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, tz0, &tzero);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dz0, &dzero);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, pz0, &pzero);
/* ---    Daily average layer mean temperature                            TSTP221 */
    tbar0 = (tzero + tszero) / 2.;
/* ---    Do 2-D interpolation on gas constant                            TSTP223 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, rz0, &rzero);
/* ---    Do 2-D interpolation on temperature and density                 TSTP225 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, tz1, &t1st);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dz1, &d1st);
/* ---    Do 2-D interpolation on max,min pressure at level k1st          TSTP228 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dmax__, &d1max);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, dmin__, &d1min);
/* ---    Do 2-D interpolation on max,min temperature at level k1st       TSTP231 */
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, t1max, &tmax1);
    twod_m05__(&testerp_m05__1.dlat, &testerp_m05__1.dls, t1min, &tmin1);
/* ---    Pressure from gas law                                           TSTP234 */
    p1st = d1st * rzero * t1st;
/* ---    Layer mean temperature at current time                          TSTP236 */
    if (*khgt == 2 && *tat5m <= 0.) {
	*tat5m = *tmgcm;
    }
    tbar = (t1st + *tat5m) / 2.;
/* ---    Pressure scale height and density scale height at current time  TSTP239 */
    *hpres = *hpres0 * tbar / tbar0;
    *hdens = hdens0 * tbar / tbar0;
/* ---    Adjust pressure to height level, using pressure scale height    TSTP242 */
    height = *ctopohgt + mgcmparm_m05__1.dzbl[*khgt - 1];
    z1st = testerp_m05__1.k1st - 6.;
    if (testerp_m05__1.k1st >= 16) {
	z1st = (testerp_m05__1.k1st - 16.) * 5. + 10.;
    }
    *pmgcm = p1st * exp((z1st - height) / *hpres);
    *presday = pzero * exp((z1st - height) / *hpres0);
/* ---    Compute density from gas law, using pressure and temperature    TSTP248 */
    *dmgcm = *pmgcm / (rzero * *tmgcm);
    *densday = *presday / (rzero * *tempday);
/* ---    Daily maximum and minimum density                               TSTP251 */
    *densmin = 9999.;
    *densmax = -9999.;
    if (*idaydata > 0) {
	hdmin = hdens0 * .5 * (tmax1 + *tempmax) / tbar0;
	hdmax = hdens0 * .5 * (tmax1 + *tempmin) / tbar0;
	*densmax = d1max * exp((z1st - height) / hdmax);
	*densmin = d1min * exp((z1st - height) / hdmin);
    }
    return 0;
} /* tessrftrp_m05__ */

/* -----------------------------------------------------------------------TSTP262 */
/* Subroutine */ int testterp_m05__(integer *khgtt, doublereal *time, 
	doublereal *ttgcm, doublereal *ptgcm, doublereal *dtgcm, doublereal *
	utgcm, doublereal *vtgcm, doublereal *zf, doublereal *tempday, 
	doublereal *presday, doublereal *densday, doublereal *uwndday, 
	doublereal *vwndday, doublereal *tempmax, doublereal *tempmin, 
	doublereal *densmax, doublereal *densmin, integer *mtesy, integer *
	idaydata)
{
    static doublereal dmin__[8]	/* was [2][2][2] */, dmax__[8]	/* was [2][2][
	    2] */, pday[8]	/* was [2][2][2] */, tday[8]	/* was [2][2][
	    2] */, uday[8]	/* was [2][2][2] */, vday[8]	/* was [2][2][
	    2] */, tmin[8]	/* was [2][2][2] */, tmax[8]	/* was [2][2][
	    2] */;
    extern doublereal tidex_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static integer i__, l, n;
    static doublereal dtime;
    static integer itime;
    extern /* Subroutine */ int threed_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static doublereal a1, a2, xtime, ttime, d0, rtgcm, p0, r0[8]	/* 
	    was [2][2][2] */, t0, p1, p2;
    extern doublereal ttidey_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal u0, v0, z0, dt[8]	/* was [2][2][2] */, pt[8]	/* 
	    was [2][2][2] */, tt[8]	/* was [2][2][2] */, ut[8]	/* 
	    was [2][2][2] */, vt[8]	/* was [2][2][2] */, zt[8]	/* 
	    was [2][2][2] */, a1p, a2p, p1p, p2p, polefac;

/* ---    Interpolates University of Michigan Mars Thermospheric General  TTTP  4 */
/*       Circulation Model (MTGCM) data to a given latitude, time of     TTTP  5 */
/*       year (Ls), for a given TES year, height index (khgtt) and       TTTP  6 */
/*       time of day (time).                                             TTTP  7 */
/*       Some input data is provided by the Common "Interp".             TTTP  8 */
/* ---    Set parameter values for number of heights (nhgtt), number      TTTP  9 */
/*       of latitudes (nlatt), and number of dust optical depth values   TTTP 10 */
/* ---    MTGCM 80 to 240  km data arrays for interpolation               TTTP 16 */
/* ---    Establish MTGCM values at corners of a 3-dimensional cube in    TTTP 42 */
/*       latitude-Ls-dust-F107 space, at the given height index (khgtt), TTTP 43 */
/*       and time of day (time)                                          TTTP 44 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	if (testerp_m05__1.ilatt == 1) {
	    if (i__ == 1) {
		polefac = testerp_m05__1.tpolefac;
	    }
	} else if (testerp_m05__1.ilatt == 35) {
	    if (i__ == 2) {
		polefac = testerp_m05__1.tpolefac;
	    }
	}
	for (l = 1; l <= 2; ++l) {
	    for (n = 1; n <= 2; ++n) {
/* ---      Daily mean temperature                                        TTTP 54 */
		t0 = tgcmtes_m05__1.ttta0[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		tday[i__ + (l + (n << 1) << 1) - 7] = t0;
/* ---      Temperature tide amplitudes and phases                        TTTP 57 */
		a1 = tgcmtes_m05__1.ttta1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p1 = tgcmtes_m05__1.tttp1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		a2 = tgcmtes_m05__1.ttta2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p2 = tgcmtes_m05__1.tttp2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
/* ---      Temperature at corners of 3-D cube                            TTTP 62 */
		tt[i__ + (l + (n << 1) << 1) - 7] = tidex_m05__(&t0, &a1, &p1,
			 &a2, &p2, time);
/* ---      Max and Min temperatures at corners of 3-D cube               TTTP 64 */
		tmax[i__ + (l + (n << 1) << 1) - 7] = -9999.;
		tmin[i__ + (l + (n << 1) << 1) - 7] = 9999.;
		if (*idaydata > 0) {
		    for (itime = 0; itime <= 23; ++itime) {
			xtime = (real) itime;
			ttime = tidex_m05__(&t0, &a1, &p1, &a2, &p2, &xtime);
			if (ttime > tmax[i__ + (l + (n << 1) << 1) - 7]) {
			    tmax[i__ + (l + (n << 1) << 1) - 7] = ttime;
			}
			if (ttime < tmin[i__ + (l + (n << 1) << 1) - 7]) {
			    tmin[i__ + (l + (n << 1) << 1) - 7] = ttime;
			}
/* L50: */
		    }
		}
/* ---      Daily mean pressure                                           TTTP 75 */
		p0 = tgcmtes_m05__1.tpta0[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		pday[i__ + (l + (n << 1) << 1) - 7] = p0;
/* ---      Pressure tide amplitudes and phases                           TTTP 78 */
		a1p = tgcmtes_m05__1.tpta1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p1p = tgcmtes_m05__1.tptp1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		a2p = tgcmtes_m05__1.tpta2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p2p = tgcmtes_m05__1.tptp2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
/* ---      Pressure at corners of 3-D cube                               TTTP 83 */
		pt[i__ + (l + (n << 1) << 1) - 7] = ttidey_m05__(&p0, &a1p, &
			p1p, &a2p, &p2p, time);
/* ---      Daily mean density                                            TTTP 85 */
		d0 = tgcmtes_m05__1.tdta0[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
/* ---      Density tide amplitudes and phases                            TTTP 87 */
		a1 = tgcmtes_m05__1.tdta1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p1 = tgcmtes_m05__1.tdtp1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		a2 = tgcmtes_m05__1.tdta2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p2 = tgcmtes_m05__1.tdtp2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
/* ---      Density at corners of 3-D cube                                TTTP 92 */
		dt[i__ + (l + (n << 1) << 1) - 7] = ttidey_m05__(&d0, &a1, &
			p1, &a2, &p2, time);
/* ---      Max and Min densities at corners of 3-D cube                  TTTP 94 */
		dmax__[i__ + (l + (n << 1) << 1) - 7] = -9999.;
		dmin__[i__ + (l + (n << 1) << 1) - 7] = 9999.;
		if (*idaydata > 0) {
		    for (itime = 0; itime <= 23; ++itime) {
			xtime = (real) itime;
			dtime = ttidey_m05__(&d0, &a1, &p1, &a2, &p2, &xtime);
			if (dtime > dmax__[i__ + (l + (n << 1) << 1) - 7]) {
			    dmax__[i__ + (l + (n << 1) << 1) - 7] = dtime;
			}
			if (dtime < dmin__[i__ + (l + (n << 1) << 1) - 7]) {
			    dmin__[i__ + (l + (n << 1) << 1) - 7] = dtime;
			}
/* L60: */
		    }
		}
/* ---      Gas constant from pressure, density, and temperature          TTTP105 */
		r0[i__ + (l + (n << 1) << 1) - 7] = 190.;
		if (t0 != 0. && d0 != 0.) {
		    r0[i__ + (l + (n << 1) << 1) - 7] = p0 / (t0 * d0);
		}
/* ---      Daily mean EW wind                                            TTTP108 */
		u0 = tgcmtes_m05__1.tuta0[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		uday[i__ + (l + (n << 1) << 1) - 7] = u0;
/* ---      EW wind tide amplitudes and phases                            TTTP111 */
		a1 = tgcmtes_m05__1.tuta1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p1 = tgcmtes_m05__1.tutp1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		a2 = tgcmtes_m05__1.tuta2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p2 = tgcmtes_m05__1.tutp2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
/* ---      EW wind at corners of 3-D cube                                TTTP116 */
		ut[i__ + (l + (n << 1) << 1) - 7] = tidex_m05__(&u0, &a1, &p1,
			 &a2, &p2, time);
/* ---      Daily mean NS wind                                            TTTP118 */
		v0 = tgcmtes_m05__1.tvta0[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		vday[i__ + (l + (n << 1) << 1) - 7] = v0;
/* ---      NS wind tide amplitudes and phases                            TTTP121 */
		a1 = tgcmtes_m05__1.tvta1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p1 = tgcmtes_m05__1.tvtp1[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
		a2 = tgcmtes_m05__1.tvta2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366] * polefac;
		p2 = tgcmtes_m05__1.tvtp2[*khgtt + (testerp_m05__1.ilatt + 
			i__ - 1 + (testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36) * 33 - 
			46366];
/* ---      NS wind at corners of 3-D cube                                TTTP126 */
		vt[i__ + (l + (n << 1) << 1) - 7] = tidex_m05__(&v0, &a1, &p1,
			 &a2, &p2, time);
/* ---      Tide amplitudes and phases for ZF=height of 1.26 nbar level   TTTP128 */
		z0 = tgcmtes_m05__1.tzfa0[testerp_m05__1.ilatt + i__ - 1 + (
			testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36 - 1405];
		a1 = tgcmtes_m05__1.tzfa1[testerp_m05__1.ilatt + i__ - 1 + (
			testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36 - 1405] 
			* polefac;
		p1 = tgcmtes_m05__1.tzfp1[testerp_m05__1.ilatt + i__ - 1 + (
			testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36 - 1405];
		a2 = tgcmtes_m05__1.tzfa2[testerp_m05__1.ilatt + i__ - 1 + (
			testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36 - 1405] 
			* polefac;
		p2 = tgcmtes_m05__1.tzfp2[testerp_m05__1.ilatt + i__ - 1 + (
			testerp_m05__1.ls + l - 1 + (*mtesy + (
			testerp_m05__1.mf10 + n - 1 << 1)) * 13) * 36 - 1405];
/* ---      ZF values at corners of 3-D cube                              TTTP134 */
		zt[i__ + (l + (n << 1) << 1) - 7] = tidex_m05__(&z0, &a1, &p1,
			 &a2, &p2, time);
/* L99: */
	    }
/* L101: */
	}
/* L102: */
    }
/* ---    Use 3-D interpolation to get temperature, pressure, density,    TTTP139 */
/*       gas constant, EW wind, NS wind, and ZF height at given lati-    TTTP140 */
/*       tude, Ls, dust optical depth, and solar activity                TTTP141 */
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, tt, ttgcm, &c__0);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, tday, tempday, &c__0);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, tmax, tempmax, &c__0);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, tmin, tempmin, &c__0);
    if (*idaydata == 1) {
	threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
		testerp_m05__1.df10, dmax__, densmax, &c__1);
    } else {
	threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
		testerp_m05__1.df10, dmax__, densmax, &c__0);
    }
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, dmin__, densmin, &c__1);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, pt, ptgcm, &c__1);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, pday, presday, &c__1);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, dt, dtgcm, &c__1);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, r0, &rtgcm, &c__0);
/* ---    Daily density from gas constant                                 TTTP152 */
    *densday = *presday / (rtgcm * *tempday);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, ut, utgcm, &c__0);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, uday, uwndday, &c__0);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, vt, vtgcm, &c__0);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, vday, vwndday, &c__0);
    threed_m05__(&testerp_m05__1.dlatt, &testerp_m05__1.dls, &
	    testerp_m05__1.df10, zt, zf, &c__0);
    return 0;
} /* testterp_m05__ */

/* -----------------------------------------------------------------------TTTP161 */
doublereal ttidey_m05__(doublereal *a0, doublereal *a1, doublereal *phi1, 
	doublereal *a2, doublereal *phi2, doublereal *t)
{
    /* System generated locals */
    doublereal ret_val, d__1, d__2, d__3, d__4;

    /* Builtin functions */
    double atan(doublereal), cos(doublereal), pow_dd(doublereal *, doublereal 
	    *);

    /* Local variables */
    static doublereal pi;

/* ---    Tide value at local solar time t, from mean value A0, amplitude TTDY  2 */
/*       A1 and phase phi1 of 24-hour period component, and amplitude A2 TTDY  3 */
/*       and phase phi2 of 12-hour period component.  Amplitudes A1 and  TTDY  4 */
/*       A2 are in relative units (% of mean term A0).  Phases are in    TTDY  5 */
/*       hours of local solar time.                                      TTDY  6 */
/* ---    This form, based on cosine variation of log of tide, allows     TTDY  7 */
/*       amplitudes to exceed 100% without tide going negative (as       TTDY  8 */
/*       required for temperature, density, and pressure).  This form is TTDY  9 */
/*       used for new, higher-altitude MTGCM data, where tidal ampitudes TTDY 10 */
/*       are more likely to get large.                                   TTDY 11 */
    pi = atan(1.) * 4.;
    d__1 = *a1 * .01 + 1.;
    d__2 = cos(pi * (*t - *phi1) / 12.);
    d__3 = *a2 * .01 + 1.;
    d__4 = cos(pi * (*t - *phi2) / 6.);
    ret_val = *a0 * pow_dd(&d__1, &d__2) * pow_dd(&d__3, &d__4);
    return ret_val;
} /* ttidey_m05__ */

/* -----------------------------------------------------------------------TTDY 18 */
/* Subroutine */ int tesgcm_m05__(doublereal *chgt, doublereal *clat, 
	doublereal *clonw, doublereal *als, doublereal *time, doublereal *
	ctemp, doublereal *cpres, doublereal *cdens, doublereal *cuwin, 
	doublereal *cvwin, doublereal *blwindew, doublereal *blwindns, 
	doublereal *blwindvert, doublereal *hpres, doublereal *hdens, 
	doublereal *zf, doublereal *pertfact, doublereal *ctopohgt, 
	doublereal *hgtasfc, doublereal *careoid, doublereal *tempday, 
	doublereal *presday, doublereal *densday, doublereal *ewwnday, 
	doublereal *nswnday, doublereal *bluday, doublereal *blvday, 
	doublereal *tempmax, doublereal *tempmin, doublereal *densmax, 
	doublereal *densmin, doublereal *tgrnd, doublereal *calbedo, integer *
	icepolar, doublereal *tat5m, doublereal *requa, doublereal *rpole, 
	integer *mapyear, integer *idaydata)
{
    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    /* Subroutine */ int s_stop(char *, ftnlen);
    double atan(doublereal), sin(doublereal), log(doublereal), exp(doublereal)
	    , sqrt(doublereal);

    /* Local variables */
    static doublereal dhgt, ofsmult1;
    static integer khgt;
    static doublereal dtdz, tbar, ofsmult2, rgas, uhgt, rref;
    extern doublereal zlogr_m05__(doublereal *, doublereal *, char *, ftnlen);
    static doublereal hpresday, dday1, dday2, dmin1, dmin2, dmax1, dmax2, 
	    rgas1, hgtk1, tsteplat, pday1, pday2, tday1, uday1, vday1, tday2, 
	    uday2, vday2, tmin1, tmin2, tmax1, tmax2, rgas2;
    extern /* Subroutine */ int slopewind_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal z1ofs, ofsz1, z2ofs, ofsz2;
    extern /* Subroutine */ int tessrftrp_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, integer *);
    static doublereal hden12;
    extern doublereal cp_m05__(doublereal *);
    static integer itime;
    static doublereal ddayx;
    extern integer ifloor_m05__(doublereal *);
    static doublereal hdensdayc;
    static integer khgtt, lhgtt;
    static doublereal globoffst, tdayx, pdayx, toprelief, dmgcm1, dmgcm2, z1, 
	    z2, udayx, curoffset, vdayx, tmaxx, tminx, dmaxx, pmgcm1, pmgcm2, 
	    dminx, hdens1, tmgcm1, umgcm1, vmgcm1, tmgcm2, umgcm2, vmgcm2, r1,
	     r2, z0, tsubl, hpres1, z5, zeval, cpoft, albedo, gz, tcheck, 
	    hdensc, factor, dmgcmx, pmgcmx, tmgcmx, umgcmx, vmgcmx, pertlo;
    extern /* Subroutine */ int rellips_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal z2x, offset1, offset2;
    static integer jbl;
    static doublereal flh, gcp, tlat1st;
    static integer loh;
    static doublereal blu, blv, zf80, polefac, gor, blw;
    extern /* Subroutine */ int bltp_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal rgasday, ofsmgcm, oldrref, wavemax, steplat, pertmax;
    extern /* Subroutine */ int subltchk_m05__(doublereal *, doublereal *, 
	    doublereal *);
    static doublereal steplon, topohgt;
    extern /* Subroutine */ int tesgterp_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, integer *), testterp_m05__(
	    integer *, doublereal *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     integer *);

/* ---    Uses interpolation routines to evaluate:                        TESG  7 */
/*                                                                       TESG  8 */
/*       ctemp    = temperature (K) at current position                  TESG  9 */
/*       cpres    = pressure (N/m**2) at current position                TESG 10 */
/*       cdens    = density (kg/m**3) at current position                TESG 11 */
/*       cuwin    = eastward wind component (m/s) at current position    TESG 12 */
/*       cvwin    = northward wind component (m/s) at current position   TESG 13 */
/*       blwinew  = eastward b.l. slope wind (m/s)                       TESG 14 */
/*       blwinns  = northward b.l. slope wind (m/s)                      TESG 15 */
/*       Hpres    = pressure scale height (km) at current position       TESG 16 */
/*       Hdens    = density scale height (km) at current position        TESG 17 */
/*       ZF       = height of 1.26 nbar level at current position        TESG 18 */
/*       pertfact = perturbation factor from random perturbation model   TESG 19 */
/*       ctopohgt = topographic height (km) at current position          TESG 20 */
/*       careoid  = local radius (km) of MOLA 1/2 degree areoid          TESG 21 */
/*       TempDay  = Local daily average temperature (K)                  TESG 22 */
/*       PresDay  = Local daily average pressure (N/m**2)                TESG 23 */
/*       DensDay  = Local daily average density (kg/m**3)                TESG 24 */
/*       EWwnDay  = Local daily average Eastward wind (m/s)              TESG 25 */
/*       NSwnDay  = Local daily average Northward wind (m/s)             TESG 26 */
/*       Tempmax  = Local daily maximum temperature (K)                  TESG 27 */
/*       Tempmin  = Local daily minimum temperature (K)                  TESG 28 */
/*       Densmax  = Local daily maximum density (kg/m**3)                TESG 29 */
/*       Densmin  = Local daily minimum density (kg/m**3)                TESG 30 */
/*       Tgrnd    = ground surface temperature (K)                       TESG 31 */
/*       calbedo  = surface albedo                                       TESG 32 */
/*       icepolar = polar ice indicator (0=no; 1=yes)                    TESG 33 */
/*                                                                       TESG 34 */
/*       at the current height (chgt), latitude (clat), current (West)   TESG 35 */
/*       longitude (clonw), for time of year given by Ls=als, and time   TESG 36 */
/*       of day (time).  Interpolation is done using either boundary     TESG 37 */
/*       layer or -5 to 80 km data from Ames Mars General Circulation    TESG 38 */
/*       model (MGCM) or from 80 to 240 km data from the University of   TESG 39 */
/*       Michigan Mars Thermospheric General Circulation Model (MTGCM).  TESG 40 */
/*                                                                       TESG 41 */
/* ---    Set parameter values for number of MGCM heights (tnhgt), number TESG 42 */
/*       of MTGCM heights (nthgtt), number of MGCM boundary layer levels TESG 43 */
/*       (nbl), number of MGCM latitudes (nlat), number of MTGCM lati-   TESG 44 */
/*       tudes (nlatt), number of MGCM longitudes (nlon), and minimum    TESG 45 */
/*       perturbation magnitude at surface (pert0)                       TESG 46 */
    pertmax = .2;
    *pertfact = 0.;
/* ---    Initialize ground surface temperature and polar ice indicator   TESG 72 */
    *tgrnd = 999.9;
    *icepolar = 99;
/* ---    Insure latitude, longitude, Ls, and time of day within proper   TESG 75 */
/*       bounds                                                          TESG 76 */
    if (abs(*clat) > 90.) {
	s_stop(" Latitude error in TESGCM_M05", (ftnlen)29);
    }
    if (abs(*clonw) > 360.) {
	s_stop(" Longitude error: TESGCM_M05", (ftnlen)28);
    }
    if (*als < 0. || *als > 360.) {
	s_stop(" Ls error: TESGCM_M05", (ftnlen)21);
    }
    if (*time < 0. || *time > 24.) {
	s_stop(" time error in TESGCM_M05", (ftnlen)25);
    }
/* ---    Latitude step size for MGCM and MTGCM data                      TESG 82 */
    steplat = 7.5;
    tsteplat = 5.;
/* ---    Most southerly MTGCM latitude                                   TESG 85 */
    tlat1st = tsteplat / 2. - 90.;
/* ---    Longitude step size for MGCM boundary layer data                TESG 87 */
    steplon = 9.;
/* ---    MGCM height index (khgt) for current height (chgt)              TESG 89 */
    d__1 = *chgt + 6.;
    khgt = ifloor_m05__(&d__1);
    if (khgt > 16) {
	d__1 = (*chgt - 10.) / 5.;
	khgt = ifloor_m05__(&d__1) + 16;
    }
/* ---    Insure khgt within proper limits                                TESG 92 */
    if (khgt < 1) {
	khgt = 1;
    }
    if (khgt > 29) {
	khgt = 29;
    }
/* ---    MGCM latitude index (ilat) from current latitude (clat)         TESG 95 */
    d__1 = (*clat + 90.) / steplat;
    testerp_m05__1.ilat = ifloor_m05__(&d__1) + 1;
    if (testerp_m05__1.ilat > 24) {
	testerp_m05__1.ilat = 24;
    }
/* ---    MGCM wind latitude index (ilatw).  MGCM V winds are offset in   TESG 98 */
/*       lat by 1/2 lat grid step.  No lat offset for U (Arakawa C-grid) TESG 99 */
    d__1 = (*clat + 90. + steplat * 1.5) / steplat;
    testerp_m05__1.ilatw = ifloor_m05__(&d__1);
/* ---    Insure ilatw within proper bounds                               TESG101 */
    if (testerp_m05__1.ilatw < 2) {
	testerp_m05__1.ilatw = 2;
    }
    if (testerp_m05__1.ilatw > 24) {
	testerp_m05__1.ilatw = 24;
    }
/* ---    MTGCM latitude index (ilatt) from current latitude (clat)       TESG104 */
    d__1 = (*clat - tlat1st) / tsteplat;
    testerp_m05__1.ilatt = ifloor_m05__(&d__1) + 1;
/* ---    Insure ilatt within proper bounds                               TESG106 */
    if (testerp_m05__1.ilatt < 1) {
	testerp_m05__1.ilatt = 1;
    }
    if (testerp_m05__1.ilatt > 35) {
	testerp_m05__1.ilatt = 35;
    }
/* ---    MGCM boundary layer longitude index (jlon)                      TESG109 */
    d__1 = *clonw / steplon;
    testerp_m05__1.jlon = ifloor_m05__(&d__1);
    if (testerp_m05__1.jlon > 39) {
	testerp_m05__1.jlon = 39;
    }
/* ---    Lon offset for C-GRID (1/2 step Eastward for U component)       TESG112 */
    d__1 = (*clonw + steplon / 2.) / steplon;
    testerp_m05__1.jlonw = ifloor_m05__(&d__1);
    if (testerp_m05__1.jlonw > 39) {
	testerp_m05__1.jlonw = 0;
    }
/* ---    Time of year index (ls) from input Ls value (als)               TESG115 */
    d__1 = *als / 30.;
    testerp_m05__1.ls = ifloor_m05__(&d__1);
    if (testerp_m05__1.ls > 11) {
	testerp_m05__1.ls = 11;
    }
/* ---    Increment of MGCM latitude (dlat) from grid point               TESG118 */
    testerp_m05__1.dlat = (*clat - steplat * (testerp_m05__1.ilat - 1.) + 90.)
	     / steplat;
/* ---    Increment of MTGCM latitude (dlatt) from grid point             TESG120 */
    testerp_m05__1.dlatt = (*clat - tsteplat * (testerp_m05__1.ilatt - 1.) - 
	    tlat1st) / tsteplat;
/* ---    Insure dlatt within proper bounds near poles                    TESG122 */
    testerp_m05__1.tpolefac = 1.;
    if (testerp_m05__1.ilatt == 1) {
	testerp_m05__1.tpolefac = .5;
	if (testerp_m05__1.dlatt <= 0.) {
	    testerp_m05__1.dlatt = 0.;
	    testerp_m05__1.tpolefac = 1. - (abs(*clat) - 85.) / 5.;
	}
    } else if (testerp_m05__1.ilatt >= 24) {
	testerp_m05__1.tpolefac = .5;
	if (testerp_m05__1.dlatt >= 1.) {
	    testerp_m05__1.dlatt = 1.;
	    testerp_m05__1.tpolefac = 1. - (abs(*clat) - 85.) / 5.;
	}
    }
/* ---    Increment of MGCM longitude (dlon) from grid point              TESG137 */
    testerp_m05__1.dlon = (*clonw - steplon * testerp_m05__1.jlon) / steplon;
    testerp_m05__1.dlonw = (*clonw - steplon * (testerp_m05__1.jlonw - .5)) / 
	    steplon;
    if (testerp_m05__1.dlonw > 40.) {
	testerp_m05__1.dlonw += -40.;
    }
/* ---    Increment of MGCM latitude from (offset) wind grid point        TESG141 */
    testerp_m05__1.dlatw = (*clat - steplat * (testerp_m05__1.ilatw - 2.) + 
	    86.25) / steplat;
    testerp_m05__1.wpolefac = 1.;
    if (testerp_m05__1.ilatw == 2) {
	testerp_m05__1.wpolefac = .75;
	if (testerp_m05__1.dlatw <= 0.) {
	    testerp_m05__1.wpolefac = 1. - (abs(*clat) - 85.) / 5.;
	    testerp_m05__1.dlatw = 0.;
	}
    } else if (testerp_m05__1.ilatw >= 24) {
	testerp_m05__1.wpolefac = .75;
	if (testerp_m05__1.dlatw >= 1.) {
	    testerp_m05__1.wpolefac = 1. - (abs(*clat) - 85.) / 5.;
	    testerp_m05__1.dlatw = 1.;
	}
    }
/* ---    Increment of solar activity (F10.7 at 1AU) for MTGCM data       TESG157 */
    testerp_m05__1.mf10 = 1;
    if (therm_m05__1.f107 > mgcmparm_m05__1.f10tes[1]) {
	testerp_m05__1.mf10 = 2;
    }
    testerp_m05__1.df10 = zlogr_m05__(&therm_m05__1.f107, &
	    mgcmparm_m05__1.f10tes[testerp_m05__1.mf10 - 1], "TESG-01", (
	    ftnlen)7) / zlogr_m05__(&mgcmparm_m05__1.f10tes[
	    testerp_m05__1.mf10], &mgcmparm_m05__1.f10tes[testerp_m05__1.mf10 
	    - 1], "TESG-02", (ftnlen)7);
/* ---    Get areoid radius and topographic height at current lat, lon    TESG162 */
    rellips_m05__(clat, clonw, careoid, chgt, &gz, &oldrref, ctopohgt, 
	    calbedo, requa, rpole);
/* ---    Compute topographic relief factor for simplified mountain       TESG165 */
/*       wave perturbation model                                         TESG166 */
    toprelief = *ctopohgt + 25.;
/* ---    Use topographic height if input height is <= -8.7 km            TESG168 */
    if (*chgt <= -8.7) {
	*chgt = *ctopohgt + *hgtasfc;
    }
/* ---    Find height index (k1st) of first -5 to 80 km MGCM level above  TESG170 */
/*       surface topographic height                                      TESG171 */
    d__1 = *ctopohgt + 7. + .3;
    testerp_m05__1.k1st = ifloor_m05__(&d__1);
    if (testerp_m05__1.k1st >= 16) {
	d__1 = (*ctopohgt + 1.) / 5. + 15.;
	testerp_m05__1.k1st = ifloor_m05__(&d__1);
    }
    if (testerp_m05__1.k1st < 1) {
	testerp_m05__1.k1st = 1;
    }
    hgtk1 = testerp_m05__1.k1st - 6.;
    if (testerp_m05__1.k1st >= 16) {
	hgtk1 = (testerp_m05__1.k1st - 16.) * 5. + 10.;
    }
/* ---    Find Ls increment (dls) from Ls "grid" on input data            TESG177 */
    testerp_m05__1.dls = (*als - testerp_m05__1.ls * 30.) / 30.;
/* ---    Initialize ZF = height of 1.26 nbar level (output value if      TESG179 */
/*       current height < 80 km)                                         TESG180 */
    *zf = 999.;
/* ---    Assign MTGCM height offset from input zoffset or array offsets  TESG182 */
    globoffst = 0.;
    curoffset = 0.;
    if (tgcmoffset_m05__1.ibougher == 2) {
	offset1 = tgcmoffset_m05__1.toffsets[testerp_m05__1.ls + *mapyear * 
		13 - 13];
	offset2 = tgcmoffset_m05__1.toffsets[testerp_m05__1.ls + 1 + *mapyear 
		* 13 - 13];
	globoffst = offset1 + (offset2 - offset1) * testerp_m05__1.dls;
    } else {
	testterp_m05__(&c__1, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, zf, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, mapyear, idaydata);
	testterp_m05__(&c__2, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, zf, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, mapyear, idaydata);
	hdensc = 5. / zlogr_m05__(&dmgcm1, &dmgcm2, "TESG-03", (ftnlen)7);
	hdensdayc = 5. / zlogr_m05__(&dday1, &dday2, "TESG-04", (ftnlen)7);
	tesgterp_m05__(&c__30, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, mapyear, idaydata);
	if (tgcmoffset_m05__1.ibougher == 3 || tgcmoffset_m05__1.ibougher < 2)
		 {
	    curoffset = hdensdayc * zlogr_m05__(&dday2, &dday1, "TESG-05", (
		    ftnlen)7);
	} else {
	    curoffset = hdensc * zlogr_m05__(&dmgcm2, &dmgcm1, "TESG-06", (
		    ftnlen)7);
	}
    }
    if (tgcmoffset_m05__1.ibougher <= 0) {
	tgcmoffset_m05__1.hgtoffset = tgcmoffset_m05__1.zoffset;
    } else if (tgcmoffset_m05__1.ibougher == 1) {
	tgcmoffset_m05__1.hgtoffset = tgcmoffset_m05__1.zoffset - sin(atan(1.)
		 * *als / 45.) * .5;
    } else if (tgcmoffset_m05__1.ibougher == 2) {
	tgcmoffset_m05__1.hgtoffset = globoffst;
    } else {
	tgcmoffset_m05__1.hgtoffset = curoffset;
    }
/* ---    MTGCM height index (khgtt) for current height                   TESG216 */
    d__1 = (*chgt - tgcmoffset_m05__1.hgtoffset - 75.) / 5.;
    khgtt = ifloor_m05__(&d__1);
/* ---    Insure khgtt within proper limits                               TESG218 */
    if (khgtt < 1) {
	khgtt = 1;
    }
    lhgtt = 1;
    if (khgtt == 1 && tgcmoffset_m05__1.hgtoffset < -4.) {
	khgtt = 2;
	lhgtt = 2;
    }
    if (khgtt > 32) {
	khgtt = 32;
    }
/* ---    Initialize MGCM height offset to zero                           TESG226 */
    tgcmoffset_m05__1.ofszl = 0.;
/* ---    Use MTGCM interpolation if height >= 80 km                      TESG228 */
    if (*chgt >= tgcmoffset_m05__1.hgtoffset + 80. + (lhgtt - 1.) * 5.) {
/* ---      Get temperature, pressure, density, and wind components at    TESG230 */
/*         height indexes above and below current height                 TESG231 */
	testterp_m05__(&khgtt, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, zf, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, mapyear, idaydata);
	i__1 = khgtt + 1;
	testterp_m05__(&i__1, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, zf, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, mapyear, idaydata);
/* ---      Height grid points above and below current height             TESG238 */
	z1 = khgtt * 5. + 75. + tgcmoffset_m05__1.hgtoffset;
	z2 = khgtt * 5. + 80. + tgcmoffset_m05__1.hgtoffset;
/* ---      Apply MTGCM height offset to ZF altitude                      TESG241 */
	*zf += tgcmoffset_m05__1.hgtoffset;
/* ---      Pressure and density scale heights                            TESG243 */
	*hpres = (z2 - z1) / zlogr_m05__(&pmgcm1, &pmgcm2, "TESG-07", (ftnlen)
		7);
	hpresday = (z2 - z1) / zlogr_m05__(&pday1, &pday2, "TESG-08", (ftnlen)
		7);
	*hdens = (z2 - z1) / zlogr_m05__(&dmgcm1, &dmgcm2, "TESG-09", (ftnlen)
		7);
	tgcmoffset_m05__1.ofszl = tgcmoffset_m05__1.hgtoffset;
/* ---    Use MGCM interpolation at 75 km and MTGCM interpolation at 80   TESG248 */
/*       km if height between 75 and 80 km                               TESG249 */
    } else if (*chgt >= 75.) {
/* ---      Get temperature, pressure, density, and wind components at    TESG251 */
/*         heights above and below current height                        TESG252 */
	tesgterp_m05__(&khgt, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, mapyear, idaydata);
	testterp_m05__(&lhgtt, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, &zf80, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2,
		 &tmin2, &dmax2, &dmin2, mapyear, idaydata);
	z1 = 75.;
	z2 = tgcmoffset_m05__1.hgtoffset + 80. + (lhgtt - 1.) * 5.;
/* ---      Apply 'equivalent' multiplier for offset between 75 & 80 km   TESG261 */
	if (tgcmoffset_m05__1.ibougher <= 1) {
	    z1ofs = 60.;
	    ofsmgcm = tgcmoffset_m05__1.hgtoffset - curoffset;
	    ofsz1 = ofsmgcm * (z1 - z1ofs) / (z2 - z1ofs);
	    i__1 = khgt + 1;
	    tesgterp_m05__(&i__1, time, &tmgcmx, &pmgcmx, &dmgcmx, &umgcmx, &
		    vmgcmx, &tdayx, &pdayx, &ddayx, &udayx, &vdayx, &tmaxx, &
		    tminx, &dmaxx, &dminx, mapyear, idaydata);
	    hden12 = 5. / log(dmgcm1 / dmgcmx);
	    ofsmult1 = exp(ofsz1 / hden12);
/* ---        Local MGCM height offset                                    TESG271 */
	    tgcmoffset_m05__1.ofszl = ofsmgcm * (*chgt - z1ofs) / (z2 - z1ofs)
		    ;
	    pmgcm1 *= ofsmult1;
	    dmgcm1 *= ofsmult1;
	    pday1 *= ofsmult1;
	    dday1 *= ofsmult1;
	    dmax1 *= ofsmult1;
	    dmin1 *= ofsmult1;
	}
/* ---      Pressure and density scale heights (km)                       TESG280 */
	*hpres = (z2 - z1) / zlogr_m05__(&pmgcm1, &pmgcm2, "TESG-10", (ftnlen)
		7);
	hpresday = (z2 - z1) / zlogr_m05__(&pday1, &pday2, "TESG-11", (ftnlen)
		7);
	*hdens = (z2 - z1) / zlogr_m05__(&dmgcm1, &dmgcm2, "TESG-12", (ftnlen)
		7);
/* ---    Use TESsrftrp_M05 routine if height within boundary layer       TESG284 */
    } else if (*chgt <= *ctopohgt + mgcmparm_m05__1.dzbl[2]) {
/* ---      Set index for surface layer data                              TESG286 */
	jbl = 1;
	if (*chgt >= *ctopohgt + mgcmparm_m05__1.dzbl[1]) {
	    jbl = 2;
	}
/* ---      Get temperature, pressure, density, and wind components at    TESG289 */
/*         heights above and below current height                        TESG290 */
	i__1 = jbl + 1;
	tessrftrp_m05__(&i__1, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, hpres, hdens, ctopohgt, &tday2, &pday2, &dday2, &
		uday2, &vday2, presday, &tmax2, &tmin2, &dmax2, &dmin2, tat5m,
		 mapyear, idaydata);
	tessrftrp_m05__(&jbl, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, &hpres1, &hdens1, ctopohgt, &tday1, &pday1, &dday1, &
		uday1, &vday1, &hpresday, &tmax1, &tmin1, &dmax1, &dmin1, 
		tat5m, mapyear, idaydata);
/* ---      Heights at two boundary layer levels                          TESG299 */
	z1 = *ctopohgt + mgcmparm_m05__1.dzbl[jbl - 1];
	z2 = *ctopohgt + mgcmparm_m05__1.dzbl[jbl];
/* ---      Get Temp at 1st height above BL, for density scale height     TESG302 */
	tesgterp_m05__(&testerp_m05__1.k1st, time, &tmgcmx, &pmgcmx, &dmgcmx, 
		&umgcmx, &vmgcmx, &tdayx, &pdayx, &ddayx, &udayx, &vdayx, &
		tmaxx, &tminx, &dmaxx, &dminx, mapyear, idaydata);
/* ---      Temperature gradient for density scale height calculation     TESG306 */
	z2x = testerp_m05__1.k1st - 6.;
	if (testerp_m05__1.k1st >= 16) {
	    z2x = (testerp_m05__1.k1st - 16.) * 5. + 10.;
	}
	dtdz = (tmgcmx - tmgcm1) / (z2x - z1);
	if (*chgt <= *ctopohgt) {
	    dtdz = 0.;
	}
/* ---      Average layer temperature for density scale height            TESG311 */
	tbar = (tmgcm1 + tmgcm2) / 2.;
/* ---      Density scale height from pressure scale height and           TESG313 */
/*         temperature gradient                                          TESG314 */
	*hdens = *hpres / (*hpres / tbar * dtdz + 1.);
/* ---      Perturbation factor = surface value                           TESG316 */
	*pertfact = .02;
/* ---    Use TESGterp_M05 routine if height above boundary layer levels  TESG318 */
/*        and height <= 75 km                                            TESG319 */
    } else if (*chgt >= hgtk1) {
/* ---      Get temperature, pressure, density, and wind components at    TESG321 */
/*         heights above and below current height                        TESG322 */
	tesgterp_m05__(&khgt, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, mapyear, idaydata);
	i__1 = khgt + 1;
	tesgterp_m05__(&i__1, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, mapyear, idaydata);
/* ---      Heights at grid points above and below current level          TESG329 */
	z1 = khgt - 6.;
	z2 = khgt - 5.;
	if (khgt >= 16) {
	    z1 = (khgt - 16) * 5. + 10.;
	    z2 = (khgt - 16) * 5. + 15.;
	}
/* ---      Apply 'equivalent' multiplier for offset below 75 km          TESG336 */
	if (tgcmoffset_m05__1.ibougher <= 1) {
	    z1ofs = 60.;
	    z2ofs = tgcmoffset_m05__1.hgtoffset + 80. + (lhgtt - 1.) * 5.;
	    ofsmgcm = tgcmoffset_m05__1.hgtoffset - curoffset;
	    if (z1 <= z1ofs) {
		ofsmult1 = 1.;
	    } else {
		ofsz1 = ofsmgcm * (z1 - z1ofs) / (z2ofs - z1ofs);
		hden12 = 5. / log(dmgcm1 / dmgcm2);
		ofsmult1 = exp(ofsz1 / hden12);
	    }
	    if (z2 <= z1ofs) {
		ofsmult2 = 1.;
	    } else {
		ofsz2 = ofsmgcm * (z2 - z1ofs) / (z2ofs - z1ofs);
		hden12 = 5. / log(dmgcm1 / dmgcm2);
		ofsmult2 = exp(ofsz2 / hden12);
	    }
/* ---        Local MGCM height offset                                    TESG355 */
	    if (*chgt > z1ofs) {
		tgcmoffset_m05__1.ofszl = ofsmgcm * (*chgt - z1ofs) / (z2ofs 
			- z1ofs);
	    }
	    pmgcm1 *= ofsmult1;
	    dmgcm1 *= ofsmult1;
	    pmgcm2 *= ofsmult2;
	    dmgcm2 *= ofsmult2;
	    pday1 *= ofsmult1;
	    dday1 *= ofsmult1;
	    dmax1 *= ofsmult1;
	    dmin1 *= ofsmult1;
	    pday2 *= ofsmult2;
	    dday2 *= ofsmult2;
	    dmax2 *= ofsmult2;
	    dmin2 *= ofsmult2;
	}
/* ---      Pressure and density scale heights (km)                       TESG370 */
	*hpres = (z2 - z1) / zlogr_m05__(&pmgcm1, &pmgcm2, "TESG-13", (ftnlen)
		7);
	hpresday = (z2 - z1) / zlogr_m05__(&pday1, &pday2, "TESG-14", (ftnlen)
		7);
	*hdens = (z2 - z1) / zlogr_m05__(&dmgcm1, &dmgcm2, "TESG-15", (ftnlen)
		7);
/* ---    Use TESsrftrp_M05 at top of boundary layer and TESGterp_M05 at  TESG374 */
/*       1st level above boundary layer if height between boundary layer TESG375 */
/*       and height index k1st                                           TESG376 */
    } else {
/* ---      Get temperature, pressure, density, and wind components at    TESG378 */
/*         heights above and below current height                        TESG379 */
	tessrftrp_m05__(&c__3, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, hpres, hdens, ctopohgt, &tday1, &pday1, &dday1, &
		uday1, &vday1, &hpresday, &tmax1, &tmin1, &dmax1, &dmin1, 
		tat5m, mapyear, idaydata);
	tesgterp_m05__(&testerp_m05__1.k1st, time, &tmgcm2, &pmgcm2, &dmgcm2, 
		&umgcm2, &vmgcm2, &tday2, &pday2, &dday2, &uday2, &vday2, &
		tmax2, &tmin2, &dmax2, &dmin2, mapyear, idaydata);
/* ---      Heights at grid points above and below current level          TESG386 */
	z1 = *ctopohgt + mgcmparm_m05__1.dzbl[2];
	z2 = testerp_m05__1.k1st - 6.;
	if (testerp_m05__1.k1st >= 16) {
	    z2 = (testerp_m05__1.k1st - 16.) * 5. + 10.;
	}
/* ---      Temperature gradient and mean temperature for density scale   TESG390 */
/*         height calculation                                            TESG391 */
	dtdz = (tmgcm2 - tmgcm1) / (z2 - z1);
	tbar = (tmgcm1 + tmgcm2) / 2.;
/* ---      Density scale height from pressure scale height and           TESG394 */
/*         temperature gradient                                          TESG395 */
	*hdens = *hpres / (*hpres / tbar * dtdz + 1.);
    }
/* ---    Get gas constant from pressure, density, and temperature        TESG398 */
    if (*chgt <= *ctopohgt) {
	rgas = pmgcm1 / (dmgcm1 * tmgcm1);
	rgasday = pday1 / (dday1 * tday1);
	dhgt = (*ctopohgt - z1) / (z2 - z1);
    } else {
	dhgt = (*chgt - z1) / (z2 - z1);
	r1 = pmgcm1 / (dmgcm1 * tmgcm1);
	r2 = pmgcm2 / (dmgcm2 * tmgcm2);
	rgas1 = pday1 / (dday1 * tday1);
	rgas2 = pday2 / (dday2 * tday2);
	rgas = r1 + dhgt * (r2 - r1);
	rgasday = rgas1 + dhgt * (rgas2 - rgas1);
    }
/* ---    Use logarithmic wind and temperature profiles (with surface     TESG412 */
/*       roughness z0) if height below lowest boundary layer level       TESG413 */
    if (*chgt < *ctopohgt + mgcmparm_m05__1.dzbl[1]) {
/* ---      Convert surface roughness to km                               TESG415 */
	z0 = mgcmparm_m05__1.zwsfc / 1e3;
/* ---      Save ground surface temperature for output                    TESG417 */
	*tgrnd = tmgcm1;
/* ---      Consistent with Ames MGCM, use z0 = 0.01 cm (1.0e-7 km) if    TESG419 */
/*         over ice (T <= CO2 sublimation temperature + 5K)              TESG420 */
	tcheck = tmgcm1;
	subltchk_m05__(&tcheck, &pmgcm2, &tsubl);
/* ---      If surface temperature near sublimation point, set polar ice  TESG423 */
/*           indicator on (= 1) and re-set surface roughness             TESG424 */
	*icepolar = 0;
	if (tmgcm1 <= tsubl + 5.) {
	    z0 = 1e-7;
	    *icepolar = 1;
	}
	uhgt = *chgt - *ctopohgt;
	if (uhgt < z0) {
	    uhgt = z0;
	}
/* ---      Compute logarithmic boundary layer shape factor for surface   TESG432 */
/*         to lowest boundary layer level                                TESG433 */
	factor = zlogr_m05__(&uhgt, &z0, "TESG-16", (ftnlen)7) / zlogr_m05__(&
		mgcmparm_m05__1.dzbl[1], &z0, "TESG-17", (ftnlen)7);
/* ---      Apply factor for wind; assume no-slip condition at surface    TESG436 */
	*cuwin = umgcm2 * factor;
	*cvwin = vmgcm2 * factor;
	*ewwnday = uday2 * factor;
	*nswnday = vday2 * factor;
/* ---      Set up parameters to evaluate temperature boundary layer      TESG441 */
/*         Convert heights to meters for input to bltp_M05 subroutine    TESG442 */
	z5 = mgcmparm_m05__1.dzbl[1] * 1e3;
	zeval = uhgt * 1e3;
/* ---      Get value of local gravity                                    TESG445 */
	rellips_m05__(clat, clonw, &rref, chgt, &gz, &oldrref, &topohgt, &
		albedo, requa, rpole);
/* ---      Use Ames MGCM boundary layer model for current temperature    TESG448 */
/*         Get specific heat at constant pressure                        TESG449 */
	cpoft = cp_m05__(&tmgcm2);
	bltp_m05__(&gz, &cpoft, &tmgcm1, &z5, &tmgcm2, &umgcm2, &vmgcm2, &
		zeval, &factor, ctemp);
/* ---      Use Ames MGCM boundary layer model for daily avg temperature  TESG453 */
	cpoft = cp_m05__(&tday2);
	bltp_m05__(&gz, &cpoft, &tday1, &z5, &tday2, &uday2, &vday2, &zeval, &
		factor, tempday);
/* ---      Use Ames MGCM boundary layer model for daily max temperature  TESG457 */
	cpoft = cp_m05__(&tmax2);
	bltp_m05__(&gz, &cpoft, &tmax1, &z5, &tmax2, &uday2, &vday2, &zeval, &
		factor, tempmax);
/* ---      Use Ames MGCM boundary layer model for daily min temperature  TESG461 */
	cpoft = cp_m05__(&tmin2);
	bltp_m05__(&gz, &cpoft, &tmin1, &z5, &tmin2, &uday2, &vday2, &zeval, &
		factor, tempmin);
/* ---      Pressure at current position from pressure scale height       TESG465 */
	*cpres = pmgcm2 * exp((z2 - *chgt) / *hpres);
	*presday = pday2 * exp((z2 - *chgt) / hpresday);
/* ---      Density at current position from gas law                      TESG468 */
	*cdens = *cpres / (rgas * *ctemp);
	*densday = *presday / (rgasday * *tempday);
	*densmin = 9999.;
	*densmax = -9999.;
	if (*idaydata > 0) {
/* ---      Daily maximum and minimum density                             TESG474 */
	    *densmin = *densday * (dmin1 / dday1 + factor * (dmin2 / dday2 - 
		    dmin1 / dday1));
	    *densmax = *densday * (dmax1 / dday1 + factor * (dmax2 / dday2 - 
		    dmax1 / dday1));
	}
/* ---    Use linear height interpolation if above logarithmic            TESG480 */
/*       surface layer                                                   TESG481 */
    } else {
	dhgt = (*chgt - z1) / (z2 - z1);
	*cuwin = umgcm1 + dhgt * (umgcm2 - umgcm1);
	*cvwin = vmgcm1 + dhgt * (vmgcm2 - vmgcm1);
	*ewwnday = uday1 + dhgt * (uday2 - uday1);
	*nswnday = vday1 + dhgt * (vday2 - vday1);
/* ---      Interpolate temperature to current height                     TESG488 */
	*ctemp = tmgcm1 + dhgt * (tmgcm2 - tmgcm1);
	*tempday = tday1 + dhgt * (tday2 - tday1);
	*tempmax = tmax1 + dhgt * (tmax2 - tmax1);
	*tempmin = tmin1 + dhgt * (tmin2 - tmin1);
/* ---      Pressure at current position from pressure scale height       TESG493 */
	*cpres = pmgcm2 * exp((z2 - *chgt) / *hpres);
	*presday = pday2 * exp((z2 - *chgt) / hpresday);
/* ---      Density at current position from gas law                      TESG496 */
	*cdens = *cpres / (rgas * *ctemp);
	*densday = *presday / (rgasday * *tempday);
	*densmin = 9999.;
	*densmax = -9999.;
	if (*idaydata > 0) {
/* ---      Daily maximum and minimum density                             TESG502 */
	    *densmin = *densday * (dmin1 / dday1 + dhgt * (dmin2 / dday2 - 
		    dmin1 / dday1));
	    *densmax = *densday * (dmax1 / dday1 + dhgt * (dmax2 / dday2 - 
		    dmax1 / dday1));
	}
    }
    if (*chgt < *ctopohgt + .5) {
	if (abs(*clat) >= 85.) {
	    polefac = 1. - (abs(*clat) - 85.) / 5.;
	    *cpres = polefac * *cpres + (1. - polefac) * *presday;
	    *cdens = polefac * *cdens + (1. - polefac) * *densday;
	    *densmin = 9999.;
	    *densmax = -9999.;
	    if (*idaydata > 0) {
		*densmax = polefac * *densmax + (1. - polefac) * *densday;
		*densmin = polefac * *densmin + (1. - polefac) * *densday;
	    }
	}
    }
/* ---    Set specific bogus values of pressure or density scale heights  TESG522 */
/*       are out of range                                                TESG523 */
    if (*hpres < -9.99) {
	*hpres = -9.99;
    }
    if (*hpres > 99.99) {
	*hpres = 99.99;
    }
    if (*hdens < -9.99) {
	*hdens = -9.99;
    }
    if (*hdens > 99.99) {
	*hdens = 99.99;
    }
    wavemax = 1.;
    if ((d__1 = z2 - z1, abs(d__1)) >= 1.) {
	dtdz = (tmgcm2 - tmgcm1) / (z2 - z1);
	loh = 2;
	flh = loh / 6.283185;
/* Computing 2nd power */
	d__1 = flh;
	flh *= sqrt(d__1 * d__1 + 1.);
	gcp = gz * 1e3 / cp_m05__(ctemp);
	gor = gz * 1e3 / rgas;
	if (dtdz < gcp * -.1) {
	    dtdz = gcp * -.1;
	}
	wavemax = flh * (dtdz + gcp) / gor;
	pertlo = *chgt * 4e-4 + .02;
	if (pertlo > .1) {
	    pertlo = .1;
	}
	if (wavemax < pertlo) {
	    wavemax = pertlo;
	}
	if (wavemax < pertmax) {
	    pertmax = wavemax;
	}
    }
/* ---    Compute perturbation factor, unless it has already been set     TESG543 */
    if (*pertfact < .02) {
/* ---      Perturbation factor from simplified mountain wave model       TESG545 */
	*pertfact = toprelief * .01 * exp((*chgt - 100.) / 40.);
	if (*pertfact > pertmax) {
	    *pertfact = pertmax;
	}
	if (*pertfact < *chgt * 2.5e-4 + .02) {
	    *pertfact = *chgt * 2.5e-4 + .02;
	}
	if (*chgt >= 100. && *pertfact < .1) {
	    *pertfact = .1;
	}
    }
/* ---    Get slope winds (0 below surface and > 4.5 km above surface)    TESG552 */
    slopewind_m05__(clat, clonw, chgt, time, cuwin, cvwin, blwindew, blwindns,
	     blwindvert);
/* ---    Compute daily average slope winds                               TESG555 */
    *bluday = 0.;
    *blvday = 0.;
    for (itime = 0; itime <= 22; itime += 2) {
	d__1 = (doublereal) itime;
	slopewind_m05__(clat, clonw, chgt, &d__1, cuwin, cvwin, &blu, &blv, &
		blw);
	*bluday += blu;
	*blvday += blv;
/* L50: */
    }
    *bluday /= 12.;
    *blvday /= 12.;
    return 0;
} /* tesgcm_m05__ */

/* -----------------------------------------------------------------------TESG568 */
/* Subroutine */ int profterp_m05__(doublereal *chgt, doublereal *clat, 
	doublereal *clon, doublereal *tin, doublereal *pin, doublereal *din, 
	doublereal *uin, doublereal *vin, doublereal *ptemp, doublereal *
	ppres, doublereal *pdens, doublereal *puwin, doublereal *pvwin, 
	integer *nprof, doublereal *profnear, doublereal *proffar, doublereal 
	*profwgt, doublereal *wavepert)
{
    /* Initialized data */

    static integer ia[2] = { 0,0 };
    static doublereal adll[2] = { 0.,0. };

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;

    /* Builtin functions */
    double atan(doublereal);
    /* Subroutine */ int s_stop(char *, ftnlen);
    double sqrt(doublereal), pow_dd(doublereal *, doublereal *), sin(
	    doublereal);

    /* Local variables */
    static doublereal dlat, dlon, dlat1, dlat2, dlon1, dlon2;
    static integer i__;
    static doublereal pilat, dplon, pilon;
    static integer i1, i2;
    static doublereal uvwgt;
    static integer ni;
    static doublereal factll, factor, radius, tpdwgt, pi2, radius1, radius2, 
	    facthgt;

/* ---    Interpolates profile data to current position (chgt,clat,clon)  PTRP  3 */
/*       and weights results (with factor profwgt) with input values     PTRP  4 */
/*       (tin,pin,din,uin,vin), yielding weighted average (ptemp,ppres,  PTRP  5 */
/*       pdens,puwin,pvwin).  Input profnear is lat-lon radius over      PTRP  6 */
/*       which profile is weighted with 1.0; proffar is lat-lon radius   PTRP  7 */
/*       beyond which profile is given zero weight.                      PTRP  8 */
/* ---    Calculate pi/2                                                  PTRP 13 */
    pi2 = 2. * atan(1.);
    i1 = 0;
    i2 = 0;
    ni = 0;
    *profwgt = (float)0.;
/* ---    Find nearest pair of points above and below current height      PTRP 18 */
    i__1 = *nprof - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	if ((*chgt - pterp_m05__1.phgt[i__ - 1]) * (*chgt - pterp_m05__1.phgt[
		i__]) < 0. || *chgt == pterp_m05__1.phgt[i__ - 1]) {
	    ++ni;
	    if (ni > 2) {
		s_stop(" Too many height pairs in profile", (ftnlen)33);
	    }
	    ia[ni - 1] = i__;
	    dlat1 = (d__1 = *clat - pterp_m05__1.plat[i__ - 1], abs(d__1));
	    dlat2 = (d__1 = *clat - pterp_m05__1.plat[i__], abs(d__1));
	    dlon1 = (d__1 = *clon - pterp_m05__1.plon[i__ - 1], abs(d__1));
	    dlon2 = (d__1 = *clon - pterp_m05__1.plon[i__], abs(d__1));
/* ---        Adjust lon difference for wrap at lon 360                   PTRP 24 */
	    if (dlon1 > 180.) {
		dlon1 = 360. - dlon1;
	    }
	    if (dlon2 > 180.) {
		dlon2 = 360. - dlon2;
	    }
/* ---        Lat-lon radius from positions of points i1 and i2           PTRP 27 */
/* Computing 2nd power */
	    d__1 = dlat1;
/* Computing 2nd power */
	    d__2 = dlon1;
	    radius1 = sqrt(d__1 * d__1 + d__2 * d__2);
/* Computing 2nd power */
	    d__1 = dlat2;
/* Computing 2nd power */
	    d__2 = dlon2;
	    radius2 = sqrt(d__1 * d__1 + d__2 * d__2);
	    adll[ni - 1] = (radius1 + radius2) / 2.;
	}
/* L20: */
    }
    if (ni > 0) {
	i1 = ia[0];
	if (ni == 2 && adll[1] < adll[0]) {
	    i1 = ia[1];
	}
	i2 = i1 + 1;
    }
    if (i1 == 0) {
	*pdens = 0.;
	*puwin = 0.;
	*pvwin = 0.;
	goto L50;
    }
/* ---    Compute factor for linear height interpolation                  PTRP 45 */
    factor = (*chgt - pterp_m05__1.phgt[i1 - 1]) / (pterp_m05__1.phgt[i2 - 1] 
	    - pterp_m05__1.phgt[i1 - 1]);
/* ---    Linear height interpolation for lat,lon,temperature,winds       PTRP 47 */
    pilat = pterp_m05__1.plat[i1 - 1] + factor * (pterp_m05__1.plat[i2 - 1] - 
	    pterp_m05__1.plat[i1 - 1]);
    dplon = pterp_m05__1.plon[i2 - 1] - pterp_m05__1.plon[i1 - 1];
    if (dplon > 180.) {
	dplon += -360.;
    }
    if (dplon < -180.) {
	dplon += 360.;
    }
    pilon = pterp_m05__1.plon[i1 - 1] + factor * dplon;
    *ptemp = pterp_m05__1.ptmp[i1 - 1] + factor * (pterp_m05__1.ptmp[i2 - 1] 
	    - pterp_m05__1.ptmp[i1 - 1]);
    *puwin = pterp_m05__1.puwn[i1 - 1] + factor * (pterp_m05__1.puwn[i2 - 1] 
	    - pterp_m05__1.puwn[i1 - 1]);
    *pvwin = pterp_m05__1.pvwn[i1 - 1] + factor * (pterp_m05__1.pvwn[i2 - 1] 
	    - pterp_m05__1.pvwn[i1 - 1]);
/* ---    Power-law interpolation for density (unless profile density     PTRP 53 */
/*       is zero, for which zero weight will be used)                    PTRP 54 */
    *pdens = 0.;
    if (pterp_m05__1.pden[i1 - 1] > 0.) {
	d__1 = pterp_m05__1.pden[i2 - 1] / pterp_m05__1.pden[i1 - 1];
	*pdens = pterp_m05__1.pden[i1 - 1] * pow_dd(&d__1, &factor);
    }
/* ---    Power-law interpolation for pressure (unless profile pressure   PTRP 58 */
/*       is zero, for which zero weight will be used)                    PTRP 59 */
    *ppres = 0.;
    if (pterp_m05__1.pprs[i1 - 1] > 0.) {
	d__1 = pterp_m05__1.pprs[i2 - 1] / pterp_m05__1.pprs[i1 - 1];
	*ppres = pterp_m05__1.pprs[i1 - 1] * pow_dd(&d__1, &factor);
    }
/* ---    Initialize weighting factor components for height and lat-lon   PTRP 63 */
    facthgt = 1.;
    factll = 1.;
    if (i1 == 1) {
/* ---    Sine-squared variation of height weighting from 0 at 1st point  PTRP 67 */
/*       to 1 at 2nd point                                               PTRP 68 */
	facthgt = (*chgt - pterp_m05__1.phgt[0]) / (pterp_m05__1.phgt[1] - 
		pterp_m05__1.phgt[0]);
/* Computing 2nd power */
	d__1 = sin(pi2 * facthgt);
	facthgt = d__1 * d__1;
    } else if (i2 == *nprof) {
/* ---    Sine-squared variation of height weighting from 0 at next-to-   PTRP 72 */
/*       last point to 1 at last point                                   PTRP 73 */
	facthgt = (*chgt - pterp_m05__1.phgt[*nprof - 1]) / (
		pterp_m05__1.phgt[*nprof - 2] - pterp_m05__1.phgt[*nprof - 1])
		;
/* Computing 2nd power */
	d__1 = sin(pi2 * facthgt);
	facthgt = d__1 * d__1;
    }
/* ---    Compute absolute lat-lon difference of current position from    PTRP 77 */
/*       profile lat-lon                                                 PTRP 78 */
    dlat = (d__1 = *clat - pilat, abs(d__1));
    dlon = (d__1 = *clon - pilon, abs(d__1));
/* ---    Adjust lon difference for wrap at lon 360                       PTRP 81 */
    if (dlon > 180.) {
	dlon = 360. - dlon;
    }
/* ---    Lat-lon radius of current position from profile lat-lon         PTRP 83 */
/* Computing 2nd power */
    d__1 = dlat;
/* Computing 2nd power */
    d__2 = dlon;
    radius = sqrt(d__1 * d__1 + d__2 * d__2);
/* ---    Use weight=0 if radius>proffar, weight=1 if radius<profnear,    PTRP 85 */
/*       with sine-squared variation between proffar and profnear        PTRP 86 */
    if (radius >= *proffar) {
	factll = 0.;
    } else if (radius <= *profnear) {
	factll = 1.;
    } else {
	factll = (*proffar - radius) / (*proffar - *profnear);
/* Computing 2nd power */
	d__1 = sin(pi2 * factll);
	factll = d__1 * d__1;
    }
/* ---    Total weight = product of weights for lat-lon and height        PTRP 95 */
    *profwgt = factll * facthgt;
L50:
    tpdwgt = *profwgt;
    uvwgt = *profwgt;
/* ---    Set profile weight to zero for p,d, & t if profile values are 0 PTRP 99 */
    if (*ptemp * *ppres * *pdens == 0.) {
	tpdwgt = 0.;
    }
/* ---    Set profile weight to zero for u & v if profile values are 0    PTRP101 */
    if (abs(*puwin) + abs(*pvwin) == 0.) {
	uvwgt = 0.;
    }
/* ---    Apply wave perturbation effect to profile values                PTRP102a */
    *ppres *= *wavepert + (float)1.;
    *pdens *= *wavepert + (float)1.;
/* ---    Apply weighted averaging of profile values with input values    PTRP103 */
    *ptemp = tpdwgt * *ptemp + (1. - tpdwgt) * *tin;
    *ppres = tpdwgt * *ppres + (1. - tpdwgt) * *pin;
    *pdens = tpdwgt * *pdens + (1. - tpdwgt) * *din;
    *puwin = uvwgt * *puwin + (1. - uvwgt) * *uin;
    *pvwin = uvwgt * *pvwin + (1. - uvwgt) * *vin;
    return 0;
} /* profterp_m05__ */

/* ---------------------------------------------------------------------- PTRP111 */
/* Subroutine */ int rdprof_m05__(char *profile, integer *nprof, integer *
	loneast, ftnlen profile_len)
{
    /* Format strings */
    static char fmt_5[] = "(a1)";

    /* System generated locals */
    integer i__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen), f_open(olist *), s_rsfe(
	    cilist *), do_fio(integer *, char *, ftnlen), e_rsfe(), s_rsle(
	    cilist *), do_lio(integer *, integer *, char *, ftnlen), e_rsle(),
	     s_wsle(cilist *), e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer f_clos(cllist *);

    /* Local variables */
    static doublereal xlat, zhgt, xlon, d__;
    static integer n;
    static doublereal p, t, u, v;
    static char dummy[1];
    static integer lenprof;

    /* Fortran I/O blocks */
    static cilist io___337 = { 0, 33, 0, fmt_5, 0 };
    static cilist io___340 = { 0, 33, 1, 0, 0 };
    static cilist io___349 = { 0, 6, 0, 0, 0 };


/* ---    Reads alternate profile data file profile. Returns number of    RDPF  2 */
/*       lines of data (nprof).  Converts input longitudes from East to  RDPF  3 */
/*       West if LonEast = 1                                             RDPF  4 */
/* ---    Compute string length for profile file name                     RDPF 11 */
    lenprof = i_indx(profile, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lenprof < 1 || lenprof > 60) {
	lenprof = 60;
    }
/* ---    Open profile data file                                          RDPF 14 */
    o__1.oerr = 0;
    o__1.ounit = 33;
    o__1.ofnmlen = lenprof;
    o__1.ofnm = profile;
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = 0;
    o__1.oblnk = 0;
    f_open(&o__1);
/* ---    Read and ignore header line                                     RDPF 16 */
    s_rsfe(&io___337);
    do_fio(&c__1, dummy, (ftnlen)1);
    e_rsfe();
    n = 0;
/* ---    Start of loop to read profile data                              RDPF 20 */
L10:
    i__1 = s_rsle(&io___340);
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&zhgt, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&xlat, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&xlon, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&t, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&p, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&d__, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&u, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = do_lio(&c__5, &c__1, (char *)&v, (ftnlen)sizeof(doublereal));
    if (i__1 != 0) {
	goto L99;
    }
    i__1 = e_rsle();
    if (i__1 != 0) {
	goto L99;
    }
/* ---    Convert negative longitudes                                     RDPF 22 */
    if (xlon < 0.) {
	xlon += 360.;
    }
/* ---    Convert to West Longitude if LonEast = 1                        RDPF 24 */
    if (*loneast == 1) {
	xlon = 360. - xlon;
    }
/* ---    Count number of lines read                                      RDPF 26 */
    ++n;
/* ---    Store profile data in arrays, for common pterp                  RDPF 28 */
    pterp_m05__1.phgt[n - 1] = zhgt;
/* ---    Stop if two successive heights are the same                     RDPF 30 */
    if (n > 1 && pterp_m05__1.phgt[n - 1] == pterp_m05__1.phgt[n - 2]) {
	s_wsle(&io___349);
	do_lio(&c__3, &c__1, (char *)&n, (ftnlen)sizeof(integer));
	do_lio(&c__5, &c__1, (char *)&pterp_m05__1.phgt[n - 1], (ftnlen)
		sizeof(doublereal));
	e_wsle();
	s_stop(" Consecutive profile heights cannot be same", (ftnlen)43);
    }
    pterp_m05__1.plat[n - 1] = xlat;
    pterp_m05__1.plon[n - 1] = xlon;
    pterp_m05__1.ptmp[n - 1] = t;
    pterp_m05__1.pprs[n - 1] = p;
    pterp_m05__1.pden[n - 1] = d__;
    pterp_m05__1.puwn[n - 1] = u;
    pterp_m05__1.pvwn[n - 1] = v;
    *nprof = n;
/* ---    Cycle back to read another line of profile data                 RDPF 41 */
    goto L10;
/* ---    Close profile input file when end-of-file encountered           RDPF 43 */
L99:
    cl__1.cerr = 0;
    cl__1.cunit = 33;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* rdprof_m05__ */

#ifdef __cplusplus
	}
#endif
