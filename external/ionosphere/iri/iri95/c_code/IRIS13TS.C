/* iris13ts.f -- translated by f2c (version 19990311).
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
static integer c__8 = 8;

/* Main program */ MAIN__()
{
    /* Initialized data */

    static char imz[4*8+1] = "    GEODGEOD            YEARL.T.";
    static char itext[5*8+1] = " H/KM LATI LONG YEARMONTH  DAYDAYOF HOUR";

    /* Format strings */
    static char fmt_8194[] = "(//////////3x,a5,4x,\002PEAK ALTITUDES IN K\
M\002,7x,\002PLASMA F\002,\002REQUENCIES IN MHz\002/4x,a4,\002    hmF2  hmF1\
   hmE   hmD   \002,\002   foF2   foF1    foE    foD\002)";
    static char fmt_8192[] = "(//////////3x,a5,4x,\002PEAK ALTITUDES IN K\
M\002,8x,\002PEAK DEN\002,\002SITIES IN CM-3\002/4x,a4,\002    hmF2  hmF1   \
hmE   hmD   \002,\002   NmF2   NmF1    NmE    NmD\002)";
    static char fmt_8193[] = "(//////////3x,a5,\002 ELECTRON DENSITY   TEMPE\
RATURES \002,7x,\002ION PERCENTAGES/%\002/4x,a4,\002 Ne/CM-3 Ne/NmF2\002,\
\002 Tn/K  Ti/K  Te/K      O+  N+  H+ He+ O2+ NO+ Clust\002)";
    static char fmt_3910[] = "(1x,f7.1,2x,4f6.1,3x,4i7)";
    static char fmt_3919[] = "(1x,f7.1,2x,4f6.1,3x,4f7.2)";
    static char fmt_7117[] = "(1x,f7.1,i8,f7.4,3i6,i8,6i4)";

    /* System generated locals */
    integer i__1;
    real r__1;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(), s_rsle(cilist *), e_rsle();
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    double sqrt(doublereal);
    /* Subroutine */ int s_stop(char *, ftnlen);

    /* Local variables */
    static real vbeg, vend;
    static integer ivar;
    static real xlat, xcor, hour, xlon;
    static char stex[4];
    static real vstp;
    static integer i__, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11;
    static logical jf[17];
    static integer li, jm;
    static real hx;
    static integer iy;
    static real piktab;
    static integer numstp;
    static real yp1, yp2, yp3, yp4;
    static integer imd;
    static real oar[3500]	/* was [35][100] */;
    extern /* Subroutine */ int iri_web__(integer *, logical *, real *, real *
	    , integer *, integer *, integer *, real *, real *, integer *, 
	    real *, real *, real *, real *, real *);
    static integer iut;
    static real out[1100]	/* was [11][100] */;
    static integer iyp1, iyp2, iyp3, iyp4;

    /* Fortran I/O blocks */
    static cilist io___3 = { 0, 6, 0, 0, 0 };
    static cilist io___4 = { 0, 5, 0, 0, 0 };
    static cilist io___13 = { 0, 6, 0, 0, 0 };
    static cilist io___14 = { 0, 5, 0, 0, 0 };
    static cilist io___19 = { 0, 6, 0, 0, 0 };
    static cilist io___20 = { 0, 6, 0, 0, 0 };
    static cilist io___21 = { 0, 6, 0, 0, 0 };
    static cilist io___22 = { 0, 5, 0, 0, 0 };
    static cilist io___24 = { 0, 6, 0, 0, 0 };
    static cilist io___25 = { 0, 5, 0, 0, 0 };
    static cilist io___26 = { 0, 6, 0, 0, 0 };
    static cilist io___27 = { 0, 5, 0, 0, 0 };
    static cilist io___28 = { 0, 6, 0, 0, 0 };
    static cilist io___29 = { 0, 5, 0, 0, 0 };
    static cilist io___30 = { 0, 6, 0, 0, 0 };
    static cilist io___31 = { 0, 5, 0, 0, 0 };
    static cilist io___32 = { 0, 6, 0, 0, 0 };
    static cilist io___33 = { 0, 5, 0, 0, 0 };
    static cilist io___34 = { 0, 6, 0, 0, 0 };
    static cilist io___35 = { 0, 5, 0, 0, 0 };
    static cilist io___36 = { 0, 6, 0, 0, 0 };
    static cilist io___37 = { 0, 5, 0, 0, 0 };
    static cilist io___38 = { 0, 6, 0, 0, 0 };
    static cilist io___39 = { 0, 5, 0, 0, 0 };
    static cilist io___40 = { 0, 6, 0, 0, 0 };
    static cilist io___41 = { 0, 5, 0, 0, 0 };
    static cilist io___42 = { 0, 6, 0, 0, 0 };
    static cilist io___43 = { 0, 5, 0, 0, 0 };
    static cilist io___44 = { 0, 6, 0, 0, 0 };
    static cilist io___45 = { 0, 5, 0, 0, 0 };
    static cilist io___46 = { 0, 6, 0, 0, 0 };
    static cilist io___47 = { 0, 5, 0, 0, 0 };
    static cilist io___48 = { 0, 6, 0, 0, 0 };
    static cilist io___49 = { 0, 5, 0, 0, 0 };
    static cilist io___50 = { 0, 6, 0, 0, 0 };
    static cilist io___51 = { 0, 5, 0, 0, 0 };
    static cilist io___52 = { 0, 6, 0, 0, 0 };
    static cilist io___53 = { 0, 5, 0, 0, 0 };
    static cilist io___55 = { 0, 6, 0, 0, 0 };
    static cilist io___56 = { 0, 5, 0, 0, 0 };
    static cilist io___57 = { 0, 6, 0, 0, 0 };
    static cilist io___58 = { 0, 5, 0, 0, 0 };
    static cilist io___59 = { 0, 6, 0, 0, 0 };
    static cilist io___60 = { 0, 5, 0, 0, 0 };
    static cilist io___61 = { 0, 6, 0, 0, 0 };
    static cilist io___62 = { 0, 5, 0, 0, 0 };
    static cilist io___63 = { 0, 6, 0, 0, 0 };
    static cilist io___64 = { 0, 5, 0, 0, 0 };
    static cilist io___65 = { 0, 6, 0, 0, 0 };
    static cilist io___66 = { 0, 5, 0, 0, 0 };
    static cilist io___67 = { 0, 6, 0, 0, 0 };
    static cilist io___68 = { 0, 5, 0, 0, 0 };
    static cilist io___74 = { 0, 6, 0, fmt_8194, 0 };
    static cilist io___75 = { 0, 6, 0, fmt_8192, 0 };
    static cilist io___76 = { 0, 6, 0, fmt_8193, 0 };
    static cilist io___83 = { 0, 6, 0, fmt_3910, 0 };
    static cilist io___88 = { 0, 6, 0, fmt_3919, 0 };
    static cilist io___100 = { 0, 6, 0, fmt_7117, 0 };


L1:
    s_wsle(&io___3);
    do_lio(&c__9, &c__1, "jmag,lati,long,year,mmdd(-ddd),iut(1=UT),hour,hx", (
	    ftnlen)48);
    e_wsle();
    s_rsle(&io___4);
    do_lio(&c__3, &c__1, (char *)&jm, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&xlat, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&xlon, (ftnlen)sizeof(real));
    do_lio(&c__3, &c__1, (char *)&iy, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&imd, (ftnlen)sizeof(integer));
    do_lio(&c__3, &c__1, (char *)&iut, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&hour, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&hx, (ftnlen)sizeof(real));
    e_rsle();
    if (hour < (float)0.) {
	goto L2;
    }
    s_wsle(&io___13);
    do_lio(&c__9, &c__1, "ivar(h,lat,lon,y,m,d,doy,t),vbeg,vend,vstp", (
	    ftnlen)42);
    e_wsle();
    s_rsle(&io___14);
    do_lio(&c__3, &c__1, (char *)&ivar, (ftnlen)sizeof(integer));
    do_lio(&c__4, &c__1, (char *)&vbeg, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vend, (ftnlen)sizeof(real));
    do_lio(&c__4, &c__1, (char *)&vstp, (ftnlen)sizeof(real));
    e_rsle();
    s_wsle(&io___19);
    do_lio(&c__9, &c__1, "Enter t for .true. or f for .false. to select vari\
ous", (ftnlen)53);
    do_lio(&c__9, &c__1, " options.", (ftnlen)9);
    e_wsle();
    s_wsle(&io___20);
    do_lio(&c__9, &c__1, "Standard is: t,t,t,t,f,t,t,t,t,t,t,t,t,t,t,t,t", (
	    ftnlen)46);
    e_wsle();
    s_wsle(&io___21);
    do_lio(&c__9, &c__1, "Compute Ne, T, Ni? (enter: t,t,t  if you want all)",
	     (ftnlen)50);
    e_wsle();
    s_rsle(&io___22);
    do_lio(&c__8, &c__1, (char *)&jf[0], (ftnlen)sizeof(logical));
    do_lio(&c__8, &c__1, (char *)&jf[1], (ftnlen)sizeof(logical));
    do_lio(&c__8, &c__1, (char *)&jf[2], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___24);
    do_lio(&c__9, &c__1, "Bottomside thickness B0: t=Table-option, f=Gulyaev\
a.", (ftnlen)52);
    e_wsle();
    s_rsle(&io___25);
    do_lio(&c__8, &c__1, (char *)&jf[3], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___26);
    do_lio(&c__9, &c__1, "foF2 model: t=CCIR, f=URSI-88", (ftnlen)29);
    e_wsle();
    s_rsle(&io___27);
    do_lio(&c__8, &c__1, (char *)&jf[4], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___28);
    do_lio(&c__9, &c__1, "Ion comp. model: t=standard, f=Danilov-Yaichnikov-\
Smirnova", (ftnlen)58);
    e_wsle();
    s_rsle(&io___29);
    do_lio(&c__8, &c__1, (char *)&jf[5], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___30);
    do_lio(&c__9, &c__1, "Ne Topside mod.: t=standard, f=IRI-79 topside", (
	    ftnlen)45);
    e_wsle();
    s_rsle(&io___31);
    do_lio(&c__8, &c__1, (char *)&jf[6], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___32);
    do_lio(&c__9, &c__1, "F2 peak density or foF2: t=model, f=user input.", (
	    ftnlen)47);
    e_wsle();
    s_rsle(&io___33);
    do_lio(&c__8, &c__1, (char *)&jf[7], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___34);
    do_lio(&c__9, &c__1, "F2 peak height or M3000F2: t=model, f=user input.", 
	    (ftnlen)49);
    e_wsle();
    s_rsle(&io___35);
    do_lio(&c__8, &c__1, (char *)&jf[8], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___36);
    do_lio(&c__9, &c__1, "Te(Ne) model: t=not used, f=correlation is used.", (
	    ftnlen)48);
    e_wsle();
    s_rsle(&io___37);
    do_lio(&c__8, &c__1, (char *)&jf[9], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___38);
    do_lio(&c__9, &c__1, "LAY version: t=standard ver., f=LAY version.", (
	    ftnlen)44);
    e_wsle();
    s_rsle(&io___39);
    do_lio(&c__8, &c__1, (char *)&jf[10], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___40);
    do_lio(&c__9, &c__1, "Message output unit: t=(UNIT=6), f=(UNIT=12).", (
	    ftnlen)45);
    e_wsle();
    s_rsle(&io___41);
    do_lio(&c__8, &c__1, (char *)&jf[11], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___42);
    do_lio(&c__9, &c__1, "F1 peak density or foF1: t=model, f=user input.", (
	    ftnlen)47);
    e_wsle();
    s_rsle(&io___43);
    do_lio(&c__8, &c__1, (char *)&jf[12], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___44);
    do_lio(&c__9, &c__1, "F1 peak height: t=model, f=user input.", (ftnlen)38)
	    ;
    e_wsle();
    s_rsle(&io___45);
    do_lio(&c__8, &c__1, (char *)&jf[13], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___46);
    do_lio(&c__9, &c__1, "E peak density or foE: t=model, f=user input.", (
	    ftnlen)45);
    e_wsle();
    s_rsle(&io___47);
    do_lio(&c__8, &c__1, (char *)&jf[14], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___48);
    do_lio(&c__9, &c__1, "E peak height: t=model, f=user input.", (ftnlen)37);
    e_wsle();
    s_rsle(&io___49);
    do_lio(&c__8, &c__1, (char *)&jf[15], (ftnlen)sizeof(logical));
    e_rsle();
    s_wsle(&io___50);
    do_lio(&c__9, &c__1, "Sunspot index: t=from file, f=user input.", (ftnlen)
	    41);
    e_wsle();
    s_rsle(&io___51);
    do_lio(&c__8, &c__1, (char *)&jf[16], (ftnlen)sizeof(logical));
    e_rsle();
    if (ivar == 1) {
	if (! jf[7]) {
	    s_wsle(&io___52);
	    do_lio(&c__9, &c__1, "foF2 or NmF2", (ftnlen)12);
	    e_wsle();
	    s_rsle(&io___53);
	    do_lio(&c__4, &c__1, (char *)&oar[0], (ftnlen)sizeof(real));
	    e_rsle();
	}
	if (! jf[8]) {
	    s_wsle(&io___55);
	    do_lio(&c__9, &c__1, "hmF2 or M3000F2", (ftnlen)15);
	    e_wsle();
	    s_rsle(&io___56);
	    do_lio(&c__4, &c__1, (char *)&oar[1], (ftnlen)sizeof(real));
	    e_rsle();
	}
	if (! jf[12]) {
	    s_wsle(&io___57);
	    do_lio(&c__9, &c__1, "foF1 or NmF1", (ftnlen)12);
	    e_wsle();
	    s_rsle(&io___58);
	    do_lio(&c__4, &c__1, (char *)&oar[2], (ftnlen)sizeof(real));
	    e_rsle();
	}
	if (! jf[13]) {
	    s_wsle(&io___59);
	    do_lio(&c__9, &c__1, "hmF1", (ftnlen)4);
	    e_wsle();
	    s_rsle(&io___60);
	    do_lio(&c__4, &c__1, (char *)&oar[3], (ftnlen)sizeof(real));
	    e_rsle();
	}
	if (! jf[14]) {
	    s_wsle(&io___61);
	    do_lio(&c__9, &c__1, "foE or NmE", (ftnlen)10);
	    e_wsle();
	    s_rsle(&io___62);
	    do_lio(&c__4, &c__1, (char *)&oar[4], (ftnlen)sizeof(real));
	    e_rsle();
	}
	if (! jf[15]) {
	    s_wsle(&io___63);
	    do_lio(&c__9, &c__1, "hmE", (ftnlen)3);
	    e_wsle();
	    s_rsle(&io___64);
	    do_lio(&c__4, &c__1, (char *)&oar[5], (ftnlen)sizeof(real));
	    e_rsle();
	}
	if (! jf[9]) {
	    s_wsle(&io___65);
	    do_lio(&c__9, &c__1, "Ne(300km),Ne(400km),Ne(600km) [-1 if not]", 
		    (ftnlen)41);
	    e_wsle();
	    s_rsle(&io___66);
	    do_lio(&c__4, &c__1, (char *)&oar[14], (ftnlen)sizeof(real));
	    do_lio(&c__4, &c__1, (char *)&oar[15], (ftnlen)sizeof(real));
	    do_lio(&c__4, &c__1, (char *)&oar[16], (ftnlen)sizeof(real));
	    e_rsle();
	}
    }
    if (! jf[16]) {
	s_wsle(&io___67);
	do_lio(&c__9, &c__1, "Rz12", (ftnlen)4);
	e_wsle();
	s_rsle(&io___68);
	do_lio(&c__4, &c__1, (char *)&oar[32], (ftnlen)sizeof(real));
	e_rsle();
	for (i__ = 2; i__ <= 100; ++i__) {
	    oar[i__ * 35 - 3] = oar[32];
	}
    }
    numstp = (integer) ((r__1 = vend - vbeg, dabs(r__1)) / dabs(vstp)) + 1;
    if (numstp > 100) {
	numstp = 100;
    }
    iri_web__(&jm, jf, &xlat, &xlon, &iy, &imd, &iut, &hour, &hx, &ivar, &
	    vbeg, &vend, &vstp, out, oar);
/*         call iris13(jf,jm,xlat,xlon,iy,imd,hour, */
/*     &          vbeg,vend,vstp,out,oarr) */

/* table head ....................................................... */
/* !output unit number */
/*        AGNR=6 */
    s_copy(stex, imz + (ivar - 1 << 2), (ftnlen)4, (ftnlen)4);
    if (jm > 0) {
	s_copy(stex, "GEOM", (ftnlen)4, (ftnlen)4);
    }
    if (iut > 0) {
	s_copy(stex, "UT", (ftnlen)4, (ftnlen)2);
    }
    piktab = (float)0.;
    if (ivar != 1) {
	if (hx < (float)1.) {
	    piktab = (float)1.;
	}
	if (hx < (float)0.) {
	    piktab = (float)2.;
	}
    }
    if (piktab > (float)1.) {
	s_wsfe(&io___74);
	do_fio(&c__1, itext + (ivar - 1) * 5, (ftnlen)5);
	do_fio(&c__1, imz + (ivar - 1 << 2), (ftnlen)4);
	e_wsfe();
    } else if (piktab > (float)0.) {
	s_wsfe(&io___75);
	do_fio(&c__1, itext + (ivar - 1) * 5, (ftnlen)5);
	do_fio(&c__1, imz + (ivar - 1 << 2), (ftnlen)4);
	e_wsfe();
    } else {
	s_wsfe(&io___76);
	do_fio(&c__1, itext + (ivar - 1) * 5, (ftnlen)5);
	do_fio(&c__1, imz + (ivar - 1 << 2), (ftnlen)4);
	e_wsfe();
    }
    xcor = vbeg;
    i__1 = numstp;
    for (li = 1; li <= i__1; ++li) {

/* special output: peak densities and altitudes */

	if (piktab == (float)1.) {
	    iyp1 = -1;
	    if (oar[li * 35 - 35] >= (float)0.) {
		iyp1 = (integer) (oar[li * 35 - 35] / (float)1e6 + (float).5);
	    }
	    iyp2 = -1;
	    if (oar[li * 35 - 33] >= (float)0.) {
		iyp2 = (integer) (oar[li * 35 - 33] / (float)1e6 + (float).5);
	    }
	    iyp3 = -1;
	    if (oar[li * 35 - 31] >= (float)0.) {
		iyp3 = (integer) (oar[li * 35 - 31] / (float)1e6 + (float).5);
	    }
	    iyp4 = -1;
	    if (oar[li * 35 - 29] >= (float)0.) {
		iyp4 = (integer) (oar[li * 35 - 29] / (float)1e6 + (float).5);
	    }
	    if (oar[li * 35 - 33] < (float).1) {
		oar[li * 35 - 32] = (float)0.;
	    }
	    s_wsfe(&io___83);
	    do_fio(&c__1, (char *)&xcor, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 34], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 32], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 30], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 28], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&iyp1, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&iyp2, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&iyp3, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&iyp4, (ftnlen)sizeof(integer));
	    e_wsfe();
	    goto L1234;
	}
	if (piktab == (float)2.) {
	    yp1 = (float)-1.;
	    if (oar[li * 35 - 35] >= (float)0.) {
		yp1 = sqrt(oar[li * 35 - 35] / (float)1.24e10);
	    }
	    yp2 = (float)-1.;
	    if (oar[li * 35 - 33] >= (float)0.) {
		yp2 = sqrt(oar[li * 35 - 33] / (float)1.24e10);
	    }
	    yp3 = (float)-1.;
	    if (oar[li * 35 - 31] >= (float)0.) {
		yp3 = sqrt(oar[li * 35 - 31] / (float)1.24e10);
	    }
	    yp4 = (float)-1.;
	    if (oar[li * 35 - 29] >= (float)0.) {
		yp4 = sqrt(oar[li * 35 - 29] / (float)1.24e10);
	    }
	    if (oar[li * 35 - 33] < (float).1) {
		oar[li * 35 - 32] = (float)0.;
	    }
	    s_wsfe(&io___88);
	    do_fio(&c__1, (char *)&xcor, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 34], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 32], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 30], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&oar[li * 35 - 28], (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&yp1, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&yp2, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&yp3, (ftnlen)sizeof(real));
	    do_fio(&c__1, (char *)&yp4, (ftnlen)sizeof(real));
	    e_wsfe();
	    goto L1234;
	}

/* normal output */

	if (ivar == 1) {
	    oar[li * 35 - 35] = oar[0];
	}
	i1 = -1;
	if (out[li * 11 - 11] >= (float)0.) {
	    i1 = (integer) (out[li * 11 - 11] / (float)1e6 + (float).5);
	}
	i2 = -1;
	if (out[li * 11 - 10] >= (float)0.) {
	    i2 = (integer) (out[li * 11 - 10] + (float).5);
	}
	i3 = -1;
	if (out[li * 11 - 9] >= (float)0.) {
	    i3 = (integer) (out[li * 11 - 9] + (float).5);
	}
	i4 = -1;
	if (out[li * 11 - 8] >= (float)0.) {
	    i4 = (integer) (out[li * 11 - 8] + (float).5);
	}
	i5 = -1;
	if (out[li * 11 - 7] >= (float)0.) {
	    i5 = (integer) (out[li * 11 - 7] + (float).5);
	}
	i6 = -1;
	if (out[li * 11 - 6] >= (float)0.) {
	    i6 = (integer) (out[li * 11 - 6] + (float).5);
	}
	i7 = -1;
	if (out[li * 11 - 5] >= (float)0.) {
	    i7 = (integer) (out[li * 11 - 5] + (float).5);
	}
	i8 = -1;
	if (out[li * 11 - 4] >= (float)0.) {
	    i8 = (integer) (out[li * 11 - 4] + (float).5);
	}
	i9 = -1;
	if (out[li * 11 - 3] >= (float)0.) {
	    i9 = (integer) (out[li * 11 - 3] + (float).5);
	}
	i10 = -1;
	if (out[li * 11 - 2] >= (float)0.) {
	    i10 = (integer) (out[li * 11 - 2] + (float).5);
	}
	i11 = -1;
	if (out[li * 11 - 1] >= (float)0.) {
	    i11 = (integer) (out[li * 11 - 1] + (float).5);
	}
/* ccc      WRITE(6,7117) XCOR,i1,out(1,li)/oar(1,li),i2,i3,i4, */
	s_wsfe(&io___100);
	do_fio(&c__1, (char *)&xcor, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&i1, (ftnlen)sizeof(integer));
	r__1 = out[li * 11 - 11] / oar[li * 35 - 35];
	do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&i2, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i3, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i4, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i5, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i11, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i6, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i7, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i8, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i9, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&i10, (ftnlen)sizeof(integer));
	e_wsfe();
L1234:
	xcor += vstp;
    }
    goto L1;
L2:
    s_stop("", (ftnlen)0);
    return 0;
} /* MAIN__ */

#ifdef __cplusplus
	}
#endif
