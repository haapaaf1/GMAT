/* iris13.f -- translated by f2c (version 19990311).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Common Block Declarations */

struct {
    real hmf2, nmf2, hmf1;
} block1_;

#define block1_1 block1_

struct {
    real umr;
} const_;

#define const_1 const_

struct {
    real humr, dum;
} const1_;

#define const1_1 const1_

struct {
    real b0, b1, c1;
} block2_;

#define block2_1 block2_

struct {
    real hz, t, hst, str;
} block3_;

#define block3_1 block3_

struct {
    real hme, nme, hef;
} block4_;

#define block4_1 block4_

struct {
    logical night;
    real e[4];
} block5_;

#define block5_1 block5_

struct {
    real hmd, nmd, hdx;
} block6_;

#define block6_1 block6_

struct {
    real d1, xkk, fp30, fp3u, fp1, fp2;
} block7_;

#define block7_1 block7_

struct {
    real hs, tnhs, xsm[4], mm[5], dti[4];
    integer mxsm;
} block8_;

#define block8_1 block8_

struct {
    real xsm1, texos, tlbdh, sigma;
} blotn_;

#define blotn_1 blotn_

struct {
    real ahh[7], ate1, stte[6], dte[5];
} blote_;

#define blote_1 blote_

struct {
    real beta, eta, delta, zeta;
} blo10_;

#define blo10_1 blo10_

struct {
    real argmax;
} argexp_;

#define argexp_1 argexp_

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static real c_b61 = (float)300.;
static integer c__0 = 0;
static real c_b73 = (float)12.;
static integer c__1976 = 1976;
static integer c__882 = 882;
static real c_b114 = (float)28.;
static real c_b115 = (float)1.;
static real c_b119 = (float)81.;
static real c_b122 = (float).06;
static real c_b130 = (float)4e8;
static real c_b132 = (float)88.;
static real c_b135 = (float).05;
static real c_b138 = (float)4.6;
static real c_b139 = (float)4.5;
static real c_b142 = (float)-11.5;
static real c_b143 = (float)-4.;
static real c_b150 = (float).001;
static real c_b180 = (float)0.;
static doublereal c_b187 = 2.;
static real c_b188 = (float)1.5;
static real c_b194 = (float)3.;
static real c_b201 = (float)130.;
static real c_b202 = (float)500.;
static real c_b205 = (float).01;
static integer c__12 = 12;
static integer c__4 = 4;
static integer c__2 = 2;
static real c_b226 = (float)10.;
static real c_b232 = (float)50.;

/* IRIS13.FOR -------------------------------------------- May 1998 */

/* includes subroutines IRIS13 to compute IRI parameters for specified */
/* location, date, time, and altitude range and subroutine IRI_WEB to */
/* computes IRI parameters for specified location, date, time and */
/* variable range; variable can be altitude, latitude, longitude, year, */
/* month, day of month, day of year, or hour (UT or LT). */

/* ***************************************************************** */
/* CHANGES FROM  IRIS11.FOR  TO   IRIS12.FOR: */
/*    - CIRA-1986 INSTEAD OF CIRA-1972 FOR NEUTRAL TEMPERATURE */
/*    - 10/30/91 VNER FOR NIGHTTIME LAY-VERSION:  ABS(..) */
/*    - 10/30/91 XNE(..) IN CASE OF LAY-VERSION */
/*    - 10/30/91 CHANGE SSIN=F/T TO IIQU=0,1,2 */
/*    - 10/30/91 Te > Ti > Tn ENFORCED IN FINAL PROFILE */
/*    - 10/30/91 SUB ALL NAMES WITH 6 OR MORE CHARACTERS */
/*    - 10/31/91 CORRECTED HF1 IN HST SEARCH:  NE(HF1)>NME */
/*    - 11/14/91 C1=0 IF NO F1-REGION */
/*    - 11/14/91 CORRECTED HHMIN AND HZ FOR LIN. APP. */
/*    -  1/28/92 RZ12=0 included */
/*    -  1/29/92 NEQV instead of NE between URSIF2 and URSIFO */
/*    -  5/ 1/92 CCIR and URSI input as in IRID12 */
/*    -  9/ 2/94 Decimal month (ZMONTH) for IONCOM */
/*    -  9/ 2/94 Replace B0POL with B0_TAB; better annually */
/*    -  1/ 4/95 DY for h>hmF2 */
/*    -  2/ 2/95 IG for foF2, topside; RZ for hmF2, B0_TAB, foF1, NmD */
/*    -  2/ 2/95 winter no longer exclusive for F1 occurrrence */
/*    -  2/ 2/95 RZ and IG included as DATA statement; smooth annual var. */
/* CHANGES FROM  IRIS12.FOR  TO   IRIS13.FOR: */
/*    - 10/26/95 include year as input and corrected MODA; nrm for zmonth */
/*    - 10/26/95 use TCON and month-month interpolation in foF2, hmF2 */
/*    - 10/26/95 TCON only if date changes */
/*    - 11/25/95 take out logicals TOPSI, BOTTO, and BELOWE */
/*    - 12/ 1/95 UT_LT for (date-)correct UT<->LT conversion */
/*    - 12/22/95 Change ZETA cov term to cov < 180; use cov inst covsat */
/*    -  2/23/96 take covmax(R<150) for topside; lyear,.. for lt */
/*    -  3/26/96 topside: 94.5/BETA inst 94.45/..; cov -> covsat(<=188) */
/*    -  5/01/96 No longer DY for h>hmF2 (because of discontinuity) */
/*    - 12/01/96 IRIV13: HOUR for IVAR=1 (height) */
/*    -  4/25/97 D-region: XKK le 10 with D1 calc accordingly. */
/*    -  1/12/97 DS model for lower ion compoistion DY model */
/*    -  5/19/98 seamon=zmonth if lati>0; zmonth= ...(1.0*iday)/.. */
/*    -  5/19/98 DY ion composition model below 300 km now DS model */
/*    -  5/19/98 DS model includes N+, Cl down to 75 km HNIA changed */
/*    -  5/28/98 User input for Rz12, foF1/NmF1, hmF1, foE/NmE, hmE; jf(17) */
/*    -  9/ 2/98 1 instead of 0 in MODA after UT_LT call */
/*    -  4/30/99 program constants moved from DATA statement into program */
/*    -  4/30/99 changed konsol-unit to 13 (12 is for IG_RZ). */
/*    -  5/29/99 the limit for IG computed from Rz12-input is 174 not 274 */
/*    - 11/08/99 jf(18)=.true. simple UT to LT conversion, otherwise UT_LT */
/*    - 11/09/99 added COMMON/const1/humr,dumr also for CIRA86 */

/* ***************************************************************** */
/* ********* INTERNATIONAL REFERENCE IONOSPHERE (IRI). ************* */
/* ***************************************************************** */
/* **************** ALL-IN-ONE SUBROUTINE  ************************* */
/* ***************************************************************** */


/* Subroutine */ int iris13_(logical *jf, integer *jmag, real *alati, real *
	along, integer *iyyyy, integer *mmdd, real *dhour, real *heibeg, real 
	*heiend, real *heistp, real *outf, real *oarr)
{
    /* Initialized data */

    static real b0b1[5] = { (float).755566,(float).778596,(float).797332,(
	    float).812928,(float).826146 };

    /* Format strings */
    static char fmt_1939[] = "(\002 *Ne* User input of hmF1 is only possible\
 for the LAY-\002,\002version\002)";
    static char fmt_104[] = "(\002/usr/local/etc/httpd/cgi-bin/models/IRI/cc\
ir\002,i2,\002.asc\002)";
    static char fmt_4689[] = "(1x,4e15.8)";
    static char fmt_1144[] = "(\002/usr/local/etc/httpd/cgi-bin/models/IRI/u\
rsi\002,i2,\002.asc\002)";
    static char fmt_8449[] = "(1x////,\002 The file \002,a30,\002is not in y\
our directory.\002)";
    static char fmt_650[] = "(1x,\002*NE* E-REGION VALLEY CAN NOT BE MODEL\
LED\002)";
    static char fmt_11[] = "(1x,\002*NE* HMF1 IS NOT EVALUATED BY THE FUNCTI\
ON XE2\002)";
    static char fmt_902[] = "(6x,\002CORR.: B1(OLD)=\002,f4.1,\002 B1(NEW)\
=\002,f4.1)";
    static char fmt_9269[] = "(1x,\002CORR.: NO F1 REGION, B1=3, C1=0.0\002)";
    static char fmt_100[] = "(1x,\002*NE* HST IS NOT EVALUATED BY THE FUNCTI\
ON XE3\002)";
    static char fmt_901[] = "(6x,\002CORR.: LIN. APP. BETWEEN HZ=\002,f5.1\
,\002 AND HEF=\002,f5.1)";
    static char fmt_7733[] = "(\002*NE* LAY amplitudes found with 2nd choice\
 of HXL(1).\002)";
    static char fmt_7722[] = "(\002*NE* LAY amplitudes could not be found\
.\002)";

    /* System generated locals */
    integer i__1;
    real r__1, r__2;
    doublereal d__1, d__2;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    double atan(doublereal), log(doublereal), sqrt(doublereal);
    integer s_wsfe(cilist *), e_wsfe(), s_wsle(cilist *), do_lio(integer *, 
	    integer *, char *, ftnlen), e_wsle();
    double tan(doublereal), exp(doublereal);
    integer s_wsfi(icilist *), do_fio(integer *, char *, ftnlen), e_wsfi(), 
	    f_open(olist *), s_rsfe(cilist *), e_rsfe(), f_clos(cllist *);
    double cos(doublereal), pow_dd(doublereal *, doublereal *), r_sign(real *,
	     real *);

    /* Local variables */
    static real tix, ho05;
    extern doublereal xen_(real *, real *, real *, real *, integer *, real *, 
	    real *, real *);
    static real tnh, tih, dela, teh, rox, rhx, rnx, afoe, ahme, hnea, hnee;
    extern /* Subroutine */ int moda_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static real arig[3];
    static integer kind, iday;
    static real dell, dnds[4], lati, dion[7];
    static integer lday;
    static real zm3000, mlat;
    static logical old79;
    static integer midm;
    static real xm3000, epin, seax;
    extern /* Subroutine */ int tcon_(integer *, integer *, integer *, 
	    integer *, real *, real *, real *, integer *), soco_(integer *, 
	    real *, real *, real *, real *, real *, real *, real *);
    static real dumr, xnar[3], rsat, grat, rzar[3], hour;
    extern doublereal fout_(real *, real *, real *, real *, real *);
    static logical rzin;
    static real fluu, alg100, rssn, xdel;
    extern doublereal hpol_(real *, real *, real *, real *, real *, real *, 
	    real *);
    static real vner, dxdx, afof1, afof2, ahmf2, ahmf1, rrrr;
    static integer iiqu;
    static real utni;
    extern /* Subroutine */ int teba_(real *, real *, integer *, real *);
    static logical f1reg;
    static real alog2, hhmf2;
    extern doublereal tede_(real *, real *, real *);
    static real hmf1m;
    static logical gulb0;
    static real fof2n, xtts;
    extern doublereal elte_(real *);
    static real hnia, nobo2, hnie, pnmf1;
    extern /* Subroutine */ int sufe_(real *, real *, integer *, real *);
    static real zzz1, delx, xm300n, zfof2, yfof2;
    extern doublereal rpid_(real *, real *, real *, integer *, real *, 
	    integer *, real *);
    static real tn1ni, rhex, rnox, ro2x;
    extern doublereal rdno_(real *, real *, real *, real *, real *);
    static real yoh0o, stte1, stte2, d__, f[3], h__;
    static integer i__, j, k, l;
    static real elede, ymo2z, hhalf, aldo21, hdeep, magbr, z__, dlndh, x;
    extern /* Subroutine */ int cira86_(integer *, real *, real *, real *, 
	    real *, real *, real *, real *, real *);
    static logical foein, hmein, noden;
    extern doublereal teder_(real *);
    static real depth;
    extern doublereal xmded_(real *, real *, real *);
    static real longi, modip, mlong;
    static integer daynr;
    static logical tecon[3];
    static real xdels[4];
    static integer iyear;
    static logical notem, noion;
    static real f2[1976]	/* was [13][76][2] */, arzin;
    static integer jxnar, month, lyear, idtmp;
    extern /* Subroutine */ int ut_lt__(integer *, real *, real *, real *, 
	    integer *, integer *);
    static integer nmono;
    extern /* Subroutine */ int rogul_(integer *, real *, real *, real *);
    extern doublereal b0_tab__(real *, real *, real *, integer *, real *, 
	    real *);
    static integer iytmp;
    extern doublereal fof1ed_(real *, real *, real *), hmf2ed_(real *, real *,
	     real *, real *);
    static real width;
    extern /* Subroutine */ int regfa1_(real *, real *, real *, real *, real *
	    , real *, E_fp, logical *, real *);
    static real rathh, xxmin, hhmin, z1;
    extern doublereal xmout_(real *, real *, real *, real *, real *);
    static real z2, b0cnew, xnehz, secni, texni;
    static logical fof1in, fof2in, hmf2in, hmf1in;
    static real signi, tlbdn, hmaxd, hmaxn, tmaxd, tmaxn, tnahh2, z3, sunde1;
    extern doublereal dtndh_(real *, real *, real *, real *);
    static real xteti;
    extern /* Subroutine */ int koefp1_(real *), koefp2_(real *), koefp3_(
	    real *);
    static integer msumo;
    static real hfixo, ymaxx, y, yo2h0o;
    static logical ursif2;
    static real hfixo2, rdo2mx;
    extern /* Subroutine */ int rdhhe_(real *, real *, real *, real *, real *,
	     real *, real *, real *);
    static real xnorm;
    static integer ki, kk;
    static real ho[4], pi, mo[5];
    static logical dy;
    extern /* Subroutine */ int fieldg_(real *, real *, real *, real *, real *
	    , real *, real *, real *, real *, real *);
    extern doublereal foeedi_(real *, real *, real *, real *);
    static real ex, zi, dndhbr;
    static integer iregfa, seaday;
    static char filnam[53];
    static integer icalls;
    static real abslat, absmdp, absmbr, ut;
    extern doublereal tn_(real *, real *, real *, real *);
    static logical schalt;
    static integer iuccir;
    static real sundec, absmlt, seamon;
    static integer idayno, numhei;
    extern /* Subroutine */ int inilay_(logical *, real *, real *, real *, 
	    real *, real *, real *, real *, real *, real *, real *, real *, 
	    real *, real *, integer *);
    static integer nseasn, season, ldaynr;
    static logical teneop;
    static integer nrdaym;
    static real ff0[988];
    static integer lnrday, iyearo;
    static real rlimit;
    static integer lmonth;
    static logical layver;
    static integer nmonth, montho, do2[2];
    static real fm3[882]	/* was [9][49][2] */, f2n[1976]	/* was [13][
	    76][2] */;
    static logical ursifo;
    static real ho2[2];
    static integer monito, konsol;
    static real mo2[3];
    extern doublereal xe2_(real *), xe3_(real *);
    static real xm0[441], zmonth, covsat, xhinon, saxnon, suxnon, rr2, rr1, 
	    ex1, xkkmax;
    static integer ib1;
    static real hf1, xf1, diplat, tnahhi, ti1;
    extern doublereal ti_(real *);
    static real rdomax;
    extern doublereal epstep_(real *, real *, real *, real *, real *);
    static real h0o, height;
    extern doublereal xe_(real *);
    static real dec;
    extern /* Subroutine */ int ioncom_new__(real *, real *, real *, real *, 
	    real *, real *);
    static real rclust, deh;
    static integer ddo[4];
    static real hoa[3], ate[7], tea[6], amp[4], rif[4], scl[4];
    extern /* Subroutine */ int ggm_(integer *, real *, real *, real *, real *
	    );
    static real xma, yma, hxl[4], zma;
    static logical sam_yea__, ext;
    static real bet, dip, rrr, cov;
    static logical sam_mon__, sam_doy__;
    static real ttt, xhi, sax, sux, foe, flu;
    extern /* Subroutine */ int tal_(real *, real *, real *, real *, logical *
	    , real *);
    static real xdx, hta, hte, sec;
    static integer iyz;
    static real ff0n[988], fof2, eta1, fof1;
    static integer idz;
    static real nmf1, fm3n[882]	/* was [9][49][2] */, tn120, pf1o[12], pg1o[
	    80], pg2o[32], pg3o[80], pf2o[4], pf3o[12], cos2, xe2h, xe3h, ett,
	     hv1r, hv2r, xm0n[441], rr2n, rr1n, tet, ten, tid1, ted1, tin1, 
	    ten1, tnn1, ti13, ti50, tex;

    /* Fortran I/O blocks */
    static cilist io___38 = { 0, 0, 0, fmt_1939, 0 };
    static cilist io___46 = { 0, 6, 0, 0, 0 };
    static cilist io___47 = { 0, 6, 0, 0, 0 };
    static cilist io___48 = { 0, 6, 0, 0, 0 };
    static cilist io___49 = { 0, 6, 0, 0, 0 };
    static cilist io___50 = { 0, 6, 0, 0, 0 };
    static cilist io___51 = { 0, 6, 0, 0, 0 };
    static cilist io___52 = { 0, 6, 0, 0, 0 };
    static cilist io___53 = { 0, 6, 0, 0, 0 };
    static cilist io___54 = { 0, 6, 0, 0, 0 };
    static cilist io___55 = { 0, 6, 0, 0, 0 };
    static cilist io___56 = { 0, 6, 0, 0, 0 };
    static cilist io___57 = { 0, 6, 0, 0, 0 };
    static cilist io___58 = { 0, 6, 0, 0, 0 };
    static cilist io___59 = { 0, 6, 0, 0, 0 };
    static icilist io___128 = { 0, filnam, 0, fmt_104, 53, 1 };
    static cilist io___129 = { 0, 0, 0, fmt_4689, 0 };
    static icilist io___132 = { 0, filnam, 0, fmt_1144, 53, 1 };
    static cilist io___133 = { 0, 0, 0, fmt_4689, 0 };
    static icilist io___134 = { 0, filnam, 0, fmt_104, 53, 1 };
    static cilist io___135 = { 0, 0, 0, fmt_4689, 0 };
    static icilist io___138 = { 0, filnam, 0, fmt_1144, 53, 1 };
    static cilist io___139 = { 0, 0, 0, fmt_4689, 0 };
    static cilist io___140 = { 0, 0, 0, fmt_8449, 0 };
    static cilist io___184 = { 0, 0, 0, fmt_650, 0 };
    static cilist io___193 = { 0, 0, 0, fmt_11, 0 };
    static cilist io___195 = { 0, 0, 0, fmt_902, 0 };
    static cilist io___197 = { 0, 0, 0, fmt_9269, 0 };
    static cilist io___207 = { 0, 0, 0, fmt_100, 0 };
    static cilist io___209 = { 0, 0, 0, fmt_901, 0 };
    static cilist io___220 = { 0, 0, 0, fmt_7733, 0 };
    static cilist io___221 = { 0, 0, 0, fmt_7722, 0 };


/* ----------------------------------------------------------------- */

/* INPUT:  JF(1:20)      true/false switches for several options */
/*         JMAG          =0 geographic   = 1 geomagnetic coordinates */
/*         ALATI,ALONG   LATITUDE NORTH AND LONGITUDE EAST IN DEGREES */
/*         IYYYY         Year as YYYY, e.g. 1985 */
/*         MMDD (-DDD)   DATE (OR DAY OF YEAR AS A NEGATIVE NUMBER) */
/*         DHOUR         LOCAL TIME (OR UNIVERSAL TIME + 25) IN DECIMAL */
/*                          HOURS */
/*         HEIBEG,       HEIGHT RANGE IN KM; maximal 50 heights, i.e. */
/*          HEIEND,HEISTP        int((heiend-heibeg)/heistp)+1.le.50 */

/*         JF switches to turn off/on (true/false) several options */

/*       jf(i)=          .true.                  .false. */
/*      --------------------------------------------------------------------- */
/*        i=1         Ne computed            Ne not computed */
/*          2         Te, Ti computed        Te, Ti not computed */
/*          3         Ni computed            Ni not computed */
/*          4         B0 - Table option      B0 - Gulyaeva (1987) */
/*          5         foF2 - CCIR            foF2 - URSI */
/*          6         Ni - Standard          Ni - Danilov-Yaichnikov-Smironova */
/*          7         Ne - Standard Topside  Ne - IRI-79 Topside */
/*          8         foF2 from model        foF2 or NmF2 - user input */
/*          9         hmF2 from model        hmF2 or M3000F2 - user input */
/*         10         Te - Standard          Te - Using Te/Ne correlation */
/*         11         Ne - Standard Profile  Ne - Lay-function formalism */
/*         12         Messages are written to unit */
/*                             6                          12 */
/*                    {you can also turn off all messages by setting */
/*                     KONSOL=1 internally} */
/*         13         foF1 from model        foF1 or NmF1 - user input */
/*         14         hmF1 from model        hmF1 - user input */
/*         15         foE  from model        foE or NmE - user input */
/*         16         hmE  from model        hmE - user input */
/*         17         Rz12 from file         Rz12 - user input */
/*         18         simple ut<->lt         using ut_lt subroutine */
/*         19    free */
/*         20    free */
/*      --------------------------------------------------------------------- */

/*  Depending on the jf() settings additional INPUT parameters may be required: */

/*          Setting              INPUT parameter */
/*       ------------------------------------------------------ */
/*       jf(8)=.false.      OARR(1)=user input for foF2/MHz or NmF2/m-3 */
/*       jf(9)=.false.      OARR(2)=user input for hmF2/km or M(3000)F2 */
/*       jf(10)=.false.     OARR(15),OARR(16),OARR(17)=user input for */
/*                            Ne(300km),Ne(400km),Ne(600km)/m-3; use */
/*                            OARR()=-1 if one of these values is not */
/*                            available. */
/*       jf(13)=.false.     OARR(3)=user input for foF1/MHz or NmF1/m-3 */
/*       jf(14)=.false.     OARR(4)=user input for hmF1/km */
/*       jf(15)=.false.     OARR(5)=user input for foE/MHz or NmE/m-3 */
/*       jf(16)=.false.     OARR(6)=user input for hmE/km */
/*       jf(17)=.flase.     OARR(33)=user input for Rz12 */

/*  OUTPUT:  OUTF(1:11,1:100) */
/*               OUTF(1,*)  ELECTRON DENSITY/M-3 */
/*               OUTF(2,*)  NEUTRAL TEMPERATURE/K */
/*               OUTF(3,*)  ION TEMPERATURE/K */
/*               OUTF(4,*)  ELECTRON TEMPERATURE/K */
/*               OUTF(5,*)  O+ ION DENSITY/M-3 */
/*               OUTF(6,*)  H+ ION DENSITY/M-3 */
/*               OUTF(7,*)  HE+ ION DENSITY/M-3 */
/*               OUTF(8,*)  O2+ ION DENSITY/M-3 */
/*               OUTF(9,*)  NO+ ION DENSITY/M-3 */
/*                 AND, IF JF(6)=.FALSE.: */
/*               OUTF(10,*)  PERCENTAGE OF CLUSTER IONS IN % */
/*               OUTF(11,*)  PERCENTAGE OF N+ IONS IN % */

/*            OARR(1:35)   ADDITIONAL OUTPUT PARAMETERS */

/*              #OARR(1) = NMF2/M-3           #OARR(2) = HMF2/KM */
/*              #OARR(3) = NMF1/M-3           #OARR(4) = HMF1/KM */
/*              #OARR(5) = NME/M-3            #OARR(6) = HME/KM */
/*               OARR(7) = NMD/M-3             OARR(8) = HMD/KM */
/*               OARR(9) = HHALF/KM            OARR(10) = B0/KM */
/*               OARR(11) =VALLEY-BASE/M-3     OARR(12) = VALLEY-TOP/KM */
/*               OARR(13) = TE-PEAK/K          OARR(14) = TE-PEAK HEIGHT/KM */
/*              #OARR(15) = TE-MOD(300KM)     #OARR(16) = TE-MOD(400KM)/K */
/*              #OARR(17) = TE-MOD(600KM)      OARR(18) = TE-MOD(1400KM)/K */
/*               OARR(19) = TE-MOD(3000KM)     OARR(20) = TE(120KM)=TN=TI/K */
/*               OARR(21) = TI-MOD(430KM)      OARR(22) = X/KM, WHERE TE=TI */
/*               OARR(23) = SOL ZENITH ANG/DEG OARR(24) = SUN DECLINATION/DEG */
/*               OARR(25) = DIP/deg            OARR(26) = DIP LATITUDE/deg */
/*               OARR(27) = MODIFIED DIP LAT.  OARR(28) = DELA */
/*               OARR(29) = sunrise/dec. hours OARR(30) = sunset/dec. hours */
/*               OARR(31) = ISEASON (1=spring) OARR(32) = NSEASON (northern) */
/*              #OARR(33) = Rz12               OARR(34) = Covington Index */
/*               OARR(35) = B1                 oarr(36) = M(3000)F2 */
/*     place_holders only: */
/*               oarr(36) = TEC/m-2            oarr(38) = TEC_top/TEC*100. */

/*      # INPUT as well as OUTPUT parameter */
/*      lower case:      special for IRIWeb */
/* ------------------------------------------------------------------- */
/* ***************************************************************** */
/* *** THE ALTITUDE LIMITS ARE:  LOWER (DAY/NIGHT)  UPPER        *** */
/* ***     ELECTRON DENSITY         60/80 KM       1000 KM       *** */
/* ***     TEMPERATURES              120 KM        3000 KM       *** */
/* ***     ION DENSITIES             100 KM        1000 KM       *** */
/* ***************************************************************** */
/* ***************************************************************** */
/* *********            INTERNALLY                    ************** */
/* *********       ALL ANGLES ARE IN DEGREE           ************** */
/* *********       ALL DENSITIES ARE IN M-3           ************** */
/* *********       ALL ALTITUDES ARE IN KM            ************** */
/* *********     ALL TEMPERATURES ARE IN KELVIN       ************** */
/* *********     ALL TIMES ARE IN DECIMAL HOURS       ************** */
/* ***************************************************************** */
/* ***************************************************************** */
/* ***************************************************************** */
/*      CHARACTER FILNAM*12 */
    /* Parameter adjustments */
    --oarr;
    outf -= 12;
    --jf;

    /* Function Body */
/* TEST   save    icalls,rssn,rsat,cov,ttt */
    for (ki = 1; ki <= 11; ++ki) {
	for (kk = 1; kk <= 100; ++kk) {
/* L7397: */
	    outf[ki + kk * 11] = (float)-1.;
	}
    }
    for (kind = 7; kind <= 14; ++kind) {
/* L8398: */
	oarr[kind] = (float)-1.;
    }
    for (kind = 18; kind <= 32; ++kind) {
/* L8378: */
	oarr[kind] = (float)-1.;
    }
    oarr[34] = (float)-1.;
    oarr[35] = (float)-1.;
    oarr[36] = (float)-1.;
    oarr[37] = (float)-1.;
    oarr[38] = (float)-1.;

/* PROGRAM CONSTANTS */

    ++icalls;
    argexp_1.argmax = (float)88.;
    pi = atan((float)1.) * (float)4.;
    const_1.umr = pi / (float)180.;
    const1_1.humr = pi / (float)12.;
    dumr = pi / (float)182.5;
    alog2 = log((float)2.);
    alg100 = log((float)100.);
    numhei = (integer) ((r__1 = *heiend - *heibeg, dabs(r__1)) / dabs(*heistp)
	    ) + 1;
    if (numhei > 100) {
	numhei = 100;
    }

/* Code inserted to aleviate block data problem for PC version. */
/* Thus avoiding DATA statement with parameters from COMMON block. */

    hoa[0] = (float)300.;
    hoa[1] = (float)400.;
    hoa[2] = (float)600.;
    xdels[0] = (float)5.;
    xdels[1] = (float)5.;
    xdels[2] = (float)5.;
    xdels[3] = (float)10.;
    dnds[0] = (float).016;
    dnds[1] = (float).01;
    dnds[2] = (float).016;
    dnds[3] = (float).016;
    ddo[0] = 9;
    ddo[1] = 5;
    ddo[2] = 5;
    ddo[3] = 25;
    do2[0] = 5;
    do2[1] = 5;
    xnar[0] = (float)0.;
    xnar[1] = (float)0.;
    xnar[2] = (float)0.;
    blote_1.ahh[0] = (float)120.;
    blote_1.ahh[1] = (float)0.;
    blote_1.ahh[2] = (float)300.;
    blote_1.ahh[3] = (float)400.;
    blote_1.ahh[4] = (float)600.;
    blote_1.ahh[5] = (float)1400.;
    blote_1.ahh[6] = (float)3e3;
    blote_1.dte[0] = (float)5.;
    blote_1.dte[1] = (float)5.;
    blote_1.dte[2] = (float)10.;
    blote_1.dte[3] = (float)20.;
    blote_1.dte[4] = (float)20.;
    block8_1.dti[0] = (float)10.;
    block8_1.dti[1] = (float)10.;
    block8_1.dti[2] = (float)20.;
    block8_1.dti[3] = (float)20.;

/* FIRST SPECIFY YOUR COMPUTERS CHANNEL NUMBERS .................... */
/* AGNR=OUTPUT (OUTPUT IS DISPLAYED OR STORED IN FILE OUTPUT.IRI)... */
/* IUCCIR=UNIT NUMBER FOR CCIR COEFFICIENTS ........................ */
/* set konsol=1 if you do not want the konsol information */

    monito = 6;
    iuccir = 10;
    konsol = 1;
/*      KONSOL=6 */
    if (! jf[12]) {
	konsol = 13;
    }

/* selection of density and ion composition options .................. */

    noden = ! jf[1];
    notem = ! jf[2];
    noion = ! jf[3];
    dy = ! jf[6];
    layver = ! jf[11];
    old79 = ! jf[7];
    gulb0 = ! jf[4];

/* rz12 input option .................................................... */

    rzin = ! jf[17];
    if (rzin) {
	arzin = oarr[33];
    } else {
	oarr[33] = (float)-1.;
    }

/* F2 peak density .................................................... */

    fof2in = ! jf[8];
    if (fof2in) {
	afof2 = oarr[1];
	if (afof2 > (float)100.) {
	    afof2 = sqrt(afof2 / (float)1.24e10);
	}
    } else {
	oarr[1] = (float)-1.;
    }
    ursif2 = ! jf[5];

/* F2 peak altitude .................................................. */

    hmf2in = ! jf[9];
    if (hmf2in) {
	ahmf2 = oarr[2];
    } else {
	oarr[2] = (float)-1.;
    }

/* F1 peak density .................................................... */

    fof1in = ! jf[13];
    if (fof1in) {
	afof1 = oarr[3];
	if (afof1 > (float)100.) {
	    afof1 = sqrt(afof1 / (float)1.24e10);
	}
    } else {
	oarr[3] = (float)-1.;
    }

/* F1 peak altitude .................................................. */

    hmf1in = ! jf[14];
    if (hmf1in) {
	ahmf1 = oarr[4];
	if (! layver && konsol > 1) {
	    io___38.ciunit = konsol;
	    s_wsfe(&io___38);
	    e_wsfe();
	}
    } else {
	oarr[4] = (float)-1.;
    }

/* E peak density .................................................... */

    foein = ! jf[15];
    if (foein) {
	afoe = oarr[5];
	if (afoe > (float)100.) {
	    afoe = sqrt(afoe / (float)1.24e10);
	}
    } else {
	oarr[5] = (float)-1.;
    }

/* E peak altitude .................................................. */

    hmein = ! jf[16];
    if (hmein) {
	ahme = oarr[6];
    } else {
	oarr[6] = (float)-1.;
    }

/* TE-NE MODEL OPTION .............................................. */

    teneop = ! jf[10];
    if (teneop) {
	for (jxnar = 1; jxnar <= 3; ++jxnar) {
	    xnar[jxnar - 1] = oarr[jxnar + 14];
	    tecon[jxnar - 1] = FALSE_;
/* L8154: */
	    if (xnar[jxnar - 1] > (float)0.) {
		tecon[jxnar - 1] = TRUE_;
	    }
	}
    } else {
	oarr[3] = (float)-1.;
	oarr[4] = (float)-1.;
	oarr[5] = (float)-1.;
    }

/* lists the selected options before starting the table */

    if (icalls > 1 || konsol == 1) {
	goto L8201;
    }
    s_wsle(&io___46);
    do_lio(&c__9, &c__1, "*** IRI parameters are being calculated ***", (
	    ftnlen)43);
    e_wsle();
    if (noden) {
	goto L2889;
    }
    if (layver) {
	s_wsle(&io___47);
	do_lio(&c__9, &c__1, "Ne, E-F: The LAY-Version is ", (ftnlen)28);
	do_lio(&c__9, &c__1, "prelimenary. Erroneous profile features can oc\
cur.", (ftnlen)50);
	e_wsle();
    }
    if (gulb0) {
	s_wsle(&io___48);
	do_lio(&c__9, &c__1, "Ne, B0: Bottomside thickness is ", (ftnlen)32);
	do_lio(&c__9, &c__1, "obtained with Gulyaeva-1987 model.", (ftnlen)34)
		;
	e_wsle();
    }
    if (old79) {
	s_wsle(&io___49);
	do_lio(&c__9, &c__1, "Ne: Using IRI-79. Correction", (ftnlen)28);
	do_lio(&c__9, &c__1, " of equatorial topside is not included.", (
		ftnlen)39);
	e_wsle();
    }
    if (fof2in) {
	s_wsle(&io___50);
	do_lio(&c__9, &c__1, "Ne, foF2/NmF2: provided by user.", (ftnlen)32);
	e_wsle();
	goto L2889;
    }
    if (ursif2) {
	s_wsle(&io___51);
	do_lio(&c__9, &c__1, "Ne, foF2: URSI model is used.", (ftnlen)29);
	e_wsle();
    } else {
	s_wsle(&io___52);
	do_lio(&c__9, &c__1, "Ne, foF2: CCIR model is used.", (ftnlen)29);
	e_wsle();
    }
L2889:
    if (hmf2in) {
	s_wsle(&io___53);
	do_lio(&c__9, &c__1, "Ne, hmF2/M3000F2: provided by user.", (ftnlen)
		35);
	e_wsle();
    }
    if (fof1in) {
	s_wsle(&io___54);
	do_lio(&c__9, &c__1, "Ne, foF1/NmF1: provided by user.", (ftnlen)32);
	e_wsle();
    }
    if (hmf1in) {
	s_wsle(&io___55);
	do_lio(&c__9, &c__1, "Ne, hmF1: prvided by user.", (ftnlen)26);
	e_wsle();
    }
    if (foein) {
	s_wsle(&io___56);
	do_lio(&c__9, &c__1, "Ne, foE/NmE: provided by user.", (ftnlen)30);
	e_wsle();
    }
    if (hmein) {
	s_wsle(&io___57);
	do_lio(&c__9, &c__1, "Ne, hmE: prvided by user.", (ftnlen)25);
	e_wsle();
    }
    if (! noion && dy) {
	s_wsle(&io___58);
	do_lio(&c__9, &c__1, "Ion Com.: Using Danilov et al. 1985/95.", (
		ftnlen)39);
	e_wsle();
    }
    if (! notem && teneop) {
	s_wsle(&io___59);
	do_lio(&c__9, &c__1, "Te: Temperature-density correlation is used.", (
		ftnlen)44);
	e_wsle();
    }
L8201:

/* CALCULATION OF GEOG. OR GEOM. COORDINATES IN DEG.................... */
/* CALCULATION OF MAGNETIC INCLINATION (DIP), DECLINATION (DEC)........ */
/*   DIP LATITUDE (MAGBR) AND MODIFIED DIP (MODIP). ALL IN DEGREE...... */

    if (*jmag > 0) {
	mlat = *alati;
	mlong = *along;
    } else {
	lati = *alati;
	longi = *along;
    }
    ggm_(jmag, &longi, &lati, &mlong, &mlat);
    abslat = dabs(lati);
    fieldg_(&lati, &longi, &c_b61, &xma, &yma, &zma, &bet, &dip, &dec, &modip)
	    ;
    magbr = atan(tan(dip * const_1.umr) * (float).5) / const_1.umr;
    absmlt = dabs(mlat);
    absmdp = dabs(modip);
    absmbr = dabs(magbr);

/* CALCULATION OF DAY OF YEAR AND SUN DECLINATION...................... */
/* CALCULATION OF UT/LT AND RELATED YEAR, MONTH, DAYNRs ............... */
/* CALCULATION OF (UT-)SEASON (SUMMER=2, WINTER=4)..................... */

    iyear = *iyyyy;
    if (iyear < 100) {
	iyear += 1900;
    }
    if (*mmdd < 0) {
	daynr = -(*mmdd);
	moda_(&c__1, &iyear, &month, &iday, &daynr, &nrdaym);
    } else {
	month = *mmdd / 100;
	iday = *mmdd - month * 100;
	moda_(&c__0, &iyear, &month, &iday, &daynr, &nrdaym);
    }

/* lyear,lmonth,lday,ldaynre,lnrday related to LT */

    lyear = iyear;
    lmonth = month;
    lday = iday;
    ldaynr = daynr;
    lnrday = nrdaym;
    if (*dhour <= (float)24.) {
	goto L2619;
    }
    ut = *dhour - (float)25.;
    iytmp = iyear;
    idtmp = daynr;
    if (jf[18]) {
	hour = ut + longi / (float)15.;
	if (hour > (float)24.) {
	    hour += (float)-24.;
	}
    } else {
	ut_lt__(&c__0, &ut, &hour, &longi, &iytmp, &idtmp);
	if (idtmp != ldaynr) {
	    lyear = iytmp;
	    ldaynr = idtmp;
	    moda_(&c__1, &lyear, &lmonth, &lday, &ldaynr, &lnrday);
	}
    }
    goto L2629;
L2619:
    hour = *dhour;
    iytmp = lyear;
    idtmp = ldaynr;
    if (jf[18]) {
	ut = hour - longi / (float)15.;
	if (ut < (float)0.) {
	    ut += (float)24.;
	}
    } else {
	ut_lt__(&c__1, &ut, &hour, &longi, &iytmp, &idtmp);
	if (idtmp != daynr) {
	    iyear = iytmp;
	    daynr = idtmp;
	    moda_(&c__1, &iyear, &month, &iday, &daynr, &nrdaym);
	}
    }
L2629:
    zmonth = lmonth + lday * (float)1. / lnrday;
    season = (integer) ((daynr + (float)45.) / (float)92.);
    if (season < 1) {
	season = 4;
    }
    nseasn = season;
    seaday = daynr;
    seamon = zmonth;
    if (lati > (float)0.) {
	goto L5592;
    }
    season += -2;
    if (season < 1) {
	season += 4;
    }
    seamon = zmonth + (float)6.;
    if (seamon >= (float)13.) {
	seamon += (float)-12.;
    }
    seaday = daynr + 183;
    if (seaday > 366) {
	seaday += -366;
    }

/* CALCULATION OF MEAN F10.7CM SOLAR RADIO FLUX (COV)................ */
/* CALCULATION OF RESTRICTED SOLAR ACTIVITIES (RSAT,COVSAT).............. */

L5592:
    sam_mon__ = month == montho;
    sam_yea__ = iyear == iyearo;
    sam_doy__ = daynr == idayno;
    if (sam_yea__ && sam_doy__) {
	goto L2910;
    }
    tcon_(&iyear, &month, &iday, &daynr, rzar, arig, &ttt, &nmonth);
    if (nmonth < 0) {
	goto L3330;
    }
    if (rzin) {
	rrr = arzin;
	rzar[0] = rrr;
	rzar[1] = rrr;
	rzar[2] = rrr;
	zi = ((float)1.4683266 - rrr * (float).00267690893) * rrr - (float)
		12.349154;
	if (zi > (float)174.) {
	    zi = (float)174.;
	}
	arig[0] = zi;
	arig[1] = zi;
	arig[2] = zi;
    }
    rssn = rzar[2];
    rsat = arig[2];
    cov = rssn * (rssn * (float)8.9e-4 + (float).728) + (float)63.75;
    rlimit = rsat;
    covsat = rlimit * (rlimit * (float)8.9e-4 + (float).728) + (float)63.75;
    if (covsat > (float)188.) {
	covsat = (float)188.;
    }

/* CALCULATION OF SOLAR ZENITH ANGLE (XHI/DEG)......................... */
/* NOON VALUE (XHINON)................................................. */

L2910:
    soco_(&ldaynr, &hour, &lati, &longi, &sundec, &xhi, &sax, &sux);
    soco_(&ldaynr, &c_b73, &lati, &longi, &sunde1, &xhinon, &saxnon, &suxnon);
    block5_1.night = FALSE_;
    if (dabs(sax) > (float)25.) {
	if (sax < (float)0.) {
	    block5_1.night = TRUE_;
	}
	goto L1334;
    }
    if (sax <= sux) {
	goto L1386;
    }
    if (hour > sux && hour < sax) {
	block5_1.night = TRUE_;
    }
    goto L1334;
L1386:
    if (hour > sux || hour < sax) {
	block5_1.night = TRUE_;
    }

/* CALCULATION OF ELECTRON DENSITY PARAMETERS................ */

L1334:
    hnea = (float)65.;
    if (block5_1.night) {
	hnea = (float)80.;
    }
    hnee = (float)2e3;
    if (noden) {
	goto L4933;
    }
    dela = (float)4.32;
    if (absmdp >= (float)18.) {
	dela = exp(-(absmdp - (float)30.) / (float)10.) + (float)1.;
    }
    dell = exp(-(abslat - (float)20.) / (float)10.) + 1;
/* !!!!!!! F-REGION PARAMETERS AND E-PEAK !!!!!!!!!!!!!!!!!!!!!!!!!! */
    if (foein) {
	foe = afoe;
    } else {
	foe = foeedi_(&cov, &xhi, &xhinon, &abslat);
    }
    block4_1.nme = foe * (float)1.24e10 * foe;
    if (hmein) {
	block4_1.hme = ahmf2;
    } else {
	block4_1.hme = (float)110.;
    }

/* READ CCIR AND URSI COEFFICIENT SET FOR CHOSEN MONTH ............ */

    if (fof2in && hmf2in) {
	goto L501;
    }
    if (ursif2 != ursifo) {
	goto L7797;
    }
    if (sam_mon__ && nmonth == nmono && sam_yea__) {
	goto L4292;
    }
    if (sam_mon__) {
	goto L4293;
    }

/* the program expects the coefficients files in ASCII format; if you */
/* want to use the binary version of the coefficients, please use the */
/* the statements that are commented-out below and comment-out the */
/* ASCII-related statements. */

L7797:
    ursifo = ursif2;
    s_wsfi(&io___128);
    i__1 = month + 10;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    e_wsfi();
    o__1.oerr = 1;
    o__1.ounit = iuccir;
    o__1.ofnmlen = 53;
    o__1.ofnm = filnam;
    o__1.orl = 0;
    o__1.osta = "OLD";
    o__1.oacc = 0;
    o__1.ofm = "FORMATTED";
    o__1.oblnk = 0;
    i__1 = f_open(&o__1);
    if (i__1 != 0) {
	goto L8448;
    }
/*     &          FORM='UNFORMATTED') */
    io___129.ciunit = iuccir;
    s_rsfe(&io___129);
    do_fio(&c__1976, (char *)&f2[0], (ftnlen)sizeof(real));
    do_fio(&c__882, (char *)&fm3[0], (ftnlen)sizeof(real));
    e_rsfe();
/*        READ(IUCCIR) F2,FM3 */
/* 104   FORMAT('ccir',I2,'.asc') */
/* 104   FORMAT('ccir',I2,'.bin') */
    cl__1.cerr = 0;
    cl__1.cunit = iuccir;
    cl__1.csta = 0;
    f_clos(&cl__1);

/* then URSI if chosen .................................... */

    if (ursif2) {
	s_wsfi(&io___132);
	i__1 = month + 10;
	do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	e_wsfi();
	o__1.oerr = 1;
	o__1.ounit = iuccir;
	o__1.ofnmlen = 53;
	o__1.ofnm = filnam;
	o__1.orl = 0;
	o__1.osta = "OLD";
	o__1.oacc = 0;
	o__1.ofm = "FORMATTED";
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L8448;
	}
/*     &         FORM='UNFORMATTED') */
	io___133.ciunit = iuccir;
	s_rsfe(&io___133);
	do_fio(&c__1976, (char *)&f2[0], (ftnlen)sizeof(real));
	e_rsfe();
/*          READ(IUCCIR) F2 */
/* 1144  FORMAT('ursi',I2,'.asc') */
/* 1144  FORMAT('ursi',I2,'.bin') */
	cl__1.cerr = 0;
	cl__1.cunit = iuccir;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }

/* READ CCIR AND URSI COEFFICIENT SET FOR NMONTH, i.e. previous */
/* month if day is less than 15 and following month otherwise */

L4293:

/* first CCIR .............................................. */

    s_wsfi(&io___134);
    i__1 = nmonth + 10;
    do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
    e_wsfi();
    o__1.oerr = 1;
    o__1.ounit = iuccir;
    o__1.ofnmlen = 53;
    o__1.ofnm = filnam;
    o__1.orl = 0;
    o__1.osta = "OLD";
    o__1.oacc = 0;
    o__1.ofm = "FORMATTED";
    o__1.oblnk = 0;
    i__1 = f_open(&o__1);
    if (i__1 != 0) {
	goto L8448;
    }
/*     &          FORM='unFORMATTED') */
    io___135.ciunit = iuccir;
    s_rsfe(&io___135);
    do_fio(&c__1976, (char *)&f2n[0], (ftnlen)sizeof(real));
    do_fio(&c__882, (char *)&fm3n[0], (ftnlen)sizeof(real));
    e_rsfe();
/*        READ(IUCCIR) F2N,FM3N */
    cl__1.cerr = 0;
    cl__1.cunit = iuccir;
    cl__1.csta = 0;
    f_clos(&cl__1);

/* then URSI if chosen ..................................... */

    if (ursif2) {
	s_wsfi(&io___138);
	i__1 = nmonth + 10;
	do_fio(&c__1, (char *)&i__1, (ftnlen)sizeof(integer));
	e_wsfi();
	o__1.oerr = 1;
	o__1.ounit = iuccir;
	o__1.ofnmlen = 53;
	o__1.ofnm = filnam;
	o__1.orl = 0;
	o__1.osta = "OLD";
	o__1.oacc = 0;
	o__1.ofm = "FORMATTED";
	o__1.oblnk = 0;
	i__1 = f_open(&o__1);
	if (i__1 != 0) {
	    goto L8448;
	}
/*     &         FORM='unFORMATTED') */
	io___139.ciunit = iuccir;
	s_rsfe(&io___139);
	do_fio(&c__1976, (char *)&f2n[0], (ftnlen)sizeof(real));
	e_rsfe();
/*          READ(IUCCIR) F2N */
	cl__1.cerr = 0;
	cl__1.cunit = iuccir;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }
    nmono = nmonth;
    montho = month;
    iyearo = iyear;
    idayno = daynr;
    goto L4291;
L8448:
    io___140.ciunit = monito;
    s_wsfe(&io___140);
    do_fio(&c__1, filnam, (ftnlen)53);
    e_wsfe();
    goto L3330;

/* LINEAR INTERPOLATION IN SOLAR ACTIVITY. RSAT used for foF2 */

L4291:
    rr2 = arig[0] / (float)100.;
    rr2n = arig[1] / (float)100.;
    rr1 = (float)1. - rr2;
    rr1n = (float)1. - rr2n;
    for (i__ = 1; i__ <= 76; ++i__) {
	for (j = 1; j <= 13; ++j) {
	    k = j + (i__ - 1) * 13;
	    ff0n[k - 1] = f2n[j + (i__ + 76) * 13 - 1002] * rr1n + f2n[j + (
		    i__ + 152) * 13 - 1002] * rr2n;
/* L20: */
	    ff0[k - 1] = f2[j + (i__ + 76) * 13 - 1002] * rr1 + f2[j + (i__ + 
		    152) * 13 - 1002] * rr2;
	}
    }
    rr2 = rzar[0] / (float)100.;
    rr2n = rzar[1] / (float)100.;
    rr1 = (float)1. - rr2;
    rr1n = (float)1. - rr2n;
    for (i__ = 1; i__ <= 49; ++i__) {
	for (j = 1; j <= 9; ++j) {
	    k = j + (i__ - 1) * 9;
	    xm0n[k - 1] = fm3n[j + (i__ + 49) * 9 - 451] * rr1n + fm3n[j + (
		    i__ + 98) * 9 - 451] * rr2n;
/* L30: */
	    xm0[k - 1] = fm3[j + (i__ + 49) * 9 - 451] * rr1 + fm3[j + (i__ + 
		    98) * 9 - 451] * rr2;
	}
    }
L4292:
    zfof2 = fout_(&modip, &lati, &longi, &ut, ff0);
    fof2n = fout_(&modip, &lati, &longi, &ut, ff0n);
    zm3000 = xmout_(&modip, &lati, &longi, &ut, xm0);
    xm300n = xmout_(&modip, &lati, &longi, &ut, xm0n);
    midm = 15;
    if (month == 2) {
	midm = 14;
    }
    if (iday < midm) {
	yfof2 = fof2n + ttt * (zfof2 - fof2n);
	xm3000 = xm300n + ttt * (zm3000 - xm300n);
    } else {
	yfof2 = zfof2 + ttt * (fof2n - zfof2);
	xm3000 = zm3000 + ttt * (xm300n - zm3000);
    }
L501:
    if (fof2in) {
	fof2 = afof2;
    } else {
	fof2 = yfof2;
    }
    block1_1.nmf2 = fof2 * (float)1.24e10 * fof2;
    if (hmf2in) {
	block1_1.hmf2 = ahmf2;
	if (ahmf2 < (float)50.) {
	    r__1 = fof2 / foe;
	    block1_1.hmf2 = hmf2ed_(&magbr, &rssn, &r__1, &ahmf2);
	}
    } else {
	r__1 = fof2 / foe;
	block1_1.hmf2 = hmf2ed_(&magbr, &rssn, &r__1, &xm3000);
    }

/* topside profile parameters ............................. */

    cos2 = cos(mlat * const_1.umr);
    cos2 *= cos2;
    flu = (covsat - (float)40.) / (float)30.;
/* orr    FLU=(COV-40.0)/30.0 */
/* orr    flueta=188. */
/* orr    flumax=(flueta-40.)/30.0 */
    if (old79) {
	eta1 = cos2 * (float)-.0070305;
    } else {
	ex = exp(-mlat / (float)15.);
	ex1 = ex + 1;
	epin = ex * (float)4. / (ex1 * ex1);
	eta1 = epin * (float)-.02;
    }
    blo10_1.eta = eta1 + (float).058798 + flu * (cos2 * (float).0069724 - (
	    float).014065) + (cos2 * (float).004281 + (float).0024287 - fof2 *
	     (float)1.528e-4) * fof2;
    fluu = flu;
/* orr    if(fluu.gt.flumax) fluu=flumax */
    blo10_1.zeta = (float).078922 - cos2 * (float).0046702 - fluu * (float)
	    .019132 + flu * (float).0076545 * cos2 + (cos2 * (float).006029 + 
	    (float).0032513 - fof2 * (float)2.0872e-4) * fof2;
    blo10_1.beta = cos2 * (float)20.253 - (float)128.03 + flu * ((float)
	    -8.0755 - cos2 * (float).65896) + (cos2 * (float).71458 + (float)
	    .44041 - fof2 * (float).042966) * fof2;
    z__ = exp((float)94.5 / blo10_1.beta);
/* orr    Z=EXP(94.45/BETA) */
    z1 = z__ + 1;
    z2 = z__ / (blo10_1.beta * z1 * z1);
    blo10_1.delta = (blo10_1.eta / z1 - blo10_1.zeta / (float)2.) / (
	    blo10_1.eta * z2 + blo10_1.zeta / (float)400.);

/* bottomside profile parameters ............................. */

/* L1501: */
    block1_1.hmf1 = block1_1.hmf2;
    block3_1.hz = block1_1.hmf2;
    block4_1.hef = block4_1.hme;
    block2_1.b1 = (float)3.;
/* !!!!!!! INTERPOLATION FOR B0 OUT OF ARRAY B0F !!!!!!!!!!!!!!!!!!!!! */
    if (gulb0) {
	rogul_(&seaday, &xhi, &seax, &grat);
/* test */
	if (block5_1.night) {
	    grat = (float).91 - block1_1.hmf2 / (float)4e3;
	}
	b0cnew = block1_1.hmf2 * ((float)1. - grat);
	block2_1.b0 = b0cnew / b0b1[0];
    } else {
	block2_1.b0 = b0_tab__(&hour, &sax, &sux, &nseasn, &rssn, &modip);
    }
/* !!!!!!! F1-REGION PARAMETERS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    f1reg = FALSE_;
    block1_1.hmf1 = (float)0.;
    pnmf1 = (float)0.;
    block2_1.c1 = (float)0.;
    if (block5_1.night) {
	goto L150;
    }
    if (fof1in) {
	fof1 = afof1;
    } else {
	fof1 = fof1ed_(&absmbr, &rssn, &xhi);
    }
    if (fof1 < (float).001) {
	goto L150;
    }
    f1reg = TRUE_;
    block2_1.c1 = (float).11 / dela + (float).09;
    pnmf1 = fof1 * (float)1.24e10 * fof1;
L150:
    nmf1 = pnmf1;
/* !!!!!!! PARAMETER FOR E AND VALLEY-REGION !!!!!!!!!!!!!!!!!!!!! */
    xdel = xdels[season - 1] / dela;
    dndhbr = dnds[season - 1] / dela;
    r__1 = (float)10.5 / dela;
    hdeep = hpol_(&hour, &r__1, &c_b114, &sax, &sux, &c_b115, &c_b115);
    r__1 = (float)17.8 / dela;
    r__2 = (float)22. / dela + (float)45.;
    width = hpol_(&hour, &r__1, &r__2, &sax, &sux, &c_b115, &c_b115);
    depth = hpol_(&hour, &xdel, &c_b119, &sax, &sux, &c_b115, &c_b115);
    dlndh = hpol_(&hour, &dndhbr, &c_b122, &sax, &sux, &c_b115, &c_b115);
    if (depth < (float)1.) {
	goto L600;
    }
    if (block5_1.night) {
	depth = -depth;
    }
    tal_(&hdeep, &depth, &width, &dlndh, &ext, block5_1.e);
    if (! ext) {
	goto L667;
    }
    if (konsol > 1) {
	io___184.ciunit = konsol;
	s_wsfe(&io___184);
	e_wsfe();
    }
L600:
    width = (float)0.;
L667:
    block4_1.hef = block4_1.hme + width;
    vner = ((float)1. - dabs(depth) / (float)100.) * block4_1.nme;

/* Parameters below E  ............................. */

/* L2727: */
    block6_1.nmd = xmded_(&xhi, &rssn, &c_b130);
    block6_1.hmd = hpol_(&hour, &c_b119, &c_b132, &sax, &sux, &c_b115, &
	    c_b115);
    r__1 = (float).03 / dela + (float).02;
    f[0] = hpol_(&hour, &r__1, &c_b135, &sax, &sux, &c_b115, &c_b115);
    f[1] = hpol_(&hour, &c_b138, &c_b139, &sax, &sux, &c_b115, &c_b115);
    f[2] = hpol_(&hour, &c_b142, &c_b143, &sax, &sux, &c_b115, &c_b115);
    block7_1.fp1 = f[0];
    block7_1.fp2 = -block7_1.fp1 * block7_1.fp1 / (float)2.;
    block7_1.fp30 = (-f[1] * block7_1.fp2 - block7_1.fp1 + (float)1. / f[1]) /
	     (f[1] * f[1]);
    block7_1.fp3u = (-f[2] * block7_1.fp2 - block7_1.fp1 - (float)1. / f[2]) /
	     (f[2] * f[2]);
/* indermediate region between D and E region; parameters xkk */
/* and d1 are found such that the function reaches hdx/xdx/dxdh */
    block6_1.hdx = block6_1.hmd + f[1];
    x = block6_1.hdx - block6_1.hmd;
    xdx = block6_1.nmd * exp(x * (block7_1.fp1 + x * (block7_1.fp2 + x * 
	    block7_1.fp30)));
    dxdx = xdx * (block7_1.fp1 + x * (block7_1.fp2 * (float)2. + x * (float)
	    3. * block7_1.fp30));
    x = block4_1.hme - block6_1.hdx;
    block7_1.xkk = -dxdx * x / (xdx * log(xdx / block4_1.nme));
/* if exponent xkk is larger than xkkmax, then xkk will be set to xkkmax and */
/* d1 will be determined such that the point hdx/xdx is reached; derivative */
/* is no longer continuous. */
    xkkmax = (float)5.;
    if (block7_1.xkk > xkkmax) {
	block7_1.xkk = xkkmax;
	d__1 = (doublereal) x;
	d__2 = (doublereal) block7_1.xkk;
	block7_1.d1 = -log(xdx / block4_1.nme) / pow_dd(&d__1, &d__2);
    } else {
	d__1 = (doublereal) x;
	d__2 = (doublereal) (block7_1.xkk - (float)1.);
	block7_1.d1 = dxdx / (xdx * block7_1.xkk * pow_dd(&d__1, &d__2));
    }

/* SEARCH FOR HMF1 .................................................. */

/* L2726: */
    if (layver) {
	goto L6153;
    }
L924:
    if (! f1reg) {
	goto L380;
    }
    xe2h = xe2_(&block4_1.hef);
    regfa1_(&block4_1.hef, &block1_1.hmf2, &xe2h, &block1_1.nmf2, &c_b150, &
	    nmf1, (E_fp)xe2_, &schalt, &block1_1.hmf1);
    if (! schalt) {
	goto L380;
    }
    if (konsol > 1) {
	io___193.ciunit = konsol;
	s_wsfe(&io___193);
	e_wsfe();
    }
    iregfa = 1;

/* change B1 and try again .......................................... */

L9244:
    if (block2_1.b1 > (float)4.5) {
	switch (iregfa) {
	    case 1:  goto L7398;
	    case 2:  goto L8922;
	}
    }
    block2_1.b1 += (float).5;
    if (konsol > 1) {
	io___195.ciunit = konsol;
	s_wsfe(&io___195);
	r__1 = block2_1.b1 - (float).5;
	do_fio(&c__1, (char *)&r__1, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&block2_1.b1, (ftnlen)sizeof(real));
	e_wsfe();
    }
    if (gulb0) {
	ib1 = (integer) (block2_1.b1 * (float)2. - (float)5.);
	block2_1.b0 = b0cnew / b0b1[ib1 - 1];
    }
    goto L924;

/* omit F1 feature .................................................... */

L7398:
    if (konsol > 1) {
	io___197.ciunit = konsol;
	s_wsfe(&io___197);
	e_wsfe();
    }
    block1_1.hmf1 = (float)0.;
    nmf1 = (float)0.;
    block2_1.c1 = (float)0.;
    block2_1.b1 = (float)3.;
    f1reg = FALSE_;

/* SEARCH FOR HST [NE3(HST)=NME] .......................................... */

L380:
    rrrr = (float).5;
    if (f1reg) {
	hf1 = block1_1.hmf1;
	xf1 = nmf1;
	goto L3972;
    }
    rathh = (float).5;
L3973:
    hf1 = block4_1.hef + (block1_1.hmf2 - block4_1.hef) * rathh;
    xf1 = xe3_(&hf1);
    if (xf1 < block4_1.nme) {
	rathh += (float).1;
	goto L3973;
    }
L3972:
    h__ = hf1;
    deh = (float)10.;
    xxmin = xf1;
    hhmin = hf1;
L3895:
    h__ -= deh;
    if (h__ < block4_1.hef) {
	h__ += deh * 2;
	deh /= (float)10.;
	if (deh < (float)1.) {
	    goto L3885;
	}
    }
    xe3h = xe3_(&h__);
    if (xe3h < xxmin) {
	xxmin = xe3h;
	hhmin = h__;
    }
    if (xe3h > block4_1.nme) {
	goto L3895;
    }
    regfa1_(&h__, &hf1, &xe3h, &xf1, &c_b150, &block4_1.nme, (E_fp)xe3_, &
	    schalt, &block3_1.hst);
    block3_1.str = block3_1.hst;
    if (! schalt) {
	goto L360;
    }
L3885:
    if (konsol > 1) {
	io___207.ciunit = konsol;
	s_wsfe(&io___207);
	e_wsfe();
    }
    iregfa = 2;
    if (xxmin / block4_1.nme < (float)1.3) {
	goto L9244;
    }

/* assume linear interpolation between HZ and HEF .................. */

L8922:
    block3_1.hz = hhmin + (hf1 - hhmin) * rrrr;
    xnehz = xe3_(&block3_1.hz);
    if (xnehz - block4_1.nme < (float).001) {
	rrrr += (float).1;
	goto L8922;
    }
    if (konsol > 1) {
	io___209.ciunit = konsol;
	s_wsfe(&io___209);
	do_fio(&c__1, (char *)&block3_1.hz, (ftnlen)sizeof(real));
	do_fio(&c__1, (char *)&block4_1.hef, (ftnlen)sizeof(real));
	e_wsfe();
    }
    block3_1.t = (xnehz - block4_1.nme) / (block3_1.hz - block4_1.hef);
    block3_1.hst = (float)-333.;
    goto L4933;

/* calculate HZ, D and T ............................................ */

L360:
    block3_1.hz = (block3_1.hst + hf1) / (float)2.;
    d__ = block3_1.hz - block3_1.hst;
    block3_1.t = d__ * d__ / (block3_1.hz - block4_1.hef - d__);
    goto L4933;

/* LAY-functions for middle ionosphere */

L6153:
    if (hmf1in) {
	hmf1m = ahmf1;
    } else {
	hmf1m = xhi * (float).6428 + (float)165.;
    }
    hhalf = grat * block1_1.hmf2;
    hv1r = block4_1.hme + width;
    hv2r = block4_1.hme + hdeep;
    hhmf2 = block1_1.hmf2;
    inilay_(&block5_1.night, &block1_1.nmf2, &nmf1, &block4_1.nme, &vner, &
	    hhmf2, &hmf1m, &block4_1.hme, &hv1r, &hv2r, &hhalf, hxl, scl, amp,
	     &iiqu);
    if (iiqu == 1 && konsol > 1) {
	io___220.ciunit = konsol;
	s_wsfe(&io___220);
	e_wsfe();
    }
    if (iiqu == 2 && konsol > 1) {
	io___221.ciunit = konsol;
	s_wsfe(&io___221);
	e_wsfe();
    }
/* ---------- CALCULATION OF NEUTRAL TEMPERATURE PARAMETER------- */
L4933:
    hta = (float)120.;
    hte = (float)3e3;
    if (notem) {
	goto L240;
    }
    sec = ut * (float)3600.;
    cira86_(&daynr, &sec, &lati, &longi, &hour, &cov, &blotn_1.texos, &tn120, 
	    &blotn_1.sigma);
    if (hour != (float)0.) {
	iyz = lyear;
	idz = ldaynr;
	if (jf[18]) {
	    secni = ((float)24. - longi / 15) * (float)3600.;
	} else {
	    ut_lt__(&c__1, &utni, &c_b180, &longi, &iyz, &idz);
	    secni = utni * (float)3600.;
	}
	cira86_(&daynr, &secni, &lati, &longi, &c_b180, &cov, &texni, &tn1ni, 
		&signi);
    } else {
	texni = blotn_1.texos;
	tn1ni = tn120;
	signi = blotn_1.sigma;
    }
    blotn_1.tlbdh = blotn_1.texos - tn120;
    tlbdn = texni - tn1ni;

/* --------- CALCULATION OF ELECTRON TEMPERATURE PARAMETER-------- */

/* L881: */
/* !!!!!!!!!! TE(120KM)=TN(120KM) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    ate[0] = tn120;
/* !!!!!!!!!! TE-MAXIMUM (JICAMARCA,ARECIBO) !!!!!!!!!!!!!!!!!!!! */
/* Computing 2nd power */
    r__1 = mlat / (float)22.41;
    hmaxd = exp(-(r__1 * r__1)) * (float)60. + (float)210.;
    hmaxn = (float)150.;
    blote_1.ahh[1] = hpol_(&hour, &hmaxd, &hmaxn, &sax, &sux, &c_b115, &
	    c_b115);
/* Computing 2nd power */
    r__1 = mlat / (float)33.;
    tmaxd = exp(-(r__1 * r__1)) * (float)800. + (float)1500.;
    tmaxn = tn_(&hmaxn, &texni, &tlbdn, &signi) + 20;
    ate[1] = hpol_(&hour, &tmaxd, &tmaxn, &sax, &sux, &c_b115, &c_b115);
/* !!!!!!!!!! TE(300,400KM)=TE-AE-C !!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
/* !!!!!!!!!! TE(1400,3000KM)=TE-ISIS !!!!!!!!!!!!!!!!!!!!!!!!!!! */
    diplat = magbr;
    teba_(&diplat, &hour, &nseasn, tea);
    ate[2] = tea[0];
    ate[3] = tea[1];
    ate[5] = tea[2];
    ate[6] = tea[3];
/* !!!!!!!!!! TE(600KM)=TE-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    ett = exp(-mlat / (float)11.35);
    d__1 = (doublereal) (ett + 1);
    tet = (float)2900. - ett * (float)5600. / pow_dd(&d__1, &c_b187);
    ten = (float)1161. / (exp(-(absmlt - (float)45.) / (float)5.) + (float)1.)
	     + (float)839.;
    ate[4] = hpol_(&hour, &tet, &ten, &sax, &sux, &c_b188, &c_b188);
/* !!!!!!!!!! OPTION TO USE TE-NE-RELATION !!!!!!!!!!!!!!!!!!!!!! */
/* !!!!!!!!!! AT 300, 400 OR 600 KM  !!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    if (teneop) {
	for (i__ = 1; i__ <= 3; ++i__) {
/* L3395: */
	    if (tecon[i__ - 1]) {
		r__1 = -cov;
		ate[i__ + 1] = tede_(&hoa[i__ - 1], &xnar[i__ - 1], &r__1);
	    }
	}
    }
/* !!!!!!!!!! TE'S ARE CORRECTED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
/* !!!!!!!!!! ALSO TE > TN ENFORCED !!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    tnahh2 = tn_(&blote_1.ahh[1], &blotn_1.texos, &blotn_1.tlbdh, &
	    blotn_1.sigma);
    if (ate[1] < tnahh2) {
	ate[1] = tnahh2;
    }
    stte1 = (ate[1] - ate[0]) / (blote_1.ahh[1] - blote_1.ahh[0]);
    for (i__ = 2; i__ <= 6; ++i__) {
	tnahhi = tn_(&blote_1.ahh[i__], &blotn_1.texos, &blotn_1.tlbdh, &
		blotn_1.sigma);
	if (ate[i__] < tnahhi) {
	    ate[i__] = tnahhi;
	}
	stte2 = (ate[i__] - ate[i__ - 1]) / (blote_1.ahh[i__] - blote_1.ahh[
		i__ - 1]);
	ate[i__ - 1] -= (stte2 - stte1) * blote_1.dte[i__ - 2] * alog2;
/* L1901: */
	stte1 = stte2;
    }
/* !!!!!!!!!! GRADIENTS ARE CALCULATED WITH !!!!!!!!!!!!!!!!!!!! */
/* !!!!!!!!!! CORRECTED REGION BOUNDARIES !!!!!!!!!!!!!!!!!!!!!! */
    for (i__ = 1; i__ <= 6; ++i__) {
/* L1902: */
	blote_1.stte[i__ - 1] = (ate[i__] - ate[i__ - 1]) / (blote_1.ahh[i__] 
		- blote_1.ahh[i__ - 1]);
    }
    blote_1.ate1 = ate[0];
/* L887: */

/* ------------ CALCULATION OF ION TEMPERATURE PARAMETERS-------- */

/* !!!!!!!!!! TI(430KM,DAY)=TI-AEROS !!!!!!!!!!!!!!!!!!!!!!!!!!! */
    blotn_1.xsm1 = (float)430.;
    block8_1.xsm[0] = blotn_1.xsm1;
    z1 = exp(mlat * (float)-.09);
    z2 = z1 + (float)1.;
    tid1 = (float)1240. - z1 * (float)1400. / (z2 * z2);
    block8_1.mm[1] = hpol_(&hour, &c_b194, &c_b180, &sax, &sux, &c_b115, &
	    c_b115);
/* !!!!!!!!!!  TI < TE   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    ted1 = tea[5] + (float)30.;
    if (tid1 > ted1) {
	tid1 = ted1;
    }
/* !!!!!!!!!! TI(430KM,NIGHT)=TI-AEROS !!!!!!!!!!!!!!!!!!!!!!!!! */
    z1 = absmlt;
    z2 = z1 * (z1 * (float).024 + (float).47) * const_1.umr;
    z3 = cos(z2);
    tin1 = (float)1200. - r_sign(&c_b115, &z3) * (float)300. * sqrt((dabs(z3))
	    );
/* !!!!!!!!!! TN < TI < TE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    ten1 = tea[4];
    tnn1 = tn_(&blotn_1.xsm1, &texni, &tlbdn, &signi);
    if (ten1 < tnn1) {
	ten1 = tnn1;
    }
    if (tin1 > ten1) {
	tin1 = ten1;
    }
    if (tin1 < tnn1) {
	tin1 = tnn1;
    }
/* !!!!!!!!!! TI(430KM,LT) FROM STEP FUNCTION !!!!!!!!!!!!!!!!!! */
    ti1 = tin1;
    if (tid1 > tin1) {
	ti1 = hpol_(&hour, &tid1, &tin1, &sax, &sux, &c_b115, &c_b115);
    }
/* !!!!!!!!!! TANGENT ON TN DETERMINES HS !!!!!!!!!!!!!!!!!!!!!! */
    ti13 = teder_(&c_b201);
    ti50 = teder_(&c_b202);
    regfa1_(&c_b201, &c_b202, &ti13, &ti50, &c_b205, &ti1, (E_fp)teder_, &
	    schalt, &block8_1.hs);
    if (schalt) {
	block8_1.hs = (float)200.;
    }
    block8_1.tnhs = tn_(&block8_1.hs, &blotn_1.texos, &blotn_1.tlbdh, &
	    blotn_1.sigma);
    block8_1.mm[0] = dtndh_(&block8_1.hs, &blotn_1.texos, &blotn_1.tlbdh, &
	    blotn_1.sigma);
    if (schalt) {
	block8_1.mm[0] = (ti1 - block8_1.tnhs) / (blotn_1.xsm1 - block8_1.hs);
    }
    block8_1.mxsm = 2;
/* !!!!!!!!!! XTETI ALTITTUDE WHERE TE=TI !!!!!!!!!!!!!!!!!!!!!! */
/* L2391: */
    xtts = (float)500.;
    x = (float)500.;
L2390:
    x += xtts;
    if (x >= blote_1.ahh[6]) {
	goto L240;
    }
    tex = elte_(&x);
    tix = ti_(&x);
    if (tix < tex) {
	goto L2390;
    }
    x -= xtts;
    xtts /= (float)10.;
    if (xtts > (float).1) {
	goto L2390;
    }
    xteti = x + xtts * (float)5.;
/* !!!!!!!!!! TI=TE ABOVE XTETI !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! */
    block8_1.mxsm = 3;
    block8_1.mm[2] = blote_1.stte[5];
    block8_1.xsm[1] = xteti;
    if (xteti > blote_1.ahh[5]) {
	goto L240;
    }
    block8_1.mxsm = 4;
    block8_1.mm[2] = blote_1.stte[4];
    block8_1.mm[3] = blote_1.stte[5];
    block8_1.xsm[2] = blote_1.ahh[5];
    if (xteti > blote_1.ahh[4]) {
	goto L240;
    }
    block8_1.mxsm = 5;
    block8_1.dti[0] = (float)5.;
    block8_1.dti[1] = (float)5.;
    block8_1.mm[2] = blote_1.stte[3];
    block8_1.mm[3] = blote_1.stte[4];
    block8_1.mm[4] = blote_1.stte[5];
    block8_1.xsm[2] = blote_1.ahh[4];
    block8_1.xsm[3] = blote_1.ahh[5];

/* CALCULATION OF ION DENSITY PARAMETER.................. */

L240:
    if (noion) {
	goto L141;
    }
    hnia = (float)100.;
    if (dy) {
	hnia = (float)75.;
    }
    hnie = (float)2e3;

/* INPUT OF THE ION DENSITY PARAMETER ARRAYS PF1O,PF2O AND PF3O...... */

    rif[0] = (float)2.;
    if (abslat < (float)30.) {
	rif[0] = (float)1.;
    }
    rif[1] = (float)2.;
    if (cov < (float)100.) {
	rif[1] = (float)1.;
    }
    rif[2] = (real) season;
    if (season == 1) {
	rif[2] = (float)3.;
    }
    rif[3] = (float)1.;
    if (block5_1.night) {
	rif[3] = (float)2.;
    }
    koefp1_(pg1o);
    koefp2_(pg2o);
    koefp3_(pg3o);
    sufe_(pg1o, rif, &c__12, pf1o);
    sufe_(pg2o, rif, &c__4, pf2o);
    sufe_(pg3o, rif, &c__12, pf3o);

/* calculate O+ profile parameters */

    if (dabs(xhi) <= (float)90.) {
	zzz1 = cos(xhi * const_1.umr);
    } else {
	zzz1 = (float)0.;
    }
    msumo = 4;
    rdomax = (float)100.;
    mo[0] = epstep_(pf1o, &pf1o[1], &pf1o[2], &pf1o[3], &zzz1);
    mo[1] = epstep_(&pf1o[4], &pf1o[5], &pf1o[6], &pf1o[7], &zzz1);
    mo[2] = (float)0.;
    ho[0] = epstep_(&pf1o[8], &pf1o[9], &pf1o[10], &pf1o[11], &zzz1);
    ho[1] = (float)290.;
    if (rif[1] == (float)2. && rif[2] == (float)2.) {
	ho[1] = (float)237.;
    }
    ho[3] = pf2o[0];
    ho05 = pf2o[3];
    mo[3] = pf2o[1];
    mo[4] = pf2o[2];

/* adjust gradient MO(4) of O+ profile segment above F peak */

L7100:
    ho[2] = (alg100 - mo[4] * (ho[3] - ho05)) / mo[3] + ho[3];
    if (ho[2] <= ho[1] + (float)20.) {
	mo[3] += (float)-.001;
	goto L7100;
    }
    hfixo = (ho[1] + ho[2]) / (float)2.;

/* find height H0O of maximum O+ relative density */

    delx = (float)5.;
    x = ho[1];
    ymaxx = (float)0.;
L7102:
    x += delx;
    y = rpid_(&x, &hfixo, &rdomax, &msumo, mo, ddo, ho);
    if (y <= ymaxx) {
	if (delx <= (float).1) {
	    goto L7104;
	}
	x -= delx;
	delx /= (float)5.;
    } else {
	ymaxx = y;
    }
    goto L7102;
L7104:
    h0o = x - delx / (float)2.;
L7101:
    if (y < (float)100.) {
	goto L7103;
    }
    rdomax += (float)-.01;
    y = rpid_(&h0o, &hfixo, &rdomax, &msumo, mo, ddo, ho);
    goto L7101;
L7103:
    yo2h0o = (float)100. - y;
    yoh0o = y;

/* calculate parameters for O2+ profile */

    hfixo2 = pf3o[0];
    rdo2mx = pf3o[1];
    for (l = 1; l <= 2; ++l) {
	i__ = l << 1;
	ho2[l - 1] = pf3o[i__] + pf3o[i__ + 1] * zzz1;
/* L7105: */
	mo2[l] = pf3o[i__ + 6] + pf3o[i__ + 7] * zzz1;
    }
    mo2[0] = pf3o[6] + pf3o[7] * zzz1;
    if (hfixo2 > ho2[0]) {
	ymo2z = mo2[1];
    } else {
	ymo2z = mo2[0];
    }
    aldo21 = log(rdo2mx) + ymo2z * (ho2[0] - hfixo2);
    hfixo2 = (ho2[1] + ho2[0]) / (float)2.;
    rdo2mx = exp(aldo21 + mo2[1] * (hfixo2 - ho2[0]));

/* make sure that rd(O2+) is less or equal 100-rd(O+) at O+ maximum */

L7106:
    y = rpid_(&h0o, &hfixo2, &rdo2mx, &c__2, mo2, do2, ho2);
    if (y > yo2h0o) {
	mo2[2] += (float)-.02;
	goto L7106;
    }

/* use ratio of NO+ to O2+ density at O+ maximum to calculate */
/* NO+ density above the O+ maximum (H0O) */

    if (y < (float)1.) {
	nobo2 = (float)0.;
    } else {
	nobo2 = (yo2h0o - y) / y;
    }

/* CALCULATION FOR THE REQUIRED HEIGHT RANGE....................... */

L141:
    if (! f1reg) {
	block1_1.hmf1 = block3_1.hz;
    }
    height = *heibeg;
    kk = 1;
L300:
    if (noden) {
	goto L330;
    }
    if (height > hnee || height < hnea) {
	goto L330;
    }
    if (layver) {
	elede = (float)-9.;
	if (iiqu < 2) {
	    elede = xen_(&height, &block1_1.hmf2, &block1_1.nmf2, &
		    block4_1.hme, &c__4, hxl, scl, amp);
	}
    } else {
	elede = xe_(&height);
    }
    outf[kk * 11 + 1] = elede;
L330:
    if (notem) {
	goto L7108;
    }
    if (height > hte || height < hta) {
	goto L7108;
    }
    tnh = tn_(&height, &blotn_1.texos, &blotn_1.tlbdh, &blotn_1.sigma);
    tih = tnh;
    if (height >= block8_1.hs) {
	tih = ti_(&height);
    }
    teh = elte_(&height);
    if (tih < tnh) {
	tih = tnh;
    }
    if (teh < tih) {
	teh = tih;
    }
    outf[kk * 11 + 2] = tnh;
    outf[kk * 11 + 3] = tih;
    outf[kk * 11 + 4] = teh;
L7108:
    if (noion) {
	goto L7118;
    }
    if (height > hnie || height < hnia) {
	goto L7118;
    }
    if (dy) {
/*      call IONCOM(HEIGHT,XHI,LATI,COV,seamon,DION) */
	ioncom_new__(&height, &xhi, &lati, &cov, &seamon, dion);
	rox = dion[0];
	rhx = dion[1];
	rnx = dion[2];
	rhex = dion[3];
	rnox = dion[4];
	ro2x = dion[5];
	rclust = dion[6];
    } else {
	rox = rpid_(&height, &hfixo, &rdomax, &msumo, mo, ddo, ho);
	ro2x = rpid_(&height, &hfixo2, &rdo2mx, &c__2, mo2, do2, ho2);
	rdhhe_(&height, &h0o, &rox, &ro2x, &nobo2, &c_b226, &rhx, &rhex);
	rnox = rdno_(&height, &h0o, &ro2x, &rox, &nobo2);
	rnx = (float)-1.;
	rclust = (float)-1.;
    }

/* ion densities are given in percent of total electron density; */
/* to get ion densities in cm-3 use the following statement */
/*        xnorm=elede/100. */
    xnorm = (float)1.;
    outf[kk * 11 + 5] = rox * xnorm;
    outf[kk * 11 + 6] = rhx * xnorm;
    outf[kk * 11 + 7] = rhex * xnorm;
    outf[kk * 11 + 8] = ro2x * xnorm;
    outf[kk * 11 + 9] = rnox * xnorm;
    outf[kk * 11 + 10] = rclust * xnorm;
    outf[kk * 11 + 11] = rnx * xnorm;
L7118:
    height += *heistp;
    ++kk;
    if (kk <= numhei) {
	goto L300;
    }

/* ADDITIONAL PARAMETER FIELD OARR */

    if (noden) {
	goto L6192;
    }
    oarr[1] = block1_1.nmf2;
    oarr[2] = block1_1.hmf2;
    oarr[3] = nmf1;
    oarr[4] = block1_1.hmf1;
    oarr[5] = block4_1.nme;
    oarr[6] = block4_1.hme;
    oarr[7] = block6_1.nmd;
    oarr[8] = block6_1.hmd;
    oarr[9] = hhalf;
    oarr[10] = block2_1.b0;
    oarr[11] = vner;
    oarr[12] = block4_1.hef;
L6192:
    if (notem) {
	goto L6092;
    }
    oarr[13] = ate[1];
    oarr[14] = blote_1.ahh[1];
    oarr[15] = ate[2];
    oarr[16] = ate[3];
    oarr[17] = ate[4];
    oarr[18] = ate[5];
    oarr[19] = ate[6];
    oarr[20] = ate[0];
    oarr[21] = ti1;
    oarr[22] = xteti;
L6092:
    oarr[23] = xhi;
    oarr[24] = sundec;
    oarr[25] = dip;
    oarr[26] = magbr;
    oarr[27] = modip;
    oarr[28] = dela;
    oarr[29] = sax;
    oarr[30] = sux;
    oarr[31] = (real) season;
    oarr[32] = (real) nseasn;
    oarr[33] = rssn;
    oarr[34] = cov;
    oarr[35] = block2_1.b1;
    oarr[36] = xm3000;
L3330:
    return 0;
} /* iris13_ */


/* Subroutine */ int iri_web__(integer *jmag, logical *jf, real *alati, real *
	along, integer *iyyyy, integer *mmdd, integer *iut, real *dhour, real 
	*height, real *h_tec_max__, integer *ivar, real *vbeg, real *vend, 
	real *vstp, real *a, real *b)
{
    /* System generated locals */
    integer i__1;
    real r__1;

    /* Local variables */
    static real tecb, tect, oarr[38], outf[1100]	/* was [11][100] */, 
	    xvar[8];
    static integer i__;
    extern /* Subroutine */ int iris13_(logical *, integer *, real *, real *, 
	    integer *, integer *, real *, real *, real *, real *, real *, 
	    real *);
    static real xhour;
    static integer ii, numstp, iii;
    static real tec, oar[38];
    extern /* Subroutine */ int iri_tec__(real *, real *, integer *, real *, 
	    real *, real *);

/* ---------------------------------------------------------------- */
/* changes: */
/*       11/16/99 jf(20) instead of jf(17) */

/* ---------------------------------------------------------------- */
/* input:        jmag,alati,along,iyyyy,mmdd,dhour  see IRIS12 */
/*               height  height in km */
/*               h_tec_max  =0 no TEC otherwise upper boundary for integral */
/*               iut     =1 for UT       =0 for LT */
/*               ivar    =1      altitude */
/*                       =2,3    latitude,longitude */
/*                       =4,5,6  year,month,day */
/*                       =7      day of year */
/*                       =8      hour (UT or LT) */
/*               vbeg,vend,vstp  variable range (begin,end,step) */
/* output:       a       similar to outf in IRIS12 */
/*               b       similar to oarr in IRIS12 */


/*               numstp  number of steps; maximal 100 */
/* ---------------------------------------------------------------- */
    /* Parameter adjustments */
    b -= 39;
    a -= 12;
    --jf;

    /* Function Body */
    numstp = (integer) ((*vend - *vbeg) / *vstp) + 1;
    if (numstp > 100) {
	numstp = 100;
    }
    for (i__ = 1; i__ <= 38; ++i__) {
/* L6249: */
	oar[i__ - 1] = b[i__ + 38];
    }
    if (*ivar == 1) {
	for (i__ = 1; i__ <= 38; ++i__) {
/* L1249: */
	    oarr[i__ - 1] = oar[i__ - 1];
	}
	xhour = *dhour + *iut * (float)25.;
	iris13_(&jf[1], jmag, alati, along, iyyyy, mmdd, &xhour, vbeg, vend, 
		vstp, &a[12], oarr);
	if (*h_tec_max__ > (float)50.) {
	    iri_tec__(&c_b232, h_tec_max__, &c__2, &tec, &tect, &tecb);
	    oarr[36] = tec;
	    oarr[37] = tect;
	}
	for (i__ = 1; i__ <= 38; ++i__) {
/* L1111: */
	    b[i__ + 38] = oarr[i__ - 1];
	}
	return 0;
    }
    if (*height <= (float)0.) {
	*height = (float)100.;
    }
    xvar[1] = *alati;
    xvar[2] = *along;
    xvar[3] = (real) (*iyyyy);
    xvar[4] = (real) (*mmdd / 100);
    xvar[5] = *mmdd - xvar[4] * 100;
    xvar[6] = (r__1 = *mmdd * (float)1., dabs(r__1));
    xvar[7] = *dhour;
    xvar[*ivar - 1] = *vbeg;
    *alati = xvar[1];
    *along = xvar[2];
    *iyyyy = (integer) xvar[3];
    if (*ivar == 7) {
	*mmdd = -((integer) (*vbeg));
    } else {
	*mmdd = (integer) (xvar[4] * 100 + xvar[5]);
    }
    *dhour = xvar[7] + *iut * (float)25.;
    i__1 = numstp;
    for (i__ = 1; i__ <= i__1; ++i__) {
	for (iii = 1; iii <= 38; ++iii) {
/* L1349: */
	    oarr[iii - 1] = oar[iii - 1];
	}
	iris13_(&jf[1], jmag, alati, along, iyyyy, mmdd, dhour, height, 
		height, &c_b115, outf, oarr);
	if (*h_tec_max__ > (float)50.) {
	    iri_tec__(&c_b232, h_tec_max__, &c__2, &tec, &tect, &tecb);
	    oarr[36] = tec;
	    oarr[37] = tect;
	}
	for (ii = 1; ii <= 11; ++ii) {
/* L2: */
	    a[ii + i__ * 11] = outf[ii - 1];
	}
	for (ii = 1; ii <= 38; ++ii) {
/* L2222: */
	    b[ii + i__ * 38] = oarr[ii - 1];
	}
	xvar[*ivar - 1] += *vstp;
	*alati = xvar[1];
	*along = xvar[2];
	*iyyyy = (integer) xvar[3];
	if (*ivar == 7) {
	    *mmdd = -xvar[6];
	} else {
	    *mmdd = (integer) (xvar[4] * 100 + xvar[5]);
	}
	*dhour = xvar[7] + *iut * (float)25.;
/* L1: */
    }
    return 0;
} /* iri_web__ */

#ifdef __cplusplus
	}
#endif
