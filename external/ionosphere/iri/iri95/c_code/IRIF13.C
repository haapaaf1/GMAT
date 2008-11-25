/* irif13.f -- translated by f2c (version 19990311).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Common Block Declarations */

union {
    struct {
	real umr;
    } _1;
    struct {
	real faktor;
    } _2;
    struct {
	real dtr;
    } _3;
} const_;

#define const_1 (const_._1)
#define const_2 (const_._2)
#define const_3 (const_._3)

struct {
    real argmax;
} argexp_;

#define argexp_1 argexp_

struct {
    real humr, dumr;
} const1_;

#define const1_1 const1_

struct {
    real hmf2, xnmf2, hmf1;
} block1_;

#define block1_1 block1_

union {
    struct {
	real beta, eta, delta, zeta;
    } _1;
    struct {
	real beta, eta, del, zeta;
    } _2;
} blo10_;

#define blo10_1 (blo10_._1)
#define blo10_2 (blo10_._2)

struct {
    real b0, b1, c1;
} block2_;

#define block2_1 block2_

struct {
    real hz, t, hst, str;
} block3_;

#define block3_1 block3_

struct {
    real hme, xnme, hef;
} block4_;

#define block4_1 block4_

struct {
    logical night;
    real e[4];
} block5_;

#define block5_1 block5_

struct {
    real hmd, xnmd, hdx;
} block6_;

#define block6_1 block6_

struct {
    real d1, xkk, fp30, fp3u, fp1, fp2;
} block7_;

#define block7_1 block7_

struct {
    real ah[7], ate1, st[6], d__[5];
} blote_;

#define blote_1 blote_

struct {
    real hs, tnhs, xsm[4], mm[5], g[4];
    integer m;
} block8_;

#define block8_1 block8_

struct {
    real xsm1, tex, tlbd, sig;
} blotn_;

#define blotn_1 blotn_

/* Table of constant values */

static real c_b3 = (float)394.5;
static real c_b5 = (float)100.;
static real c_b6 = (float)300.;
static real c_b18 = (float)1.;
static integer c__8 = 8;
static doublereal c_b34 = 10.;
static integer c__6 = 6;
static integer c__9 = 9;
static integer c__76 = 76;
static integer c__13 = 13;
static integer c__988 = 988;
static integer c__4 = 4;
static integer c__7 = 7;
static integer c__49 = 49;
static integer c__441 = 441;
static doublereal c_b91 = .25;
static doublereal c_b94 = 2.7;
static real c_b200 = (float)2.;
static real c_b204 = (float).1;
static real c_b205 = (float).15;
static integer c__3 = 3;
static integer c__1 = 1;
static integer c__0 = 0;

/* IRIF13.FOR */
/* ************************************************************** */
/* changes from IRIFU9 to IRIF10: */
/*       SOCO for solar zenith angle */
/*       ACOS and ASIN argument forced to be within -1 / +1 */
/*       EPSTEIN functions corrected for large arguments */
/* ************************************************************** */
/* changes from IRIF10 to IRIF11: */
/*       LAY subroutines introduced */
/*       TEBA corrected for 1400 km */
/* ************************************************************** */
/* changes from IRIF11 to IRIF12: */
/*       Neutral temperature subroutines now in CIRA86.FOR */
/*       TEDER changed */
/*       All names with 6 or more characters replaced */
/*       10/29/91 XEN: 10^ in loop, instead of at the end */
/*       1/21/93 B0_TAB instead of B0POL */
/*       9/22/94 Alleviate underflow condition in IONCOM exp() */
/* ************************************************************** */
/* changes from IRIF12 to IRIF13: */
/*        9/18/95 MODA: add leap year and number of days in month */
/*        9/29/95 replace F2out with FOUT and XMOUT. */
/*       10/ 5/95 add TN and DTNDH; earlier in CIRA86.FOR */
/*       10/ 6/95 add TCON for reading indices */
/*       10/20/95 MODA: IN=1 MONTH=IMO */
/*       10/20/95 TCON: now includes RZ interpolation */
/*       11/05/95 IONCOM->IONCO1, added IONCOM_new, IONCO2 */
/*       11/05/95 LSTID added for strom-time updating */
/*       11/06/95 ROGUL: transition 20. instead of 15. */
/*       12/01/95 add UT_LT for (date-)correct UT<->LT conversion */
/*       01/16/96 TCON: add IMST to SAVE statement */
/*       02/02/96 ROGUL: 15. reinstated */
/*       02/07/96 UT_LT: ddd, dddend integer, no leap year 2000 */
/*       03/15/96 ZERO: finding delta for topside */
/*       03/18/96 UT_LT: mode=1, change of year */
/*       12/09/96 since 2000 is leap, delete y/100*100 condition */
/*       04/25/97 XMDED: minimal value also daytime */
/*       05/18/98 TCON: changes to IG_RZ (update date); -R = Cov */
/*       05/19/98 Replaced IONCO2&APROK; HEI,XHI in IONCOM_NEW */
/*       10/01/98 added INITIALIZE */
/*       04/30/99 MODA: reset bb(2)=28 */
/*       11/08/99 avoid negative x value in function XE2. Set x=0. */
/*       11/09/99 added COMMON/const1/humr,dumr also for CIRA86 */
/*       11/16/99 EXIT statement in APROK leaves do loop - problem for C */

/* ************************************************************** */
/* ********** INTERNATIONAL REFERENCE IONOSPHERE **************** */
/* ************************************************************** */
/* ****************  FUNCTIONS,SUBROUTINES  ********************* */
/* ************************************************************** */
/* ** IMPORTANT!! INITIALIZE (needs to be called before using */
/* **                 subroutines or functions) */
/* ** NE:         XE1,ZERO,DXE1N,XE2,XE3,XE4,XE5,XE6,XE */
/* ** TE/TI:      TEBA,SPHARM,ELTE,TEDE,TI,TEDER,TN,DTNDH */
/* ** NI:         RPID,RDHHE,RDNO,KOEFP1,KOEFP2,KOEFP3,SUFE */
/* ** PEAKS:      FOUT,XMOUT,HMF2ED,FOF1ED,FOEEDI,XMDED,GAMMA1 */
/* ** MAG. FIELD: GGM,FIELDG */
/* ** FUNCTIONS:  REGFA1,TAL */
/* ** TIME:       SOCO,HPOL,MODA,UT_LT */
/* ** INTERPOL.:  B0POL,B0_TAB */
/* ** EPSTEIN:    RLAY,D1LAY,D2LAY,EPTR,EPST,EPSTEP,EPLA */
/* ** LAY:        XE2TO5,XEN,ROGUL,VALGUL,LNGLSN,LSKNM,INILAY */
/* ** NI-new:     IONCOM,IONCOM_NEW,IONCO1,IONCO2,APROK */
/* ** INDICES:    TCON */
/* ** Updating:   LSTID */
/* ************************************************************** */

/* ************************************************************** */
/* ***  -------------------ADDRESSES------------------------  *** */
/* ***  I  PROF. K. RAWER             DR. D. BILITZA       I  *** */
/* ***  I  HERRENSTR. 43              GSFC CODE 933        I  *** */
/* ***  I  7801 MARCH 1               GREENBELT MD 20771   I  *** */
/* ***  I  F.R.G.                     USA                  I  *** */
/* ***  ----------------------------------------------------  *** */
/* ************************************************************** */
/* ************************************************************** */


/* Subroutine */ int initialize_()
{
    /* Builtin functions */
    double atan(doublereal);

    /* Local variables */
    static real pi;

    argexp_1.argmax = (float)88.;
    pi = atan((float)1.) * (float)4.;
    const_1.umr = pi / (float)180.;
    const1_1.humr = pi / (float)12.;
    const1_1.dumr = pi / (float)182.5;
    return 0;
} /* initialize_ */



/* ************************************************************* */
/* *************** ELECTRON DENSITY **************************** */
/* ************************************************************* */


doublereal xe1_(real *h__)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double r_sign(real *, real *), exp(doublereal);

    /* Local variables */
    static real dxdh;
    extern doublereal eptr_(real *, real *, real *);
    static real eptr1, eptr2, x, y, x0, xmx0;

/* ---------------------------------------------------------------- */
/* REPRESENTING ELECTRON DENSITY(M-3) IN THE TOPSIDE IONOSPHERE */
/* (H=HMF2....1000 KM) BY HARMONIZED BENT-MODEL ADMITTING */
/* VARIABILITY OFGLOBAL PARAMETER ETA,ZETA,BETA,DELTA WITH */
/* GEOM. LATITUDE, SMOOTHED SOLAR FLUX AND CRITICAL FREQUENCY */
/* (SEE MAIN PROGRAM). */
/* [REF.:K.RAWER,S.RAMAKRISHNAN,1978] */
/* ---------------------------------------------------------------- */
    dxdh = ((float)1e3 - block1_1.hmf2) / (float)700.;
    x0 = (float)300. - blo10_1.delta;
    xmx0 = (*h__ - block1_1.hmf2) / dxdh;
    x = xmx0 + x0;
    eptr1 = eptr_(&x, &blo10_1.beta, &c_b3) - eptr_(&x0, &blo10_1.beta, &c_b3)
	    ;
    eptr2 = eptr_(&x, &c_b5, &c_b6) - eptr_(&x0, &c_b5, &c_b6);
    y = blo10_1.beta * blo10_1.eta * eptr1 + blo10_1.zeta * (eptr2 * (float)
	    100. - xmx0);
    y *= dxdh;
    if (dabs(y) > argexp_1.argmax) {
	y = r_sign(&argexp_1.argmax, &y);
    }
    ret_val = block1_1.xnmf2 * exp(-y);
    return ret_val;
} /* xe1_ */



doublereal zero_(real *delta)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real z1, z2, arg1;

/* FOR A PEAK AT X0 THE FUNCTION ZERO HAS TO BE EQUAL TO 0. */
    arg1 = *delta / (float)100.;
    if (dabs(arg1) < argexp_1.argmax) {
	z1 = (float)1. / (exp(arg1) + (float)1.);
    } else if (arg1 < (float)0.) {
	z1 = (float)1.;
    } else {
	z1 = (float)0.;
    }
    arg1 = (*delta + (float)94.5) / blo10_2.beta;
    if (dabs(arg1) < argexp_1.argmax) {
	z2 = (float)1. / (exp(arg1) + (float)1.);
    } else if (arg1 < (float)0.) {
	z2 = (float)1.;
    } else {
	z2 = (float)0.;
    }
    ret_val = blo10_2.zeta * ((float)1. - z1) - blo10_2.eta * z2;
    return ret_val;
} /* zero_ */



doublereal dxe1n_(real *h__)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal epst_(real *, real *, real *);
    static real epst1, epst2, x, x0;

/* LOGARITHMIC DERIVATIVE OF FUNCTION XE1 (KM-1). */
    x0 = (float)300. - blo10_1.delta;
    x = (*h__ - block1_1.hmf2) / ((float)1e3 - block1_1.hmf2) * (float)700. + 
	    x0;
    epst2 = epst_(&x, &c_b5, &c_b6);
    epst1 = epst_(&x, &blo10_1.beta, &c_b3);
    ret_val = -blo10_1.eta * epst1 + blo10_1.zeta * ((float)1. - epst2);
    return ret_val;
} /* dxe1n_ */



doublereal xe2_(real *h__)
{
    /* System generated locals */
    real ret_val;
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), exp(doublereal), cosh(
	    doublereal);

    /* Local variables */
    static real x, z__;

/* ELECTRON DENSITY FOR THE BOTTOMSIDE F-REGION (HMF1...HMF2). */
    x = (block1_1.hmf2 - *h__) / block2_1.b0;
    if (x <= (float)0.) {
	x = (float)0.;
    }
    d__1 = (doublereal) x;
    d__2 = (doublereal) block2_1.b1;
    z__ = pow_dd(&d__1, &d__2);
    if (z__ > argexp_1.argmax) {
	z__ = argexp_1.argmax;
    }
    ret_val = block1_1.xnmf2 * exp(-z__) / cosh(x);
    return ret_val;
} /* xe2_ */



doublereal xe3_(real *h__)
{
    /* System generated locals */
    real ret_val, r__1;

    /* Builtin functions */
    double sqrt(doublereal);

    /* Local variables */
    extern doublereal xe2_(real *);

/* ELECTRON DENSITY FOR THE F1-LAYER (HZ.....HMF1). */
    ret_val = xe2_(h__) + block1_1.xnmf2 * block2_1.c1 * sqrt((r__1 = 
	    block1_1.hmf1 - *h__, dabs(r__1)) / block2_1.b0);
    return ret_val;
} /* xe3_ */



doublereal xe4_(real *h__)
{
    /* System generated locals */
    real ret_val, r__1;

    /* Builtin functions */
    double r_sign(real *, real *), sqrt(doublereal);

    /* Local variables */
    extern doublereal xe3_(real *);

/* ELECTRON DENSITY FOR THE INDERMEDIUM REGION (HEF..HZ). */
    if (block3_1.hst < (float)0.) {
	goto L100;
    }
    r__1 = block3_1.hz + block3_1.t / (float)2. - r_sign(&c_b18, &block3_1.t) 
	    * sqrt(block3_1.t * (block3_1.hz - *h__ + block3_1.t / (float)4.))
	    ;
    ret_val = xe3_(&r__1);
    return ret_val;
L100:
    ret_val = block4_1.xnme + block3_1.t * (*h__ - block4_1.hef);
    return ret_val;
} /* xe4_ */



doublereal xe5_(real *h__)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real t1, t3;

/* ELECTRON DENSITY FOR THE E AND VALLEY REGION (HME..HEF). */
    t3 = *h__ - block4_1.hme;
    t1 = t3 * t3 * (block5_1.e[0] + t3 * (block5_1.e[1] + t3 * (block5_1.e[2] 
	    + t3 * block5_1.e[3])));
    if (block5_1.night) {
	goto L100;
    }
    ret_val = block4_1.xnme * (t1 + 1);
    return ret_val;
L100:
    ret_val = block4_1.xnme * exp(t1);
    return ret_val;
} /* xe5_ */



doublereal xe6_(real *h__)
{
    /* System generated locals */
    real ret_val;
    doublereal d__1, d__2;

    /* Builtin functions */
    double exp(doublereal), pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static real z__, fp3;

/* ELECTRON DENSITY FOR THE D REGION (HA...HME). */
    if (*h__ > block6_1.hdx) {
	goto L100;
    }
    z__ = *h__ - block6_1.hmd;
    fp3 = block7_1.fp3u;
    if (z__ > (float)0.) {
	fp3 = block7_1.fp30;
    }
    ret_val = block6_1.xnmd * exp(z__ * (block7_1.fp1 + z__ * (block7_1.fp2 + 
	    z__ * fp3)));
    return ret_val;
L100:
    z__ = block4_1.hme - *h__;
    d__1 = (doublereal) z__;
    d__2 = (doublereal) block7_1.xkk;
    ret_val = block4_1.xnme * exp(-block7_1.d1 * pow_dd(&d__1, &d__2));
    return ret_val;
} /* xe6_ */



doublereal xe_(real *h__)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal xe1_(real *), xe2_(real *), xe3_(real *), xe4_(real *), 
	    xe5_(real *), xe6_(real *);

/* ELECTRON DENSITY BEETWEEN HA(KM) AND 1000 KM */
/* SUMMARIZING PROCEDURES  NE1....6; */
    if (*h__ < block1_1.hmf2) {
	goto L100;
    }
    ret_val = xe1_(h__);
    return ret_val;
L100:
    if (*h__ < block1_1.hmf1) {
	goto L300;
    }
    ret_val = xe2_(h__);
    return ret_val;
L300:
    if (*h__ < block3_1.hz) {
	goto L400;
    }
    ret_val = xe3_(h__);
    return ret_val;
L400:
    if (*h__ < block4_1.hef) {
	goto L500;
    }
    ret_val = xe4_(h__);
    return ret_val;
L500:
    if (*h__ < block4_1.hme) {
	goto L600;
    }
    ret_val = xe5_(h__);
    return ret_val;
L600:
    ret_val = xe6_(h__);
    return ret_val;
} /* xe_ */


/* ********************************************************** */
/* ***************** ELECTRON TEMPERATURE ******************** */
/* ********************************************************** */

/* Subroutine */ int teba_(real *dipl, real *slt, integer *ns, real *te)
{
    /* Initialized data */

    static real c__[648]	/* was [4][2][81] */ = { (float)3.1,(float)
	    3.136,(float)3.372,(float)3.574,(float)3.13654,(float)3.144,(
	    float)3.367,(float)3.574,(float)-.003215,(float).006498,(float)
	    .01006,(float)0.,(float).006796,(float).008571,(float).01038,(
	    float)-.005639,(float).244,(float).2289,(float).1436,(float)
	    .07537,(float).181413,(float).2539,(float).1407,(float).07094,(
	    float)-4.613e-4,(float).01859,(float).002023,(float)0.,(float)
	    .08564,(float).06937,(float).03622,(float)-.03347,(float)-.01711,(
	    float)-.03328,(float)-.05166,(float)-.08459,(float)-.032856,(
	    float)-.01667,(float)-.03144,(float)-.0861,(float).02605,(float)
	    -.004889,(float).009606,(float)0.,(float)-.003508,(float).02249,(
	    float).0112,(float)-.02877,(float)-.09546,(float)-.03054,(float)
	    -.05596,(float)-.0294,(float)-.01438,(float)-.04162,(float)
	    -.05674,(float)-.03154,(float).01794,(float)-.01773,(float)
	    4.914e-4,(float)0.,(float)-.02454,(float).01201,(float).03219,(
	    float)-.002847,(float).0127,(float)-.01728,(float)-.003124,(float)
	    .04547,(float).002745,(float).02435,(float).001288,(float).01235,(
	    float).02791,(float).06555,(float)-.04713,(float)-.05321,(float)
	    .05284,(float).05232,(float)-.05799,(float)-.05966,(float).01536,(
	    float).01775,(float)-.007371,(float)0.,(float).01136,(float)
	    .02521,(float)-.004609,(float)-.003236,(float)-.006629,(float)
	    -.02488,(float)-.004823,(float).004328,(float)-.01956,(float)
	    -.0199,(float).003252,(float)3.795e-4,(float)-.003616,(float)
	    -.009498,(float)-.002213,(float)0.,(float)-.005805,(float)
	    -.007671,(float)-2.859e-4,(float)-8.634e-4,(float).01229,(float)
	    .01493,(float).006569,(float).006022,(float).002801,(float).01264,
	    (float).01226,(float).003377,(float)4.147e-4,(float).00281,(float)
	    -1.962e-4,(float)0.,(float)-.001211,(float)-.001551,(float)
	    -.004539,(float)-1.071e-4,(float).001447,(float).002406,(float)
	    3.309e-4,(float)-9.168e-4,(float).004127,(float)-.001928,(float)
	    .00131,(float)-.002151,(float)-4.453e-4,(float).005436,(float)
	    -3.908e-4,(float)0.,(float).002909,(float).003652,(float)
	    -5.603e-4,(float)-4.057e-4,(float)-.1853,(float)-.2115,(float)
	    -.2836,(float)-.1768,(float)-.25751,(float)-.2019,(float)-.311,(
	    float)-.1783,(float)-.01245,(float).007007,(float).007829,(float)
	    0.,(float)-.0037915,(float).005697,(float)-.001268,(float).0126,(
	    float)-.03675,(float)-.05129,(float).01175,(float).0294,(float)
	    -.0136,(float)-.03159,(float).01539,(float).02835,(float).004965,(
	    float)-.007327,(float)9.919e-4,(float)0.,(float)-.013225,(float)
	    -.01451,(float).003146,(float)-.00242,(float).00546,(float).02402,
	    (float).006589,(float)5.902e-4,(float).01202,(float).02868,(float)
	    .007787,(float).003002,(float).008117,(float).004772,(float)
	    .002045,(float)0.,(float).01256,(float).01377,(float)-.00143,(
	    float)-.004684,(float)-.01002,(float)-.007374,(float)-.007346,(
	    float)-.009047,(float)-.012165,(float)-.004383,(float)-.00482,(
	    float)-.006756,(float)5.466e-4,(float)-3.835e-4,(float)-8.9e-4,(
	    float)0.,(float).01326,(float).01172,(float).002924,(float)
	    -7.493e-4,(float)-.03087,(float)-.05013,(float)-.0347,(float)
	    -.06555,(float)-.07123,(float)-.05683,(float)-.09981,(float)
	    -.06147,(float)-.003435,(float).002866,(float)-.004977,(float)0.,(
	    float)5.793e-4,(float).003593,(float)-.007838,(float)-.005636,(
	    float)-1.107e-4,(float).002216,(float).00147,(float)-.001033,(
	    float).001537,(float).003571,(float)-1.663e-4,(float)-.001234,(
	    float).002199,(float)2.412e-4,(float)-2.823e-6,(float)0.,(float)
	    .006914,(float).003282,(float)4.769e-4,(float)-.001613,(float)
	    4.115e-4,(float).002094,(float)6.465e-4,(float).001674,(float)
	    -.004173,(float).001732,(float).004148,(float)-6.353e-5,(float)
	    6.061e-4,(float).00122,(float)-1.448e-4,(float)0.,(float)1.052e-4,
	    (float)-4.921e-4,(float)-.001008,(float)-2.503e-4,(float)2.916e-4,
	    (float)-1.703e-4,(float).001401,(float)2.802e-4,(float)-5.765e-4,(
	    float)-.001165,(float)-9.79e-4,(float)-1.729e-4,(float)-.06584,(
	    float)-.1082,(float)-.08988,(float)-.06786,(float)-.04041,(float)
	    -.1066,(float)-.09049,(float)-.07148,(float).004729,(float)
	    -.004992,(float)-3.293e-5,(float)0.,(float)-.001752,(float)
	    -.01892,(float)-.002994,(float).005326,(float)-.001523,(float)
	    -.004065,(float)-.001848,(float).004193,(float)-.00542,(float)
	    .00357,(float)-.006748,(float).004006,(float)6.689e-4,(float)
	    .003615,(float)4.439e-4,(float)0.,(float)-.00684,(float)-8.631e-4,
	    (float)-9.889e-4,(float)6.484e-4,(float).001031,(float)-.002738,(
	    float)-.001263,(float)-6.448e-4,(float)8.921e-4,(float)-.001876,(
	    float).001488,(float)-1.046e-4,(float)5.398e-4,(float)-7.177e-4,(
	    float)3.17e-4,(float)0.,(float)-.002228,(float)-8.414e-5,(float)
	    -.001154,(float)-6.034e-4,(float)-.001924,(float)2.173e-4,(float)
	    -6.227e-4,(float)9.277e-4,(float).001428,(float).002356,(float)
	    -8.412e-5,(float)-9.435e-4,(float)-.04565,(float)-.04373,(float)
	    .01721,(float)-.01634,(float).006635,(float)-.04259,(float)
	    -.01302,(float)-.002385,(float).007244,(float)-.00375,(float)
	    -.00199,(float)0.,(float)-.0048045,(float)-.00322,(float)-.004859,
	    (float).006853,(float)-8.543e-5,(float).005507,(float)-4.627e-4,(
	    float)-.002531,(float)-.001659,(float).004641,(float)-7.172e-4,(
	    float).00151,(float).001052,(float)-.001567,(float)2.897e-6,(
	    float)0.,(float)-9.341e-4,(float)6.223e-4,(float)-9.401e-4,(float)
	    .001319,(float)-6.696e-4,(float)-.001458,(float)-5.454e-4,(float)
	    1.93e-5,(float)2.23e-4,(float)-.00168,(float)9.101e-4,(float)
	    9.049e-5,(float)-7.492e-4,(float)-7.397e-4,(float)3.385e-4,(float)
	    0.,(float)-9.995e-4,(float)-1.243e-4,(float)-1.735e-4,(float)
	    -1.999e-4,(float).04405,(float).07903,(float).08432,(float).0528,(
	    float).04285,(float).07393,(float).07055,(float).03976,(float)
	    .003047,(float).004131,(float)-.001951,(float)0.,(float)-5.211e-4,
	    (float)-.003143,(float).006398,(float).002802,(float).002858,(
	    float).003714,(float).001487,(float).002438,(float)-.003293,(
	    float)-.002362,(float)-.003103,(float)-.00103,(float)-1.465e-4,(
	    float).001073,(float).001042,(float)0.,(float).00179,(float)
	    .001235,(float)-9.38e-4,(float)5.599e-4,(float).001195,(float)
	    -8.991e-4,(float)-4.788e-4,(float)-5.292e-4,(float)6.435e-4,(
	    float)-.001551,(float)-4e-4,(float)-4.791e-4,(float)-1.024e-4,(
	    float)2.976e-4,(float)-1.276e-4,(float)0.,(float)-1.891e-4,(float)
	    2.099e-4,(float)-.001165,(float)-8.46e-5,(float).04582,(float)
	    .02623,(float).02373,(float).01555,(float).03844,(float).02299,(
	    float).02713,(float).02683,(float)8.749e-4,(float).002344,(float)
	    .002409,(float)0.,(float).00359,(float).005301,(float)-.001654,(
	    float).00427,(float)3.011e-4,(float)5.608e-4,(float)5.263e-4,(
	    float)-.003259,(float)-8.139e-4,(float)-.004306,(float).002781,(
	    float)5.911e-4,(float)4.473e-4,(float)4.124e-4,(float).001301,(
	    float)0.,(float)-.001996,(float)-.001303,(float)-5.215e-6,(float)
	    2.987e-4,(float)-2.782e-4,(float)1.509e-4,(float)-4.177e-4,(float)
	    -5.998e-4,(float)2.398e-4,(float)7.687e-6,(float)2.258e-4,(float)
	    -2.08e-4,(float).04911,(float).05103,(float).03974,(float).03168,(
	    float).02938,(float).05305,(float).05022,(float).01396,(float)
	    -.01016,(float).00345,(float)1.418e-4,(float)0.,(float).00761,(
	    float).006642,(float).0095,(float)-.001922,(float).0027,(float)
	    .001283,(float)-.001048,(float).002382,(float).00347655,(float)
	    -.001686,(float)4.147e-4,(float)-.001063,(float)-9.304e-4,(float)
	    7.238e-4,(float)-2.982e-4,(float)0.,(float).001707,(float).001048,
	    (float)3.499e-4,(float)3.803e-4,(float)-.001202,(float)-3.464e-5,(
	    float)-3.396e-5,(float)-4.078e-4,(float)2.769e-4,(float)5.958e-4,(
	    float)-6.097e-4,(float)1.343e-4,(float).0221,(float).01663,(float)
	    .0131,(float).02312,(float)-.0157,(float).04341,(float).04118,(
	    float).01771,(float).002566,(float)-.001644,(float).001413,(float)
	    0.,(float)9.83e-4,(float)-8.819e-5,(float).006556,(float)-.001038,
	    (float)-1.22e-4,(float)-7.1e-4,(float)-1.373e-4,(float)1.481e-4,(
	    float)-6.532e-4,(float)-3.33e-4,(float).003793,(float)-4.645e-4,(
	    float)3.987e-4,(float)5.281e-4,(float)2.638e-4,(float)0.,(float)
	    9.29e-5,(float)-2.158e-4,(float)-1.226e-4,(float)-2.481e-4,(float)
	    -.05744,(float)-.02729,(float)-.04171,(float)-.01885,(float)
	    -.02506,(float)-.04106,(float)-.02517,(float)-.02251,(float)
	    .004408,(float).003556,(float)-5.932e-4,(float)0.,(float).004681,(
	    float).004191,(float)1.491e-4,(float)-.0029,(float)-.003497,(
	    float)-.003391,(float)-7.523e-4,(float).001144,(float).001461,(
	    float).002045,(float).001075,(float)-3.977e-4,(float)8.3e-4,(
	    float)-1.787e-4,(float)-6.883e-4,(float)0.,(float)-3.757e-6,(
	    float)-1.437e-4,(float)4.531e-4,(float)-5.16e-4,(float)-.03536,(
	    float).002154,(float)-.02355,(float)-.009952,(float)-.009728,(
	    float)-.01803,(float)-.009012,(float)-.008079,(float)-.008813,(
	    float).006476,(float)5.695e-4,(float)0.,(float).002315,(float)
	    -8.072e-4,(float).003343,(float)-.001528,(float).002423,(float)
	    -8.282e-4,(float)-2.219e-5,(float)-5.51e-4,(float)6.377e-4,(float)
	    -4.24e-4,(float).003431,(float)3.06e-4,(float)-.02994,(float)
	    -.02361,(float)-.02301,(float)-.0202,(float)-.01705,(float)-.026,(
	    float)-.02519,(float)-.01582,(float)-.001929,(float)9.557e-4,(
	    float)-9.962e-5,(float)0.,(float).002767,(float)-.002329,(float)
	    3.793e-5,(float)-8.536e-4,(float)-5.268e-4,(float)3.205e-4,(float)
	    -6.761e-4,(float)-7.283e-5,(float)-6.992e-4,(float)5.949e-4,(
	    float)5.973e-4,(float)1.565e-4,(float)-.02228,(float)-.02301,(
	    float).00204,(float)-.01272,(float)-.0115,(float)-.01371,(float)
	    -.01423,(float)-.01252,(float).003385,(float)-8.54e-4,(float)
	    -5.479e-4,(float)0.,(float)-.001644,(float)-.002188,(float)
	    -.00132,(float)2.319e-4,(float).0413,(float)-.01126,(float).02591,
	    (float).002224,(float).003355,(float).01788,(float)-.006048,(
	    float).004311,(float).004876,(float)-.002323,(float)-.002425,(
	    float)0.,(float)-.004326,(float)6.405e-4,(float)-.005005,(float)
	    .001024,(float).02692,(float)-.008582,(float).01583,(float)
	    -.00251,(float).02035,(float).005977,(float)-.0115,(float)
	    1.296e-6,(float).001684,(float).02683,(float).009577,(float)
	    .02434,(float).02985,(float).01333,(float).02574,(float).0179 };

    /* System generated locals */
    integer i__1;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static integer kend;
    static real a[82];
    static integer i__, j, k;
    static real colat, az;
    static integer is;
    extern /* Subroutine */ int spharm_(real *, integer *, integer *, real *, 
	    real *);
    static real ste;

/* CALCULATES ELECTRON TEMPERATURES TE(1) TO TE(4) AT ALTITUDES */
/* 300, 400, 1400 AND 3000 KM FOR DIP-LATITUDE DIPL/DEG AND */
/* LOCAL SOLAR TIME SLT/H USING THE BRACE-THEIS-MODELS (J. ATMOS. */
/* TERR. PHYS. 43, 1317, 1981); NS IS SEASON IN NORTHERN */
/* HEMISOHERE: IS=1 SPRING, IS=2 SUMMER .... */
/* ALSO CALCULATED ARE THE TEMPERATURES AT 400 KM ALTITUDE FOR */
/* MIDNIGHT (TE(5)) AND NOON (TE(6)). */
    /* Parameter adjustments */
    --te;

    /* Function Body */
    if (*ns < 3) {
	is = *ns;
    } else if (*ns > 3) {
	is = 2;
	*dipl = -(*dipl);
    } else {
	is = 1;
    }
    colat = const_1.umr * ((float)90. - *dipl);
    az = const1_1.humr * *slt;
    spharm_(a, &c__8, &c__8, &colat, &az);
    if (is == 2) {
	kend = 3;
    } else {
	kend = 4;
    }
    i__1 = kend;
    for (k = 1; k <= i__1; ++k) {
	ste = (float)0.;
	for (i__ = 1; i__ <= 81; ++i__) {
/* L1: */
	    ste += a[i__ - 1] * c__[k + (is + (i__ << 1) << 2) - 13];
	}
/* L2: */
	d__1 = (doublereal) ste;
	te[k] = pow_dd(&c_b34, &d__1);
    }
    if (is == 2) {
	*dipl = -(*dipl);
	colat = const_1.umr * ((float)90. - *dipl);
	spharm_(a, &c__8, &c__8, &colat, &az);
	ste = (float)0.;
	for (i__ = 1; i__ <= 81; ++i__) {
/* L11: */
	    ste += a[i__ - 1] * c__[((i__ << 1) + 2 << 2) - 9];
	}
	d__1 = (doublereal) ste;
	te[4] = pow_dd(&c_b34, &d__1);
    }
/* ---------- TEMPERATURE AT 400KM AT MIDNIGHT AND NOON */
    for (j = 1; j <= 2; ++j) {
	ste = (float)0.;
	az = const1_1.humr * (j - 1) * (float)12.;
	spharm_(a, &c__8, &c__8, &colat, &az);
	for (i__ = 1; i__ <= 81; ++i__) {
/* L3: */
	    ste += a[i__ - 1] * c__[(is + (i__ << 1) << 2) - 11];
	}
/* L4: */
	d__1 = (doublereal) ste;
	te[j + 4] = pow_dd(&c_b34, &d__1);
    }
    return 0;
} /* teba_ */


/* Subroutine */ int spharm_(real *c__, integer *l, integer *m, real *colat, 
	real *az)
{
    /* System generated locals */
    integer i__1, i__2;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal), pow_ri(real *, integer *);

    /* Local variables */
    static integer i__, k, n;
    static real x, y;
    static integer mt;
    static real caz, saz;

/* CALCULATES THE COEFFICIENTS OF THE SPHERICAL HARMONIC */
/* EXPANSION THAT WAS USED FOR THE BRACE-THEIS-MODELS. */
    /* Parameter adjustments */
    --c__;

    /* Function Body */
    c__[1] = (float)1.;
    k = 2;
    x = cos(*colat);
    c__[k] = x;
    ++k;
    i__1 = *l;
    for (i__ = 2; i__ <= i__1; ++i__) {
	c__[k] = (((i__ << 1) - 1) * x * c__[k - 1] - (i__ - 1) * c__[k - 2]) 
		/ i__;
/* L10: */
	++k;
    }
    y = sin(*colat);
    i__1 = *m;
    for (mt = 1; mt <= i__1; ++mt) {
	caz = cos(mt * *az);
	saz = sin(mt * *az);
	c__[k] = pow_ri(&y, &mt);
	++k;
	if (mt == *l) {
	    goto L16;
	}
	c__[k] = c__[k - 1] * x * ((mt << 1) + 1);
	++k;
	if (mt + 1 == *l) {
	    goto L16;
	}
	i__2 = *l;
	for (i__ = mt + 2; i__ <= i__2; ++i__) {
	    c__[k] = (((i__ << 1) - 1) * x * c__[k - 1] - (i__ + mt - 1) * 
		    c__[k - 2]) / (i__ - mt);
/* L15: */
	    ++k;
	}
L16:
	n = *l - mt + 1;
	i__2 = n;
	for (i__ = 1; i__ <= i__2; ++i__) {
	    c__[k] = c__[k - n] * caz;
	    c__[k - n] *= saz;
/* L18: */
	    ++k;
	}
/* L20: */
    }
    return 0;
} /* spharm_ */



doublereal elte_(real *h__)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal eptr_(real *, real *, real *);
    static integer i__;
    static real aa, bb, sum;

/* ---------------------------------------------------------------- */
/* ELECTRON TEMPERATURE PROFILE BASED ON THE TEMPERATURES AT 120 */
/* HMAX,300,400,600,1400,3000 KM ALTITUDE. INBETWEEN CONSTANT */
/* GRADIENT IS ASSUMED. ARGMAX IS MAXIMUM ARGUMENT ALLOWED FOR */
/* EXP-FUNCTION. */
/* ---------------------------------------------------------------- */

    sum = blote_1.ate1 + blote_1.st[0] * (*h__ - blote_1.ah[0]);
    for (i__ = 1; i__ <= 5; ++i__) {
	aa = eptr_(h__, &blote_1.d__[i__ - 1], &blote_1.ah[i__]);
	bb = eptr_(blote_1.ah, &blote_1.d__[i__ - 1], &blote_1.ah[i__]);
/* L1: */
	sum += (blote_1.st[i__] - blote_1.st[i__ - 1]) * (aa - bb) * 
		blote_1.d__[i__ - 1];
    }
    ret_val = sum;
    return ret_val;
} /* elte_ */



doublereal tede_(real *h__, real *den, real *cov)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real acov, y, yc;

/* ELECTRON TEMEPERATURE MODEL AFTER BRACE,THEIS . */
/* FOR NEG. COV THE MEAN COV-INDEX (3 SOLAR ROT.) IS EXPECTED. */
/* DEN IS THE ELECTRON DENSITY IN M-3. */
    y = (*h__ * (float)17.01 - (float)2746.) * exp(*h__ * (float)-5.122e-4 + (
	    (float)6.094e-12 - *h__ * (float)3.353e-14) * *den) + (float)
	    1051.;
    acov = dabs(*cov);
    yc = (acov * (float).00202 + (float).117) / (exp(-(acov - (float)102.5) / 
	    (float)5.) + (float)1.) + (float)1.;
    if (*cov < (float)0.) {
	yc = (acov * (float).00169 + (float).123) / (exp(-(acov - (float)115.)
		 / (float)10.) + (float)1.) + (float)1.;
    }
    ret_val = y * yc;
    return ret_val;
} /* tede_ */



/* ************************************************************* */
/* **************** ION TEMPERATURE **************************** */
/* ************************************************************* */


doublereal ti_(real *h__)
{
    /* System generated locals */
    integer i__1;
    real ret_val;

    /* Local variables */
    extern doublereal eptr_(real *, real *, real *);
    static integer i__;
    static real aa, bb, sum;

/* ---------------------------------------------------------------- */
/* ION TEMPERATURE FOR HEIGHTS NOT GREATER 1000 KM AND NOT LESS HS */
/* EXPLANATION SEE FUNCTION RPID. */
/* ---------------------------------------------------------------- */
    sum = block8_1.mm[0] * (*h__ - block8_1.hs) + block8_1.tnhs;
    i__1 = block8_1.m - 1;
    for (i__ = 1; i__ <= i__1; ++i__) {
	aa = eptr_(h__, &block8_1.g[i__ - 1], &block8_1.xsm[i__ - 1]);
	bb = eptr_(&block8_1.hs, &block8_1.g[i__ - 1], &block8_1.xsm[i__ - 1])
		;
/* L100: */
	sum += (block8_1.mm[i__] - block8_1.mm[i__ - 1]) * (aa - bb) * 
		block8_1.g[i__ - 1];
    }
    ret_val = sum;
    return ret_val;
} /* ti_ */



doublereal teder_(real *h__)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    static real dtdx;
    extern doublereal dtndh_(real *, real *, real *, real *), tn_(real *, 
	    real *, real *, real *);
    static real tnh;

/* THIS FUNCTION ALONG WITH PROCEDURE REGFA1 ALLOWS TO FIND */
/* THE  HEIGHT ABOVE WHICH TN BEGINS TO BE DIFFERENT FROM TI */
    tnh = tn_(h__, &blotn_1.tex, &blotn_1.tlbd, &blotn_1.sig);
    dtdx = dtndh_(h__, &blotn_1.tex, &blotn_1.tlbd, &blotn_1.sig);
    ret_val = dtdx * (blotn_1.xsm1 - *h__) + tnh;
    return ret_val;
} /* teder_ */



doublereal tn_(real *h__, real *tinf, real *tlbd, real *s)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real zg2;

/* -------------------------------------------------------------------- */
/*       Calculate Temperature for MSIS/CIRA-86 model */
/* -------------------------------------------------------------------- */
    zg2 = (*h__ - (float)120.) * (float)6476.77 / (*h__ + (float)6356.77);
    ret_val = *tinf - *tlbd * exp(-(*s) * zg2);
    return ret_val;
} /* tn_ */



doublereal dtndh_(real *h__, real *tinf, real *tlbd, real *s)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real zg1, zg2, zg3;

/* --------------------------------------------------------------------- */
    zg1 = *h__ + (float)6356.77;
    zg2 = (float)6476.77 / zg1;
    zg3 = (*h__ - (float)120.) * zg2;
    ret_val = -(*tlbd) * exp(-(*s) * zg3) * (*s / zg1 * (zg3 - zg2));
    return ret_val;
} /* dtndh_ */



/* ************************************************************* */
/* ************* ION RELATIVE PRECENTAGE DENSITY ***************** */
/* ************************************************************* */


doublereal rpid_(real *h__, real *h0, real *n0, integer *m, real *st, integer 
	*id, real *xs)
{
    /* System generated locals */
    integer i__1;
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    extern doublereal eptr_(real *, real *, real *);
    static integer i__;
    static real aa, bb, sm, xi, sum;

/* ------------------------------------------------------------------ */
/* D.BILITZA,1977,THIS ANALYTIC FUNCTION IS USED TO REPRESENT THE */
/* RELATIVE PRECENTAGE DENSITY OF ATOMAR AND MOLECULAR OXYGEN IONS. */
/* THE M+1 HEIGHT GRADIENTS ST(M+1) ARE CONNECTED WITH EPSTEIN- */
/* STEP-FUNCTIONS AT THE STEP HEIGHTS XS(M) WITH TRANSITION */
/* THICKNESSES ID(M). RPID(H0,H0,N0,....)=N0. */
/* ARGMAX is the highest allowed argument for EXP in your system. */
/* ------------------------------------------------------------------ */
    /* Parameter adjustments */
    --xs;
    --id;
    --st;

    /* Function Body */
    sum = (*h__ - *h0) * st[1];
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	xi = (real) id[i__];
	aa = eptr_(h__, &xi, &xs[i__]);
	bb = eptr_(h0, &xi, &xs[i__]);
/* L100: */
	sum += (st[i__ + 1] - st[i__]) * (aa - bb) * xi;
    }
    if (dabs(sum) < argexp_1.argmax) {
	sm = exp(sum);
    } else if (sum > (float)0.) {
	sm = exp(argexp_1.argmax);
    } else {
	sm = (float)0.;
    }
    ret_val = *n0 * sm;
    return ret_val;
} /* rpid_ */



/* Subroutine */ int rdhhe_(real *h__, real *hb, real *rdoh, real *rdo2h, 
	real *rno, real *pehe, real *rdh, real *rdhe)
{
    static real rest;

/* BILITZA,FEB.82,H+ AND HE+ RELATIVE PERECENTAGE DENSITY BELOW */
/* 1000 KM. THE O+ AND O2+ REL. PER. DENSITIES SHOULD BE GIVEN */
/* (RDOH,RDO2H). HB IS THE ALTITUDE OF MAXIMAL O+ DENSITY. PEHE */
/* IS THE PRECENTAGE OF HE+ IONS COMPARED TO ALL LIGHT IONS. */
/* RNO IS THE RATIO OF NO+ TO O2+DENSITY AT H=HB. */
    *rdhe = (float)0.;
    *rdh = (float)0.;
    if (*h__ <= *hb) {
	goto L100;
    }
    rest = (float)100. - *rdoh - *rdo2h - *rno * *rdo2h;
    *rdh = rest * ((float)1. - *pehe / (float)100.);
    *rdhe = rest * *pehe / (float)100.;
L100:
    return 0;
} /* rdhhe_ */



doublereal rdno_(real *h__, real *hb, real *rdo2h, real *rdoh, real *rno)
{
    /* System generated locals */
    real ret_val;

/* D.BILITZA, 1978. NO+ RELATIVE PERCENTAGE DENSITY ABOVE 100KM. */
/* FOR MORE INFORMATION SEE SUBROUTINE RDHHE. */
    if (*h__ > *hb) {
	goto L200;
    }
    ret_val = (float)100. - *rdo2h - *rdoh;
    return ret_val;
L200:
    ret_val = *rno * *rdo2h;
    return ret_val;
} /* rdno_ */



/* Subroutine */ int koefp1_(real *pg1o)
{
    /* Initialized data */

    static real feld[80] = { (float)-11.,(float)-11.,(float)4.,(float)-11.,(
	    float).08018,(float).13027,(float).04216,(float).25,(float)
	    -.00686,(float).00999,(float)5.113,(float).1,(float)170.,(float)
	    180.,(float).1175,(float).15,(float)-11.,(float)1.,(float)2.,(
	    float)-11.,(float).069,(float).161,(float).254,(float).18,(float)
	    .0161,(float).0216,(float).03014,(float).1,(float)152.,(float)
	    167.,(float).04916,(float).17,(float)-11.,(float)2.,(float)2.,(
	    float)-11.,(float).072,(float).092,(float).014,(float).21,(float)
	    .01389,(float).03863,(float).05762,(float).12,(float)165.,(float)
	    168.,(float).008,(float).258,(float)-11.,(float)1.,(float)3.,(
	    float)-11.,(float).091,(float).088,(float).008,(float).34,(float)
	    .0067,(float).0195,(float).04,(float).1,(float)158.,(float)172.,(
	    float).01,(float).24,(float)-11.,(float)2.,(float)3.,(float)-11.,(
	    float).083,(float).102,(float).045,(float).03,(float).00127,(
	    float).01,(float).05,(float).09,(float)167.,(float)185.,(float)
	    .015,(float).18 };

    static integer i__, k;

/* THIEMANN,1979,COEFFICIENTS PG1O FOR CALCULATING  O+ PROFILES */
/* BELOW THE F2-MAXIMUM. CHOSEN TO APPROACH DANILOV- */
/* SEMENOV'S COMPILATION. */
    /* Parameter adjustments */
    --pg1o;

    /* Function Body */
    k = 0;
    for (i__ = 1; i__ <= 80; ++i__) {
	++k;
/* L10: */
	pg1o[k] = feld[i__ - 1];
    }
    return 0;
} /* koefp1_ */



/* Subroutine */ int koefp2_(real *pg2o)
{
    /* Initialized data */

    static real feld[32] = { (float)1.,(float)-11.,(float)-11.,(float)1.,(
	    float)695.,(float)-7.81e-4,(float)-.00264,(float)2177.,(float)1.,(
	    float)-11.,(float)-11.,(float)2.,(float)570.,(float)-.002,(float)
	    -.0052,(float)1040.,(float)2.,(float)-11.,(float)-11.,(float)1.,(
	    float)695.,(float)-7.86e-4,(float)-.00165,(float)3367.,(float)2.,(
	    float)-11.,(float)-11.,(float)2.,(float)575.,(float)-.00126,(
	    float)-.00524,(float)1380. };

    static integer i__, k;

/* THIEMANN,1979,COEFFICIENTS FOR CALCULATION OF O+ PROFILES */
/* ABOVE THE F2-MAXIMUM (DUMBS,SPENNER:AEROS-COMPILATION) */
    /* Parameter adjustments */
    --pg2o;

    /* Function Body */
    k = 0;
    for (i__ = 1; i__ <= 32; ++i__) {
	++k;
/* L10: */
	pg2o[k] = feld[i__ - 1];
    }
    return 0;
} /* koefp2_ */



/* Subroutine */ int koefp3_(real *pg3o)
{
    /* Initialized data */

    static real feld[80] = { (float)-11.,(float)1.,(float)2.,(float)-11.,(
	    float)160.,(float)31.,(float)130.,(float)-10.,(float)198.,(float)
	    0.,(float).05922,(float)-.07983,(float)-.00397,(float)8.5e-4,(
	    float)-.00313,(float)0.,(float)-11.,(float)2.,(float)2.,(float)
	    -11.,(float)140.,(float)30.,(float)130.,(float)-10.,(float)190.,(
	    float)0.,(float).05107,(float)-.07964,(float)9.7e-4,(float)
	    -.01118,(float)-.02614,(float)-.09537,(float)-11.,(float)1.,(
	    float)3.,(float)-11.,(float)140.,(float)37.,(float)125.,(float)0.,
	    (float)182.,(float)0.,(float).0307,(float)-.04968,(float)-.00248,(
	    float)-.02451,(float)-.00313,(float)0.,(float)-11.,(float)2.,(
	    float)3.,(float)-11.,(float)140.,(float)37.,(float)125.,(float)0.,
	    (float)170.,(float)0.,(float).02806,(float)-.04716,(float)6.6e-4,(
	    float)-.02763,(float)-.02247,(float)-.01919,(float)-11.,(float)
	    -11.,(float)4.,(float)-11.,(float)140.,(float)45.,(float)136.,(
	    float)-9.,(float)181.,(float)-26.,(float).02994,(float)-.04879,(
	    float)-.01396,(float)8.9e-4,(float)-.09929,(float).05589 };

    static integer i__, k;

/* THIEMANN,1979,COEFFICIENTS FOR CALCULATING O2+ PROFILES. */
/* CHOSEN AS TO APPROACH DANILOV-SEMENOV'S COMPILATION. */
    /* Parameter adjustments */
    --pg3o;

    /* Function Body */
    k = 0;
    for (i__ = 1; i__ <= 80; ++i__) {
	++k;
/* L10: */
	pg3o[k] = feld[i__ - 1];
    }
    return 0;
} /* koefp3_ */



/* Subroutine */ int sufe_(real *field, real *rfe, integer *m, real *fe)
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i__, k;
    static real efe[4];

/* SELECTS THE REQUIRED ION DENSITY PARAMETER SET. */
/* THE INPUT FIELD INCLUDES DIFFERENT SETS OF DIMENSION M EACH */
/* CARACTERISED BY 4 HEADER NUMBERS. RFE(4) SHOULD CONTAIN THE */
/* CHOSEN HEADER NUMBERS.FE(M) IS THE CORRESPONDING SET. */
    /* Parameter adjustments */
    --fe;
    --rfe;
    --field;

    /* Function Body */
    k = 0;
L100:
    for (i__ = 1; i__ <= 4; ++i__) {
	++k;
/* L101: */
	efe[i__ - 1] = field[k];
    }
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	++k;
/* L111: */
	fe[i__] = field[k];
    }
    for (i__ = 1; i__ <= 4; ++i__) {
	if (efe[i__ - 1] > (float)-10. && rfe[i__] != efe[i__ - 1]) {
	    goto L100;
	}
/* L120: */
    }
    return 0;
} /* sufe_ */



/* ************************************************************* */
/* ************* PEAK VALUES ELECTRON DENSITY ****************** */
/* ************************************************************* */


doublereal fout_(real *xmodip, real *xlati, real *xlongi, real *ut, real *ff0)
{
    /* Initialized data */

    static integer qf[9] = { 11,11,8,4,1,0,0,0,0 };

    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal gamma1_(real *, real *, real *, real *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, real *);

/* CALCULATES CRITICAL FREQUENCY FOF2/MHZ USING SUBROUTINE GAMMA1. */
/* XMODIP = MODIFIED DIP LATITUDE, XLATI = GEOG. LATITUDE, XLONGI= */
/* LONGITUDE (ALL IN DEG.), MONTH = MONTH, UT =  UNIVERSAL TIME */
/* (DEC. HOURS), FF0 = ARRAY WITH RZ12-ADJUSTED CCIR/URSI COEFF. */
/* D.BILITZA,JULY 85. */
    /* Parameter adjustments */
    --ff0;

    /* Function Body */
    ret_val = gamma1_(xmodip, xlati, xlongi, ut, &c__6, qf, &c__9, &c__76, &
	    c__13, &c__988, &ff0[1]);
    return ret_val;
} /* fout_ */



doublereal xmout_(real *xmodip, real *xlati, real *xlongi, real *ut, real *
	xm0)
{
    /* Initialized data */

    static integer qm[7] = { 6,7,5,2,1,0,0 };

    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal gamma1_(real *, real *, real *, real *, integer *, 
	    integer *, integer *, integer *, integer *, integer *, real *);

/* CALCULATES PROPAGATION FACTOR M3000 USING THE SUBROUTINE GAMMA1. */
/* XMODIP = MODIFIED DIP LATITUDE, XLATI = GEOG. LATITUDE, XLONGI= */
/* LONGITUDE (ALL IN DEG.), MONTH = MONTH, UT =  UNIVERSAL TIME */
/* (DEC. HOURS), XM0 = ARRAY WITH RZ12-ADJUSTED CCIR/URSI COEFF. */
/* D.BILITZA,JULY 85. */
    /* Parameter adjustments */
    --xm0;

    /* Function Body */
    ret_val = gamma1_(xmodip, xlati, xlongi, ut, &c__4, qm, &c__7, &c__49, &
	    c__9, &c__441, &xm0[1]);
    return ret_val;
} /* xmout_ */



doublereal hmf2ed_(real *xmagbr, real *r__, real *x, real *xm3)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real delm, f1, f2, f3;

/* CALCULATES THE PEAK HEIGHT HMF2/KM FOR THE MAGNETIC */
/* LATITUDE XMAGBR/DEG. AND THE SMOOTHED ZUERICH SUNSPOT */
/* NUMBER R USING CCIR-M3000 XM3 AND THE RATIO X=FOF2/FOE. */
/* [REF. D.BILITZA ET AL., TELECOMM.J., 46, 549-553, 1979] */
/* D.BILITZA,1980. */
    f1 = *r__ * (float).00232 + (float).222;
    f2 = (float)1.2 - exp(*r__ * (float).0239) * (float).0116;
    f3 = (*r__ - (float)25.) * (float).096 / (float)150.;
    delm = f1 * ((float)1. - *r__ / (float)150. * exp(-(*xmagbr) * *xmagbr / (
	    float)1600.)) / (*x - f2) + f3;
    ret_val = (float)1490. / (*xm3 + delm) - (float)176.;
    return ret_val;
} /* hmf2ed_ */



doublereal fof1ed_(real *ylati, real *r__, real *chi)
{
    /* System generated locals */
    real ret_val;
    doublereal d__1, d__2;

    /* Builtin functions */
    double cos(doublereal), pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static real chim, xmue, chi100, f0, fs, f100, dla, chi0, fof1;

/* -------------------------------------------------------------- */
/* CALCULATES THE F1 PEAK PLASMA FREQUENCY (FOF1/MHZ) */
/* FOR   DIP-LATITUDE (YLATI/DEGREE) */
/*       SMOOTHED ZURICH SUNSPOT NUMBER (R) */
/*       SOLAR ZENITH ANGLE (CHI/DEGREE) */
/* REFERENCE: */
/*       E.D.DUCHARME ET AL., RADIO SCIENCE 6, 369-378, 1971 */
/*                                      AND 8, 837-839, 1973 */
/*       HOWEVER WITH MAGNETIC DIP LATITUDE INSTEAD OF GEOMAGNETIC */
/*       DIPOLE LATITUDE, EYFRIG, 1979 */
/* --------------------------------------------- D. BILITZA, 1988. */
    fof1 = (float)0.;
    dla = *ylati;
    chi0 = dla * (float).349504 + (float)49.84733;
    chi100 = dla * (float).509932 + (float)38.96113;
    chim = chi0 + (chi100 - chi0) * *r__ / (float)100.;
    if (*chi > chim) {
	goto L1;
    }
    f0 = dla * ((float).0058 - dla * (float)1.2e-4) + (float)4.35;
    f100 = dla * ((float).011 - dla * (float)2.3e-4) + (float)5.348;
    fs = f0 + (f100 - f0) * *r__ / (float)100.;
    xmue = dla * ((float).0046 - dla * (float)5.4e-5) + (float).093 + *r__ * (
	    float)3e-4;
    d__1 = (doublereal) cos(*chi * const_1.umr);
    d__2 = (doublereal) xmue;
    fof1 = fs * pow_dd(&d__1, &d__2);
L1:
    ret_val = fof1;
    return ret_val;
} /* fof1ed_ */



doublereal foeedi_(real *cov, real *xhi, real *xhim, real *xlati)
{
    /* System generated locals */
    real ret_val;
    doublereal d__1, d__2;

    /* Builtin functions */
    double cos(doublereal), pow_dd(doublereal *, doublereal *), exp(
	    doublereal), log(doublereal);

    /* Local variables */
    static real xhic, smin, r4foe, a, b, c__, d__, sl, sm, sp;

/* ------------------------------------------------------- */
/* CALCULATES FOE/MHZ BY THE EDINBURGH-METHOD. */
/* INPUT: MEAN 10.7CM SOLAR RADIO FLUX (COV), GEOGRAPHIC */
/* LATITUDE (XLATI/DEG), SOLAR ZENITH ANGLE (XHI/DEG AND */
/* XHIM/DEG AT NOON). */
/* REFERENCE: */
/*       KOURIS-MUGGELETON, CCIR DOC. 6/3/07, 1973 */
/*       TROST, J. GEOPHYS. RES. 84, 2736, 1979 (was used */
/*               to improve the nighttime varition) */
/* D.BILITZA--------------------------------- AUGUST 1986. */
/* variation with solar activity (factor A) ............... */
    a = (*cov - (float)66.) * (float).0094 + (float)1.;
/* variation with noon solar zenith angle (B) and with latitude (C) */
    sl = cos(*xlati * const_1.umr);
    if (*xlati < (float)32.) {
	sm = sl * (float)1.92 - (float)1.93;
	c__ = sl * (float)116. + (float)23.;
    } else {
	sm = (float).11 - sl * (float).49;
	c__ = sl * (float)35. + (float)92.;
    }
    if (*xhim >= (float)90.) {
	*xhim = (float)89.999;
    }
    d__1 = (doublereal) cos(*xhim * const_1.umr);
    d__2 = (doublereal) sm;
    b = pow_dd(&d__1, &d__2);
/* variation with solar zenith angle (D) .......................... */
    if (*xlati > (float)12.) {
	sp = (float)1.2;
    } else {
	sp = (float)1.31;
    }
/* adjusted solar zenith angle during nighttime (XHIC) ............. */
    xhic = *xhi - log(exp((*xhi - (float)89.98) / (float)3.) + (float)1.) * (
	    float)3.;
    d__1 = (doublereal) cos(xhic * const_1.umr);
    d__2 = (doublereal) sp;
    d__ = pow_dd(&d__1, &d__2);
/* determine foE**4 ................................................ */
    r4foe = a * b * c__ * d__;
/* minimum allowable foE (sqrt[SMIN])............................... */
    smin = (*cov - (float)60.) * (float).0015 + (float).121;
    smin *= smin;
    if (r4foe < smin) {
	r4foe = smin;
    }
    d__1 = (doublereal) r4foe;
    ret_val = pow_dd(&d__1, &c_b91);
    return ret_val;
} /* foeedi_ */



doublereal xmded_(real *xhi, real *r__, real *yw)
{
    /* System generated locals */
    real ret_val;
    doublereal d__1;

    /* Builtin functions */
    double cos(doublereal), pow_dd(doublereal *, doublereal *), exp(
	    doublereal);

    /* Local variables */
    static real y, yy, ymd;

/* D. BILITZA, 1978, CALCULATES ELECTRON DENSITY OF D MAXIMUM. */
/* XHI/DEG. IS SOLAR ZENITH ANGLE, R SMOOTHED ZURICH SUNSPOT NUMBER */
/* AND YW/M-3 THE ASSUMED CONSTANT NIGHT VALUE. */
/* [REF.: D.BILITZA, WORLD DATA CENTER A REPORT UAG-82,7,BOULDER,1981] */
/* corrected 4/25/97 - D. Bilitza */


    if (*xhi >= (float)90.) {
	goto L100;
    }
    y = *r__ * (float)8.8e6 + (float)6.05e8;
    yy = cos(*xhi * const_1.umr);
    d__1 = (doublereal) yy;
    ymd = y * exp((float)-.1 / pow_dd(&d__1, &c_b94));
    if (ymd < *yw) {
	ymd = *yw;
    }
    ret_val = ymd;
    return ret_val;
L100:
    ret_val = *yw;
    return ret_val;
} /* xmded_ */



doublereal gamma1_(real *smodip, real *slat, real *slong, real *hour, integer 
	*iharm, integer *nq, integer *k1, integer *m, integer *mm, integer *
	m3, real *sfe)
{
    /* System generated locals */
    integer i__1, i__2;
    real ret_val;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal);

    /* Local variables */
    static doublereal coef[100], c__[12];
    static integer i__, j, l;
    static doublereal s[12];
    static integer index;
    static real s0, s1, s3, s2, xsinx[13];
    static integer mi, np;
    static real ss, hou;
    static doublereal sum;

/* CALCULATES GAMMA1=FOF2 OR M3000 USING CCIR NUMERICAL MAP */
/* COEFFICIENTS SFE(M3) FOR MODIFIED DIP LATITUDE (SMODIP/DEG) */
/* GEOGRAPHIC LATITUDE (SLAT/DEG) AND LONGITUDE (SLONG/DEG) */
/* AND UNIVERSIAL TIME (HOUR/DECIMAL HOURS). */
/* NQ(K1) IS AN INTEGER ARRAY GIVING THE HIGHEST DEGREES IN */
/* LATITUDE FOR EACH LONGITUDE HARMONIC. */
/* M=1+NQ1+2(NQ2+1)+2(NQ3+1)+... . */
/* SHEIKH,4.3.77. */
    /* Parameter adjustments */
    --nq;
    --sfe;

    /* Function Body */
    hou = (*hour * (float)15. - (float)180.) * const_1.umr;
    s[0] = sin(hou);
    c__[0] = cos(hou);
    i__1 = *iharm;
    for (i__ = 2; i__ <= i__1; ++i__) {
	c__[i__ - 1] = c__[0] * c__[i__ - 2] - s[0] * s[i__ - 2];
	s[i__ - 1] = c__[0] * s[i__ - 2] + s[0] * c__[i__ - 2];
/* L250: */
    }
    i__1 = *m;
    for (i__ = 1; i__ <= i__1; ++i__) {
	mi = (i__ - 1) * *mm;
	coef[i__ - 1] = sfe[mi + 1];
	i__2 = *iharm;
	for (j = 1; j <= i__2; ++j) {
	    coef[i__ - 1] = coef[i__ - 1] + sfe[mi + (j << 1)] * s[j - 1] + 
		    sfe[mi + (j << 1) + 1] * c__[j - 1];
/* L300: */
	}
    }
    sum = coef[0];
    ss = sin(*smodip * const_1.umr);
    s3 = ss;
    xsinx[0] = (float)1.;
    index = nq[1];
    i__2 = index;
    for (j = 1; j <= i__2; ++j) {
	sum += coef[j] * ss;
	xsinx[j] = ss;
	ss *= s3;
/* L350: */
    }
    xsinx[nq[1] + 1] = ss;
    np = nq[1] + 1;
    ss = cos(*slat * const_1.umr);
    s3 = ss;
    i__2 = *k1;
    for (j = 2; j <= i__2; ++j) {
	s0 = *slong * (j - (float)1.) * const_1.umr;
	s1 = cos(s0);
	s2 = sin(s0);
	index = nq[j] + 1;
	i__1 = index;
	for (l = 1; l <= i__1; ++l) {
	    ++np;
	    sum += coef[np - 1] * xsinx[l - 1] * ss * s1;
	    ++np;
	    sum += coef[np - 1] * xsinx[l - 1] * ss * s2;
/* L450: */
	}
	ss *= s3;
/* L400: */
    }
    ret_val = sum;
    return ret_val;
} /* gamma1_ */



/* ************************************************************ */
/* *************** EARTH MAGNETIC FIELD *********************** */
/* ************************************************************** */


/* Subroutine */ int ggm_(integer *art, real *long__, real *lati, real *mlong,
	 real *mlat)
{
    /* Builtin functions */
    double cos(doublereal), sin(doublereal), r_sign(real *, real *), asin(
	    doublereal), acos(doublereal);

    /* Local variables */
    static real ci, si, cbg, cbm, clg, clm, sbg, sbm, slg, slm, ylg, zpi;

/* CALCULATES GEOMAGNETIC LONGITUDE (MLONG) AND LATITUDE (MLAT) */
/* FROM GEOGRAFIC LONGITUDE (LONG) AND LATITUDE (LATI) FOR ART=0 */
/* AND REVERSE FOR ART=1. ALL ANGLES IN DEGREE. */
/* LATITUDE:-90 TO 90. LONGITUDE:0 TO 360 EAST. */
    zpi = const_2.faktor * (float)360.;
    cbg = const_2.faktor * (float)11.4;
    ci = cos(cbg);
    si = sin(cbg);
    if (*art == 0) {
	goto L10;
    }
    cbm = cos(*mlat * const_2.faktor);
    sbm = sin(*mlat * const_2.faktor);
    clm = cos(*mlong * const_2.faktor);
    slm = sin(*mlong * const_2.faktor);
    sbg = sbm * ci - cbm * clm * si;
    if (dabs(sbg) > (float)1.) {
	sbg = r_sign(&c_b18, &sbg);
    }
    *lati = asin(sbg);
    cbg = cos(*lati);
    slg = cbm * slm / cbg;
    clg = (sbm * si + cbm * clm * ci) / cbg;
    if (dabs(clg) > (float)1.) {
	clg = r_sign(&c_b18, &clg);
    }
    *long__ = acos(clg);
    if (slg < (float)0.) {
	*long__ = zpi - *long__;
    }
    *lati /= const_2.faktor;
    *long__ /= const_2.faktor;
    *long__ += (float)-69.8;
    if (*long__ < (float)0.) {
	*long__ += (float)360.;
    }
    return 0;
L10:
    ylg = *long__ + (float)69.8;
    cbg = cos(*lati * const_2.faktor);
    sbg = sin(*lati * const_2.faktor);
    clg = cos(ylg * const_2.faktor);
    slg = sin(ylg * const_2.faktor);
    sbm = sbg * ci + cbg * clg * si;
    if (dabs(sbm) > (float)1.) {
	sbm = r_sign(&c_b18, &sbm);
    }
    *mlat = asin(sbm);
    cbm = cos(*mlat);
    slm = cbg * slg / cbm;
    clm = (-sbg * si + cbg * clg * ci) / cbm;
    if (dabs(clm) > (float)1.) {
	clm = r_sign(&c_b18, &clm);
    }
    *mlong = acos(clm);
    if (slm < (float)0.) {
	*mlong = zpi - *mlong;
    }
    *mlat /= const_2.faktor;
    *mlong /= const_2.faktor;
    return 0;
} /* ggm_ */



/* Subroutine */ int fieldg_(real *dlat, real *dlong, real *alt, real *x, 
	real *y, real *z__, real *f, real *dip, real *dec, real *smodip)
{
    /* Initialized data */

    static real fel1[72] = { (float)0.,(float).1506723,(float).0101742,(float)
	    -.0286519,(float).0092606,(float)-.0130846,(float).0089594,(float)
	    -.0136808,(float)-1.508e-4,(float)-.0093977,(float).013065,(float)
	    .002052,(float)-.0121956,(float)-.0023451,(float)-.0208555,(float)
	    .0068416,(float)-.0142659,(float)-.0093322,(float)-.0021364,(
	    float)-.007891,(float).0045586,(float).0128904,(float)-2.951e-4,(
	    float)-.0237245,(float).0289493,(float).0074605,(float)-.0105741,(
	    float)-5.116e-4,(float)-.0105732,(float)-.0058542,(float).0033268,
	    (float).0078164,(float).0211234,(float).0099309,(float).0362792,(
	    float)-.020107,(float)-.004635,(float)-.0058722,(float).0011147,(
	    float)-.0013949,(float)-.0108838,(float).0322263,(float)-.014739,(
	    float).0031247,(float).0111986,(float)-.0109394,(float).0058112,(
	    float).2739046,(float)-.0155682,(float)-.0253272,(float).0163782,(
	    float).020573,(float).0022081,(float).0112749,(float)-.0098427,(
	    float).0072705,(float).0195189,(float)-.0081132,(float)-.0071889,(
	    float)-.057997,(float)-.0856642,(float).188426,(float)-.7391512,(
	    float).1210288,(float)-.0241888,(float)-.0052464,(float)-.0096312,
	    (float)-.0044834,(float).0201764,(float).0258343,(float).0083033,(
	    float).0077187 };
    static real fel2[72] = { (float).0586055,(float).0102236,(float)-.0396107,
	    (float)-.016786,(float)-.2019911,(float)-.5810815,(float).0379916,
	    (float)3.7508268,(float)1.813303,(float)-.056425,(float)-.0557352,
	    (float).1335347,(float)-.0142641,(float)-.1024618,(float).0970994,
	    (float)-.075183,(float)-.1274948,(float).0402073,(float).038629,(
	    float).1883088,(float).183896,(float)-.7848989,(float).7591817,(
	    float)-.9302389,(float)-.856096,(float).663325,(float)-4.6363869,(
	    float)-13.2599277,(float).1002136,(float).0855714,(float)
	    -.0991981,(float)-.0765378,(float)-.0455264,(float).1169326,(
	    float)-.2604067,(float).1800076,(float)-.2223685,(float)-.6347679,
	    (float).5334222,(float)-.3459502,(float)-.1573697,(float).8589464,
	    (float)1.781599,(float)-6.3347645,(float)-3.1513653,(float)
	    -9.992775,(float)13.3327637,(float)-35.4897308,(float)37.3466339,(
	    float)-.5257398,(float).0571474,(float)-.5421217,(float).240477,(
	    float)-.1747774,(float)-.3433644,(float).4829708,(float).3935944,(
	    float).4885033,(float).8488121,(float)-.7640999,(float)-1.8884945,
	    (float)3.2930784,(float)-7.3497229,(float).1672821,(float)
	    -.2306652,(float)10.5782146,(float)12.6031065,(float)8.6579742,(
	    float)215.5209961,(float)-27.141922,(float)22.3405762,(float)
	    1108.6394043 };

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), sqrt(doublereal), r_sign(real *, 
	    real *), asin(doublereal);

    /* Local variables */
    static integer imax;
    static real rlat;
    static integer nmax, last;
    static real d__, g[144], h__[144];
    static integer i__, k, m;
    static real s;
    static integer ihmax;
    static real rlong, zdivf, f1, ydivs, x1, y1, z1;
    static integer ih;
    static real cp;
    static integer il;
    static real ct, xi[3], sp, rq, st, xt, dipdiv, rho, xxx, yyy, brh0, zzz;

/* THIS IS A SPECIAL VERSION OF THE POGO 68/10 MAGNETIC FIELD */
/* LEGENDRE MODEL. TRANSFORMATION COEFF. G(144) VALID FOR 1973. */
/* INPUT: DLAT, DLONG=GEOGRAPHIC COORDINATES/DEG.(-90/90,0/360), */
/*        ALT=ALTITUDE/KM. */
/* OUTPUT: F TOTAL FIELD (GAUSS), Z DOWNWARD VERTICAL COMPONENT */
/*        X,Y COMPONENTS IN THE EQUATORIAL PLANE (X TO ZERO LONGITUDE). */
/*        DIP INCLINATION ANGLE(DEGREE). SMODIP RAWER'S MODFIED DIP. */
/* SHEIK,1977. */
    k = 0;
    for (i__ = 1; i__ <= 72; ++i__) {
	++k;
	g[k - 1] = fel1[i__ - 1];
/* L10: */
	g[k + 71] = fel2[i__ - 1];
    }
    rlat = *dlat * const_1.umr;
    ct = sin(rlat);
    st = cos(rlat);
    nmax = 11;
    d__ = sqrt((float)40680925. - ct * (float)272336. * ct);
    rlong = *dlong * const_1.umr;
    cp = cos(rlong);
    sp = sin(rlong);
    zzz = (*alt + (float)40408589. / d__) * ct / (float)6371.2;
    rho = (*alt + (float)40680925. / d__) * st / (float)6371.2;
    xxx = rho * cp;
    yyy = rho * sp;
    rq = (float)1. / (xxx * xxx + yyy * yyy + zzz * zzz);
    xi[0] = xxx * rq;
    xi[1] = yyy * rq;
    xi[2] = zzz * rq;
    ihmax = nmax * nmax + 1;
    last = ihmax + nmax + nmax;
    imax = nmax + nmax - 1;
    i__1 = last;
    for (i__ = ihmax; i__ <= i__1; ++i__) {
/* L100: */
	h__[i__ - 1] = g[i__ - 1];
    }
    for (k = 1; k <= 3; k += 2) {
	i__ = imax;
	ih = ihmax;
L300:
	il = ih - i__;
	f1 = (float)2. / (i__ - k + (float)2.);
	x1 = xi[0] * f1;
	y1 = xi[1] * f1;
	z1 = xi[2] * (f1 + f1);
	i__ += -2;
	if (i__ - 1 < 0) {
	    goto L400;
	}
	if (i__ - 1 == 0) {
	    goto L500;
	}
	i__1 = i__;
	for (m = 3; m <= i__1; m += 2) {
	    h__[il + m] = g[il + m] + z1 * h__[ih + m] + x1 * (h__[ih + m + 2]
		     - h__[ih + m - 2]) - y1 * (h__[ih + m + 1] + h__[ih + m 
		    - 3]);
	    h__[il + m - 1] = g[il + m - 1] + z1 * h__[ih + m - 1] + x1 * (
		    h__[ih + m + 1] - h__[ih + m - 3]) + y1 * (h__[ih + m + 2]
		     + h__[ih + m - 2]);
/* L600: */
	}
L500:
	h__[il + 1] = g[il + 1] + z1 * h__[ih + 1] + x1 * h__[ih + 3] - y1 * (
		h__[ih + 2] + h__[ih - 1]);
	h__[il] = g[il] + z1 * h__[ih] + y1 * h__[ih + 3] + x1 * (h__[ih + 2] 
		- h__[ih - 1]);
L400:
	h__[il - 1] = g[il - 1] + z1 * h__[ih - 1] + (x1 * h__[ih] + y1 * h__[
		ih + 1]) * (float)2.;
/* L700: */
	ih = il;
	if (i__ >= k) {
	    goto L300;
	}
/* L200: */
    }
    s = h__[0] * (float).5 + (h__[1] * xi[2] + h__[2] * xi[0] + h__[3] * xi[1]
	    ) * (float)2.;
    xt = (rq + rq) * sqrt(rq);
    *x = xt * (h__[2] - s * xxx);
    *y = xt * (h__[3] - s * yyy);
    *z__ = xt * (h__[1] - s * zzz);
    *f = sqrt(*x * *x + *y * *y + *z__ * *z__);
    brh0 = *y * sp + *x * cp;
    *y = *y * cp - *x * sp;
    *x = *z__ * st - brh0 * ct;
    *z__ = -(*z__) * ct - brh0 * st;
    zdivf = *z__ / *f;
    if (dabs(zdivf) > (float)1.) {
	zdivf = r_sign(&c_b18, &zdivf);
    }
    *dip = asin(zdivf);
    ydivs = *y / sqrt(*x * *x + *y * *y);
    if (dabs(ydivs) > (float)1.) {
	ydivs = r_sign(&c_b18, &ydivs);
    }
    *dec = asin(ydivs);
    dipdiv = *dip / sqrt(*dip * *dip + st);
    if (dabs(dipdiv) > (float)1.) {
	dipdiv = r_sign(&c_b18, &dipdiv);
    }
    *smodip = asin(dipdiv);
    *dip /= const_1.umr;
    *dec /= const_1.umr;
    *smodip /= const_1.umr;
    return 0;
} /* fieldg_ */



/* ************************************************************ */
/* *********** INTERPOLATION AND REST *************************** */
/* ************************************************************** */


/* Subroutine */ int regfa1_(real *x11, real *x22, real *fx11, real *fx22, 
	real *eps, real *fw, E_fp f, logical *schalt, real *x)
{
    /* System generated locals */
    real r__1;

    /* Local variables */
    static logical k, links;
    static real f1, f2;
    static logical l1;
    static real x1, x2, ep;
    static integer ng;
    static real dx, fx;
    static integer lfd;

/* REGULA-FALSI-PROCEDURE TO FIND X WITH F(X)-FW=0. X1,X2 ARE THE */
/* STARTING VALUES. THE COMUTATION ENDS WHEN THE X-INTERVAL */
/* HAS BECOME LESS THAN EPS . IF SIGN(F(X1)-FW)= SIGN(F(X2)-FW) */
/* THEN SCHALT=.TRUE. */
    *schalt = FALSE_;
    ep = *eps;
    x1 = *x11;
    x2 = *x22;
    f1 = *fx11 - *fw;
    f2 = *fx22 - *fw;
    k = FALSE_;
    ng = 2;
    lfd = 0;
    if (f1 * f2 <= (float)0.) {
	goto L200;
    }
    *x = (float)0.;
    *schalt = TRUE_;
    return 0;
L200:
    *x = (x1 * f2 - x2 * f1) / (f2 - f1);
    goto L400;
L300:
    l1 = links;
    dx = (x2 - x1) / ng;
    if (! links) {
	dx *= ng - 1;
    }
    *x = x1 + dx;
L400:
    fx = (*f)(x) - *fw;
    ++lfd;
    if (lfd > 20) {
	ep *= (float)10.;
	lfd = 0;
    }
    links = f1 * fx > (float)0.;
    k = ! k;
    if (links) {
	x1 = *x;
	f1 = fx;
    } else {
	x2 = *x;
	f2 = fx;
    }
    if ((r__1 = x2 - x1, dabs(r__1)) <= ep) {
	goto L800;
    }
    if (k) {
	goto L300;
    }
    if (links && ! l1 || ! links && l1) {
	ng <<= 1;
    }
    goto L200;
L800:
    return 0;
} /* regfa1_ */



/* Subroutine */ int tal_(real *shabr, real *sdelta, real *shbr, real *sdtdh0,
	 logical *aus6, real *spt)
{
    /* Builtin functions */
    double log(doublereal), sqrt(doublereal);

    /* Local variables */
    static real b, c__, z1, z2, z3, z4;

/* CALCULATES THE COEFFICIENTS SPT FOR THE POLYNOMIAL */
/* Y(X)=1+SPT(1)*X**2+SPT(2)*X**3+SPT(3)*X**4+SPT(4)*X**5 */
/* TO FIT THE VALLEY IN Y, REPRESENTED BY: */
/* Y(X=0)=1, THE X VALUE OF THE DEEPEST VALLEY POINT (SHABR), */
/* THE PRECENTAGE DEPTH (SDELTA), THE WIDTH (SHBR) AND THE */
/* DERIVATIVE DY/DX AT THE UPPER VALLEY BOUNDRY (SDTDH0). */
/* IF THERE IS AN UNWANTED ADDITIONAL EXTREMUM IN THE VALLEY */
/* REGION, THEN AUS6=.TRUE., ELSE AUS6=.FALSE.. */
/* FOR -SDELTA THE COEFF. ARE CALCULATED FOR THE FUNCTION */
/* Y(X)=EXP(SPT(1)*X**2+...+SPT(4)*X**5). */
    /* Parameter adjustments */
    --spt;

    /* Function Body */
    z1 = -(*sdelta) / (*shabr * (float)100. * *shabr);
    if (*sdelta > (float)0.) {
	goto L500;
    }
    *sdelta = -(*sdelta);
    z1 = log((float)1. - *sdelta / (float)100.) / (*shabr * *shabr);
L500:
    z3 = *sdtdh0 / (*shbr * (float)2.);
    z4 = *shabr - *shbr;
    spt[4] = (z1 * (*shbr - *shabr * (float)2.) * *shbr + z3 * z4 * *shabr) * 
	    (float)2. / (*shabr * *shbr * z4 * z4 * z4);
    spt[3] = z1 * (*shbr * (float)2. - *shabr * (float)3.) / (*shabr * z4 * 
	    z4) - (*shabr * (float)2. + *shbr) * spt[4];
    spt[2] = z1 * (float)-2. / *shabr - *shabr * (float)2. * spt[3] - *shabr *
	     (float)3. * *shabr * spt[4];
    spt[1] = z1 - *shabr * (spt[2] + *shabr * (spt[3] + *shabr * spt[4]));
    *aus6 = FALSE_;
    b = spt[3] * (float)4. / (spt[4] * (float)5.) + *shabr;
    c__ = spt[1] * (float)-2. / (spt[4] * 5 * *shabr);
    z2 = b * b / (float)4. - c__;
    if (z2 < (float)0.) {
	goto L300;
    }
    z3 = sqrt(z2);
    z1 = b / (float)2.;
    z2 = -z1 + z3;
    if (z2 > (float)0. && z2 < *shbr) {
	*aus6 = TRUE_;
    }
    if (dabs(z3) > (float)1e-15) {
	goto L400;
    }
    z2 = c__ / z2;
    if (z2 > (float)0. && z2 < *shbr) {
	*aus6 = TRUE_;
    }
    return 0;
L400:
    z2 = -z1 - z3;
    if (z2 > (float)0. && z2 < *shbr) {
	*aus6 = TRUE_;
    }
L300:
    return 0;
} /* tal_ */



/* ****************************************************************** */
/* ********** ZENITH ANGLE, DAY OF YEAR, TIME *********************** */
/* ****************************************************************** */


/* Subroutine */ int soco_(integer *ld, real *t, real *flat, real *elon, real 
	*declin, real *zenith, real *sunrse, real *sunset)
{
    /* Initialized data */

    static real p1 = (float).017203534;
    static real p2 = (float).034407068;
    static real p3 = (float).051610602;
    static real p4 = (float).068814136;
    static real p6 = (float).103221204;

    /* Builtin functions */
    double sin(doublereal), cos(doublereal), r_sign(real *, real *), acos(
	    doublereal);

    /* Local variables */
    static real cosx, wlon, a, b, dc, fa, ch, td, te, tf, et, secphi, cosphi, 
	    dcl, phi, eqt;

/* -------------------------------------------------------------------- */
/*       s/r to calculate the solar declination, zenith angle, and */
/*       sunrise & sunset times  - based on Newbern Smith's algorithm */
/*       [leo mcnamara, 1-sep-86, last modified 16-jun-87] */
/*       {dieter bilitza, 30-oct-89, modified for IRI application} */

/* in:   ld      local day of year */
/*       t       local hour (decimal) */
/*       flat    northern latitude in degrees */
/*       elon    east longitude in degrees */

/* out:  declin      declination of the sun in degrees */
/*       zenith      zenith angle of the sun in degrees */
/*       sunrse      local time of sunrise in hours */
/*       sunset      local time of sunset in hours */
/* ------------------------------------------------------------------- */

/* amplitudes of Fourier coefficients  --  1955 epoch................. */

/* s/r is formulated in terms of WEST longitude....................... */
    wlon = (float)360. - *elon;

/* time of equinox for 1980........................................... */
    td = *ld + (*t + wlon / (float)15.) / (float)24.;
    te = td + (float).9369;

/* declination of the sun.............................................. */
    dcl = sin(p1 * (te - (float)82.242)) * (float)23.256 + sin(p2 * (te - (
	    float)44.855)) * (float).381 + sin(p3 * (te - (float)23.355)) * (
	    float).167 - sin(p4 * (te + (float)11.97)) * (float).013 + sin(p6 
	    * (te - (float)10.41)) * (float).011 + (float).339137;
    *declin = dcl;
    dc = dcl * const_3.dtr;

/* the equation of time................................................ */
    tf = te - (float).5;
    eqt = sin(p1 * (tf - (float)4.)) * (float)-7.38 - sin(p2 * (tf + (float)
	    9.)) * (float)9.87 + sin(p3 * (tf - (float)53.)) * (float).27 - 
	    cos(p4 * (tf - (float)17.)) * (float).2;
    et = eqt * const_3.dtr / (float)4.;

    fa = *flat * const_3.dtr;
    phi = const1_1.humr * (*t - (float)12.) + et;

    a = sin(fa) * sin(dc);
    b = cos(fa) * cos(dc);
    cosx = a + b * cos(phi);
    if (dabs(cosx) > (float)1.) {
	cosx = r_sign(&c_b18, &cosx);
    }
    *zenith = acos(cosx) / const_3.dtr;

/* calculate sunrise and sunset times --  at the ground........... */
/* see Explanatory Supplement to the Ephemeris (1961) pg 401...... */
/* sunrise at height h metres is at............................... */
/*       chi(h) = 90.83 + 0.0347 * sqrt(h)........................ */
/* this includes corrections for horizontal refraction and........ */
/* semi-diameter of the solar disk................................ */
    ch = cos(const_3.dtr * (float)90.83);
    cosphi = (ch - a) / b;
/* if abs(secphi) > 1., sun does not rise/set..................... */
/* allow for sun never setting - high latitude summer............. */
    secphi = (float)999999.;
    if (cosphi != (float)0.) {
	secphi = (float)1. / cosphi;
    }
    *sunset = (float)99.;
    *sunrse = (float)99.;
    if (secphi > (float)-1. && secphi <= (float)0.) {
	return 0;
    }
/* allow for sun never rising - high latitude winter.............. */
    *sunset = (float)-99.;
    *sunrse = (float)-99.;
    if (secphi > (float)0. && secphi < (float)1.) {
	return 0;
    }

    if (cosphi > (float)1.) {
	cosphi = r_sign(&c_b18, &cosphi);
    }
    phi = acos(cosphi);
    et /= const1_1.humr;
    phi /= const1_1.humr;
    *sunrse = (float)12. - phi - et;
    *sunset = phi + (float)12. - et;
    if (*sunrse < (float)0.) {
	*sunrse += (float)24.;
    }
    if (*sunset >= (float)24.) {
	*sunset += (float)-24.;
    }

    return 0;
} /* soco_ */



doublereal hpol_(real *hour, real *tw, real *xnw, real *sa, real *su, real *
	dsa, real *dsu)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal epst_(real *, real *, real *);

/* ------------------------------------------------------- */
/* PROCEDURE FOR SMOOTH TIME-INTERPOLATION USING EPSTEIN */
/* STEP FUNCTION AT SUNRISE (SA) AND SUNSET (SU). THE */
/* STEP-WIDTH FOR SUNRISE IS DSA AND FOR SUNSET DSU. */
/* TW,NW ARE THE DAY AND NIGHT VALUE OF THE PARAMETER TO */
/* BE INTERPOLATED. SA AND SU ARE TIME OF SUNRIES AND */
/* SUNSET IN DECIMAL HOURS. */
/* BILITZA----------------------------------------- 1979. */
    if (dabs(*su) > (float)25.) {
	if (*su > (float)0.) {
	    ret_val = *tw;
	} else {
	    ret_val = *xnw;
	}
	return ret_val;
    }
    ret_val = *xnw + (*tw - *xnw) * epst_(hour, dsa, sa) + (*xnw - *tw) * 
	    epst_(hour, dsu, su);
    return ret_val;
} /* hpol_ */



/* Subroutine */ int moda_(integer *in, integer *iyear, integer *month, 
	integer *iday, integer *idoy, integer *nrdaymo)
{
    /* Initialized data */

    static integer mm[12] = { 31,28,31,30,31,30,31,31,30,31,30,31 };

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer mobe, i__, moold, mosum, imo;

/* ------------------------------------------------------------------- */
/* CALCULATES DAY OF YEAR (IDOY, ddd) FROM YEAR (IYEAR, yy or yyyy), */
/* MONTH (MONTH, mm) AND DAY OF MONTH (IDAY, dd) IF IN=0, OR MONTH */
/* AND DAY FROM YEAR AND DAY OF YEAR IF IN=1. NRDAYMO is an output */
/* parameter providing the number of days in the specific month. */
/* ------------------------------------------------------------------- */
    imo = 0;
    mobe = 0;

/*  leap year rule: years evenly divisible by 4 are leap years, except */
/*  years also evenly divisible by 100 are not leap years, except years also */
/*  evenly divisible by 400 are leap years. The year 2000 therefore is a */
/*  leap year. The 100 and 400 year exception rule */
/*       if((iyear/4*4.eq.iyear).and.(iyear/100*100.ne.iyear)) mm(2)=29 */
/*  will become important again in the year 2100 which is not a leap year. */

    mm[1] = 28;
    if (*iyear / 4 << 2 == *iyear) {
	mm[1] = 29;
    }
    if (*in > 0) {
	goto L5;
    }
    mosum = 0;
    if (*month > 1) {
	i__1 = *month - 1;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L1234: */
	    mosum += mm[i__ - 1];
	}
    }
    *idoy = mosum + *iday;
    *nrdaymo = mm[*month - 1];
    return 0;
L5:
    ++imo;
    if (imo > 12) {
	goto L55;
    }
    moold = mobe;
    *nrdaymo = mm[imo - 1];
    mobe += *nrdaymo;
    if (mobe < *idoy) {
	goto L5;
    }
L55:
    *month = imo;
    *iday = *idoy - moold;
    return 0;
} /* moda_ */



/* Subroutine */ int ut_lt__(integer *mode, real *ut, real *slt, real *glong, 
	integer *iyyy, integer *ddd)
{
    static real xlong;
    static integer dddend;

/* ----------------------------------------------------------------- */
/* Converts Universal Time UT (decimal hours) into Solar Local Time */
/* SLT (decimal hours) for given date (iyyy is year, e.g. 1995; ddd */
/* is day of year, e.g. 1 for Jan 1) and geodatic longitude in degrees. */
/* For mode=0 UT->LT and for mode=1 LT->UT */
/* Please NOTE that iyyy and ddd are input as well as output parameters */
/* since the determined LT may be for a day before or after the UT day. */
/* ------------------------------------------------- bilitza nov 95 */
    xlong = *glong;
    if (*glong > (float)180.) {
	xlong = *glong - 360;
    }
    if (*mode != 0) {
	goto L1;
    }

/* UT ---> LT */

    *slt = *ut + xlong / (float)15.;
    if (*slt >= (float)0. && *slt <= (float)24.) {
	goto L2;
    }
    if (*slt > (float)24.) {
	goto L3;
    }
    *slt += (float)24.;
    --(*ddd);
    if ((real) (*ddd) < (float)1.) {
	--(*iyyy);
	*ddd = 365;

/* leap year if evenly divisible by 4 and not by 100, except if evenly */
/* divisible by 400. Thus 2000 will be a leap year. */

	if (*iyyy / 4 << 2 == *iyyy) {
	    *ddd = 366;
	}
    }
    goto L2;
L3:
    *slt += (float)-24.;
    ++(*ddd);
    dddend = 365;
    if (*iyyy / 4 << 2 == *iyyy) {
	dddend = 366;
    }
    if (*ddd > dddend) {
	++(*iyyy);
	*ddd = 1;
    }
    goto L2;

/* LT ---> UT */

L1:
    *ut = *slt - xlong / (float)15.;
    if (*ut >= (float)0. && *ut <= (float)24.) {
	goto L2;
    }
    if (*ut > (float)24.) {
	goto L5;
    }
    *ut += (float)24.;
    --(*ddd);
    if ((real) (*ddd) < (float)1.) {
	--(*iyyy);
	*ddd = 365;
	if (*iyyy / 4 << 2 == *iyyy) {
	    *ddd = 366;
	}
    }
    goto L2;
L5:
    *ut += (float)-24.;
    ++(*ddd);
    dddend = 365;
    if (*iyyy / 4 << 2 == *iyyy) {
	dddend = 366;
    }
    if (*ddd > dddend) {
	++(*iyyy);
	*ddd = 1;
    }
L2:
    return 0;
} /* ut_lt__ */



doublereal b0_tab__(real *hour, real *sax, real *sux, integer *nseasn, real *
	r__, real *zmodip)
{
    /* Initialized data */

    static real b0f[32]	/* was [2][4][2][2] */ = { (float)114.,(float)64.,(
	    float)134.,(float)77.,(float)128.,(float)66.,(float)75.,(float)
	    73.,(float)113.,(float)115.,(float)150.,(float)116.,(float)138.,(
	    float)123.,(float)94.,(float)132.,(float)72.,(float)84.,(float)
	    83.,(float)89.,(float)75.,(float)85.,(float)57.,(float)76.,(float)
	    102.,(float)100.,(float)120.,(float)110.,(float)107.,(float)103.,(
	    float)76.,(float)86. };
    static real zx[5] = { (float)45.,(float)70.,(float)90.,(float)110.,(float)
	    135. };
    static real dd[5] = { (float)2.5,(float)2.,(float)2.,(float)2.,(float)2.5 
	    };

    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal hpol_(real *, real *, real *, real *, real *, real *, 
	    real *);
    static real dsum;
    extern doublereal eptr_(real *, real *, real *);
    static real g[6];
    static integer i__;
    static real aa, bb, dayval;
    static integer jseasn;
    static real zz, bb0, nitval, yb4, yb5, zz0, bfd[4]	/* was [2][2] */, bfr[
	    8]	/* was [2][2][2] */;
    static integer isd, isl, iss;
    static real sum;

/* ----------------------------------------------------------------- */
/* Interpolation procedure for bottomside thickness parameter B0. */
/* Array B0F(ILT,ISEASON,IR,ILATI) distinguishes between day and */
/* night (ILT=1,2), four seasons (ISEASON is northern season with */
/* ISEASON=1 northern spring), low and high solar activity Rz12=10, */
/* 100 (IR=1,2), and low and middle modified dip latitudes 18 and 45 */
/* degress (ILATI=1,2). In the DATA statement the first value */
/* corresponds to B0F(1,1,1,1), the second to B0F(2,1,1,1), the */
/* third to B0F(1,2,1,1) and so on. */
/* JUNE 1989 --------------------------------------- Dieter Bilitza */

/* corrected to include a smooth transition at the modip equator */
/* and no discontinuity at the equatorial change in season. */
/* JAN 1993 ---------------------------------------- Dieter Bilitza */

/* jseasn is southern hemisphere season */
    jseasn = *nseasn + 2;
    if (jseasn > 4) {
	jseasn += -4;
    }
    zz = *zmodip + (float)90.;
    zz0 = (float)0.;
/* Interpolation in Rz12: linear from 10 to 100 */
    for (isl = 1; isl <= 2; ++isl) {
	for (isd = 1; isd <= 2; ++isd) {
	    bfr[isd + ((isl << 1) + 1 << 1) - 7] = b0f[isd + (*nseasn + ((isl 
		    << 1) + 1 << 2) << 1) - 27] + (b0f[isd + (*nseasn + ((isl 
		    << 1) + 2 << 2) << 1) - 27] - b0f[isd + (*nseasn + ((isl 
		    << 1) + 1 << 2) << 1) - 27]) / (float)90. * (*r__ - (
		    float)10.);
	    bfr[isd + ((isl << 1) + 2 << 1) - 7] = b0f[isd + (jseasn + ((isl 
		    << 1) + 1 << 2) << 1) - 27] + (b0f[isd + (jseasn + ((isl 
		    << 1) + 2 << 2) << 1) - 27] - b0f[isd + (jseasn + ((isl <<
		     1) + 1 << 2) << 1) - 27]) / (float)90. * (*r__ - (float)
		    10.);
/* L7034: */
	}
/* Interpolation day/night with transitions at SAX (sunrise) and SUX (sunset) */
	for (iss = 1; iss <= 2; ++iss) {
	    dayval = bfr[(iss + (isl << 1) << 1) - 6];
	    nitval = bfr[(iss + (isl << 1) << 1) - 5];
	    bfd[iss + (isl << 1) - 3] = hpol_(hour, &dayval, &nitval, sax, 
		    sux, &c_b18, &c_b18);
/* L7033: */
	}
/* L7035: */
    }
/* Interpolation with epstein-transitions in modified dip latitude. */
/* Transitions at +/-18 and +/-45 degrees; constant above +/-45. */

/* g(1:5) are the latitudinal slopes; g(1) is for the region from -90 */
/* to -45 degrees, g(2) for -45/-20, g(3) for -20/0, g(4) for 0/20, */
/* g(5) for 20/45, and g(6) for 45/90. B0=bfd(2,2) at modip = -90, */
/* bfd(2,2) at modip = -45, bfd(2,1) at modip = -20, bfd(2,1)+delta at */
/* modip = -10 and 0, bfd(1,1) at modip = 20, bfd(1,2) at modip = 45 and 90. */
    g[0] = (float)0.;
    g[1] = (bfd[1] - bfd[3]) / (float)25.;
    g[4] = (bfd[2] - bfd[0]) / (float)25.;
    g[5] = (float)0.;
    if (bfd[1] > bfd[0]) {
	g[2] = g[1] / (float)4.;
	yb4 = bfd[1] + g[2] * (float)20.;
	g[3] = (bfd[0] - yb4) / (float)20.;
    } else {
	g[3] = g[4] / (float)4.;
	yb5 = bfd[0] - g[3] * (float)20.;
	g[2] = (yb5 - bfd[1]) / (float)20.;
    }
    bb0 = bfd[3];
    sum = bb0;
    for (i__ = 1; i__ <= 5; ++i__) {
	aa = eptr_(&zz, &dd[i__ - 1], &zx[i__ - 1]);
	bb = eptr_(&zz0, &dd[i__ - 1], &zx[i__ - 1]);
	dsum = (g[i__] - g[i__ - 1]) * (aa - bb) * dd[i__ - 1];
	sum += dsum;
/* L1: */
    }
    ret_val = sum;
    return ret_val;
} /* b0_tab__ */



doublereal b0_new__(real *hour, real *sax, real *sux, integer *nseasn, real *
	r__, real *zmodip)
{
    /* Initialized data */

    static real b0f[48]	/* was [2][4][2][3] */ = { (float)185.,(float)82.,(
	    float)185.,(float)82.,(float)185.,(float)82.,(float)185.,(float)
	    82.,(float)255.,(float)70.,(float)255.,(float)70.,(float)255.,(
	    float)70.,(float)255.,(float)70.,(float)114.,(float)64.,(float)
	    134.,(float)77.,(float)128.,(float)66.,(float)75.,(float)73.,(
	    float)113.,(float)115.,(float)150.,(float)116.,(float)138.,(float)
	    123.,(float)94.,(float)132.,(float)72.,(float)84.,(float)83.,(
	    float)89.,(float)75.,(float)85.,(float)57.,(float)76.,(float)102.,
	    (float)100.,(float)120.,(float)110.,(float)107.,(float)103.,(
	    float)76.,(float)86. };
    static real zx[5] = { (float)45.,(float)70.,(float)90.,(float)110.,(float)
	    135. };
    static real dd[5] = { (float)2.5,(float)2.,(float)2.,(float)2.,(float)2.5 
	    };

    /* System generated locals */
    integer i__1;
    real ret_val;

    /* Local variables */
    extern doublereal hpol_(real *, real *, real *, real *, real *, real *, 
	    real *);
    static real dsum;
    extern doublereal eptr_(real *, real *, real *);
    static real g[6];
    static integer i__;
    static real aa, bb, dayval;
    static integer jseasn;
    static real zz, bb0, nitval, zz0, bfd[6]	/* was [2][3] */, bfr[12]	
	    /* was [2][2][3] */;
    static integer isd, isl, iss;
    static real sum;
    static integer num_lat__;

/* ----------------------------------------------------------------- */
/* Interpolation procedure for bottomside thickness parameter B0. */
/* Array B0F(ILT,ISEASON,IR,ILATI) distinguishes between day and */
/* night (ILT=1,2), four seasons (ISEASON is northern season with */
/* ISEASON=1 northern spring), low and high solar activity Rz12=10, */
/* 100 (IR=1,2), and modified dip latitudes of 0, 18 and 45 */
/* degress (ILATI=1,2,3). In the DATA statement the first value */
/* corresponds to B0F(1,1,1,1), the second to B0F(2,1,1,1), the */
/* third to B0F(1,2,1,1) and so on. */
/* JUNE 1989 --------------------------------------- Dieter Bilitza */

/* corrected to include a smooth transition at the modip equator */
/* and no discontinuity at the equatorial change in season. */
/* JAN 1993 ---------------------------------------- Dieter Bilitza */

    num_lat__ = 3;
/* jseasn is southern hemisphere season */
    jseasn = *nseasn + 2;
    if (jseasn > 4) {
	jseasn += -4;
    }
    zz = *zmodip + (float)90.;
    zz0 = (float)0.;
/* Interpolation in Rz12: linear from 10 to 100 */
    i__1 = num_lat__;
    for (isl = 1; isl <= i__1; ++isl) {
	for (isd = 1; isd <= 2; ++isd) {
	    bfr[isd + ((isl << 1) + 1 << 1) - 7] = b0f[isd + (*nseasn + ((isl 
		    << 1) + 1 << 2) << 1) - 27] + (b0f[isd + (*nseasn + ((isl 
		    << 1) + 2 << 2) << 1) - 27] - b0f[isd + (*nseasn + ((isl 
		    << 1) + 1 << 2) << 1) - 27]) / (float)90. * (*r__ - (
		    float)10.);
	    bfr[isd + ((isl << 1) + 2 << 1) - 7] = b0f[isd + (jseasn + ((isl 
		    << 1) + 1 << 2) << 1) - 27] + (b0f[isd + (jseasn + ((isl 
		    << 1) + 2 << 2) << 1) - 27] - b0f[isd + (jseasn + ((isl <<
		     1) + 1 << 2) << 1) - 27]) / (float)90. * (*r__ - (float)
		    10.);
/* L7034: */
	}
/* Interpolation day/night with transitions at SAX (sunrise) and SUX (sunset) */
	for (iss = 1; iss <= 2; ++iss) {
	    dayval = bfr[(iss + (isl << 1) << 1) - 6];
	    nitval = bfr[(iss + (isl << 1) << 1) - 5];
	    bfd[iss + (isl << 1) - 3] = hpol_(hour, &dayval, &nitval, sax, 
		    sux, &c_b18, &c_b18);
/* L7033: */
	}
/* L7035: */
    }
/* Interpolation with epstein-transitions in modified dip latitude. */
/* Transitions at +/-18 and +/-45 degrees; constant above +/-45. */

/* g(1:5) are the latitudinal slopes; g(1) is for the region from -90 */
/* to -45 degrees, g(2) for -45/-20, g(3) for -20/0, g(4) for 0/20, */
/* g(5) for 20/45, and g(6) for 45/90. B0=bfd(2,2) at modip = -90, */
/* bfd(2,2) at modip = -45, bfd(2,1) at modip = -20, bfd(2,1)+delta at */
/* modip = -10 and 0, bfd(1,1) at modip = 20, bfd(1,2) at modip = 45 and 90. */
    g[0] = (float)0.;
    g[1] = (bfd[3] - bfd[5]) / (float)25.;
    g[2] = (bfd[1] - bfd[3]) / (float)20.;
    g[3] = (bfd[2] - bfd[0]) / (float)20.;
    g[4] = (bfd[4] - bfd[2]) / (float)25.;
    g[5] = (float)0.;
/*       if(bfd(2,1).gt.bfd(1,1)) then */
/*               g(3) = g(2) / 4. */
/*               yb4 = bfd(2,1) + 20. * g(3) */
/*               g(4) = ( bfd(1,1) - yb4 ) / 20. */
/*       else */
/*               g(4) = g(5) / 4. */
/*               yb5 = bfd(1,1) - 20. * g(4) */
/*               g(3) = ( yb5 - bfd(2,1) ) / 20. */
/*       endif */
    bb0 = bfd[5];
    sum = bb0;
    for (i__ = 1; i__ <= 5; ++i__) {
	aa = eptr_(&zz, &dd[i__ - 1], &zx[i__ - 1]);
	bb = eptr_(&zz0, &dd[i__ - 1], &zx[i__ - 1]);
	dsum = (g[i__] - g[i__ - 1]) * (aa - bb) * dd[i__ - 1];
	sum += dsum;
/* L1: */
    }
    ret_val = sum;
    return ret_val;
} /* b0_new__ */



doublereal b0pol_(real *hour, real *sax, real *sux, integer *iseason, real *
	r__, real *dela)
{
    /* Initialized data */

    static real b0f[32]	/* was [2][4][2][2] */ = { (float)114.,(float)64.,(
	    float)134.,(float)77.,(float)128.,(float)66.,(float)75.,(float)
	    73.,(float)113.,(float)115.,(float)150.,(float)116.,(float)138.,(
	    float)123.,(float)94.,(float)132.,(float)72.,(float)84.,(float)
	    83.,(float)89.,(float)75.,(float)85.,(float)57.,(float)76.,(float)
	    102.,(float)100.,(float)120.,(float)110.,(float)107.,(float)103.,(
	    float)76.,(float)86. };

    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal hpol_(real *, real *, real *, real *, real *, real *, 
	    real *);
    static real siph[2], sipl[2], dayval, nitval;
    static integer isl, isr;

/* ----------------------------------------------------------------- */
/* Interpolation procedure for bottomside thickness parameter B0. */
/* Array B0F(ILT,ISEASON,IR,ILATI) distinguishes between day and */
/* night (ILT=1,2), four seasons (ISEASON=1 spring), low and high */
/* solar activity (IR=1,2), and low and middle modified dip */
/* latitudes (ILATI=1,2). In the DATA statement the first value */
/* corresponds to B0F(1,1,1,1), the second to B0F(2,1,1,1), the */
/* third to B0F(1,2,1,1) and so on. */
/* JUNE 1989 --------------------------------------- Dieter Bilitza */

    for (isr = 1; isr <= 2; ++isr) {
	for (isl = 1; isl <= 2; ++isl) {
	    dayval = b0f[(*iseason + (isr + (isl << 1) << 2) << 1) - 26];
	    nitval = b0f[(*iseason + (isr + (isl << 1) << 2) << 1) - 25];
/* Interpolation day/night with transitions at SAX (sunrise) and SUX (sunset) */
/* L7034: */
	    siph[isl - 1] = hpol_(hour, &dayval, &nitval, sax, sux, &c_b18, &
		    c_b18);
	}
/* Interpolation low/middle modip with transition at 30 degrees modip */
/* L7033: */
	sipl[isr - 1] = siph[0] + (siph[1] - siph[0]) / *dela;
    }
/* Interpolation low/high Rz12: linear from 10 to 100 */
    ret_val = sipl[0] + (sipl[1] - sipl[0]) / (float)90. * (*r__ - (float)10.)
	    ;
    return ret_val;
} /* b0pol_ */



/* ********************************************************************* */
/* ************************ EPSTEIN FUNCTIONS ************************** */
/* ********************************************************************* */
/* REF:  H. G. BOOKER, J. ATMOS. TERR. PHYS. 39, 619-623, 1977 */
/*       K. RAWER, ADV. SPACE RES. 4, #1, 11-15, 1984 */
/* ********************************************************************* */


doublereal rlay_(real *x, real *xm, real *sc, real *hx)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal eptr_(real *, real *, real *), epst_(real *, real *, 
	    real *);
    static real y1, y1m, y2m;

/* -------------------------------------------------------- RAWER  LAYER */
    y1 = eptr_(x, sc, hx);
    y1m = eptr_(xm, sc, hx);
    y2m = epst_(xm, sc, hx);
    ret_val = y1 - y1m - (*x - *xm) * y2m / *sc;
    return ret_val;
} /* rlay_ */



doublereal d1lay_(real *x, real *xm, real *sc, real *hx)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal epst_(real *, real *, real *);

/* ------------------------------------------------------------ dLAY/dX */
    ret_val = (epst_(x, sc, hx) - epst_(xm, sc, hx)) / *sc;
    return ret_val;
} /* d1lay_ */



doublereal d2lay_(real *x, real *xm, real *sc, real *hx)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal epla_(real *, real *, real *);

/* ---------------------------------------------------------- d2LAY/dX2 */
    ret_val = epla_(x, sc, hx) / (*sc * *sc);
    return ret_val;
} /* d2lay_ */



doublereal eptr_(real *x, real *sc, real *hx)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal), log(doublereal);

    /* Local variables */
    static real d1;

/* ------------------------------------------------------------ TRANSITION */
    d1 = (*x - *hx) / *sc;
    if (dabs(d1) < argexp_1.argmax) {
	goto L1;
    }
    if (d1 > (float)0.) {
	ret_val = d1;
    } else {
	ret_val = (float)0.;
    }
    return ret_val;
L1:
    ret_val = log(exp(d1) + (float)1.);
    return ret_val;
} /* eptr_ */



doublereal epst_(real *x, real *sc, real *hx)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real d1;

/* -------------------------------------------------------------- STEP */
    d1 = (*x - *hx) / *sc;
    if (dabs(d1) < argexp_1.argmax) {
	goto L1;
    }
    if (d1 > (float)0.) {
	ret_val = (float)1.;
    } else {
	ret_val = (float)0.;
    }
    return ret_val;
L1:
    ret_val = (float)1. / (exp(-d1) + (float)1.);
    return ret_val;
} /* epst_ */



doublereal epstep_(real *y2, real *y1, real *sc, real *hx, real *x)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal epst_(real *, real *, real *);

/* ---------------------------------------------- STEP FROM Y1 TO Y2 */
    ret_val = *y1 + (*y2 - *y1) * epst_(x, sc, hx);
    return ret_val;
} /* epstep_ */



doublereal epla_(real *x, real *sc, real *hx)
{
    /* System generated locals */
    real ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static real d0, d1, d2;

/* ------------------------------------------------------------ PEAK */
    d1 = (*x - *hx) / *sc;
    if (dabs(d1) < argexp_1.argmax) {
	goto L1;
    }
    ret_val = (float)0.;
    return ret_val;
L1:
    d0 = exp(d1);
    d2 = d0 + (float)1.;
    ret_val = d0 / (d2 * d2);
    return ret_val;
} /* epla_ */



doublereal xe2to5_(real *h__, real *hmf2, integer *nl, real *hx, real *sc, 
	real *amp)
{
    /* System generated locals */
    integer i__1;
    real ret_val;
    doublereal d__1;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *);

    /* Local variables */
    extern doublereal rlay_(real *, real *, real *, real *);
    static real ylay, zlay;
    static integer i__;
    static real sum;

/* ---------------------------------------------------------------------- */
/* NORMALIZED ELECTRON DENSITY (N/NMF2) FOR THE MIDDLE IONOSPHERE FROM */
/* HME TO HMF2 USING LAY-FUNCTIONS. */
/* ---------------------------------------------------------------------- */
    /* Parameter adjustments */
    --amp;
    --sc;
    --hx;

    /* Function Body */
    sum = (float)1.;
    i__1 = *nl;
    for (i__ = 1; i__ <= i__1; ++i__) {
	ylay = amp[i__] * rlay_(h__, hmf2, &sc[i__], &hx[i__]);
	d__1 = (doublereal) ylay;
	zlay = pow_dd(&c_b34, &d__1);
/* L1: */
	sum *= zlay;
    }
    ret_val = sum;
    return ret_val;
} /* xe2to5_ */



doublereal xen_(real *h__, real *hmf2, real *xnmf2, real *hme, integer *nl, 
	real *hx, real *sc, real *amp)
{
    /* System generated locals */
    real ret_val;

    /* Local variables */
    extern doublereal xe2to5_(real *, real *, integer *, real *, real *, real 
	    *), xe1_(real *), xe6_(real *);

/* ---------------------------------------------------------------------- */
/* ELECTRON DENSITY WITH NEW MIDDLE IONOSPHERE */
/* ---------------------------------------------------------------------- */

    /* Parameter adjustments */
    --amp;
    --sc;
    --hx;

    /* Function Body */
    if (*h__ < *hmf2) {
	goto L100;
    }
    ret_val = xe1_(h__);
    return ret_val;
L100:
    if (*h__ < *hme) {
	goto L200;
    }
    ret_val = *xnmf2 * xe2to5_(h__, hmf2, nl, &hx[1], &sc[1], &amp[1]);
    return ret_val;
L200:
    ret_val = xe6_(h__);
    return ret_val;
} /* xen_ */



/* Subroutine */ int valgul_(real *xhi, real *hvb, real *vwu, real *vwa, real 
	*vdp)
{
    /* Builtin functions */
    double cos(doublereal), log(doublereal);

    /* Local variables */
    static real cs, abc, arl, zzz;

/* --------------------------------------------------------------------- */
/*   CALCULATES E-F VALLEY PARAMETERS; T.L. GULYAEVA, ADVANCES IN */
/*   SPACE RESEARCH 7, #6, 39-48, 1987. */

/*       INPUT:  XHI     SOLAR ZENITH ANGLE [DEGREE] */

/*       OUTPUT: VDP     VALLEY DEPTH  (NVB/NME) */
/*               VWU     VALLEY WIDTH  [KM] */
/*               VWA     VALLEY WIDTH  (SMALLER, CORRECTED BY RAWER) */
/*               HVB     HEIGHT OF VALLEY BASE [KM] */
/* ----------------------------------------------------------------------- */


    cs = cos(const_1.umr * *xhi) + (float).1;
    abc = dabs(cs);
    *vdp = cs * (float).45 / (abc + (float).1) + (float).55;
    arl = (abc + (float).1 + cs) / (abc + (float).1 - cs);
    zzz = log(arl);
    *vwu = (float)45. - zzz * (float)10.;
    *vwa = (float)45. - zzz * (float)5.;
    *hvb = (float)1e3 / (cs * (float).224 + (float)7.024 + abc * (float).966);
    return 0;
} /* valgul_ */



/* Subroutine */ int rogul_(integer *iday, real *xhi, real *sx, real *gro)
{
    /* Builtin functions */
    double cos(doublereal), exp(doublereal);

    /* Local variables */
    static real xs;

/* --------------------------------------------------------------------- */
/*   CALCULATES RATIO H0.5/HMF2 FOR HALF-DENSITY POINT (NE(H0.5)=0.5*NMF2) */
/*   T.L. GULYAEVA, ADVANCES IN SPACE RESEARCH 7, #6, 39-48, 1987. */

/*       INPUT:  IDAY    DAY OF YEAR */
/*               XHI     SOLAR ZENITH ANGLE [DEGREE] */

/*       OUTPUT: GRO     RATIO OF HALF DENSITY HEIGHT TO F PEAK HEIGHT */
/*               SX      SMOOTHLY VARYING SEASON PARAMTER (SX=1 FOR */
/*                       DAY=1; SX=3 FOR DAY=180; SX=2 FOR EQUINOX) */
/* ----------------------------------------------------------------------- */

    *sx = (float)2. - cos(*iday * const1_1.dumr);
    xs = (*xhi - *sx * (float)20.) / (float)15.;
    *gro = (float).8 - (float).2 / (exp(xs) + (float)1.);
/* same as gro=0.6+0.2/(1+exp(-xs)) */
    return 0;
} /* rogul_ */



/* Subroutine */ int lnglsn_(integer *n, real *a, real *b, logical *aus)
{
    /* System generated locals */
    integer i__1, i__2, i__3;
    real r__1;

    /* Local variables */
    static real amax;
    static integer imax, k, l, m, nn, izg;
    static real hsp, azv[10];

/* -------------------------------------------------------------------- */
/* SOLVES QUADRATIC SYSTEM OF LINEAR EQUATIONS: */

/*       INPUT:  N       NUMBER OF EQUATIONS (= NUMBER OF UNKNOWNS) */
/*               A(N,N)  MATRIX (LEFT SIDE OF SYSTEM OF EQUATIONS) */
/*               B(N)    VECTOR (RIGHT SIDE OF SYSTEM) */

/*       OUTPUT: AUS     =.TRUE.   NO SOLUTION FOUND */
/*                       =.FALSE.  SOLUTION IS IN  A(N,J) FOR J=1,N */
/* -------------------------------------------------------------------- */


    /* Parameter adjustments */
    --b;
    a -= 6;

    /* Function Body */
    nn = *n - 1;
    *aus = FALSE_;
    i__1 = *n - 1;
    for (k = 1; k <= i__1; ++k) {
	imax = k;
	l = k;
	izg = 0;
	amax = (r__1 = a[k + k * 5], dabs(r__1));
L110:
	++l;
	if (l > *n) {
	    goto L111;
	}
	hsp = (r__1 = a[l + k * 5], dabs(r__1));
	if (hsp < (float)1e-8) {
	    ++izg;
	}
	if (hsp <= amax) {
	    goto L110;
	}
L111:
	if (dabs(amax) >= (float)1e-10) {
	    goto L133;
	}
	*aus = TRUE_;
	return 0;
L133:
	if (imax == k) {
	    goto L112;
	}
	i__2 = *n;
	for (l = k; l <= i__2; ++l) {
	    azv[l] = a[imax + l * 5];
	    a[imax + l * 5] = a[k + l * 5];
/* L2: */
	    a[k + l * 5] = azv[l];
	}
	azv[0] = b[imax];
	b[imax] = b[k];
	b[k] = azv[0];
L112:
	if (izg == *n - k) {
	    goto L1;
	}
	amax = (float)1. / a[k + k * 5];
	azv[0] = b[k] * amax;
	i__2 = *n;
	for (m = k + 1; m <= i__2; ++m) {
/* L3: */
	    azv[m] = a[k + m * 5] * amax;
	}
	i__2 = *n;
	for (l = k + 1; l <= i__2; ++l) {
	    amax = a[l + k * 5];
	    if (dabs(amax) < (float)1e-8) {
		goto L4;
	    }
	    a[l + k * 5] = (float)0.;
	    b[l] -= azv[0] * amax;
	    i__3 = *n;
	    for (m = k + 1; m <= i__3; ++m) {
/* L5: */
		a[l + m * 5] -= amax * azv[m];
	    }
L4:
	    ;
	}
L1:
	;
    }
    for (k = *n; k >= 1; --k) {
	amax = (float)0.;
	if (k < *n) {
	    i__1 = *n;
	    for (l = k + 1; l <= i__1; ++l) {
/* L7: */
		amax += a[k + l * 5] * a[*n + l * 5];
	    }
	}
	if ((r__1 = a[k + k * 5], dabs(r__1)) < (float)1e-6) {
	    a[*n + k * 5] = (float)0.;
	} else {
	    a[*n + k * 5] = (b[k] - amax) / a[k + k * 5];
	}
/* L6: */
    }
    return 0;
} /* lnglsn_ */



/* Subroutine */ int lsknm_(integer *n, integer *m, integer *m0, integer *m1, 
	real *hm, real *sc, real *hx, real *w, real *x, real *y, real *var, 
	logical *sing)
{
    /* System generated locals */
    integer i__1, i__2, i__3;

    /* Local variables */
    extern doublereal rlay_(real *, real *, real *, real *), d1lay_(real *, 
	    real *, real *, real *), d2lay_(real *, real *, real *, real *);
    static integer i__, j, k, m01;
    extern /* Subroutine */ int lnglsn_(integer *, real *, real *, logical *);
    static real ali[25]	/* was [5][5] */, bli[5], scm, xli[50]	/* was [5][10]
	     */;

/* -------------------------------------------------------------------- */
/*   DETERMINES LAY-FUNCTIONS AMPLITUDES FOR A NUMBER OF CONSTRAINTS: */

/*       INPUT:  N       NUMBER OF AMPLITUDES ( LAY-FUNCTIONS) */
/*               M       NUMBER OF CONSTRAINTS */
/*               M0      NUMBER OF POINT CONSTRAINTS */
/*               M1      NUMBER OF FIRST DERIVATIVE CONSTRAINTS */
/*               HM      F PEAK ALTITUDE  [KM] */
/*               SC(N)   SCALE PARAMETERS FOR LAY-FUNCTIONS  [KM] */
/*               HX(N)   HEIGHT PARAMETERS FOR LAY-FUNCTIONS  [KM] */
/*               W(M)    WEIGHT OF CONSTRAINTS */
/*               X(M)    ALTITUDES FOR CONSTRAINTS  [KM] */
/*               Y(M)    LOG(DENSITY/NMF2) FOR CONSTRAINTS */

/*       OUTPUT: VAR(M)  AMPLITUDES */
/*               SING    =.TRUE.   NO SOLUTION */
/* ------------------------------------------------------------------------ */


    /* Parameter adjustments */
    --var;
    --hx;
    --sc;
    --y;
    --x;
    --w;

    /* Function Body */
    m01 = *m0 + *m1;
    scm = (float)0.;
    for (j = 1; j <= 5; ++j) {
	bli[j - 1] = (float)0.;
	for (i__ = 1; i__ <= 5; ++i__) {
/* L1: */
	    ali[j + i__ * 5 - 6] = (float)0.;
	}
    }
    i__1 = *n;
    for (i__ = 1; i__ <= i__1; ++i__) {
	i__2 = *m0;
	for (k = 1; k <= i__2; ++k) {
/* L3: */
	    xli[i__ + k * 5 - 6] = rlay_(&x[k], hm, &sc[i__], &hx[i__]);
	}
	i__2 = m01;
	for (k = *m0 + 1; k <= i__2; ++k) {
/* L4: */
	    xli[i__ + k * 5 - 6] = d1lay_(&x[k], hm, &sc[i__], &hx[i__]);
	}
	i__2 = *m;
	for (k = m01 + 1; k <= i__2; ++k) {
/* L5: */
	    xli[i__ + k * 5 - 6] = d2lay_(&x[k], hm, &sc[i__], &hx[i__]);
	}
/* L2: */
    }
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	i__2 = *m;
	for (k = 1; k <= i__2; ++k) {
	    bli[j - 1] += w[k] * y[k] * xli[j + k * 5 - 6];
	    i__3 = *n;
	    for (i__ = 1; i__ <= i__3; ++i__) {
/* L6: */
		ali[j + i__ * 5 - 6] += w[k] * xli[i__ + k * 5 - 6] * xli[j + 
			k * 5 - 6];
	    }
	}
/* L7: */
    }
    lnglsn_(n, ali, bli, sing);
    if (! (*sing)) {
	i__1 = *n;
	for (i__ = 1; i__ <= i__1; ++i__) {
/* L8: */
	    var[i__] = ali[*n + i__ * 5 - 6];
	}
    }
    return 0;
} /* lsknm_ */



/* Subroutine */ int inilay_(logical *night, real *xnmf2, real *xnmf1, real *
	xnme, real *vne, real *hmf2, real *hmf1, real *hme, real *hv1, real *
	hv2, real *hhalf, real *hxl, real *scl, real *amp, integer *iqual)
{
    /* Builtin functions */
    double r_lg10(real *);

    /* Local variables */
    static real hfff, xfff;
    extern doublereal epst_(real *, real *, real *);
    static logical ssin;
    static real alg102, hxl1t, alogf, xhalf;
    extern /* Subroutine */ int lsknm_(integer *, integer *, integer *, 
	    integer *, real *, real *, real *, real *, real *, real *, real *,
	     logical *);
    static real alogef, ww[8], xx[8], yy[8];
    static integer numcon, nc0, nc1, numlay;
    static real zet, scl0;

/* ------------------------------------------------------------------- */
/* CALCULATES AMPLITUDES FOR LAY FUNCTIONS */
/* D. BILITZA, DECEMBER 1988 */

/* INPUT:        NIGHT   LOGICAL VARIABLE FOR DAY/NIGHT DISTINCTION */
/*               XNMF2   F2 PEAK ELECTRON DENSITY [M-3] */
/*               XNMF1   F1 PEAK ELECTRON DENSITY [M-3] */
/*               XNME    E  PEAK ELECTRON DENSITY [M-3] */
/*               VNE     ELECTRON DENSITY AT VALLEY BASE [M-3] */
/*               HMF2    F2 PEAK ALTITUDE [KM] */
/*               HMF1    F1 PEAK ALTITUDE [KM] */
/*               HME     E  PEAK ALTITUDE [KM] */
/*               HV1     ALTITUDE OF VALLEY TOP [KM] */
/*               HV2     ALTITUDE OF VALLEY BASE [KM] */
/*               HHALF   ALTITUDE OF HALF-F2-PEAK-DENSITY [KM] */

/* OUTPUT:       HXL(4)  HEIGHT PARAMETERS FOR LAY FUNCTIONS [KM] */
/*               SCL(4)  SCALE PARAMETERS FOR LAY FUNCTIONS [KM] */
/*               AMP(4)  AMPLITUDES FOR LAY FUNCTIONS */
/*               IQUAL   =0 ok, =1 ok using second choice for HXL(1) */
/*                       =2 NO SOLUTION */
/* --------------------------------------------------------------- */

/* constants -------------------------------------------------------- */
    /* Parameter adjustments */
    --amp;
    --scl;
    --hxl;

    /* Function Body */
    numlay = 4;
    nc1 = 2;
    alg102 = r_lg10(&c_b200);

/* constraints: xx == height     yy == log(Ne/NmF2)    ww == weights */
/* ----------------------------------------------------------------- */
    alogf = r_lg10(xnmf2);
    alogef = r_lg10(xnme) - alogf;
    xhalf = *xnmf2 / (float)2.;
    xx[0] = *hhalf;
    xx[1] = *hv1;
    xx[2] = *hv2;
    xx[3] = *hme;
    xx[4] = *hme - (*hv2 - *hme);
    yy[0] = -alg102;
    yy[1] = alogef;
    yy[2] = r_lg10(vne) - alogf;
    yy[3] = alogef;
    yy[4] = yy[2];
    yy[6] = (float)0.;
    ww[1] = (float)1.;
    ww[2] = (float)2.;
    ww[3] = (float)5.;

/* geometric paramters for LAY ------------------------------------- */
/* difference to earlier version:  HXL(3) = HV2 + SCL(3) */

    scl0 = ((*hmf2 - *hhalf) * (float).216 + (float)56.8) * (float).7;
    scl[1] = scl0 * (float).8;
    scl[2] = (float)10.;
    scl[3] = (float)9.;
    scl[4] = (float)6.;
    hxl[3] = *hv2;

/* DAY CONDITION-------------------------------------------------- */
/* earlier tested:       HXL(2) = HMF1 + SCL(2) */

    if (*night) {
	goto L7711;
    }
    numcon = 8;
    hxl[1] = *hmf2 * (float).9;
    hxl1t = *hhalf;
    hxl[2] = *hmf1;
    hxl[4] = *hme - scl[4];
    xx[5] = *hmf1;
    xx[6] = *hv2;
    xx[7] = *hme;
    yy[7] = (float)0.;
    ww[4] = (float)1.;
    ww[6] = (float)50.;
    ww[7] = (float)500.;
/* without F-region ---------------------------------------------- */
    if (*xnmf1 > (float)0.) {
	goto L100;
    }
    hxl[2] = (*hmf2 + *hhalf) / (float)2.;
    yy[5] = (float)0.;
    ww[5] = (float)0.;
    ww[0] = (float)1.;
    goto L7722;
/* with F-region -------------------------------------------- */
L100:
    yy[5] = r_lg10(xnmf1) - alogf;
    ww[5] = (float)3.;
    if ((*xnmf1 - xhalf) * (*hmf1 - *hhalf) < (float)0.) {
	ww[0] = (float).5;
    } else {
	zet = yy[0] - yy[5];
	ww[0] = epst_(&zet, &c_b204, &c_b205);
    }
    if (*hhalf > *hmf1) {
	hfff = *hmf1;
	xfff = *xnmf1;
    } else {
	hfff = *hhalf;
	xfff = xhalf;
    }
    goto L7722;

/* NIGHT CONDITION--------------------------------------------------- */
/* different HXL,SCL values were tested including: */
/*       SCL(1) = HMF2 * 0.15 - 27.1     HXL(2) = 200. */
/*       HXL(2) = HMF1 + SCL(2)          HXL(3) = 140. */
/*       SCL(3) = 5.                     HXL(4) = HME + SCL(4) */
/*       HXL(4) = 105. */

L7711:
    numcon = 7;
    hxl[1] = *hhalf;
    hxl1t = *hmf2 * (float).4 + (float)30.;
    hxl[2] = (*hmf2 + *hv1) / (float)2.;
    hxl[4] = *hme;
    xx[5] = *hv2;
    xx[6] = *hme;
    yy[5] = (float)0.;
    ww[0] = (float)1.;
    ww[2] = (float)3.;
    ww[4] = (float).5;
    ww[5] = (float)50.;
    ww[6] = (float)500.;
    hfff = *hhalf;
    xfff = xhalf;

/* are valley-top and bottomside point compatible ? ------------- */

L7722:
    if ((*hv1 - hfff) * (*xnme - xfff) < (float)0.) {
	ww[1] = (float).5;
    }
    if (*hv1 <= *hv2 + (float)5.) {
	ww[1] = (float).5;
    }

/* DETERMINE AMPLITUDES----------------------------------------- */

    nc0 = numcon - nc1;
    *iqual = 0;
L2299:
    lsknm_(&numlay, &numcon, &nc0, &nc1, hmf2, &scl[1], &hxl[1], ww, xx, yy, &
	    amp[1], &ssin);
    if (*iqual > 0) {
	goto L1937;
    }
    if (dabs(amp[1]) > (float)10. || ssin) {
	*iqual = 1;
	hxl[1] = hxl1t;
	goto L2299;
    }
L1937:
    if (ssin) {
	*iqual = 2;
    }
    return 0;
} /* inilay_ */



/* Subroutine */ int ioncom_(real *h__, real *zd, real *fd, real *fs, real *t,
	 real *cn)
{
    /* Initialized data */

    static real po[30]	/* was [5][6] */ = { (float)0.,(float)0.,(float)0.,(
	    float)0.,(float)98.5,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)320.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    -2.59e-4,(float)2.79e-4,(float)-.00333,(float)-.00352,(float)
	    -.00516,(float)-.0247,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)-2.5e-6,(float).00104,(float)-1.79e-4,(float)-4.29e-5,(
	    float)1.01e-5,(float)-.00127 };
    static real ph[30]	/* was [5][6] */ = { (float)-4.97e-7,(float)-.121,(
	    float)-.131,(float)0.,(float)98.1,(float)355.,(float)-191.,(float)
	    -127.,(float)0.,(float)2040.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)-4.79e-6,(float)-2e-4,(float)5.67e-4,(float)2.6e-4,(
	    float)0.,(float)-.00508,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0. };
    static real pn[30]	/* was [5][6] */ = { (float).76,(float)-5.62,(float)
	    -4.99,(float)0.,(float)5.79,(float)83.,(float)-369.,(float)-324.,(
	    float)0.,(float)593.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)-6.3e-5,(float)-.00674,(float)-.00793,(float)-.00465,(float)
	    0.,(float)-.00326,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    -1.17e-5,(float).00488,(float)-.00131,(float)-7.03e-4,(float)0.,(
	    float)-.00238 };
    static real phe[30]	/* was [5][6] */ = { (float)-.895,(float)6.1,(float)
	    5.39,(float)0.,(float)8.01,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)1200.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    -1.04e-5,(float).0019,(float)9.53e-4,(float).00106,(float)0.,(
	    float)-.00344,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0. };
    static real pno[30]	/* was [5][6] */ = { (float)-22.4,(float)17.7,(float)
	    -13.4,(float)-4.88,(float)62.3,(float)32.7,(float)0.,(float)19.8,(
	    float)2.07,(float)115.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float).00394,(float)0.,(float).00248,(float)2.15e-4,(
	    float).00667,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)-.0084,(float)0.,(float)-.00364,(float).002,(float)-.0259 };
    static real po2[30]	/* was [5][6] */ = { (float)8.,(float)-12.2,(float)
	    9.9,(float)5.8,(float)53.4,(float)-25.2,(float)0.,(float)-28.5,(
	    float)-6.72,(float)120.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)-.014,(float)0.,(float)-.0093,(float).0033,(float)
	    .028,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    .00425,(float)0.,(float)-.00604,(float).00385,(float)-.0364 };
    static real pcl[30]	/* was [5][6] */ = { (float)0.,(float)0.,(float)0.,(
	    float)0.,(float)100.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)75.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)-.00904,(float)-.00728,(float)0.,(
	    float)0.,(float).00346,(float)-.0211 };

    /* Builtin functions */
    double cos(doublereal), exp(doublereal);

    /* Local variables */
    static real beth[7], betl[7], f;
    static integer i__, j;
    static real p[210]	/* was [5][6][7] */, s, z__, cm[7], hm[7], hx, alh[7],
	     all[7], arg, var[6];

/* --------------------------------------------------------------- */
/* ion composition model */
/* A.D. Danilov and A.P. Yaichnikov, A New Model of the Ion */
/*   Composition at 75 to 1000 km for IRI, Adv. Space Res. 5, #7, */
/*   75-79, 107-108, 1985 */

/*       h       altitude in km */
/*       zd      solar zenith angle in degrees */
/*       fd      latitude in degrees */
/*       fs      10.7cm solar radio flux */
/*       t       season (decimal month) */
/*       cn(1)   O+  relative density in percent */
/*       cn(2)   H+  relative density in percent */
/*       cn(3)   N+  relative density in percent */
/*       cn(4)   He+ relative density in percent */
/*       cn(5)   NO+ relative density in percent */
/*       cn(6)   O2+ relative density in percent */
/*       cn(7)   cluster ions  relative density in percent */
/* --------------------------------------------------------------- */

    /* Parameter adjustments */
    --cn;

    /* Function Body */
    z__ = *zd * const_1.umr;
    f = *fd * const_1.umr;
    for (i__ = 1; i__ <= 5; ++i__) {
	for (j = 1; j <= 6; ++j) {
	    p[i__ + (j + 6) * 5 - 36] = po[i__ + j * 5 - 6];
	    p[i__ + (j + 12) * 5 - 36] = ph[i__ + j * 5 - 6];
	    p[i__ + (j + 18) * 5 - 36] = pn[i__ + j * 5 - 6];
	    p[i__ + (j + 24) * 5 - 36] = phe[i__ + j * 5 - 6];
	    p[i__ + (j + 30) * 5 - 36] = pno[i__ + j * 5 - 6];
	    p[i__ + (j + 36) * 5 - 36] = po2[i__ + j * 5 - 6];
	    p[i__ + (j + 42) * 5 - 36] = pcl[i__ + j * 5 - 6];
/* L8: */
	}
    }
    s = (float)0.;
    for (i__ = 1; i__ <= 7; ++i__) {
	for (j = 1; j <= 6; ++j) {
	    var[j - 1] = p[(j + i__ * 6) * 5 - 35] * cos(z__) + p[(j + i__ * 
		    6) * 5 - 34] * cos(f) + p[(j + i__ * 6) * 5 - 33] * cos(((
		    float)300. - *fs) * (float).013) + p[(j + i__ * 6) * 5 - 
		    32] * cos((*t - (float)6.) * (float).52) + p[(j + i__ * 6)
		     * 5 - 31];
/* L7: */
	}
	cm[i__ - 1] = var[0];
	hm[i__ - 1] = var[1];
	all[i__ - 1] = var[2];
	betl[i__ - 1] = var[3];
	alh[i__ - 1] = var[4];
	beth[i__ - 1] = var[5];
	hx = *h__ - hm[i__ - 1];
	if (hx < (float)0.) {
	    goto L1;
	} else if (hx == 0) {
	    goto L2;
	} else {
	    goto L3;
	}
L1:
	arg = hx * (hx * all[i__ - 1] + betl[i__ - 1]);
	cn[i__] = (float)0.;
	if (arg > -argexp_1.argmax) {
	    cn[i__] = cm[i__ - 1] * exp(arg);
	}
	goto L4;
L2:
	cn[i__] = cm[i__ - 1];
	goto L4;
L3:
	arg = hx * (hx * alh[i__ - 1] + beth[i__ - 1]);
	cn[i__] = (float)0.;
	if (arg > -argexp_1.argmax) {
	    cn[i__] = cm[i__ - 1] * exp(arg);
	}
L4:
	if (cn[i__] < cm[i__ - 1] * (float).005) {
	    cn[i__] = (float)0.;
	}
	if (cn[i__] > cm[i__ - 1]) {
	    cn[i__] = cm[i__ - 1];
	}
	s += cn[i__];
/* L5: */
    }
    for (i__ = 1; i__ <= 7; ++i__) {
/* L6: */
	cn[i__] = cn[i__] / s * (float)100.;
    }
    return 0;
} /* ioncom_ */



/* Subroutine */ int ionco2_(real *hei, real *xhi, integer *it, real *f, real 
	*r1, real *r2, real *r3, real *r4)
{
    /* Initialized data */

    static integer j1ms70[7] = { 11,11,10,10,11,9,11 };
    static integer j2ms70[7] = { 13,11,10,11,11,9,11 };
    static real h1s70[91]	/* was [13][7] */ = { (float)75.,(float)85.,(
	    float)90.,(float)95.,(float)100.,(float)120.,(float)130.,(float)
	    200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)0.,(
	    float)75.,(float)85.,(float)90.,(float)95.,(float)100.,(float)
	    120.,(float)130.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)0.,(float)0.,(float)75.,(float)85.,(float)90.,(float)95.,(
	    float)100.,(float)115.,(float)200.,(float)220.,(float)250.,(float)
	    270.,(float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)
	    95.,(float)100.,(float)120.,(float)140.,(float)200.,(float)220.,(
	    float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)75.,(
	    float)80.,(float)95.,(float)100.,(float)120.,(float)150.,(float)
	    170.,(float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(
	    float)0.,(float)75.,(float)80.,(float)95.,(float)100.,(float)140.,
	    (float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(float)
	    95.,(float)100.,(float)110.,(float)145.,(float)200.,(float)220.,(
	    float)250.,(float)270.,(float)0.,(float)0. };
    static real h2s70[91]	/* was [13][7] */ = { (float)75.,(float)80.,(
	    float)90.,(float)95.,(float)100.,(float)120.,(float)130.,(float)
	    140.,(float)150.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)75.,(float)80.,(float)90.,(float)95.,(float)100.,(float)
	    120.,(float)130.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)0.,(float)0.,(float)75.,(float)80.,(float)90.,(float)95.,(
	    float)100.,(float)115.,(float)200.,(float)220.,(float)250.,(float)
	    270.,(float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)
	    95.,(float)100.,(float)120.,(float)140.,(float)150.,(float)200.,(
	    float)220.,(float)250.,(float)270.,(float)0.,(float)0.,(float)75.,
	    (float)80.,(float)95.,(float)100.,(float)120.,(float)150.,(float)
	    170.,(float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(
	    float)0.,(float)75.,(float)80.,(float)95.,(float)100.,(float)140.,
	    (float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)90.,(float)
	    95.,(float)100.,(float)110.,(float)145.,(float)200.,(float)220.,(
	    float)250.,(float)270.,(float)0.,(float)0. };
    static real r1ms70[91]	/* was [13][7] */ = { (float)6.,(float)30.,(
	    float)60.,(float)63.,(float)59.,(float)59.,(float)66.,(float)52.,(
	    float)20.,(float)4.,(float)2.,(float)0.,(float)0.,(float)6.,(
	    float)30.,(float)60.,(float)63.,(float)69.,(float)62.,(float)66.,(
	    float)52.,(float)20.,(float)4.,(float)2.,(float)0.,(float)0.,(
	    float)6.,(float)30.,(float)60.,(float)63.,(float)80.,(float)68.,(
	    float)53.,(float)20.,(float)4.,(float)2.,(float)0.,(float)0.,(
	    float)0.,(float)4.,(float)10.,(float)60.,(float)85.,(float)65.,(
	    float)65.,(float)52.,(float)25.,(float)12.,(float)4.,(float)0.,(
	    float)0.,(float)0.,(float)4.,(float)10.,(float)60.,(float)89.,(
	    float)72.,(float)60.,(float)60.,(float)52.,(float)30.,(float)20.,(
	    float)10.,(float)0.,(float)0.,(float)4.,(float)10.,(float)60.,(
	    float)92.,(float)68.,(float)54.,(float)40.,(float)25.,(float)13.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)1.,(float)8.,(float)
	    20.,(float)60.,(float)95.,(float)93.,(float)69.,(float)65.,(float)
	    45.,(float)30.,(float)20.,(float)0.,(float)0. };
    static real r2ms70[91]	/* was [13][7] */ = { (float)4.,(float)10.,(
	    float)30.,(float)32.,(float)41.,(float)41.,(float)32.,(float)29.,(
	    float)34.,(float)28.,(float)15.,(float)3.,(float)1.,(float)4.,(
	    float)10.,(float)30.,(float)32.,(float)31.,(float)38.,(float)32.,(
	    float)28.,(float)15.,(float)3.,(float)1.,(float)0.,(float)0.,(
	    float)4.,(float)10.,(float)30.,(float)32.,(float)20.,(float)32.,(
	    float)28.,(float)15.,(float)3.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)2.,(float)6.,(float)30.,(float)15.,(float)35.,(
	    float)30.,(float)34.,(float)26.,(float)19.,(float)8.,(float)3.,(
	    float)0.,(float)0.,(float)2.,(float)6.,(float)30.,(float)11.,(
	    float)28.,(float)38.,(float)29.,(float)29.,(float)25.,(float)12.,(
	    float)5.,(float)0.,(float)0.,(float)2.,(float)6.,(float)30.,(
	    float)8.,(float)32.,(float)30.,(float)20.,(float)14.,(float)8.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)1.,(float)2.,(float)
	    10.,(float)20.,(float)5.,(float)7.,(float)31.,(float)23.,(float)
	    18.,(float)15.,(float)10.,(float)0.,(float)0. };
    static real rk1ms70[91]	/* was [13][7] */ = { (float)2.4,(float)6.,(
	    float).6,(float)-.8,(float)0.,(float).7,(float)-.2,(float)-1.6,(
	    float)-.533,(float)-.1,(float)-.067,(float)0.,(float)0.,(float)
	    2.4,(float)6.,(float).6,(float)1.2,(float)-.35,(float).4,(float)
	    -.2,(float)-1.6,(float)-.533,(float)-.1,(float)-.067,(float)0.,(
	    float)0.,(float)2.4,(float)6.,(float).6,(float)3.4,(float)-.8,(
	    float)-.176,(float)-1.65,(float)-.533,(float)-.1,(float)-.067,(
	    float)0.,(float)0.,(float)0.,(float)1.2,(float)3.333,(float)5.,(
	    float)-1.,(float)0.,(float)-.216,(float)-1.35,(float)-.433,(float)
	    -.4,(float)-.1,(float)0.,(float)0.,(float)0.,(float)1.2,(float)
	    3.333,(float)5.8,(float)-.85,(float)-.4,(float)0.,(float)-.267,(
	    float)-1.1,(float)-.333,(float)-.4,(float)-.2,(float)0.,(float)0.,
	    (float)1.2,(float)3.333,(float)6.4,(float)-.6,(float)-.233,(float)
	    -.7,(float)-.5,(float)-.6,(float)-.267,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)1.4,(float)2.4,(float)4.,(float)7.,(float)-.2,
	    (float)-.686,(float)-.072,(float)-1.,(float)-.5,(float)-.5,(float)
	    -.5,(float)0.,(float)0. };
    static real rk2ms70[91]	/* was [13][7] */ = { (float)1.2,(float)2.,(
	    float).4,(float)1.8,(float)0.,(float)-.9,(float)-.3,(float).5,(
	    float)-.12,(float)-.65,(float)-.4,(float)-.1,(float)-.033,(float)
	    1.2,(float)2.,(float).4,(float)-.2,(float).35,(float)-.6,(float)
	    -.057,(float)-.65,(float)-.4,(float)-.1,(float)-.033,(float)0.,(
	    float)0.,(float)1.2,(float)2.,(float).4,(float)-2.4,(float).8,(
	    float)-.047,(float)-.65,(float)-.4,(float)-.1,(float)-.033,(float)
	    0.,(float)0.,(float)0.,(float).8,(float)1.6,(float)-3.,(float)1.,(
	    float)-.25,(float).4,(float)-.16,(float)-.35,(float)-.367,(float)
	    -.25,(float)-.1,(float)0.,(float)0.,(float).8,(float)1.6,(float)
	    -3.8,(float).85,(float).333,(float)-.45,(float)0.,(float)-.2,(
	    float)-.433,(float)-.35,(float)-.1,(float)0.,(float)0.,(float).8,(
	    float)1.6,(float)-4.4,(float).6,(float)-.033,(float)-.5,(float)
	    -.2,(float)-.3,(float)-.2,(float)0.,(float)0.,(float)0.,(float)0.,
	    (float).2,(float).8,(float)2.,(float)-3.,(float).2,(float).686,(
	    float)-.145,(float)-.25,(float)-.1,(float)-.25,(float)-.2,(float)
	    0.,(float)0. };
    static integer j1ms140[7] = { 11,11,10,10,9,9,12 };
    static integer j2ms140[7] = { 11,11,10,9,10,10,12 };
    static real h1s140[91]	/* was [13][7] */ = { (float)75.,(float)85.,(
	    float)90.,(float)95.,(float)100.,(float)120.,(float)130.,(float)
	    140.,(float)200.,(float)220.,(float)250.,(float)0.,(float)0.,(
	    float)75.,(float)85.,(float)90.,(float)95.,(float)100.,(float)
	    120.,(float)130.,(float)140.,(float)200.,(float)220.,(float)250.,(
	    float)0.,(float)0.,(float)75.,(float)85.,(float)90.,(float)95.,(
	    float)100.,(float)120.,(float)140.,(float)200.,(float)220.,(float)
	    250.,(float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)
	    95.,(float)100.,(float)120.,(float)140.,(float)200.,(float)220.,(
	    float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)75.,(
	    float)80.,(float)95.,(float)100.,(float)120.,(float)200.,(float)
	    220.,(float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)75.,(float)80.,(float)95.,(float)100.,(float)130.,(
	    float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(float)
	    95.,(float)100.,(float)110.,(float)140.,(float)180.,(float)200.,(
	    float)220.,(float)250.,(float)270.,(float)0. };
    static real h2s140[91]	/* was [13][7] */ = { (float)75.,(float)80.,(
	    float)90.,(float)95.,(float)100.,(float)120.,(float)130.,(float)
	    155.,(float)200.,(float)220.,(float)250.,(float)0.,(float)0.,(
	    float)75.,(float)80.,(float)90.,(float)95.,(float)100.,(float)
	    120.,(float)130.,(float)160.,(float)200.,(float)220.,(float)250.,(
	    float)0.,(float)0.,(float)75.,(float)80.,(float)90.,(float)95.,(
	    float)100.,(float)120.,(float)165.,(float)200.,(float)220.,(float)
	    250.,(float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)
	    95.,(float)100.,(float)120.,(float)180.,(float)200.,(float)250.,(
	    float)270.,(float)0.,(float)0.,(float)0.,(float)0.,(float)75.,(
	    float)80.,(float)95.,(float)100.,(float)120.,(float)160.,(float)
	    200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)0.,(
	    float)0.,(float)75.,(float)80.,(float)95.,(float)100.,(float)130.,
	    (float)160.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)90.,(
	    float)95.,(float)100.,(float)110.,(float)140.,(float)180.,(float)
	    200.,(float)220.,(float)250.,(float)270.,(float)0. };
    static real r1ms140[91]	/* was [13][7] */ = { (float)6.,(float)30.,(
	    float)60.,(float)63.,(float)59.,(float)59.,(float)66.,(float)66.,(
	    float)38.,(float)14.,(float)1.,(float)0.,(float)0.,(float)6.,(
	    float)30.,(float)60.,(float)63.,(float)69.,(float)62.,(float)66.,(
	    float)66.,(float)38.,(float)14.,(float)1.,(float)0.,(float)0.,(
	    float)6.,(float)30.,(float)60.,(float)63.,(float)80.,(float)65.,(
	    float)65.,(float)38.,(float)14.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)4.,(float)10.,(float)60.,(float)85.,(float)66.,(
	    float)66.,(float)38.,(float)22.,(float)9.,(float)1.,(float)0.,(
	    float)0.,(float)0.,(float)4.,(float)10.,(float)60.,(float)89.,(
	    float)71.,(float)42.,(float)26.,(float)17.,(float)10.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)4.,(float)10.,(float)60.,(
	    float)93.,(float)71.,(float)48.,(float)35.,(float)22.,(float)10.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)1.,(float)8.,(float)
	    20.,(float)60.,(float)95.,(float)93.,(float)72.,(float)60.,(float)
	    58.,(float)40.,(float)26.,(float)13.,(float)0. };
    static real r2ms140[91]	/* was [13][7] */ = { (float)4.,(float)10.,(
	    float)30.,(float)32.,(float)41.,(float)41.,(float)30.,(float)30.,(
	    float)10.,(float)6.,(float)1.,(float)0.,(float)0.,(float)4.,(
	    float)10.,(float)30.,(float)32.,(float)31.,(float)38.,(float)31.,(
	    float)29.,(float)9.,(float)6.,(float)1.,(float)0.,(float)0.,(
	    float)4.,(float)10.,(float)30.,(float)32.,(float)20.,(float)35.,(
	    float)26.,(float)9.,(float)6.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)2.,(float)6.,(float)30.,(float)15.,(float)34.,(
	    float)24.,(float)10.,(float)5.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)2.,(float)6.,(float)30.,(float)11.,(
	    float)28.,(float)37.,(float)21.,(float)14.,(float)8.,(float)5.,(
	    float)0.,(float)0.,(float)0.,(float)2.,(float)6.,(float)30.,(
	    float)7.,(float)29.,(float)36.,(float)29.,(float)20.,(float)13.,(
	    float)5.,(float)0.,(float)0.,(float)0.,(float)1.,(float)2.,(float)
	    10.,(float)20.,(float)5.,(float)7.,(float)28.,(float)32.,(float)
	    28.,(float)20.,(float)14.,(float)7.,(float)0. };
    static real rk1ms140[91]	/* was [13][7] */ = { (float)2.4,(float)6.,(
	    float).6,(float)-.8,(float)0.,(float).7,(float)0.,(float)-.467,(
	    float)-1.2,(float)-.433,(float)0.,(float)0.,(float)0.,(float)2.4,(
	    float)6.,(float).6,(float)1.2,(float)-.35,(float).4,(float)0.,(
	    float)-.467,(float)-1.2,(float)-.433,(float)0.,(float)0.,(float)
	    0.,(float)2.4,(float)6.,(float).6,(float)3.4,(float)-.75,(float)
	    0.,(float)-.45,(float)-1.2,(float)-.433,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)1.2,(float)3.333,(float)5.,(float)-.95,(
	    float)0.,(float)-.467,(float)-.8,(float)-.433,(float)-.4,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)1.2,(float)3.333,(float)
	    5.8,(float)-.9,(float)-.363,(float)-.8,(float)-.3,(float)-.35,(
	    float)-.3,(float)0.,(float)0.,(float)0.,(float)0.,(float)1.2,(
	    float)3.333,(float)6.6,(float)-.733,(float)-.329,(float)-.65,(
	    float)-.433,(float)-.6,(float)-.267,(float)0.,(float)0.,(float)0.,
	    (float)0.,(float)1.4,(float)2.4,(float)4.,(float)7.,(float)-.2,(
	    float)-.7,(float)-.3,(float)-.1,(float)-.9,(float)-.467,(float)
	    -.65,(float)-.333,(float)0. };
    static real rk2ms140[91]	/* was [13][7] */ = { (float)1.2,(float)2.,(
	    float).4,(float)1.8,(float)0.,(float)-1.1,(float)0.,(float)-.444,(
	    float)-.2,(float)-.166,(float)0.,(float)0.,(float)0.,(float)1.2,(
	    float)2.,(float).4,(float)-.2,(float).35,(float)-.7,(float)-.067,(
	    float)-.5,(float)-.15,(float)-.166,(float)0.,(float)0.,(float)0.,(
	    float)1.2,(float)2.,(float).4,(float)-2.4,(float).75,(float)-.2,(
	    float)-.486,(float)-.15,(float)-.166,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float).8,(float)1.6,(float)-3.,(float).95,(float)
	    -.167,(float)-.7,(float)-.1,(float)-.2,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float).8,(float)1.6,(float)-3.8,(float)
	    .85,(float).225,(float)-.4,(float)-.35,(float)-.2,(float)-.15,(
	    float)-.133,(float)0.,(float)0.,(float)0.,(float).8,(float)1.6,(
	    float)-4.6,(float).733,(float).233,(float)-.175,(float)-.45,(
	    float)-.233,(float)-.4,(float)-.1,(float)0.,(float)0.,(float)0.,(
	    float).2,(float).8,(float)2.,(float)-3.,(float).2,(float).7,(
	    float).1,(float)-.2,(float)-.4,(float)-.2,(float)-.35,(float)
	    -.167,(float)0. };
    static integer j1mr70[7] = { 12,12,12,9,10,11,13 };
    static integer j2mr70[7] = { 9,9,10,13,12,11,11 };
    static real h1r70[91]	/* was [13][7] */ = { (float)75.,(float)80.,(
	    float)90.,(float)95.,(float)100.,(float)120.,(float)140.,(float)
	    180.,(float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(
	    float)75.,(float)80.,(float)90.,(float)95.,(float)100.,(float)
	    120.,(float)145.,(float)180.,(float)200.,(float)220.,(float)250.,(
	    float)270.,(float)0.,(float)75.,(float)80.,(float)90.,(float)95.,(
	    float)100.,(float)120.,(float)145.,(float)180.,(float)200.,(float)
	    220.,(float)250.,(float)270.,(float)0.,(float)75.,(float)95.,(
	    float)100.,(float)110.,(float)140.,(float)180.,(float)200.,(float)
	    250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    75.,(float)95.,(float)125.,(float)150.,(float)185.,(float)195.,(
	    float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)
	    0.,(float)0.,(float)75.,(float)95.,(float)100.,(float)150.,(float)
	    160.,(float)170.,(float)190.,(float)200.,(float)220.,(float)250.,(
	    float)270.,(float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(
	    float)95.,(float)100.,(float)140.,(float)160.,(float)170.,(float)
	    190.,(float)200.,(float)220.,(float)250.,(float)270. };
    static real h2r70[91]	/* was [13][7] */ = { (float)75.,(float)95.,(
	    float)100.,(float)120.,(float)180.,(float)200.,(float)220.,(float)
	    250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    75.,(float)95.,(float)100.,(float)120.,(float)180.,(float)200.,(
	    float)220.,(float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)75.,(float)95.,(float)100.,(float)120.,(float)
	    130.,(float)190.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(
	    float)95.,(float)100.,(float)110.,(float)130.,(float)180.,(float)
	    190.,(float)200.,(float)220.,(float)250.,(float)270.,(float)75.,(
	    float)80.,(float)85.,(float)95.,(float)100.,(float)125.,(float)
	    150.,(float)190.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)0.,(float)75.,(float)80.,(float)85.,(float)95.,(float)100.,(
	    float)150.,(float)190.,(float)200.,(float)220.,(float)250.,(float)
	    270.,(float)0.,(float)0.,(float)75.,(float)85.,(float)95.,(float)
	    100.,(float)140.,(float)180.,(float)190.,(float)200.,(float)220.,(
	    float)250.,(float)270.,(float)0.,(float)0. };
    static real r1mr70[91]	/* was [13][7] */ = { (float)13.,(float)17.,(
	    float)57.,(float)57.,(float)30.,(float)53.,(float)58.,(float)38.,(
	    float)33.,(float)14.,(float)6.,(float)2.,(float)0.,(float)13.,(
	    float)17.,(float)57.,(float)57.,(float)37.,(float)56.,(float)56.,(
	    float)38.,(float)33.,(float)14.,(float)6.,(float)2.,(float)0.,(
	    float)13.,(float)17.,(float)57.,(float)57.,(float)47.,(float)58.,(
	    float)55.,(float)37.,(float)33.,(float)14.,(float)6.,(float)2.,(
	    float)0.,(float)5.,(float)65.,(float)54.,(float)58.,(float)58.,(
	    float)38.,(float)33.,(float)9.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)5.,(float)65.,(float)65.,(float)54.,(
	    float)40.,(float)40.,(float)45.,(float)26.,(float)17.,(float)10.,(
	    float)0.,(float)0.,(float)0.,(float)5.,(float)65.,(float)76.,(
	    float)56.,(float)57.,(float)48.,(float)44.,(float)51.,(float)35.,(
	    float)22.,(float)10.,(float)0.,(float)0.,(float)3.,(float)11.,(
	    float)35.,(float)75.,(float)90.,(float)65.,(float)63.,(float)54.,(
	    float)54.,(float)50.,(float)40.,(float)26.,(float)13. };
    static real r2mr70[91]	/* was [13][7] */ = { (float)7.,(float)43.,(
	    float)70.,(float)47.,(float)15.,(float)17.,(float)10.,(float)4.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)7.,(float)
	    43.,(float)63.,(float)44.,(float)17.,(float)17.,(float)10.,(float)
	    4.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)7.,(
	    float)43.,(float)53.,(float)42.,(float)42.,(float)13.,(float)17.,(
	    float)10.,(float)4.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)3.,(float)5.,(float)26.,(float)34.,(float)46.,(float)42.,(
	    float)41.,(float)23.,(float)16.,(float)16.,(float)10.,(float)1.,(
	    float)0.,(float)3.,(float)5.,(float)26.,(float)34.,(float)35.,(
	    float)35.,(float)42.,(float)25.,(float)22.,(float)14.,(float)8.,(
	    float)5.,(float)0.,(float)3.,(float)5.,(float)26.,(float)34.,(
	    float)24.,(float)41.,(float)31.,(float)26.,(float)20.,(float)13.,(
	    float)5.,(float)0.,(float)0.,(float)3.,(float)15.,(float)15.,(
	    float)10.,(float)35.,(float)35.,(float)30.,(float)34.,(float)20.,(
	    float)14.,(float)7.,(float)0.,(float)0. };
    static real rk1mr70[91]	/* was [13][7] */ = { (float).8,(float)4.,(
	    float)0.,(float)-5.4,(float)1.15,(float).25,(float)-.5,(float)
	    -.25,(float)-.95,(float)-.267,(float)-.2,(float)-.067,(float)0.,(
	    float).8,(float)4.,(float)0.,(float)-4.,(float).95,(float)0.,(
	    float)-.514,(float)-.25,(float)-.95,(float)-.267,(float)-.2,(
	    float)-.067,(float)0.,(float).8,(float)4.,(float)0.,(float)-2.,(
	    float).55,(float)-.12,(float)-.514,(float)-.2,(float)-.95,(float)
	    -.267,(float)-.2,(float)-.067,(float)0.,(float)3.,(float)-2.2,(
	    float).4,(float)0.,(float)-.5,(float)-.25,(float)-.48,(float)-.4,(
	    float)-.033,(float)0.,(float)0.,(float)0.,(float)0.,(float)3.,(
	    float)0.,(float)-.44,(float)-.466,(float)0.,(float)1.,(float)-.95,
	    (float)-.3,(float)-.35,(float)-.3,(float)0.,(float)0.,(float)0.,(
	    float)3.,(float)2.2,(float)-.4,(float).1,(float)-.9,(float)-.2,(
	    float).7,(float)-.8,(float)-.433,(float)-.6,(float)-.267,(float)
	    0.,(float)0.,(float)1.6,(float)4.8,(float)4.,(float)3.,(float)
	    -.625,(float)-.1,(float)-.9,(float)0.,(float)-.4,(float)-.5,(
	    float)-.467,(float)-.65,(float)-.3 };
    static real rk2mr70[91]	/* was [13][7] */ = { (float)1.8,(float)5.4,(
	    float)-1.15,(float)-.533,(float).1,(float)-.35,(float)-.2,(float)
	    -.2,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)1.8,(
	    float)4.,(float)-.95,(float)-.45,(float)0.,(float)-.35,(float)-.2,
	    (float)-.2,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)1.8,(float)2.,(float)-.55,(float)0.,(float)-.483,(float).4,(
	    float)-.35,(float)-.2,(float)-.2,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float).4,(float)4.2,(float).8,(float)2.4,(float)-.4,(
	    float)-.05,(float)-.36,(float)-.7,(float)0.,(float)-.3,(float)-.3,
	    (float)-.05,(float)0.,(float).4,(float)4.2,(float).8,(float).2,(
	    float)0.,(float).28,(float)-.425,(float)-.3,(float)-.4,(float)-.2,
	    (float)-.15,(float)-.133,(float)0.,(float).4,(float)4.2,(float).8,
	    (float)-2.,(float).34,(float)-.25,(float)-.5,(float)-.3,(float)
	    -.233,(float)-.4,(float)-.1,(float)0.,(float)0.,(float)1.2,(float)
	    0.,(float)-1.,(float).625,(float)0.,(float)-.5,(float).4,(float)
	    -.7,(float)-.2,(float)-.35,(float)-.167,(float)0.,(float)0. };
    static integer j1mr140[7] = { 12,12,11,12,9,9,13 };
    static integer j2mr140[7] = { 10,9,10,12,13,13,12 };
    static real h1r140[91]	/* was [13][7] */ = { (float)75.,(float)80.,(
	    float)90.,(float)95.,(float)100.,(float)115.,(float)130.,(float)
	    145.,(float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(
	    float)75.,(float)80.,(float)90.,(float)95.,(float)100.,(float)
	    110.,(float)120.,(float)145.,(float)200.,(float)220.,(float)250.,(
	    float)270.,(float)0.,(float)75.,(float)80.,(float)90.,(float)95.,(
	    float)100.,(float)115.,(float)150.,(float)200.,(float)220.,(float)
	    250.,(float)270.,(float)0.,(float)0.,(float)75.,(float)95.,(float)
	    100.,(float)120.,(float)130.,(float)140.,(float)150.,(float)190.,(
	    float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)
	    75.,(float)95.,(float)120.,(float)150.,(float)190.,(float)200.,(
	    float)220.,(float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)75.,(float)95.,(float)100.,(float)145.,(float)
	    190.,(float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(
	    float)95.,(float)100.,(float)120.,(float)160.,(float)170.,(float)
	    190.,(float)200.,(float)220.,(float)250.,(float)270. };
    static real h2r140[91]	/* was [13][7] */ = { (float)75.,(float)95.,(
	    float)100.,(float)115.,(float)130.,(float)175.,(float)200.,(float)
	    220.,(float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)
	    75.,(float)95.,(float)100.,(float)110.,(float)175.,(float)200.,(
	    float)220.,(float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)75.,(float)95.,(float)100.,(float)115.,(float)
	    130.,(float)180.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(
	    float)95.,(float)100.,(float)120.,(float)130.,(float)190.,(float)
	    200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)75.,(
	    float)80.,(float)85.,(float)95.,(float)100.,(float)120.,(float)
	    140.,(float)160.,(float)190.,(float)200.,(float)220.,(float)250.,(
	    float)270.,(float)75.,(float)80.,(float)85.,(float)95.,(float)
	    100.,(float)145.,(float)165.,(float)180.,(float)190.,(float)200.,(
	    float)220.,(float)250.,(float)270.,(float)75.,(float)85.,(float)
	    95.,(float)100.,(float)120.,(float)145.,(float)170.,(float)190.,(
	    float)200.,(float)220.,(float)250.,(float)270.,(float)0. };
    static real r1mr140[91]	/* was [13][7] */ = { (float)13.,(float)17.,(
	    float)57.,(float)57.,(float)28.,(float)51.,(float)56.,(float)56.,(
	    float)12.,(float)8.,(float)1.,(float)0.,(float)0.,(float)13.,(
	    float)17.,(float)57.,(float)57.,(float)36.,(float)46.,(float)55.,(
	    float)56.,(float)10.,(float)8.,(float)1.,(float)0.,(float)0.,(
	    float)13.,(float)17.,(float)57.,(float)57.,(float)46.,(float)56.,(
	    float)55.,(float)12.,(float)8.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)5.,(float)65.,(float)54.,(float)59.,(float)56.,(
	    float)56.,(float)53.,(float)23.,(float)16.,(float)13.,(float)3.,(
	    float)1.,(float)0.,(float)5.,(float)65.,(float)65.,(float)54.,(
	    float)29.,(float)16.,(float)16.,(float)10.,(float)2.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)5.,(float)65.,(float)76.,(
	    float)58.,(float)36.,(float)25.,(float)20.,(float)12.,(float)7.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)3.,(float)11.,(
	    float)35.,(float)75.,(float)91.,(float)76.,(float)58.,(float)49.,(
	    float)45.,(float)32.,(float)28.,(float)20.,(float)12. };
    static real r2mr140[91]	/* was [13][7] */ = { (float)7.,(float)43.,(
	    float)72.,(float)49.,(float)44.,(float)14.,(float)7.,(float)4.,(
	    float)1.,(float)0.,(float)0.,(float)0.,(float)0.,(float)7.,(float)
	    43.,(float)64.,(float)51.,(float)14.,(float)7.,(float)4.,(float)
	    1.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)7.,(
	    float)43.,(float)54.,(float)44.,(float)44.,(float)13.,(float)7.,(
	    float)4.,(float)1.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    3.,(float)5.,(float)26.,(float)34.,(float)46.,(float)41.,(float)
	    44.,(float)9.,(float)11.,(float)7.,(float)2.,(float)1.,(float)0.,(
	    float)3.,(float)5.,(float)26.,(float)34.,(float)35.,(float)35.,(
	    float)40.,(float)40.,(float)16.,(float)14.,(float)9.,(float)5.,(
	    float)2.,(float)3.,(float)5.,(float)26.,(float)34.,(float)24.,(
	    float)40.,(float)40.,(float)32.,(float)19.,(float)20.,(float)10.,(
	    float)7.,(float)3.,(float)3.,(float)15.,(float)15.,(float)9.,(
	    float)24.,(float)35.,(float)40.,(float)28.,(float)28.,(float)20.,(
	    float)10.,(float)8.,(float)0. };
    static real rk1mr140[91]	/* was [13][7] */ = { (float).8,(float)4.,(
	    float)0.,(float)-5.8,(float)1.533,(float).333,(float)0.,(float)
	    -.8,(float)-.2,(float)-.233,(float)-.05,(float)0.,(float)0.,(
	    float).8,(float)4.,(float)0.,(float)-4.2,(float)1.3,(float).6,(
	    float).04,(float)-.836,(float)-.1,(float)-.233,(float)-.05,(float)
	    0.,(float)0.,(float).8,(float)4.,(float)0.,(float)-2.2,(float)
	    .667,(float)-.029,(float)-.86,(float)-.2,(float)-.233,(float)-.05,
	    (float)0.,(float)0.,(float)0.,(float)3.,(float)-2.2,(float).25,(
	    float)-.3,(float)0.,(float)-.3,(float)-.75,(float)-.7,(float)-.15,
	    (float)-.333,(float)-.1,(float)-.033,(float)0.,(float)3.,(float)
	    0.,(float)-.367,(float)-.625,(float)-1.3,(float)0.,(float)-.2,(
	    float)-.4,(float)-.067,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)3.,(float)2.2,(float)-.4,(float)-.489,(float)-1.1,(float)
	    -.25,(float)-.267,(float)-.25,(float)-.2,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)1.6,(float)4.8,(float)4.,(float)3.2,(
	    float)-.75,(float)-.45,(float)-.9,(float)-.2,(float)-1.3,(float)
	    -.2,(float)-.267,(float)-.4,(float)-.3 };
    static real rk2mr140[91]	/* was [13][7] */ = { (float)1.8,(float)5.8,(
	    float)-1.533,(float)-.333,(float)-.667,(float)-.28,(float)-.15,(
	    float)-.1,(float)-.05,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)1.8,(float)4.2,(float)-1.3,(float)-.569,(float)-.28,(float)
	    -.15,(float)-.1,(float)-.05,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)1.8,(float)2.2,(float)-.667,(float)0.,(float)
	    -.62,(float)-.3,(float)-.15,(float)-.1,(float)-.05,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float).4,(float)4.2,(float).8,(
	    float)2.4,(float)-.25,(float).3,(float)-.583,(float).2,(float)-.2,
	    (float)-.167,(float)-.05,(float)-.033,(float)0.,(float).4,(float)
	    4.2,(float).8,(float).02,(float)0.,(float).25,(float)0.,(float)
	    -.6,(float)-.2,(float)-.25,(float)-.133,(float)-.15,(float)-.067,(
	    float).4,(float)4.2,(float).8,(float)-2.,(float).356,(float)0.,(
	    float)-.533,(float)-1.3,(float).1,(float)-.5,(float)-.1,(float)
	    -.2,(float)-.1,(float)1.2,(float)0.,(float)-1.2,(float).75,(float)
	    .44,(float).2,(float)-.6,(float)0.,(float)-.4,(float)-.333,(float)
	    -.1,(float)-.2,(float)0. };
    static integer j1mw70[7] = { 13,13,13,13,9,8,9 };
    static integer j2mw70[7] = { 10,10,11,11,9,8,11 };
    static real h1w70[91]	/* was [13][7] */ = { (float)75.,(float)80.,(
	    float)85.,(float)95.,(float)100.,(float)110.,(float)125.,(float)
	    145.,(float)180.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)75.,(float)80.,(float)85.,(float)95.,(float)100.,(float)
	    110.,(float)120.,(float)150.,(float)180.,(float)200.,(float)220.,(
	    float)250.,(float)270.,(float)75.,(float)80.,(float)85.,(float)
	    95.,(float)100.,(float)110.,(float)120.,(float)155.,(float)180.,(
	    float)200.,(float)220.,(float)250.,(float)270.,(float)75.,(float)
	    80.,(float)90.,(float)100.,(float)110.,(float)120.,(float)140.,(
	    float)160.,(float)190.,(float)200.,(float)220.,(float)250.,(float)
	    270.,(float)75.,(float)80.,(float)90.,(float)110.,(float)150.,(
	    float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(float)
	    0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)90.,(float)
	    100.,(float)150.,(float)200.,(float)250.,(float)270.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(
	    float)90.,(float)100.,(float)120.,(float)130.,(float)140.,(float)
	    200.,(float)270.,(float)0.,(float)0.,(float)0.,(float)0. };
    static real h2w70[91]	/* was [13][7] */ = { (float)75.,(float)90.,(
	    float)95.,(float)100.,(float)110.,(float)125.,(float)190.,(float)
	    200.,(float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)
	    75.,(float)90.,(float)95.,(float)100.,(float)110.,(float)125.,(
	    float)190.,(float)200.,(float)250.,(float)270.,(float)0.,(float)
	    0.,(float)0.,(float)75.,(float)90.,(float)95.,(float)100.,(float)
	    110.,(float)120.,(float)145.,(float)190.,(float)200.,(float)250.,(
	    float)270.,(float)0.,(float)0.,(float)75.,(float)80.,(float)95.,(
	    float)100.,(float)110.,(float)120.,(float)150.,(float)200.,(float)
	    220.,(float)250.,(float)270.,(float)0.,(float)0.,(float)75.,(
	    float)80.,(float)90.,(float)95.,(float)110.,(float)145.,(float)
	    200.,(float)250.,(float)270.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)75.,(float)80.,(float)90.,(float)100.,(float)140.,(
	    float)150.,(float)200.,(float)250.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(float)90.,(
	    float)100.,(float)120.,(float)130.,(float)140.,(float)160.,(float)
	    200.,(float)270.,(float)0.,(float)0. };
    static real r1mw70[91]	/* was [13][7] */ = { (float)28.,(float)35.,(
	    float)65.,(float)65.,(float)28.,(float)44.,(float)46.,(float)50.,(
	    float)25.,(float)25.,(float)10.,(float)5.,(float)0.,(float)28.,(
	    float)35.,(float)65.,(float)65.,(float)36.,(float)49.,(float)47.,(
	    float)47.,(float)25.,(float)25.,(float)10.,(float)5.,(float)0.,(
	    float)28.,(float)35.,(float)65.,(float)65.,(float)48.,(float)54.,(
	    float)51.,(float)43.,(float)25.,(float)25.,(float)10.,(float)5.,(
	    float)0.,(float)16.,(float)24.,(float)66.,(float)54.,(float)58.,(
	    float)50.,(float)50.,(float)38.,(float)25.,(float)25.,(float)10.,(
	    float)5.,(float)0.,(float)16.,(float)24.,(float)66.,(float)66.,(
	    float)46.,(float)30.,(float)20.,(float)6.,(float)3.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)16.,(float)24.,(float)66.,(
	    float)76.,(float)49.,(float)32.,(float)12.,(float)7.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)6.,(float)19.,(
	    float)67.,(float)91.,(float)64.,(float)68.,(float)60.,(float)40.,(
	    float)12.,(float)0.,(float)0.,(float)0.,(float)0. };
    static real r2mw70[91]	/* was [13][7] */ = { (float)5.,(float)35.,(
	    float)35.,(float)72.,(float)56.,(float)54.,(float)12.,(float)12.,(
	    float)2.,(float)0.,(float)0.,(float)0.,(float)0.,(float)5.,(float)
	    35.,(float)35.,(float)64.,(float)51.,(float)53.,(float)12.,(float)
	    12.,(float)2.,(float)0.,(float)0.,(float)0.,(float)0.,(float)5.,(
	    float)35.,(float)35.,(float)52.,(float)46.,(float)49.,(float)41.,(
	    float)12.,(float)12.,(float)2.,(float)0.,(float)0.,(float)0.,(
	    float)4.,(float)10.,(float)40.,(float)46.,(float)42.,(float)50.,(
	    float)41.,(float)12.,(float)7.,(float)2.,(float)0.,(float)0.,(
	    float)0.,(float)4.,(float)10.,(float)30.,(float)34.,(float)34.,(
	    float)51.,(float)14.,(float)4.,(float)2.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)4.,(float)10.,(float)30.,(float)24.,(
	    float)45.,(float)48.,(float)20.,(float)5.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)2.,(float)6.,(float)17.,(
	    float)23.,(float)9.,(float)36.,(float)32.,(float)40.,(float)40.,(
	    float)20.,(float)6.,(float)0.,(float)0. };
    static real rk1mw70[91]	/* was [13][7] */ = { (float)1.4,(float)6.,(
	    float)0.,(float)-7.4,(float)1.6,(float).133,(float).2,(float)
	    -.714,(float)0.,(float)-.75,(float)-.167,(float)-.25,(float)0.,(
	    float)1.4,(float)6.,(float)0.,(float)-5.8,(float)1.3,(float)-.2,(
	    float)0.,(float)-.733,(float)0.,(float)-.75,(float)-.167,(float)
	    -.25,(float)0.,(float)1.4,(float)6.,(float)0.,(float)-3.4,(float)
	    .6,(float)-.3,(float)-.229,(float)-.72,(float)0.,(float)-.75,(
	    float)-.167,(float)-.25,(float)0.,(float)1.6,(float)4.2,(float)
	    -1.2,(float).4,(float)-.8,(float)0.,(float)-.6,(float)-.433,(
	    float)0.,(float)-.75,(float)-.167,(float)-.25,(float)0.,(float)
	    1.6,(float)4.2,(float)0.,(float)-.5,(float)-.32,(float)-.5,(float)
	    -.467,(float)-.15,(float)-.1,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)1.6,(float)4.2,(float)1.,(float)-.54,(float)-.34,(float)
	    -.4,(float)-.25,(float)-.2,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)0.,(float)2.6,(float)4.8,(float)2.4,(float)-1.35,(float)
	    .4,(float)-.8,(float)-.333,(float)-.4,(float)-.3,(float)0.,(float)
	    0.,(float)0.,(float)0. };
    static real rk2mw70[91]	/* was [13][7] */ = { (float)2.,(float)0.,(
	    float)7.4,(float)-1.6,(float)-.133,(float)-.646,(float)0.,(float)
	    -.2,(float)-.1,(float)0.,(float)0.,(float)0.,(float)0.,(float)2.,(
	    float)0.,(float)5.8,(float)-1.3,(float).133,(float)-.631,(float)
	    0.,(float)-.2,(float)-.1,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)2.,(float)0.,(float)3.4,(float)-.6,(float).3,(float)-.32,(
	    float)-.644,(float)0.,(float)-.2,(float)-.1,(float)0.,(float)0.,(
	    float)0.,(float)1.2,(float)2.,(float)1.2,(float)-.4,(float).8,(
	    float)-.3,(float)-.58,(float)-.25,(float)-.167,(float)-.1,(float)
	    0.,(float)0.,(float)0.,(float)1.2,(float)2.,(float).8,(float)0.,(
	    float).486,(float)-.673,(float)-.2,(float)-.1,(float)-.066,(float)
	    0.,(float)0.,(float)0.,(float)0.,(float)1.2,(float)2.,(float)-.6,(
	    float).525,(float).3,(float)-.56,(float)-.3,(float)-.1,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float).8,(float)2.2,(
	    float)1.2,(float)-1.4,(float)1.35,(float)-.4,(float).8,(float)0.,(
	    float)-.5,(float)-.2,(float)-.167,(float)0.,(float)0. };
    static integer j1mw140[7] = { 12,11,11,11,11,10,12 };
    static integer j2mw140[7] = { 10,11,11,11,11,10,12 };
    static real h1w140[91]	/* was [13][7] */ = { (float)75.,(float)80.,(
	    float)85.,(float)95.,(float)100.,(float)110.,(float)125.,(float)
	    145.,(float)190.,(float)200.,(float)220.,(float)250.,(float)0.,(
	    float)75.,(float)80.,(float)85.,(float)95.,(float)100.,(float)
	    110.,(float)120.,(float)150.,(float)190.,(float)220.,(float)250.,(
	    float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(float)95.,(
	    float)100.,(float)110.,(float)120.,(float)155.,(float)190.,(float)
	    220.,(float)250.,(float)0.,(float)0.,(float)75.,(float)80.,(float)
	    90.,(float)100.,(float)110.,(float)120.,(float)140.,(float)160.,(
	    float)190.,(float)220.,(float)250.,(float)0.,(float)0.,(float)75.,
	    (float)80.,(float)90.,(float)110.,(float)150.,(float)160.,(float)
	    190.,(float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(
	    float)0.,(float)75.,(float)80.,(float)90.,(float)100.,(float)150.,
	    (float)160.,(float)190.,(float)200.,(float)250.,(float)270.,(
	    float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)90.,(
	    float)100.,(float)120.,(float)130.,(float)140.,(float)160.,(float)
	    190.,(float)200.,(float)250.,(float)270.,(float)0. };
    static real h2w140[91]	/* was [13][7] */ = { (float)75.,(float)90.,(
	    float)95.,(float)100.,(float)110.,(float)125.,(float)190.,(float)
	    200.,(float)220.,(float)250.,(float)0.,(float)0.,(float)0.,(float)
	    75.,(float)90.,(float)95.,(float)100.,(float)110.,(float)120.,(
	    float)125.,(float)190.,(float)200.,(float)220.,(float)250.,(float)
	    0.,(float)0.,(float)75.,(float)90.,(float)95.,(float)100.,(float)
	    110.,(float)120.,(float)145.,(float)190.,(float)200.,(float)220.,(
	    float)250.,(float)0.,(float)0.,(float)75.,(float)80.,(float)95.,(
	    float)100.,(float)110.,(float)120.,(float)150.,(float)190.,(float)
	    200.,(float)220.,(float)250.,(float)0.,(float)0.,(float)75.,(
	    float)80.,(float)90.,(float)95.,(float)110.,(float)145.,(float)
	    190.,(float)200.,(float)220.,(float)250.,(float)270.,(float)0.,(
	    float)0.,(float)75.,(float)80.,(float)90.,(float)100.,(float)140.,
	    (float)150.,(float)200.,(float)220.,(float)250.,(float)270.,(
	    float)0.,(float)0.,(float)0.,(float)75.,(float)80.,(float)85.,(
	    float)90.,(float)100.,(float)120.,(float)130.,(float)140.,(float)
	    160.,(float)180.,(float)200.,(float)220.,(float)0. };
    static real r1mw140[91]	/* was [13][7] */ = { (float)28.,(float)35.,(
	    float)65.,(float)65.,(float)28.,(float)44.,(float)46.,(float)50.,(
	    float)9.,(float)6.,(float)2.,(float)0.,(float)0.,(float)28.,(
	    float)35.,(float)65.,(float)65.,(float)36.,(float)49.,(float)47.,(
	    float)47.,(float)8.,(float)2.,(float)0.,(float)0.,(float)0.,(
	    float)28.,(float)35.,(float)65.,(float)65.,(float)48.,(float)54.,(
	    float)51.,(float)43.,(float)8.,(float)2.,(float)0.,(float)0.,(
	    float)0.,(float)16.,(float)24.,(float)66.,(float)54.,(float)58.,(
	    float)50.,(float)50.,(float)42.,(float)8.,(float)2.,(float)0.,(
	    float)0.,(float)0.,(float)16.,(float)24.,(float)66.,(float)66.,(
	    float)46.,(float)49.,(float)9.,(float)10.,(float)7.,(float)2.,(
	    float)0.,(float)0.,(float)0.,(float)16.,(float)24.,(float)66.,(
	    float)76.,(float)49.,(float)54.,(float)10.,(float)14.,(float)4.,(
	    float)1.,(float)0.,(float)0.,(float)0.,(float)6.,(float)19.,(
	    float)67.,(float)91.,(float)64.,(float)68.,(float)60.,(float)58.,(
	    float)11.,(float)20.,(float)5.,(float)2.,(float)0. };
    static real r2mw140[91]	/* was [13][7] */ = { (float)5.,(float)35.,(
	    float)35.,(float)72.,(float)56.,(float)54.,(float)5.,(float)5.,(
	    float)1.,(float)0.,(float)0.,(float)0.,(float)0.,(float)5.,(float)
	    35.,(float)35.,(float)64.,(float)51.,(float)53.,(float)53.,(float)
	    5.,(float)5.,(float)1.,(float)0.,(float)0.,(float)0.,(float)5.,(
	    float)35.,(float)35.,(float)52.,(float)46.,(float)49.,(float)41.,(
	    float)5.,(float)5.,(float)1.,(float)0.,(float)0.,(float)0.,(float)
	    4.,(float)10.,(float)40.,(float)46.,(float)42.,(float)50.,(float)
	    41.,(float)5.,(float)5.,(float)1.,(float)0.,(float)0.,(float)0.,(
	    float)4.,(float)10.,(float)30.,(float)34.,(float)34.,(float)51.,(
	    float)10.,(float)5.,(float)3.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)4.,(float)10.,(float)30.,(float)24.,(float)45.,(
	    float)48.,(float)4.,(float)2.,(float)1.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)2.,(float)6.,(float)17.,(float)23.,(
	    float)9.,(float)36.,(float)32.,(float)40.,(float)39.,(float)29.,(
	    float)1.,(float)0.,(float)0. };
    static real rk1mw140[91]	/* was [13][7] */ = { (float)1.4,(float)6.,(
	    float)0.,(float)-7.4,(float)1.6,(float).133,(float).2,(float)
	    -.911,(float)-.3,(float)-.2,(float)-.066,(float)0.,(float)0.,(
	    float)1.4,(float)6.,(float)0.,(float)-5.8,(float)1.3,(float)-.2,(
	    float)0.,(float)-.975,(float)-.2,(float)-.066,(float)0.,(float)0.,
	    (float)0.,(float)1.4,(float)6.,(float)0.,(float)-3.4,(float).6,(
	    float)-.3,(float)-.229,(float)-1.,(float)-.2,(float)-.066,(float)
	    0.,(float)0.,(float)0.,(float)1.6,(float)4.2,(float)-1.2,(float)
	    .4,(float)-.8,(float)0.,(float)-.4,(float)-1.133,(float)-.2,(
	    float)-.066,(float)0.,(float)0.,(float)0.,(float)1.6,(float)4.2,(
	    float)0.,(float)-.5,(float).3,(float)-1.133,(float).1,(float)-.15,
	    (float)-.166,(float)-.1,(float)0.,(float)0.,(float)0.,(float)1.6,(
	    float)4.2,(float)1.,(float)-.54,(float).5,(float)-1.466,(float).4,
	    (float)-.2,(float)-.15,(float)-.0333,(float)0.,(float)0.,(float)
	    0.,(float)2.6,(float)4.8,(float)2.4,(float)-1.35,(float).4,(float)
	    -.8,(float)-.1,(float)-1.566,(float).9,(float)-.3,(float)-.15,(
	    float)-.05,(float)0. };
    static real rk2mw140[91]	/* was [13][7] */ = { (float)2.,(float)0.,(
	    float)7.4,(float)-1.6,(float)-.133,(float)-.754,(float)0.,(float)
	    -.2,(float)-.033,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    2.,(float)0.,(float)5.8,(float)-1.3,(float).2,(float)0.,(float)
	    -.738,(float)0.,(float)-.2,(float)-.033,(float)0.,(float)0.,(
	    float)0.,(float)2.,(float)0.,(float)3.4,(float)-.6,(float).3,(
	    float)-.32,(float)-.8,(float)0.,(float)-.2,(float)-.033,(float)0.,
	    (float)0.,(float)0.,(float)1.2,(float)2.,(float)1.2,(float)-.4,(
	    float).8,(float)-.3,(float)-.9,(float)0.,(float)-.2,(float)-.033,(
	    float)0.,(float)0.,(float)0.,(float)1.2,(float)2.,(float).8,(
	    float)0.,(float).486,(float)-.911,(float)-.5,(float)-.1,(float)
	    -.066,(float)-.05,(float)0.,(float)0.,(float)0.,(float)1.2,(float)
	    2.,(float)-.6,(float).525,(float).3,(float)-.88,(float)-.1,(float)
	    -.033,(float)-.05,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    .8,(float)2.2,(float)1.2,(float)-1.4,(float)1.35,(float)-.4,(
	    float).8,(float)-.05,(float)-.5,(float)-1.4,(float)-.05,(float)0.,
	    (float)0. };

    /* Builtin functions */
    double r_nint(real *);

    /* Local variables */
    static real h__, z__;
    extern /* Subroutine */ int aprok_(integer *, integer *, real *, real *, 
	    real *, real *, real *, real *, real *, real *, real *, real *);
    static real r170, r270, r1140, r2140;

/* ---------------------------------------------------------------- */
/*     INPUT DATA : */
/*      hei  -  altitude in km */
/*      xhi  -  solar zenith angle in degree */
/*      it   -  season (month) */
/*      F    -  10.7cm solar radio flux */
/*     OUTPUT DATA : */
/*     R1 -  NO+ concentration (in percent) */
/*     R2 -  O2+ concentration (in percent) */
/*     R3 -  Cb+ concentration (in percent) */
/*     R4 -  O+  concentration (in percent) */

/*  A.D. Danilov and N.V. Smirnova, Improving the 75 to 300 km ion */
/*  composition model of the IRI, Adv. Space Res. 15, #2, 171-177, 1995. */

/* ----------------------------------------------------------------- */
    h__ = *hei;
    z__ = *xhi;
    if (z__ < (float)20.) {
	z__ = (float)20.;
    }
    if (z__ > (float)90.) {
	z__ = (float)90.;
    }
    if (*it == 1 || *it == 2 || *it == 11 || *it == 12) {
	if (*f < (float)140.) {
	    aprok_(j1mw70, j2mw70, h1w70, h2w70, r1mw70, r2mw70, rk1mw70, 
		    rk2mw70, &h__, &z__, r1, r2);
	    r170 = *r1;
	    r270 = *r2;
	}
	if (*f > (float)70.) {
	    aprok_(j1mw140, j2mw140, h1w140, h2w140, r1mw140, r2mw140, 
		    rk1mw140, rk2mw140, &h__, &z__, r1, r2);
	    r1140 = *r1;
	    r2140 = *r2;
	}
	if (*f > (float)70. && *f < (float)140.) {
	    *r1 = r170 + (r1140 - r170) * (*f - 70) / 70;
	    *r2 = r270 + (r2140 - r270) * (*f - 70) / 70;
	}
    }
    if (*it == 5 || *it == 6 || *it == 7 || *it == 8) {
	if (*f < (float)140.) {
	    aprok_(j1ms70, j2ms70, h1s70, h2s70, r1ms70, r2ms70, rk1ms70, 
		    rk2ms70, &h__, &z__, r1, r2);
	    r170 = *r1;
	    r270 = *r2;
	}
	if (*f > (float)70.) {
	    aprok_(j1ms140, j2ms140, h1s140, h2s140, r1ms140, r2ms140, 
		    rk1ms140, rk2ms140, &h__, &z__, r1, r2);
	    r1140 = *r1;
	    r2140 = *r2;
	}
	if (*f > (float)70. && *f < (float)140.) {
	    *r1 = r170 + (r1140 - r170) * (*f - 70) / 70;
	    *r2 = r270 + (r2140 - r270) * (*f - 70) / 70;
	}
    }
    if (*it == 3 || *it == 4 || *it == 9 || *it == 10) {
	if (*f < (float)140.) {
	    aprok_(j1mr70, j2mr70, h1r70, h2r70, r1mr70, r2mr70, rk1mr70, 
		    rk2mr70, &h__, &z__, r1, r2);
	    r170 = *r1;
	    r270 = *r2;
	}
	if (*f > (float)70.) {
	    aprok_(j1mr140, j2mr140, h1r140, h2r140, r1mr140, r2mr140, 
		    rk1mr140, rk2mr140, &h__, &z__, r1, r2);
	    r1140 = *r1;
	    r2140 = *r2;
	}
	if (*f > (float)70. && *f < (float)140.) {
	    *r1 = r170 + (r1140 - r170) * (*f - 70) / 70;
	    *r2 = r270 + (r2140 - r270) * (*f - 70) / 70;
	}
    }
    *r3 = (float)0.;
    *r4 = (float)0.;
    if (h__ < (float)100.) {
	*r3 = 100 - (*r1 + *r2);
    }
    if (h__ >= (float)100.) {
	*r4 = 100 - (*r1 + *r2);
    }
    if (*r3 < (float)0.) {
	*r3 = (float)0.;
    }
    if (*r4 < (float)0.) {
	*r4 = (float)0.;
    }
    *r1 = r_nint(r1);
    *r2 = r_nint(r2);
    *r3 = r_nint(r3);
    *r4 = r_nint(r4);
/* L300: */
    return 0;
} /* ionco2_ */



/* Subroutine */ int aprok_(integer *j1m, integer *j2m, real *h1, real *h2, 
	real *r1m, real *r2m, real *rk1m, real *rk2m, real *hei, real *xhi, 
	real *r1, real *r2)
{
    /* Initialized data */

    static real zm[7] = { (float)20.,(float)40.,(float)60.,(float)70.,(float)
	    80.,(float)85.,(float)90. };

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static real h__;
    static integer i__;
    static real z__;
    static integer j1, j2, i1, i2, i3;
    static real h01, h02, r01, r02, r11, r12, rk, rk1, rk2;

/* --- Modified by A. Edinburg, Software Now cc, 1st December 1999. */
/* --- Modfications as prescribed by D Belitza. */
/* ----------------------------------------------------------------- */
    /* Parameter adjustments */
    rk2m -= 14;
    rk1m -= 14;
    r2m -= 14;
    r1m -= 14;
    h2 -= 14;
    h1 -= 14;
    --j2m;
    --j1m;

    /* Function Body */
    h__ = *hei;
    z__ = *xhi;
    j1 = 1;
    j2 = 1;
    i1 = 1;
    for (i__ = 1; i__ <= 7; ++i__) {
	i1 = i__;
	if (z__ == zm[i__ - 1]) {
	    j1 = 0;
	}
/* ----- Old code 1st Dec 1999        if(z.le.zm(i)) exit */
	if (z__ <= zm[i__ - 1]) {
	    goto L11;
	}
/* L1: */
    }
L11:
    i2 = 1;
    i__1 = j1m[i1];
    for (i__ = 2; i__ <= i__1; ++i__) {
	i2 = i__ - 1;
/* ----- Old code 1st Dec 1999 if(h.lt.h1(i,i1)) exit */
	if (h__ < h1[i__ + i1 * 13]) {
	    goto L22;
	}
	i2 = j1m[i1];
/* L2: */
    }
/* ----- following line added 1st Dec 1999. */
L22:
    i3 = 1;
    i__1 = j2m[i1];
    for (i__ = 2; i__ <= i__1; ++i__) {
	i3 = i__ - 1;
/* ----- Old code 1st Dec 1999 if(h.lt.h2(i,i1)) exit */
	if (h__ < h2[i__ + i1 * 13]) {
	    goto L33;
	}
	i3 = j2m[i1];
/* L3: */
    }
/* ----- following line added 1st Dec 1999. */
L33:
    r01 = r1m[i2 + i1 * 13];
    r02 = r2m[i3 + i1 * 13];
    rk1 = rk1m[i2 + i1 * 13];
    rk2 = rk2m[i3 + i1 * 13];
    h01 = h1[i2 + i1 * 13];
    h02 = h2[i3 + i1 * 13];
    *r1 = r01 + rk1 * (h__ - h01);
    *r2 = r02 + rk2 * (h__ - h02);
    if (j1 == 1) {
	j1 = 0;
	j2 = 0;
	--i1;
	r11 = *r1;
	r12 = *r2;
	goto L11;
    }
    if (j2 == 0) {
	rk = (z__ - zm[i1 - 1]) / (zm[i1] - zm[i1 - 1]);
	*r1 += (r11 - *r1) * rk;
	*r2 += (r12 - *r2) * rk;
    }
    return 0;
} /* aprok_ */



/* Subroutine */ int ioncom_new__(real *hei, real *xhia, real *slati, real *
	covi, real *zmos, real *dion)
{
    static real h__;
    static integer i__;
    static real diont[7], xlati;
    extern /* Subroutine */ int ionco1_(real *, real *, real *, real *, real *
	    , real *), ionco2_(real *, real *, integer *, real *, real *, 
	    real *, real *, real *);
    static real ro;
    static integer monsea;
    static real zmosea, ro2, rcl, cov, dup[4], xhi, rno;

/* ------------------------------------------------------- */
/*       see IONCO1 for explanation of i/o parameters */
/*       NOTE: xhi,xlati are in DEGREES !!! */
/*       NOTE: zmosea is the seasonal northern month, so */
/*             for the southern hemisphere zmosea=month+6 */
/* ------------------------------------------------------- */
    /* Parameter adjustments */
    --dion;

    /* Function Body */
    for (i__ = 1; i__ <= 7; ++i__) {
/* L1122: */
	diont[i__ - 1] = (float)0.;
    }
    h__ = *hei;
    xhi = *xhia;
    xlati = *slati;
    cov = *covi;
    zmosea = *zmos;
    if (h__ > (float)300.) {
	ionco1_(&h__, &xhi, &xlati, &cov, &zmosea, dup);
	diont[0] = dup[0];
	diont[1] = dup[1];
	diont[2] = dup[2];
	diont[3] = dup[3];
    } else {
	monsea = (integer) zmosea;
	ionco2_(&h__, &xhi, &monsea, &cov, &rno, &ro2, &rcl, &ro);
	diont[4] = rno;
	diont[5] = ro2;
	diont[6] = rcl;
	diont[0] = ro;
    }
    for (i__ = 1; i__ <= 7; ++i__) {
	dion[i__] = diont[i__ - 1];
/* L1: */
    }
    return 0;
} /* ioncom_new__ */



/* Subroutine */ int ionco1_(real *h__, real *zd, real *fd, real *fs, real *t,
	 real *cn)
{
    /* Initialized data */

    static real po[30]	/* was [5][6] */ = { (float)0.,(float)0.,(float)0.,(
	    float)0.,(float)98.5,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)320.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    -2.59e-4,(float)2.79e-4,(float)-.00333,(float)-.00352,(float)
	    -.00516,(float)-.0247,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)-2.5e-6,(float).00104,(float)-1.79e-4,(float)-4.29e-5,(
	    float)1.01e-5,(float)-.00127 };
    static real ph[30]	/* was [5][6] */ = { (float)-4.97e-7,(float)-.121,(
	    float)-.131,(float)0.,(float)98.1,(float)355.,(float)-191.,(float)
	    -127.,(float)0.,(float)2040.,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)-4.79e-6,(float)-2e-4,(float)5.67e-4,(float)2.6e-4,(
	    float)0.,(float)-.00508,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(float)0. };
    static real pn[30]	/* was [5][6] */ = { (float).76,(float)-5.62,(float)
	    -4.99,(float)0.,(float)5.79,(float)83.,(float)-369.,(float)-324.,(
	    float)0.,(float)593.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)-6.3e-5,(float)-.00674,(float)-.00793,(float)-.00465,(float)
	    0.,(float)-.00326,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    -1.17e-5,(float).00488,(float)-.00131,(float)-7.03e-4,(float)0.,(
	    float)-.00238 };
    static real phe[30]	/* was [5][6] */ = { (float)-.895,(float)6.1,(float)
	    5.39,(float)0.,(float)8.01,(float)0.,(float)0.,(float)0.,(float)
	    0.,(float)1200.,(float)0.,(float)0.,(float)0.,(float)0.,(float)
	    -1.04e-5,(float).0019,(float)9.53e-4,(float).00106,(float)0.,(
	    float)-.00344,(float)0.,(float)0.,(float)0.,(float)0.,(float)0.,(
	    float)0.,(float)0.,(float)0.,(float)0.,(float)0. };

    /* Builtin functions */
    double cos(doublereal), exp(doublereal);

    /* Local variables */
    static real beth[4], betl[4], f;
    static integer i__, j;
    static real p[120]	/* was [5][6][4] */, s, z__, cm[4], hm[4], hx, alh[4],
	     all[4], arg, var[6];

/* --------------------------------------------------------------- */
/* ion composition model */
/*   A.D. Danilov and A.P. Yaichnikov, A New Model of the Ion */
/*   Composition at 75 to 1000 km for IRI, Adv. Space Res. 5, #7, */
/*   75-79, 107-108, 1985 */

/*       h       altitude in km */
/*       zd      solar zenith angle in degrees */
/*       fd      latitude in degrees */
/*       fs      10.7cm solar radio flux */
/*       t       season (decimal month) */
/*       cn(1)   O+  relative density in percent */
/*       cn(2)   H+  relative density in percent */
/*       cn(3)   N+  relative density in percent */
/*       cn(4)   He+ relative density in percent */
/* Please note: molecular ions are now computed in IONCO2 */
/*       [cn(5)   NO+ relative density in percent */
/*       [cn(6)   O2+ relative density in percent */
/*       [cn(7)   cluster ions  relative density in percent */
/* --------------------------------------------------------------- */

/*        dimension       cn(7),cm(7),hm(7),alh(7),all(7),beth(7), */
/*     &                  betl(7),p(5,6,7),var(6),po(5,6),ph(5,6), */
/*     &                  pn(5,6),phe(5,6),pno(5,6),po2(5,6),pcl(5,6) */
    /* Parameter adjustments */
    --cn;

    /* Function Body */
/*       data pno/-22.4,17.7,-13.4,-4.88,62.3,32.7,0.,19.8,2.07,115., */
/*    &          5*0.,3.94E-3,0.,2.48E-3,2.15E-4,6.67E-3,5*0., */
/*    &          -8.4E-3,0.,-3.64E-3,2.E-3,-2.59E-2/ */
/*       data po2/8.,-12.2,9.9,5.8,53.4,-25.2,0.,-28.5,-6.72,120., */
/*    &          5*0.,-1.4E-2,0.,-9.3E-3,3.3E-3,2.8E-2,5*0.,4.25E-3, */
/*    &          0.,-6.04E-3,3.85E-3,-3.64E-2/ */
/*       data pcl/4*0.,100.,4*0.,75.,10*0.,4*0.,-9.04E-3,-7.28E-3, */
/*    &          2*0.,3.46E-3,-2.11E-2/ */
    z__ = *zd * const_1.umr;
    f = *fd * const_1.umr;
    for (i__ = 1; i__ <= 5; ++i__) {
	for (j = 1; j <= 6; ++j) {
	    p[i__ + (j + 6) * 5 - 36] = po[i__ + j * 5 - 6];
	    p[i__ + (j + 12) * 5 - 36] = ph[i__ + j * 5 - 6];
	    p[i__ + (j + 18) * 5 - 36] = pn[i__ + j * 5 - 6];
	    p[i__ + (j + 24) * 5 - 36] = phe[i__ + j * 5 - 6];
/*               p(i,j,5)=pno(i,j) */
/*               p(i,j,6)=po2(i,j) */
/*               p(i,j,7)=pcl(i,j) */
/* L8: */
	}
    }
    s = (float)0.;
/*       do 5 i=1,7 */
    for (i__ = 1; i__ <= 4; ++i__) {
	for (j = 1; j <= 6; ++j) {
	    var[j - 1] = p[(j + i__ * 6) * 5 - 35] * cos(z__) + p[(j + i__ * 
		    6) * 5 - 34] * cos(f) + p[(j + i__ * 6) * 5 - 33] * cos(((
		    float)300. - *fs) * (float).013) + p[(j + i__ * 6) * 5 - 
		    32] * cos((*t - (float)6.) * (float).52) + p[(j + i__ * 6)
		     * 5 - 31];
/* L7: */
	}
	cm[i__ - 1] = var[0];
	hm[i__ - 1] = var[1];
	all[i__ - 1] = var[2];
	betl[i__ - 1] = var[3];
	alh[i__ - 1] = var[4];
	beth[i__ - 1] = var[5];
	hx = *h__ - hm[i__ - 1];
	if (hx < (float)0.) {
	    goto L1;
	} else if (hx == 0) {
	    goto L2;
	} else {
	    goto L3;
	}
L1:
	arg = hx * (hx * all[i__ - 1] + betl[i__ - 1]);
	cn[i__] = (float)0.;
	if (arg > -argexp_1.argmax) {
	    cn[i__] = cm[i__ - 1] * exp(arg);
	}
	goto L4;
L2:
	cn[i__] = cm[i__ - 1];
	goto L4;
L3:
	arg = hx * (hx * alh[i__ - 1] + beth[i__ - 1]);
	cn[i__] = (float)0.;
	if (arg > -argexp_1.argmax) {
	    cn[i__] = cm[i__ - 1] * exp(arg);
	}
L4:
	if (cn[i__] < cm[i__ - 1] * (float).005) {
	    cn[i__] = (float)0.;
	}
	if (cn[i__] > cm[i__ - 1]) {
	    cn[i__] = cm[i__ - 1];
	}
	s += cn[i__];
/* L5: */
    }
/*       do 6 i=1,7 */
    for (i__ = 1; i__ <= 4; ++i__) {
/* L6: */
	cn[i__] = cn[i__] / s * (float)100.;
    }
    return 0;
} /* ionco1_ */



/* Subroutine */ int tcon_(integer *yr, integer *mm, integer *day, integer *
	idn, real *rz, real *ig, real *rsn, integer *nmonth)
{
    /* Format strings */
    static char fmt_8000[] = "(1x,i10,\002** OUT OF RANGE **\002/,5x,\002The\
 file IG_RZ.DAT which contains the indices Rz12\002,\002 and IG12\002/5x,\
\002currently only covers the time period\002,\002 (yymm) : \002,i6,\002-\
\002,i6)";

    /* System generated locals */
    integer i__1;
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer f_open(olist *), s_rsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_rsle();
    double sqrt(doublereal);
    integer f_clos(cllist *), s_wsfe(cilist *), do_fio(integer *, char *, 
	    ftnlen), e_wsfe();

    /* Local variables */
    extern /* Subroutine */ int moda_(integer *, integer *, integer *, 
	    integer *, integer *, integer *);
    static integer midm, iupd;
    static real covr;
    static integer iupm, imst, iupy, iyst;
    static real ionoindx[602];
    static integer i__, iflag, imend, iyend;
    static real indrz[602];
    static integer iytmp, iymst, inum_vals__, jj;
    static real zi;
    static integer iymend, nrdaym, num;
    static real rrr;
    static integer idd1, idd2, imm2, iyy2;

    /* Fortran I/O blocks */
    static cilist io___444 = { 0, 12, 0, 0, 0 };
    static cilist io___448 = { 0, 12, 0, 0, 0 };
    static cilist io___456 = { 0, 12, 0, 0, 0 };
    static cilist io___459 = { 0, 12, 0, 0, 0 };
    static cilist io___466 = { 0, 6, 0, fmt_8000, 0 };


/* ---------------------------------------------------------------- */
/* input:        yr,mm,day       year(yyyy),month(mm),day(dd) */
/*               idn             day of year(ddd) */
/* output:       rz(3)           12-month-smoothed solar sunspot number */
/*               ig(3)           12-month-smoothed IG index */
/*               rsn             interpolation parameter */
/*               nmonth          previous or following month depending */
/*                               on day */

/* rz(1), ig(1) contain the indices for the month mm and rz(2), ig(2) */
/* for the previous month (if day less than 15) or for the following */
/* month (otherwise). These indices are for the mid of the month. The */
/* indices for the given day are obtained by linear interpolation and */
/* are stored in rz(3) and ig(3). */


/* Rz12 and IG are determined from the file IG_RZ.DAT which has the */
/* following structure: */
/* day, month, year of the last update of this file, */
/* start month, start year, end month, end year, */
/* the 12 IG indices (13-months running mean) for the first year, */
/* the 12 IG indices for the second year and so on until the end year, */
/* the 12 Rz indices (13-months running mean) for the first year, */
/* the 12 Rz indices for the second year and so on until the end year. */
/* The inteporlation procedure also requires the IG and Rz values for */
/* the month preceeding the start month and the IG and Rz values for the */
/* month following the end month. These values are also included in IG_RZ. */

/* A negative Rz index means that the given index is the 13-months-running */
/* mean of the solar radio flux (F10.7). The close correlation between (Rz)12 */
/* and (F10.7)12 is used to derive the (Rz)12 indices. */

/* An IG index of -111 indicates that no IG values are available for the */
/* time period. In this case a correlation function between (IG)12 and (Rz)12 */
/* is used to obtain (IG)12. */

/* The computation of the 13-month-running mean for month M requires the indices */
/* for the six months preceeding M and the six months following M (month: M-6, */
/* ..., M+6). To calculate the current running mean one therefore requires */
/* predictions of the indix for the next six months. Starting from six months */
/* before the UPDATE DATE (listed at the top of the file) and onward the */
/* indices are therefore based on indices predictions. */

    /* Parameter adjustments */
    --ig;
    --rz;

    /* Function Body */
    if (iflag == 0) {
/*         open(unit=12,file='ig_rz.dat',status='old') */
	o__1.oerr = 0;
	o__1.ounit = 12;
	o__1.ofnmlen = 49;
	o__1.ofnm = "/usr/local/etc/httpd/cgi-bin/models/IRI/ig_rz.dat";
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);

/* Read the update date, the start date and the end date (mm,yyyy), and */
/* get number of data points to read. */

	s_rsle(&io___444);
	do_lio(&c__3, &c__1, (char *)&iupd, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&iupm, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&iupy, (ftnlen)sizeof(integer));
	e_rsle();
	s_rsle(&io___448);
	do_lio(&c__3, &c__1, (char *)&imst, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&iyst, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&imend, (ftnlen)sizeof(integer));
	do_lio(&c__3, &c__1, (char *)&iyend, (ftnlen)sizeof(integer));
	e_rsle();
	iymst = iyst * 100 + imst;
	iymend = iyend * 100 + imend;
/* inum_vals= 12-imst+1+(iyend-iyst-1)*12 +imend + 2 */
/*            1st year \ full years       \last y\ before & after */
	inum_vals__ = 3 - imst + (iyend - iyst) * 12 + imend;

/* Read all the ionoindx and indrz values */
	s_rsle(&io___456);
	i__1 = inum_vals__;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__4, &c__1, (char *)&ionoindx[i__ - 1], (ftnlen)sizeof(
		    real));
	}
	e_rsle();
	s_rsle(&io___459);
	i__1 = inum_vals__;
	for (i__ = 1; i__ <= i__1; ++i__) {
	    do_lio(&c__4, &c__1, (char *)&indrz[i__ - 1], (ftnlen)sizeof(real)
		    );
	}
	e_rsle();
	i__1 = inum_vals__;
	for (jj = 1; jj <= i__1; ++jj) {
	    rrr = indrz[jj - 1];
	    if (rrr < (float)0.) {
		covr = dabs(rrr);
		rrr = sqrt(covr + (float)85.12) * (float)33.52 - (float)
			408.99;
		if (rrr < (float)0.) {
		    rrr = (float)0.;
		}
		indrz[jj - 1] = rrr;
	    }
	    if (ionoindx[jj - 1] > (float)-90.) {
		goto L1;
	    }
	    zi = ((float)1.4683266 - rrr * (float).00267690893) * rrr - (
		    float)12.349154;
	    if (zi > (float)274.) {
		zi = (float)274.;
	    }
	    ionoindx[jj - 1] = zi;
L1:
	    ;
	}
	cl__1.cerr = 0;
	cl__1.cunit = 12;
	cl__1.csta = 0;
	f_clos(&cl__1);
	iflag = 1;
    }
    iytmp = *yr * 100 + *mm;
    if (iytmp < iymst || iytmp > iymend) {
	s_wsfe(&io___466);
	do_fio(&c__1, (char *)&iytmp, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iymst, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&iymend, (ftnlen)sizeof(integer));
	e_wsfe();
	*nmonth = -1;
	return 0;
    }
/*       num=12-imst+1+(yr-iyst-1)*12+mm+1 */
    num = 2 - imst + (*yr - iyst) * 12 + *mm;
    rz[1] = indrz[num - 1];
    ig[1] = ionoindx[num - 1];
    midm = 15;
    if (*mm == 2) {
	midm = 14;
    }
    moda_(&c__0, yr, mm, &midm, &idd1, &nrdaym);
    if (*day < midm) {
	goto L1926;
    }
    imm2 = *mm + 1;
    if (imm2 > 12) {
	imm2 = 1;
	iyy2 = *yr + 1;
	idd2 = 380;
/*               if((yr/4*4.eq.yr).and.(yr/100*100.ne.yr)) idd2=381 */
	if (*yr / 4 << 2 == *yr) {
	    idd2 = 381;
	}
	if (*yr / 4 << 2 == *yr) {
	    idd2 = 381;
	}
    } else {
	iyy2 = *yr;
	midm = 15;
	if (imm2 == 2) {
	    midm = 14;
	}
	moda_(&c__0, &iyy2, &imm2, &midm, &idd2, &nrdaym);
    }
    rz[2] = indrz[num];
    ig[2] = ionoindx[num];
    *rsn = (*idn - idd1) * (float)1. / (idd2 - idd1);
    rz[3] = rz[1] + (rz[2] - rz[1]) * *rsn;
    ig[3] = ig[1] + (ig[2] - ig[1]) * *rsn;
    goto L1927;
L1926:
    imm2 = *mm - 1;
    if (imm2 < 1) {
	imm2 = 12;
	idd2 = -16;
	iyy2 = *yr - 1;
    } else {
	iyy2 = *yr;
	midm = 15;
	if (imm2 == 2) {
	    midm = 14;
	}
	moda_(&c__0, &iyy2, &imm2, &midm, &idd2, &nrdaym);
    }
    rz[2] = indrz[num - 2];
    ig[2] = ionoindx[num - 2];
    *rsn = (*idn - idd2) * (float)1. / (idd1 - idd2);
    rz[3] = rz[2] + (rz[1] - rz[2]) * *rsn;
    ig[3] = ig[2] + (ig[1] - ig[2]) * *rsn;
L1927:
    *nmonth = imm2;
    return 0;
} /* tcon_ */



/* Subroutine */ int lstid_(real *fi, integer *icez, real *r__, real *ae, 
	real *tm, real *sax, real *sux, real *ts70, real *df0f2, real *dhf2)
{
    /* Initialized data */

    static real y1[84] = { (float)150.,(float)250.,(float)207.8,(float)140.7,(
	    float)158.3,(float)87.2,(float)158.,(float)150.,(float)250.,(
	    float)207.8,(float)140.7,(float)158.3,(float)87.2,(float)158.,(
	    float)115.,(float)115.,(float)183.5,(float)144.2,(float)161.4,(
	    float)151.9,(float)272.4,(float)115.,(float)115.,(float)183.5,(
	    float)144.2,(float)161.4,(float)151.9,(float)272.4,(float)64.,(
	    float)320.,(float)170.6,(float)122.3,(float)139.,(float)79.6,(
	    float)180.6,(float)64.,(float)320.,(float)170.6,(float)122.3,(
	    float)139.,(float)79.6,(float)180.6,(float)72.,(float)84.,(float)
	    381.9,(float)20.1,(float)75.1,(float)151.2,(float)349.5,(float)
	    120.,(float)252.,(float)311.2,(float)241.,(float)187.4,(float)
	    230.1,(float)168.7,(float)245.,(float)220.,(float)294.7,(float)
	    181.2,(float)135.5,(float)237.7,(float)322.,(float)170.,(float)
	    110.,(float)150.2,(float)136.3,(float)137.4,(float)177.,(float)
	    114.,(float)170.,(float)314.,(float)337.8,(float)155.5,(float)
	    157.4,(float)196.7,(float)161.8,(float)100.,(float)177.,(float)
	    159.8,(float)165.6,(float)137.5,(float)132.2,(float)94.3 };
    static real y2[84] = { (float)2.5,(float)2.,(float)1.57,(float)2.02,(
	    float)2.12,(float)1.46,(float)2.46,(float)2.5,(float)2.,(float)
	    1.57,(float)2.02,(float)2.12,(float)1.46,(float)2.46,(float)2.3,(
	    float)1.6,(float)1.68,(float)1.65,(float)2.09,(float)2.25,(float)
	    2.82,(float)2.3,(float)1.6,(float)1.68,(float)1.65,(float)2.09,(
	    float)2.25,(float)2.82,(float).8,(float)2.,(float)1.41,(float)
	    1.57,(float)1.51,(float)1.46,(float)2.2,(float).8,(float)2.,(
	    float)1.41,(float)1.57,(float)1.51,(float)1.46,(float)2.2,(float)
	    3.7,(float)1.8,(float)3.21,(float)3.31,(float)2.61,(float)2.82,(
	    float)2.34,(float)2.8,(float)3.2,(float)3.32,(float)3.33,(float)
	    2.96,(float)3.43,(float)2.44,(float)3.5,(float)2.8,(float)2.37,(
	    float)2.79,(float)2.26,(float)3.4,(float)2.28,(float)3.9,(float)
	    2.,(float)2.22,(float)1.98,(float)2.33,(float)3.07,(float)1.56,(
	    float)3.7,(float)3.,(float)3.3,(float)2.99,(float)3.57,(float)
	    2.98,(float)3.02,(float)2.6,(float)2.8,(float)1.66,(float)2.04,(
	    float)1.91,(float)1.49,(float).43 };
    static real y3[84] = { (float)-1.8,(float)-1.9,(float)-1.42,(float)-1.51,(
	    float)-1.53,(float)-1.05,(float)-1.66,(float)-1.8,(float)-1.9,(
	    float)-1.42,(float)-1.51,(float)-1.53,(float)-1.05,(float)-1.66,(
	    float)-1.5,(float)-1.3,(float)-1.46,(float)-1.39,(float)-1.53,(
	    float)-1.59,(float)-1.9,(float)-1.5,(float)-1.3,(float)-1.46,(
	    float)-1.39,(float)-1.53,(float)-1.59,(float)-1.9,(float)-.7,(
	    float)-2.,(float)-1.41,(float)-1.09,(float)-1.22,(float)-.84,(
	    float)-1.32,(float)-.7,(float)-2.,(float)-1.41,(float)-1.09,(
	    float)-1.22,(float)-.84,(float)-1.32,(float)-1.7,(float)-1.,(
	    float)-2.08,(float)-1.8,(float)-1.35,(float)-1.55,(float)-1.79,(
	    float)-1.5,(float)-2.,(float)-2.08,(float)-2.16,(float)-1.86,(
	    float)-2.19,(float)-1.7,(float)-2.2,(float)-1.7,(float)-1.57,(
	    float)-1.62,(float)-1.19,(float)-1.89,(float)-1.47,(float)-1.9,(
	    float)-1.5,(float)-1.26,(float)-1.23,(float)-1.52,(float)-1.89,(
	    float)-1.02,(float)-1.7,(float)-1.7,(float)-1.76,(float)-1.43,(
	    float)-1.66,(float)-1.54,(float)-1.24,(float)-1.1,(float)-1.5,(
	    float)-1.09,(float)-1.23,(float)-1.11,(float)-1.14,(float)-.4 };
    static real y4[84] = { (float)-2.,(float)-5.,(float)-5.,(float)0.,(float)
	    0.,(float)0.,(float)2.,(float)-2.,(float)-5.,(float)-5.,(float)0.,
	    (float)0.,(float)0.,(float)2.,(float)-5.,(float)-5.,(float)6.,(
	    float)0.,(float)1.,(float)5.,(float)2.,(float)-5.,(float)-5.,(
	    float)6.,(float)0.,(float)1.,(float)5.,(float)2.,(float)0.,(float)
	    -7.,(float)-3.,(float)-6.,(float)2.,(float)2.,(float)3.,(float)0.,
	    (float)-7.,(float)-3.,(float)-6.,(float)2.,(float)2.,(float)3.,(
	    float)-5.,(float)-1.,(float)-11.,(float)-6.,(float)0.,(float)-5.,(
	    float)-6.,(float)-5.,(float)-10.,(float)1.,(float)4.,(float)-6.,(
	    float)-2.,(float)1.,(float)2.,(float)-13.,(float)-10.,(float)0.,(
	    float)-8.,(float)10.,(float)-16.,(float)0.,(float)-3.,(float)-7.,(
	    float)-2.,(float)-2.,(float)4.,(float)2.,(float)-11.,(float)-12.,(
	    float)-13.,(float)0.,(float)0.,(float)7.,(float)0.,(float)-8.,(
	    float)6.,(float)-1.,(float)-5.,(float)-7.,(float)4.,(float)-4. };
    static real y5[28] = { (float)0.,(float)0.,(float)-.1,(float)-.19,(float)
	    -.19,(float)-.25,(float)-.06,(float)0.,(float)0.,(float)-.31,(
	    float)-.28,(float)-.27,(float)-.06,(float).02,(float)0.,(float)0.,
	    (float).18,(float)-.07,(float)-.2,(float)-.1,(float).3,(float)0.,(
	    float)0.,(float)-.24,(float)-.5,(float)-.4,(float)-.27,(float)
	    -.48 };
    static real y6[28] = { (float)0.,(float)0.,(float)-3.5e-4,(float)-2.8e-4,(
	    float)-3.3e-4,(float)-2.3e-4,(float)-7e-4,(float)0.,(float)0.,(
	    float)-3e-4,(float)-2.5e-4,(float)-3e-4,(float)-6e-4,(float)
	    -7.3e-4,(float)0.,(float)0.,(float)-.0011,(float)-6e-4,(float)
	    -3e-4,(float)-5e-4,(float)-.0015,(float)0.,(float)0.,(float)-8e-4,
	    (float)-.003,(float)-2e-4,(float)-5e-4,(float)-3e-4 };

    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();
    double exp(doublereal), pow_dd(doublereal *, doublereal *);

    /* Local variables */
    static real a[84]	/* was [7][2][3][2] */, b[84]	/* was [7][2][3][2] */
	    , c__[84]	/* was [7][2][3][2] */, d__[84]	/* was [7][2][3][2] */
	    ;
    static integer i__, j, k, m, n;
    static real t, a1[28]	/* was [7][2][2] */, b1[28]	/* was [7][2][
	    2] */;
    static integer n1;
    static real ts, df1, df2, dh1, dh2;
    static integer inn;

    /* Fortran I/O blocks */
    static cilist io___493 = { 0, 6, 0, 0, 0 };
    static cilist io___495 = { 0, 6, 0, 0, 0 };


/* ***************************************************************** */
/*   COMPUTER PROGRAM FOR UPDATING FOF2 AND HMF2 FOR EFFECTS OF */
/*   THE LARGE SCALE SUBSTORM. */

/*   P.V.Kishcha, V.M.Shashunkina, E.E.Goncharova, Modelling of the */
/*   ionospheric effects of isolated and consecutive substorms on */
/*   the basis of routine magnetic data, Geomagn. and Aeronomy v.32, */
/*   N.3, 172-175, 1992. */

/*   P.V.Kishcha et al. Updating the IRI ionospheric model for */
/*   effects of substorms, Adv. Space Res.(in press) 1992. */

/*   Address: Dr. Pavel V. Kishcha, */
/*            Institute of Terrestrial Magnetism,Ionosphere and Radio */
/*            Wave Propagation, Russian Academy of Sciences, */
/*            142092, Troitsk, Moscow Region, Russia */

/* ***       INPUT PARAMETERS: */
/*       FI ------ GEOMAGNETIC LATITUDE, */
/*       ICEZ ---- INDEX OF SEASON(1-WINTER AND EQUINOX,2-SUMMER), */
/*       R ------- ZURICH SUNSPOT NUMBER, */
/*       AE ------ MAXIMUM AE-INDEX REACHED DURING SUBSTORM, */
/*       TM ------ LOCAL TIME, */
/*       SAX,SUX - TIME OF SUNSET AND SUNRISE, */
/*       TS70 ---- ONSET TIME (LT) OF SUBSTORMS ONSET */
/*                        STARTING ON FI=70 DEGR. */
/* ***      OUTPUT PARAMETERS: */
/*       DF0F2,DHF2- CORRECTIONS TO foF2 AND hmF2 FROM IRI OR */
/*                   OBSERVATIONAL MEDIAN  OF THOSE VALUES. */
/* ***************************************************************** */
    inn = 0;
    if (*ts70 > (float)12. && *tm < *sax) {
	inn = 1;
    }
    if (*fi < (float)0.) {
	*fi = dabs(*fi);
    }
    n = 0;
    for (m = 1; m <= 2; ++m) {
	for (k = 1; k <= 3; ++k) {
	    for (j = 1; j <= 2; ++j) {
		for (i__ = 1; i__ <= 7; ++i__) {
		    ++n;
		    a[i__ + (j + (k + m * 3 << 1)) * 7 - 64] = y1[n - 1];
		    b[i__ + (j + (k + m * 3 << 1)) * 7 - 64] = y2[n - 1];
		    c__[i__ + (j + (k + m * 3 << 1)) * 7 - 64] = y3[n - 1];
/* L1: */
		    d__[i__ + (j + (k + m * 3 << 1)) * 7 - 64] = y4[n - 1];
		}
	    }
	}
    }
    n1 = 0;
    for (m = 1; m <= 2; ++m) {
	for (j = 1; j <= 2; ++j) {
	    for (i__ = 1; i__ <= 7; ++i__) {
		++n1;
		a1[i__ + (j + (m << 1)) * 7 - 22] = y5[n1 - 1];
/* L2: */
		b1[i__ + (j + (m << 1)) * 7 - 22] = y6[n1 - 1];
	    }
	}
    }
    if (*fi > (float)65. || *ae < (float)500.) {
	s_wsle(&io___493);
	do_lio(&c__9, &c__1, "LSTID are for AE>500. and ABS(FI)<65.", (ftnlen)
		37);
	e_wsle();
	goto L4;
    }
    ts = *ts70 + (*fi * (float)-1.5571 + (float)109.) / (float)60.;
    if (ts < *sux && ts > *sax) {
	s_wsle(&io___495);
	do_lio(&c__9, &c__1, " LSTID are only at night", (ftnlen)24);
	e_wsle();
	goto L4;
    }
    if (inn == 1) {
	*tm += (float)24.;
    }
    if (ts >= *tm || ts < *tm - (float)5.) {
/*        WRITE(*,*)'LSTID are onli if  TM-5.<TS<TM ;Here TS=',TS,'TM=',TM */
	goto L4;
    }
    for (i__ = 1; i__ <= 7; ++i__) {
	if (*fi >= (i__ - 1) * (float)10. - (float)5. && *fi < (i__ - 1) * (
		float)10. + (float)5.) {
	    goto L8;
	}
/* L7: */
    }
L8:
    j = *icez;
    if (*ae >= (float)500. && *ae <= (float)755.) {
	k = 1;
    }
    if (*ae > (float)755. && *ae < (float)1e3) {
	k = 2;
    }
    if (*ae >= (float)1e3) {
	k = 3;
    }
    m = -1;
    if (*r__ <= (float)20.) {
	m = 1;
    }
    if (*r__ >= (float)120.) {
	m = 2;
    }
    t = *tm - ts;
    if (m < 0) {
	goto L3;
    }
/*        WRITE(*,*)'A1=',A1(I,J,M),' B1=',B1(I,J,M) */
/*        WRITE(*,*)'A=',A(I,J,K,M),' B=',B(I,J,K,M),' C=',C(I,J,K,M), */
/*     *'D=',D(I,J,K,M) */
    *df0f2 = a1[i__ + (j + (m << 1)) * 7 - 22] + b1[i__ + (j + (m << 1)) * 7 
	    - 22] * *ae;
    d__1 = (doublereal) t;
    d__2 = (doublereal) b[i__ + (j + (k + m * 3 << 1)) * 7 - 64];
    *dhf2 = a[i__ + (j + (k + m * 3 << 1)) * 7 - 64] * pow_dd(&d__1, &d__2) * 
	    exp(c__[i__ + (j + (k + m * 3 << 1)) * 7 - 64] * t) + d__[i__ + (
	    j + (k + m * 3 << 1)) * 7 - 64];
    goto L5;
L3:
    df1 = a1[i__ + (j + 2) * 7 - 22] + b1[i__ + (j + 2) * 7 - 22] * *ae;
    df2 = a1[i__ + (j + 4) * 7 - 22] + b1[i__ + (j + 4) * 7 - 22] * *ae;
    *df0f2 = df1 + (df2 - df1) * (*r__ - (float)20.) / (float)100.;
    d__1 = (doublereal) t;
    d__2 = (doublereal) b[i__ + (j + (k + 3 << 1)) * 7 - 64];
    dh1 = a[i__ + (j + (k + 3 << 1)) * 7 - 64] * pow_dd(&d__1, &d__2) * exp(
	    c__[i__ + (j + (k + 3 << 1)) * 7 - 64] * t) + d__[i__ + (j + (k + 
	    3 << 1)) * 7 - 64];
    d__1 = (doublereal) t;
    d__2 = (doublereal) b[i__ + (j + (k + 6 << 1)) * 7 - 64];
    dh2 = a[i__ + (j + (k + 6 << 1)) * 7 - 64] * pow_dd(&d__1, &d__2) * exp(
	    c__[i__ + (j + (k + 6 << 1)) * 7 - 64] * t) + d__[i__ + (j + (k + 
	    6 << 1)) * 7 - 64];
    *dhf2 = dh1 + (dh2 - dh1) * (*r__ - (float)20.) / (float)100.;
    goto L5;
L4:
    *dhf2 = (float)0.;
    *df0f2 = (float)0.;
L5:
    if (inn == 1) {
	*tm += (float)-24.;
    }
    return 0;
} /* lstid_ */

#ifdef __cplusplus
	}
#endif
