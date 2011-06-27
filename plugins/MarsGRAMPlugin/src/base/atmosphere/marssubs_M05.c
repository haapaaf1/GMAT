/* marssubs_M05.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Common Block Declarations */

struct {
    doublereal f107, stdl, fmol[9];
} therm_m05__;

#define therm_m05__1 therm_m05__

struct {
    doublereal zc[164], tc[164], pc[164], dc[164];
} cosparnh_m05__;

#define cosparnh_m05__1 cosparnh_m05__

struct {
    doublereal dtr, day, dustlat, dustlon, radmax, rref, als0, alsdur, intens,
	     deltatex, rpscale, dusttau, dustmin, dustmax, dustod, dustnu, 
	    dustdiam, dustdens, rwscale, wlscale, requa, rpole, wmscale, 
	    blwinfac;
    integer mapyear, idaydata, npos, nvarx, nvary, logscale, iu0, iup, ipclat,
	     molahgts;
} datacom_m05__;

#define datacom_m05__1 datacom_m05__

struct {
    char lstfl[60], outfl[60];
} filename_m05__;

#define filename_m05__1 filename_m05__

struct {
    doublereal wavea0, wavea1, wavephi1, wavea2, wavephi2, wavea3, wavephi3, 
	    wavetime[100], wavedata[1100]	/* was [100][11] */, wscale, 
	    phi1dot, phi2dot, phi3dot, wavedate;
    integer nwave, iuwave;
} wavecoef_m05__;

#define wavecoef_m05__1 wavecoef_m05__

struct {
    doublereal offsets[39]	/* was [13][3] */, toffsets[26]	/* was [13][2]
	     */, zoffset, hgtoffset, ofszl;
    integer ibougher;
} tgcmoffset_m05__;

#define tgcmoffset_m05__1 tgcmoffset_m05__

struct {
    doublereal tza0[16575]	/* was [17][25][13][3] */, tza1[16575]	/* 
	    was [17][25][13][3] */, tzp1[16575]	/* was [17][25][13][3] */, 
	    tza2[16575]	/* was [17][25][13][3] */, tzp2[16575]	/* was [17][
	    25][13][3] */, pza0[16575]	/* was [17][25][13][3] */, pza1[16575]
	    	/* was [17][25][13][3] */, pzp1[16575]	/* was [17][25][13][3]
	     */, pza2[16575]	/* was [17][25][13][3] */, pzp2[16575]	/* 
	    was [17][25][13][3] */, dza0[16575]	/* was [17][25][13][3] */, 
	    uza0[16575]	/* was [17][25][13][3] */, uza1[16575]	/* was [17][
	    25][13][3] */, uzp1[16575]	/* was [17][25][13][3] */, uza2[16575]
	    	/* was [17][25][13][3] */, uzp2[16575]	/* was [17][25][13][3]
	     */, vza0[16575]	/* was [17][25][13][3] */, vza1[16575]	/* 
	    was [17][25][13][3] */, vzp1[16575]	/* was [17][25][13][3] */, 
	    vza2[16575]	/* was [17][25][13][3] */, vzp2[16575]	/* was [17][
	    25][13][3] */;
} mgcmdata_m05__;

#define mgcmdata_m05__1 mgcmdata_m05__

struct {
    doublereal dlat, dlon, dls, ddust, dlatw, dlatt, df10, wpolefac, tpolefac;
    integer ilat, jlon, ls, mdust, k1st, ilatw, ilatt, mf10;
} interp_m05__;

#define interp_m05__1 interp_m05__

struct {
    doublereal dust[3], dzbl[3], zwsfc, f10val[2], f10tes[3];
    char dustc[6], solact[4], tesyr[4], soltes[6];
} mgcmparm_m05__;

#define mgcmparm_m05__1 mgcmparm_m05__

struct {
    integer ix, iy, iz;
} randcom_m05__;

#define randcom_m05__1 randcom_m05__

struct {
    doublereal tsa0[119925]	/* was [3][25][41][13][3] */, tsa1[119925]	
	    /* was [3][25][41][13][3] */, tsp1[119925]	/* was [3][25][41][13]
	    [3] */, tsa2[119925]	/* was [3][25][41][13][3] */, tsp2[
	    119925]	/* was [3][25][41][13][3] */, usa0[119925]	/* 
	    was [3][25][41][13][3] */, usa1[119925]	/* was [3][25][41][13]
	    [3] */, usp1[119925]	/* was [3][25][41][13][3] */, usa2[
	    119925]	/* was [3][25][41][13][3] */, usp2[119925]	/* 
	    was [3][25][41][13][3] */, vsa0[119925]	/* was [3][25][41][13]
	    [3] */, vsa1[119925]	/* was [3][25][41][13][3] */, vsp1[
	    119925]	/* was [3][25][41][13][3] */, vsa2[119925]	/* 
	    was [3][25][41][13][3] */, vsp2[119925]	/* was [3][25][41][13]
	    [3] */;
} surfdata_m05__;

#define surfdata_m05__1 surfdata_m05__

struct {
    doublereal tta0[53352]	/* was [19][36][13][3][2] */, tta1[53352]	
	    /* was [19][36][13][3][2] */, ttp1[53352]	/* was [19][36][13][3]
	    [2] */, tta2[53352]	/* was [19][36][13][3][2] */, ttp2[53352]	
	    /* was [19][36][13][3][2] */, pta0[53352]	/* was [19][36][13][3]
	    [2] */, pta1[53352]	/* was [19][36][13][3][2] */, ptp1[53352]	
	    /* was [19][36][13][3][2] */, pta2[53352]	/* was [19][36][13][3]
	    [2] */, ptp2[53352]	/* was [19][36][13][3][2] */, dta0[53352]	
	    /* was [19][36][13][3][2] */, dta1[53352]	/* was [19][36][13][3]
	    [2] */, dtp1[53352]	/* was [19][36][13][3][2] */, dta2[53352]	
	    /* was [19][36][13][3][2] */, dtp2[53352]	/* was [19][36][13][3]
	    [2] */, uta0[53352]	/* was [19][36][13][3][2] */, uta1[53352]	
	    /* was [19][36][13][3][2] */, utp1[53352]	/* was [19][36][13][3]
	    [2] */, uta2[53352]	/* was [19][36][13][3][2] */, utp2[53352]	
	    /* was [19][36][13][3][2] */, vta0[53352]	/* was [19][36][13][3]
	    [2] */, vta1[53352]	/* was [19][36][13][3][2] */, vtp1[53352]	
	    /* was [19][36][13][3][2] */, vta2[53352]	/* was [19][36][13][3]
	    [2] */, vtp2[53352]	/* was [19][36][13][3][2] */, zfa0[2808]	
	    /* was [36][13][3][2] */, zfa1[2808]	/* was [36][13][3][2] 
	    */, zfp1[2808]	/* was [36][13][3][2] */, zfa2[2808]	/* 
	    was [36][13][3][2] */, zfp2[2808]	/* was [36][13][3][2] */;
} tgcmdata_m05__;

#define tgcmdata_m05__1 tgcmdata_m05__

struct {
    doublereal areorad[261364]	/* was [362][722] */, topomola[261364]	/* 
	    was [362][722] */, albedo[65884]	/* was [182][362] */;
} terhgt_m05__;

#define terhgt_m05__1 terhgt_m05__

struct {
    doublereal testau[147600]	/* was [2][72][25][41] */;
} tesdust_m05__;

#define tesdust_m05__1 tesdust_m05__

/* Table of constant values */

static doublereal c_b2 = 0.;
static doublereal c_b7 = 1.;
static doublereal c_b8 = -1.;
static doublereal c_b11 = .25;
static doublereal c_b12 = -.5;
static doublereal c_b17 = 90.;
static integer c__5 = 5;
static integer c__1 = 1;
static doublereal c_b35 = 180.;
static doublereal c_b80 = -1.253;
static integer c__9 = 9;
static integer c__0 = 0;
static integer c__2 = 2;
static integer c__17 = 17;
static integer c__3 = 3;
static integer c__6 = 6;
static doublereal c_b619 = .0525;
static doublereal c_b624 = .2;
static integer c__4 = 4;
static integer c_b797 = 147600;

/*     Subroutines and Functions for                                     ATM2  1 */
/*     Mars-GRAM 2005 - Version 1.3,  Sep, 2009                          ATM2  2 */
/* -----------------------------------------------------------------------ATM2  3 */
/*                                                                       ATM2  4 */
/* Subroutine */ int atmos2_m05__(doublereal *hgtin, doublereal *clat, 
	doublereal *clon, doublereal *marsau, doublereal *sunlat, doublereal *
	sunlon, doublereal *als, doublereal *h__, doublereal *temp, 
	doublereal *denst, doublereal *upfctr, doublereal *lwfctr, doublereal 
	*pres, doublereal *thgt, doublereal *careoid, doublereal *zf, integer 
	*iu0, doublereal *deltatex, doublereal *texos, doublereal *tbase, 
	doublereal *hrho, doublereal *amz, doublereal *dusttau, doublereal *
	dustmin, doublereal *dustmax, doublereal *dustod, doublereal *ewwind, 
	doublereal *nswind, doublereal *blwindew, doublereal *blwindns, 
	doublereal *blwindvert, doublereal *hatzf, doublereal *wavepert, 
	doublereal *tempday, doublereal *presday, doublereal *densday, 
	doublereal *ewwnday, doublereal *nswnday, doublereal *bluday, 
	doublereal *blvday, doublereal *hgtasfc, doublereal *patsurf, 
	doublereal *tempmax, doublereal *tempmin, doublereal *densmax, 
	doublereal *densmin, doublereal *tgrnd, doublereal *talb, integer *
	icepolar, doublereal *gz, doublereal *oldrref, doublereal *requa, 
	doublereal *rpole, integer *mapyear, doublereal *profnear, doublereal 
	*proffar, integer *nprof, doublereal *profwgt, integer *idaydata)
{
    /* Initialized data */

    static doublereal rstar = 8314.472;
    static doublereal amw = 43.49;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double atan(doublereal), log(doublereal), sqrt(doublereal);

    /* Local variables */
    static doublereal tempday1;
    extern doublereal dustvsls_m05__(doublereal *, doublereal *, doublereal *)
	    ;
    static doublereal presday1, tempmin1, tempmax1, ewwnday1, amhi, nswnday1, 
	    chgt;
    static integer icex;
    static doublereal amlo;
    static integer icez;
    static doublereal hrhi, time, tinf, rgas, hrlo, shgt, ublx, vblx, htop;
    extern /* Subroutine */ int tesod_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal wblz, wblx, dout, pout, topz, tout, uout, vout, 
	    tgradtop, hrho1, temp1, tat5m, pres1;
    extern doublereal cp_m05__(doublereal *);
    static doublereal tgrad, ddtex, talbx, ddayz, bludx, blvdx, dmaxz, dminz, 
	    talbz, pdayz;
    extern /* Subroutine */ int tesgcm_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     doublereal *, doublereal *, integer *, integer *);
    static doublereal dmaxx, hrhoz, dustm, tdayz, udayz, tempz, vdayz, tminz, 
	    pitwo, presz, blwuz, blwvz, blwwz, tmaxz, tmaxx, tminx, dminx, h1,
	     amtop, stewfdens, denst1, tgrnd1, stewfpres, ewwnd1, nswnd1, tf, 
	    dx, hz, px, denshi, ux, vx, tgradm, tgradp, denslo, temphi;
    extern /* Subroutine */ int marsgcm_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *, doublereal *, doublereal *, integer *
	    );
    static doublereal preshi, templo, preslo, denstz, tgrndz, tgrdnx;
    extern /* Subroutine */ int rellips_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal tgrndx, zf1, bluday1, blvday1, dustoffset, hhi, flh, 
	    gcp, ddx, hlo, hdx;
    static integer loh;
    static doublereal gor, upf, pdx;
    extern /* Subroutine */ int stewart2_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal zfp, udx, tdz, vdx, tdx, zfz, areoidx, tfactor;
    static integer ice1;
    static doublereal wavemax, denstop;
    extern /* Subroutine */ int dustfact_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal hrhotop, temptop, amz1, prestop, tophgtx;
    extern /* Subroutine */ int profterp_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *, doublereal *,
	     doublereal *, doublereal *, doublereal *);
    static doublereal stormdz, densday1, densmin1, densmax1;

/*                                                                       ATM2 15 */
/*     HGTIN    Planeto-Centric HEIGHT ABOVE MOLA AREOID (KM)(INPUT)     ATM2 16 */
/*     CLAT     Planeto-Centric LATITUDE (DEGREES) (INPUT)               ATM2 17 */
/*     CLON     WEST LONGITUDE OF SPACECRAFT (DEGREES) (INPUT)           ATM2 18 */
/*     MARSAU   MARS ORBITAL RADIUS (AU) (INPUT)                         ATM2 19 */
/*     SUNLAT   AREOCENTRIC LATITUDE OF SUN (DEGREES) (INPUT)            ATM2 20 */
/*     SUNLON   MARS WEST LONGITUDE OF SUN (DEGREES) (INPUT)             ATM2 21 */
/*     ALS      AREOCENTRIC LONGITUDE OF SUN ORBIT (INPUT)               ATM2 22 */
/*     H        SCALE HEIGHT AT SPACECRAFT POSITION (KM) (OUTPUT)        ATM2 23 */
/*     TEMP     TEMPERATURE AT SPACECRAFT POSITION (K) (OUTPUT)          ATM2 24 */
/*     DENST    MASS DENSITY AT SPACECRAFT POSITION (KG/M**3) (OUTPUT)   ATM2 25 */
/*     UPFCTR   UPPER DEVIATION FACTOR ON MASS DENSITY  (OUTPUT)         ATM2 26 */
/*     LWFCTR   LOWER DEVIATION FACTOR ON MASS DENSITY  (OUTPUT)         ATM2 27 */
/*     PRES     PRESSURE AT SPACECRAFT POSITION (N/M**2) (OUTPUT)        ATM2 28 */
/*     thgt     LOCAL SURFACE HEIGHT RELATIVE TO MOLA AREOID (KM)        ATM2 29 */
/*              (INPUT)                                                  ATM2 30 */
/*     careoid  MOLA 1/2 degree areoid (km) (OUTPUT)                     ATM2 31 */
/*     ZF       Local height of base of thermosphere (OUTPUT)            ATM2 32 */
/*     iu0      unit number for messages (ONPUT)                         ATM2 33 */
/*     deltaTEX adjustment in exospheric temperature (K) (INPUT)         ATM2 34 */
/*     Texos    local exospheric temperature (K) (OUTPUT)                ATM2 35 */
/*     Tbase    local temperature for base of exosphere (K) (OUTPUT)     ATM2 36 */
/*     Hrho     density scale height (km) (OUTPUT)                       ATM2 37 */
/*     AMz      molecular weight (OUTPUT)                                ATM2 38 */
/*     Dusttau  dust optical depth (or 0 for assumed seasonal            ATM2 39 */
/*               variation) (INPUT)                                      ATM2 40 */
/*     Dustmin  minimum optical depth for seasonal variation (INPUT)     ATM2 41 */
/*     Dustmax  maximum optical depth for seasonal variation (INPUT)     ATM2 42 */
/*     DustOD   dust optical depth (OUTPUT)                              ATM2 43 */
/*     EWWIND   Eastward wind component (m/s) (OUTPUT)                   ATM2 44 */
/*     NSWIND   Northward wind component (m/s) (OUTPUT)                  ATM2 45 */
/*     blwindew Eastward b.l. slope wind (m/s) (OUTPUT)                  ATM2 46 */
/*     blwindns Northward b.l. slope wind (m/s) (OUTPUT)                 ATM2 47 */
/*     HatZF    pressure scale height at altitude ZF (km) (OUTPUT)       ATM2 48 */
/*     wavepert perturbation (% of mean) from longitude-dependent wave   ATM2 49 */
/*     TempDay  daily average temperature (K) (OUTPUT)                   ATM2 50 */
/*     PresDay  daily average pressure (N/m**2) (OUTPUT)                 ATM2 51 */
/*     DensDay  daily average density (kg/m**3) (OUTPUT)                 ATM2 52 */
/*     EWwnDay  daily average eastward wind component (m/s) (OUTPUT)     ATM2 53 */
/*     NSwnDay  dailt average northward wind component (m/s) (OUTPUT)    ATM2 54 */
/*     hgtasfc  height above surface for evaluation of boundary layer    ATM2 55 */
/*               variables (km) (INPUT)                                  ATM2 56 */
/*     Patsurf  atmospheric pressure at surface (N/m**2) (OUTPUT)        ATM2 57 */
/*     Tempmax  daily maximum temperature (K) (OUTPUT)                   ATM2 58 */
/*     Tempmin  daily minimum temperature (K) (OUTPUT)                   ATM2 59 */
/*     Densmax  daily maximum density (kg/m**3) (OUTPUT)                 ATM2 60 */
/*     Densmin  daily minimum density (kg/m**3) (OUTPUT)                 ATM2 61 */
/*     Tgrnd    ground surface temperature (K) (OUTPUT)                  ATM2 62 */
/*     talb     surface albedo (OUTPUT)                                  ATM2 63 */
/*     icepolar polar ice indicator (0=no, 1=yes) (OUTPUT)               ATM2 64 */
/*     gz       gravity (m/s**2) at input height (OUTPUT)                ATM2 65 */
/*     Oldrref  local radius (km) of reference ellipsoid (OUTPUT)        ATM2 66 */
/*     requa    equatorial radius (km) of reference ellipsoid (INPUT)    ATM2 67 */
/*     rpole    polar radius (km) of reference ellipsoid (INPUT)         ATM2 68 */
/*                                                                       ATM2 69 */
/* ---------------------------------------------------------------------  ATM2 70 */
/*                                                                       ATM2 71 */
/* ---  Min and max perturbation factors in thermosphere                  ATM2 72 */
/* ---  Rstar = Universal gas constant                                    ATM2 75 */
/* ---  AMW = average molecular weight up to turbopause                   ATM2 76 */
/* ---  pi/2                                                              ATM2 78 */
    pitwo = 2. * atan(1.);
/* ---  Top height = 170 km for MapYear=0 otherwise Top height = 240 km   ATM2 80 */
    topz = 170.;
    if (*mapyear > 0) {
	topz = 240.;
    }
/* ---  Set slope winds to zero                                           ATM2 83 */
    *blwindew = 0.;
    *blwindns = 0.;
/* ---  Local solar time (Mars hours = 1/24th Sol)                        ATM2 86 */
    time = (*sunlon - *clon) / 15. + 12.;
    if (time < 0.) {
	time += 24.;
    }
    if (time > 24.) {
	time += -24.;
    }
    if (*mapyear > 0) {
/* ---    Evaluate dust from Mapping Year 1 or 2 input file               ATM2 91 */
	tesod_m05__(mapyear, als, clat, clon, dustod);
    } else {
/* ---    Evaluate dust optical depth from NameList input parameters      ATM2 94 */
	if (*dusttau > 0.) {
	    *dustod = *dusttau;
	    if (*dustod < .1) {
		*dustod = .1;
	    }
	} else {
	    *dustod = dustvsls_m05__(als, dustmin, dustmax);
	}
    }
/* ---  Evaluate factor for dust storm model                              ATM2102 */
    dustfact_m05__(clat, clon, als, &dustm, &stormdz);
    *dustod += dustm;
/* ---  Evaluate MTGCM height offset due to dust storm                    ATM2105 */
    dustoffset = stormdz;
/* ---  Get areoid radius and topographic height at current lat, lon      ATM2107 */
    rellips_m05__(clat, clon, careoid, hgtin, gz, oldrref, thgt, talb, requa, 
	    rpole);
/* ---  Evaluate pressure and temperature at surface, Patsurf and Tat5m   ATM2110 */
    tat5m = 0.;
    if (*mapyear == 0) {
	marsgcm_m05__(thgt, clat, clon, als, dustod, &time, &tempz, &presz, &
		denstz, ewwind, nswind, &blwuz, &blwvz, &blwwz, &hz, &hrhoz, &
		zfz, &tfactor, thgt, hgtasfc, careoid, &tdayz, &pdayz, &ddayz,
		 &udayz, &vdayz, &bludx, &blvdx, &tmaxz, &tminz, &dmaxz, &
		dminz, &tgrndz, &talbz, &icez, &tat5m, &dustoffset, requa, 
		rpole, idaydata);
    } else {
	tesgcm_m05__(thgt, clat, clon, als, &time, &tempz, &presz, &denstz, 
		ewwind, nswind, &blwuz, &blwvz, &blwwz, &hz, &hrhoz, &zfz, &
		tfactor, thgt, hgtasfc, careoid, &tdayz, &pdayz, &ddayz, &
		udayz, &vdayz, &bludx, &blvdx, &tmaxz, &tminz, &dmaxz, &dminz,
		 &tgrndz, &talbz, &icez, &tat5m, requa, rpole, mapyear, 
		idaydata);
    }
    *patsurf = presz;
    *tgrnd = tgrndz;
/* ---  Adjust Patsurf for wave perturbation                              ATM2127 */
    *patsurf *= *wavepert + 1.;
/* ---  Tat5m is air temperature at surface + 5 meters                    ATM2129 */
/* ---  Set evaluation hgt (CHGT) to HGTIN or to thgt+hgtasfc if HGTIN <= ATM2130 */
/*     -8.7 km                                                           ATM2131 */
    chgt = *hgtin;
    if (*hgtin <= -8.7) {
	chgt = *thgt + *hgtasfc;
    }
/* ---  Compute atmosphere using only MGCM data if height below 80 km     ATM2136 */
    if (chgt <= 80.) {
	if (*mapyear == 0) {
	    marsgcm_m05__(&chgt, clat, clon, als, dustod, &time, temp, pres, 
		    denst, ewwind, nswind, blwindew, blwindns, blwindvert, 
		    h__, hrho, zf, &tfactor, thgt, hgtasfc, careoid, tempday, 
		    presday, densday, ewwnday, nswnday, bluday, blvday, 
		    tempmax, tempmin, densmax, densmin, &tgrndz, talb, 
		    icepolar, &tat5m, &dustoffset, requa, rpole, idaydata);
	} else {
	    tesgcm_m05__(&chgt, clat, clon, als, &time, temp, pres, denst, 
		    ewwind, nswind, blwindew, blwindns, blwindvert, h__, hrho,
		     zf, &tfactor, thgt, hgtasfc, careoid, tempday, presday, 
		    densday, ewwnday, nswnday, bluday, blvday, tempmax, 
		    tempmin, densmax, densmin, &tgrndz, talb, icepolar, &
		    tat5m, requa, rpole, mapyear, idaydata);
	}
/* ---    Adjust PRES, and DENST for wave perturbation                    ATM2153 */
	*pres *= *wavepert + 1.;
	*denst *= *wavepert + 1.;
	*densday *= *wavepert + 1.;
	*presday *= *wavepert + 1.;
	if (*idaydata > 0) {
	    *densmax *= *wavepert + 1.;
	    *densmin *= *wavepert + 1.;
	}
	*hatzf = 0.;
	*upfctr = tfactor + 1.;
	*lwfctr = 0.;
	*amz = amw;
    } else if (chgt <= topz) {
/* ---    This section uses MTGCM for density, pressure, temperature and  ATM2166 */
/*       Stewart model for mixing ratios                                 ATM2167 */
/* ---    Evaluate MTGCM at current height                                ATM2168 */
	if (*mapyear == 0) {
	    marsgcm_m05__(&chgt, clat, clon, als, dustod, &time, temp, pres, 
		    denst, ewwind, nswind, blwindew, blwindns, blwindvert, 
		    h__, hrho, zf, &tfactor, thgt, hgtasfc, careoid, tempday, 
		    presday, densday, ewwnday, nswnday, bluday, blvday, 
		    tempmax, tempmin, densmax, densmin, &tgrndz, talb, 
		    icepolar, &tat5m, &dustoffset, requa, rpole, idaydata);
	} else {
	    tesgcm_m05__(&chgt, clat, clon, als, &time, temp, pres, denst, 
		    ewwind, nswind, blwindew, blwindns, blwindvert, h__, hrho,
		     zf, &tfactor, thgt, hgtasfc, careoid, tempday, presday, 
		    densday, ewwnday, nswnday, bluday, blvday, tempmax, 
		    tempmin, densmax, densmin, &tgrndz, talb, icepolar, &
		    tat5m, requa, rpole, mapyear, idaydata);
	}
/* ---    Adjust PRES, and DENST for wave perturbation                    ATM2184 */
	*pres *= *wavepert + 1.;
	*denst *= *wavepert + 1.;
	*densday *= *wavepert + 1.;
	*presday *= *wavepert + 1.;
	if (*idaydata > 0) {
	    *densmax *= *wavepert + 1.;
	    *densmin *= *wavepert + 1.;
	}
/* ---    Compute molecular weight                                        ATM2192 */
	*amz = *denst * rstar * *temp / *pres;
/* ---    Find temperature TF at height ZF = altitude of 1.26 nbar level  ATM2194 */
	if (*zf > 900.) {
	    tf = 999.9;
	} else {
	    if (*mapyear == 0) {
		marsgcm_m05__(zf, clat, clon, als, dustod, &time, &tf, &px, &
			dx, &ux, &vx, &ublx, &vblx, &wblz, hatzf, &hdx, &zfp, 
			&upf, &tophgtx, hgtasfc, &areoidx, &tdz, &pdx, &ddx, &
			udx, &vdx, &bludx, &blvdx, &tmaxx, &tminx, &dmaxx, &
			dminx, &tgrdnx, &talbx, &icex, &tat5m, &dustoffset, 
			requa, rpole, idaydata);
	    } else {
		tesgcm_m05__(zf, clat, clon, als, &time, &tf, &px, &dx, &ux, &
			vx, &ublx, &vblx, &wblz, hatzf, &hdx, &zfp, &upf, &
			tophgtx, hgtasfc, &areoidx, &tdz, &pdx, &ddx, &udx, &
			vdx, &bludx, &blvdx, &tmaxx, &tminx, &dmaxx, &dminx, &
			tgrdnx, &talbx, &icex, &tat5m, requa, rpole, mapyear, 
			idaydata);
	    }
/* ---      Adjust ZF for wave perturbation                               ATM2211 */
	    *zf += *hatzf * log(*wavepert + 1.);
	}
/* ---    Save temperature at 1.26 nbar level                             ATM2214 */
	*tbase = tf;
	*upfctr = tfactor + 1.;
	*lwfctr = 1. - tfactor;
/* ---    Use MTGCM data to get (unperturbed) values at TopZ km           ATM2218 */
	if (*mapyear == 0) {
	    marsgcm_m05__(&topz, clat, clon, als, dustod, &time, &temp1, &
		    pres1, &denst1, &ewwnd1, &nswnd1, blwindew, blwindns, 
		    blwindvert, &h1, &hrho1, &zf1, &tfactor, thgt, hgtasfc, 
		    careoid, &tempday1, &presday1, &densday1, &ewwnday1, &
		    nswnday1, &bluday1, &blvday1, &tempmax1, &tempmin1, &
		    densmax1, &densmin1, &tgrnd1, talb, &ice1, &tat5m, &
		    dustoffset, requa, rpole, idaydata);
	} else {
	    tesgcm_m05__(&topz, clat, clon, als, &time, &temp1, &pres1, &
		    denst1, &ewwnd1, &nswnd1, blwindew, blwindns, blwindvert, 
		    &h1, &hrho1, &zf1, &tfactor, thgt, hgtasfc, careoid, &
		    tempday1, &presday1, &densday1, &ewwnday1, &nswnday1, &
		    bluday1, &blvday1, &tempmax1, &tempmin1, &densmax1, &
		    densmin1, &tgrnd1, talb, &ice1, &tat5m, requa, rpole, 
		    mapyear, idaydata);
	}
/* ---    Evaluate Stewart thermosphere at TopZ for deltaTEX adjustment   ATM2234 */
	shgt = topz;
	stewart2_m05__(marsau, clat, clon, &time, &prestop, &temptop, &
		denstop, &shgt, &rstar, &htop, &amtop, &c_b2, iu0, sunlat, 
		deltatex, &tinf, &tf, &zf1, &hrhotop, requa, rpole, &tgradtop)
		;
/* ---    Adjust deltaTEX for temperature difference at TopZ              ATM2239 */
	ddtex = *deltatex + temp1 - temptop;
	shgt = chgt;
	if (chgt < *zf) {
	    shgt = *zf;
	}
/* ---    Evaluate thermospheric parameters at current height             ATM2243 */
	stewart2_m05__(marsau, clat, clon, &time, &pres1, &temp1, &denst1, &
		shgt, &rstar, &h1, &amz1, &c_b2, iu0, sunlat, &ddtex, &tinf, &
		tf, zf, &hrho1, requa, rpole, &tgrad);
/* ---    Save exospheric temperature                                     ATM2247 */
	*texos = tinf;
    } else {
/* ---    For height above TopZ -  Use MTGCM data to get (unperturbed)    ATM2250 */
/*       values at TopZ                                                  ATM2251 */
	if (*mapyear == 0) {
	    marsgcm_m05__(&topz, clat, clon, als, dustod, &time, &temp1, &
		    pres1, &denst1, ewwind, nswind, blwindew, blwindns, 
		    blwindvert, &h1, &hrho1, zf, &tfactor, thgt, hgtasfc, 
		    careoid, &tempday1, &presday1, &densday1, &ewwnday1, &
		    nswnday1, &bluday1, &blvday1, &tempmax1, &tempmin1, &
		    densmax1, &densmin1, &tgrnd1, talb, &ice1, &tat5m, &
		    dustoffset, requa, rpole, idaydata);
	} else {
	    tesgcm_m05__(&topz, clat, clon, als, &time, &temp1, &pres1, &
		    denst1, ewwind, nswind, blwindew, blwindns, blwindvert, &
		    h1, &hrho1, zf, &tfactor, thgt, hgtasfc, careoid, &
		    tempday1, &presday1, &densday1, &ewwnday1, &nswnday1, &
		    bluday1, &blvday1, &tempmax1, &tempmin1, &densmax1, &
		    densmin1, &tgrnd1, talb, &ice1, &tat5m, requa, rpole, 
		    mapyear, idaydata);
	}
/* ---    Find temperature TF at height ZF  = height of 1.26 nbar level   ATM2267 */
	if (*mapyear == 0) {
	    marsgcm_m05__(zf, clat, clon, als, dustod, &time, &tf, &px, &dx, &
		    ux, &vx, &ublx, &vblx, &wblx, hatzf, &hdx, &zfp, &upf, &
		    tophgtx, hgtasfc, &areoidx, &tdx, &pdx, &ddx, &udx, &vdx, 
		    &bludx, &blvdx, &tmaxx, &tminx, &dmaxx, &dminx, &tgrndx, &
		    talbx, &icex, &tat5m, &dustoffset, requa, rpole, idaydata)
		    ;
	} else {
	    tesgcm_m05__(zf, clat, clon, als, &time, &tf, &px, &dx, &ux, &vx, 
		    &ublx, &vblx, &wblx, hatzf, &hdx, &zfp, &upf, &tophgtx, 
		    hgtasfc, &areoidx, &tdx, &pdx, &ddx, &udx, &vdx, &bludx, &
		    blvdx, &tmaxx, &tminx, &dmaxx, &dminx, &tgrndx, &talbx, &
		    icex, &tat5m, requa, rpole, mapyear, idaydata);
	}
/* ---    Evaluate Stewart thermosphere at TopZ for deltaTEX adjustment   ATM2280 */
	shgt = topz;
	stewart2_m05__(marsau, clat, clon, &time, &prestop, &temptop, &
		denstop, &shgt, &rstar, &htop, &amtop, &c_b2, iu0, sunlat, 
		deltatex, &tinf, &tf, zf, &hrhotop, requa, rpole, &tgradtop);
/* ---    Adjust deltaTEX for temperature difference at TopZ              ATM2285 */
	ddtex = *deltatex + temp1 - temptop;
/* ---    Adjust ZF for wave perturbation, using scale height at ZF       ATM2286a */
	*zf += *hatzf * log(*wavepert + 1.);
/* ---    Evaluate Stewart thermosphere at TopZ for adjustment factors    ATM2287 */
	stewart2_m05__(marsau, clat, clon, &time, &prestop, &temptop, &
		denstop, &shgt, &rstar, &htop, &amtop, &c_b2, iu0, sunlat, &
		ddtex, &tinf, &tf, zf, &hrhotop, requa, rpole, &tgradtop);
/* ---    Set daily average values to zero above TopZ                     ATM2291 */
	*tempday = 0.;
	*presday = 0.;
	*densday = 0.;
	*ewwnday = 0.;
	*nswnday = 0.;
/* ---    Set daily max, min Temp and Density to zero above TopZ          ATM2297 */
	*tempmax = 0.;
	*tempmin = 0.;
	*densmax = 0.;
	*densmin = 0.;
/* ---    Save temperature at 1.26 nbar level                             ATM2304 */
	*tbase = tf;
	shgt = chgt;
/* ---    Evaluate thermospheric parameters at current height             ATM2307 */
	stewart2_m05__(marsau, clat, clon, &time, pres, temp, denst, &shgt, &
		rstar, h__, amz, &c_b2, iu0, sunlat, &ddtex, &tinf, &tf, zf, 
		hrho, requa, rpole, &tgrad);
/* ---    Save exospheric temperature                                     ATM2311 */
	*texos = tinf;
/* ---    Evaluate thermospheric parameters at deviation=+1, for DENSHI   ATM2313 */
	stewart2_m05__(marsau, clat, clon, &time, &preshi, &temphi, &denshi, &
		shgt, &rstar, &hhi, &amhi, &c_b7, iu0, sunlat, &ddtex, &tinf, 
		&tf, zf, &hrhi, requa, rpole, &tgradp);
/* ---    Evaluate thermospheric parameters at deviation=-1, for DENSLO   ATM2317 */
	stewart2_m05__(marsau, clat, clon, &time, &preslo, &templo, &denslo, &
		shgt, &rstar, &hlo, &amlo, &c_b8, iu0, sunlat, &ddtex, &tinf, 
		&tf, zf, &hrlo, requa, rpole, &tgradm);
	*upfctr = denshi / *denst;
	*lwfctr = denslo / *denst;
	if (*upfctr < *lwfctr) {
	    *upfctr = denslo / *denst;
	    *lwfctr = denshi / *denst;
	}
	loh = 2;
	flh = loh / 6.283185;
/* Computing 2nd power */
	d__1 = flh;
	flh *= sqrt(d__1 * d__1 + 1.);
	gcp = *gz * 1e3 / cp_m05__(temp);
	rgas = *pres / (*denst * *temp);
	gor = *gz * 1e3 / rgas;
	wavemax = flh * (tgrad + gcp) / gor;
	if (*upfctr > wavemax + 1.) {
	    *upfctr = wavemax + 1.;
	}
	if (*lwfctr < 1. - wavemax) {
	    *lwfctr = 1. - wavemax;
	}
/* ---    Apply adjustment factors to density and pressure                ATM2336 */
	stewfdens = denst1 / denstop;
	*denst = *denst * stewfdens * (*wavepert + 1.);
	stewfpres = pres1 / prestop;
	*pres = *pres * stewfpres * (*wavepert + 1.);
/* ---    Adjust molecular weight                                         ATM2341 */
	*amz = rstar * *denst * *temp / *pres;
    }
/* ---  Insure thermospheric variability factors are in allowable ranges  ATM2344 */
    if (*lwfctr > (float)0.) {
	if (*upfctr > 1.1499999999999999) {
	    *upfctr = 1.1499999999999999;
	}
	if (*upfctr < 1.02) {
	    *upfctr = 1.02;
	}
	if (*lwfctr < .84999999999999998) {
	    *lwfctr = .84999999999999998;
	}
	if (*lwfctr > .97999999999999998) {
	    *lwfctr = .97999999999999998;
	}
	if (tfactor > .15) {
	    tfactor = .15;
	}
	if (tfactor < .02) {
	    tfactor = .02;
	}
    }
/* ---  Use weighted average profile data if profnear > 0. Weight=1 if    ATM2353 */
/*     lat-lon radius < profnear. Weight=0 if lat-lon radius > proffar.  ATM2354 */
    if (*profnear > 0.) {
	profterp_m05__(&chgt, clat, clon, temp, pres, denst, ewwind, nswind, &
		tout, &pout, &dout, &uout, &vout, nprof, profnear, proffar, 
		profwgt, wavepert);
	*temp = tout;
	*pres = pout;
	*denst = dout;
	*ewwind = uout;
	*nswind = vout;
    }
    return 0;
} /* atmos2_m05__ */

/* -----------------------------------------------------------------------ATM2366 */
/* Subroutine */ int bltp_m05__(doublereal *gz, doublereal *cp, doublereal *
	tg, doublereal *z5, doublereal *t5, doublereal *u5, doublereal *v5, 
	doublereal *zeval, doublereal *factor, doublereal *tempz)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double pow_dd(doublereal *, doublereal *), sqrt(doublereal);

    /* Local variables */
    static doublereal thet5;
    static integer i__;
    static doublereal t0, dt, ri, udenom, sqrtfh;

/*                                                                       BLTP  2 */
/* ---    Mars boundary layer temperature from methods used in the NASA   BLTP  3 */
/*       Ames Mars General Circulation Model (MGCM), as described by     BLTP  4 */
/*       Haberle et al., Jour. Geophys. Res. 104(E4), 8957-8974, 1999,   BLTP  5 */
/*       (referred to as H99 below).                                     BLTP  6 */
/*                                                                       BLTP  7 */
/* ---    Input parameters:                                               BLTP  8 */
/*       gz = local acceleration of gravity (m/s**2)                     BLTP  9 */
/*       Cp = specific heat at constant pressure (joules kg**-1 K**-1)   BLTP 10 */
/*       Tg = temperature (K) at ground surface                          BLTP 11 */
/*       z5 = first MGCM height level (nominally 5 m)                    BLTP 12 */
/*       T5 = temperature (K) at first MGCM level (nominally 5 m)        BLTP 13 */
/*       U5 = zonal wind (m/s) at first MGCM level                       BLTP 14 */
/*       V5 = meridional wind (m/s) at first MGCM level                  BLTP 15 */
/*       zeval = height (m) at which to evaluate temperature             BLTP 16 */
/*       factor = factor for calculations = Log(zeval/z0)/Log(5./z0),    BLTP 17 */
/*                where z0 is surface roughness parameter (m)            BLTP 18 */
/* ---    Output parameter:                                               BLTP 19 */
/*       Tempz = temperature (K) at height zeval                         BLTP 20 */
/* ---------------------------------------------------------------------  BLTP 21 */
/*                                                                       BLTP 22 */
/* ---    Set some initial values for iterative solution                  BLTP 23 */
    sqrtfh = 1.;
    dt = *t5 - *tg;
    t0 = *t5;
/* ---    Potential temperature at first MGCM level                       BLTP 28 */
    thet5 = *t5 + *gz * *z5 / *cp;
/* ---    Iterative calculation of boundary layer parameters              BLTP 30 */
    for (i__ = 1; i__ <= 10; ++i__) {
/* ---      Richardson number from temperature and wind gradients         BLTP 32 */
/* Computing 2nd power */
	d__1 = *u5;
/* Computing 2nd power */
	d__2 = *v5;
	udenom = d__1 * d__1 + d__2 * d__2;
	if (udenom <= .1) {
	    udenom = .1;
	}
	ri = *gz * sqrtfh / thet5 * ((thet5 - *tg) / (sqrtfh + 1.)) * *z5 / 
		udenom;
/* ---      Next iteration of temperature solution by method in Section   BLTP 36 */
/*         4 of H99 (convert from potential to regular temperature)      BLTP 37 */
	*tempz = *tg + (thet5 - *tg) * (sqrtfh * *factor + 1.) / (sqrtfh + 1.)
		 - *gz * *zeval / *cp;
/* ---      Change in temperature from previous iteration                 BLTP 40 */
	dt = *tempz - t0;
	t0 = *tempz;
/* ---      End iteration if sufficient temperature precision achieved    BLTP 43 */
	if (abs(dt) < .01) {
	    return 0;
	}
/* ---      Next iteration of Sqrt(Fh), where Fh is stability function    BLTP 45 */
/*         from Section 4 of H99                                         BLTP 46 */
	if (ri < 0.) {
	    d__1 = 1. - ri * 16.;
	    sqrtfh = pow_dd(&d__1, &c_b11);
	} else {
	    d__1 = ri * 15. / sqrt(ri * 5. + 1.) + 1.;
	    sqrtfh = pow_dd(&d__1, &c_b12);
	}
/* L10: */
    }
    return 0;
} /* bltp_m05__ */

/* -----------------------------------------------------------------------BLTP 55 */
/* Subroutine */ int caltojul_m05__(integer *iy, integer *im, integer *id, 
	integer *ihour, integer *imin, doublereal *sec, doublereal *xjd)
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
/* ---  Consider Jan or Feb as if months 13 and 14 of previous year       CTOJ 14 */
    if (*im <= 2) {
	y = *iy - 1;
	m = *im + 12;
    }
/* ---  Compute day of month plus fractional part                         CTOJ 19 */
    d__ = *id + *ihour / 24. + *imin / 1440. + *sec / 86400.;
    a = (integer) (y / 100.);
    b = 2 - a + (integer) (a / 4.);
/* ---  Compute Julian day with fractional part                           CTOJ 23 */
    *xjd = (integer) ((y + 4716) * 365.25) + (integer) ((m + 1) * 30.6001) + 
	    d__ + b - 1524.5;
    return 0;
} /* caltojul_m05__ */

/* ---------------------------------------------------------------------- CTOJ 28 */
/* Subroutine */ int cospar_m05__(doublereal *z__, doublereal *t, doublereal *
	p, doublereal *rho)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double log(doublereal), pow_dd(doublereal *, doublereal *), exp(
	    doublereal);

    /* Local variables */
    static doublereal aexp, h__, r__, r1, r2, dz;
    static integer iz;

/*                                                                       COSP  2 */
/*     COSPAR N.H. mean temperature (t, K), pressure (p, N/m**2) and     COSP  3 */
/*     density (rho, kg/m**3) versus height (z, km)                      COSP  4 */
/*     Note: input pressures (pc) are mb, densities (dc) are g/cm**3     COSP  5 */
/*                                                                       COSP  6 */
/*     COSPAR values from Table XI, "The Mars Atmosphere: Observations   COSP  7 */
/*     and Model Profiles for Mars Missions", David E. Pitts et al.,     COSP  8 */
/*     eds., JSC-24455                                                   COSP  9 */
/*                                                                       COSP 10 */
/*                                                                       COSP 13 */
/*     1 km interval from -10 to 130 km, 10 km interval 130-360 km       COSP 14 */
    if (*z__ < 130.) {
	iz = (integer) (*z__ + 11.);
    } else {
	iz = (integer) ((*z__ - 130.) / 10.) + 141;
    }
/*     Set values to 0 if z out of range                                 COSP 20 */
    if (iz < 1 || iz > 164) {
	*t = 0.;
	*p = 0.;
	*rho = 0.;
	return 0;
    }
    if (iz > 163) {
	iz = 163;
    }
/*     Linear interpolation on temperature                               COSP 28 */
    dz = (*z__ - cosparnh_m05__1.zc[iz - 1]) / (cosparnh_m05__1.zc[iz] - 
	    cosparnh_m05__1.zc[iz - 1]);
    *t = cosparnh_m05__1.tc[iz - 1] + (cosparnh_m05__1.tc[iz] - 
	    cosparnh_m05__1.tc[iz - 1]) * dz;
/*     Pressure from hydrostatic relation (with special isothermal case) COSP 31 */
    if ((d__1 = cosparnh_m05__1.tc[iz] - cosparnh_m05__1.tc[iz - 1], abs(d__1)
	    ) > .01) {
	aexp = log(cosparnh_m05__1.pc[iz] / cosparnh_m05__1.pc[iz - 1]) / log(
		cosparnh_m05__1.tc[iz] / cosparnh_m05__1.tc[iz - 1]);
	d__1 = *t / cosparnh_m05__1.tc[iz - 1];
	*p = cosparnh_m05__1.pc[iz - 1] * 100. * pow_dd(&d__1, &aexp);
    } else {
	h__ = (cosparnh_m05__1.zc[iz] - cosparnh_m05__1.zc[iz - 1]) / log(
		cosparnh_m05__1.pc[iz - 1] / cosparnh_m05__1.pc[iz]);
	*p = cosparnh_m05__1.pc[iz - 1] * 100. * exp(-(*z__ - 
		cosparnh_m05__1.zc[iz - 1]) / h__);
    }
/*     Linear interpolation on gas constant                              COSP 39 */
    r1 = cosparnh_m05__1.pc[iz - 1] / (cosparnh_m05__1.dc[iz - 1] * 
	    cosparnh_m05__1.tc[iz - 1]);
    r2 = cosparnh_m05__1.pc[iz] / (cosparnh_m05__1.dc[iz] * 
	    cosparnh_m05__1.tc[iz]);
    r__ = r1 + (r2 - r1) * dz;
/*     density from perfect gas law (and convert units to kg/m**3)       COSP 43 */
    *rho = *p * 10. / (r__ * *t);
    return 0;
} /* cospar_m05__ */

/* -----------------------------------------------------------------------COSP 47 */
doublereal cp_m05__(doublereal *t)
{
    /* System generated locals */
    doublereal ret_val;

/* ---  Specific heat at constant pressure, as function of temperature    CPOT  2 */
/* ---  T in kelvins; Cp in joules kg**-1 K**-1                           CPOT  3 */
    ret_val = *t * .123687 + 639.5 + *t * .00200225 * *t;
    return ret_val;
} /* cp_m05__ */

/* -----------------------------------------------------------------------CPOT  8 */
/* Subroutine */ int geocenttogeodet_m05__(doublereal *r__, doublereal *zin, 
	doublereal *fi, doublereal *h__, doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_sign(doublereal *, doublereal *), sqrt(doublereal), log(
	    doublereal), exp(doublereal), acos(doublereal), cos(doublereal), 
	    atan(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal d__, e, f, g, p, q, s, t, v, z__;

/* ---  Program to transform Cartesian to geodetic coordinates            CTOD  2 */
/*     Code adapted from Polish version of K.M.Borkowski, Bull. Geod.    CTOD  3 */
/*     vol 63, pp.50-56 (1989).                                          CTOD  4 */
/* ---  Input:   r, z = equatorial and polar Cartesian components [km]    CTOD  5 */
/*              a, b = equatorial and polar planetary radii [km]         CTOD  6 */
/* ---  Output: fi, h = geodetic coord's (latitude [deg], height [km])    CTOD  7 */
/*     Special case for poles                                            CTOD  8 */
    if (abs(*r__) < .001) {
	*fi = d_sign(&c_b17, zin);
	*h__ = abs(*zin) - *b;
    } else {
/* ---   Analytical solution for non-polar case                           CTOD 14 */
/*      See also page K12 of Astronomical Almanac for iterative          CTOD 15 */
/*      solution                                                         CTOD 16 */
	z__ = abs(*zin);
	e = ((z__ + *b) * *b / *a - *a) / *r__;
	f = ((z__ - *b) * *b / *a + *a) / *r__;
	p = (e * f + 1.) * 4. / 3.;
	q = (e * e - f * f) * 2.;
	d__ = p * p * p + q * q;
	if (d__ >= 0.) {
	    s = sqrt(d__) + q;
	    d__1 = exp(log((abs(s))) / 3.);
	    s = d_sign(&d__1, &s);
	    v = p / s - s;
	    v = -(q + q + v * v * v) / (p * 3.);
	} else {
	    v = sqrt(-p) * 2. * cos(acos(q / p / sqrt(-p)) / 3.);
	}
	g = (e + sqrt(e * e + v)) * .5;
	t = sqrt(g * g + (f - v * g) / (g + g - e)) - g;
	*fi = atan((1. - t * t) * *a / (*b * 2. * t));
	*h__ = (*r__ - *a * t) * cos(*fi) + (z__ - *b) * sin(*fi);
/* ---     Convert to degrees                                             CTOD 35 */
	*fi = *fi * 45. / atan(1.);
	if (*zin < 0.) {
	    *fi = -(*fi);
	}
    }
    return 0;
} /* geocenttogeodet_m05__ */

/* ---------------------------------------------------------------------- CTOD 41 */
/* Subroutine */ int geodettogeocent_m05__(doublereal *fidet, doublereal *h__,
	 doublereal *ficent, doublereal *rtot, doublereal *xy, doublereal *
	z__, doublereal *a, doublereal *b)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double atan(doublereal), d_sign(doublereal *, doublereal *), sin(
	    doublereal), cos(doublereal), sqrt(doublereal);

    /* Local variables */
    static doublereal cphi, sphi, c__, omf, pi180;

/* ---  Program to transform geodetic latitude, height to geocentric      DTOC  2 */
/*     latitude, radius                                                  DTOC  3 */
/*     Method from page K12 of Astronomical Almanac                      DTOC  4 */
/* ---  Input:  fidet, h = geodetic lat (deg), height (km)                DTOC  5 */
/*             a, b = equatorial and polar planetary radii (km)          DTOC  6 */
/* ---  Output: ficent = geocentric lat (deg)                             DTOC  7 */
/*             rtot = geocentric total radius (km)                       DTOC  8 */
/*             xy, z = equatorial, polar cartesian components (km)       DTOC  9 */
    pi180 = atan(1.) / 45.;
/* ---  Special case for poles                                            DTOC 12 */
    if (abs(*fidet) == 90.) {
	*ficent = d_sign(&c_b17, fidet);
	*rtot = *b + *h__;
	*xy = 0.;
	*z__ = d_sign(rtot, fidet);
    } else {
/* ---    1 - flattening                                                  DTOC 19 */
	omf = *b / *a;
/* ---    Sin and Cos of geodetic lat                                     DTOC 21 */
	sphi = sin(pi180 * *fidet);
	cphi = cos(pi180 * *fidet);
/* ---    Computational factor C for cartesian coordinates                DTOC 24 */
/* Computing 2nd power */
	d__1 = cphi;
/* Computing 2nd power */
	d__2 = omf * sphi;
	c__ = 1. / sqrt(d__1 * d__1 + d__2 * d__2);
/* ---    Polar and equatorial cartesian coordinates                      DTOC 26 */
/* Computing 2nd power */
	d__1 = omf;
	*z__ = (*a * c__ * (d__1 * d__1) + *h__) * sphi;
	*xy = (*a * c__ + *h__) * cphi;
/* ---    Total geocentric radius                                         DTOC 29 */
/* Computing 2nd power */
	d__1 = *xy;
/* Computing 2nd power */
	d__2 = *z__;
	*rtot = sqrt(d__1 * d__1 + d__2 * d__2);
/* ---    Geocentric latitude, deg                                        DTOC 31 */
	*ficent = atan(*z__ / *xy) / pi180;
    }
    return 0;
} /* geodettogeocent_m05__ */

/* ---------------------------------------------------------------------- DTOC 36 */
/* Subroutine */ int dustfact_m05__(doublereal *clat, doublereal *clon, 
	doublereal *als, doublereal *dustm, doublereal *stormdz)
{
    /* System generated locals */
    doublereal d__1, d__2;

    /* Builtin functions */
    double cos(doublereal), sqrt(doublereal);

    /* Local variables */
    static doublereal dlon, sizefact, dlsmax, rad, dew, dls, dns, raddust;

/* ---  Computes dust storm intensity factor dustM                        DSTF  2 */
/* ---  as a function of the time since start of the storm,               DSTF  3 */
/* ---  (als - als0), measured in Ls angle (degrees), and as a            DSTF  4 */
/* ---  function of the storm intensity, intens.  dustM is for            DSTF  5 */
/* ---  magnitude of effect on dust optical depth (0.0 - 3.0).            DSTF  6 */
    dls = *als - datacom_m05__1.als0;
    dlsmax = datacom_m05__1.alsdur;
    if (dlsmax < 12.) {
	dlsmax = 12.;
    }
    if (dlsmax > 48.) {
	dlsmax = 48.;
    }
/* ---  Return dust factor of 0 if Ls-Ls0 < 0 or > dlsmax degrees         DSTF 18 */
    if (dls <= 0. || dls > dlsmax || datacom_m05__1.intens <= 0.) {
	*dustm = 0.;
	*stormdz = 0.;
	return 0;
    }
/* ---  Compute initial dustM factor (0-1) from time (Ls) profile         DSTF 24 */
    if (dls <= dlsmax / 8.) {
	*dustm = dls * 8. / dlsmax;
    } else if (dls >= dlsmax / 2.) {
	*dustm = (1. - dls / dlsmax) * 2.;
    } else {
	*dustm = 1.;
    }
    sizefact = 1.;
/* ---  Compute parameters of local storm if radmax is not 0              DSTF 33 */
    if (datacom_m05__1.radmax != 0.) {
	sizefact = 0.;
/* ---    dns,dew,rad = horizontal coordinates from center of dust storm  DSTF 36 */
	dns = datacom_m05__1.dtr * datacom_m05__1.rref * (*clat - 
		datacom_m05__1.dustlat);
	dlon = (d__1 = *clon - datacom_m05__1.dustlon, abs(d__1));
	if (dlon > 180.) {
	    dlon = 360. - dlon;
	}
	dew = datacom_m05__1.dtr * datacom_m05__1.rref * cos(
		datacom_m05__1.dtr * *clat) * dlon;
/* Computing 2nd power */
	d__1 = dns;
/* Computing 2nd power */
	d__2 = dew;
	rad = sqrt(d__1 * d__1 + d__2 * d__2);
/* ---    raddust = actual horizontal size of storm                       DSTF 42 */
	raddust = *dustm * datacom_m05__1.radmax;
/* ---    sizefact = position-dependent measure of relative storm effect  DSTF 44 */
	if (rad < raddust * 2.) {
	    sizefact = (cos(datacom_m05__1.dtr * 90. * rad / raddust) + 1.) * 
		    .5;
	}
    }
/* ---  Final factor dependent on position and on storm intensity         DSTF 48 */
    *dustm = sizefact * *dustm;
    *stormdz = *dustm * 5. * sqrt(datacom_m05__1.intens);
    *dustm *= datacom_m05__1.intens;
    return 0;
} /* dustfact_m05__ */

/* -----------------------------------------------------------------------DSTF 54 */
/* Subroutine */ int datastep_m05__(integer *i__, doublereal *chgt, 
	doublereal *clat, doublereal *clon, doublereal *csec, doublereal *
	day0, doublereal *rhod, doublereal *rhou, doublereal *rhov, 
	doublereal *rhow, integer *eof, doublereal *delhgt, doublereal *
	dellat, doublereal *dellon, doublereal *deltime, doublereal *temp, 
	doublereal *pres, doublereal *denslo, doublereal *dens, doublereal *
	denshi, doublereal *densp, doublereal *ewwind, doublereal *ewpert, 
	doublereal *nswind, doublereal *nspert, doublereal *vwpert, 
	doublereal *hrho, doublereal *hscale, doublereal *dsunlat, doublereal 
	*dsunlon, doublereal *dsunls, doublereal *dradau, doublereal *dowlt, 
	integer *lonew, doublereal *corlim, doublereal *denstot, integer *
	numwave, doublereal *hgtasfc, integer *iert, integer *iutc, 
	doublereal *pertstep, doublereal *corlmin, integer *iupdate, 
	doublereal *als, doublereal *szang, doublereal *owlt, doublereal *
	sunlat, doublereal *sunlon, doublereal *marsau, doublereal *tlocal, 
	doublereal *profnear, doublereal *proffar, integer *nprof)
{
    /* Format strings */
    static char fmt_292[] = "(\002 A0,A1,phi1,A2,phi2,A3,phi3=\002,f6.3,3(f6\
.3,f7.1))";
    static char fmt_294[] = "(\002 Traveling wave phases initialized at Juli\
an DAY\002,\002 =\002,f13.3/\002 at phase rates (phi1dot,phi2dot,phi3dot,\
\002,\002 deg/day)=\002,3f8.3)";
    static char fmt_293[] = "(\002   Wave Scale =\002,f8.1,\002 km.    Wave \
phases are in\002,\002 degrees of \002,a4,\002 Longitude\002)";
    static char fmt_590[] = "(\002 Time (rel. to T0) =\002,f10.1,\002 sec. \
(\002,f8.3,\002 sols)\002,\002  Ls =\002,f6.1,\002  Dust =\002,f5.2/\002 Hei\
ght Above MOLA (or Surface)\002,\002 =\002,f8.3,\002 km (\002,f8.3,\002 km) \
 OWLT =\002,f6.2,\002 Min\002/\002 Topographic Height = \002,f8.3,\002 km   \
Radius (Areoid) = \002,f8.3,\002 (\002,f8.3,\002) km\002/\002 Hgt Above Elli\
psoid =\002,f8.3,\002 km  \002,\002 Scale Hgt H(p)=\002,f7.2,\002 H(rho)=\
\002,f7.2,\002 km\002/\002 Height Offset Parameters:   ibougher =\002,i2,\
\002    Local Height\002,\002 Offset =\002,f7.3,\002 km\002/\002 Planeto-Cen\
tric Lat = \002,f7.2,\002 deg  Longitude = \002,f7.2,\002 W (\002,f7.2,\002 \
E) deg.\002/\002 Planeto-Graphic Lat =\002,f8.2,\002 deg  Planeto-\002,\002G\
raphic Hgt (Ellps)=\002,f9.3,\002 km\002/\002 Planeto-Cent Sun Lat = \002,f6\
.2,\002 deg  Mars Orbital Radius =\002,f6.3,\002 AU\002/\002 Sun Longitude \
=\002,f8.2,\002 deg.W      Local True Solar Time = \002,f6.2,\002 Mars hr\
s\002)";
    static char fmt_595[] = "(\002 Exospheric Temp. = \002,f6.1,\002 K\002,8\
x,\002Tbase = \002,f6.1,\002 K\002,3x,\002 Zbase = \002,f6.1,\002 km\002)";
    static char fmt_596[] = "(\002 Solar Zenith Angle =\002,f6.1,\002 deg   \
  F1 peak =\002,f6.1,\002 km\002)";
    static char fmt_600[] = "(\002 Temperature = \002,f7.1,\002 K\002,7x,\
\002 Pressure =\002,1p,e10.3,0p,\002 N/m**2   profwgt =\002,f6.3/\002 Densit\
y (Low, Avg., High) =\002,1p,3e12.3,0p,1x,a8/\002 Departure, COSPAR NH Mean =\
\002,f9.1,\002 %\002,2(f10.1,\002 %\002),\002  iupdate =\002,i2/\002 Tot.Den\
s. =\002,1p,e10.3,0p,1x,a8,\002  Dens.Pert. =\002,f7.2,\002% Wave =\002,f7.2,\
\002% of mean\002/\002 Eastward Wind (Mean,Perturbed,Total) =\002,3f7.1,\002\
 m/s   \002,\002 VertWind\002/\002 Northward Wind(Mean,Perturbed,Total) =\
\002,3f7.1,\002 m/s\002,f8.1,\002 m/s\002)";
    static char fmt_610[] = "(\002 Warning: Step size smaller than accuracy \
limit by a \002,\002factor of\002,f6.3)";
    static char fmt_650[] = "(\002 -----------------------------------------\
------------\002,\002------------------------\002)";
    static char fmt_796[] = "(g13.5,1p,4e11.3,0p,f7.4,f9.3,2f6.3,i5,f13.3,i6\
,i8,f9.3)";
    static char fmt_790[] = "(g13.5,f6.2,3f10.3,1p,e10.3,0p,2f7.2,i5)";
    static char fmt_781[] = "(g13.5,7f8.2,i3)";
    static char fmt_798[] = "(g13.5,2(f7.1,1p,e11.3,0p),2f8.2,f6.2,f7.3,f6.1\
,f9.3,f8.3,10f6.2,i5)";
    static char fmt_780[] = "(g13.5,f7.1,1p,2e11.3,0p,4f8.1,1p,2e11.3,i5,e14\
.3)";
    static char fmt_791[] = "(g13.5,3f8.1,f8.2,f8.1,f10.3,i6)";
    static char fmt_793[] = "(g13.5,f6.3,f9.5,1p,4e9.2,0p,i3)";
    static char fmt_797[] = "(2g13.5,1p,4e11.3,0p,f7.4,f9.3,2f6.3,i5,f13.3,i\
6,i8,f9.3)";
    static char fmt_795[] = "(2g13.5,f6.2,3f10.3,1p,e10.3,0p,2f7.2,i5)";
    static char fmt_782[] = "(2g13.5,7f8.2,i3)";
    static char fmt_799[] = "(2g13.5,2(f7.1,1p,e11.3,0p),2f8.2,f6.2,f7.3,f6.\
1,f9.3,f8.3,10f6.2,i5)";
    static char fmt_785[] = "(2g13.5,f7.1,1p,2e11.3,0p,4f8.1,1p,2e11.3,i5,e1\
4.3)";
    static char fmt_792[] = "(2g13.5,3f8.1,f8.2,f8.1,f10.3,i6)";
    static char fmt_794[] = "(2g13.5,f6.3,f9.5,1p,4e9.2,0p,i3)";
    static char fmt_800[] = "(f10.0,2f7.2,f8.2,1p,e10.3,0p,3f7.1,2f6.1,f5.2,\
10f6.2)";
    static char fmt_810[] = "(f10.0,2f7.2,f8.2,f10.3,3f7.1,2f6.1,f5.2,10f6.2)"
	    ;

    /* System generated locals */
    integer i__1;
    doublereal d__1, d__2;
    alist al__1;

    /* Builtin functions */
    double cos(doublereal), sin(doublereal);
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle();
    double d_sign(doublereal *, doublereal *);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    double tan(doublereal), atan(doublereal), sqrt(doublereal), log(
	    doublereal), exp(doublereal), acos(doublereal), pow_dd(doublereal 
	    *, doublereal *), d_lg10(doublereal *);
    integer f_rew(alist *);

    /* Local variables */
    static doublereal fmassh2o, talb, dareaden, sigd, hgtm, delz, thgt;
    static integer icepolar;
    static doublereal dtimecor, ohgt, densrand, topz, denswave, blwindew, 
	    blwindns, sigu, sigw, elon, radtotal, tcos, pcos, dens0, varx, 
	    vary, wavepert, alogdens, olat, olon;
    extern /* Subroutine */ int atmos2_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, integer *, doublereal *, doublereal *, integer *, 
	    doublereal *, integer *);
    static integer l;
    extern doublereal cp_m05__(doublereal *);
    static doublereal clatc, chgtc, clatg, chgtg, tbase, devhi, delew, delns, 
	    zbase, devav, dcosp, fmass[9], devlo, hatzf, tgrnd, ttsec, hgtms;
    extern doublereal random_m05__(integer *);
    static doublereal dplus, ohgts, texos, ewtot;
    extern /* Subroutine */ int cospar_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal qsurf, nstot, f1peak, z1, z2;
    static char densunits[8];
    static doublereal albedo, dt, dz, facthi, zf, gz, factlo, bluday, blvday, 
	    oldhgt, correl;
    static integer ifault;
    static doublereal cszang, devday, devmax;
    static char lonvar[4];
    static doublereal dminus, devtot;
    extern /* Subroutine */ int rellips_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), 
	    wavelon_m05__(integer *, doublereal *, doublereal *, doublereal *,
	     doublereal *, doublereal *);
    static doublereal devmin;
    extern /* Subroutine */ int species_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *, doublereal *, doublereal *,
	     doublereal *, doublereal *);
    static doublereal presmb, sigmalevel, blwindvert, fmolh2o;
    extern /* Subroutine */ int geocenttogeodet_m05__(doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *), geodettogeocent_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal careoid, hls, rsc, eot, amz, ssposr2, dareoid, ogz, 
	    corbeta;
    extern doublereal ppnd_m05__(doublereal *, integer *);
    static doublereal dmasden, dxy, vls, densday, var, oldrref, trajsec, 
	    trajlat, trajhgt, tempday, presday, densmax, trajlon;
    extern /* Subroutine */ int marsephm_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static doublereal tempmax, ewwnday, nswnday, topohgt, patsurf, tempmin, 
	    densmin, profwgt, airmass, offsetl, preshgt, dmixrat, expfact, 
	    dnumden;

    /* Fortran I/O blocks */
    static cilist io___183 = { 1, 7, 1, 0, 0 };
    static cilist io___188 = { 0, 0, 0, fmt_292, 0 };
    static cilist io___189 = { 0, 0, 0, fmt_294, 0 };
    static cilist io___191 = { 0, 0, 0, fmt_293, 0 };
    static cilist io___192 = { 0, 0, 0, fmt_292, 0 };
    static cilist io___193 = { 0, 0, 0, fmt_294, 0 };
    static cilist io___194 = { 0, 0, 0, fmt_293, 0 };
    static cilist io___258 = { 0, 0, 0, fmt_590, 0 };
    static cilist io___259 = { 0, 0, 0, fmt_595, 0 };
    static cilist io___260 = { 0, 0, 0, fmt_596, 0 };
    static cilist io___272 = { 0, 0, 0, fmt_600, 0 };
    static cilist io___276 = { 0, 0, 0, fmt_610, 0 };
    static cilist io___277 = { 0, 0, 0, fmt_650, 0 };
    static cilist io___290 = { 0, 21, 0, fmt_796, 0 };
    static cilist io___291 = { 0, 22, 0, fmt_790, 0 };
    static cilist io___292 = { 0, 23, 0, fmt_781, 0 };
    static cilist io___293 = { 0, 24, 0, fmt_798, 0 };
    static cilist io___294 = { 0, 25, 0, fmt_780, 0 };
    static cilist io___295 = { 0, 26, 0, fmt_791, 0 };
    static cilist io___296 = { 0, 27, 0, fmt_793, 0 };
    static cilist io___297 = { 0, 21, 0, fmt_797, 0 };
    static cilist io___298 = { 0, 22, 0, fmt_795, 0 };
    static cilist io___299 = { 0, 23, 0, fmt_782, 0 };
    static cilist io___300 = { 0, 24, 0, fmt_799, 0 };
    static cilist io___301 = { 0, 25, 0, fmt_785, 0 };
    static cilist io___302 = { 0, 26, 0, fmt_792, 0 };
    static cilist io___303 = { 0, 27, 0, fmt_794, 0 };
    static cilist io___307 = { 0, 29, 0, fmt_800, 0 };
    static cilist io___308 = { 0, 29, 0, fmt_810, 0 };


/* ---  If ipclat (in Common DATACOM) is 1, latitude CLAT and height CHGT DSTP  7 */
/*     are planeto-centric, otherwise CLAT and CHGT are planeto-graphic, DSTP  8 */
/*     with CHGT measured above reference ellipsoid.                     DSTP  9 */
/*     If input parameter MOLAhgts is 1, CHGT is height above the MOLA   DSTP 10 */
/*     areoid; otherwise CHGT is height above reference ellipsoid,       DSTP 11 */
/*     in which case height is converted to HGTM, height above MOLA      DSTP 12 */
/*     areoid for call to subroutine ATMOS2_M05.                         DSTP 13 */
/* ---  If MOLAhgts = 1, then ipclat must also be 1                       DSTP 14 */
/*                                                                       DSTP 15 */
    *eof = 0;
/* ---  Top height = 170 for 2001 MGCM data = 240 km for TES yr1&2 data   DSTP 40 */
    topz = 170.;
    if (datacom_m05__1.mapyear > 0) {
	topz = 240.;
    }
/* ---  Compute planeto-centric and planeto-graphic coordinates, as       DSTP 43 */
/*      necessary: CLATc,CHGTc=planeto-centric; CLATg,CHGTg=planeto-     DSTP 44 */
/*      graphic (same as geodetic)                                       DSTP 45 */
    if (datacom_m05__1.molahgts == 1 || datacom_m05__1.ipclat == 1) {
/* ---    Input CLAT is planeto-centric wrt MOLA or ellipsoid             DSTP 46a */
	clatc = *clat;
/* ---    Get MOLA radius Rref and ellipsoid radius Oldrref for current   DSTP 46c */
/*         position at height = 0                                        DSTP 46d */
	rellips_m05__(&clatc, clon, &datacom_m05__1.rref, &c_b2, &ogz, &
		oldrref, &topohgt, &albedo, &datacom_m05__1.requa, &
		datacom_m05__1.rpole);
/* ---    Interpret CHGT as planeto-centric radius if CHGT > 3000 km      DSTP 46g */
	if (*chgt > 3e3) {
	    rsc = *chgt;
	    if (datacom_m05__1.molahgts == 1) {
		chgtc = *chgt - datacom_m05__1.rref;
		hgtm = chgtc;
	    } else {
		chgtc = *chgt - oldrref;
		hgtm = *chgt - datacom_m05__1.rref;
	    }
	} else {
/* ---      Interpret CHGT as planeto-centric altitude above MOLA or      DSTP 53a */
/*         ellipsoid (depending on MOLAhgts value)                       DSTP 53b */
	    chgtc = *chgt;
/* ---      Total radius RSC                                              DSTP 55 */
	    rsc = datacom_m05__1.rref + chgtc;
	    hgtm = chgtc;
/* ---      Height wrt ellipsoid if MOLAhgts ne 1                         DSTP 57 */
	    if (datacom_m05__1.molahgts != 1) {
		rsc = oldrref + chgtc;
		hgtm = rsc - datacom_m05__1.rref;
	    }
	}
/* ---    Get equatorial and polar cartesian components                   DSTP 62 */
	dxy = rsc * cos(datacom_m05__1.dtr * clatc);
	dz = rsc * sin(datacom_m05__1.dtr * clatc);
/* ---    Convert planeto-centric to planeto-graphic                      DSTP 65 */
	geocenttogeodet_m05__(&dxy, &dz, &clatg, &chgtg, &
		datacom_m05__1.requa, &datacom_m05__1.rpole);
    } else {
/* ---    Error for radius input (height > 3000 km) if ipclat ne 1        DSTP 67a */
	if (*chgt > 3e3) {
	    s_stop(" Radius input requires ipclat=1", (ftnlen)31);
	}
/* ---    Input CLAT&CHGT are planeto-graphic (wrt to ellipsoid)          DSTP 68 */
	clatg = *clat;
	chgtg = *chgt;
/* ---    Convert planeto-graphic to planeto-centric                      DSTP 71 */
	geodettogeocent_m05__(&clatg, &chgtg, &clatc, &rsc, &dxy, &dz, &
		datacom_m05__1.requa, &datacom_m05__1.rpole);
/* ---    Get local radii for MOLA (Rref) and ellipsoid (Oldrref)         DSTP 74 */
	rellips_m05__(&clatc, clon, &datacom_m05__1.rref, &chgtg, &ogz, &
		oldrref, &topohgt, &albedo, &datacom_m05__1.requa, &
		datacom_m05__1.rpole);
	chgtc = rsc - oldrref;
	hgtm = rsc - datacom_m05__1.rref;
    }
/* ---  Set vertical and horizontal scale parameters                      DSTP 80 */
    vls = datacom_m05__1.wlscale * 8.;
/* Computing 2nd power */
    d__1 = hgtm;
    hls = d__1 * d__1 * .01875 + 30.;
    if (hls > 600.) {
	hls = 600.;
    }
    hls *= datacom_m05__1.wlscale;
/* ---  Relative displacements between previous and current position      DSTP 85 */
    delns = datacom_m05__1.dtr * rsc * *dellat / hls;
    delew = -datacom_m05__1.dtr * rsc * cos(datacom_m05__1.dtr * clatc) * *
	    dellon / hls;
    delz = *delhgt / vls;
    if (datacom_m05__1.npos <= 0) {
/* ---    Read new position if trajectory data file is being used         DSTP 90 */
	i__1 = s_rsle(&io___183);
	if (i__1 != 0) {
	    goto L100001;
	}
	i__1 = do_lio(&c__5, &c__1, (char *)&trajsec, (ftnlen)sizeof(
		doublereal));
	if (i__1 != 0) {
	    goto L100001;
	}
	i__1 = do_lio(&c__5, &c__1, (char *)&trajhgt, (ftnlen)sizeof(
		doublereal));
	if (i__1 != 0) {
	    goto L100001;
	}
	i__1 = do_lio(&c__5, &c__1, (char *)&trajlat, (ftnlen)sizeof(
		doublereal));
	if (i__1 != 0) {
	    goto L100001;
	}
	i__1 = do_lio(&c__5, &c__1, (char *)&trajlon, (ftnlen)sizeof(
		doublereal));
	if (i__1 != 0) {
	    goto L100001;
	}
	i__1 = e_rsle();
L100001:
	if (i__1 < 0) {
	    goto L999;
	}
	if (i__1 > 0) {
	    goto L9998;
	}
/* ---    Convert negative longitudes                                     DSTP 92 */
	if (trajlon < 0.) {
	    trajlon += 360.;
	}
/* ---    Convert to West Longitude if LonEW = 1                          DSTP 94 */
	if (*lonew == 1) {
	    trajlon = 360. - trajlon;
	}
	if (*i__ > 0) {
/* ---      Compute displacement magnitude of new from previous position  DSTP 97 */
	    *deltime = trajsec - *csec;
	    *delhgt = trajhgt - *chgt;
	    *dellat = (d__1 = trajlat - *clat, abs(d__1));
	    *dellon = (d__1 = trajlon - *clon, abs(d__1));
	}
/* ---    Correct DELLON for cases near 0/360 longitude discontinuity     DSTP103 */
	if (*dellon > 180.) {
	    *dellon = 360. - *dellon;
	}
	if (*dellon < 0.) {
	    *dellon += 360.;
	}
/* ---    Correct DELLON and DELLAT near polar discontinuities            DSTP106 */
	if (*dellon > 90. && (abs(trajlat) >= 70. || abs(*clat) >= 70.)) {
	    *dellon = (d__1 = 180. - *dellon, abs(d__1));
	    *dellat = 180. - abs(trajlat) - abs(*clat);
	}
/* ---    Relative displacements between previous and current position    DSTP112 */
	delns = datacom_m05__1.dtr * rsc * *dellat / hls;
	delew = -datacom_m05__1.dtr * rsc * cos(datacom_m05__1.dtr * clatc) * 
		*dellon / hls;
	delz = *delhgt / vls;
/* ---    Set current position to new position                            DSTP116 */
	*csec = trajsec;
	*chgt = trajhgt;
	*clat = trajlat;
	*clon = trajlon;
    } else if (*i__ > 0) {
	*chgt += *delhgt;
	*clat += *dellat;
	*clon += *dellon;
	*csec += *deltime;
    }
/* ---  Correct latitude and longitude if position crosses either pole    DSTP127 */
    if (abs(*clat) > 90.) {
	*clat = d_sign(&c_b35, clat) - *clat;
	*clon += 180.;
	*dellat = -(*dellat);
    }
    if (*clon < 0.) {
	*clon += 360.;
    }
    if (*clon >= 360.) {
	*clon += -360.;
    }
/* ---  Compute planeto-centric and planeto-grapic coordinates for new    DSTP135 */
/*     position, as necessary                                            DSTP136 */
    if (datacom_m05__1.molahgts == 1 || datacom_m05__1.ipclat == 1) {
/* ---    Input CLAT is planeto-centric wrt MOLA or ellipsoid             DSTP137a */
	clatc = *clat;
/* ---    Get MOLA radius Rref and ellipsoid radius Oldrref for current   DSTP137c */
/*         position at height = 0                                        DSTP137d */
	rellips_m05__(&clatc, clon, &datacom_m05__1.rref, &c_b2, &ogz, &
		oldrref, &topohgt, &albedo, &datacom_m05__1.requa, &
		datacom_m05__1.rpole);
/* ---    Interpret CHGT as planeto-centric radius if CHGT > 3000 km      DSTP137g */
	if (*chgt > 3e3) {
	    rsc = *chgt;
	    if (datacom_m05__1.molahgts == 1) {
		chgtc = *chgt - datacom_m05__1.rref;
		hgtm = chgtc;
	    } else {
		chgtc = *chgt - oldrref;
		hgtm = *chgt - datacom_m05__1.rref;
	    }
	} else {
/* ---      Interpret CHGT as planeto-centric altitude above MOLA or      DSTP144a */
/*         ellipsoid (depending on MOLAhgts value)                       DSTP144b */
	    chgtc = *chgt;
/* ---      Total radius RSC                                              DSTP146 */
	    rsc = datacom_m05__1.rref + chgtc;
	    hgtm = chgtc;
/* ---      Height wrt ellipsoid if MOLAhgts ne 1                         DSTP148 */
	    if (datacom_m05__1.molahgts != 1) {
		rsc = oldrref + chgtc;
		hgtm = rsc - datacom_m05__1.rref;
	    }
	}
/* ---    Get equatorial and polar cartesian components                   DSTP153 */
	dxy = rsc * cos(datacom_m05__1.dtr * clatc);
	dz = rsc * sin(datacom_m05__1.dtr * clatc);
/* ---    Convert planeto-centric to planeto-graphic                      DSTP156 */
	geocenttogeodet_m05__(&dxy, &dz, &clatg, &chgtg, &
		datacom_m05__1.requa, &datacom_m05__1.rpole);
    } else {
/* ---    Error for radius input (height > 3000 km) if ipclat ne 1        DSTP158a */
	if (*chgt > 3e3) {
	    s_stop(" Radius input requires ipclat=1", (ftnlen)31);
	}
/* ---    CLAT&CHGT are planeto-graphic (wrt to ellipsoid)                DSTP159 */
	clatg = *clat;
	chgtg = *chgt;
/* ---    Convert planeto-graphic to planeto-centric                      DSTP162 */
	geodettogeocent_m05__(&clatg, &chgtg, &clatc, &rsc, &dxy, &dz, &
		datacom_m05__1.requa, &datacom_m05__1.rpole);
/* ---    Get local radii for MOLA (Rref) and ellipsoid (Oldrref)         DSTP165 */
	rellips_m05__(&clatc, clon, &datacom_m05__1.rref, &chgtg, &ogz, &
		oldrref, &topohgt, &albedo, &datacom_m05__1.requa, &
		datacom_m05__1.rpole);
	chgtc = rsc - oldrref;
	hgtm = rsc - datacom_m05__1.rref;
    }
    datacom_m05__1.day = *day0 + *csec / 86400.;
/* ---  Update wave coefficients if necessary                             DSTP172 */
    if (wavecoef_m05__1.iuwave > 0 && *numwave < wavecoef_m05__1.nwave) {
	if (*csec >= wavecoef_m05__1.wavetime[*numwave]) {
L100:
	    ++(*numwave);
	    wavecoef_m05__1.wavea0 = wavecoef_m05__1.wavedata[*numwave - 1];
	    wavecoef_m05__1.wavedate = wavecoef_m05__1.wavedata[*numwave + 99]
		    ;
	    wavecoef_m05__1.wavea1 = wavecoef_m05__1.wavedata[*numwave + 199];
	    wavecoef_m05__1.wavephi1 = wavecoef_m05__1.wavedata[*numwave + 
		    299];
	    wavecoef_m05__1.phi1dot = wavecoef_m05__1.wavedata[*numwave + 399]
		    ;
	    wavecoef_m05__1.wavea2 = wavecoef_m05__1.wavedata[*numwave + 499];
	    wavecoef_m05__1.wavephi2 = wavecoef_m05__1.wavedata[*numwave + 
		    599];
	    wavecoef_m05__1.phi2dot = wavecoef_m05__1.wavedata[*numwave + 699]
		    ;
	    wavecoef_m05__1.wavea3 = wavecoef_m05__1.wavedata[*numwave + 799];
	    wavecoef_m05__1.wavephi3 = wavecoef_m05__1.wavedata[*numwave + 
		    899];
	    wavecoef_m05__1.phi3dot = wavecoef_m05__1.wavedata[*numwave + 999]
		    ;
/* ---      Check to see if more than one wave time exceeded              DSTP187 */
	    if (*numwave < wavecoef_m05__1.nwave) {
		if (*csec >= wavecoef_m05__1.wavetime[*numwave]) {
		    goto L100;
		}
	    }
/* ---      Write out updated wave coefficients                           DSTP191 */
	    if (datacom_m05__1.iup > 0) {
		io___188.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___188);
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavea0, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavea1, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavephi1, (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavea2, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavephi2, (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavea3, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavephi3, (ftnlen)
			sizeof(doublereal));
		e_wsfe();
		if (wavecoef_m05__1.wavedate > 0.) {
		    io___189.ciunit = datacom_m05__1.iup;
		    s_wsfe(&io___189);
		    do_fio(&c__1, (char *)&wavecoef_m05__1.wavedate, (ftnlen)
			    sizeof(doublereal));
		    do_fio(&c__1, (char *)&wavecoef_m05__1.phi1dot, (ftnlen)
			    sizeof(doublereal));
		    do_fio(&c__1, (char *)&wavecoef_m05__1.phi2dot, (ftnlen)
			    sizeof(doublereal));
		    do_fio(&c__1, (char *)&wavecoef_m05__1.phi3dot, (ftnlen)
			    sizeof(doublereal));
		    e_wsfe();
		}
		s_copy(lonvar, "West", (ftnlen)4, (ftnlen)4);
		if (*lonew == 1) {
		    s_copy(lonvar, "East", (ftnlen)4, (ftnlen)4);
		}
		io___191.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___191);
		do_fio(&c__1, (char *)&wavecoef_m05__1.wscale, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, lonvar, (ftnlen)4);
		e_wsfe();
	    }
	}
    }
/* ---  Write wave coefficients if not read from file                     DSTP209 */
    if (wavecoef_m05__1.iuwave == 0 && *numwave == 0) {
	if (datacom_m05__1.iup > 0) {
	    io___192.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___192);
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wavea0, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wavea1, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wavephi1, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wavea2, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wavephi2, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wavea3, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wavephi3, (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	    if (wavecoef_m05__1.wavedate > 0.) {
		io___193.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___193);
		do_fio(&c__1, (char *)&wavecoef_m05__1.wavedate, (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.phi1dot, (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.phi2dot, (ftnlen)
			sizeof(doublereal));
		do_fio(&c__1, (char *)&wavecoef_m05__1.phi3dot, (ftnlen)
			sizeof(doublereal));
		e_wsfe();
	    }
	    s_copy(lonvar, "West", (ftnlen)4, (ftnlen)4);
	    if (*lonew == 1) {
		s_copy(lonvar, "East", (ftnlen)4, (ftnlen)4);
	    }
	    io___194.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___194);
	    do_fio(&c__1, (char *)&wavecoef_m05__1.wscale, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, lonvar, (ftnlen)4);
	    e_wsfe();
	}
	*numwave = 1;
    }
/* ---  Sun and Mars positions at new time                                DSTP222 */
/*     Use high precision values if inputs > 0 or ephemeris subroutine   DSTP223 */
/*     otherwise                                                         DSTP224 */
    if (*dradau > 0.) {
	*sunlon = *dsunlon;
	*sunlat = *dsunlat;
	*als = *dsunls;
	*marsau = *dradau;
	*owlt = *dowlt;
    } else {
/* ---    Use built-in Mars ephemeris routine                             DSTP232 */
/* ---    Convert to Terrestrial (Dynamical) Time, if necessary           DSTP233 */
	ttsec = 0.;
	if (*iutc == 1) {
/* ---      Get terrestrial dynamical time offset (seconds)               DSTP236 */
	    dt = (datacom_m05__1.day - 2451545.) / 36525.;
/* ---      Terrestrial time offset (in seconds) TT = UTC + ttsec         DSTP238 */
/* Computing 2nd power */
	    d__1 = dt;
	    ttsec = (dt * 95. + 64.184 + d__1 * d__1 * 35.) / 86400.;
	}
	d__1 = datacom_m05__1.day + ttsec;
	marsephm_m05__(&d__1, sunlat, sunlon, als, marsau, owlt, &eot);
/* ---    Convert to Mars-Event Time, if necessary                        DSTP242 */
	if (*iert == 1) {
	    d__1 = datacom_m05__1.day + ttsec - *owlt / 1440.;
	    marsephm_m05__(&d__1, sunlat, sunlon, als, marsau, owlt, &eot);
	}
/* ---    Convert planetographic sun latitude to planetocentric           DSTP245 */
	*sunlat = atan(tan(*sunlat * datacom_m05__1.dtr) / 1.0104941290313674)
		 / datacom_m05__1.dtr;
    }
/* ---  Evaluate longitude-dependent wave perturbation                    DSTP248 */
    wavelon_m05__(lonew, clon, &clatc, &hgtm, &datacom_m05__1.day, &wavepert);
/* ---  Convert wave perturbation to % for output                         DSTP250 */
    denswave = wavepert * 100.;
/* ---  Evaluate atmospheric parameters                                   DSTP252 */
    atmos2_m05__(&hgtm, &clatc, clon, marsau, sunlat, sunlon, als, hscale, 
	    temp, dens, &facthi, &factlo, pres, &thgt, &careoid, &zf, &
	    datacom_m05__1.iu0, &datacom_m05__1.deltatex, &texos, &tbase, 
	    hrho, &amz, &datacom_m05__1.dusttau, &datacom_m05__1.dustmin, &
	    datacom_m05__1.dustmax, &datacom_m05__1.dustod, ewwind, nswind, &
	    blwindew, &blwindns, &blwindvert, &hatzf, &wavepert, &tempday, &
	    presday, &densday, &ewwnday, &nswnday, &bluday, &blvday, hgtasfc, 
	    &patsurf, &tempmax, &tempmin, &densmax, &densmin, &tgrnd, &talb, &
	    icepolar, &gz, &oldrref, &datacom_m05__1.requa, &
	    datacom_m05__1.rpole, &datacom_m05__1.mapyear, profnear, proffar, 
	    nprof, &profwgt, &datacom_m05__1.idaydata);
/* ---  Save value of height of 1.26 nbar level                           DSTP260 */
    zbase = zf;
/* ---  Compute exponential correlation across displacement from          DSTP262 */
/*     previous position.                                                DSTP263 */
/* ---  Include effects of time displacement, using wind speed magnitude  DSTP264 */
/*     and horizontal perturbation scale size.                           DSTP265 */
/* Computing 2nd power */
    d__1 = *ewwind;
/* Computing 2nd power */
    d__2 = *nswind;
    dtimecor = sqrt(d__1 * d__1 + d__2 * d__2) * *deltime / (hls * 1e3);
/* ---  Unperturbed mean density (approximate if HGTM > ZF)               DSTP267 */
    dens0 = *dens / (wavepert + 1.);
/* ---  HGTM = height above MOLA areoid                                   DSTP269 */
/* ---  HGTMS = height above local MOLA topographic surface               DSTP270 */
    hgtms = hgtm - thgt;
/* ---  Evaluate correlation and step size relative to accuracy limit     DSTP272 */
    if (*iupdate >= 0) {
	*pertstep = *pertstep + abs(delns) + abs(delew) + abs(delz) + abs(
		dtimecor);
    }
    *corlim = -(*pertstep) / log(.995);
    if (*corlim <= *corlmin || *iupdate < 0) {
	correl = 1.;
	corbeta = 0.;
	if (*iupdate < 0) {
	    *iupdate = -1;
	} else {
	    *iupdate = 0;
	}
    } else {
/* ---    Get uniform RANDOM number and Gaussian random number from PPND  DSTP285 */
L480:
	z2 = random_m05__(&l);
	if (l == 1) {
	    goto L480;
	}
	z1 = ppnd_m05__(&z2, &ifault);
	if (ifault == 1) {
	    s_stop(" PPND ERROR", (ftnlen)11);
	}
	correl = exp(-(*pertstep));
/* Computing 2nd power */
	d__1 = correl;
	corbeta = sqrt(1. - d__1 * d__1);
	*pertstep = 0.;
	*iupdate = 1;
    }
    *denshi = *dens * facthi;
    dplus = *denshi - *dens;
    if (factlo <= 0.) {
	factlo = 2. - facthi;
    }
    *denslo = *dens * factlo;
    dminus = *dens - *denslo;
/* ---  Local time in "Martian hours" (1/24th Sols)                       DSTP300 */
    *tlocal = (*sunlon - *clon) / 15. + 12.;
    if (*tlocal < 0.) {
	*tlocal += (float)24.;
    }
    if (*tlocal > 24.) {
	*tlocal += -24.;
    }
/* ---  Output height above MOLA areoid (HGTM) or above local             DSTP304 */
/* ---  terrain (HGTMS)                                                   DSTP305 */
    ohgt = hgtm;
    ohgts = hgtms;
/* ---  Set output heights to terrain height if <= -8.7 km                DSTP308 */
    if (ohgt <= -8.7) {
	ohgt = thgt + *hgtasfc;
	ohgts = *hgtasfc;
    }
/* ---  Current random density perturbation value, correlated with        DSTP313 */
/* ---  previous random density perturbation                              DSTP314 */
    *rhod = correl * *rhod + corbeta * z1;
    if (*rhod < 0.) {
	densrand = *rhod * dminus * datacom_m05__1.rpscale;
    }
    if (*rhod >= 0.) {
	densrand = *rhod * dplus * datacom_m05__1.rpscale;
    }
/* ---  Add random density perturbation                                   DSTP318 */
    *densp = *dens + densrand;
/* ---  Check upper and lower bounds on density perturbations             DSTP320 */
    if (*densp < dens0 * .1) {
	*densp = dens0 * .1;
    }
    if (*densp > dens0 * 10.) {
	*densp = dens0 * 10.;
    }
/* ---  Save as total density, for output                                 DSTP323 */
    *denstot = *densp;
/* ---  Standard deviation in random density perturbation (% of           DSTP325 */
/* ---  unperturbed mean) for output                                      DSTP326 */
    sigd = (d__1 = *denshi - *denslo, abs(d__1)) * 50. / dens0;
    sigd = datacom_m05__1.rpscale * sigd;
/* ---  Standard deviations for wind perturbations                        DSTP329 */
    sigu = chgtc * .1 + 2.;
/* ---  Added contribution to SIGU for near-surface heights               DSTP330a */
    if (ohgts >= 0. && ohgts <= 4.5) {
	sigu += (1. - ohgts / 4.5) * 1.5;
    }
    if (sigu > 25.) {
	sigu = 25.;
    }
    sigu = datacom_m05__1.rwscale * sigu;
    if (sigu > 50.) {
	sigu = 50.;
    }
    sigw = sigu / 5.;
/* ---  Added contribution to SIGW for near-surface heights               DSTP334a */
    if (ohgts >= 0. && ohgts <= 4.5) {
	sigw += (1. - ohgts / 4.5) * 1.5 * datacom_m05__1.rwscale;
    }
/* ---  Adjust random DENSHI, DENSLO for rpscale                          DSTP335 */
    *denshi = *dens + datacom_m05__1.rpscale * (*denshi - *dens);
    *denslo = *dens + datacom_m05__1.rpscale * (*denslo - *dens);
    if (*denslo < dens0 * .1) {
	*denslo = dens0 * .1;
    }
/* ---  Convert random density perturbation to % of (unperturbed) mean    DSTP339 */
    densrand = (*densp - *dens) * 100. / dens0;
/* ---  Compute total density perturbation as % of (unperturbed) mean     DSTP341 */
    *densp = densrand + denswave;
/* ---  Compute EW and NS wind perturbations and total wind               DSTP343 */
L586:
    if (corbeta != 0.) {
	z2 = random_m05__(&l);
	if (l == 1) {
	    goto L586;
	}
	z1 = ppnd_m05__(&z2, &ifault);
    }
    *rhou = correl * *rhou + corbeta * z1;
/* ---  Limit winds to sound speed/Sqrt(2)   (ssposr2)                    DSTP348 */
/*     Assume specific heat ratio = 4/3                                  DSTP349 */
    ssposr2 = sqrt(cp_m05__(temp) * *temp / 6.);
/* ---  Add slope winds, scale mean winds, and limit to 0.7*ssposr2       DSTP351 */
    *ewwind = datacom_m05__1.wmscale * *ewwind + datacom_m05__1.blwinfac * 
	    blwindew;
    *nswind = datacom_m05__1.wmscale * *nswind + datacom_m05__1.blwinfac * 
	    blwindns;
    if (abs(*ewwind) > ssposr2 * .7) {
	d__1 = ssposr2 * .7;
	*ewwind = d_sign(&d__1, ewwind);
    }
    if (abs(*nswind) > ssposr2 * .7) {
	d__1 = ssposr2 * .7;
	*nswind = d_sign(&d__1, nswind);
    }
/* ---  Add slope winds, scale daily mean winds, and limit to 0.7*ssposr2 DSTP358 */
    ewwnday = datacom_m05__1.wmscale * ewwnday + datacom_m05__1.blwinfac * 
	    bluday;
    nswnday = datacom_m05__1.wmscale * nswnday + datacom_m05__1.blwinfac * 
	    blvday;
    if (abs(ewwnday) > ssposr2 * .7) {
	d__1 = ssposr2 * .7;
	ewwnday = d_sign(&d__1, &ewwnday);
    }
    if (abs(nswnday) > ssposr2 * .7) {
	d__1 = ssposr2 * .7;
	nswnday = d_sign(&d__1, &nswnday);
    }
/* ---  EW component of perturbation in wind and total wind               DSTP365 */
    *ewpert = *rhou * sigu;
    ewtot = *ewwind + *ewpert;
    if (abs(ewtot) > ssposr2) {
	ewtot = d_sign(&ssposr2, &ewtot);
	*ewpert = ewtot - *ewwind;
    }
L587:
    if (corbeta != 0.) {
	z2 = random_m05__(&l);
	if (l == 1) {
	    goto L587;
	}
	z1 = ppnd_m05__(&z2, &ifault);
    }
    *rhov = correl * *rhov + corbeta * z1;
/* ---  NS component of perturbation in wind and total wind               DSTP376 */
    *nspert = *rhov * sigu;
    nstot = *nswind + *nspert;
    if (abs(nstot) > ssposr2) {
	nstot = d_sign(&ssposr2, &nstot);
	*nspert = nstot - *nswind;
    }
L588:
    if (corbeta != 0.) {
	z2 = random_m05__(&l);
	if (l == 1) {
	    goto L588;
	}
	z1 = ppnd_m05__(&z2, &ifault);
    }
    *rhow = correl * *rhow + corbeta * z1;
/* ---  Vertical component of perturbation in wind plus slope wind        DSTP387 */
    *vwpert = *rhow * sigw + datacom_m05__1.blwinfac * blwindvert;
/* ---  Compute cosine of solar zenith angle                              DSTP389 */
    cszang = sin(datacom_m05__1.dtr * *sunlat) * sin(datacom_m05__1.dtr * 
	    clatc) + cos(datacom_m05__1.dtr * *sunlat) * cos(
	    datacom_m05__1.dtr * clatc) * cos(datacom_m05__1.dtr * (*sunlon - 
	    *clon));
    *szang = acos(cszang) / datacom_m05__1.dtr;
    f1peak = 999.9;
/* ---  Compute height of F1 peak if solar zenith angle < 90 deg          DSTP394 */
    if (cszang > 0. && zbase < 900.) {
/* ---    relative Air mass                                               DSTP396 */
	d__1 = 93.885 - *szang;
	airmass = 1. / (cszang + pow_dd(&d__1, &c_b80) * .15);
/* ---    F1 peak height (km)                                             DSTP398 */
	f1peak = zbase + hatzf * log(airmass);
    }
/* ---  Write descriptively formatted data on LIST.txt file               DSTP401 */
    elon = 360. - *clon;
/* ---  Total radius (areoid + height)                                    DSTP403 */
    radtotal = careoid + ohgt;
/* ---  Compute height above reference ellipsoid                          DSTP405 */
    oldhgt = radtotal - oldrref;
/* ---  Difference of MOLA areoid from ellipsoid radius                   DSTP407 */
    dareoid = careoid - oldrref;
/* ---  Set output value of local height offset for MGCM or MTGCM data    DSTP409 */
    offsetl = tgcmoffset_m05__1.hgtoffset;
    if (ohgt <= 80.) {
	offsetl = tgcmoffset_m05__1.ofszl;
    }
    if (datacom_m05__1.iup > 0) {
	io___258.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___258);
	do_fio(&c__1, (char *)&(*csec), (ftnlen)sizeof(doublereal));
	d__1 = *csec / 88775.245;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*als), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.dustod, (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&ohgt, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ohgts, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*owlt), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&thgt, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&radtotal, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&careoid, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&oldhgt, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*hscale), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*hrho), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&tgcmoffset_m05__1.ibougher, (ftnlen)sizeof(
		integer));
	do_fio(&c__1, (char *)&offsetl, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&clatc, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*clon), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&elon, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&clatg, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&chgtg, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*sunlat), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*marsau), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*sunlon), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*tlocal), (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    if (datacom_m05__1.iup > 0 && ohgt > 80.) {
	io___259.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___259);
	do_fio(&c__1, (char *)&texos, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&tbase, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&zbase, (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___260.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___260);
	do_fio(&c__1, (char *)&(*szang), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&f1peak, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
/* ---  Compute percent deviations from COSPAR values                     DSTP437 */
    cospar_m05__(&ohgt, &tcos, &pcos, &dcosp);
    if (dcosp <= 0.) {
	devlo = -99.9;
	devav = -99.9;
	devhi = -99.9;
	devtot = -99.9;
	devday = -99.9;
	devmax = -99.9;
	devmin = -99.9;
    } else {
	devlo = (*denslo - dcosp) * 100. / dcosp;
	devav = (*dens - dcosp) * 100. / dcosp;
	devhi = (*denshi - dcosp) * 100. / dcosp;
	devtot = (*denstot - dcosp) * 100. / dcosp;
	devday = (densday - dcosp) * 100. / dcosp;
	if (datacom_m05__1.idaydata > 0) {
	    devmax = (densmax - dcosp) * 100. / dcosp;
	    devmin = (densmin - dcosp) * 100. / dcosp;
	}
    }
    s_copy(densunits, "kg/m**3 ", (ftnlen)8, (ftnlen)8);
/* ---  Convert density units to kg/km**3 if logscale = 3                 DSTP459 */
    if (datacom_m05__1.logscale == 3) {
	*dens *= 1e9;
	*denslo *= 1e9;
	*denshi *= 1e9;
	*denstot *= 1e9;
	densday *= 1e9;
	if (datacom_m05__1.idaydata > 0) {
	    densmax *= 1e9;
	    densmin *= 1e9;
	}
	s_copy(densunits, "kg/km**3", (ftnlen)8, (ftnlen)8);
    }
/* ---  Write formatted output to list file                               DSTP472 */
    if (datacom_m05__1.iup > 0) {
	io___272.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___272);
	do_fio(&c__1, (char *)&(*temp), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*pres), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&profwgt, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denslo), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*dens), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denshi), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, densunits, (ftnlen)8);
	do_fio(&c__1, (char *)&devlo, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&devav, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&devhi, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*iupdate), (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&(*denstot), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, densunits, (ftnlen)8);
	do_fio(&c__1, (char *)&(*densp), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&denswave, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*ewwind), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*ewpert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ewtot, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*nswind), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*nspert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&nstot, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*vwpert), (ftnlen)sizeof(doublereal));
	e_wsfe();
	species_m05__(&ohgt, &clatc, als, &zbase, &amz, dens, pres, temp, &
		datacom_m05__1.iup, therm_m05__1.fmol, fmass, &fmolh2o, &
		fmassh2o);
	if (*i__ > 0 && *corlim < 1. && abs(*clat) < 89.99) {
	    io___276.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___276);
	    do_fio(&c__1, (char *)&(*corlim), (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	io___277.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___277);
	e_wsfe();
    }
    presmb = *pres / (float)100.;
    sigmalevel = *pres / patsurf;
    preshgt = -(*hscale) * log(sigmalevel);
/* ---  Compute dust density variables by methods of Haberle et al.,      DSTP498 */
/*      Icarus, 50, 322 (1982) and Haberle et al., J. Geophys. Res.,     DSTP499 */
/*      104, 8957 (1999)                                                 DSTP500 */
/* ---  Dust column areal density (kg/m**2)                               DSTP501 */
    dareaden = datacom_m05__1.dustod * .005;
/* ---  Dust mixing ratio (kg dust/kg air) at surface                     DSTP503 */
    qsurf = dareaden * gz / (exp(-datacom_m05__1.dustnu) * .994 * patsurf);
/* ---  Dust mixing ratio at current position and pressure                DSTP505 */
    dmixrat = 0.;
    expfact = datacom_m05__1.dustnu * (1. - 1. / sigmalevel);
    if (expfact > -85.) {
	dmixrat = qsurf * exp(expfact);
    }
/* ---  Dust mass density (micrograms dust / m**3)                        DSTP509 */
    dmasden = dmixrat * 1e9 * *dens;
    if (datacom_m05__1.logscale == 3) {
	dmasden /= 1e9;
    }
/* ---  Dust number density (number dust particles / m**3)                DSTP511 */
/* Computing 3rd power */
    d__1 = datacom_m05__1.dustdiam;
    dnumden = dmasden / (datacom_m05__1.dustdens * 5.23599e-10 * (d__1 * (
	    d__1 * d__1)));
    if (datacom_m05__1.nvarx == 9) {
	varx = presmb;
    }
    if (datacom_m05__1.nvary == 9) {
	vary = presmb;
    }
    if (datacom_m05__1.nvarx == 10) {
	varx = preshgt;
    }
    if (datacom_m05__1.nvary == 10) {
	vary = preshgt;
    }
    if (datacom_m05__1.nvarx == 11) {
	varx = sigmalevel;
    }
    if (datacom_m05__1.nvary == 11) {
	vary = sigmalevel;
    }
    if (datacom_m05__1.nvarx == 12) {
	varx = oldhgt;
    }
    if (datacom_m05__1.nvary == 12) {
	vary = oldhgt;
    }
    if (datacom_m05__1.nvarx == 13) {
	varx = chgtg;
    }
    if (datacom_m05__1.nvary == 13) {
	vary = chgtg;
    }
    if (datacom_m05__1.nvarx == 14) {
	varx = clatg;
    }
    if (datacom_m05__1.nvary == 14) {
	vary = clatg;
    }
/* ---  Output deviations from COSPAR if logscale = 2                     DSTP525 */
    if (datacom_m05__1.logscale == 2) {
	*denslo = devlo;
	*dens = devav;
	*denshi = devhi;
	*denstot = devtot;
	densday = devday;
	densmax = devmax;
	densmin = devmin;
	if (pcos <= 0.) {
	    *pres = -99.9;
	    presday = -99.9;
	} else {
	    *pres = (*pres - pcos) * 100. / pcos;
	    presday = (presday - pcos) * 100. / pcos;
	}
    }
/* ---  Write parameters on plot format files                             DSTP542 */
    if (datacom_m05__1.nvarx == 1) {
	varx = ohgt;
    }
    if (datacom_m05__1.nvarx == 2) {
	varx = ohgts;
    }
    if (datacom_m05__1.nvarx == 3) {
	varx = clatc;
    }
    if (datacom_m05__1.nvarx == 4) {
	varx = *clon;
	if (*lonew == 1) {
	    varx = 360. - *clon;
	}
    }
    if (datacom_m05__1.nvarx == 15) {
	varx = *clon;
	if (varx > 180.) {
	    varx += -360.;
	}
	if (*lonew == 1) {
	    varx = -varx;
	}
    }
    if (datacom_m05__1.nvarx == 5) {
	varx = *csec;
    }
    if (datacom_m05__1.nvarx == 6) {
	varx = *csec / 88775.245;
    }
    if (datacom_m05__1.nvarx == 7) {
	varx = *als;
    }
    if (datacom_m05__1.nvarx == 8) {
	varx = *tlocal;
    }
    alogdens = 0.;
    if (datacom_m05__1.logscale != 2) {
	alogdens = d_lg10(dens);
    }
    if (datacom_m05__1.logscale == 1) {
	*dens = alogdens;
	*pres = d_lg10(pres);
	*denslo = d_lg10(denslo);
	*denshi = d_lg10(denshi);
	*denstot = d_lg10(denstot);
	if (ohgt <= topz) {
	    densday = d_lg10(&densday);
	    presday = d_lg10(&presday);
	    if (datacom_m05__1.idaydata > 0) {
		densmax = d_lg10(&densmax);
		densmin = d_lg10(&densmin);
	    }
	}
    }
    if (datacom_m05__1.nvary == 0 && datacom_m05__1.iup > 0) {
	s_wsfe(&io___290);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denslo), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*dens), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denshi), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denstot), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.dustod, (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&radtotal, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&gz, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*marsau), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.logscale, (ftnlen)sizeof(
		integer));
	do_fio(&c__1, (char *)&offsetl, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&tgcmoffset_m05__1.ibougher, (ftnlen)sizeof(
		integer));
	do_fio(&c__1, (char *)&datacom_m05__1.mapyear, (ftnlen)sizeof(integer)
		);
	do_fio(&c__1, (char *)&profwgt, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___291);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sigd, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&densrand, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&denswave, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*densp), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*corlim), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sigu, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sigw, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*iupdate), (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___292);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*ewwind), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*ewpert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ewtot, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*nswind), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*nspert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&nstot, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*vwpert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*iupdate), (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___293);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*temp), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*pres), (ftnlen)sizeof(doublereal));
	d__1 = *temp - 273.15;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&presmb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*hrho), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*hscale), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&amz, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&thgt, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&tgrnd, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&careoid, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dareoid, (ftnlen)sizeof(doublereal));
	do_fio(&c__9, (char *)&therm_m05__1.fmol[0], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&fmolh2o, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.logscale, (ftnlen)sizeof(
		integer));
	e_wsfe();
	if (ohgt <= topz && *profnear <= (float)0.) {
	    s_wsfe(&io___294);
	    do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tempday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&presday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&densday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ewwnday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&nswnday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tempmin, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tempmax, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&densmin, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&densmax, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.logscale, (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&(*dens), (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	if (ohgt > 80.) {
	    s_wsfe(&io___295);
	    do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tbase, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&zbase, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&f1peak, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&amz, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&texos, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tgcmoffset_m05__1.hgtoffset, (ftnlen)
		    sizeof(doublereal));
	    do_fio(&c__1, (char *)&tgcmoffset_m05__1.ibougher, (ftnlen)sizeof(
		    integer));
	    e_wsfe();
	}
	s_wsfe(&io___296);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&talb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&cszang, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dareaden, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dmixrat, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dmasden, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dnumden, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&icepolar, (ftnlen)sizeof(integer));
	e_wsfe();
    } else if (datacom_m05__1.iup > 0) {
	if (datacom_m05__1.nvary == 1) {
	    vary = ohgt;
	}
	if (datacom_m05__1.nvary == 2) {
	    vary = ohgts;
	}
	if (datacom_m05__1.nvary == 3) {
	    vary = clatc;
	}
	if (datacom_m05__1.nvary == 4) {
	    vary = *clon;
	    if (*lonew == 1) {
		vary = 360. - *clon;
	    }
	}
	if (datacom_m05__1.nvary == 15) {
	    vary = *clon;
	    if (vary > 180.) {
		vary += -360.;
	    }
	    if (*lonew == 1) {
		vary = -vary;
	    }
	}
	if (datacom_m05__1.nvary == 5) {
	    vary = *csec;
	}
	if (datacom_m05__1.nvary == 6) {
	    vary = *csec / 88775.245;
	}
	if (datacom_m05__1.nvary == 7) {
	    vary = *als;
	}
	if (datacom_m05__1.nvary == 8) {
	    vary = *tlocal;
	}
	s_wsfe(&io___297);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vary, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denslo), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*dens), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denshi), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*denstot), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.dustod, (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&radtotal, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&gz, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*marsau), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.logscale, (ftnlen)sizeof(
		integer));
	do_fio(&c__1, (char *)&offsetl, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&tgcmoffset_m05__1.ibougher, (ftnlen)sizeof(
		integer));
	do_fio(&c__1, (char *)&datacom_m05__1.mapyear, (ftnlen)sizeof(integer)
		);
	do_fio(&c__1, (char *)&profwgt, (ftnlen)sizeof(doublereal));
	e_wsfe();
	s_wsfe(&io___298);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vary, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sigd, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&densrand, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&denswave, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*densp), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*corlim), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sigu, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&sigw, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*iupdate), (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___299);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vary, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*ewwind), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*ewpert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&ewtot, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*nswind), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*nspert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&nstot, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*vwpert), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*iupdate), (ftnlen)sizeof(integer));
	e_wsfe();
	s_wsfe(&io___300);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vary, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*temp), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*pres), (ftnlen)sizeof(doublereal));
	d__1 = *temp - 273.15;
	do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&presmb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*hrho), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*hscale), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&amz, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&thgt, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&tgrnd, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&careoid, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dareoid, (ftnlen)sizeof(doublereal));
	do_fio(&c__9, (char *)&therm_m05__1.fmol[0], (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&fmolh2o, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.logscale, (ftnlen)sizeof(
		integer));
	e_wsfe();
	if (ohgt <= topz && *profnear <= (float)0.) {
	    s_wsfe(&io___301);
	    do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vary, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tempday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&presday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&densday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&ewwnday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&nswnday, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tempmin, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tempmax, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&densmin, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&densmax, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.logscale, (ftnlen)sizeof(
		    integer));
	    do_fio(&c__1, (char *)&(*dens), (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
	if (ohgt > 80.) {
	    s_wsfe(&io___302);
	    do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&vary, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tbase, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&zbase, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&f1peak, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&amz, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&texos, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&tgcmoffset_m05__1.hgtoffset, (ftnlen)
		    sizeof(doublereal));
	    do_fio(&c__1, (char *)&tgcmoffset_m05__1.ibougher, (ftnlen)sizeof(
		    integer));
	    e_wsfe();
	}
	s_wsfe(&io___303);
	do_fio(&c__1, (char *)&varx, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&vary, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&talb, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&cszang, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dareaden, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dmixrat, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dmasden, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&dnumden, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&icepolar, (ftnlen)sizeof(integer));
	e_wsfe();
    }
/* ---  Write non-descriptively formatted data on OUTPUT file             DSTP641 */
    if (datacom_m05__1.molahgts == 1) {
	var = ohgt;
	if (datacom_m05__1.nvarx == 2 || datacom_m05__1.nvary == 2) {
	    var = ohgts;
	}
	olat = clatc;
    } else {
	var = oldhgt;
	olat = clatc;
	if (datacom_m05__1.ipclat != 1) {
	    var = chgtg;
	    olat = clatg;
	}
    }
    if (datacom_m05__1.iup > 0) {
	olon = *clon;
	if (*lonew == 1) {
	    olon = 360. - *clon;
	}
	if (datacom_m05__1.nvarx == 15 || datacom_m05__1.nvary == 15) {
	    if (olon > 180.) {
		olon += -360.;
	    }
	}
	if (datacom_m05__1.logscale == 0 || datacom_m05__1.logscale == 3) {
	    s_wsfe(&io___307);
	    do_fio(&c__1, (char *)&(*csec), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&var, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&olat, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&olon, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*dens), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*temp), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*ewwind), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*nswind), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&sigd, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*als), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.dustod, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__9, (char *)&fmass[0], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&fmassh2o, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	} else {
	    s_wsfe(&io___308);
	    do_fio(&c__1, (char *)&(*csec), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&var, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&olat, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&olon, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*dens), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*temp), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*ewwind), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*nswind), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&sigd, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*als), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.dustod, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__9, (char *)&fmass[0], (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&fmassh2o, (ftnlen)sizeof(doublereal));
	    e_wsfe();
	}
    }
    return 0;
L999:
    *eof = 1;
    if (datacom_m05__1.npos <= 0) {
	al__1.aerr = 0;
	al__1.aunit = 7;
	f_rew(&al__1);
    }
    return 0;
L9998:
    s_stop(" Error termination reading trajectory data file!", (ftnlen)48);
    return 0;
} /* datastep_m05__ */

/* -----------------------------------------------------------------------DSTP676 */
doublereal dustvsls_m05__(doublereal *als, doublereal *dustmin, doublereal *
	dustmax)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double atan(doublereal), sin(doublereal);

    /* Local variables */
    static doublereal pi180;

/* ---  Assumed seasonal variation (versus solar angle Ls) for non-       DVLS  2 */
/*     dust-storm optical depth (Dustmin at Ls=90; Dustmax at Ls=270)    DVLS  3 */
    pi180 = atan(1.) / 45.;
    ret_val = (*dustmax + *dustmin - (*dustmax - *dustmin) * sin(pi180 * *als)
	    ) / 2.;
    return ret_val;
} /* dustvsls_m05__ */

/* -----------------------------------------------------------------------DVLS 10 */
/* Subroutine */ int escalc_m05__(doublereal *stdl, doublereal *sigma, 
	doublereal *es)
{
    /* Initialized data */

    static doublereal eps[12] = { 0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0.,0. };

    static integer i__;
    static doublereal sig[12];

/* ---  EPS ARE STD VARNS FROM NOMINAL VALUES                             ESCL  2 */
/* ---  0,2,....10  LONG - TERM                                           ESCL  3 */
/* ---  1,3.....,11  SHORT-TERM                                           ESCL  4 */
    for (i__ = 2; i__ <= 10; i__ += 2) {
	eps[i__] = *stdl;
/* L10: */
    }
    for (i__ = 3; i__ <= 9; i__ += 2) {
	eps[i__] = *sigma;
/* L20: */
    }
    sig[0] = .25;
/* ---  LONG TERM -FBAR                                                   ESCL 15 */
/* ---  THESE ARE COEF. OF VARIATION, I.E. SIGMA/MU,    %/100.0           ESCL 16 */
    sig[1] = 0.;
/* ---  SHORT TERM-FBAR                                                   ESCL 18 */
    sig[2] = .16;
/* ---  LONG TERM-TINF                                                    ESCL 20 */
    sig[3] = .12;
/* ---  SHORT TERM-TINF                                                   ESCL 22 */
    sig[4] = .4;
/* ---  LONG TERM - FOXY                                                  ESCL 24 */
    sig[5] = .12;
/* ---  SHORT TERM -FOXY                                                  ESCL 26 */
    sig[6] = 0.;
/* ---  LONG TERM -AOXY                                                   ESCL 28 */
    sig[7] = .21;
/* ---  SHORT TERM -AOXY                                                  ESCL 30 */
    sig[8] = .045;
/* ---  LONG TERM - ZF                                                    ESCL 32 */
    sig[9] = .0225;
/* ---  SHORT TERM - ZF                                                   ESCL 34 */
    sig[10] = .3;
/* ---  LONG TERM- DZDUST                                                 ESCL 36 */
    sig[11] = 0.;
/* ---  SHORT TERM - DZDUST                                               ESCL 38 */
    for (i__ = 0; i__ <= 11; ++i__) {
	es[i__] = eps[i__] * sig[i__];
/* L100: */
    }
    return 0;
} /* escalc_m05__ */

/* -----------------------------------------------------------------------ESCL 44 */
/* Subroutine */ int fourd_m05__(doublereal *dx, doublereal *dy, doublereal *
	dz, doublereal *dq, doublereal *array, doublereal *value, integer *
	lint)
{
    /* Builtin functions */
    double log(doublereal), exp(doublereal);

    /* Local variables */
    static doublereal dqp, dxp, dyp, dzp;

/* ---    4-Dimensional linear interpolation within a 1x1x1x1 hypercube   FORD  2 */
/*       (x,y,z,q) of Array(2,2,2,2) to position dx,dy,dz,dq (all 0-1).  FORD  3 */
/*       Value is value of interpolated output.                          FORD  4 */
/* ---    Complementary displacements in x,y,z,q                          FORD  7 */
    /* Parameter adjustments */
    array -= 15;

    /* Function Body */
    dxp = 1. - *dx;
    dyp = 1. - *dy;
    dzp = 1. - *dz;
    dqp = 1. - *dq;
    if (*lint != 1) {
/* ---      Compute 4-dimensional linear interpolated value               FORD 12 */
	*value = dxp * dyp * dzp * dqp * array[15] + dxp * dyp * dzp * *dq * 
		array[23] + dxp * dyp * *dz * dqp * array[19] + dxp * *dy * 
		dzp * dqp * array[17] + *dx * dyp * dzp * dqp * array[16] + 
		dxp * dyp * *dz * *dq * array[27] + dxp * *dy * dzp * *dq * 
		array[25] + dxp * *dy * *dz * dqp * array[21] + *dx * dyp * 
		dzp * *dq * array[24] + *dx * dyp * *dz * dqp * array[20] + *
		dx * *dy * dzp * dqp * array[18] + dxp * *dy * *dz * *dq * 
		array[29] + *dx * dyp * *dz * *dq * array[28] + *dx * *dy * 
		dzp * *dq * array[26] + *dx * *dy * *dz * dqp * array[22] + *
		dx * *dy * *dz * *dq * array[30];
    } else {
/* ---      Compute 4-dimensional linear interpolated value               FORD 28b */
	*value = exp(dxp * dyp * dzp * dqp * log(array[15]) + dxp * dyp * dzp 
		* *dq * log(array[23]) + dxp * dyp * *dz * dqp * log(array[19]
		) + dxp * *dy * dzp * dqp * log(array[17]) + *dx * dyp * dzp *
		 dqp * log(array[16]) + dxp * dyp * *dz * *dq * log(array[27])
		 + dxp * *dy * dzp * *dq * log(array[25]) + dxp * *dy * *dz * 
		dqp * log(array[21]) + *dx * dyp * dzp * *dq * log(array[24]) 
		+ *dx * dyp * *dz * dqp * log(array[20]) + *dx * *dy * dzp * 
		dqp * log(array[18]) + dxp * *dy * *dz * *dq * log(array[29]) 
		+ *dx * dyp * *dz * *dq * log(array[28]) + *dx * *dy * dzp * *
		dq * log(array[26]) + *dx * *dy * *dz * dqp * log(array[22]) 
		+ *dx * *dy * *dz * *dq * log(array[30]));
    }
    return 0;
} /* fourd_m05__ */

/* -----------------------------------------------------------------------FORD 31 */
/* Subroutine */ int mgcmterp_m05__(integer *khgt, doublereal *time, 
	doublereal *tmgcm, doublereal *pmgcm, doublereal *dmgcm, doublereal *
	umgcm, doublereal *vmgcm, doublereal *tempday, doublereal *presday, 
	doublereal *densday, doublereal *uwndday, doublereal *vwndday, 
	doublereal *tempmax, doublereal *tempmin, doublereal *densmax, 
	doublereal *densmin, integer *idaydata)
{
    static doublereal dmin__[8]	/* was [2][2][2] */, dmax__[8]	/* was [2][2][
	    2] */, tday[8]	/* was [2][2][2] */, pday[8]	/* was [2][2][
	    2] */, uday[8]	/* was [2][2][2] */, vday[8]	/* was [2][2][
	    2] */, tmin[8]	/* was [2][2][2] */, tmax[8]	/* was [2][2][
	    2] */, upolefac;
    extern doublereal tidex_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), tidey_m05__(doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    static integer i__, l, m;
    static doublereal dtime, rmgcm;
    static integer itime;
    static doublereal ptime;
    extern /* Subroutine */ int threed_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static doublereal ttime, d0, a1, xtime, a2, p0, p1, r0[8]	/* was [2][2][
	    2] */, p2, t0, u0, v0, pm[8]	/* was [2][2][2] */, tm[8]	
	    /* was [2][2][2] */, um[8]	/* was [2][2][2] */, vm[8]	/* 
	    was [2][2][2] */, a1p, a2p, a1t, a2t, p1p, p2p, p1t, p2t, polefac;

/* ---    Interpolates Ames Mars General Circulation Model (MGCM) data    GTRP  4 */
/*       to a given latitude, time of year (Ls), and dust optical        GTRP  5 */
/*       depth, for a given height index (khgt) and time of day (time).  GTRP  6 */
/*       Some input data is provided by the Common "Interp".             GTRP  7 */
/* ---    Set parameter values for number of heights, latitudes, and      GTRP  8 */
/*       number of dust optical depth values                             GTRP  9 */
/* ---    MGCM 0-80 km data arrays for interpolation                      GTRP 14 */
/* ---    Establish MGCM values at corners of a 3-dimensional cube in     GTRP 31 */
/*       latitude-Ls-dust space, at the given height index (khgt), and   GTRP 32 */
/*       time of day (time)                                              GTRP 33 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	upolefac = 1.;
	if (interp_m05__1.ilat == 1) {
	    polefac = i__ - 1.;
	} else if (interp_m05__1.ilat == 24) {
	    polefac = 2. - i__;
	}
	if (interp_m05__1.ilatw == 2) {
	    if (i__ == 1) {
		upolefac = interp_m05__1.wpolefac;
	    }
	} else if (interp_m05__1.ilatw == 24) {
	    if (i__ == 2) {
		upolefac = interp_m05__1.wpolefac;
	    }
	}
	for (l = 1; l <= 2; ++l) {
	    for (m = 1; m <= 2; ++m) {
/* ---      Daily mean temperature                                        GTRP 49 */
		t0 = mgcmdata_m05__1.tza0[*khgt + (interp_m05__1.ilat + i__ - 
			1 + (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			+ m - 1) * 13) * 25) * 17 - 5543];
		tday[i__ + (l + (m << 1) << 1) - 7] = t0;
/* ---      Temperature tide amplitudes and phases                        GTRP 52 */
		a1t = mgcmdata_m05__1.tza1[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* polefac;
		p1t = mgcmdata_m05__1.tzp1[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
		a2t = mgcmdata_m05__1.tza2[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* polefac;
		p2t = mgcmdata_m05__1.tzp2[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
/* ---      Temperature at corners of 3-D cube                            GTRP 57 */
		tm[i__ + (l + (m << 1) << 1) - 7] = tidex_m05__(&t0, &a1t, &
			p1t, &a2t, &p2t, time);
/* ---      Daily mean pressure                                           GTRP 59 */
		p0 = mgcmdata_m05__1.pza0[*khgt + (interp_m05__1.ilat + i__ - 
			1 + (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			+ m - 1) * 13) * 25) * 17 - 5543];
		pday[i__ + (l + (m << 1) << 1) - 7] = p0;
/* ---      Pressure tide amplitudes and phases                           GTRP 62 */
		a1p = mgcmdata_m05__1.pza1[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* polefac;
		p1p = mgcmdata_m05__1.pzp1[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
		a2p = mgcmdata_m05__1.pza2[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* polefac;
		p2p = mgcmdata_m05__1.pzp2[*khgt + (interp_m05__1.ilat + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
/* ---      Pressure at corners of 3-D cube                               GTRP 67 */
		pm[i__ + (l + (m << 1) << 1) - 7] = tidey_m05__(&p0, &a1p, &
			p1p, &a2p, &p2p, time);
/* ---      Daily average density D0                                      GTRP 69 */
		d0 = mgcmdata_m05__1.dza0[*khgt + (interp_m05__1.ilat + i__ - 
			1 + (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			+ m - 1) * 13) * 25) * 17 - 5543];
/* ---      Gas constant from pressure, density and temperature           GTRP 71 */
		r0[i__ + (l + (m << 1) << 1) - 7] = 190.;
		if (t0 != 0. && d0 != 0.) {
		    r0[i__ + (l + (m << 1) << 1) - 7] = p0 / (t0 * d0);
		}
/* ---      Max and Min temperature and density at corners of 3-D cube    GTRP 74 */
		tmax[i__ + (l + (m << 1) << 1) - 7] = -9999.;
		tmin[i__ + (l + (m << 1) << 1) - 7] = 9999.;
		dmax__[i__ + (l + (m << 1) << 1) - 7] = -9999.;
		dmin__[i__ + (l + (m << 1) << 1) - 7] = 9999.;
		if (*idaydata > 0) {
		    for (itime = 0; itime <= 23; ++itime) {
			xtime = (real) itime;
			ttime = tidex_m05__(&t0, &a1t, &p1t, &a2t, &p2t, &
				xtime);
			ptime = tidey_m05__(&p0, &a1p, &p1p, &a2p, &p2p, &
				xtime);
			dtime = ptime / (r0[i__ + (l + (m << 1) << 1) - 7] * 
				ttime);
			if (ttime > tmax[i__ + (l + (m << 1) << 1) - 7]) {
			    tmax[i__ + (l + (m << 1) << 1) - 7] = ttime;
			}
			if (ttime < tmin[i__ + (l + (m << 1) << 1) - 7]) {
			    tmin[i__ + (l + (m << 1) << 1) - 7] = ttime;
			}
			if (dtime > dmax__[i__ + (l + (m << 1) << 1) - 7]) {
			    dmax__[i__ + (l + (m << 1) << 1) - 7] = dtime;
			}
			if (dtime < dmin__[i__ + (l + (m << 1) << 1) - 7]) {
			    dmin__[i__ + (l + (m << 1) << 1) - 7] = dtime;
			}
/* L50: */
		    }
		}
/* ---      Daily mean EW wind                                            GTRP 91 */
		u0 = mgcmdata_m05__1.uza0[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
		uday[i__ + (l + (m << 1) << 1) - 7] = u0;
/* ---      EW wind tide amplitudes and phases                            GTRP 94 */
		a1 = mgcmdata_m05__1.uza1[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* upolefac;
		p1 = mgcmdata_m05__1.uzp1[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
		a2 = mgcmdata_m05__1.uza2[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* upolefac;
		p2 = mgcmdata_m05__1.uzp2[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
/* ---      EW wind at corners of 3-D cube                                GTRP 99 */
		um[i__ + (l + (m << 1) << 1) - 7] = tidex_m05__(&u0, &a1, &p1,
			 &a2, &p2, time);
/* ---      Daily mean NS wind                                            GTRP101 */
		v0 = mgcmdata_m05__1.vza0[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
		vday[i__ + (l + (m << 1) << 1) - 7] = v0;
/* ---      NS wind tide amplitudes and phases                            GTRP104 */
		a1 = mgcmdata_m05__1.vza1[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* upolefac;
		p1 = mgcmdata_m05__1.vzp1[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
		a2 = mgcmdata_m05__1.vza2[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543] 
			* upolefac;
		p2 = mgcmdata_m05__1.vzp2[*khgt + (interp_m05__1.ilatw + i__ 
			- 1 + (interp_m05__1.ls + l - 1 + (
			interp_m05__1.mdust + m - 1) * 13) * 25) * 17 - 5543];
/* ---      NS wind at corners of 3-D cube                                GTRP109 */
		vm[i__ + (l + (m << 1) << 1) - 7] = tidex_m05__(&v0, &a1, &p1,
			 &a2, &p2, time);
/* L100: */
	    }
/* L101: */
	}
/* L102: */
    }
/* ---    Use 3-D interpolation to get temperature, pressure, gas         GTRP114 */
/*       constant, EW wind, and NS wind at given latitude, Ls, and       GTRP115 */
/*       dust optical depth                                              GTRP116 */
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, tm, tmgcm, &c__0);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, tday, tempday, &c__0);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, tmax, tempmax, &c__0);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, tmin, tempmin, &c__0);
    if (*idaydata == 1) {
	threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
		interp_m05__1.ddust, dmax__, densmax, &c__1);
    } else {
	threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
		interp_m05__1.ddust, dmax__, densmax, &c__0);
    }
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, dmin__, densmin, &c__1);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, pm, pmgcm, &c__1);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, pday, presday, &c__1);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, r0, &rmgcm, &c__0);
    threed_m05__(&interp_m05__1.dlatw, &interp_m05__1.dls, &
	    interp_m05__1.ddust, um, umgcm, &c__0);
    threed_m05__(&interp_m05__1.dlatw, &interp_m05__1.dls, &
	    interp_m05__1.ddust, uday, uwndday, &c__0);
    threed_m05__(&interp_m05__1.dlatw, &interp_m05__1.dls, &
	    interp_m05__1.ddust, vm, vmgcm, &c__0);
    threed_m05__(&interp_m05__1.dlatw, &interp_m05__1.dls, &
	    interp_m05__1.ddust, vday, vwndday, &c__0);
/* ---    Compute density from temperature, pressure, and gas constant    GTRP130 */
    *dmgcm = *pmgcm / (rmgcm * *tmgcm);
    *densday = *presday / (rmgcm * *tempday);
    return 0;
} /* mgcmterp_m05__ */

/* -----------------------------------------------------------------------GTRP135 */
integer ifloor_m05__(doublereal *x)
{
    /* System generated locals */
    integer ret_val;

    /* Local variables */
    static integer iflr;

/* ---    Integer floor function, greatest integer <= x (provided for     IFLR  2 */
/*       compilers that do not have this intrinsic function)             IFLR  3 */
    iflr = (integer) (*x);
    if ((doublereal) iflr > *x) {
	--iflr;
    }
    ret_val = iflr;
    return ret_val;
} /* ifloor_m05__ */

/* -----------------------------------------------------------------------IFLR 10 */
/* Subroutine */ int marsephm_m05__(doublereal *xday, doublereal *sunlat, 
	doublereal *sunlon, doublereal *sunlsubs, doublereal *radius, 
	doublereal *owlt, doublereal *eot)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double atan(doublereal), sin(doublereal), asin(doublereal), cos(
	    doublereal), tan(doublereal), sqrt(doublereal), d_int(doublereal *
	    );

    /* Local variables */
    static doublereal eqcenter, dlat1, dlat2, dlat3, trueanom, anom0, anomm, 
	    alphs, siday, anlon0, vmday0, ge, re, dt, xe, ye, vm, alsrad, 
	    helone;
    extern /* Subroutine */ int rescale_m05__(doublereal *);
    static doublereal argper, coslat, yranom, vm0;
    extern /* Subroutine */ int perturb_m05__(doublereal *, doublereal *);
    static doublereal yrtrop, ecc, perlon0, veqlon0, veqlon1, inc, obl, pbs, 
	    pmr, xpl, ypl, zpl, alphfms, helilon, ecc2, ecc3, ecc4, ecc5, 
	    ecc6;
    extern /* Subroutine */ int shiftdif_m05__(doublereal *);
    static doublereal rad0, pi180;

/* ---  Computes sunlat, sunlon= latitude and longitude of sub-solar      MEPH  3 */
/*     point on the surface, sunLsubs= areocentric longitude of the Sun  MEPH  4 */
/*     (Ls), radius= current orbital radius from Sun to Mars, heliolon=  MEPH  5 */
/*     Mars heliocentric longitude, owlt= Mars-Earth one-way light       MEPH  6 */
/*     time (minutes), and EOT= equation of time (deg), calculated from  MEPH  7 */
/*     Julian day and time, xday.  Notes: input xday is NOT UTC, but     MEPH  8 */
/*     Terrestrial (Dynamical) Mars-Event Time (NOT Earth-Receive Time). MEPH  9 */
/*     Mars Local Mean Solar Time (hrs ) = Local True Solar Time (hrs)   MEPH 10 */
/*     minus EOT (in hrs). Output is for Terrestrial (Dynamical)         MEPH 11 */
/*     Mars Event Time (corresponding to input xday).                    MEPH 12 */
/*                                                                       MEPH 13 */
/*     Equations for "moderately accurate" Mars solar time, seasonal     MEPH 14 */
/*     parameters, and one-way Mars-Earth light time, from Allison and   MEPH 15 */
/*     McEwen, Planet. Space Sci., 48, 215-235 (2000), and Allison       MEPH 16 */
/*     Geophys Res. Lett., 24(16), 1967-1970 (1997).                     MEPH 17 */
/*                                                                       MEPH 18 */
    pi180 = atan(1.) / 45.;
/* ---  Days since 2000 January 1.5                                       MEPH 28 */
    dt = *xday - 2451545.;
/*                                                                       MEPH 30 */
/* ---------------------------------------------------------------------  MEPH 31 */
/*                                                                       MEPH 32 */
/* ---  Planetary orbit parameters                                        MEPH 33 */
/*                                                                       MEPH 34 */
/*     Semi-major axis (AU) = mean distance from Sun                     MEPH 35 */
    rad0 = 1.52368;
/*     Anomalistic year (days, perihelion-to-perihelion)                 MEPH 37 */
    yranom = 686.9957;
/*     Tropical year (days, for rate of fictitious mean sun)             MEPH 39 */
    yrtrop = 686.9726;
/*     Mean anomaly for J2000 (degrees)                                  MEPH 41 */
    anom0 = 19.387;
/*     Heliocentric longitude of perihelion at J2000 (deg)               MEPH 43 */
    perlon0 = 336.0602;
/*     Terms for heliocentric longitude at Ls=0 (deg)                    MEPH 45 */
    veqlon0 = 85.061;
    veqlon1 = 5.5e-6;
/*     Eccentricity and powers                                           MEPH 48 */
    ecc = dt * 2.477e-9 + .0934;
/* Computing 2nd power */
    d__1 = ecc;
    ecc2 = d__1 * d__1;
    ecc3 = ecc2 * ecc;
    ecc4 = ecc3 * ecc;
    ecc5 = ecc4 * ecc;
    ecc6 = ecc5 * ecc;
/*     Obliquity angle (radians)                                         MEPH 55 */
    obl = (dt * 3.45e-7 + 25.1919) * pi180;
/*     Inclination (radians)                                             MEPH 57 */
    inc = (1.8497 - dt * 2.23e-7) * pi180;
/*     Longitude of ascending node at J2000 (deg)                        MEPH 59 */
    anlon0 = 49.5581;
/*     Sidereal period of rotation (Earth days)                          MEPH 61 */
/* ---  Empirical correction in last digit, to agree with Horizons        MEPH 62 */
    siday = 1.025956749;
/*     Heliocentric lon of prime meridian (deg) at Julian day Vmday0     MEPH 64 */
    vm0 = 133.476;
/* ---  Empirical correction for agreement with Horizons data             MEPH 66 */
    vm0 += -.09746;
    vmday0 = 2451545.;
/*     Difference terms, planetocentric to planetographic lat (deg)      MEPH 69 */
    dlat1 = .269;
    dlat2 = .003;
    dlat3 = .008;
/*                                                                       MEPH 73 */
/* ---------------------------------------------------------------------  MEPH 74 */
/*                                                                       MEPH 75 */
/* ---  Mean anomaly (radians)                                            MEPH 76 */
/* ---  Allison & McEwen (2000) equation (16)                             MEPH 77 */
    anomm = (anom0 + 360. / yranom * dt) * pi180;
/* ---  Right ascension of fictitious mean sun (deg)                      MEPH 79 */
/* ---  Allison & McEwen (2000) equation (17)                             MEPH 80 */
    alphfms = perlon0 - veqlon0 + anom0 + 360. / yrtrop * dt;
/* ---  Mars equation of center, A&M eqn. (4) (degrees)                   MEPH 82 */
    eqcenter = ((ecc * 2. - ecc3 * .25 + ecc5 * .052083333333333336) * sin(
	    anomm) + (ecc2 * 1.25 - ecc4 * .45833333333333331 + ecc6 * 
	    .088541666666666671) * sin(anomm * 2.) + (ecc3 * 
	    1.0833333333333333 - ecc5 * .68253968253968256) * sin(anomm * 3.) 
	    + (ecc4 * 1.0729166666666667 - ecc6 * .93958333333333333) * sin(
	    anomm * 4.) + ecc5 * 1.1427083333333334 * sin(anomm * 5.) + ecc6 *
	     12.836458333333333 * sin(anomm * 6.)) / pi180;
/* ---  True areocentric solar longitude (Ls), A&M eqns. (2) and (4)      MEPH 92 */
    *sunlsubs = alphfms + eqcenter;
/* ---  Add perturbations due to Jupiter, Earth, and Venus, A&M eqns.     MEPH 94 */
/*     (18) and (19)                                                     MEPH 95 */
    perturb_m05__(&dt, &pbs);
    *sunlsubs += pbs;
    rescale_m05__(sunlsubs);
/* ---  Ls angle in radians                                               MEPH 99 */
    alsrad = *sunlsubs * pi180;
/* ---  Sub-solar latitude of sun (planetographic solar declination),     MEPH101 */
/*     Allison (1997) eqn. (5) with empirical Ls and 3*Ls terms          MEPH102 */
    *sunlat = asin(sin(obl) * sin(alsrad)) / pi180 + dlat1 * sin(alsrad) + 
	    dlat2 * cos(alsrad) + dlat3 * sin(alsrad * 3.);
/* ---  Solar right ascension, un-numbered equation, A&M page 217         MEPH105 */
    alphs = atan(cos(obl) * tan(alsrad)) / pi180;
/* ---  Put alphs into right quadrant                                     MEPH107 */
    if ((d__1 = *sunlsubs - alphs, abs(d__1)) > 270.) {
	alphs += 360.;
    } else if ((d__1 = *sunlsubs - alphs, abs(d__1)) > 90.) {
	alphs += 180.;
    }
    rescale_m05__(&alphs);
/* ---  Mars orbital radius, Astronomical Almanac page E4                 MEPH114 */
    *radius = rad0 * (1. - ecc2) / (ecc * cos(anomm + alsrad - alphfms * 
	    pi180) + 1.);
/* ---  Approximate Mars heliocentric longitude, A&M eqn, (11)            MEPH117 */
/* Computing 2nd power */
    d__1 = tan(inc * .5);
    helilon = *sunlsubs + veqlon0 - veqlon1 * dt - d__1 * d__1 * sin((alsrad 
	    + (veqlon0 - anlon0) * pi180) * 2.) / pi180;
    rescale_m05__(&helilon);
/* ---  Equation of time (deg)                                            MEPH121 */
    *eot = alphfms - alphs;
    rescale_m05__(eot);
    shiftdif_m05__(eot);
/* ---  Earth heliocentric distance and longitude, Allison eqns (20)-     MEPH125 */
/*     (22)                                                              MEPH126 */
    ge = (dt * .9856003 + 357.528) * pi180;
    re = 1.00014 - cos(ge) * .01671 - cos(ge * 2.) * 1.4e-4;
    helone = dt * .9856474 + 100.472 + sin(ge) * 1.915 + sin(ge * 2.) * .02;
/* ---  Earth Cartesian coordinates                                       MEPH131 */
    xe = re * cos(helone * pi180);
    ye = re * sin(helone * pi180);
/* ---  Mars true anolmaly (radians)                                      MEPH134 */
    trueanom = eqcenter * pi180 + anomm;
/* ---  Mars argument of perihelion (radians)                             MEPH136 */
    argper = (dt * 2.92961e-5 + 286.5016) * pi180;
/* ---  Mars Cartesian coordinates                                        MEPH138 */
    zpl = *radius * sin(trueanom + argper) * sin(inc);
/* Computing 2nd power */
    d__1 = zpl / *radius;
    coslat = sqrt(1. - d__1 * d__1);
    xpl = *radius * cos((helilon + dt * 3.82394e-5) * pi180) * coslat;
    ypl = *radius * sin((helilon + dt * 3.82394e-5) * pi180) * coslat;
/* ---  One-way light time (minutes), Allison eqn.(19)                    MEPH143 */
/* Computing 2nd power */
    d__1 = xpl - xe;
/* Computing 2nd power */
    d__2 = ypl - ye;
/* Computing 2nd power */
    d__3 = zpl;
    *owlt = sqrt(d__1 * d__1 + d__2 * d__2 + d__3 * d__3) * 499.005 / 60.;
/* ---  Mars (Heliocentric) prime meridian, Allison eqn (11)              MEPH145 */
    vm = vm0 + 360. / siday * (*xday - vmday0);
/* ---  True solar time (Mars hours) at Mars prime meridian, A&M          MEPH147 */
/*     page 217                                                          MEPH148 */
    pmr = (vm - alphs) / 360.;
    *sunlon = (pmr - d_int(&pmr)) * 360. - 180.;
    rescale_m05__(sunlon);
    return 0;
} /* marsephm_m05__ */

/* ---------------------------------------------------------------------  MEPH154 */
/* Subroutine */ int marsgcm_m05__(doublereal *chgt, doublereal *clat, 
	doublereal *clonw, doublereal *als, doublereal *dusttau, doublereal *
	time, doublereal *ctemp, doublereal *cpres, doublereal *cdens, 
	doublereal *cuwin, doublereal *cvwin, doublereal *blwindew, 
	doublereal *blwindns, doublereal *blwindvert, doublereal *hpres, 
	doublereal *hdens, doublereal *zf, doublereal *pertfact, doublereal *
	ctopohgt, doublereal *hgtasfc, doublereal *careoid, doublereal *
	tempday, doublereal *presday, doublereal *densday, doublereal *
	ewwnday, doublereal *nswnday, doublereal *bluday, doublereal *blvday, 
	doublereal *tempmax, doublereal *tempmin, doublereal *densmax, 
	doublereal *densmin, doublereal *tgrnd, doublereal *calbedo, integer *
	icepolar, doublereal *tat5m, doublereal *dustoffset, doublereal *
	requa, doublereal *rpole, integer *idaydata)
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
	    rgas1, pday1, tsteplat, pday2, rgas2, tday1, uday1, vday1, tday2, 
	    uday2, vday2, tmin1, tmin2, tmax1, tmax2;
    extern /* Subroutine */ int slopewind_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    static doublereal z1ofs, ofsz1, z2ofs, ofsz2, hden12;
    extern doublereal cp_m05__(doublereal *);
    static integer itime;
    static doublereal ddayx;
    extern integer ifloor_m05__(doublereal *);
    static doublereal hdensdayc;
    static integer khgtt, lhgtt;
    static doublereal tdayx, globoffst, pdayx, udayx, toprelief, dmgcm1, 
	    dmgcm2, z1, z2, vdayx, curoffset, tmaxx, tminx, dmaxx, dminx, 
	    pmgcm1, pmgcm2, hdens1, r1, tmgcm1, umgcm1, vmgcm1, tmgcm2, 
	    umgcm2, vmgcm2, r2, z0, tsubl, z5, hpres1, zeval, cpoft, albedo, 
	    gz, tcheck, hdensc, factor, dmgcmx, offset[4]	/* was [2][2] 
	    */, pmgcmx, tmgcmx, umgcmx, vmgcmx, pertlo;
    extern /* Subroutine */ int rellips_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal z2x;
    static integer jbl;
    static doublereal flh, gcp, tlat1st;
    static integer loh;
    static doublereal blu, blv, zf80, polefac, gor, blw;
    extern /* Subroutine */ int bltp_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal rgasday, ofsmgcm, oldrref;
    extern /* Subroutine */ int twod_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal wavemax, steplat, pertmax;
    extern /* Subroutine */ int mgcmterp_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, integer *);
    static doublereal steplon;
    extern /* Subroutine */ int tgcmterp_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *), 
	    subltchk_m05__(doublereal *, doublereal *, doublereal *);
    static doublereal topohgt;
    extern /* Subroutine */ int surfterp_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);

/* ---    Uses interpolation routines to evaluate:                        MGCM  7 */
/*                                                                       MGCM  8 */
/*       ctemp    = temperature (K) at current position                  MGCM  9 */
/*       cpres    = pressure (N/m**2) at current position                MGCM 10 */
/*       cdens    = density (kg/m**3) at current position                MGCM 11 */
/*       cuwin    = eastward wind component (m/s) at current position    MGCM 12 */
/*       cvwin    = northward wind component (m/s) at current position   MGCM 13 */
/*       blwinew  = eastward b.l. slope wind (m/s)                       MGCM 14 */
/*       blwinns  = northward b.l. slope wind (m/s)                      MGCM 15 */
/*       Hpres    = pressure scale height (km) at current position       MGCM 16 */
/*       Hdens    = density scale height (km) at current position        MGCM 17 */
/*       ZF       = height of 1.26 nbar level at current position        MGCM 18 */
/*       pertfact = perturbation factor from random perturbation model   MGCM 19 */
/*       ctopohgt = topographic height (km) at current position          MGCM 20 */
/*       careoid  = local radius (km) of MOLA 1/2 degree areoid          MGCM 21 */
/*       TempDay  = Local daily average temperature (K)                  MGCM 22 */
/*       PresDay  = Local daily average pressure (N/m**2)                MGCM 23 */
/*       DensDay  = Local daily average density (kg/m**3)                MGCM 24 */
/*       EWwnDay  = Local daily average Eastward wind (m/s)              MGCM 25 */
/*       NSwnDay  = Local daily average Northward wind (m/s)             MGCM 26 */
/*       Tempmax  = Local daily maximum temperature (K)                  MGCM 27 */
/*       Tempmin  = Local daily minimum temperature (K)                  MGCM 28 */
/*       Densmax  = Local daily maximum density (kg/m**3)                MGCM 29 */
/*       Densmin  = Local daily minimum density (kg/m**3)                MGCM 30 */
/*       Tgrnd    = ground surface temperature (K)                       MGCM 31 */
/*       calbedo  = surface albedo                                       MGCM 32 */
/*       icepolar = polar ice indicator (0=no; 1=yes)                    MGCM 33 */
/*                                                                       MGCM 34 */
/*       at the current height (chgt), latitude (clat), current (West)   MGCM 35 */
/*       longitude (clonw), for time of year given by Ls=als, and time   MGCM 36 */
/*       of day (time).  Interpolation is done using either boundary     MGCM 37 */
/*       layer or 0-80 km data from the Ames Mars General Circulation    MGCM 38 */
/*       model (MGCM) or from 80-170 km data from the University of      MGCM 39 */
/*       Michigan Mars Thermospheric General Circulation Model (MTGCM).  MGCM 40 */
/*                                                                       MGCM 41 */
/* ---    Set parameter values for number of MGCM heights (nhgt), number  MGCM 42 */
/*       of MTGCM heights (nhgtt), number of MGCM boundary layer levels  MGCM 43 */
/*       (nbl), number of MGCM latitudes (nlat), number of MTGCM lati-   MGCM 44 */
/*       tudes (nlatt), number of MGCM longitudes (nlon), number of dust MGCM 45 */
/*       optical depths (ndust), and minimum perturbation magnitude at   MGCM 46 */
/*       surface (pert0)                                                 MGCM 47 */
    pertmax = .2;
    *pertfact = 0.;
/* ---    Initialize ground surface temperature and polar ice indicator   MGCM 73 */
    *tgrnd = 999.9;
    *icepolar = 99;
/* ---    Insure latitude, longitude, Ls, and time of day within proper   MGCM 76 */
/*       bounds                                                          MGCM 77 */
    if (abs(*clat) > 90.) {
	s_stop(" Latitude error in MarsGCM_M05", (ftnlen)30);
    }
    if (abs(*clonw) > 360.) {
	s_stop(" Longitude error: MarsGCM_M05", (ftnlen)29);
    }
    if (*als < 0. || *als > 360.) {
	s_stop(" Ls error MarsGCM_M05", (ftnlen)21);
    }
    if (*time < 0. || *time > 24.) {
	s_stop(" time error in MarsGCM_M05", (ftnlen)26);
    }
/* ---    Latitude step size for MGCM and MTGCM data                      MGCM 83 */
    steplat = 7.5;
    tsteplat = 5.;
/* ---    Most southerly MTGCM latitude                                   MGCM 86 */
    tlat1st = tsteplat / 2. - 90.;
/* ---    Longitude step size for MGCM boundary layer data                MGCM 88 */
    steplon = 9.;
/* ---    MGCM height index (khgt) for current height (chgt)              MGCM 90 */
    d__1 = *chgt / 5. + 1.;
    khgt = ifloor_m05__(&d__1);
/* ---    Insure khgt within proper limits                                MGCM 92 */
    if (khgt < 1) {
	khgt = 1;
    }
    if (khgt > 16) {
	khgt = 16;
    }
/* ---    MGCM latitude index (ilat) from current latitude (clat)         MGCM 95 */
    d__1 = (*clat + 90.) / steplat;
    interp_m05__1.ilat = ifloor_m05__(&d__1) + 1;
    if (interp_m05__1.ilat > 24) {
	interp_m05__1.ilat = 24;
    }
/* ---    MGCM wind latitude index (ilatw).  MGCM winds are offset in     MGCM 98 */
/*       latitude by 1/2 latitude grid step.                             MGCM 99 */
    d__1 = (*clat + 86.25) / steplat;
    interp_m05__1.ilatw = ifloor_m05__(&d__1) + 2;
/* ---    Insure ilatw within proper bounds                               MGCM101 */
    if (interp_m05__1.ilatw < 2) {
	interp_m05__1.ilatw = 2;
    }
    if (interp_m05__1.ilatw > 24) {
	interp_m05__1.ilatw = 24;
    }
/* ---    MTGCM latitude index (ilatt) from current latitude (clat)       MGCM104 */
    d__1 = (*clat - tlat1st) / tsteplat;
    interp_m05__1.ilatt = ifloor_m05__(&d__1) + 1;
/* ---    Insure ilatt within proper bounds                               MGCM106 */
    if (interp_m05__1.ilatt < 1) {
	interp_m05__1.ilatt = 1;
    }
    if (interp_m05__1.ilatt > 35) {
	interp_m05__1.ilatt = 35;
    }
/* ---    MGCM boundary layer longitude index (jlon)                      MGCM109 */
    d__1 = *clonw / steplon;
    interp_m05__1.jlon = ifloor_m05__(&d__1);
    if (interp_m05__1.jlon > 39) {
	interp_m05__1.jlon = 39;
    }
/* ---    Time of year index (ls) from input Ls value (als)               MGCM112 */
    d__1 = *als / 30.;
    interp_m05__1.ls = ifloor_m05__(&d__1);
    if (interp_m05__1.ls > 11) {
	interp_m05__1.ls = 11;
    }
/* ---    Dust index (mdust) for input dust optical depth (dusttau)       MGCM115 */
    if (*dusttau < mgcmparm_m05__1.dust[1]) {
	interp_m05__1.mdust = 1;
    } else {
	interp_m05__1.mdust = 2;
    }
/* ---    Increment of MGCM latitude (dlat) from grid point               MGCM121 */
    interp_m05__1.dlat = (*clat - steplat * (interp_m05__1.ilat - 1.) + 90.) /
	     steplat;
/* ---    Increment of MTGCM latitude (dlatt) from grid point             MGCM123 */
    interp_m05__1.dlatt = (*clat - tsteplat * (interp_m05__1.ilatt - 1.) - 
	    tlat1st) / tsteplat;
/* ---    Insure dlatt within proper bounds near poles                    MGCM125 */
    interp_m05__1.tpolefac = 1.;
    if (interp_m05__1.ilatt == 1) {
	interp_m05__1.tpolefac = .5;
	if (interp_m05__1.dlatt <= 0.) {
	    interp_m05__1.dlatt = 0.;
	    interp_m05__1.tpolefac = 1. - (abs(*clat) - 85.) / 5.;
	}
    } else if (interp_m05__1.ilatt >= 24) {
	interp_m05__1.tpolefac = .5;
	if (interp_m05__1.dlatt >= 1.) {
	    interp_m05__1.dlatt = 1.;
	    interp_m05__1.tpolefac = 1. - (abs(*clat) - 85.) / 5.;
	}
    }
/* ---    Increment of MGCM longitude (dlon) from grid point              MGCM140 */
    interp_m05__1.dlon = (*clonw - steplon * interp_m05__1.jlon) / steplon;
/* ---    Increment of MGCM latitude from (offset) wind grid point        MGCM142 */
    interp_m05__1.dlatw = (*clat - steplat * (interp_m05__1.ilatw - 2.) + 
	    86.25) / steplat;
    interp_m05__1.wpolefac = 1.;
    if (interp_m05__1.ilatw == 2) {
	interp_m05__1.wpolefac = .75;
	if (interp_m05__1.dlatw <= 0.) {
	    interp_m05__1.wpolefac = 1. - (abs(*clat) - 85.) / 5.;
	    interp_m05__1.dlatw = 0.;
	}
    } else if (interp_m05__1.ilatw >= 24) {
	interp_m05__1.wpolefac = .75;
	if (interp_m05__1.dlatw >= 1.) {
	    interp_m05__1.wpolefac = 1. - (abs(*clat) - 85.) / 5.;
	    interp_m05__1.dlatw = 1.;
	}
    }
/* ---    Increment of solar activity (F10.7 at 1AU) for MTGCM data       MGCM158 */
    interp_m05__1.mf10 = 1;
    interp_m05__1.df10 = zlogr_m05__(&therm_m05__1.f107, &
	    mgcmparm_m05__1.f10val[interp_m05__1.mf10 - 1], "MGCM-01", (
	    ftnlen)7) / zlogr_m05__(&mgcmparm_m05__1.f10val[
	    interp_m05__1.mf10], &mgcmparm_m05__1.f10val[interp_m05__1.mf10 - 
	    1], "MGCM-02", (ftnlen)7);
/* ---    Get areoid radius and topographic height at current lat, lon    MGCM162 */
    rellips_m05__(clat, clonw, careoid, chgt, &gz, &oldrref, ctopohgt, 
	    calbedo, requa, rpole);
/* ---    Compute topographic relief factor for simplified mountain       MGCM165 */
/*       wave perturbation model                                         MGCM166 */
    toprelief = *ctopohgt + 25.;
/* ---    Use topographic height if input height is <= -8.7 km            MGCM168 */
    if (*chgt <= -8.7) {
	*chgt = *ctopohgt + *hgtasfc;
    }
/* ---    Find height index (k1st) of first 0-80 km MGCM level above      MGCM170 */
/*       surface topographic height                                      MGCM171 */
    d__1 = (*ctopohgt + 1.) / 5. + 2.;
    interp_m05__1.k1st = ifloor_m05__(&d__1);
    if (interp_m05__1.k1st < 1) {
	interp_m05__1.k1st = 1;
    }
/* ---    Find Ls increment (dls) from Ls "grid" on input data            MGCM174 */
    interp_m05__1.dls = (*als - interp_m05__1.ls * 30.) / 30.;
/* ---    Compute dust increment (ddust) from dust optical depth "grid"   MGCM176 */
/*       points                                                          MGCM177 */
    interp_m05__1.ddust = zlogr_m05__(dusttau, &mgcmparm_m05__1.dust[
	    interp_m05__1.mdust - 1], "MGCM-03", (ftnlen)7) / zlogr_m05__(&
	    mgcmparm_m05__1.dust[interp_m05__1.mdust], &mgcmparm_m05__1.dust[
	    interp_m05__1.mdust - 1], "MGCM-04", (ftnlen)7);
/* ---    Insure ddust within proper range                                MGCM180 */
    if (interp_m05__1.ddust < 0. && interp_m05__1.mdust == 1) {
	interp_m05__1.ddust = (*dusttau - mgcmparm_m05__1.dust[0]) / (
		mgcmparm_m05__1.dust[1] - mgcmparm_m05__1.dust[0]);
    }
    if (interp_m05__1.ddust > 1.) {
	interp_m05__1.ddust = 1.;
    }
/* ---    Initialize ZF = height of 1.26 nbar level (output value if      MGCM184 */
/*       current height < 80 km)                                         MGCM185 */
    *zf = 999.;
/* ---    Assign MTGCM height offset from input zoffset or array offsets  MGCM187 */
    globoffst = 0.;
    curoffset = 0.;
    if (tgcmoffset_m05__1.ibougher == 2) {
	offset[0] = tgcmoffset_m05__1.offsets[interp_m05__1.ls + 
		interp_m05__1.mdust * 13 - 13];
	offset[2] = tgcmoffset_m05__1.offsets[interp_m05__1.ls + (
		interp_m05__1.mdust + 1) * 13 - 13];
	offset[1] = tgcmoffset_m05__1.offsets[interp_m05__1.ls + 1 + 
		interp_m05__1.mdust * 13 - 13];
	offset[3] = tgcmoffset_m05__1.offsets[interp_m05__1.ls + 1 + (
		interp_m05__1.mdust + 1) * 13 - 13];
	twod_m05__(&interp_m05__1.dls, &interp_m05__1.ddust, offset, &
		globoffst);
    } else {
	tgcmterp_m05__(&c__1, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, zf, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, idaydata);
	tgcmterp_m05__(&c__2, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, zf, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, idaydata);
	hdensc = 5. / zlogr_m05__(&dmgcm1, &dmgcm2, "MGCM-05", (ftnlen)7);
	hdensdayc = 5. / zlogr_m05__(&dday1, &dday2, "MGCM-06", (ftnlen)7);
	mgcmterp_m05__(&c__17, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, idaydata);
	if (tgcmoffset_m05__1.ibougher == 3 || tgcmoffset_m05__1.ibougher < 2)
		 {
	    curoffset = hdensdayc * zlogr_m05__(&dday2, &dday1, "MGCM-07", (
		    ftnlen)7);
	} else {
	    curoffset = hdensc * zlogr_m05__(&dmgcm2, &dmgcm1, "MGCM-08", (
		    ftnlen)7);
	}
    }
    if (tgcmoffset_m05__1.ibougher <= 0) {
	tgcmoffset_m05__1.hgtoffset = tgcmoffset_m05__1.zoffset;
    } else if (tgcmoffset_m05__1.ibougher == 1) {
	tgcmoffset_m05__1.hgtoffset = tgcmoffset_m05__1.zoffset - sin(atan(1.)
		 * *als / 45.) * 2.5;
    } else if (tgcmoffset_m05__1.ibougher == 2) {
	tgcmoffset_m05__1.hgtoffset = globoffst;
    } else {
	tgcmoffset_m05__1.hgtoffset = curoffset;
    }
/* ---    Add height offset due to dust storm                             MGCM223 */
    tgcmoffset_m05__1.hgtoffset += *dustoffset;
/* ---    MTGCM height index (khgtt) for current height                   MGCM225 */
    d__1 = (*chgt - tgcmoffset_m05__1.hgtoffset - 75.) / 5.;
    khgtt = ifloor_m05__(&d__1);
/* ---    Insure khgtt within proper limits                               MGCM227 */
    if (khgtt < 1) {
	khgtt = 1;
    }
    lhgtt = 1;
    if (khgtt == 1 && tgcmoffset_m05__1.hgtoffset < -4.) {
	khgtt = 2;
	lhgtt = 2;
    }
    if (khgtt > 18) {
	khgtt = 18;
    }
/* ---    Initialize MGCM height offset to zero                           MGCM235 */
    tgcmoffset_m05__1.ofszl = 0.;
/* ---    Use MTGCM interpolation if height >= 80 km                      MGCM237 */
    if (*chgt >= tgcmoffset_m05__1.hgtoffset + 80. + (lhgtt - 1.) * 5.) {
/* ---      Get temperature, pressure, density, and wind components at    MGCM239 */
/*         height indexes above and below current height                 MGCM240 */
	tgcmterp_m05__(&khgtt, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, zf, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, idaydata);
	i__1 = khgtt + 1;
	tgcmterp_m05__(&i__1, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, zf, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, idaydata);
/* ---      Height grid points above and below current height             MGCM247 */
	z1 = khgtt * 5. + 75. + tgcmoffset_m05__1.hgtoffset;
	z2 = khgtt * 5. + 80. + tgcmoffset_m05__1.hgtoffset;
/* ---      Apply MTGCM height offset to ZF altitude                      MGCM250 */
	*zf += tgcmoffset_m05__1.hgtoffset;
/* ---      Pressure and density scale heights                            MGCM252 */
	*hpres = (z2 - z1) / zlogr_m05__(&pmgcm1, &pmgcm2, "MGCM-09", (ftnlen)
		7);
	hpresday = (z2 - z1) / zlogr_m05__(&pday1, &pday2, "MGCM-10", (ftnlen)
		7);
	*hdens = (z2 - z1) / zlogr_m05__(&dmgcm1, &dmgcm2, "MGCM-11", (ftnlen)
		7);
	tgcmoffset_m05__1.ofszl = tgcmoffset_m05__1.hgtoffset;
/* ---    Use MGCM interpolation at 75 km and MTGCM interpolation at 80   MGCM257 */
/*       km if height between 75 and 80 km                               MGCM258 */
    } else if (*chgt >= 75.) {
/* ---      Get temperature, pressure, density, and wind components at    MGCM260 */
/*         heights above and below current height                        MGCM261 */
	mgcmterp_m05__(&khgt, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, idaydata);
	tgcmterp_m05__(&lhgtt, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, &zf80, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2,
		 &tmin2, &dmax2, &dmin2, idaydata);
	z1 = 75.;
	z2 = tgcmoffset_m05__1.hgtoffset + 80. + (lhgtt - 1.) * 5.;
/* ---      Apply 'equivalent' multiplier for offset between 75 & 80 km   MGCM270 */
	if (tgcmoffset_m05__1.ibougher <= 1 || *dustoffset > 0.) {
	    z1ofs = 60.;
	    ofsmgcm = tgcmoffset_m05__1.hgtoffset - curoffset;
	    if (tgcmoffset_m05__1.ibougher > 1) {
		ofsmgcm = *dustoffset;
	    }
	    ofsz1 = ofsmgcm * (z1 - z1ofs) / (z2 - z1ofs);
	    i__1 = khgt + 1;
	    mgcmterp_m05__(&i__1, time, &tmgcmx, &pmgcmx, &dmgcmx, &umgcmx, &
		    vmgcmx, &tdayx, &pdayx, &ddayx, &udayx, &vdayx, &tmaxx, &
		    tminx, &dmaxx, &dminx, idaydata);
	    hden12 = 5. / log(dmgcm1 / dmgcmx);
	    ofsmult1 = exp(ofsz1 / hden12);
/* ---        Local MGCM height offset                                    MGCM281 */
	    tgcmoffset_m05__1.ofszl = ofsmgcm * (*chgt - z1ofs) / (z2 - z1ofs)
		    ;
	    pmgcm1 *= ofsmult1;
	    dmgcm1 *= ofsmult1;
	    pday1 *= ofsmult1;
	    dday1 *= ofsmult1;
	    dmax1 *= ofsmult1;
	    dmin1 *= ofsmult1;
	}
/* ---      Pressure and density scale heights (km)                       MGCM290 */
	*hpres = (z2 - z1) / zlogr_m05__(&pmgcm1, &pmgcm2, "MGCM-12", (ftnlen)
		7);
	hpresday = (z2 - z1) / zlogr_m05__(&pday1, &pday2, "MGCM-13", (ftnlen)
		7);
	*hdens = (z2 - z1) / zlogr_m05__(&dmgcm1, &dmgcm2, "MGCM-14", (ftnlen)
		7);
/* ---    Use surfterp_M05 routine if height within boundary layer        MGCM294 */
    } else if (*chgt <= *ctopohgt + mgcmparm_m05__1.dzbl[2]) {
/* ---      Set index for surface layer data                              MGCM296 */
	jbl = 1;
	if (*chgt >= *ctopohgt + mgcmparm_m05__1.dzbl[1]) {
	    jbl = 2;
	}
/* ---      Get temperature, pressure, density, and wind components at    MGCM299 */
/*         heights above and below current height                        MGCM300 */
	i__1 = jbl + 1;
	surfterp_m05__(&i__1, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, hpres, hdens, ctopohgt, &tday2, &pday2, &dday2, &
		uday2, &vday2, &hpresday, &tmax2, &tmin2, &dmax2, &dmin2, 
		tat5m, idaydata);
	surfterp_m05__(&jbl, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, &hpres1, &hdens1, ctopohgt, &tday1, &pday1, &dday1, &
		uday1, &vday1, &hpresday, &tmax1, &tmin1, &dmax1, &dmin1, 
		tat5m, idaydata);
/* ---      Heights at two boundary layer levels                          MGCM307 */
	z1 = *ctopohgt + mgcmparm_m05__1.dzbl[jbl - 1];
	z2 = *ctopohgt + mgcmparm_m05__1.dzbl[jbl];
/* ---      Get Temperature at 1st MGCM height above BL, for computing    MGCM310 */
/*         density scale height                                          MGCM311 */
	mgcmterp_m05__(&interp_m05__1.k1st, time, &tmgcmx, &pmgcmx, &dmgcmx, &
		umgcmx, &vmgcmx, &tdayx, &pdayx, &ddayx, &udayx, &vdayx, &
		tmaxx, &tminx, &dmaxx, &dminx, idaydata);
/* ---      Temperature gradient for density scale height calculation     MGCM315 */
	z2x = (interp_m05__1.k1st - 1.) * 5.;
	dtdz = (tmgcmx - tmgcm1) / (z2x - z1);
	if (*chgt <= *ctopohgt) {
	    dtdz = 0.;
	}
/* ---      Average layer temperature for density scale height            MGCM319 */
	tbar = (tmgcm1 + tmgcm2) / 2.;
/* ---      Density scale height from pressure scale height and           MGCM321 */
/*         temperature gradient                                          MGCM322 */
	*hdens = *hpres / (*hpres / tbar * dtdz + 1.);
/* ---      Perturbation factor = surface value                           MGCM324 */
	*pertfact = .02;
/* ---    Use MGCMterp_M05 routine if height above boundary layer levels  MGCM326 */
/*        and height <= 75 km                                            MGCM327 */
    } else if (*chgt >= (interp_m05__1.k1st - 1.) * 5.) {
/* ---      Get temperature, pressure, density, and wind components at    MGCM329 */
/*         heights above and below current height                        MGCM330 */
	mgcmterp_m05__(&khgt, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, &tday1, &pday1, &dday1, &uday1, &vday1, &tmax1, &
		tmin1, &dmax1, &dmin1, idaydata);
	i__1 = khgt + 1;
	mgcmterp_m05__(&i__1, time, &tmgcm2, &pmgcm2, &dmgcm2, &umgcm2, &
		vmgcm2, &tday2, &pday2, &dday2, &uday2, &vday2, &tmax2, &
		tmin2, &dmax2, &dmin2, idaydata);
/* ---      Heights at grid points above and below current level          MGCM337 */
	z1 = (khgt - 1.) * 5.;
	z2 = khgt * 5.;
/* ---      Apply 'equivalent' multiplier for offset below 75 km          MGCM340 */
	if (tgcmoffset_m05__1.ibougher <= 1 || *dustoffset > 0.) {
	    z1ofs = 60.;
	    z2ofs = tgcmoffset_m05__1.hgtoffset + 80. + (lhgtt - 1.) * 5.;
	    ofsmgcm = tgcmoffset_m05__1.hgtoffset - curoffset;
	    if (tgcmoffset_m05__1.ibougher > 1) {
		ofsmgcm = *dustoffset;
	    }
	    if (z1 <= z1ofs) {
		ofsmult1 = 1.;
	    } else {
		hden12 = 5. / log(dmgcm1 / dmgcm2);
		ofsz1 = ofsmgcm * (z1 - z1ofs) / (z2ofs - z1ofs);
		ofsmult1 = exp(ofsz1 / hden12);
	    }
	    if (z2 <= z1ofs) {
		ofsmult2 = 1.;
	    } else {
		hden12 = 5. / log(dmgcm1 / dmgcm2);
		ofsz2 = ofsmgcm * (z2 - z1ofs) / (z2ofs - z1ofs);
		ofsmult2 = exp(ofsz2 / hden12);
	    }
/* ---        Local MGCM height offset                                    MGCM360 */
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
/* ---      Pressure and density scale heights (km)                       MGCM375 */
	*hpres = (z2 - z1) / zlogr_m05__(&pmgcm1, &pmgcm2, "MGCM-15", (ftnlen)
		7);
	hpresday = (z2 - z1) / zlogr_m05__(&pday1, &pday2, "MGCM-16", (ftnlen)
		7);
	*hdens = (z2 - z1) / zlogr_m05__(&dmgcm1, &dmgcm2, "MGCM-17", (ftnlen)
		7);
/* ---    Use surfterp_M05 at top of boundary layer and MGCMterp_M05 at   MGCM379 */
/*       1st level above boundary layer if height between boundary       MGCM380 */
/*       layer and height index k1st                                     MGCM381 */
    } else {
/* ---      Get temperature, pressure, density, and wind components at    MGCM383 */
/*         heights above and below current height                        MGCM384 */
	surfterp_m05__(&c__3, time, &tmgcm1, &pmgcm1, &dmgcm1, &umgcm1, &
		vmgcm1, hpres, hdens, ctopohgt, &tday1, &pday1, &dday1, &
		uday1, &vday1, &hpresday, &tmax1, &tmin1, &dmax1, &dmin1, 
		tat5m, idaydata);
	mgcmterp_m05__(&interp_m05__1.k1st, time, &tmgcm2, &pmgcm2, &dmgcm2, &
		umgcm2, &vmgcm2, &tday2, &pday2, &dday2, &uday2, &vday2, &
		tmax2, &tmin2, &dmax2, &dmin2, idaydata);
/* ---      Heights at grid points above and below current level          MGCM391 */
	z1 = *ctopohgt + mgcmparm_m05__1.dzbl[2];
	z2 = (interp_m05__1.k1st - 1.) * 5.;
/* ---      Temperature gradient and mean temperature for density scale   MGCM394 */
/*         height calculation                                            MGCM395 */
	dtdz = (tmgcm2 - tmgcm1) / (z2 - z1);
	tbar = (tmgcm1 + tmgcm2) / 2.;
/* ---      Density scale height from pressure scale height and           MGCM398 */
/*         temperature gradient                                          MGCM399 */
	*hdens = *hpres / (*hpres / tbar * dtdz + 1.);
    }
/* ---    Get gas constant from pressure, density, and temperature        MGCM402 */
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
/* ---    Use logarithmic wind and temperature profiles (with surface     MGCM416 */
/*       roughness z0) if height below lowest boundary layer level       MGCM417 */
    if (*chgt < *ctopohgt + mgcmparm_m05__1.dzbl[1]) {
/* ---      Convert surface roughness to km                               MGCM419 */
	z0 = mgcmparm_m05__1.zwsfc / 1e3;
/* ---      Save ground surface temperature for output                    MGCM421 */
	*tgrnd = tmgcm1;
/* ---      Consistent with Ames MGCM, use z0 = 0.01 cm (1.0e-7 km) if    MGCM423 */
/*         over ice (T <= CO2 sublimation temperature + 5K)              MGCM424 */
	tcheck = tmgcm1;
	subltchk_m05__(&tcheck, &pmgcm2, &tsubl);
/* ---      If surface temperature near sublimation point, set polar ice  MGCM427 */
/*           indicator on (= 1) and re-set surface roughness             MGCM428 */
	*icepolar = 0;
	if (tmgcm1 <= tsubl + 5.) {
	    z0 = 1e-7;
	    *icepolar = 1;
	}
	uhgt = *chgt - *ctopohgt;
	if (uhgt < z0) {
	    uhgt = z0;
	}
/* ---      Compute logarithmic boundary layer shape factor for surface   MGCM436 */
/*         to lowest boundary layer level                                MGCM437 */
	factor = zlogr_m05__(&uhgt, &z0, "MGCM-18", (ftnlen)7) / zlogr_m05__(&
		mgcmparm_m05__1.dzbl[1], &z0, "MGCM-19", (ftnlen)7);
/* ---      Apply wind factor; assume no-slip wind condition at surface   MGCM440 */
	*cuwin = umgcm2 * factor;
	*cvwin = vmgcm2 * factor;
	*ewwnday = uday2 * factor;
	*nswnday = vday2 * factor;
/* ---      Set up parameters to evaluate temperature boundary layer      MGCM445 */
/*         Convert heights to meters for input to bltp_M05 subroutine    MGCM446 */
	z5 = mgcmparm_m05__1.dzbl[1] * 1e3;
	zeval = uhgt * 1e3;
/* ---      Get value of local gravity                                    MGCM449 */
	rellips_m05__(clat, clonw, &rref, chgt, &gz, &oldrref, &topohgt, &
		albedo, requa, rpole);
/* ---      Use Ames MGCM boundary layer model for current temperature    MGCM452 */
/*         Get specific heat at constant pressure                        MGCM453 */
	cpoft = cp_m05__(&tmgcm2);
	bltp_m05__(&gz, &cpoft, &tmgcm1, &z5, &tmgcm2, &umgcm2, &vmgcm2, &
		zeval, &factor, ctemp);
/* ---      Use Ames MGCM boundary layer model for daily avg temperature  MGCM457 */
	cpoft = cp_m05__(&tday2);
	bltp_m05__(&gz, &cpoft, &tday1, &z5, &tday2, &uday2, &vday2, &zeval, &
		factor, tempday);
/* ---      Use Ames MGCM boundary layer model for daily max temperature  MGCM461 */
	cpoft = cp_m05__(&tmax2);
	bltp_m05__(&gz, &cpoft, &tmax1, &z5, &tmax2, &uday2, &vday2, &zeval, &
		factor, tempmax);
/* ---      Use Ames MGCM boundary layer model for daily min temperature  MGCM465 */
	cpoft = cp_m05__(&tmin2);
	bltp_m05__(&gz, &cpoft, &tmin1, &z5, &tmin2, &uday2, &vday2, &zeval, &
		factor, tempmin);
/* ---      Pressure at current position from pressure scale height       MGCM469 */
	*cpres = pmgcm2 * exp((z2 - *chgt) / *hpres);
	*presday = pday2 * exp((z2 - *chgt) / hpresday);
/* ---      Density at current position from gas law                      MGCM472 */
	*cdens = *cpres / (rgas * *ctemp);
	*densday = *presday / (rgasday * *tempday);
	*densmin = 9999.;
	*densmax = -9999.;
	if (*idaydata > 0) {
/* ---      Daily maximum and minimum density                             MGCM478 */
	    *densmin = *densday * (dmin1 / dday1 + factor * (dmin2 / dday2 - 
		    dmin1 / dday1));
	    *densmax = *densday * (dmax1 / dday1 + factor * (dmax2 / dday2 - 
		    dmax1 / dday1));
	}
/* ---    Use linear height interpolation if above logarithmic            MGCM484 */
/*       surface layer                                                   MGCM485 */
    } else {
	dhgt = (*chgt - z1) / (z2 - z1);
	*cuwin = umgcm1 + dhgt * (umgcm2 - umgcm1);
	*cvwin = vmgcm1 + dhgt * (vmgcm2 - vmgcm1);
	*ewwnday = uday1 + dhgt * (uday2 - uday1);
	*nswnday = vday1 + dhgt * (vday2 - vday1);
/* ---      Interpolate temperature to current height                     MGCM492 */
	*ctemp = tmgcm1 + dhgt * (tmgcm2 - tmgcm1);
	*tempday = tday1 + dhgt * (tday2 - tday1);
	*tempmax = tmax1 + dhgt * (tmax2 - tmax1);
	*tempmin = tmin1 + dhgt * (tmin2 - tmin1);
/* ---      Pressure at current position from pressure scale height       MGCM497 */
	*cpres = pmgcm2 * exp((z2 - *chgt) / *hpres);
	*presday = pday2 * exp((z2 - *chgt) / hpresday);
/* ---      Density at current position from gas law                      MGCM500 */
	*cdens = *cpres / (rgas * *ctemp);
	*densday = *presday / (rgasday * *tempday);
	*densmin = 9999.;
	*densmax = -9999.;
	if (*idaydata > 0) {
/* ---      Daily maximum and minimum density                             MGCM506 */
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
/* ---    Set specific bogus values of pressure or density scale heights  MGCM526 */
/*       are out of range                                                MGCM527 */
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
/* ---    Compute perturbation factor, unless it has already been set     MGCM547 */
    if (*pertfact < .02) {
/* ---      Perturbation factor from simplified mountain wave model       MGCM549 */
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
/* ---    Get slope winds (0 below surface and > 4.5 km above surface)    MGCM556 */
    slopewind_m05__(clat, clonw, chgt, time, cuwin, cvwin, blwindew, blwindns,
	     blwindvert);
/* ---    Compute daily average slope winds                               MGCM559 */
    *bluday = 0.;
    *blvday = 0.;
    if (*idaydata > 0) {
	for (itime = 0; itime <= 22; itime += 2) {
	    d__1 = (doublereal) itime;
	    slopewind_m05__(clat, clonw, chgt, &d__1, cuwin, cvwin, &blu, &
		    blv, &blw);
	    *bluday += blu;
	    *blvday += blv;
/* L50: */
	}
	*bluday /= 12.;
	*blvday /= 12.;
    }
    return 0;
} /* marsgcm_m05__ */

/* -----------------------------------------------------------------------MGCM574 */
/* Subroutine */ int perturb_m05__(doublereal *dt, doublereal *pbs)
{
    /* Initialized data */

    static doublereal a[7] = { .007,.006,.004,.004,.002,.002,.002 };
    static doublereal tau[7] = { 2.2353,2.7543,1.1177,15.7866,2.1354,2.4694,
	    32.8493 };
    static doublereal phi[7] = { 49.909,168.173,191.837,21.736,15.704,95.528,
	    49.095 };

    /* Builtin functions */
    double atan(doublereal), cos(doublereal);

    /* Local variables */
    static integer i__;
    static doublereal per, pi180;

/* ---  Mars Ls perturbations from Jupiter, Earth, and Venus, Table 5     PTRB  2 */
/*     and eqn (18) of Allison and McEwen, Planet. Space Sci., 48, 215-  PTRB  3 */
/*     235 (2000). dt is time (days) after J2000 Terrestrial Time.       PTRB  4 */
/*     Table 5 Data: amplitudes (deg), periods (J yrs), phases (deg)     PTRB  8 */
    pi180 = atan(1.) / 45.;
    per = pi180 * .98562628336755642;
    *pbs = 0.;
    for (i__ = 1; i__ <= 7; ++i__) {
	*pbs += a[i__ - 1] * cos(per * *dt / tau[i__ - 1] + phi[i__ - 1] * 
		pi180);
/* L10: */
    }
    return 0;
} /* perturb_m05__ */

/* -----------------------------------------------------------------------PTRB 22 */
doublereal ppnd_m05__(doublereal *p, integer *ifault)
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal half = .5;
    static doublereal one = 1.;
    static doublereal split = .42;
    static doublereal a0 = 2.50662823884;
    static doublereal a1 = -18.61500062529;
    static doublereal a2 = 41.39119773534;
    static doublereal a3 = -25.44106049637;
    static doublereal b1 = -8.4735109309;
    static doublereal b2 = 23.08336743743;
    static doublereal b3 = -21.06224101826;
    static doublereal b4 = 3.13082909833;
    static doublereal c0 = -2.78718931138;
    static doublereal c1 = -2.29796479134;
    static doublereal c2 = 4.85014127135;
    static doublereal c3 = 2.32121276858;
    static doublereal d1 = 3.54388924762;
    static doublereal d2 = 1.63706781897;

    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double log(doublereal), sqrt(doublereal);

    /* Local variables */
    static doublereal q, r__;

/*                                                                       PPND  2 */
/*     Algorithm AS 111 Appl. Statist. (1977) Vol. 26, p. 118            PPND  3 */
/*                                                                       PPND  4 */
/*     Produces normal deviate corresponding to lower tail area of p.    PPND  5 */
/*     Returns ifault = 1 in input p >= 1 or <= 0, ifault = 0            PPND  6 */
/*     otherwise.  If ifault = 1, PPND_M05 value is set to 0.            PPND  7 */
/*     Single precision version with error epsilon = 2 ** (-31).         PPND  8 */
/*     For double precision version, change REAL to DOUBLE PRECISION     PPND  9 */
/*     in the FUNCTION statement and the declaration of variables;       PPND 10 */
/*     change E0 to D0 in the DATA statements and change ABS, ALOG       PPND 11 */
/*     and SQRT to DABS, DLOG and DSQRT in the assignment statements.    PPND 12 */
/*     The hash sums are the sums of the moduli of the coefficients.     PPND 13 */
/*     They have no inherent meanings, but are included for use in       PPND 14 */
/*     checking transpositions.                                          PPND 15 */
/*                                                                       PPND 16 */
/*                                                                       PPND 18 */
/*                                                                       PPND 20 */
/*                                                                       PPND 29 */
/*     Hash sum for a & b = 143.70383558076                              PPND 30 */
/*                                                                       PPND 31 */
/*                                                                       PPND 38 */
/*     Hash sum for c & d = 17.43746520924                               PPND 39 */
/*                                                                       PPND 40 */
/*                                                                       PPND 41 */
    *ifault = 0;
    q = *p - half;
    if (abs(q) > split) {
	goto L1;
    }
    r__ = q * q;
    ret_val = q * (((a3 * r__ + a2) * r__ + a1) * r__ + a0) / ((((b4 * r__ + 
	    b3) * r__ + b2) * r__ + b1) * r__ + one);
    return ret_val;
L1:
    r__ = *p;
    if (q > zero) {
	r__ = one - *p;
    }
    if (r__ < zero) {
	goto L2;
    }
    r__ = sqrt(-log(r__));
    ret_val = (((c3 * r__ + c2) * r__ + c1) * r__ + c0) / ((d2 * r__ + d1) * 
	    r__ + one);
    if (q < zero) {
	ret_val = -ret_val;
    }
    return ret_val;
L2:
    *ifault = 1;
    ret_val = zero;
    return ret_val;
} /* ppnd_m05__ */

/* -----------------------------------------------------------------------PPND 61 */
/* Subroutine */ int prseas_m05__(doublereal *lsun, doublereal *lat, 
	doublereal *pr)
{
    /* Initialized data */

    static doublereal a11 = .0847194;
    static doublereal a12 = -5.70405e-6;
    static doublereal a21 = .0690599;
    static doublereal a22 = -1.32689e-6;
    static doublereal phi11 = 304.041;
    static doublereal phi12 = .0080602;
    static doublereal phi21 = 61.362;
    static doublereal phi22 = .0016533;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double atan(doublereal), cos(doublereal);

    /* Local variables */
    static doublereal a1, a2, pi180, lat2, phi1, phi2;

/* ---  Relative seasonal pressure variation on reference ellipsoid       PRSE  4 */
    pi180 = atan(1.) / 45.;
/* Computing 2nd power */
    d__1 = *lat;
    lat2 = d__1 * d__1;
/* ---  a1, a2 = amplitudes of cos(Ls) and cos(2*Ls) terms                PRSE 11 */
    a1 = a11 + a12 * lat2;
    a2 = a21 + a22 * lat2;
/* ---  phi1, phi2 = phases of cos(Ls) and cos(2*Ls) terms                PRSE 14 */
    phi1 = phi11 + phi12 * lat2;
    phi2 = phi21 + phi22 * lat2;
/* ---  Relative variation in pressure on reference ellipsoid, due to     PRSE 17 */
/*     latitude and time (Ls) variations                                 PRSE 18 */
    *pr = a1 * cos(pi180 * (*lsun - phi1)) + 1. + a2 * cos(pi180 * 2. * (*
	    lsun - phi2));
    return 0;
} /* prseas_m05__ */

/* ---------------------------------------------------------------------- PRSE 23 */
doublereal qrhtp_m05__(doublereal *rh, doublereal *t, doublereal *p)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static doublereal esmax, es;

/*     Specific humidity q (g/kg) versus RH (0-1), temperature (K), and  QRHT  2 */
/*     pressure (mb) (Savijarvi, Contr. Atmos. Phys., 64, 103, 1991)     QRHT  3 */
/*                                                                       QRHT  4 */
/*     Saturation water vapor pressure es vs temperature T               QRHT  5 */
    es = exp((*t - 273.16) * 22.542 / (*t + .32)) * 6.1135;
    esmax = *p / 1.59;
    if (es > esmax) {
	es = esmax;
    }
    ret_val = *rh * 1e3 * .407 * es / (*p - es * .59);
    return ret_val;
} /* qrhtp_m05__ */

/* ---------------------------------------------------------------------- QRHT 11 */
doublereal random_m05__(integer *l)
{
    /* Initialized data */

    static doublereal one = 1.;
    static doublereal zero = 0.;

    /* System generated locals */
    doublereal ret_val, d__1;

    /* Builtin functions */
    double d_mod(doublereal *, doublereal *);

/*                                                                       RAND  2 */
/*     Algorithm AS 183 Appl. Statist. (1982) Vol. 31, p.188             RAND  3 */
/*                                                                       RAND  4 */
/*     Returns a pseudo-random number rectangularly distributed          RAND  5 */
/*     between 0 and 1.                                                  RAND  6 */
/*                                                                       RAND  7 */
/*     IX, IY and IZ should be set to integer values between             RAND  8 */
/*     1 and 30,000 before first entry.                                  RAND  9 */
/*                                                                       RAND 10 */
/*     Integer arithmetic up to 30323 is required.                       RAND 11 */
/*                                                                       RAND 12 */
/*     Returns L = 0 unless random = 0 or random = 1, in which           RAND 13 */
/*     case L = 1                                                        RAND 14 */
/*                                                                       RAND 15 */
/*     IX = 171 * Mod(IX, 177) -  2 * (IX / 177)                         RAND 19 */
/*     IY = 172 * Mod(IY, 176) - 35 * (IY / 176)                         RAND 20 */
/*     IZ = 170 * Mod(IZ, 178) - 63 * (IZ / 178)                         RAND 21 */
/*                                                                       RAND 22 */
/*     If (IX .lt. 0) IX = IX + 30269                                    RAND 23 */
/*     If (IY .lt. 0) IY = IY + 30307                                    RAND 24 */
/*     If (IZ .lt. 0) IZ = IZ + 30323                                    RAND 25 */
/*                                                                       RAND 26 */
/*     If integer arithmetic up to 5,212,632 is not available,           RAND 27 */
/*     the preceding 6 statements may be used instead of the following 3 RAND 28 */
/*                                                                       RAND 29 */
    randcom_m05__1.ix = 171 * randcom_m05__1.ix % 30269;
    randcom_m05__1.iy = randcom_m05__1.iy * 172 % 30307;
    randcom_m05__1.iz = randcom_m05__1.iz * 170 % 30323;
/*                                                                       RAND 33 */
    d__1 = (doublereal) randcom_m05__1.ix / 30269. + (doublereal) 
	    randcom_m05__1.iy / 30307. + (doublereal) randcom_m05__1.iz / 
	    30323.;
    ret_val = d_mod(&d__1, &one);
    *l = 0;
    if (ret_val <= zero || ret_val >= one) {
	*l = 1;
    }
    return ret_val;
} /* random_m05__ */

/* -----------------------------------------------------------------------RAND 40 */
/* Subroutine */ int readmgcm_m05__(char *gcmdir, char *version, ftnlen 
	gcmdir_len, ftnlen version_len)
{
    /* Initialized data */

    static doublereal df[3] = { .995,.9996,1.203 };

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
    static integer lsea, ihgt;
    static doublereal xlat, ylat;
    static integer ilatstep, i__, k, m, ls, lendir, lastls, lat, ils;

    /* Fortran I/O blocks */
    static cilist io___563 = { 0, 32, 1, 0, 0 };
    static cilist io___567 = { 0, 33, 1, 0, 0 };


/* ---    Reads NASA Ames Mars General Circulation Model (MGCM) 0-80 km   RDMG  2 */
/*       data (in binary format) and loads into data arrays for common   RDMG  3 */
/*       MGCMdata                                                        RDMG  4 */
/*       GCMDIR is directory name where MGCM data resides                RDMG  5 */
/* ---    Set parameters for ndust=number of dust optical depths, nhgt=   RDMG 10 */
/*       number of MGCM heights, nlat=number of MGCM latitudes           RDMG 11 */
/* ---    Set parameter for form= in binary file open statement           RDMG 19 */
/* ---    Factors for Map Year 0 density and pressure to better agree     RDMG 21a */
/*         with Map Years 1 and 2                                        RDMG 21b */
/*                                                                       RDMG 36 */
/* ---    Initialize last Ls value processed to 0                         RDMG 37 */
    lastls = 0;
/* ---    Set ilatstep = latitude step size x 10                          RDMG 39 */
    ilatstep = 75;
/* ---    Compute string length for directory name                        RDMG 41 */
    lendir = i_indx(gcmdir, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lendir < 1 || lendir > 60) {
	lendir = 60;
    }
/* ---    Step through all dust optical depths                            RDMG 44 */
    for (m = 1; m <= 3; ++m) {
/* ---      Open MGCM input files for temperature, pressure, and density  RDMG 46 */
	o__1.oerr = 0;
	o__1.ounit = 32;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "tpdlo";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.dustc + (m - 1 << 1);
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
/* ---      Open MGCM input files for wind components                     RDMG 49 */
	o__1.oerr = 0;
	o__1.ounit = 33;
	o__1.ofnmlen = lendir + 11;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 4, a__1[1] = "uvlo";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.dustc + (m - 1 << 1);
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
/* ---      Step through all Ls values                                    RDMG 52 */
	for (lsea = 30; lsea <= 360; lsea += 30) {
	    ls = lsea / 30;
/* ---      Step through all latitude grid points                         RDMG 55 */
	    i__2 = ilatstep;
	    for (lat = -900; i__2 < 0 ? lat >= 900 : lat <= 900; lat += i__2) 
		    {
		xlat = lat / 10.;
		i__ = (lat + 900) / ilatstep + 1;
/* ---      Step through all height levels                                RDMG 59 */
		for (k = 17; k >= 1; --k) {
/* ---        Read (binary) tide coefficients for temperature, pressure,  RDMG 61 */
/*           and density                                                 RDMG 62 */
		    i__3 = s_rsue(&io___563);
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
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.tza0[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.tza1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.tzp1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.tza2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.tzp2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.pza0[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.pza1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.pzp1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.pza2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.pzp2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.dza0[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
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
		    if (ihgt != (k - 1) * 5) {
			s_stop(" Bad tpd Height", (ftnlen)15);
		    }
		    if (ylat != xlat) {
			s_stop(" Bad tpd Latitude", (ftnlen)17);
		    }
/* ---        Adjust pressure and density to agree with Map Year 2        RDMG 69a */
		    mgcmdata_m05__1.pza0[k + (i__ + (ls + m * 13) * 25) * 17 
			    - 5543] *= df[m - 1];
		    mgcmdata_m05__1.dza0[k + (i__ + (ls + m * 13) * 25) * 17 
			    - 5543] *= df[m - 1];
/* ---        Read (binary) tide coefficients for wind components         RDMG 70 */
		    i__3 = s_rsue(&io___567);
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
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.uza0[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.uza1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.uzp1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.uza2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.uzp2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.vza0[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.vza1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.vzp1[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.vza2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
			    sizeof(doublereal));
		    if (i__3 != 0) {
			goto L99;
		    }
		    i__3 = do_uio(&c__1, (char *)&mgcmdata_m05__1.vzp2[k + (
			    i__ + (ls + m * 13) * 25) * 17 - 5543], (ftnlen)
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
/* ---        Reset value of last Ls processed                            RDMG 76 */
		    lastls = ils;
		    if (ihgt != (k - 1) * 5) {
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
/* ---      Set data for Ls = 0 to data for Ls = 360                      RDMG 83 */
	for (k = 1; k <= 17; ++k) {
	    for (i__ = 1; i__ <= 25; ++i__) {
		mgcmdata_m05__1.tza0[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.tza0[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.tza1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.tza1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.tzp1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.tzp1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.tza2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.tza2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.tzp2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.tzp2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.pza0[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.pza0[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.pza1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.pza1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.pzp1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.pzp1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.pza2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.pza2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.pzp2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.pzp2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.dza0[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.dza0[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.uza0[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.uza0[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.uza1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.uza1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.uzp1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.uzp1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.uza2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.uza2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.uzp2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.uzp2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.vza0[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.vza0[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.vza1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.vza1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.vzp1[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.vzp1[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.vza2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.vza2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
		mgcmdata_m05__1.vzp2[k + (i__ + m * 325) * 17 - 5543] = 
			mgcmdata_m05__1.vzp2[k + (i__ + (m * 13 + 12) * 25) * 
			17 - 5543];
/* L60: */
	    }
/* L61: */
	}
/* ---    Close input files to re-use same unit number for next dust      RDMG109 */
/*       value                                                           RDMG110 */
	cl__1.cerr = 0;
	cl__1.cunit = 32;
	cl__1.csta = 0;
	f_clos(&cl__1);
	cl__1.cerr = 0;
	cl__1.cunit = 33;
	cl__1.csta = 0;
	f_clos(&cl__1);
	goto L100;
/* ---    Terminate if not all Ls values have been processed              RDMG114 */
L99:
	if (lastls != 360) {
	    s_stop(" Incomplete 0-80 km MGCM data", (ftnlen)29);
	}
L100:
	;
    }
    return 0;
} /* readmgcm_m05__ */

/* -----------------------------------------------------------------------RDMG119 */
/* Subroutine */ int readsurf_m05__(char *gcmdir, char *version, ftnlen 
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
    static integer ilatstep, i__, k, m, ls, lendir, lastls, lat, ils, lon;

    /* Fortran I/O blocks */
    static cilist io___579 = { 0, 0, 1, 0, 0 };
    static cilist io___583 = { 0, 0, 1, 0, 0 };


/* ---    Reads NASA Ames Mars General Circulation Model (MGCM) surface   RDSF  2 */
/*       data (in binary format) and loads into data arrays for common   RDSF  3 */
/*       surfdata                                                        RDSF  4 */
/*       GCMDIR is directory name where MGCM data resides                RDSF  5 */
/* ---    Set parameter values for ndust=number of dust optical depths,   RDSF 10 */
/*       nbl=number of boundary layer levels, nlat=number of MGCM lati-  RDSF 11 */
/*       tudes, nlon=number of MGCM longitudes                           RDSF 12 */
/* ---    Set parameter for form= in binary file open statement           RDSF 20 */
/*                                                                       RDSF 41 */
/* ---    Initialize last Ls value processed to 0                         RDSF 42 */
    lastls = 0;
/* ---    Set ilatstep = latitude step size x 10                          RDSF 44 */
    ilatstep = 75;
/* ---    Compute string length for directory name                        RDSF 46 */
    lendir = i_indx(gcmdir, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lendir < 1 || lendir > 60) {
	lendir = 60;
    }
/* ---    Step through all dust optical depths                            RDSF 49 */
    for (m = 1; m <= 3; ++m) {
/* ---      Open surface data files for surface level                     RDSF 51 */
	o__1.oerr = 0;
	o__1.ounit = 33;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "sfc00";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.dustc + (m - 1 << 1);
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
/* ---      Open surface data files for 5 meter level above surface       RDSF 54 */
	o__1.oerr = 0;
	o__1.ounit = 34;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "sfc05";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.dustc + (m - 1 << 1);
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
/* ---      Open surface data files for 30 meter level above surface      RDSF 57 */
	o__1.oerr = 0;
	o__1.ounit = 35;
	o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 5, a__1[1] = "sfc30";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.dustc + (m - 1 << 1);
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
/* ---      Step through all Ls values                                    RDSF 60 */
	for (lsea = 30; lsea <= 360; lsea += 30) {
	    ls = lsea / 30;
/* ---      Step through all latitudes                                    RDSF 63 */
	    i__2 = ilatstep;
	    for (lat = -900; i__2 < 0 ? lat >= 900 : lat <= 900; lat += i__2) 
		    {
		xlat = lat / 10.;
		i__ = (lat + 900) / ilatstep + 1;
/* ---      Step through all boundary layer levels                        RDSF 67 */
		for (k = 1; k <= 3; ++k) {
/* ---      Step through all longitudes                                   RDSF 69 */
		    for (lon = 40; lon >= 1; --lon) {
/* ---        Read (binary) tide coefficients for temperature and wind    RDSF 71 */
/*           components at all boundary layer levels                     RDSF 72 */
			if (k == 1) {
			    io___579.ciunit = k + 32;
			    i__3 = s_rsue(&io___579);
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
				    surfdata_m05__1.tsa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsp2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = e_rsue();
			    if (i__3 != 0) {
				goto L99;
			    }
/* ---         Assume surface wind = 0 (no slip condition)                RDSF 77 */
			    surfdata_m05__1.usa0[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.usa1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.usp1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.usa2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.usp2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.vsa0[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.vsa1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.vsp1[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.vsa2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			    surfdata_m05__1.vsp2[k + (i__ + (lon + (ls + m * 
				    13) * 41) * 25) * 3 - 39979] = 0.;
			} else {
			    io___583.ciunit = k + 32;
			    i__3 = s_rsue(&io___583);
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
				    surfdata_m05__1.tsa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.tsp2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.usa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.usa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.usp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.usa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.usp2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.vsa0[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.vsa1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.vsp1[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.vsa2[k + (i__ + (lon + (
				    ls + m * 13) * 41) * 25) * 3 - 39979], (
				    ftnlen)sizeof(doublereal));
			    if (i__3 != 0) {
				goto L99;
			    }
			    i__3 = do_uio(&c__1, (char *)&
				    surfdata_m05__1.vsp2[k + (i__ + (lon + (
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
/* ---        Reset value of last Ls processed                            RDSF 97 */
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
/* ---      Set all values at Ls=0 to values at Ls=360                    RDSF105 */
	for (k = 1; k <= 3; ++k) {
	    for (i__ = 1; i__ <= 25; ++i__) {
		for (lon = 1; lon <= 40; ++lon) {
		    surfdata_m05__1.tsa0[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.tsa0[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.tsa1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.tsa1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.tsp1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.tsp1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.tsa2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.tsa2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.tsp2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.tsp2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.usa0[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.usa0[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.usa1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.usa1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.usp1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.usp1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.usa2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.usa2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.usp2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.usp2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.vsa0[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.vsa0[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.vsa1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.vsa1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.vsp1[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.vsp1[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.vsa2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.vsa2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
		    surfdata_m05__1.vsp2[k + (i__ + (lon + m * 533) * 25) * 3 
			    - 39979] = surfdata_m05__1.vsp2[k + (i__ + (lon + 
			    (m * 13 + 12) * 41) * 25) * 3 - 39979];
/* L60: */
		}
/*           Set all values at Lon=0 to values at Lon=360                RDSF125 */
		for (ls = 0; ls <= 12; ++ls) {
		    surfdata_m05__1.tsa0[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.tsa0[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.tsa1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.tsa1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.tsp1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.tsp1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.tsa2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.tsa2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.tsp2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.tsp2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.usa0[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.usa0[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.usa1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.usa1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.usp1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.usp1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.usa2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.usa2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.usp2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.usp2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.vsa0[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.vsa0[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.vsa1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.vsa1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.vsp1[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.vsp1[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.vsa2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.vsa2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
		    surfdata_m05__1.vsp2[k + (i__ + (ls + m * 13) * 1025) * 3 
			    - 39979] = surfdata_m05__1.vsp2[k + (i__ + ((ls + 
			    m * 13) * 41 + 40) * 25) * 3 - 39979];
/* L70: */
		}
/* L80: */
	    }
	}
/* ---      Close input file units                                        RDSF144 */
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
/* ---      Terminate if not all Ls values processed                      RDSF149 */
L99:
	if (lastls != 360) {
	    s_stop(" Incomplete surface GCM data", (ftnlen)28);
	}
L100:
	;
    }
    return 0;
} /* readsurf_m05__ */

/* -----------------------------------------------------------------------RDSF154 */
/* Subroutine */ int readtgcm_m05__(char *gcmdir, char *version, ftnlen 
	gcmdir_len, ftnlen version_len)
{
    /* System generated locals */
    address a__1[5], a__2[6];
    integer i__1[5], i__2[6], i__3, i__4, i__5;
    char ch__1[71], ch__2[72];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsue(cilist *), do_uio(integer *, char *, 
	    ftnlen), e_rsue();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_rsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_rsle(), f_clos(cllist *);

    /* Local variables */
    static integer lsea, ihgt;
    static doublereal xlat, ylat;
    static integer ilatstep, ilat1, ilat2, i__, k, m, n, ls, lendir, lastls;
    static doublereal zfdust;
    static integer lat, ils;

    /* Fortran I/O blocks */
    static cilist io___597 = { 0, 32, 1, 0, 0 };
    static cilist io___601 = { 0, 33, 1, 0, 0 };
    static cilist io___602 = { 0, 34, 1, 0, 0 };


/* ---    Reads University of Michigan Mars Thermospheric General Circu-  RDTG  2 */
/*       lation Model (MTGCM) data (in binary format) and loads into     RDTG  3 */
/*       data arrays for common TGCMdata                                 RDTG  4 */
/*       GCMDIR is directory name where MTGCM data resides               RDTG  5 */
/* ---    Set parameter values for ndust=number of dust optical depths,   RDTG 10 */
/*       nhgtt=number of MTGCM heights, nlatt=number of MTGCM latitudes  RDTG 11 */
/* ---    Set parameter for form= in binary file open statement           RDTG 19 */
/*                                                                       RDTG 44 */
/* ---    Initialize last Ls value processed to 0                         RDTG 45 */
    lastls = 0;
/* ---    Set ilatstep = latitude step size x 10                          RDTG 47 */
    ilatstep = 50;
/* ---    Set initial and final latitudes (x 10) for stepping             RDTG 49 */
    ilat1 = ilatstep / 2 - 900;
    ilat2 = 900 - ilatstep / 2;
/* ---    Compute string length for directory name                        RDTG 52 */
    lendir = i_indx(gcmdir, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lendir < 1 || lendir > 60) {
	lendir = 60;
    }
/* ---    Step through all solar activity levels                          RDTG 55 */
    for (n = 1; n <= 2; ++n) {
	o__1.oerr = 0;
	o__1.ounit = 34;
	o__1.ofnmlen = lendir + 11;
/* Writing concatenation */
	i__1[0] = lendir, a__1[0] = gcmdir;
	i__1[1] = 4, a__1[1] = "zfht";
	i__1[2] = 2, a__1[2] = mgcmparm_m05__1.solact + (n - 1 << 1);
	i__1[3] = 1, a__1[3] = version;
	i__1[4] = 4, a__1[4] = ".txt";
	s_cat(ch__1, a__1, i__1, &c__5, (ftnlen)71);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	f_open(&o__1);
/* ---    Step through all dust optical depths                            RDTG 59 */
	for (m = 1; m <= 3; ++m) {
/* ---      Open MTGCM data files for temperature, pressure, and density  RDTG 61 */
	    o__1.oerr = 0;
	    o__1.ounit = 32;
	    o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
	    i__2[0] = lendir, a__2[0] = gcmdir;
	    i__2[1] = 3, a__2[1] = "tpd";
	    i__2[2] = 2, a__2[2] = mgcmparm_m05__1.solact + (n - 1 << 1);
	    i__2[3] = 2, a__2[3] = mgcmparm_m05__1.dustc + (m - 1 << 1);
	    i__2[4] = 1, a__2[4] = version;
	    i__2[5] = 4, a__2[5] = ".bin";
	    s_cat(ch__2, a__2, i__2, &c__6, (ftnlen)72);
	    o__1.ofnm = ch__2;
	    o__1.orl = 0;
	    o__1.osta = "old";
	    o__1.oacc = 0;
	    o__1.ofm = "unformatted";
	    o__1.oblnk = 0;
	    f_open(&o__1);
/* ---      Open MTGCM data files for wind components                     RDTG 64 */
	    o__1.oerr = 0;
	    o__1.ounit = 33;
	    o__1.ofnmlen = lendir + 11;
/* Writing concatenation */
	    i__2[0] = lendir, a__2[0] = gcmdir;
	    i__2[1] = 2, a__2[1] = "uv";
	    i__2[2] = 2, a__2[2] = mgcmparm_m05__1.solact + (n - 1 << 1);
	    i__2[3] = 2, a__2[3] = mgcmparm_m05__1.dustc + (m - 1 << 1);
	    i__2[4] = 1, a__2[4] = version;
	    i__2[5] = 4, a__2[5] = ".bin";
	    s_cat(ch__1, a__2, i__2, &c__6, (ftnlen)71);
	    o__1.ofnm = ch__1;
	    o__1.orl = 0;
	    o__1.osta = "old";
	    o__1.oacc = 0;
	    o__1.ofm = "unformatted";
	    o__1.oblnk = 0;
	    f_open(&o__1);
/* ---      Step through all Ls values                                    RDTG 67 */
	    for (lsea = 30; lsea <= 360; lsea += 30) {
		ls = lsea / 30;
/* ---      Step through all latitudes                                    RDTG 70 */
		i__3 = ilat2;
		i__4 = ilatstep;
		for (lat = ilat1; i__4 < 0 ? lat >= i__3 : lat <= i__3; lat +=
			 i__4) {
		    xlat = lat / 10.;
		    i__ = (lat - ilat1) / ilatstep + 1;
/* ---      Step through all heights                                      RDTG 74 */
		    for (k = 1; k <= 19; ++k) {
/* ---        Read (binary) tide coefficients for temperature, pressure,  RDTG 76 */
/*           and density                                                 RDTG 77 */
			i__5 = s_rsue(&io___597);
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				integer));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(
				integer));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.tta0[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.tta1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.ttp1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.tta2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.ttp2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.pta0[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.pta1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.ptp1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.pta2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.ptp2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.dta0[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.dta1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.dtp1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.dta2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.dtp2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = e_rsue();
			if (i__5 != 0) {
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
/* ---        Read (binary) tide coefficients for wind components         RDTG 87 */
			i__5 = s_rsue(&io___601);
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&ils, (ftnlen)sizeof(
				integer));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&ihgt, (ftnlen)sizeof(
				integer));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&ylat, (ftnlen)sizeof(
				doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.uta0[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.uta1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.utp1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.uta2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.utp2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.vta0[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.vta1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.vtp1[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.vta2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = do_uio(&c__1, (char *)&tgcmdata_m05__1.vtp2[k 
				+ (i__ + (ls + (m + n * 3) * 13) * 36) * 19 - 
				35588], (ftnlen)sizeof(doublereal));
			if (i__5 != 0) {
			    goto L99;
			}
			i__5 = e_rsue();
			if (i__5 != 0) {
			    goto L99;
			}
			if (ils != lsea) {
			    s_stop(" Bad uv Ls", (ftnlen)10);
			}
/* ---        Reset last Ls value processed                               RDTG 93 */
			lastls = ils;
			if (ihgt != (k - 1) * 5 + 80) {
			    s_stop(" Bad uv Height", (ftnlen)14);
			}
			if (ylat != xlat) {
			    s_stop(" Bad uv Latitude", (ftnlen)16);
			}
/* L40: */
		    }
/* ---      Read tide coefficient data for ZF=height of 1.26 nbar level   RDTG 98 */
		    i__5 = s_rsle(&io___602);
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&zfdust, (ftnlen)
			    sizeof(doublereal));
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
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmdata_m05__1.zfa0[
			    i__ + (ls + (m + n * 3) * 13) * 36 - 1873], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmdata_m05__1.zfa1[
			    i__ + (ls + (m + n * 3) * 13) * 36 - 1873], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmdata_m05__1.zfp1[
			    i__ + (ls + (m + n * 3) * 13) * 36 - 1873], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmdata_m05__1.zfa2[
			    i__ + (ls + (m + n * 3) * 13) * 36 - 1873], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = do_lio(&c__5, &c__1, (char *)&tgcmdata_m05__1.zfp2[
			    i__ + (ls + (m + n * 3) * 13) * 36 - 1873], (
			    ftnlen)sizeof(doublereal));
		    if (i__5 != 0) {
			goto L99;
		    }
		    i__5 = e_rsle();
		    if (i__5 != 0) {
			goto L99;
		    }
		    if (zfdust != mgcmparm_m05__1.dust[m - 1]) {
			s_stop(" Bad ZF dust value", (ftnlen)18);
		    }
		    if (ils != lsea) {
			s_stop(" Bad ZF Ls", (ftnlen)10);
		    }
		    if (ylat != xlat) {
			s_stop(" Bad ZF Latitude", (ftnlen)16);
		    }
/* L50: */
		}
	    }
/* ---      Set all values at Ls=0 to values at Ls=360                    RDTG105 */
	    for (i__ = 1; i__ <= 36; ++i__) {
		tgcmdata_m05__1.zfa0[i__ + (m + n * 3) * 468 - 1873] = 
			tgcmdata_m05__1.zfa0[i__ + ((m + n * 3) * 13 + 12) * 
			36 - 1873];
		tgcmdata_m05__1.zfa1[i__ + (m + n * 3) * 468 - 1873] = 
			tgcmdata_m05__1.zfa1[i__ + ((m + n * 3) * 13 + 12) * 
			36 - 1873];
		tgcmdata_m05__1.zfp1[i__ + (m + n * 3) * 468 - 1873] = 
			tgcmdata_m05__1.zfp1[i__ + ((m + n * 3) * 13 + 12) * 
			36 - 1873];
		tgcmdata_m05__1.zfa2[i__ + (m + n * 3) * 468 - 1873] = 
			tgcmdata_m05__1.zfa2[i__ + ((m + n * 3) * 13 + 12) * 
			36 - 1873];
		tgcmdata_m05__1.zfp2[i__ + (m + n * 3) * 468 - 1873] = 
			tgcmdata_m05__1.zfp2[i__ + ((m + n * 3) * 13 + 12) * 
			36 - 1873];
		for (k = 1; k <= 19; ++k) {
		    tgcmdata_m05__1.tta0[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.tta0[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.tta1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.tta1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.ttp1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.ttp1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.tta2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.tta2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.ttp2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.ttp2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.pta0[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.pta0[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.pta1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.pta1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.ptp1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.ptp1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.pta2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.pta2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.ptp2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.ptp2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.dta0[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.dta0[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.dta1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.dta1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.dtp1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.dtp1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.dta2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.dta2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.dtp2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.dtp2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.uta0[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.uta0[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.uta1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.uta1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.utp1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.utp1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.uta2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.uta2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.utp2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.utp2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.vta0[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.vta0[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.vta1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.vta1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.vtp1[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.vtp1[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.vta2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.vta2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
		    tgcmdata_m05__1.vtp2[k + (i__ + (m + n * 3) * 468) * 19 - 
			    35588] = tgcmdata_m05__1.vtp2[k + (i__ + ((m + n *
			     3) * 13 + 12) * 36) * 19 - 35588];
/* L60: */
		}
	    }
/* ---    Close input file unit numbers                                   RDTG139 */
	    cl__1.cerr = 0;
	    cl__1.cunit = 32;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    cl__1.cerr = 0;
	    cl__1.cunit = 33;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
	    goto L100;
/* ---    Terminate if not all Ls values processed                        RDTG143 */
L99:
	    if (lastls != 360) {
		s_stop(" Incomplete 80-170 km MTGCM data", (ftnlen)32);
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
} /* readtgcm_m05__ */

/* -----------------------------------------------------------------------RDTG150 */
/* Subroutine */ int rellips_m05__(doublereal *lat, doublereal *lonw, 
	doublereal *rref, doublereal *z__, doublereal *gz, doublereal *
	oldrref, doublereal *ctopohgt, doublereal *calbedo, doublereal *requa,
	 doublereal *rpole)
{
    /* System generated locals */
    doublereal d__1, d__2, d__3;

    /* Builtin functions */
    double sqrt(doublereal), tan(doublereal), sin(doublereal), cos(doublereal)
	    ;

    /* Local variables */
    static doublereal tlat, a, b, c__, p2, ab, rz, xx, yy;
    static integer iau;
    extern /* Subroutine */ int topoareo_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);

/* ---  Calculates Mars areoid radius (Rref, in km) from 1/2 by 1/2       RLPS  5 */
/*     degree MOLA data, for given latitude (LAT, in degrees), and West  RLPS  6 */
/*     longitude (LonW, in degrees).                                     RLPS  7 */
/* ---  Also computes acceleration of gravity, gz in m/s**2, for          RLPS  8 */
/*     altitude z in km, at given latitude, longitude                    RLPS  9 */
/* ---  Also calculates MARS radius (Oldrref, in km) of reference         RLPS 10 */
/*     ellipsoid                                                         RLPS 11 */
/* ---  RADEG = DEGREES/RADIAN                                            RLPS 12 */
/* ---  Mars rotation rate, radians per second                            RLPS 14 */
/* ---  Planetary constant (GM) from Mars Pathfinder Project              RLPS 16 */
/*     Planetary Constants and Models, JPL D-12947, Dec. 1995            RLPS 17 */
/*     and JPL planetary ephemeris DE403                                 RLPS 18 */
/* ---  GM = gravitational constant * mass of Mars                        RLPS 19 */
/* ---  J2 = coefficient of 1st non-spherical gravity term                RLPS 21 */
/* ---  User-selectable inputs for equatorial and polar ellipsoid radii   RLPS 23 */
    a = *requa;
    b = *requa;
    c__ = *rpole;
/* ---  Mean equatorial radius for ellipsoid                              RLPS 27 */
    ab = sqrt(a * b);
    tlat = tan(*lat / 57.29577958);
/* ---  XX, YY = squares of x, y components of local ellipsoid radius     RLPS 30 */
/* Computing 2nd power */
    d__1 = ab * c__;
/* Computing 2nd power */
    d__2 = c__;
/* Computing 2nd power */
    d__3 = ab * tlat;
    xx = d__1 * d__1 / (d__2 * d__2 + d__3 * d__3);
/* Computing 2nd power */
    d__1 = tlat;
    yy = xx * (d__1 * d__1);
/* ---  Reference ellipsoid radius                                        RLPS 33 */
    *oldrref = sqrt(xx + yy);
/* ---  Get MOLA areoid (Rref)                                            RLPS 35 */
    iau = 2000;
    topoareo_m05__(lat, lonw, rref, ctopohgt, calbedo, &iau);
/* ---  Rz = total radius to current height z                             RLPS 38 */
    rz = *rref + *z__;
/* ---  Acceleration of gravity including J2 and centrifugal terms        RLPS 40 */
/* Computing 2nd power */
    d__1 = sin(*lat / 57.29577958);
    p2 = d__1 * d__1 * 1.5 - .5;
/* Computing 2nd power */
    d__1 = rz;
/* Computing 2nd power */
    d__2 = ab / rz;
    *gz = 42828314.258 / (d__1 * d__1) * (1. - d__2 * d__2 * 
	    .0058758483840000006 * p2);
/* Computing 2nd power */
    d__1 = cos(*lat / 57.29577958) * 7.0882184163834007e-5;
    *gz -= rz * 1e3 * (d__1 * d__1);
    return 0;
} /* rellips_m05__ */

/* -----------------------------------------------------------------------RLPS 46 */
/* Subroutine */ int rescale_m05__(doublereal *x)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double d_int(doublereal *);

/* ---  Puts x into range 0 - 360                                         RSCL  2 */
    d__1 = *x / 360.;
    *x = *x / 360. - d_int(&d__1) + 1.;
    *x = (*x - d_int(x)) * 360.;
    return 0;
} /* rescale_m05__ */

/* ---------------------------------------------------------------------- RSCL  8 */
/* Subroutine */ int shiftdif_m05__(doublereal *x)
{
/* ---  Shifts difference x to be +/- and close to 0.0                    SHFD  2 */
    if (*x > 180.) {
	*x += -360.;
    } else if (*x < -180.) {
	*x += 360.;
    }
    return 0;
} /* shiftdif_m05__ */

/* ---------------------------------------------------------------------- SHFD 11 */
/* Subroutine */ int slopewind_m05__(doublereal *xlat, doublereal *xlonw, 
	doublereal *hgtmola, doublereal *time, doublereal *umean, doublereal *
	vmean, doublereal *uew, doublereal *vns, doublereal *wvert)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double atan(doublereal), cos(doublereal), d_sign(doublereal *, doublereal 
	    *), sin(doublereal), sqrt(doublereal), exp(doublereal);

    /* Local variables */
    static doublereal rref, ctopohgt, b, f, h__, p, omega, qslam, c0, b1, 
	    xlatm, b2, b3, xlatp, b4, xlonm, topom, xlonp, topop, wb, rm, xi, 
	    rp, alphax, hgtsfc, alphay, fcross, fupslp, calbedo;
    static integer iau;
    static doublereal cosfact, alphaxw, alphayw, zfactor, pi180;
    extern /* Subroutine */ int topoareo_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);

/* ---    Analytical slope winds solution from Ye, Segal, and Pielke,     SLWN  3 */
/*       J. Atmos. Sci., 47, 612, 1990.  (hereafter YSP)                 SLWN  4 */
/*                                                                       SLWN  5 */
/*       Inputs:                                                         SLWN  6 */
/*         xlat, xlonW = planeto-centric latitude, longitude(West), deg  SLWN  7 */
/*         hgtMOLA = planeto-centric height above MOLA areoid (km)       SLWN  8 */
/*         time    = local solar time (Mars hours)                       SLWN  9 */
/*       Outputs:                                                        SLWN 10 */
/*         uew, vns = eastward, northward wind components (from MOLA     SLWN 11 */
/*           slopes in eastward and northward directions)                SLWN 12 */
/*                                                                       SLWN 13 */
    pi180 = atan(1.) / 45.;
    iau = 2000;
/* ---    Cosine factor for diurnal time dependence                       SLWN 17 */
    cosfact = cos(pi180 * 15. * (*time - 15.));
/* ---    Assumed diurnal variation in BL height                          SLWN 19 */
    h__ = cosfact + 3.5;
/* ---    Get MOLA areoid (Rref, km)                                      SLWN 21 */
    topoareo_m05__(xlat, xlonw, &rref, &ctopohgt, &calbedo, &iau);
/* ---    Get height above surface                                        SLWN 23 */
    hgtsfc = *hgtmola - ctopohgt;
/* ---    Zero slope winds at and below surface and above 4.5 km          SLWN 25 */
    if (hgtsfc <= (float)0. || hgtsfc >= h__) {
	*uew = 0.;
	*vns = 0.;
	*wvert = 0.;
	return 0;
    } else {
/* ---      Set typical values of parameters from YSP                     SLWN 32 */
	c0 = .07;
/* ---      Assumed diurnal variation in magnitude of Qs                  SLWN 34 */
	qslam = cosfact * 150. + 550.;
/* ---      Normalized height above surface                               SLWN 36 */
	xi = hgtsfc / h__;
/* ---      Get northward slope (alphay) from MOLA                        SLWN 38 */
	xlatp = *xlat + .25;
	xlatm = *xlat - .25;
	xlonp = *xlonw;
	xlonm = *xlonw;
	if (xlatp > 90.) {
	    xlatp = 180. - xlatp;
	    xlonp += 180.;
	    if (xlonp > 360.) {
		xlonp += -360.;
	    }
	}
	if (xlatm < -90.) {
	    xlatm = -180. - xlatm;
	    xlonm += 180.;
	    if (xlonm > 360.) {
		xlonm += -360.;
	    }
	}
	topoareo_m05__(&xlatp, &xlonp, &rp, &topop, &calbedo, &iau);
	topoareo_m05__(&xlatm, &xlonm, &rm, &topom, &calbedo, &iau);
	alphay = (topop - topom) / (pi180 * .5 * rref);
	alphayw = alphay;
/* ---      Limit slope to ~ 3 degrees (and use sin(alpha) ~ alpha)       SLWN 57 */
	if (abs(alphay) > .0525) {
	    alphay = d_sign(&c_b619, &alphay);
	}
/* ---      Get eastward slope (alphax) from MOLA                         SLWN 59 */
	if (abs(*xlat) > 89.75) {
	    alphax = 0.;
	} else {
	    xlonp = *xlonw - .25;
	    xlonm = *xlonw + .25;
	    if (xlonp < 0.) {
		xlonp += 360.;
	    }
	    if (xlonm > 360.) {
		xlonm += -360.;
	    }
	    topoareo_m05__(xlat, &xlonp, &rp, &topop, &calbedo, &iau);
	    topoareo_m05__(xlat, &xlonm, &rm, &topom, &calbedo, &iau);
	    alphax = (topop - topom) / (pi180 * .5 * cos(pi180 * *xlat) * 
		    rref);
	    alphaxw = alphax;
/* ---        Limit slope to ~ 3 degrees (and use sin(alpha) ~ alpha)     SLWN 71 */
	    if (abs(alphax) > .0525) {
		alphax = d_sign(&c_b619, &alphax);
	    }
	}
/* ---      For xlat ~ 0 use YSP equation (12)                            SLWN 74 */
	if (abs(*xlat) < .01) {
/* Computing 2nd power */
	    d__1 = xi;
	    fupslp = qslam / c0 * ((d__1 * d__1 + 2.) / 3. - xi) * xi;
	    fcross = 0.;
	} else {
/* ---        For xlat ne 0, use YSP equations (13) and (14)              SLWN 79 */
	    omega = 7.0777e-5;
	    f = omega * 2. * sin(pi180 * *xlat);
	    wb = qslam * 2. / (h__ * 1e3 * abs(f));
	    b = sqrt(abs(f) * 1e3 * h__ / (c0 * 2.));
	    p = exp(b * -4.) - exp(b * -2.) * 2. * cos(b * 2.) + 1.;
	    b1 = exp(-b * xi);
	    b2 = exp(-b * (xi + 2.));
	    b3 = exp(-b * (2. - xi));
	    b4 = exp(-b * (4. - xi));
/* ---        Upslope wind factor (YSP equation 13)                       SLWN 89 */
	    fupslp = wb * ((b1 - b4) * sin(b * xi) + (b2 - b3) * sin(b * (2. 
		    - xi))) / p;
/* ---        Cross-slope wind factor (YSP equation 14)                   SLWN 92 */
	    fcross = f / abs(f) * wb * (xi - 1. + ((b1 + b4) * cos(b * xi) - (
		    b2 + b3) * cos(b * (2. - xi))) / p);
	}
/* ---      Eastward and northward wind components from up-slope and      SLWN 96 */
/*         cross-slope components and eastward and northward slopes      SLWN 97 */
	*uew = fupslp * alphax + fcross * alphay;
	*vns = fupslp * alphay - fcross * alphax;
    }
/* ---    Compute assumed time-of-day dependence on wind components       SLWN101 */
    *uew *= cosfact;
    *vns *= sin(pi180 * 15. * (*time - 11.));
    zfactor = 1.;
    if (xi >= .5) {
	zfactor = (cos(pi180 * 360. * (xi - .5)) + 1.) * .5;
    }
    *wvert = zfactor * (alphaxw * (*umean + *uew) + alphayw * (*vmean + *vns))
	    ;
    return 0;
} /* slopewind_m05__ */

/* -----------------------------------------------------------------------SLWN110 */
/* Subroutine */ int species_m05__(doublereal *hgt, doublereal *xlat, 
	doublereal *als, doublereal *zbase, doublereal *amz, doublereal *dens,
	 doublereal *pres, doublereal *temp, integer *iup, doublereal *fmol, 
	doublereal *fmass, doublereal *fmolh2o, doublereal *fmassh2o)
{
    /* Initialized data */

    static doublereal amx[9] = { 44.00903,28.01781,39.96093,31.99799,28.01003,
	    15.999,4.0026,2.01746,1.00873 };
    static doublereal fbar[9] = { .9537,.0275,.0165,.0013,.001,0.,0.,0.,0. };

    /* Format strings */
    static char fmt_40[] = "(\002 CO2=\002,1p,e9.3,\002 N2=\002,e9.3,\002 \
Ar=\002,e9.3,\002 O2=\002,e9.3,\002 CO=\002,e9.3,\002 #/m**3\002/0p,f14.3,4f\
13.3,\002 % by mass\002/f14.3,4f13.3,\002 % by volume\002)";
    static char fmt_50[] = "(\002   O=\002,1p,e9.3,\002 He=\002,e9.3,\002 \
H2=\002,e9.3,\002  H=\002,e9.3,\002   Total=\002,e9.3,\002 #/m**3\002/0p,f14\
.3,3f13.3,\002 % by mass  \002,\002MolWgt=\002,f6.3/f14.3,3f13.3,\002 % volu\
me (mole) fraction\002)";
    static char fmt_60[] = "(\002 H2O=\002,1p,e9.3,\002  O=\002,e9.3,\002 \
He=\002,e9.3,\002 H2=\002,e9.3,\002   Total=\002,e9.3,\002 #/m**3\002/0p,f14\
.3,3f13.3,\002 % by mass  \002,\002MolWgt=\002,f6.3/f14.3,3f13.3,\002 % volu\
me (mole) fraction\002)";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    static doublereal fsum;
    extern doublereal qrhtp_m05__(doublereal *, doublereal *, doublereal *);
    static doublereal amh2o, fn2in, fo2in, d__;
    static integer i__;
    static doublereal fcoin, farin, fmsum, totnd;
    extern /* Subroutine */ int prseas_m05__(doublereal *, doublereal *, 
	    doublereal *);
    static doublereal xndh2o, pr, avn, xnd[9], hgtfact, sum, fmoltot;

    /* Fortran I/O blocks */
    static cilist io___668 = { 0, 0, 0, fmt_40, 0 };
    static cilist io___669 = { 0, 0, 0, fmt_50, 0 };
    static cilist io___670 = { 0, 0, 0, fmt_60, 0 };


/*                                                                       SPEC  3 */
/* ---  Computes species concentrations as mole (or volume) fraction,     SPEC  4 */
/*     mass fraction, and number density (#/m**3).  Average mole         SPEC  5 */
/*     fraction and isotope ratio data are taken from Kieffer et al.,    SPEC  6 */
/*     editors, "Mars" (1992) and Tables A-5 and  A-6 of NASA/TM-2001-   SPEC  7 */
/*     210935 (2001).                                                    SPEC  8 */
/*                                                                       SPEC  9 */
/*     Notes:                                                            SPEC 10 */
/*                                                                       SPEC 11 */
/*     (1) Below 80 km, Mars MGCM assumes pure CO2 atmosphere (for       SPEC 12 */
/*     which molecular weight would be 44.01).  In this height range,    SPEC 13 */
/*     molecular weight computed from the perfect gas law relation       SPEC 14 */
/*     M = R0 * rho * T / p would give values close to, but not exactly  SPEC 15 */
/*     this value.  Deviations would be caused by the fact that the      SPEC 16 */
/*     ratio of the averages is not the same as the average of the       SPEC 17 */
/*     ratio, i.e.                                                       SPEC 18 */
/*          Avg(rho) * Avg(T) / Avg(p)    .ne.    Avg( rho * T / p)      SPEC 19 */
/*                                                                       SPEC 20 */
/*     (2) Below 80 km, this subroutine computes species concentrations  SPEC 21 */
/*     and resultant average molecular weight by assumptions given in    SPEC 22 */
/*     note (3). Therefore average molecular weight given by this        SPEC 23 */
/*     subroutine is not exactly the same as average molecular weight    SPEC 24 */
/*     computed from the perfect gas law [as given in note (1)].         SPEC 25 */
/*                                                                       SPEC 26 */
/*     (3) Below 80 km, this subroutine computes species concentrations  SPEC 27 */
/*     by assuming atmospheric mass density from MGCM is correct,        SPEC 28 */
/*     but species concentrations are calculated using the following     SPEC 29 */
/*     assumptions: (a) Average dry-atmosphere mole fractions (fbar),    SPEC 30 */
/*     are assumed. (b) Mole fractions are adjusted for seasonal         SPEC 31 */
/*     variation of mass of CO2 in the atmosphere, due to freezing and   SPEC 32 */
/*     sublimation of CO2 at the poles. (c) Only the partial pressure    SPEC 33 */
/*     of CO2 is assumed to vary seasonally, not the partial pressures   SPEC 34 */
/*     of other constituents, which are assumed not to freeze out of     SPEC 35 */
/*     the atmosphere. However, mole fractions of all species vary       SPEC 36 */
/*     seasonally due to this effect. (d) Seasonal variation of          SPEC 37 */
/*     total pressure is taken from subroutine PRSEAS_M05, which was     SPEC 38 */
/*     used in the original Stewart thermosphere model, and was          SPEC 39 */
/*     employed in Mars-GRAM up through version 3.8.  (e) Water vapor    SPEC 40 */
/*     concentration is added to the dry atmosphere by assuming relative SPEC 41 */
/*     humidity = 20% (computed by function qrhtp, the same as used in   SPEC 42 */
/*     marsrad.f calculations).                                          SPEC 43 */
/*                                                                       SPEC 44 */
/*     (4) Between 80 km and the base of the Stewart thermosphere (at    SPEC 45 */
/*     altitude zbase), a combination of information is used from MTGCM  SPEC 46 */
/*     and the modified Stewart model. MTGCM data used in Mars-GRAM do   SPEC 47 */
/*     not include calculated mole fractions or number densities.  Mole  SPEC 48 */
/*     fractions between 80 km and zbase height are computed from the    SPEC 49 */
/*     following assumptions: (a) Mole fractions for N2, Ar, and O2 are  SPEC 50 */
/*     assumed to be the same as their value at 80 km (from methods      SPEC 51 */
/*     described in note (3). (b) Mole fractions for He, H2, H, and H2O  SPEC 52 */
/*     are assumed to be zero. (c) Mole fractions for N2, Ar, O2, and CO SPEC 53 */
/*     are assumed to vary linearly from their values at 80 km [from     SPEC 54 */
/*     method in note (3)] to their values at zbase altitude (from the   SPEC 55 */
/*     Stewart model). (d) Mole fractions for CO2 and O are computed     SPEC 56 */
/*     from two constraint conditions - (i) Sum of the mole fractions    SPEC 57 */
/*     must be 1.0, and (ii) Input molecular weight (AMz, from MTGCM     SPEC 58 */
/*     value) is preserved.                                              SPEC 59 */
/*                                                                       SPEC 60 */
/*     (5) From height zbase up until MTGCM data runs out, a combination SPEC 61 */
/*     of information is used from MTGCM and the modified Stewart model. SPEC 62 */
/*     In this height range, the following assumptions are used: (a)     SPEC 63 */
/*     Mole fractions for constituents other than CO2 and O are taken    SPEC 64 */
/*     from the modified Stewart model, (b) Molecular weight from MTGCM  SPEC 65 */
/*     is assumed, (c)  Mole fractions for CO2 and O are computed from   SPEC 66 */
/*     two constraint conditions - (i) Sum of the mole fractions must    SPEC 67 */
/*     be 1.0, and (ii) Input molecular weight (AMz, from MTGCM value)   SPEC 68 */
/*     is preserved.                                                     SPEC 69 */
/*                                                                       SPEC 70 */
/*     (6) Above the top altitude for which MTGCM data are available,    SPEC 71 */
/*     the same methodology is used as in note (5), except that the      SPEC 72 */
/*     input value for molecular weight (AMz) is taken directly from     SPEC 73 */
/*     the modified Stewart model.                                       SPEC 74 */
/*                                                                       SPEC 75 */
/* ---  Molecular weights computed using isotope ratios applicable for    SPEC 79 */
/*     Mars atmosphere. See Kieffer et al., editors, "Mars" (1992) and   SPEC 80 */
/*     Table A-6 of NASA/TM-2001-210935 (2001)                           SPEC 81 */
/* ---  Assumed average mole fractions for dry Mars atmosphere below 80   SPEC 84 */
/*     km altitude.  See Kieffer et al., editors, "Mars" (1992) and      SPEC 85 */
/*     Table A-5 of NASA/TM-2001-210935 (2001)                           SPEC 86 */
/* ---  Avaogadro's number (number per kg-mole)                           SPEC 88 */
    avn = 6.02214e26;
/* ---  Molecular weight for water vapor                                  SPEC 90 */
    amh2o = 18.01646;
/* ---  Store input values of N2, Ar, O2, and CO mole fraction (= values  SPEC 92 */
/*     at height zbase if input height is between 80 km and zbase)       SPEC 93 */
    fn2in = fmol[1];
    farin = fmol[2];
    fo2in = fmol[3];
    fcoin = fmol[4];
/* ---  Assume dry atmosphere, unless modified for heights up to 80 km    SPEC 98 */
    *fmassh2o = 0.;
    *fmolh2o = 0.;
    xndh2o = 0.;
    if (*hgt <= *zbase) {
	prseas_m05__(als, xlat, &pr);
	fmol[0] = 1. - (1. - fbar[0]) / pr;
	fmoltot = fmol[0];
	for (i__ = 1; i__ <= 8; ++i__) {
	    fmol[i__] = fbar[i__] / pr;
	    fmoltot += fmol[i__];
/* L10: */
	}
/* ---    Calculations for heights up to 80 km                            SPEC110 */
	if (*hgt <= 80.) {
	    *amz = 0.;
/* ---      Molecular weight of dry atmosphere                            SPEC113 */
	    for (i__ = 0; i__ <= 8; ++i__) {
		*amz += fmol[i__] * amx[i__];
/* L15: */
	    }
/* ---      Water vapor mass mixing ratio (kg-water/kg-dry atmosphere)    SPEC117 */
	    d__1 = *pres / 100.;
	    *fmassh2o = qrhtp_m05__(&c_b624, temp, &d__1) / 1e3;
/* ---      Water vapor mole fraction                                     SPEC119 */
	    *fmolh2o = *amz * *fmassh2o / amh2o;
/* ---      Rescale total mole fraction                                   SPEC121 */
	    fmoltot += *fmolh2o;
/* ---      Reclculate mole fractions and molecular weight of moist       SPEC123 */
/*         atmosphere                                                    SPEC124 */
	    *amz = (float)0.;
	    for (i__ = 0; i__ <= 8; ++i__) {
		fmol[i__] /= fmoltot;
		*amz += fmol[i__] * amx[i__];
/* L18: */
	    }
	    *amz += *fmolh2o * amh2o;
/* ---      Water vapor number density                                    SPEC131 */
	    xndh2o = *fmolh2o * *dens * avn / *amz;
/* ---    Calculations for heights between 80 km and zbase altitude       SPEC133 */
	} else {
/* ---      Assume N2, Ar, O2, and CO mole fractions varies linearly      SPEC135 */
/*         between 80 km and zbase altitude                              SPEC136 */
	    hgtfact = (*hgt - 80.) / (*zbase - 80.);
	    fmol[1] += (fn2in - fmol[1]) * hgtfact;
	    fmol[2] += (farin - fmol[2]) * hgtfact;
	    fmol[3] += (fo2in - fmol[3]) * hgtfact;
	    fmol[4] += (fcoin - fmol[4]) * hgtfact;
/* ---      Solve for CO2 and O mole fractions using the following        SPEC142 */
/*         assumptions: (a) sum of mole fractions must be 1.0, and       SPEC143 */
/*         (b) input value of molecular weight (AMz) is preserved.       SPEC144 */
/* ---      Set up two simultaneous linear equations from these           SPEC145 */
/*         assumptions.                                                  SPEC146 */
	    fsum = 1. - fmol[1] - fmol[2] - fmol[3] - fmol[4] - fmol[6] - 
		    fmol[7] - fmol[8];
	    fmsum = *amz - fmol[1] * amx[1] - fmol[2] * amx[2] - fmol[3] * 
		    amx[3] - fmol[4] * amx[4] - fmol[6] * amx[6] - fmol[7] * 
		    amx[7] - fmol[8] * amx[8];
	    d__ = amx[5] - amx[0];
/* ---      Re-computed mole fractions for CO2 and O from two linear      SPEC153 */
/*         constraint equations                                          SPEC154 */
	    fmol[0] = (fsum * amx[5] - fmsum) / d__;
	    fmol[5] = (fmsum - amx[0] * fsum) / d__;
	    if (fmol[5] < 0.) {
		fmol[5] = 0.;
		fmol[0] = fsum;
		*amz = fmol[0] * amx[0] + *amz - fmsum;
	    }
	}
/* ---  Calculations for heights above zbase altitude                     SPEC163 */
    } else {
/* ---    Solve for CO2 and O mole fractions using the following          SPEC165 */
/*       assumptions: (a) sum of mole fractions must be 1.0, and         SPEC166 */
/*       (b) input value of molecular weight (AMz) is preserved.         SPEC167 */
/* ---    Set up two simultaneous linear equations from these             SPEC168 */
/*       assumptions.                                                    SPEC169 */
	fsum = 1. - fmol[1] - fmol[2] - fmol[3] - fmol[4] - fmol[6] - fmol[7] 
		- fmol[8];
	fmsum = *amz - fmol[1] * amx[1] - fmol[2] * amx[2] - fmol[3] * amx[3] 
		- fmol[4] * amx[4] - fmol[6] * amx[6] - fmol[7] * amx[7] - 
		fmol[8] * amx[8];
	d__ = amx[5] - amx[0];
/* ---    Re-computed mole fractions for CO2 and O from two linear        SPEC176 */
/*       constraint equations                                            SPEC177 */
	fmol[0] = (fsum * amx[5] - fmsum) / d__;
	fmol[5] = (fmsum - amx[0] * fsum) / d__;
	if (fmol[5] < 0.) {
	    fmol[5] = 0.;
	    fmol[0] = fsum;
	    *amz = fmol[0] * amx[0] + *amz - fmsum;
	}
	if (fmol[0] < 0.) {
	    fmol[0] = 0.;
	    fmol[5] = fsum;
	    *amz = fmol[5] * amx[5] + *amz - fmsum;
	}
    }
/* ---  Calculation of number densities and mass fractions (applicable    SPEC191 */
/*     for all height ranges)                                            SPEC192 */
    totnd = 0.;
    sum = 0.;
/* ---  Species number densities and total number density                 SPEC195 */
    for (i__ = 0; i__ <= 8; ++i__) {
	xnd[i__] = fmol[i__] * *dens * avn / *amz;
	totnd += xnd[i__];
	sum += xnd[i__] * amx[i__];
/* L20: */
    }
/* ---  Add water vapor number density to total                           SPEC201 */
    totnd += xndh2o;
/* ---  Compute mass fraction in % and rescale mole fraction to %         SPEC203 */
    sum += xndh2o * amh2o;
    for (i__ = 0; i__ <= 8; ++i__) {
	fmass[i__] = xnd[i__] * 100. * amx[i__] / sum;
	fmol[i__] *= 100.;
/* L30: */
    }
    if (*iup > 0) {
/* ---    Write species concentration data to LIST.txt file               SPEC210 */
	io___668.ciunit = *iup;
	s_wsfe(&io___668);
	for (i__ = 0; i__ <= 4; ++i__) {
	    do_fio(&c__1, (char *)&xnd[i__], (ftnlen)sizeof(doublereal));
	}
	for (i__ = 0; i__ <= 4; ++i__) {
	    do_fio(&c__1, (char *)&fmass[i__], (ftnlen)sizeof(doublereal));
	}
	for (i__ = 0; i__ <= 4; ++i__) {
	    do_fio(&c__1, (char *)&fmol[i__], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
/* ---    Water vapor not included above 80 km                            SPEC212 */
	if (*hgt > 80.) {
	    io___669.ciunit = *iup;
	    s_wsfe(&io___669);
	    for (i__ = 5; i__ <= 8; ++i__) {
		do_fio(&c__1, (char *)&xnd[i__], (ftnlen)sizeof(doublereal));
	    }
	    do_fio(&c__1, (char *)&totnd, (ftnlen)sizeof(doublereal));
	    for (i__ = 5; i__ <= 8; ++i__) {
		do_fio(&c__1, (char *)&fmass[i__], (ftnlen)sizeof(doublereal))
			;
	    }
	    do_fio(&c__1, (char *)&(*amz), (ftnlen)sizeof(doublereal));
	    for (i__ = 5; i__ <= 8; ++i__) {
		do_fio(&c__1, (char *)&fmol[i__], (ftnlen)sizeof(doublereal));
	    }
	    e_wsfe();
	} else {
/* ---      Water vapor included below 80 km                              SPEC217 */
	    *fmassh2o *= 100.;
	    *fmolh2o *= 100.;
	    io___670.ciunit = *iup;
	    s_wsfe(&io___670);
	    do_fio(&c__1, (char *)&xndh2o, (ftnlen)sizeof(doublereal));
	    for (i__ = 5; i__ <= 7; ++i__) {
		do_fio(&c__1, (char *)&xnd[i__], (ftnlen)sizeof(doublereal));
	    }
	    do_fio(&c__1, (char *)&totnd, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*fmassh2o), (ftnlen)sizeof(doublereal));
	    for (i__ = 5; i__ <= 7; ++i__) {
		do_fio(&c__1, (char *)&fmass[i__], (ftnlen)sizeof(doublereal))
			;
	    }
	    do_fio(&c__1, (char *)&(*amz), (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&(*fmolh2o), (ftnlen)sizeof(doublereal));
	    for (i__ = 5; i__ <= 7; ++i__) {
		do_fio(&c__1, (char *)&fmol[i__], (ftnlen)sizeof(doublereal));
	    }
	    e_wsfe();
	}
    }
    return 0;
} /* species_m05__ */

/* -----------------------------------------------------------------------SPEC235 */
/* Subroutine */ int subltchk_m05__(doublereal *temp, doublereal *pres, 
	doublereal *tsubl)
{
    /* Builtin functions */
    double log(doublereal);

/* ---  Assure temperatures greater than sublimation point for CO2.       STCK  2 */
/*     Coefficients from Kieffer et al. eds. "Mars" (Univ. Arizona       STCK  3 */
/*     Press book) 1992, page 959.                                       STCK  4 */
/*     Inputs are temp = temperature (K), pres = pressure (N/m**2)       STCK  5 */
    *tsubl = 3182.48 / (23.3494 - log(*pres / 100.));
    if (*temp < *tsubl) {
	*temp = *tsubl;
    }
    return 0;
} /* subltchk_m05__ */

/* -----------------------------------------------------------------------STCK 11 */
/* Subroutine */ int surfterp_m05__(integer *khgt, doublereal *time, 
	doublereal *tmgcm, doublereal *pmgcm, doublereal *dmgcm, doublereal *
	umgcm, doublereal *vmgcm, doublereal *hpres, doublereal *hdens, 
	doublereal *ctopohgt, doublereal *tempday, doublereal *presday, 
	doublereal *densday, doublereal *uwndday, doublereal *vwndday, 
	doublereal *hpres0, doublereal *tempmax, doublereal *tempmin, 
	doublereal *densmax, doublereal *densmin, doublereal *tat5m, integer *
	idaydata)
{
    /* Builtin functions */
    double exp(doublereal);

    /* Local variables */
    static doublereal pzh1[8]	/* was [2][2][2] */, t1st, p1st, d1st, z1st, 
	    tbar, tday[16]	/* was [2][2][2][2] */, uday[16]	/* 
	    was [2][2][2][2] */, vday[16]	/* was [2][2][2][2] */, tmin[
	    16]	/* was [2][2][2][2] */, pmax[8]	/* was [2][2][2] */, tmax[16]	
	    /* was [2][2][2][2] */, pmin[8]	/* was [2][2][2] */, upolefac;
    extern doublereal tidex_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);
    extern /* Subroutine */ int fourd_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *)
	    ;
    extern doublereal tidey_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), zlogr_m05__(doublereal 
	    *, doublereal *, char *, ftnlen);
    static doublereal tbar0, dzk1h, p1min, p1max, t1min[8]	/* was [2][2][
	    2] */, tmin1, t1max[8]	/* was [2][2][2] */, tmax1, pzk1h;
    static integer i__, j, l, m;
    static doublereal dzk1h1;
    static integer itime;
    static doublereal ptime;
    extern /* Subroutine */ int threed_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static doublereal ttime, a1, xtime, a2, d0, pzk1h1, hpmin, hpmax, pzero, 
	    p0, p1, p2, t0, u0, v0, tzero, rzero, hdens0, t1time, tm[16]	
	    /* was [2][2][2][2] */, um[16]	/* was [2][2][2][2] */, vm[16]
	    	/* was [2][2][2][2] */, height;
    static integer k1h;
    static doublereal a1t, a2t, p1t, p2t, ts0[16]	/* was [2][2][2][2] */
	    , tszero, pz0[8]	/* was [2][2][2] */, pz1[8]	/* was [2][2][
	    2] */, rz0[8]	/* was [2][2][2] */, tz0[8]	/* was [2][2][
	    2] */, tz1[8]	/* was [2][2][2] */, dzh[8]	/* was [2][2][
	    2] */, polefac, pzh[8]	/* was [2][2][2] */, dzh1[8]	/* 
	    was [2][2][2] */;

/* ---    Interpolates Ames Mars General Circulation Model (MGCM) surface STRP  4 */
/*       data to a given latitude, longitude, time of year (Ls), and     STRP  5 */
/*       dust optical depth, for a given height index (khgt) and time of STRP  6 */
/*       day (time).                                                     STRP  7 */
/*       Some input data is provided by the Common "Interp".             STRP  8 */
/* ---    Set parameter values for number of heights, boundary layer      STRP  9 */
/*       levels, latitudes, longitudes, and number of dust optical depth STRP 10 */
/*       values                                                          STRP 11 */
/* ---    MGCM surface data arrays                                        STRP 22 */
/* ---    MGCM 0-80 km data arrays                                        STRP 38 */
/* ---    Establish MGCM surface values at corners of a 4-dimensional     STRP 61 */
/*       cube in latitude-longitude-Ls-dust space, at a given height     STRP 62 */
/*       index (khgt) and time of day (time)                             STRP 63 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	upolefac = 1.;
	if (interp_m05__1.ilat == 1) {
	    polefac = i__ - 1.;
	} else if (interp_m05__1.ilat == 24) {
	    polefac = 2. - i__;
	}
	if (interp_m05__1.ilatw == 2) {
	    if (i__ == 1) {
		upolefac = interp_m05__1.wpolefac;
	    }
	} else if (interp_m05__1.ilatw == 24) {
	    if (i__ == 2) {
		upolefac = interp_m05__1.wpolefac;
	    }
	}
	for (j = 1; j <= 2; ++j) {
	    for (l = 1; l <= 2; ++l) {
		for (m = 1; m <= 2; ++m) {
/* ---      Daily mean temperature at level khgt                          STRP 80 */
		    t0 = surfdata_m05__1.tsa0[*khgt + (interp_m05__1.ilat + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
		    tday[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = t0;
/* ---      Temperature tide amplitudes and phases                        STRP 83 */
		    a1t = surfdata_m05__1.tsa1[*khgt + (interp_m05__1.ilat + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979] * polefac;
		    p1t = surfdata_m05__1.tsp1[*khgt + (interp_m05__1.ilat + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
		    a2t = surfdata_m05__1.tsa2[*khgt + (interp_m05__1.ilat + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979] * polefac;
		    p2t = surfdata_m05__1.tsp2[*khgt + (interp_m05__1.ilat + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
/* ---      Temperature at corners of 4-D cube                            STRP 88 */
		    tm[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = 
			    tidex_m05__(&t0, &a1t, &p1t, &a2t, &p2t, time);
/* ---      Daily mean temperature at surface                             STRP 90 */
		    ts0[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = 
			    surfdata_m05__1.tsa0[(interp_m05__1.ilat + i__ - 
			    1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39978];
/* ---      Max and Min temperatures at corners of 4-D cube               STRP 92 */
		    tmax[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = -9999.;
		    tmin[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = 9999.;
		    if (*idaydata > 0) {
			for (itime = 0; itime <= 23; ++itime) {
			    xtime = (real) itime;
			    ttime = tidex_m05__(&t0, &a1t, &p1t, &a2t, &p2t, &
				    xtime);
			    if (ttime > tmax[i__ + (j + (l + (m << 1) << 1) <<
				     1) - 15]) {
				tmax[i__ + (j + (l + (m << 1) << 1) << 1) - 
					15] = ttime;
			    }
			    if (ttime < tmin[i__ + (j + (l + (m << 1) << 1) <<
				     1) - 15]) {
				tmin[i__ + (j + (l + (m << 1) << 1) << 1) - 
					15] = ttime;
			    }
/* L50: */
			}
		    }
/* ---      Daily mean EW wind at level khgt                              STRP103 */
		    u0 = surfdata_m05__1.usa0[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
		    uday[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = u0;
/* ---      EW wind tide coefficient amplitudes and phases                STRP106 */
		    a1 = surfdata_m05__1.usa1[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979] * upolefac;
		    p1 = surfdata_m05__1.usp1[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
		    a2 = surfdata_m05__1.usa2[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979] * upolefac;
		    p2 = surfdata_m05__1.usp2[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
/* ---      EW wind at corners of 4-D cube                                STRP111 */
		    um[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = 
			    tidex_m05__(&u0, &a1, &p1, &a2, &p2, time);
/* ---      Daily mean NS wind at level khgt                              STRP113 */
		    v0 = surfdata_m05__1.vsa0[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
		    vday[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = v0;
/* ---      NS wind coefficient amplitudes and phases                     STRP116 */
		    a1 = surfdata_m05__1.vsa1[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979] * upolefac;
		    p1 = surfdata_m05__1.vsp1[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
		    a2 = surfdata_m05__1.vsa2[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979] * upolefac;
		    p2 = surfdata_m05__1.vsp2[*khgt + (interp_m05__1.ilatw + 
			    i__ - 1 + (interp_m05__1.jlon + j - 1 + (
			    interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + 
			    m - 1) * 13) * 41) * 25) * 3 - 39979];
/* ---      NS wind at corners of 4-D cube                                STRP121 */
		    vm[i__ + (j + (l + (m << 1) << 1) << 1) - 15] = 
			    tidex_m05__(&v0, &a1, &p1, &a2, &p2, time);
/* L100: */
		}
/* L101: */
	    }
/* L102: */
	}
/* L103: */
    }
/* ---    Use 4-D interpolation to get temperature, EW wind, NS wind,     STRP127 */
/*       and daily mean surface temperature at given latitude,           STRP128 */
/*       longitude, Ls, and dust optical depth                           STRP129 */
    fourd_m05__(&interp_m05__1.dlat, &interp_m05__1.dlon, &interp_m05__1.dls, 
	    &interp_m05__1.ddust, tm, tmgcm, &c__0);
    fourd_m05__(&interp_m05__1.dlatw, &interp_m05__1.dlon, &interp_m05__1.dls,
	     &interp_m05__1.ddust, um, umgcm, &c__0);
    fourd_m05__(&interp_m05__1.dlatw, &interp_m05__1.dlon, &interp_m05__1.dls,
	     &interp_m05__1.ddust, vm, vmgcm, &c__0);
    fourd_m05__(&interp_m05__1.dlat, &interp_m05__1.dlon, &interp_m05__1.dls, 
	    &interp_m05__1.ddust, ts0, &tszero, &c__0);
    fourd_m05__(&interp_m05__1.dlat, &interp_m05__1.dlon, &interp_m05__1.dls, 
	    &interp_m05__1.ddust, tday, tempday, &c__0);
    fourd_m05__(&interp_m05__1.dlat, &interp_m05__1.dlon, &interp_m05__1.dls, 
	    &interp_m05__1.ddust, tmax, tempmax, &c__0);
    fourd_m05__(&interp_m05__1.dlat, &interp_m05__1.dlon, &interp_m05__1.dls, 
	    &interp_m05__1.ddust, tmin, tempmin, &c__0);
    fourd_m05__(&interp_m05__1.dlatw, &interp_m05__1.dlon, &interp_m05__1.dls,
	     &interp_m05__1.ddust, uday, uwndday, &c__0);
    fourd_m05__(&interp_m05__1.dlatw, &interp_m05__1.dlon, &interp_m05__1.dls,
	     &interp_m05__1.ddust, vday, vwndday, &c__0);
/* ---    k1h = height index just below k1st                              STRP139 */
    k1h = interp_m05__1.k1st - 1;
    if (k1h < 1) {
	k1h = 1;
    }
/* ---    Establish MGCM values at height levels k1h, k1st and corners of STRP142 */
/*       a 3-dimensional cube in latitude-Ls-dust space, at given time   STRP143 */
/*       of day (time)                                                   STRP144 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	if (interp_m05__1.ilat == 1) {
	    polefac = i__ - 1.;
	} else if (interp_m05__1.ilat == 24) {
	    polefac = 2. - i__;
	}
	for (l = 1; l <= 2; ++l) {
	    for (m = 1; m <= 2; ++m) {
/* ---      Daily average pressure and density at level k1h               STRP154 */
		pzh[i__ + (l + (m << 1) << 1) - 7] = mgcmdata_m05__1.pza0[k1h 
			+ (interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + 
			l - 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 
			17 - 5543];
		dzh[i__ + (l + (m << 1) << 1) - 7] = mgcmdata_m05__1.dza0[k1h 
			+ (interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + 
			l - 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 
			17 - 5543];
		pzh1[i__ + (l + (m << 1) << 1) - 7] = mgcmdata_m05__1.pza0[
			k1h + 1 + (interp_m05__1.ilat + i__ - 1 + (
			interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + m - 
			1) * 13) * 25) * 17 - 5543];
		dzh1[i__ + (l + (m << 1) << 1) - 7] = mgcmdata_m05__1.dza0[
			k1h + 1 + (interp_m05__1.ilat + i__ - 1 + (
			interp_m05__1.ls + l - 1 + (interp_m05__1.mdust + m - 
			1) * 13) * 25) * 17 - 5543];
/* ---      Pressure tide coefficient amplitudes and phases               STRP159 */
		p0 = mgcmdata_m05__1.pza0[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543];
		a1 = mgcmdata_m05__1.pza1[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543] * polefac;
		p1 = mgcmdata_m05__1.pzp1[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543];
		a2 = mgcmdata_m05__1.pza2[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543] * polefac;
		p2 = mgcmdata_m05__1.pzp2[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543];
/* ---      Pressure values at corners of 3-D cube                        STRP165 */
		pz1[i__ + (l + (m << 1) << 1) - 7] = tidey_m05__(&p0, &a1, &
			p1, &a2, &p2, time);
/* ---      Daily average pressure at level k1st                          STRP167 */
		pz0[i__ + (l + (m << 1) << 1) - 7] = p0;
/* ---      Level k1st Pressure at corners of 3-D cube                    STRP169 */
		pmax[i__ + (l + (m << 1) << 1) - 7] = -9999.;
		pmin[i__ + (l + (m << 1) << 1) - 7] = 9999.;
		if (*idaydata > 0) {
		    for (itime = 0; itime <= 23; ++itime) {
			xtime = (real) itime;
			ptime = tidey_m05__(&p0, &a1, &p1, &a2, &p2, &xtime);
			if (ptime > pmax[i__ + (l + (m << 1) << 1) - 7]) {
			    pmax[i__ + (l + (m << 1) << 1) - 7] = ptime;
			}
			if (ptime < pmin[i__ + (l + (m << 1) << 1) - 7]) {
			    pmin[i__ + (l + (m << 1) << 1) - 7] = ptime;
			}
/* L150: */
		    }
		}
/* ---      Temperature tide coefficient amplitudes and phases            STRP180 */
		t0 = mgcmdata_m05__1.tza0[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543];
		a1 = mgcmdata_m05__1.tza1[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543] * polefac;
		p1 = mgcmdata_m05__1.tzp1[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543];
		a2 = mgcmdata_m05__1.tza2[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543] * polefac;
		p2 = mgcmdata_m05__1.tzp2[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543];
/* ---      temperature values at corners of 3-D cube                     STRP186 */
		tz1[i__ + (l + (m << 1) << 1) - 7] = tidex_m05__(&t0, &a1, &
			p1, &a2, &p2, time);
/* ---      Level k1st Temperature at corners of 3-D cube                 STRP188 */
		t1max[i__ + (l + (m << 1) << 1) - 7] = -9999.;
		t1min[i__ + (l + (m << 1) << 1) - 7] = 9999.;
		if (*idaydata > 0) {
		    for (itime = 0; itime <= 23; ++itime) {
			xtime = (real) itime;
			t1time = tidex_m05__(&t0, &a1, &p1, &a2, &p2, &xtime);
			if (t1time > t1max[i__ + (l + (m << 1) << 1) - 7]) {
			    t1max[i__ + (l + (m << 1) << 1) - 7] = t1time;
			}
			if (t1time < t1min[i__ + (l + (m << 1) << 1) - 7]) {
			    t1min[i__ + (l + (m << 1) << 1) - 7] = t1time;
			}
/* L160: */
		    }
		}
/* ---      Daily average temperature at level k1st                       STRP199 */
		tz0[i__ + (l + (m << 1) << 1) - 7] = t0;
/* ---      Daily average density at level k1st                           STRP201 */
		d0 = mgcmdata_m05__1.dza0[interp_m05__1.k1st + (
			interp_m05__1.ilat + i__ - 1 + (interp_m05__1.ls + l 
			- 1 + (interp_m05__1.mdust + m - 1) * 13) * 25) * 17 
			- 5543];
/* ---      Gas constant from pressure, density, and temperature          STRP203 */
		rz0[i__ + (l + (m << 1) << 1) - 7] = 190.;
		if (t0 != 0. && d0 != 0.) {
		    rz0[i__ + (l + (m << 1) << 1) - 7] = p0 / (t0 * d0);
		}
/* L200: */
	    }
/* L201: */
	}
/* L202: */
    }
/* ---    Do 3-D interpolation on pressure                                STRP209 */
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, pzh, &pzk1h, &c__1);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, pzh1, &pzk1h1, &c__1);
/* ---    Daily average pressure scale height                             STRP212 */
    *hpres0 = 5. / zlogr_m05__(&pzk1h, &pzk1h1, "STRP-01", (ftnlen)7);
/* ---    Do 3-D interpolation on density                                 STRP214 */
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, dzh, &dzk1h, &c__1);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, dzh1, &dzk1h1, &c__1);
/* ---    Daily average density scale height                              STRP217 */
    hdens0 = 5. / zlogr_m05__(&dzk1h, &dzk1h1, "STRP-02", (ftnlen)7);
/* ---    Do 3-D interpolation on daily mean temperature                  STRP219 */
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, tz0, &tzero, &c__0);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, pz0, &pzero, &c__1);
/* ---    Daily average layer mean temperature                            STRP222 */
    tbar0 = (tzero + tszero) / 2.;
/* ---    Do 3-D interpolation on gas constant                            STRP224 */
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, rz0, &rzero, &c__0);
/* ---    Do 3-D interpolation on temperature and pressure                STRP226 */
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, tz1, &t1st, &c__0);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, pz1, &p1st, &c__1);
/* ---    Do 3-D interpolation on max,min pressure at level k1st          STRP229 */
    if (*idaydata == 1) {
	threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
		interp_m05__1.ddust, pmax, &p1max, &c__1);
    } else {
	threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
		interp_m05__1.ddust, pmax, &p1max, &c__0);
    }
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, pmin, &p1min, &c__1);
/* ---    Do 3-D interpolation on max,min temperature at level k1st       STRP232 */
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, t1max, &tmax1, &c__0);
    threed_m05__(&interp_m05__1.dlat, &interp_m05__1.dls, &
	    interp_m05__1.ddust, t1min, &tmin1, &c__0);
/* ---    Density from gas law                                            STRP235 */
    d1st = p1st / (rzero * t1st);
/* ---    Layer mean temperature at current time                          STRP237 */
    if (*khgt == 2 && *tat5m <= 0.) {
	*tat5m = *tmgcm;
    }
    tbar = (t1st + *tat5m) / 2.;
/* ---    Pressure scale height and density scale height at current time  STRP240 */
    *hpres = *hpres0 * tbar / tbar0;
    *hdens = hdens0 * tbar / tbar0;
/* ---    Adjust pressure to height level, using pressure scale height    STRP243 */
    height = *ctopohgt + mgcmparm_m05__1.dzbl[*khgt - 1];
    z1st = (interp_m05__1.k1st - 1.) * 5.;
    *pmgcm = p1st * exp((z1st - height) / *hpres);
    *presday = pzero * exp((z1st - height) / *hpres0);
/* ---    Compute density from gas law, using pressure and temperature    STRP248 */
    *dmgcm = *pmgcm / (rzero * *tmgcm);
    *densday = *presday / (rzero * *tempday);
    *densmax = -9999.;
    *densmin = 9999.;
    if (*idaydata > 0) {
/* ---    Daily maximum and minimum density                               STRP254 */
	hpmin = *hpres0 * .5 * (tmax1 + *tempmax) / tbar0;
	hpmax = *hpres0 * .5 * (tmax1 + *tempmin) / tbar0;
	p1max *= exp((z1st - height) / hpmax);
	p1min *= exp((z1st - height) / hpmin);
	*densmax = *densday * (p1max / *presday) * (*tempday / *tempmin);
	*densmin = *densday * (p1min / *presday) * (*tempday / *tempmax);
    }
    return 0;
} /* surfterp_m05__ */

/* -----------------------------------------------------------------------STRP264 */
/* Subroutine */ int stewart2_m05__(doublereal *raui, doublereal *lat, 
	doublereal *lon, doublereal *lst, doublereal *totalprz, doublereal *
	tz, doublereal *totalmdz, doublereal *chgt, doublereal *rstar, 
	doublereal *h__, doublereal *molwtg, doublereal *sigma, integer *iu0, 
	doublereal *sunlat, doublereal *deltatex, doublereal *tinf, 
	doublereal *tf, doublereal *zf, doublereal *hrho, doublereal *requa, 
	doublereal *rpole, doublereal *tgrad)
{
    /* Format strings */
    static char fmt_160[] = "(f7.2,f8.2,3f7.2)";

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double exp(doublereal);
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle(), s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), 
	    e_wsfe();

    /* Local variables */
    static integer flag__;
    static doublereal fbar, rref, dmdz, ctopohgt, totalndz, tinf0;
    static integer i__;
    static doublereal scale;
    extern /* Subroutine */ int escalc_m05__(doublereal *, doublereal *, 
	    doublereal *);
    static doublereal fbarr, fmolx[9], albedo, es[12], rf, gz;
    extern /* Subroutine */ int rellips_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal tf0, zf0;
    extern /* Subroutine */ int thermos_m05__(integer *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *,
	     doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal rau, rog, ndz[9], mdz[9], zzf, prz[9], oldrref;
    extern /* Subroutine */ int thermpar_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *);

    /* Fortran I/O blocks */
    static cilist io___754 = { 0, 0, 0, 0, 0 };
    static cilist io___756 = { 0, 0, 0, 0, 0 };
    static cilist io___758 = { 0, 0, 0, 0, 0 };
    static cilist io___759 = { 0, 0, 0, 0, 0 };
    static cilist io___760 = { 0, 0, 0, fmt_160, 0 };
    static cilist io___761 = { 0, 0, 0, 0, 0 };


/*                                                                       STW2  4 */
/* ---  TIME-DEPENDENT MARS ATMOSPHERE MODEL, FORTRAN VERSION OF PROGRAM  STW2  5 */
/*     BY IAN STEWART, LABORATORY FOR ATMOSPHERIC AND SPACE PHYSICS,     STW2  6 */
/*     UNIV. OF COLORADO. FINAL REPORT JPL PO # NQ-802429                STW2  7 */
/* --------------------------------------------------------------------   STW2  8 */
/* ---  ES ARE STD DEVIATIONS FROM NOMINAL VALUES                         STW2 15 */
/* ---  0,2,....10  LONG - TERM                                           STW2 16 */
/* ---  1,3.....,11  SHORT-TERM                                           STW2 17 */
/* ---  FOR   FBAR,TINF,FOXY,AOXY,ZF,DZDUST                               STW2 18 */
/* ---  RETURNS TEMP,#DENS,MDENS,PRESSURE                                 STW2 19 */
    flag__ = 0;
    escalc_m05__(&therm_m05__1.stdl, sigma, es);
/* ---  DEVIATIONS FROM NOMINAL VALUES                                    STW2 24 */
    fbar = therm_m05__1.f107 * exp(es[0]);
/* ---  3 MONTH RUNNING MEAN OF 10.7 CM SOLAR FLUX                        STW2 26 */
/* ---  IN UNITS OF 1.0E-22 W/CM**2                                       STW2 27 */
    rau = *raui;
/* ---  Convert solar 10.7 cm flux to Mars position                       STW2 29 */
/* Computing 2nd power */
    d__1 = rau;
    fbarr = fbar / (d__1 * d__1);
    rellips_m05__(lat, lon, &rref, chgt, &gz, &oldrref, &ctopohgt, &albedo, 
	    requa, rpole);
/* ---  Evaluate the basic parameters for the thermosphere model          STW2 33 */
    thermpar_m05__(&rau, &fbarr, lat, lst, sunlat, &tinf0, &tf0, &zf0, &scale)
	    ;
    if (flag__ > 0) {
	io___754.ciunit = *iu0;
	s_wsle(&io___754);
	do_lio(&c__9, &c__1, " RREF, RAU, GZ = ", (ftnlen)17);
	do_lio(&c__5, &c__1, (char *)&rref, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&rau, (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&gz, (ftnlen)sizeof(doublereal));
	e_wsle();
    }
/* ---  Height above base of thermosphere                                 STW2 36 */
    zzf = *chgt - *zf;
    if (flag__ > 0) {
	io___756.ciunit = *iu0;
	s_wsle(&io___756);
	do_lio(&c__9, &c__1, " ZF,CHGT,ZZF = ", (ftnlen)15);
	do_lio(&c__5, &c__1, (char *)&(*zf), (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&(*chgt), (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&zzf, (ftnlen)sizeof(doublereal));
	e_wsle();
    }
    rf = rref + *zf;
/* ---  Exospheric temperature                                            STW2 40 */
    *tinf = tinf0 * exp(es[2] + es[3]) + *deltatex;
    if (flag__ > 0) {
	io___758.ciunit = *iu0;
	s_wsle(&io___758);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
	io___759.ciunit = *iu0;
	s_wsle(&io___759);
	do_lio(&c__9, &c__1, "FROM PROC. DRAG-- ZF,RF,TINF,TF = ", (ftnlen)34)
		;
	e_wsle();
	io___760.ciunit = *iu0;
	s_wsfe(&io___760);
	do_fio(&c__1, (char *)&(*zf), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&rf, (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*tinf), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*tf), (ftnlen)sizeof(doublereal));
	e_wsfe();
	io___761.ciunit = *iu0;
	s_wsle(&io___761);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
    }
    thermos_m05__(&flag__, es, tinf, tf, lat, lon, lst, zf, &rf, &zzf, 
	    totalprz, &totalndz, tz, molwtg, prz, ndz, mdz, totalmdz, iu0, &
	    scale, tgrad, &dmdz, requa, rpole, fmolx);
    if (abs(*sigma) < .01) {
	for (i__ = 0; i__ <= 8; ++i__) {
	    therm_m05__1.fmol[i__] = fmolx[i__];
/* L170: */
	}
    }
/* ---  SCALE HEIGHT, km                                                  STW2 57 */
    rog = *rstar / (*molwtg * 1e3 * gz);
    *h__ = rog * *tz;
    *hrho = *h__ / (*tgrad * rog + 1. - *h__ / *molwtg * dmdz);
/* ---  Convert pressure to N/m**2                                        STW2 61 */
    *totalprz *= (float)1e5;
/* ---  Convert density to kg/m**3                                        STW2 63 */
    *totalmdz *= 1e3;
    return 0;
} /* stewart2_m05__ */

/* -----------------------------------------------------------------------STW2 67 */
/* Subroutine */ int threed_m05__(doublereal *dx, doublereal *dy, doublereal *
	dz, doublereal *array, doublereal *value, integer *lint)
{
    /* Builtin functions */
    double log(doublereal), exp(doublereal);

    /* Local variables */
    static doublereal dxp, dyp, dzp;

/* ---    3-Dimensional linear interpolation within a 1x1x1 cube (x,y,z)  THRD  2 */
/*       of Array(2,2,2) to position dx,dy,dz (all 0-1).                 THRD  3 */
/*       Value is value of interpolated output.                          THRD  4 */
/* ---    Complementary displacements in x,y,z                            THRD  7 */
    /* Parameter adjustments */
    array -= 7;

    /* Function Body */
    dxp = 1. - *dx;
    dyp = 1. - *dy;
    dzp = 1. - *dz;
    if (*lint != 1) {
/* ---      3-D interpolated Value from Array                             THRD 11 */
	*value = dxp * dyp * dzp * array[7] + dxp * dyp * *dz * array[11] + 
		dxp * *dy * dzp * array[9] + *dx * dyp * dzp * array[8] + dxp 
		* *dy * *dz * array[13] + *dx * dyp * *dz * array[12] + *dx * 
		*dy * dzp * array[10] + *dx * *dy * *dz * array[14];
    } else {
/* ---      3-D interpolated Value from log of Array                      THRD 15b */
	*value = exp(dxp * dyp * dzp * log(array[7]) + dxp * dyp * *dz * log(
		array[11]) + dxp * *dy * dzp * log(array[9]) + *dx * dyp * 
		dzp * log(array[8]) + dxp * *dy * *dz * log(array[13]) + *dx *
		 dyp * *dz * log(array[12]) + *dx * *dy * dzp * log(array[10])
		 + *dx * *dy * *dz * log(array[14]));
    }
    return 0;
} /* threed_m05__ */

/* -----------------------------------------------------------------------THRD 18 */
/* Subroutine */ int thermos_m05__(integer *flag__, doublereal *es, 
	doublereal *tinf, doublereal *tf, doublereal *lat, doublereal *lon, 
	doublereal *lst, doublereal *zf, doublereal *rf, doublereal *zzf, 
	doublereal *totalprz, doublereal *totalndz, doublereal *tz, 
	doublereal *molwtg, doublereal *prz, doublereal *ndz, doublereal *mdz,
	 doublereal *totalmdz, integer *iu0, doublereal *scale, doublereal *
	tgrad, doublereal *dmdz, doublereal *requa, doublereal *rpole, 
	doublereal *fmol)
{
    /* Format strings */
    static char fmt_220[] = "(2f6.1,f5.1,6e10.3/17x,6e10.3)";

    /* Builtin functions */
    double exp(doublereal), sin(doublereal), cos(doublereal), sqrt(doublereal)
	    , log(doublereal);
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe(), 
	    s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();

    /* Local variables */
    static doublereal pfhe, rref, exps, sysc, p1bar, ctopohgt;
    static integer i__, j, k;
    static doublereal m[9], radeg, ratio, tsubl, sprzi, sprzj, ff[9], bk, gf, 
	    ao, hh[9], dm[9], fo, albedo, pressf;
    extern /* Subroutine */ int rellips_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *);
    static doublereal stotpr, pfh, xff[9], xhh[9], xdm[9], ysc, oldrref, stz, 
	    amumass;
    extern /* Subroutine */ int subltchk_m05__(doublereal *, doublereal *, 
	    doublereal *);
    static doublereal pfh2, smolwtg;

    /* Fortran I/O blocks */
    static cilist io___807 = { 0, 0, 0, fmt_220, 0 };
    static cilist io___809 = { 0, 0, 0, 0, 0 };


/* ---  RETURNS TEMP & COMPOSITION VS ALTITUDE ABOVE ZF                   THRM  9 */
/* ---  ES (EPS*SIG)   DEVIATIONS FROM NOMINAL VALUES                     THRM 10 */
/* ---  0,2,....,10 LONG-TERM                                             THRM 11 */
/* ---  1,3,....11   SHORT-TERM                                           THRM 12 */
/* ---  FOR   FBAR, TINF, FOXY, ADXY,ZF,DZDUST                            THRM 13 */
/* ---  ZZF  = INPUT ALTITUDES ABOVE ZF                                   THRM 14 */
/* ---  TZ  = TEMPERATURE VS ALTITUDE, DEG K                              THRM 15 */
/* ---  NDZ= # DENSITY VS ALTITUDE, #/cc                                  THRM 16 */
/* ---  MDZ= MASS DENSITY VS ALTITUDE, gm/cc                              THRM 17 */
/* ---  PRZ= PRESSURE VS ALTITUDE, BARS                                   THRM 18 */
/* ---  FLAG=    0  CALCULATIONS ONLY                                     THRM 19 */
/* ---  FLAG=   1 PRINT DEBUG OUTPUT                                      THRM 20 */
    radeg = 57.29577958;
/*     RADEG = DEGREES/RADIAN                                            THRM 27 */
    bk = 1.38065e-16;
    amumass = 1.66054e-24;
/* ---  CONSTANT FOR NUMBER DENSITY CALCULATIONS                          THRM 30 */
    rellips_m05__(lat, lon, &rref, zf, &gf, &oldrref, &ctopohgt, &albedo, 
	    requa, rpole);
    gf *= 100.;
/* ---  ACC. OF GRAVITY AT ZF                                             THRM 34 */
/* ---  PRESSF = 1.24E-9   (as originally in Stewart's model)             THRM 35 */
    pressf = 1.26e-9;
/* ---  1.26E-3 dynes/cm**2 = 1.26 nbar AT ZF, BASE OF THERMOSPHERE       THRM 37 */
/* ---  P1BAR = 1.013E6   (as originally in Stewart's model)              THRM 38 */
    p1bar = 1e6;
/* ---  EARTH SURFACE PRESSURE                                            THRM 40 */
    ao = (es[7] + 1.) * .18;
/* ---  AO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT              THRM 42 */
    fo = exp(es[4] + es[5]) * .01;
/* ---  FO = PARAMETER IN EQUATION FOR ATOMIC OXYGEN CONTENT I            THRM 44 */
/* ---  Molecular weights using Mars isotopic ratios                      THRM 45 */
    m[0] = 44.009;
/* ---  CO2 MOLECULAR WEIGHT                                              THRM 47 */
    m[1] = 28.0178;
/* ---  N2                                                                THRM 49 */
    m[2] = 39.9609;
/* ---  ARGON                                                             THRM 51 */
    m[3] = 31.998;
/* ---  MOLECULAR OXYGEN                                                  THRM 53 */
    m[4] = 28.01;
/* ---  CARBON MONOXIDE                                                   THRM 55 */
    m[5] = 15.999;
/* ---  ATOMIC OXYGEN                                                     THRM 57 */
    m[6] = 4.0026;
/* ---  HELIUM                                                            THRM 59 */
    m[7] = 2.01746;
/* ---  MOLECULAR HYDROGEN                                                THRM 61 */
    m[8] = 1.00873;
/* ---  ATOMIC HYDROGEN MOLECULAR WEIGHT                                  THRM 63 */
    dm[0] = amumass * m[0];
/* ---  CO2 MOLECULAR MASS                                                THRM 65 */
    dm[1] = amumass * m[1];
/* ---  N2 MOLECULAR MASS                                                 THRM 67 */
    dm[2] = amumass * m[2];
/* ---  ARGON MOLECULAR MASS                                              THRM 69 */
    dm[3] = amumass * m[3];
/* ---  02 MOLECULAR MASS                                                 THRM 71 */
    dm[4] = amumass * m[4];
/* ---  CARBON MONOXIDE MOLECULAR MASS                                    THRM 73 */
    dm[5] = amumass * m[5];
/* ---  ATOMIC OXYGEN MOLECULAR MASS                                      THRM 75 */
    xdm[0] = amumass * m[6];
/* ---  HELIUM MOLECULAR MASS                                             THRM 77 */
    xdm[1] = amumass * m[7];
/* ---  H2 MOLECULAR MASS                                                 THRM 79 */
    xdm[2] = amumass * m[8];
/* ---  H   MOLECULAR MASS                                                THRM 81 */
/* ---  THE FOLLOWING IS THE COMPOSITION OF THE HETEROSPHERE              THRM 82 */
    ff[0] = .932;
/* ---  CO2                                                               THRM 84 */
    ff[1] = .027;
/* ---  NITROGEN                                                          THRM 86 */
    ff[2] = .016;
/* ---  ARGON                                                             THRM 88 */
    ff[3] = .002;
/* ---  MOLECULAR OXYGEN                                                  THRM 90 */
    ff[4] = .013;
/* ---  CARBON MONOXIDE                                                   THRM 92 */
    ff[5] = .01;
/* ---  ATOMIC OXYGEN                                                     THRM 94 */
    ff[5] = fo * (1. - ao * sin(*lst * 15. / radeg) * cos(*lat / radeg));
    pfhe = *tinf * 3.3e-16;
/* ---  EXOBASE (ZF) HELIUM PARTIAL PRESSURE                              THRM 97 */
    pfh2 = 2.4e-15;
/* ---  EXOBASE (ZF) H2 PARTIAL PRESSURE                                  THRM 99 */
    if (*tinf <= 330.) {
	pfh = *tinf * 5.2e-16 * exp(-(*tinf) / 70.);
    }
/* ---  EXOBASE (ZF) H PARTIAL PRESSURE                                   THRM101 */
    if (*tinf > 330.) {
	ratio = 1440. / *tinf;
	pfh = sqrt(*tinf) * 5.8e-18 * exp(ratio) / (ratio + 1.);
/* ---    EXOBASE (ZF) H PARTIAL PRESSURE                                 THRM105 */
    }
    xff[0] = pfhe / pressf;
    xff[1] = pfh2 / pressf;
    xff[2] = pfh / pressf;
    *molwtg = 0.;
    *totalprz = 0.;
    *totalmdz = 0.;
    ysc = *zzf * *rf / (*rf + *zzf);
    exps = exp(-ysc / *scale);
    *tz = *tinf - (*tinf - *tf) * exps;
/* ---  Parameters for gradient of molecular weight                       THRM116 */
    smolwtg = 0.;
    stotpr = 0.;
    sysc = (*zzf + 1.) * *rf / (*rf + *zzf + 1.);
    stz = *tinf - (*tinf - *tf) * exp(-sysc / *scale);
/* ---  Temperature gradient, K/km                                        THRM121 */
    *tgrad = (*tinf - *tf) * (*rf + ysc) / (*scale * (*rf + *zzf)) * exps;
    for (i__ = 0; i__ <= 5; ++i__) {
	hh[i__] = bk * *tinf / (gf * dm[i__]) / 1e5;
/* ---    SCALE HEIGHT                                                    THRM125 */
	prz[i__] = pressf * ff[i__] * exp(-ysc / hh[i__] - *scale / hh[i__] * 
		log(*tz / *tf));
	ndz[i__] = p1bar * prz[i__] / (bk * *tz);
/* ---    NUMBER DENSITY HEAVY GASES                                      THRM129 */
	mdz[i__] = ndz[i__] * dm[i__];
	*totalmdz += mdz[i__];
	*totalprz += prz[i__];
	*molwtg += prz[i__] * m[i__];
/* ---    Molecular weight 1 km higher                                    THRM134 */
	sprzi = pressf * ff[i__] * exp(-sysc / hh[i__] - *scale / hh[i__] * 
		log(stz / *tf));
	stotpr += sprzi;
	smolwtg += sprzi * m[i__];
/* L200: */
    }
    for (j = 0; j <= 2; ++j) {
	xhh[j] = bk * *tinf / (gf * xdm[j]) / 1e5;
/* ---    SCALE HEIGHT                                                    THRM142 */
	prz[j + 6] = pressf * xff[j] * exp(-ysc / xhh[j] - *scale / xhh[j] * 
		log(*tz / *tf));
	ndz[j + 6] = p1bar * prz[j + 6] / (bk * *tz);
/* ---    NUMBER DENSITY LIGHT GASES                                      THRM146 */
	mdz[j + 6] = ndz[j + 6] * xdm[j];
	*totalmdz += mdz[j + 6];
	*totalprz += prz[j + 6];
	*molwtg += prz[j + 6] * m[j + 6];
/* ---    Molecular weight 1 km higher                                    THRM151 */
	sprzj = pressf * xff[j] * exp(-sysc / xhh[j] - *scale / xhh[j] * log(
		stz / *tf));
	stotpr += sprzj;
	smolwtg += sprzj * m[j + 6];
/* L210: */
    }
/* ---  Check that temperature >= sublimation temperature                 THRM157 */
    subltchk_m05__(tz, totalprz, &tsubl);
    *molwtg /= *totalprz;
/* ---  Change in molecular weight over 1 km                              THRM160 */
    *dmdz = smolwtg / stotpr - *molwtg;
    *totalndz = p1bar * *totalprz / (bk * *tz);
    if (*flag__ > 0) {
	io___807.ciunit = *iu0;
	s_wsfe(&io___807);
	do_fio(&c__1, (char *)&(*zzf), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*tz), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*molwtg), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*totalprz), (ftnlen)sizeof(doublereal));
	do_fio(&c__1, (char *)&(*totalndz), (ftnlen)sizeof(doublereal));
	for (k = 0; k <= 3; ++k) {
	    do_fio(&c__1, (char *)&ndz[k], (ftnlen)sizeof(doublereal));
	}
	do_fio(&c__1, (char *)&(*totalmdz), (ftnlen)sizeof(doublereal));
	for (k = 4; k <= 8; ++k) {
	    do_fio(&c__1, (char *)&ndz[k], (ftnlen)sizeof(doublereal));
	}
	e_wsfe();
	io___809.ciunit = *iu0;
	s_wsle(&io___809);
	do_lio(&c__9, &c__1, " ", (ftnlen)1);
	e_wsle();
    }
    for (k = 0; k <= 8; ++k) {
	fmol[k] = ndz[k] / *totalndz;
/* L230: */
    }
    return 0;
} /* thermos_m05__ */

/* -----------------------------------------------------------------------THRM174 */
doublereal tidex_m05__(doublereal *a0, doublereal *a1, doublereal *phi1, 
	doublereal *a2, doublereal *phi2, doublereal *t)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double atan(doublereal), cos(doublereal);

    /* Local variables */
    static doublereal pi;

/* ---    Tide value at local solar time t, from mean value A0, amplitude TIDX  2 */
/*       A1 and phase phi1 of 24-hour period component, and amplitude A2 TIDX  3 */
/*       and phase phi2 of 12-hour period component.  Amplitudes A1 and  TIDX  4 */
/*       A2 are in same physical units as mean term A0.  Phases are in   TIDX  5 */
/*       hours of local solar time.                                      TIDX  6 */
    pi = atan(1.) * 4.;
    ret_val = *a0 + *a1 * cos(pi * (*t - *phi1) / 12.) + *a2 * cos(pi * (*t - 
	    *phi2) / 6.);
    return ret_val;
} /* tidex_m05__ */

/* -----------------------------------------------------------------------TIDX 13 */
doublereal tidey_m05__(doublereal *a0, doublereal *a1, doublereal *phi1, 
	doublereal *a2, doublereal *phi2, doublereal *t)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    double atan(doublereal), cos(doublereal);

    /* Local variables */
    static doublereal pi;

/* ---    Tide value at local solar time t, from mean value A0, amplitude TIDY  2 */
/*       A1 and phase phi1 of 24-hour period component, and amplitude A2 TIDY  3 */
/*       and phase phi2 of 12-hour period component.  Amplitudes A1 and  TIDY  4 */
/*       A2 are in relative units (% of mean term A0).  Phases are in    TIDY  5 */
/*       hours of local solar time.                                      TIDY  6 */
    pi = atan(1.) * 4.;
    ret_val = *a0 * ((*a1 * cos(pi * (*t - *phi1) / 12.) + *a2 * cos(pi * (*t 
	    - *phi2) / 6.)) / 100. + 1.);
    if (ret_val <= *a0 * .1) {
	ret_val = *a0 * .1;
    }
    return ret_val;
} /* tidey_m05__ */

/* -----------------------------------------------------------------------TIDY 13 */
/* Subroutine */ int topoareo_m05__(doublereal *clat, doublereal *clonwin, 
	doublereal *careoid, doublereal *ctopohgt, doublereal *calbedo, 
	integer *iau)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    integer i_dnnt(doublereal *);

    /* Local variables */
    static doublereal stepmola;
    static integer n1lat, n2lat, n3lat, n4lat;
    static doublereal clonw;
    extern integer ifloor_m05__(doublereal *);
    static doublereal areoid[4]	/* was [2][2] */, albint[4]	/* was [2][2] 
	    */, dlatmt;
    static integer ilatmt;
    static doublereal dlonmt;
    static integer jlonmt;
    static doublereal stepalblat, stepalblon, dlatalb;
    static integer ilatalb;
    static doublereal dlonalb;
    static integer jlonalb;
    static doublereal stepalb;
    extern /* Subroutine */ int twod_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *);
    static doublereal topohgt[4]	/* was [2][2] */, stepmolalat, 
	    stepmolalon;

/* ---    Convert back to IAU 1991 rotation coordinates, if necessary     TOPO 11 */
    clonw = *clonwin;
    if (*iau == 2000) {
	clonw += .238;
	if (clonw > 360.) {
	    clonw += -360.;
	}
    }
/* ---    Latitude, Longitude steps sizes for MOLA data                   TOPO 17 */
    stepmolalat = .5;
    stepmolalon = .5;
    d__1 = 1000 / stepmolalat;
    n1lat = i_dnnt(&d__1);
    n2lat = n1lat * 90 + 500;
/* ---    Compute MOLA latitude index and lat increment from gridpoint    TOPO 22 */
    stepmola = stepmolalat;
    if (*clat < stepmolalat / 2. - 90.) {
	stepmola = stepmolalat / 2.;
	ilatmt = 0;
	dlatmt = (*clat + 90.) / stepmola;
    } else if (*clat > 90. - stepmolalat / 2.) {
	stepmola = stepmolalat / 2.;
	ilatmt = 360;
	dlatmt = (*clat - 90. + stepmola) / stepmola;
    } else {
	d__1 = *clat * n1lat;
	ilatmt = (ifloor_m05__(&d__1) + n2lat) / 1000;
	dlatmt = (*clat + 90.) / stepmolalat - (ilatmt - .5);
    }
    if (ilatmt > 360) {
	ilatmt = 360;
    }
    if (ilatmt < 0) {
	ilatmt = 0;
    }
/* ---    Compute MOLA longitude index and lon increment from gridpoint   TOPO 38 */
    d__1 = (clonw + stepmolalon / 2.) / stepmolalon;
    jlonmt = ifloor_m05__(&d__1);
    if (jlonmt > 720) {
	jlonmt = 720;
    }
    if (jlonmt < 0) {
	jlonmt = 0;
    }
    dlonmt = (clonw + stepmolalon / 2. - stepmolalon * jlonmt) / stepmolalon;
/* ---    Topographic heights at corners of 2-D square grid points        TOPO 43 */
    topohgt[0] = terhgt_m05__1.topomola[ilatmt + jlonmt * 362];
    topohgt[2] = terhgt_m05__1.topomola[ilatmt + (jlonmt + 1) * 362];
    topohgt[1] = terhgt_m05__1.topomola[ilatmt + 1 + jlonmt * 362];
    topohgt[3] = terhgt_m05__1.topomola[ilatmt + 1 + (jlonmt + 1) * 362];
/* ---    Areoid radius at corners of 2-D square grid                     TOPO 48 */
    areoid[0] = terhgt_m05__1.areorad[ilatmt + jlonmt * 362];
    areoid[2] = terhgt_m05__1.areorad[ilatmt + (jlonmt + 1) * 362];
    areoid[1] = terhgt_m05__1.areorad[ilatmt + 1 + jlonmt * 362];
    areoid[3] = terhgt_m05__1.areorad[ilatmt + 1 + (jlonmt + 1) * 362];
/* ---    Use 2-D interpolation to get topographic height at current      TOPO 53 */
/*       position                                                        TOPO 54 */
    twod_m05__(&dlatmt, &dlonmt, topohgt, ctopohgt);
/* ---    Use 2-D interpolation to get areoid radius at current position  TOPO 56 */
    twod_m05__(&dlatmt, &dlonmt, areoid, careoid);
/* ---    Latitude, Longitude steps sizes for albedo data                 TOPO 58 */
    stepalblat = 1.;
    stepalblon = 1.;
    d__1 = 1000 / stepalblat;
    n3lat = i_dnnt(&d__1);
    n4lat = n3lat * 90 + 500;
/* ---    Compute albedo latitude index and lat increment from gridpoint  TOPO 63 */
    stepalb = stepalblat;
    if (*clat < stepalblat / 2. - 90.) {
	stepalb = stepalblat / 2.;
	ilatalb = 0;
	dlatalb = (*clat + 90.) / stepalb;
    } else if (*clat > 90. - stepalblat / 2.) {
	stepalb = stepalblat / 2.;
	ilatalb = 180;
	dlatalb = (*clat - 90. + stepalb) / stepalb;
    } else {
	d__1 = *clat * n3lat;
	ilatalb = (ifloor_m05__(&d__1) + n4lat) / 1000;
	dlatalb = (*clat + 90.) / stepalblat - (ilatalb - .5);
    }
    if (ilatalb > 180) {
	ilatalb = 180;
    }
    if (ilatalb < 0) {
	ilatalb = 0;
    }
/* ---    Compute albedo longitude index and lon increment from gridpoint TOPO 79 */
    d__1 = (clonw + stepalblon / 2.) / stepalblon;
    jlonalb = ifloor_m05__(&d__1);
    if (jlonalb > 360) {
	jlonalb = 360;
    }
    if (jlonalb < 0) {
	jlonalb = 0;
    }
    dlonalb = (clonw + stepalblon / 2. - stepalblon * jlonalb) / stepalblon;
/* ---    Albedo at corners of 2-D square grid points                     TOPO 84 */
    albint[0] = terhgt_m05__1.albedo[ilatalb + jlonalb * 182];
    albint[2] = terhgt_m05__1.albedo[ilatalb + (jlonalb + 1) * 182];
    albint[1] = terhgt_m05__1.albedo[ilatalb + 1 + jlonalb * 182];
    albint[3] = terhgt_m05__1.albedo[ilatalb + 1 + (jlonalb + 1) * 182];
/* ---    Use 2-D interpolation to get albedo at current position         TOPO 89 */
    twod_m05__(&dlatalb, &dlonalb, albint, calbedo);
    return 0;
} /* topoareo_m05__ */

/* -----------------------------------------------------------------------TOPO 93 */
/* Subroutine */ int thermpar_m05__(doublereal *rau, doublereal *fbarr, 
	doublereal *lat, doublereal *lst, doublereal *sunlat, doublereal *
	tinf0, doublereal *tf0, doublereal *zf0, doublereal *scale)
{
    /* Initialized data */

    static doublereal latmax = 25.4;

    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double atan(doublereal), cos(doublereal);

    /* Local variables */
    static doublereal cphi, tbar, zbar, tavg, zavg, a1, a2, t1, t2, factlat, 
	    poleamp, pi180;

/*                                                                       TPAR  6 */
/* --------------------------------------------------------------------   TPAR  7 */
/*                                                                       TPAR  8 */
/* ---  Thermospheric parameters, revised from the original Stewart       TPAR  9 */
/*     parameterizations:                                                TPAR 10 */
/*     SMA = 1.523691                                                    TPAR 11 */
/*     ZF0 = 124.4 * (SMA/RAU)                                           TPAR 12 */
/*     TINF0 = 4.11 * (11.0 + FBARR)                                     TPAR 13 */
/*     TF0 = 170.0 * (SMA/RAU)                                           TPAR 14 */
/*     SCALE = TF0 / 9.20                                                TPAR 15 */
/*                                                                       TPAR 16 */
/*     The new parameterizations are based on four data sets from the    TPAR 17 */
/*     University of Michigan Mars Thermospheric General Circulation     TPAR 18 */
/*     Model (MTGCM), cases MGS97L, MGS98L, MANC00, and MGS97E. For      TPAR 19 */
/*     a description of the MTGCM model and its output, see Bougher,     TPAR 20 */
/*     et al., Journal of Geophysical Research, vol. 95 (B9), pp.        TPAR 21 */
/*     14,811 - 14,827, August 30, 1990.                                 TPAR 22 */
/*                                                                       TPAR 23 */
/* -------------------------------------------------------------------    TPAR 24 */
/*                                                                       TPAR 25 */
/*     Inputs:                                                           TPAR 26 */
/*       RAU    = orbital position radius (AU)                           TPAR 27 */
/*       FBARR  = 10.7 cm solar flux at Mars position                    TPAR 28 */
/*       lat    = latitude for evaluation of parameters (degrees)        TPAR 29 */
/*       LST    = local solar time (Mars hours) at evaluation point      TPAR 30 */
/*       sunlat = latitude of sun (degrees)                              TPAR 31 */
/*     Outputs:                                                          TPAR 32 */
/*       TINF0  = Exospheric temperature (K)                             TPAR 33 */
/*       TF0    = Temperature at base of thermosphere (K)                TPAR 34 */
/*       ZF0    = Height of base of thermosphere (km)                    TPAR 35 */
/*       SCALE  = Scale height for temperature variations (km)           TPAR 36 */
/*                                                                       TPAR 37 */
/*     Output values are un-corrected for Stewart (ES array) variations, TPAR 38 */
/*     pressure and dust effects.  These factors are accounted for in    TPAR 39 */
/*     the STEWART2_M05 subroutine.  Adjustment factor deltaTEX is added TPAR 40 */
/*     after computation of exospheric temperature.                      TPAR 41 */
/* --------------------------------------------------------------------   TPAR 42 */
/*                                                                       TPAR 43 */
/* ---  Degrees to radians conversion factor                              TPAR 44 */
    pi180 = atan(1.) / 45.;
/* ---  Global mean exospheric temperature (K) versus 10.7 cm flux        TPAR 46 */
    tbar = *fbarr * .9427 + 156.3;
/* ---  Zonal average exospheric temperature (K) versus latitude          TPAR 48 */
    tavg = tbar * (*sunlat * 1.369e-4 * *lat + 1.);
/* ---  Phase angles (hours) for local solar time variation               TPAR 50 */
    t1 = 13.2 - *sunlat * .00119 * *lat;
    t2 = 9.4 - *sunlat * .00231 * *lat;
/* ---  Amplitude factor for local solar time variation                   TPAR 53 */
    poleamp = 1.;
    if (abs(*lat) > 85.) {
	poleamp = 1. - (abs(*lat) - 85.) / 5.;
    }
    cphi = cos(pi180 * (*lat + *sunlat) / (latmax / 90. + 1.)) * poleamp;
/* ---  Exospheric temperature (K) versus local solar time                TPAR 57 */
    *tinf0 = tavg * (cphi * .22 * cos(pi180 * 15. * (*lst - t1)) + 1. + cphi *
	     .04 * cos(pi180 * 30. * (*lst - t2)));
/* ---  Global mean height of thermosphere base (km)                      TPAR 60 */
    zbar = 197.94 - *rau * 49.058;
/* ---  Latitude variation factor                                         TPAR 62 */
/* Computing 3rd power */
    d__1 = *lat / 77.5;
    factlat = *sunlat / latmax * (d__1 * (d__1 * d__1));
/* ---  Zonal average base height (km) versus latitude                    TPAR 64 */
    zavg = zbar + factlat * 4.3;
/* ---  Amplitudes for local solar time variation                         TPAR 66 */
    a1 = (1.5 - cos(pi180 * 4. * *lat)) * poleamp;
/* Computing 3rd power */
    d__1 = cos(pi180 * (*lat + *sunlat * .5));
    a2 = d__1 * (d__1 * d__1) * 2.3 * poleamp;
/* ---  Phase angles (hours) for local solar time variation               TPAR 69 */
    t1 = 16.2 - *sunlat / latmax * atan(pi180 * 10. * *lat);
    t2 = 11.5;
/* ---  Base height of thermosphere (km) versus local solar time          TPAR 72 */
    *zf0 = zavg + a1 * cos(pi180 * 15. * (*lst - t1)) + a2 * cos(pi180 * 30. *
	     (*lst - t2));
/* ---  Global mean temperature (K) at thermosphere base, versus FBARR    TPAR 75 */
    tbar = *fbarr * .5791 + 113.7;
/* ---  Zonal average temperature at thermosphere base (K) vs. latitude   TPAR 77 */
    tavg = tbar * (factlat * .186 + 1.);
/* ---  Amplitudes for local solar time variation                         TPAR 79 */
    a1 = (.06 - cos(pi180 * 4. * *lat) * .05) * poleamp;
/* Computing 3rd power */
    d__1 = cos(pi180 * (*lat + *sunlat * .5));
    a2 = d__1 * (d__1 * d__1) * .1 * poleamp;
/* ---  Phase angles (hours) for local solar time variation               TPAR 82 */
    t1 = 17.5 - *sunlat / latmax * 2.5 * atan(pi180 * 10. * *lat);
/* Computing 2nd power */
    d__1 = *lat / 77.5;
    t2 = d__1 * d__1 * 2. + 10.;
/* ---  Thermosphere base temperature (K) versus local solar time         TPAR 85 */
    *tf0 = tavg * (a1 * cos(pi180 * 15. * (*lst - t1)) + 1. + a2 * cos(pi180 *
	     30. * (*lst - t2)));
/* ---  Global mean scale height (km) of thermospheric temperature        TPAR 88 */
    *scale = *fbarr * .09725 + 8.38;
/* ---  Zonal average temperature scale height (km) vs. latitude          TPAR 90 */
    *scale *= 1.14 - cos(pi180 * *lat) * .18;
    return 0;
} /* thermpar_m05__ */

/* -----------------------------------------------------------------------TPAR 94 */
/* Subroutine */ int tgcmterp_m05__(integer *khgtt, doublereal *time, 
	doublereal *ttgcm, doublereal *ptgcm, doublereal *dtgcm, doublereal *
	utgcm, doublereal *vtgcm, doublereal *zf, doublereal *tempday, 
	doublereal *presday, doublereal *densday, doublereal *uwndday, 
	doublereal *vwndday, doublereal *tempmax, doublereal *tempmin, 
	doublereal *densmax, doublereal *densmin, integer *idaydata)
{
    static doublereal dmin__[16]	/* was [2][2][2][2] */, dmax__[16]	
	    /* was [2][2][2][2] */, pday[16]	/* was [2][2][2][2] */, tday[
	    16]	/* was [2][2][2][2] */, uday[16]	/* was [2][2][2][2] */
	    , vday[16]	/* was [2][2][2][2] */, tmin[16]	/* was [2][2][
	    2][2] */, tmax[16]	/* was [2][2][2][2] */;
    extern doublereal tidex_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *), tidey_m05__(doublereal 
	    *, doublereal *, doublereal *, doublereal *, doublereal *, 
	    doublereal *);
    extern /* Subroutine */ int fourd_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *, integer *)
	    ;
    static integer i__, l, m, n;
    static doublereal dtime;
    static integer itime;
    static doublereal rtgcm, a1, a2, xtime, ttime, d0, p0, p1, r0[16]	/* 
	    was [2][2][2][2] */, p2, t0, u0, v0, z0, dt[16]	/* was [2][2][
	    2][2] */, pt[16]	/* was [2][2][2][2] */, tt[16]	/* was [2][2][
	    2][2] */, ut[16]	/* was [2][2][2][2] */, vt[16]	/* was [2][2][
	    2][2] */, zt[16]	/* was [2][2][2][2] */, a1p, a2p, p1p, p2p, 
	    polefac;

/* ---    Interpolates University of Michigan Mars Thermospheric General  TTRP  4 */
/*       Circulation Model (MTGCM) data to a given latitude, time of     TTRP  5 */
/*       year (Ls), and dust optical depth, for a given height index     TTRP  6 */
/*       (khgtt) and time of day (time).                                 TTRP  7 */
/*       Some input data is provided by the Common "Interp".             TTRP  8 */
/* ---    Set parameter values for number of heights (nhgtt), number      TTRP  9 */
/*       of latitudes (nlatt), and number of dust optical depth values   TTRP 10 */
/* ---    MTGCM 80-170 km data arrays for interpolation                   TTRP 16 */
/* ---    Establish MTGCM values at corners of a 4-dimensional cube in    TTRP 42 */
/*       latitude-Ls-dust-F107 space, at the given height index (khgtt), TTRP 43 */
/*       and time of day (time)                                          TTRP 44 */
    for (i__ = 1; i__ <= 2; ++i__) {
	polefac = 1.;
	if (interp_m05__1.ilatt == 1) {
	    if (i__ == 1) {
		polefac = interp_m05__1.tpolefac;
	    }
	} else if (interp_m05__1.ilatt == 35) {
	    if (i__ == 2) {
		polefac = interp_m05__1.tpolefac;
	    }
	}
	for (l = 1; l <= 2; ++l) {
	    for (m = 1; m <= 2; ++m) {
		for (n = 1; n <= 2; ++n) {
/* ---      Daily mean temperature                                        TTRP 55 */
		    t0 = tgcmdata_m05__1.tta0[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    tday[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = t0;
/* ---      Temperature tide amplitudes and phases                        TTRP 58 */
		    a1 = tgcmdata_m05__1.tta1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p1 = tgcmdata_m05__1.ttp1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    a2 = tgcmdata_m05__1.tta2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p2 = tgcmdata_m05__1.ttp2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
/* ---      Temperature at corners of 3-D cube                            TTRP 63 */
		    tt[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 
			    tidex_m05__(&t0, &a1, &p1, &a2, &p2, time);
/* ---      Max and Min temperatures at corners of 3-D cube               TTRP 65 */
		    tmax[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = -9999.;
		    tmin[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 9999.;
		    if (*idaydata > 0) {
			for (itime = 0; itime <= 23; ++itime) {
			    xtime = (real) itime;
			    ttime = tidex_m05__(&t0, &a1, &p1, &a2, &p2, &
				    xtime);
			    if (ttime > tmax[i__ + (l + (m + (n << 1) << 1) <<
				     1) - 15]) {
				tmax[i__ + (l + (m + (n << 1) << 1) << 1) - 
					15] = ttime;
			    }
			    if (ttime < tmin[i__ + (l + (m + (n << 1) << 1) <<
				     1) - 15]) {
				tmin[i__ + (l + (m + (n << 1) << 1) << 1) - 
					15] = ttime;
			    }
/* L50: */
			}
		    }
/* ---      Daily mean pressure                                           TTRP 76 */
		    p0 = tgcmdata_m05__1.pta0[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    pday[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = p0;
/* ---      Pressure tide amplitudes and phases                           TTRP 79 */
		    a1p = tgcmdata_m05__1.pta1[*khgtt + (interp_m05__1.ilatt 
			    + i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p1p = tgcmdata_m05__1.ptp1[*khgtt + (interp_m05__1.ilatt 
			    + i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    a2p = tgcmdata_m05__1.pta2[*khgtt + (interp_m05__1.ilatt 
			    + i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p2p = tgcmdata_m05__1.ptp2[*khgtt + (interp_m05__1.ilatt 
			    + i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
/* ---      Pressure at corners of 3-D cube                               TTRP 84 */
		    pt[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 
			    tidey_m05__(&p0, &a1p, &p1p, &a2p, &p2p, time);
/* ---      Daily mean density                                            TTRP 86 */
		    d0 = tgcmdata_m05__1.dta0[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
/* ---      Density tide amplitudes and phases                            TTRP 88 */
		    a1 = tgcmdata_m05__1.dta1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p1 = tgcmdata_m05__1.dtp1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    a2 = tgcmdata_m05__1.dta2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p2 = tgcmdata_m05__1.dtp2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
/* ---      Density at corners of 3-D cube                                TTRP 93 */
		    dt[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 
			    tidey_m05__(&d0, &a1, &p1, &a2, &p2, time);
/* ---      Max and Min densities at corners of 3-D cube                  TTRP 95 */
		    dmax__[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 
			    -9999.;
		    dmin__[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 9999.;
		    if (*idaydata > 0) {
			for (itime = 0; itime <= 23; ++itime) {
			    xtime = (real) itime;
			    dtime = tidey_m05__(&d0, &a1, &p1, &a2, &p2, &
				    xtime);
			    if (dtime > dmax__[i__ + (l + (m + (n << 1) << 1) 
				    << 1) - 15]) {
				dmax__[i__ + (l + (m + (n << 1) << 1) << 1) - 
					15] = dtime;
			    }
			    if (dtime < dmin__[i__ + (l + (m + (n << 1) << 1) 
				    << 1) - 15]) {
				dmin__[i__ + (l + (m + (n << 1) << 1) << 1) - 
					15] = dtime;
			    }
/* L60: */
			}
		    }
/* ---      Gas constant from pressure, density, and temperature          TTRP106 */
		    r0[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 190.;
		    if (t0 != 0. && d0 != 0.) {
			r0[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = p0 / (
				t0 * d0);
		    }
/* ---      Daily mean EW wind                                            TTRP109 */
		    u0 = tgcmdata_m05__1.uta0[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    uday[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = u0;
/* ---      EW wind tide amplitudes and phases                            TTRP112 */
		    a1 = tgcmdata_m05__1.uta1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p1 = tgcmdata_m05__1.utp1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    a2 = tgcmdata_m05__1.uta2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p2 = tgcmdata_m05__1.utp2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
/* ---      EW wind at corners of 3-D cube                                TTRP117 */
		    ut[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 
			    tidex_m05__(&u0, &a1, &p1, &a2, &p2, time);
/* ---      Daily mean NS wind                                            TTRP119 */
		    v0 = tgcmdata_m05__1.vta0[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    vday[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = v0;
/* ---      NS wind tide amplitudes and phases                            TTRP122 */
		    a1 = tgcmdata_m05__1.vta1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p1 = tgcmdata_m05__1.vtp1[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
		    a2 = tgcmdata_m05__1.vta2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588] * polefac;
		    p2 = tgcmdata_m05__1.vtp2[*khgtt + (interp_m05__1.ilatt + 
			    i__ - 1 + (interp_m05__1.ls + l - 1 + (
			    interp_m05__1.mdust + m - 1 + (interp_m05__1.mf10 
			    + n - 1) * 3) * 13) * 36) * 19 - 35588];
/* ---      NS wind at corners of 3-D cube                                TTRP127 */
		    vt[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 
			    tidex_m05__(&v0, &a1, &p1, &a2, &p2, time);
/* ---      Tide amplitudes and phases for ZF=height of 1.26 nbar level   TTRP129 */
		    z0 = tgcmdata_m05__1.zfa0[interp_m05__1.ilatt + i__ - 1 + 
			    (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			    + m - 1 + (interp_m05__1.mf10 + n - 1) * 3) * 13) 
			    * 36 - 1873];
		    a1 = tgcmdata_m05__1.zfa1[interp_m05__1.ilatt + i__ - 1 + 
			    (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			    + m - 1 + (interp_m05__1.mf10 + n - 1) * 3) * 13) 
			    * 36 - 1873] * polefac;
		    p1 = tgcmdata_m05__1.zfp1[interp_m05__1.ilatt + i__ - 1 + 
			    (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			    + m - 1 + (interp_m05__1.mf10 + n - 1) * 3) * 13) 
			    * 36 - 1873];
		    a2 = tgcmdata_m05__1.zfa2[interp_m05__1.ilatt + i__ - 1 + 
			    (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			    + m - 1 + (interp_m05__1.mf10 + n - 1) * 3) * 13) 
			    * 36 - 1873] * polefac;
		    p2 = tgcmdata_m05__1.zfp2[interp_m05__1.ilatt + i__ - 1 + 
			    (interp_m05__1.ls + l - 1 + (interp_m05__1.mdust 
			    + m - 1 + (interp_m05__1.mf10 + n - 1) * 3) * 13) 
			    * 36 - 1873];
/* ---      ZF values at corners of 3-D cube                              TTRP135 */
		    zt[i__ + (l + (m + (n << 1) << 1) << 1) - 15] = 
			    tidex_m05__(&z0, &a1, &p1, &a2, &p2, time);
/* L99: */
		}
/* L100: */
	    }
/* L101: */
	}
/* L102: */
    }
/* ---    Use 4-D interpolation to get temperature, pressure, density,    TTRP141 */
/*       gas constant, EW wind, NS wind, and ZF height at given lati-    TTRP142 */
/*       tude, Ls, dust optical depth, and solar activity                TTRP143 */
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, tt, ttgcm, &c__0);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, tday, tempday, &c__0);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, tmax, tempmax, &c__0);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, tmin, tempmin, &c__0);
    if (*idaydata == 1) {
	fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
		interp_m05__1.ddust, &interp_m05__1.df10, dmax__, densmax, &
		c__1);
    } else {
	fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
		interp_m05__1.ddust, &interp_m05__1.df10, dmax__, densmax, &
		c__0);
    }
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, dmin__, densmin, &c__1);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, pt, ptgcm, &c__1);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, pday, presday, &c__1);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, dt, dtgcm, &c__1);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, r0, &rtgcm, &c__0);
/* ---    Daily density from gas constant                                 TTRP154 */
    *densday = *presday / (rtgcm * *tempday);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, ut, utgcm, &c__0);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, uday, uwndday, &c__0);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, vt, vtgcm, &c__0);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, vday, vwndday, &c__0);
    fourd_m05__(&interp_m05__1.dlatt, &interp_m05__1.dls, &
	    interp_m05__1.ddust, &interp_m05__1.df10, zt, zf, &c__0);
    return 0;
} /* tgcmterp_m05__ */

/* -----------------------------------------------------------------------TTRP163 */
/* Subroutine */ int twod_m05__(doublereal *dx, doublereal *dy, doublereal *
	array, doublereal *value)
{
    static doublereal dxp, dyp;

/* ---    2-Dimensional linear interpolation within a 1x1 cube (x,y) of   TWOD  2 */
/*       Array(2,2) to position dx,dy (both 0-1).                        TWOD  3 */
/*       Value is value of interpolated output.                          TWOD  4 */
/* ---    Complementary displacements in x,y,z                            TWOD  7 */
    /* Parameter adjustments */
    array -= 3;

    /* Function Body */
    dxp = 1. - *dx;
    dyp = 1. - *dy;
/* ---    Do 2-D linear interpolation to get Value from Array             TWOD 10 */
    *value = dxp * dyp * array[3] + dxp * *dy * array[5] + *dx * dyp * array[
	    4] + *dx * *dy * array[6];
    return 0;
} /* twod_m05__ */

/* -----------------------------------------------------------------------TWOD 15 */
/* Subroutine */ int wavelon_m05__(integer *lonew, doublereal *wlon, 
	doublereal *clat, doublereal *height, doublereal *day, doublereal *
	wavepert)
{
    /* System generated locals */
    doublereal d__1;

    /* Builtin functions */
    double exp(doublereal), atan(doublereal), cos(doublereal);

    /* Local variables */
    static doublereal polefact, scale, dd, heightfact, dphi1dt, dphi2dt, 
	    dphi3dt, pi180, phi1, phi2, phi3;

/* ---  Inputs                                                            WAVE  2 */
/*       LonEW:  0 if West Longitude phases, 1 if East Longitude phases  WAVE  3 */
/*       Wlon:   Current West Longitude (degrees)                        WAVE  4 */
/*       CLat:   Current Latitude (degrees)                              WAVE  5 */
/*       Height: Current altitude (km)                                   WAVE  6 */
/*     Output:                                                           WAVE  7 */
/*       wavepert: relative perturbation due to wave model               WAVE  8 */
/*                                                                       WAVE  9 */
/* ---  Scale for height variation of wave perturbations below 100 km     WAVE 14 */
    scale = wavecoef_m05__1.wscale;
    heightfact = 1.;
/* ---  Assume exponential variation with height for waves below 100 km   WAVE 17 */
    if (*height < 100.) {
	heightfact = exp((*height - 100.) / scale);
    }
    pi180 = atan(1.) / 45.;
    dd = *day - wavecoef_m05__1.wavedate;
    phi1 = wavecoef_m05__1.wavephi1;
    phi2 = wavecoef_m05__1.wavephi2;
    phi3 = wavecoef_m05__1.wavephi3;
    dphi1dt = wavecoef_m05__1.phi1dot;
    dphi2dt = wavecoef_m05__1.phi2dot;
    dphi3dt = wavecoef_m05__1.phi3dot;
/* ---  Convert phases to West longitude if LonEW = 1                     WAVE 27 */
    if (*lonew == 1) {
	phi1 = 360. - phi1;
	phi2 = 360. - phi2;
	phi3 = 360. - phi3;
	dphi1dt = -wavecoef_m05__1.phi1dot;
	dphi2dt = -wavecoef_m05__1.phi2dot;
	dphi3dt = -wavecoef_m05__1.phi3dot;
    }
    if (wavecoef_m05__1.wavedate <= 0.) {
	dd = 0.;
	dphi1dt = 0.;
	dphi2dt = 0.;
	dphi3dt = 0.;
    }
/* ---  Relative perturbation factor due to wave model                    WAVE 42 */
    *wavepert = (wavecoef_m05__1.wavea0 + wavecoef_m05__1.wavea1 * cos(pi180 *
	     (*wlon - phi1 - dphi1dt * dd)) + wavecoef_m05__1.wavea2 * cos(
	    pi180 * 2. * (*wlon - phi2 - dphi2dt * dd)) + 
	    wavecoef_m05__1.wavea3 * cos(pi180 * 3. * (*wlon - phi3 - dphi3dt 
	    * dd)) - 1.) * heightfact;
/* ---  Insure wave perturbation goes to (WaveA0-1.)*Heightfact at poles  WAVE 47 */
    if (abs(*clat) >= 85.) {
/* Computing 2nd power */
	d__1 = cos(pi180 * 18. * (abs(*clat) - 85.));
	polefact = d__1 * d__1;
	*wavepert = (*wavepert - (wavecoef_m05__1.wavea0 - 1.) * heightfact) *
		 polefact + (wavecoef_m05__1.wavea0 - 1.) * heightfact;
    }
/* ---  Insure wave perturbation within proper limits                     WAVE 53 */
    if (*wavepert < -.9) {
	*wavepert = -.9;
    }
    if (*wavepert > 9.) {
	*wavepert = 9.;
    }
    return 0;
} /* wavelon_m05__ */

/* -----------------------------------------------------------------------WAVE 58 */
doublereal zlogr_m05__(doublereal *znumer, doublereal *zdenom, char *label, 
	ftnlen label_len)
{
    /* System generated locals */
    doublereal ret_val;

    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();
    double log(doublereal);

    /* Fortran I/O blocks */
    static cilist io___899 = { 0, 6, 0, 0, 0 };


/* ---    Log of znumer/zdenom with error message for znumer, zdenom <= 0 ZLGR  2 */
    if (*znumer <= 0. || *zdenom <= 0.) {
	ret_val = 1.;
	s_wsle(&io___899);
	do_lio(&c__9, &c__1, " Log error at ", (ftnlen)14);
	do_lio(&c__9, &c__1, label, (ftnlen)7);
	do_lio(&c__5, &c__1, (char *)&(*znumer), (ftnlen)sizeof(doublereal));
	do_lio(&c__5, &c__1, (char *)&(*zdenom), (ftnlen)sizeof(doublereal));
	e_wsle();
    } else {
	ret_val = log(*znumer / *zdenom);
    }
    return ret_val;
} /* zlogr_m05__ */

/* -----------------------------------------------------------------------ZLGR 13 */
/* Subroutine */ int readtes_m05__(char *datadir, char *version, ftnlen 
	datadir_len, ftnlen version_len)
{
    /* System generated locals */
    address a__1[4];
    integer i__1[4];
    char ch__1[72];
    olist o__1;
    cllist cl__1;

    /* Builtin functions */
    integer i_indx(char *, char *, ftnlen, ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer f_open(olist *), s_rsue(cilist *), do_uio(integer *, char *, 
	    ftnlen), e_rsue(), f_clos(cllist *);

    /* Local variables */
    static integer iunit, lendir;

    /* Fortran I/O blocks */
    static cilist io___902 = { 0, 0, 0, 0, 0 };


/* ---     Set parameter values for number of years, Ls's, lats and lons  RTES  6 */
/* ---     Set parameter for "form=" in binary file Open statement        RTES 11 */
    iunit = 67;
/* ---     Compute character string length of DATADIR path name           RTES 15 */
    lendir = i_indx(datadir, " ", (ftnlen)60, (ftnlen)1) - 1;
    if (lendir < 1 || lendir > 60) {
	lendir = 60;
    }
/* ---     Open and read TES dust optical depths vs year, Ls, lat, lon    RTES 18 */
    o__1.oerr = 0;
    o__1.ounit = iunit;
    o__1.ofnmlen = lendir + 12;
/* Writing concatenation */
    i__1[0] = lendir, a__1[0] = datadir;
    i__1[1] = 7, a__1[1] = "TESdust";
    i__1[2] = 1, a__1[2] = version;
    i__1[3] = 4, a__1[3] = ".bin";
    s_cat(ch__1, a__1, i__1, &c__4, (ftnlen)72);
    o__1.ofnm = ch__1;
    o__1.orl = 0;
    o__1.osta = "old";
    o__1.oacc = 0;
    o__1.ofm = "unformatted";
    o__1.oblnk = 0;
    f_open(&o__1);
    io___902.ciunit = iunit;
    s_rsue(&io___902);
    do_uio(&c_b797, (char *)&tesdust_m05__1.testau[0], (ftnlen)sizeof(
	    doublereal));
    e_rsue();
    cl__1.cerr = 0;
    cl__1.cunit = iunit;
    cl__1.csta = 0;
    f_clos(&cl__1);
    return 0;
} /* readtes_m05__ */

/* ---------------------------------------------------------------------  RTES 25 */
/* Subroutine */ int tesod_m05__(integer *mapyear, doublereal *xls, 
	doublereal *ylat, doublereal *zlon, doublereal *dustod)
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static doublereal dlat, dlon, halfstep;
    static integer jlat1, jlat2, jlon1, jlon2;
    static doublereal ylat1;
    extern /* Subroutine */ int threed_m05__(doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, integer *);
    static doublereal array[8]	/* was [2][2][2] */;
    extern integer ifloor_m05__(doublereal *);
    static doublereal stepls, dls, steplat, steplon;
    static integer ils1, ils2;
    static doublereal xls1;

/* ---     Interpolate TES optical depths vs Ls, lat and lon for given    TESD  2 */
/*        TES mapping year                                               TESD  3 */
/* ---     Set parameter values for number of years, Ls's, lats and lons  TESD  4 */
/* ---     Set latitude, longitude and Ls data step intervals             TESD 12 */
    steplat = 7.5;
    steplon = 9.;
    stepls = 5.;
    halfstep = stepls / 2.;
/* ---     Find lat interpolation index values jlat1, jlat2               TESD 17 */
    jlat1 = (integer) ((*ylat + 90. + steplat) / steplat);
    if (jlat1 >= 25) {
	jlat1 = 24;
    }
    jlat2 = jlat1 + 1;
    ylat1 = -(steplat + 90.) + steplat * jlat1;
/* ---     Increment in lat within interpolation box                      TESD 22 */
    dlat = (*ylat - ylat1) / steplat;
    ils1 = (integer) ((*xls + halfstep) / stepls);
    xls1 = stepls * ils1 - halfstep;
/* ---     Find Ls interpolation index values ils1, ils2                  TESD 26 */
    ils2 = ils1 + 1;
/* ---     Increment in Ls within interpolation box                       TESD 28 */
    dls = (*xls - xls1) / stepls;
/* ---     Adjust Ls index values near Ls = 0 or 360                      TESD 30 */
    if (ils1 == 0 || ils1 == 72) {
	ils1 = 72;
	ils2 = 1;
    }
/* ---     Find lon interpolation index jlon                              TESD 35 */
    d__1 = *zlon / steplon;
    jlon1 = ifloor_m05__(&d__1);
    if (jlon1 > 39) {
	jlon1 = 39;
    }
    jlon2 = jlon1 + 1;
    dlon = (*zlon - steplon * jlon1) / steplon;
/* ---     Fill 3-d array box for interpolation                           TESD 40 */
    array[0] = tesdust_m05__1.testau[*mapyear + (ils1 + (jlat1 + jlon1 * 25) *
	     72 << 1) - 147];
    array[4] = tesdust_m05__1.testau[*mapyear + (ils1 + (jlat1 + jlon2 * 25) *
	     72 << 1) - 147];
    array[2] = tesdust_m05__1.testau[*mapyear + (ils1 + (jlat2 + jlon1 * 25) *
	     72 << 1) - 147];
    array[6] = tesdust_m05__1.testau[*mapyear + (ils1 + (jlat2 + jlon2 * 25) *
	     72 << 1) - 147];
    array[1] = tesdust_m05__1.testau[*mapyear + (ils2 + (jlat1 + jlon1 * 25) *
	     72 << 1) - 147];
    array[5] = tesdust_m05__1.testau[*mapyear + (ils2 + (jlat1 + jlon2 * 25) *
	     72 << 1) - 147];
    array[3] = tesdust_m05__1.testau[*mapyear + (ils2 + (jlat2 + jlon1 * 25) *
	     72 << 1) - 147];
    array[7] = tesdust_m05__1.testau[*mapyear + (ils2 + (jlat2 + jlon2 * 25) *
	     72 << 1) - 147];
/* ---     Do 3-D interpolation on Ls, lat and lon                        TESD 49 */
    threed_m05__(&dls, &dlat, &dlon, array, dustod, &c__0);
    return 0;
} /* tesod_m05__ */

#ifdef __cplusplus
	}
#endif
