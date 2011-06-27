/* setup_MGRAM05.f -- translated by f2c (version 20000704).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#ifdef __cplusplus
extern "C" {
#endif
#include "f2c.h"

/* Common Block Declarations */

struct {
    char trajfl[60], profile[60], wavefile[60], datadir[60];
    integer iert, iutc;
    char gcmdir[60];
    integer month, mday, myear, ihr, imin;
    doublereal sec;
    integer lonew, nr1;
    doublereal flat, flon, fhgt, hgtasfcm, delhgt, dellat, dellon, deltime, 
	    deltatex, profnear, proffar, rpscale, corlmin;
    integer nmonte;
} setup_05__;

#define setup_05__1 setup_05__

struct {
    integer ix, iy, iz;
} randcom_m05__;

#define randcom_m05__1 randcom_m05__

struct {
    doublereal areorad[261364]	/* was [362][722] */, topomola[261364]	/* 
	    was [362][722] */, albedo[65884]	/* was [182][362] */;
} terhgt_m05__;

#define terhgt_m05__1 terhgt_m05__

struct {
    doublereal offsets[39]	/* was [13][3] */, toffsets[26]	/* was [13][2]
	     */, zoffset, hgtoffset, ofszl;
    integer ibougher;
} tgcmoffset_m05__;

#define tgcmoffset_m05__1 tgcmoffset_m05__

struct {
    doublereal zc[164], tc[164], pc[164], dc[164];
} cosparnh_m05__;

#define cosparnh_m05__1 cosparnh_m05__

struct {
    doublereal f107, stdl, fmol[9];
} therm_m05__;

#define therm_m05__1 therm_m05__

struct {
    doublereal dtr, day, dustlat, dustlon, radmax, rref, als0, alsdur, intens,
	     dtex, rpfactor, dusttau, dustmin, dustmax, dustod, dustnu, 
	    dustdiam, dustdens, rwscale, wlscale, requa, rpole, wmscale, 
	    blwinfac;
    integer mapyear, idaydata, npos, nvarx, nvary, logscale, iu0, iup, ipclat,
	     molahgts;
} datacom_m05__;

#define datacom_m05__1 datacom_m05__

struct {
    doublereal dust[3], dzbl[3], zwsfc, f10val[2], f10tes[3];
    char dustc[6], solact[4], tesyr[4], soltes[6];
} mgcmparm_m05__;

#define mgcmparm_m05__1 mgcmparm_m05__

struct {
    doublereal wavea0, wavea1, wavephi1, wavea2, wavephi2, wavea3, wavephi3, 
	    wavetime[100], wavedata[1100]	/* was [100][11] */, wscale, 
	    phi1dot, phi2dot, phi3dot, wavedate;
    integer nwave, iuwave;
} wavecoef_m05__;

#define wavecoef_m05__1 wavecoef_m05__

struct {
    char lstfl[60], outfl[60];
} filename_m05__;

#define filename_m05__1 filename_m05__

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__5 = 5;
static integer c__2 = 2;
static integer c__3 = 3;
static integer c_b166 = 261364;
static integer c__65884 = 65884;

/* Subroutine */ int setup_m05__(doublereal *chgt, doublereal *clat, 
	doublereal *clon, integer *iyear, integer *imonth, integer *iday, 
	integer *ihour, integer *iminute, doublereal *csec, doublereal *day0, 
	doublereal *rhod, doublereal *rhou, doublereal *rhov, doublereal *
	rhow, doublereal *dhgt, doublereal *dlat, doublereal *dlon, 
	doublereal *dtime, integer *maxnum, integer *nrn1, integer *nmcr1, 
	doublereal *dsunls, doublereal *dradau, doublereal *dowlt, integer *
	lnew, char *inputfl, integer *iustdout, integer *iulist, doublereal *
	hgtasfc, integer *inert, integer *inutc, doublereal *stepmin, 
	doublereal *profnr, doublereal *proffr, integer *nprof, integer *
	isload, ftnlen inputfl_len)
{
    /* Initialized data */

    static char files[60*13+1] = "LIST.txt                                  \
                  Density.txt                                               \
  Perturb.txt                                                 Winds.txt     \
                                              TPresHgt.txt                  \
                              DayData.txt                                   \
              ThrmData.txt                                                mo\
latoph.bin                                                COSPAR2.DAT       \
                                          OUTPUT.txt                        \
                          hgtoffst.dat                                      \
          albedo1.bin                                                 MarsRa\
d.txt                                                 ";

    /* Format strings */
    static char fmt_1[] = "(\002 Mars-GRAM 2005 (Version \002,a1,\002.3) - S\
ep, 2009\002)";
    static char fmt_382[] = "(\002 Error in first latitude or longitude.\002)"
	    ;
    static char fmt_11[] = "(\002 Unable to open Trajectory Data file!\002)";
    static char fmt_13[] = "(\002 Profile file= \002,(a))";
    static char fmt_17[] = "(\002 Wave file= \002,(a))";
    static char fmt_14[] = "(\002 LIST file= \002,(a)/\002 OUTPUT file= \002\
,(a))";
    static char fmt_24[] = "(\002 Data directory= \002,(a)/\002 GCM  directo\
ry= \002,(a))";
    static char fmt_16[] = "(\002 Trajectory file= \002,(a))";
    static char fmt_621[] = "(\002    Var_X        DENSLO     DENSAV     DEN\
SHI\002,\002    DENSTOT  DustOD  Radius   Grav RadAU LOGSCALE\002,\002 hgtof\
fset ibougher MapYear profwgt\002)";
    static char fmt_721[] = "(\002    Var_X        Var_Y        DENSLO     D\
ENSAV     \002,\002DENSHI    DENSTOT  DustOD  Radius   Grav RadAU LOGSCAL\
E\002,\002 hgtoffset ibougher MapYear profwgt\002)";
    static char fmt_622[] = "(\002    Var_X      SigD  DensRand  DensWave\
\002,\002     DensP    corlim   SigU   SigW iupdate\002)";
    static char fmt_722[] = "(\002    Var_X        Var_Y      SigD  DensRan\
d\002,\002  DensWave     DensP    corlim   SigU   SigW iupdate\002)";
    static char fmt_623[] = "(\002    Var_X      EWmean  EWpert   EWtot  \
\002,\002NSmean  NSpert   NStot  VWpert iupdate\002)";
    static char fmt_723[] = "(\002    Var_X        Var_Y      EWmean  EWpert\
   \002,\002EWtot  NSmean  NSpert   NStot  VWpert iupdate\002)";
    static char fmt_624[] = "(\002    Var_X       Temp      Pres   TdegC   P\
res_mb\002,\002     Hrho   Hpres MolWt TerHgt Tgrnd  Areoid  dAreoid\002,\
\002 CO2%v  N2%v  Ar%v  O2%v  CO%v   O%v  He%v  H2%v   H%v\002,\002 H2O%v LO\
GSCALE\002)";
    static char fmt_724[] = "(\002    Var_X        Var_Y       Temp      Pre\
s   TdegC\002,\002   Pres_mb     Hrho   Hpres MolWt TerHgt Tgrnd  Areoid\002,\
\002  dAreoid CO2%v  N2%v  Ar%v  O2%v  CO%v   O%v  He%v  H2%v\002,\002   H%v\
 H2O%v LOGSCALE\002)";
    static char fmt_625[] = "(\002    Var_X    TempDay  PresDay    DensDa\
y\002,\002   EWwnDay NSwnDay Tempmin Tempmax   Densmin    Densmax\002,\002  \
LOGSCALE  DensAV\002)";
    static char fmt_725[] = "(\002    Var_X        Var_Y    TempDay  PresDa\
y\002,\002    DensDay   EWwnDay NSwnDay Tempmin Tempmax   Densmin\002,\002  \
  Densmax  LOGSCALE  DensAV\002)";
    static char fmt_626[] = "(\002    Var_X       Tbase   Zbase  F1peak\002\
,\002  MolWgt   Texos  hgtoffset ibougher\002)";
    static char fmt_726[] = "(\002    Var_X        Var_Y       Tbase   Zbas\
e\002,\002  F1peak  MolWgt   Texos  hgtoffset ibougher\002)";
    static char fmt_627[] = "(\002    Var_X       alb      mu0 Dareaden  Dmi\
xrat\002,\002  Dmasden  Dnumden Ice\002)";
    static char fmt_727[] = "(\002    Var_X        Var_Y       alb      mu0 \
Dareaden\002,\002  Dmixrat  Dmasden  Dnumden Ice\002)";
    static char fmt_629[] = "(\002     Time  \002,a7,1x,a5,\002   Lon\002,a1\
,3x,a8,\002   Temp\002,\002  EWind  NWind  sigD  Ls   Dust CO2%m  N2%m  Ar%m\
  O2%m\002,\002  CO%m   O%m  He%m  H2%m   H%m H2O%m\002)";
    static char fmt_60[] = "(\002 File open error! Error =\002,i5,1x,(a))";
    static char fmt_66[] = "(a1)";
    static char fmt_92[] = "(\002 Input error in hour, minute or seconds.\
\002)";
    static char fmt_91[] = "(\002 Input error in month, day or year.\002)";
    static char fmt_283[] = "(\002 Input time is Earth-Receive Time (ERT)\
\002)";
    static char fmt_284[] = "(\002 Input time is Mars-Event Time (MET)\002)";
    static char fmt_285[] = "(\002 Input time is Coordinated Universal Time \
(UTC)\002)";
    static char fmt_286[] = "(\002 Input time is Terrestrial (Dynamical) Tim\
e (TT)\002)";
    static char fmt_290[] = "(\002 Date = \002,i2,\002/\002,i2,\002/\002,i4\
,\002  Julian Day = \002,f13.5,\002  Time = \002,i2,\002:\002,i2,\002:\002,f\
4.1)";
    static char fmt_291[] = "(\002 deltaTEX=\002,f7.1,\002K\002)";
    static char fmt_292[] = "(\002 Input heights are planeto-centric, relati\
ve to MOLA\002,\002 areoid\002)";
    static char fmt_294[] = "(\002 Input heights are planeto-centric, relati\
ve to \002,\002 reference ellipsoid\002)";
    static char fmt_293[] = "(\002 Input heights are planeto-graphic (relati\
ve to \002,\002 reference ellipsoid)\002)";
    static char fmt_297[] = "(\002 Reference ellipsoid radii (km): Equator \
=\002,f8.2,\002 Pole =\002,f8.2)";
    static char fmt_296[] = "(\002 Output heights are planeto-centric, excep\
t as noted.\002/\002 Longitude & ephemeris use IAU 2000 rotational system\
.\002)";
    static char fmt_298[] = "(\002 Error in starting random number.\002)";
    static char fmt_363[] = "(\002 Local scale dust storm, starting at Ls =\
 \002,f5.1,\002 deg.,  Intensity = \002,f3.1/\002  with duration = \002,f5.1,\
\002 degrees of Ls angle.\002/\002 Max. radius = \002,f7.1,\002 km,  At Lat-\
Lon = \002,f6.2,\002 N,  \002,f6.2,\002 W (\002,f6.2,\002 E)\002)";
    static char fmt_366[] = "(\002 Global scale dust storm, starting at Ls\
 = \002,f5.1,\002 deg.,  Intensity = \002,f3.1/\002  with duration = \002,f5\
.1,\002 degrees of Ls angle.\002)";
    static char fmt_368[] = "(\002 F10.7 flux = \002,f5.1,\002 (1 AU)  \002,\
f5.1,\002 (Mars),  standard deviation = \002,f4.1)";
    static char fmt_371[] = "(\002 Dust optical depth from NAMELIST input\
\002)";
    static char fmt_372[] = "(\002 Dust optical depth vs lat and Ls from TES\
 Mapping Year \002,i1,\002 data\002)";
    static char fmt_369[] = "(\002 Dustnu =\002,f7.4,\002   Dustdiam =\002,f\
6.2,\002 E-6 meters   Dustdens =\002,f8.1,\002 kg/m**3\002)";
    static char fmt_370[] = "(\002   Random seed =\002,i6,\002  Dens.Pert.Sc\
ale Factor =\002,f5.2,\002   corlmin =\002,f6.3/\002   Wind.Pert.Scale Facto\
r =\002,f6.2,\002    Wavelength Scale Factor =\002,f6.2/\002   Mean Wind Sca\
le\002,\002 Factor =\002,f6.2,\002    Slope Wind Scale Factor =\002,f6.2)";
    static char fmt_380[] = "(/\002 Select x-code and y-code for plotable ou\
tput versus\002,\002 desired parameter(s):\002//\002 Code              Param\
eter\002/\002 ----   -------------------------------------------------\002\
/\002   1    Height (above MOLA areoid, km)\002/\002   2    Height (above lo\
cal MOLA topographic surface, km)\002/\002   3    Latitude (deg.)\002/\002  \
 4    Longitude (deg.) West if LonEW=0, East if LonEW=1\002/\002   5    Time\
 from start (Earth seconds)\002/\002   6    Time from start (Martian Sols\
)\002/\002   7    Areocentric Longitude of Sun, Ls (deg.)\002/\002   8    Lo\
cal Solar Time (Mars hours = 1/24th sol)\002/\002   9    Pressure (mb)\002\
/\002  10    Pressure Height (km) [-H*log(Pres/PresSurf) = \002,\002-H*log(s\
igma)]\002/\002  11    Sigma coordinate [sigma=Pressure/(Surface Pressure)\
]\002/\002  12    Height (km) above reference ellipsoid\002/\002  13    Plan\
eto-Graphic Height (km) above reference ellipsoid\002/\002  14    Planeto-Gr\
aphic Latitude (deg.)\002/\002  15    Longitude in range -180 to +180 deg. (\
East or West)\002//\002 Use y-code = 0 for plotable output vs x-code variabl\
e only\002)";
    static char fmt_381[] = "(\002 x-code or y-code input error.\002)";

    /* System generated locals */
    address a__1[2];
    integer i__1, i__2[2];
    doublereal d__1, d__2;
    char ch__1[120];
    olist o__1;
    cllist cl__1;
    static integer equiv_12[13];

    /* Builtin functions */
    /* Subroutine */ int s_copy(char *, char *, ftnlen, ftnlen);
    integer f_open(olist *), s_wsle(cilist *), do_lio(integer *, integer *, 
	    char *, ftnlen), e_wsle();
    /* Subroutine */ int s_stop(char *, ftnlen);
    integer s_rsne(cilist *), f_clos(cllist *), s_wsfe(cilist *), e_wsfe(), 
	    s_rsle(cilist *), e_rsle(), s_cmp(char *, char *, ftnlen, ftnlen),
	     do_fio(integer *, char *, ftnlen), i_indx(char *, char *, ftnlen,
	     ftnlen);
    /* Subroutine */ int s_cat(char *, char **, integer *, integer *, ftnlen);
    integer s_rsfe(cilist *), e_rsfe(), s_rsue(cilist *), do_uio(integer *, 
	    char *, ftnlen), e_rsue();
    double atan(doublereal), tan(doublereal);

    /* Local variables */
#define ierr (equiv_12)
    extern /* Subroutine */ int chkdt_m05__(integer *, integer *, integer *, 
	    integer *, integer *, doublereal *, integer *);
    static doublereal owlt;
    extern /* Subroutine */ int rdtesmgcm_m05__(char *, char *, ftnlen, 
	    ftnlen), rdtestgcm_m05__(char *, char *, ftnlen, ftnlen);
#define ierr1 (equiv_12)
#define ierr2 (equiv_12 + 1)
#define ierr3 (equiv_12 + 2)
#define ierr4 (equiv_12 + 3)
#define ierr5 (equiv_12 + 4)
#define ierr6 (equiv_12 + 5)
#define ierr7 (equiv_12 + 6)
#define ierr8 (equiv_12 + 7)
#define ierr9 (equiv_12 + 8)
    static integer i__, j, l;
#define ierr10 (equiv_12 + 9)
#define ierr11 (equiv_12 + 10)
#define ierr12 (equiv_12 + 11)
#define ierr13 (equiv_12 + 12)
    static char ewlon[1], dummy[1];
    static integer ioerr;
    extern /* Subroutine */ int rdprof_m05__(char *, integer *, integer *, 
	    ftnlen);
    static doublereal ttsec;
    extern doublereal random_m05__(integer *);
    static doublereal z1, dt;
    static integer ls;
    static char latlbl[5], hgtlbl[7];
    static doublereal siload;
    static integer lendir;
    extern /* Subroutine */ int readtes_m05__(char *, char *, ftnlen, ftnlen);
    static doublereal marsau;
    static integer idterr;
    static doublereal requat, rpoles, sunlat, sunlon, als, eot;
    extern /* Subroutine */ int readmgcm_m05__(char *, char *, ftnlen, ftnlen)
	    ;
    extern doublereal ppnd_m05__(doublereal *, integer *);
    static char denslbl[8];
    extern /* Subroutine */ int readtgcm_m05__(char *, char *, ftnlen, ftnlen)
	    ;
    static integer inpclat;
    extern /* Subroutine */ int readsurf_m05__(char *, char *, ftnlen, ftnlen)
	    , marsephm_m05__(doublereal *, doublereal *, doublereal *, 
	    doublereal *, doublereal *, doublereal *, doublereal *), 
	    caltojul_m05__(integer *, integer *, integer *, integer *, 
	    integer *, doublereal *, doublereal *), rdtessrf_m05__(char *, 
	    char *, ftnlen, ftnlen);

    /* Namelist stuff */

    static Vardesc wavephi1_dv = { "WAVEPHI1", (char *)&
	    wavecoef_m05__1.wavephi1, (ftnlen *)0, 5 };
    static Vardesc wavephi2_dv = { "WAVEPHI2", (char *)&
	    wavecoef_m05__1.wavephi2, (ftnlen *)0, 5 };
    static Vardesc wavephi3_dv = { "WAVEPHI3", (char *)&
	    wavecoef_m05__1.wavephi3, (ftnlen *)0, 5 };
    static Vardesc flat_dv = { "FLAT", (char *)&setup_05__1.flat, (ftnlen *)0,
	     5 };
    static Vardesc mday_dv = { "MDAY", (char *)&setup_05__1.mday, (ftnlen *)0,
	     3 };
    static Vardesc imin_dv = { "IMIN", (char *)&setup_05__1.imin, (ftnlen *)0,
	     3 };
    static Vardesc flon_dv = { "FLON", (char *)&setup_05__1.flon, (ftnlen *)0,
	     5 };
    static Vardesc fhgt_dv = { "FHGT", (char *)&setup_05__1.fhgt, (ftnlen *)0,
	     5 };
    static Vardesc iert_dv = { "IERT", (char *)&setup_05__1.iert, (ftnlen *)0,
	     3 };
    static Vardesc iutc_dv = { "IUTC", (char *)&setup_05__1.iutc, (ftnlen *)0,
	     3 };
    static Vardesc stdl_dv = { "STDL", (char *)&therm_m05__1.stdl, (ftnlen *)
	    0, 5 };
    static Vardesc blwinfac_dv = { "BLWINFAC", (char *)&
	    datacom_m05__1.blwinfac, (ftnlen *)0, 5 };
    static Vardesc idaydata_dv = { "IDAYDATA", (char *)&
	    datacom_m05__1.idaydata, (ftnlen *)0, 3 };
    static Vardesc hgtasfcm_dv = { "HGTASFCM", (char *)&setup_05__1.hgtasfcm, 
	    (ftnlen *)0, 5 };
    static Vardesc logscale_dv = { "LOGSCALE", (char *)&
	    datacom_m05__1.logscale, (ftnlen *)0, 3 };
    static Vardesc npos_dv = { "NPOS", (char *)&datacom_m05__1.npos, (ftnlen *
	    )0, 3 };
    static Vardesc wavefile_dv = { "WAVEFILE", setup_05__1.wavefile, (ftnlen *
	    )0, -60 };
    static Vardesc ibougher_dv = { "IBOUGHER", (char *)&
	    tgcmoffset_m05__1.ibougher, (ftnlen *)0, 3 };
    static Vardesc wavedate_dv = { "WAVEDATE", (char *)&
	    wavecoef_m05__1.wavedate, (ftnlen *)0, 5 };
    static Vardesc deltatex_dv = { "DELTATEX", (char *)&setup_05__1.deltatex, 
	    (ftnlen *)0, 5 };
    static Vardesc dustdiam_dv = { "DUSTDIAM", (char *)&
	    datacom_m05__1.dustdiam, (ftnlen *)0, 5 };
    static Vardesc profnear_dv = { "PROFNEAR", (char *)&setup_05__1.profnear, 
	    (ftnlen *)0, 5 };
    static Vardesc molahgts_dv = { "MOLAHGTS", (char *)&
	    datacom_m05__1.molahgts, (ftnlen *)0, 3 };
    static Vardesc dustdens_dv = { "DUSTDENS", (char *)&
	    datacom_m05__1.dustdens, (ftnlen *)0, 5 };
    static Vardesc myear_dv = { "MYEAR", (char *)&setup_05__1.myear, (ftnlen *
	    )0, 3 };
    static Vardesc requa_dv = { "REQUA", (char *)&datacom_m05__1.requa, (
	    ftnlen *)0, 5 };
    static Vardesc lstfl_dv = { "LSTFL", filename_m05__1.lstfl, (ftnlen *)0, 
	    -60 };
    static Vardesc month_dv = { "MONTH", (char *)&setup_05__1.month, (ftnlen *
	    )0, 3 };
    static Vardesc outfl_dv = { "OUTFL", filename_m05__1.outfl, (ftnlen *)0, 
	    -60 };
    static Vardesc lonew_dv = { "LONEW", (char *)&setup_05__1.lonew, (ftnlen *
	    )0, 3 };
    static Vardesc rpole_dv = { "RPOLE", (char *)&datacom_m05__1.rpole, (
	    ftnlen *)0, 5 };
    static Vardesc nvarx_dv = { "NVARX", (char *)&datacom_m05__1.nvarx, (
	    ftnlen *)0, 3 };
    static Vardesc nvary_dv = { "NVARY", (char *)&datacom_m05__1.nvary, (
	    ftnlen *)0, 3 };
    static Vardesc wavea0_dv = { "WAVEA0", (char *)&wavecoef_m05__1.wavea0, (
	    ftnlen *)0, 5 };
    static Vardesc wavea1_dv = { "WAVEA1", (char *)&wavecoef_m05__1.wavea1, (
	    ftnlen *)0, 5 };
    static Vardesc wavea2_dv = { "WAVEA2", (char *)&wavecoef_m05__1.wavea2, (
	    ftnlen *)0, 5 };
    static Vardesc wavea3_dv = { "WAVEA3", (char *)&wavecoef_m05__1.wavea3, (
	    ftnlen *)0, 5 };
    static Vardesc gcmdir_dv = { "GCMDIR", setup_05__1.gcmdir, (ftnlen *)0, 
	    -60 };
    static Vardesc dellat_dv = { "DELLAT", (char *)&setup_05__1.dellat, (
	    ftnlen *)0, 5 };
    static Vardesc delhgt_dv = { "DELHGT", (char *)&setup_05__1.delhgt, (
	    ftnlen *)0, 5 };
    static Vardesc dellon_dv = { "DELLON", (char *)&setup_05__1.dellon, (
	    ftnlen *)0, 5 };
    static Vardesc radmax_dv = { "RADMAX", (char *)&datacom_m05__1.radmax, (
	    ftnlen *)0, 5 };
    static Vardesc ipclat_dv = { "IPCLAT", (char *)&datacom_m05__1.ipclat, (
	    ftnlen *)0, 3 };
    static Vardesc wscale_dv = { "WSCALE", (char *)&wavecoef_m05__1.wscale, (
	    ftnlen *)0, 5 };
    static Vardesc trajfl_dv = { "TRAJFL", setup_05__1.trajfl, (ftnlen *)0, 
	    -60 };
    static Vardesc alsdur_dv = { "ALSDUR", (char *)&datacom_m05__1.alsdur, (
	    ftnlen *)0, 5 };
    static Vardesc intens_dv = { "INTENS", (char *)&datacom_m05__1.intens, (
	    ftnlen *)0, 5 };
    static Vardesc nmonte_dv = { "NMONTE", (char *)&setup_05__1.nmonte, (
	    ftnlen *)0, 3 };
    static Vardesc f107_dv = { "F107", (char *)&therm_m05__1.f107, (ftnlen *)
	    0, 5 };
    static Vardesc iuwave_dv = { "IUWAVE", (char *)&wavecoef_m05__1.iuwave, (
	    ftnlen *)0, 3 };
    static Vardesc nr1_dv = { "NR1", (char *)&setup_05__1.nr1, (ftnlen *)0, 3 
	    };
    static Vardesc dustnu_dv = { "DUSTNU", (char *)&datacom_m05__1.dustnu, (
	    ftnlen *)0, 5 };
    static Vardesc phi1dot_dv = { "PHI1DOT", (char *)&wavecoef_m05__1.phi1dot,
	     (ftnlen *)0, 5 };
    static Vardesc phi2dot_dv = { "PHI2DOT", (char *)&wavecoef_m05__1.phi2dot,
	     (ftnlen *)0, 5 };
    static Vardesc phi3dot_dv = { "PHI3DOT", (char *)&wavecoef_m05__1.phi3dot,
	     (ftnlen *)0, 5 };
    static Vardesc sec_dv = { "SEC", (char *)&setup_05__1.sec, (ftnlen *)0, 5 
	    };
    static Vardesc ihr_dv = { "IHR", (char *)&setup_05__1.ihr, (ftnlen *)0, 3 
	    };
    static Vardesc datadir_dv = { "DATADIR", setup_05__1.datadir, (ftnlen *)0,
	     -60 };
    static Vardesc iup_dv = { "IUP", (char *)&datacom_m05__1.iup, (ftnlen *)0,
	     3 };
    static Vardesc deltime_dv = { "DELTIME", (char *)&setup_05__1.deltime, (
	    ftnlen *)0, 5 };
    static Vardesc rpscale_dv = { "RPSCALE", (char *)&setup_05__1.rpscale, (
	    ftnlen *)0, 5 };
    static Vardesc wlscale_dv = { "WLSCALE", (char *)&datacom_m05__1.wlscale, 
	    (ftnlen *)0, 5 };
    static Vardesc wmscale_dv = { "WMSCALE", (char *)&datacom_m05__1.wmscale, 
	    (ftnlen *)0, 5 };
    static Vardesc mapyear_dv = { "MAPYEAR", (char *)&datacom_m05__1.mapyear, 
	    (ftnlen *)0, 3 };
    static Vardesc proffar_dv = { "PROFFAR", (char *)&setup_05__1.proffar, (
	    ftnlen *)0, 5 };
    static Vardesc profile_dv = { "PROFILE", setup_05__1.profile, (ftnlen *)0,
	     -60 };
    static Vardesc rwscale_dv = { "RWSCALE", (char *)&datacom_m05__1.rwscale, 
	    (ftnlen *)0, 5 };
    static Vardesc corlmin_dv = { "CORLMIN", (char *)&setup_05__1.corlmin, (
	    ftnlen *)0, 5 };
    static Vardesc zoffset_dv = { "ZOFFSET", (char *)&
	    tgcmoffset_m05__1.zoffset, (ftnlen *)0, 5 };
    static Vardesc dustlat_dv = { "DUSTLAT", (char *)&datacom_m05__1.dustlat, 
	    (ftnlen *)0, 5 };
    static Vardesc als0_dv = { "ALS0", (char *)&datacom_m05__1.als0, (ftnlen *
	    )0, 5 };
    static Vardesc dustmin_dv = { "DUSTMIN", (char *)&datacom_m05__1.dustmin, 
	    (ftnlen *)0, 5 };
    static Vardesc dustmax_dv = { "DUSTMAX", (char *)&datacom_m05__1.dustmax, 
	    (ftnlen *)0, 5 };
    static Vardesc dustlon_dv = { "DUSTLON", (char *)&datacom_m05__1.dustlon, 
	    (ftnlen *)0, 5 };
    static Vardesc dusttau_dv = { "DUSTTAU", (char *)&datacom_m05__1.dusttau, 
	    (ftnlen *)0, 5 };

    static Vardesc *input_vl[] = { &lstfl_dv, &outfl_dv, &trajfl_dv, &
	    profile_dv, &wavefile_dv, &datadir_dv, &iert_dv, &iutc_dv, &
	    gcmdir_dv, &month_dv, &mday_dv, &myear_dv, &npos_dv, &ihr_dv, &
	    imin_dv, &sec_dv, &lonew_dv, &dusttau_dv, &dustmin_dv, &
	    dustmax_dv, &dustnu_dv, &dustdiam_dv, &dustdens_dv, &als0_dv, &
	    alsdur_dv, &intens_dv, &radmax_dv, &dustlat_dv, &dustlon_dv, &
	    f107_dv, &stdl_dv, &nr1_dv, &nvarx_dv, &nvary_dv, &logscale_dv, &
	    flat_dv, &flon_dv, &fhgt_dv, &molahgts_dv, &hgtasfcm_dv, &
	    zoffset_dv, &ibougher_dv, &delhgt_dv, &dellat_dv, &dellon_dv, &
	    deltime_dv, &deltatex_dv, &profnear_dv, &proffar_dv, &rpscale_dv, 
	    &rwscale_dv, &wlscale_dv, &wmscale_dv, &blwinfac_dv, &nmonte_dv, &
	    iup_dv, &wavea0_dv, &wavedate_dv, &wavea1_dv, &wavephi1_dv, &
	    phi1dot_dv, &wavea2_dv, &wavephi2_dv, &phi2dot_dv, &wavea3_dv, &
	    wavephi3_dv, &phi3dot_dv, &iuwave_dv, &wscale_dv, &corlmin_dv, &
	    ipclat_dv, &requa_dv, &rpole_dv, &mapyear_dv, &idaydata_dv };
    static Namelist input = { "INPUT", input_vl, 75 };

    /* Fortran I/O blocks */
    static cilist io___18 = { 0, 0, 0, 0, 0 };
    static cilist io___19 = { 0, 8, 0, (char *)&input, 0 };
    static cilist io___20 = { 0, 0, 0, fmt_382, 0 };
    static cilist io___21 = { 0, 0, 0, fmt_382, 0 };
    static cilist io___23 = { 0, 0, 1, 0, 0 };
    static cilist io___28 = { 0, 0, 0, fmt_11, 0 };
    static cilist io___29 = { 0, 0, 0, fmt_13, 0 };
    static cilist io___30 = { 0, 0, 0, fmt_17, 0 };
    static cilist io___31 = { 0, 0, 0, fmt_1, 0 };
    static cilist io___32 = { 0, 0, 0, fmt_14, 0 };
    static cilist io___33 = { 0, 0, 0, fmt_24, 0 };
    static cilist io___34 = { 0, 0, 0, fmt_16, 0 };
    static cilist io___35 = { 0, 0, 0, fmt_13, 0 };
    static cilist io___36 = { 0, 0, 0, fmt_17, 0 };
    static cilist io___37 = { 0, 21, 0, fmt_621, 0 };
    static cilist io___38 = { 0, 21, 0, fmt_721, 0 };
    static cilist io___39 = { 0, 22, 0, fmt_622, 0 };
    static cilist io___40 = { 0, 22, 0, fmt_722, 0 };
    static cilist io___41 = { 0, 23, 0, fmt_623, 0 };
    static cilist io___42 = { 0, 23, 0, fmt_723, 0 };
    static cilist io___43 = { 0, 24, 0, fmt_624, 0 };
    static cilist io___44 = { 0, 24, 0, fmt_724, 0 };
    static cilist io___45 = { 0, 25, 0, fmt_625, 0 };
    static cilist io___46 = { 0, 25, 0, fmt_725, 0 };
    static cilist io___47 = { 0, 25, 0, 0, 0 };
    static cilist io___48 = { 0, 26, 0, fmt_626, 0 };
    static cilist io___49 = { 0, 26, 0, fmt_726, 0 };
    static cilist io___50 = { 0, 27, 0, fmt_627, 0 };
    static cilist io___51 = { 0, 27, 0, fmt_727, 0 };
    static cilist io___56 = { 0, 29, 0, fmt_629, 0 };
    static cilist io___59 = { 0, 0, 0, fmt_60, 0 };
    static cilist io___60 = { 1, 10, 1, 0, 0 };
    static cilist io___61 = { 0, 11, 0, fmt_66, 0 };
    static cilist io___63 = { 1, 11, 0, 0, 0 };
    static cilist io___65 = { 0, 11, 0, fmt_66, 0 };
    static cilist io___66 = { 1, 11, 0, 0, 0 };
    static cilist io___67 = { 0, 9, 0, 0, 0 };
    static cilist io___68 = { 0, 12, 0, 0, 0 };
    static cilist io___70 = { 0, 0, 0, fmt_92, 0 };
    static cilist io___71 = { 0, 0, 0, fmt_91, 0 };
    static cilist io___72 = { 0, 0, 0, fmt_283, 0 };
    static cilist io___73 = { 0, 0, 0, fmt_284, 0 };
    static cilist io___74 = { 0, 0, 0, fmt_285, 0 };
    static cilist io___75 = { 0, 0, 0, fmt_286, 0 };
    static cilist io___76 = { 0, 0, 0, fmt_290, 0 };
    static cilist io___77 = { 0, 0, 0, fmt_291, 0 };
    static cilist io___78 = { 0, 0, 0, fmt_292, 0 };
    static cilist io___79 = { 0, 0, 0, fmt_294, 0 };
    static cilist io___80 = { 0, 0, 0, fmt_293, 0 };
    static cilist io___81 = { 0, 0, 0, fmt_297, 0 };
    static cilist io___82 = { 0, 0, 0, fmt_296, 0 };
    static cilist io___91 = { 0, 0, 0, 0, 0 };
    static cilist io___92 = { 0, 0, 0, 0, 0 };
    static cilist io___93 = { 0, 0, 0, 0, 0 };
    static cilist io___94 = { 0, 0, 0, 0, 0 };
    static cilist io___95 = { 0, 0, 0, 0, 0 };
    static cilist io___96 = { 0, 0, 0, fmt_298, 0 };
    static cilist io___99 = { 0, 0, 0, fmt_298, 0 };
    static cilist io___100 = { 0, 0, 0, fmt_363, 0 };
    static cilist io___101 = { 0, 0, 0, fmt_366, 0 };
    static cilist io___102 = { 0, 0, 0, fmt_368, 0 };
    static cilist io___103 = { 0, 0, 0, fmt_371, 0 };
    static cilist io___104 = { 0, 0, 0, fmt_372, 0 };
    static cilist io___105 = { 0, 0, 0, fmt_369, 0 };
    static cilist io___106 = { 0, 0, 0, fmt_370, 0 };
    static cilist io___107 = { 0, 0, 0, fmt_381, 0 };
    static cilist io___108 = { 0, 0, 0, fmt_380, 0 };
    static cilist io___109 = { 0, 0, 0, fmt_381, 0 };
    static cilist io___110 = { 0, 0, 0, fmt_380, 0 };


/*     Subroutine Setup_M05(CHGT,CLAT,CLON,CSEC,DAY0,RHOd,RHOu,RHOv,     SETU  1 */
/*    & RHOw,DHGT,DLAT,DLON,DTIME,MAXNUM,NRN1,NMCR1,dsunLs,dradau,dowlt, SETU  2 */
/*    & LnEW,INPUTFL,iustdout,iulist,hgtasfc,InERT,InUTC,stepmin,profnr, SETU  3 */
/*    & proffr,nprof)                                                    SETU  4 */
/*                                                                       SETU  5 */
/*                                                                       SETU  7 */
/* ...  Set parameter values for number of MOLA topography lat-lons,      SETU  8 */
/*     number of albedo lat-lons, number of boundary layer levels,       SETU  9 */
/*     number of dust optical depths, and number of F10 values           SETU 10 */
/*                                                                       SETU 28 */
/*                                                                       SETU 33 */
/* ...................................................................... SETU 52 */
/*     If output to the list file, output file, and plotable files       SETU 53 */
/*     is not desired, the following statements may be removed           SETU 54 */
/*                                                                       SETU 55 */
/*                                                                       SETU 61 */
/*     Note, however, molatoph.bin, COSPAR2.DAT, and albedo1.bin files   SETU 62 */
/*     are required                                                      SETU 63 */
/*                                                                       SETU 64 */
/* L1: */
/* ...................................................................... SETU 69 */
/* ...................................................................... SETU 83 */
/*     Definition of the Namelist input data                             SETU 84 */
/* .....................................................................  SETU 95 */
/*                                                                       SETU 96 */
/* ...  Default Mapping Year = 1                                          SETU 97 */
/*      MapYear = 1                                                       SETU 98 */
/* ...  Default profile parameters                                        SETU 99 */
/*      profnear = 0.0d0                                                  SETU100 */
/*      proffar = 0.0d0                                                   SETU101 */
/* ...  Default daily max/min data                                        SETU102 */
/*      idaydata = 1                                                      SETU103 */
/* ...  Default perturbation factors                                      SETU104 */
/*      rwscale = 1.0d0                                                   SETU105 */
/*      wlscale = 1.0d0                                                   SETU106 */
/*      blwinfac = 1.0d0                                                  SETU106a */
/* ...  Default mean wind scale factor                                    SETU107 */
/*      wmscale = 1.0d0                                                   SETU108 */
/* ...  Default corlmin value                                             SETU109 */
/*      corlmin = 0.0d0                                                   SETU110 */
/* ...  Default time options IERT = 1 for Earth-receive time and IUTC =   SETU111 */
/*     1 for UTC time (not Terrestrial Dynamical Time)                   SETU112 */
/*      IERT = 1                                                          SETU113 */
/*      IUTC = 1                                                          SETU114 */
/* ...  Default planetry radii and lat/height input                       SETU115 */
/*      requa = 3396.19d0                                                 SETU116 */
/*      rpole = 3376.20d0                                                 SETU117 */
/*      ipclat = 1                                                        SETU118 */
/* ...  Default with no wave model modification                           SETU119 */
/*      WaveA0 = 1.0d0                                                    SETU120 */
/*      WaveDate = 0.0d0                                                  SETU121 */
/*      WaveA1 = 0.0d0                                                    SETU122 */
/*      Wavephi1 = 0.0d0                                                  SETU123 */
/*      phi1dot = 0.0d0                                                   SETU124 */
/*      WaveA2 = 0.0d0                                                    SETU125 */
/*      Wavephi2 = 0.0d0                                                  SETU126 */
/*      phi2dot = 0.0d0                                                   SETU127 */
/*      WaveA3 = 0.0d0                                                    SETU128 */
/*      Wavephi3 = 0.0d0                                                  SETU129 */
/*      phi3dot = 0.0d0                                                   SETU130 */
/*      iuwave = 0                                                        SETU131 */
/*      Wscale = 20.0d0                                                   SETU132 */
/* ...  Set dust optical depths for GCM data                              SETU133 */
    mgcmparm_m05__1.dust[0] = .3;
    mgcmparm_m05__1.dust[1] = 1.;
    mgcmparm_m05__1.dust[2] = 3.;
    s_copy(mgcmparm_m05__1.dustc, "03", (ftnlen)2, (ftnlen)2);
    s_copy(mgcmparm_m05__1.dustc + 2, "10", (ftnlen)2, (ftnlen)2);
    s_copy(mgcmparm_m05__1.dustc + 4, "30", (ftnlen)2, (ftnlen)2);
/* ...  Set heights for GCM boundary layer data                           SETU140 */
    mgcmparm_m05__1.dzbl[0] = 0.;
    mgcmparm_m05__1.dzbl[1] = .005;
    mgcmparm_m05__1.dzbl[2] = .03;
/* ...  Set surface roughness parameter (m).  Value 1 cm (0.01 m) is      SETU144 */
/*     consistent with NASA Ames MGCM boundary layer model               SETU145 */
    mgcmparm_m05__1.zwsfc = .01;
/* ...  Set solar activity values (F10.7 at 1AU)                          SETU147 */
    mgcmparm_m05__1.f10val[0] = 70.;
    mgcmparm_m05__1.f10val[1] = 130.;
    s_copy(mgcmparm_m05__1.solact, "ls", (ftnlen)2, (ftnlen)2);
    s_copy(mgcmparm_m05__1.solact + 2, "ms", (ftnlen)2, (ftnlen)2);
/* ...  Set values for TES year 1 and year 2 data files                   SETU152 */
    s_copy(mgcmparm_m05__1.tesyr, "y1", (ftnlen)2, (ftnlen)2);
    s_copy(mgcmparm_m05__1.tesyr + 2, "y2", (ftnlen)2, (ftnlen)2);
    s_copy(mgcmparm_m05__1.soltes, "ls", (ftnlen)2, (ftnlen)2);
    s_copy(mgcmparm_m05__1.soltes + 2, "ms", (ftnlen)2, (ftnlen)2);
    s_copy(mgcmparm_m05__1.soltes + 4, "hs", (ftnlen)2, (ftnlen)2);
    mgcmparm_m05__1.f10tes[0] = 70.;
    mgcmparm_m05__1.f10tes[1] = 130.;
    mgcmparm_m05__1.f10tes[2] = 200.;
/* ...  Set unit number for screen I/O and pass it into Common            SETU161 */
    datacom_m05__1.iu0 = *iustdout;
/* ...  default list and output files                                     SETU163 */
/*      lstfl = 'LIST.txt'                                                SETU164 */
/*      outfl = 'OUTPUT.txt'                                              SETU165 */
/* ...  Default number of positions                                       SETU166 */
/*      NPOS = 21                                                         SETU167 */
/* ...  Default use West Longitude positive (USGS Convention)             SETU168 */
/*      LonEW = 0                                                         SETU169 */
/* ...  Default background dust optical depth, and min & max              SETU170 */
/*      Dusttau = 0.3d0                                                   SETU171 */
/*      Dustmin = 0.3d0                                                   SETU172 */
/*      Dustmax = 1.0d0                                                   SETU173 */
/* ...  Default dust particle density parameters                          SETU174 */
/*     See Fig. 2 of Haberle et al., J. geophys. Res., vol 104, p. 8957  SETU175 */
/*     (1999) and Haberle et al., Icarus, vol 50, p. 322 (1982)          SETU176 */
/*      Dustnu = 0.003d0                                                  SETU177 */
/*      Dustdiam = 5.0d0                                                  SETU178 */
/*      Dustdens = 3000.0d0                                               SETU179 */
/* ...  Default no dust storm                                             SETU180 */
/*      ALS0 = 0.0d0                                                      SETU181 */
/*      ALSDUR = 48.0d0                                                   SETU182 */
/*      INTENS = 0.0d0                                                    SETU183 */
/*      RADMAX = 0.0d0                                                    SETU184 */
/*      DUSTLAT = 0.0d0                                                   SETU185 */
/*      DUSTLON = 0.0d0                                                   SETU186 */
/* ...  Default Solar Flux parameters                                     SETU187 */
/*      F107 = 68.0d0                                                     SETU188 */
/*      Stdl = 0.0d0                                                      SETU189 */
/* ...  Default plot variable = height above MOLA areoid                  SETU190 */
/*      NVARX = 1                                                         SETU191 */
/*      NVARY = 0                                                         SETU192 */
/* ...  Default to regular linear scale                                   SETU193 */
/*      LOGSCALE = 0                                                      SETU194 */
/* ...  Default random number seed and Number of Monte Carlo runs         SETU195 */
/*      NR1 = 1001                                                        SETU196 */
/*      NMONTE = 1                                                        SETU197 */
/* ...  Default unit number for print output data file                    SETU198 */
/*      iup = 13                                                          SETU199 */
/* ...  Set length of Mars day                                            SETU200 */
    datacom_m05__1.day = 24.622962;
/* ...  Default to input heights above MOLA areoid                        SETU202 */
/*      MOLAhgts = 1                                                      SETU203 */
/* ...  Default height above surface                                      SETU204 */
/*      hgtasfcm = 0.0d0                                                  SETU205 */
/* ...  Default (global) MTGCM height offset                              SETU206 */
/*      zoffset = 5.0d0                                                   SETU207 */
/*      ibougher = 2                                                      SETU208 */
/* ...  Open Namelist data file                                           SETU209 */
    if (*isload == 0) {
	o__1.oerr = 1;
	o__1.ounit = 8;
	o__1.ofnmlen = 60;
	o__1.ofnm = inputfl;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	ioerr = f_open(&o__1);
	if (ioerr != 0) {
	    io___18.ciunit = *iustdout;
	    s_wsle(&io___18);
	    do_lio(&c__9, &c__1, " Error opening NAMELIST input file", (
		    ftnlen)34);
	    e_wsle();
	    s_stop("", (ftnlen)0);
	}
/* ...    Read Namelist data                                              SETU215 */
	s_rsne(&io___19);
	cl__1.cerr = 0;
	cl__1.cunit = 8;
	cl__1.csta = 0;
	f_clos(&cl__1);
    }
/* .....................................................................  SETU218 */
/*     For compilers not supporting the NAMELIST input mode, the         SETU219 */
/*     previous Read statement may be replaced by:                       SETU220 */
/*                                                                       SETU221 */
/*     Read(8,10)LSTFL                                                   SETU222 */
/*     Read(8,10)OUTFL                                                   SETU223 */
/*     Read(8,10)TRAJFL                                                  SETU224 */
/*     Read(8,10)WaveFile                                                SETU225 */
/*     Read(8,10)DATADIR                                                 SETU226 */
/*     Read(8,10)GCMDIR                                                  SETU227 */
/*     Read(8,10)profile                                                 SETU228 */
/* 10  Format(A)                                                         SETU229 */
/*     Read(8,*)IERT,IUTC,MONTH,MDAY,MYEAR,NPOS,IHR,IMIN,SEC,LonEW,      SETU230 */
/*    & Dusttau,Dustmin,Dustmax,Dustnu,Dustdiam,Dustdens,ALS0,ALSDUR,    SETU231 */
/*    & INTENS,RADMAX,DUSTLAT,DUSTLON,F107,STDL,NR1,NVARX,NVARY,         SETU232 */
/*    & LOGSCALE,FLAT,FLON,FHGT,MOLAhgts,hgtasfcm,zoffset,ibougher,      SETU233 */
/*    & DELHGT,DELLAT,DELLON,DELTIME,deltaTEX,profnear,proffar,rpscale,  SETU234 */
/*    & rwscale,wlscale,wmscale,blwinfac,NMONTE,iup,WaveA0,WaveDate,     SETU235 */
/*    & WaveA1,Wavephi1,phi1dot,WaveA2,Wavephi2,phi2dot,WaveA3,          SETU236 */
/*    & Wavephi3,phi3dot,iuwave,Wscale,corlmin,ipclat,requa,rpole,       SETU237 */
/*    & MapYear,idaydata                                                 SETU238 */
/*                                                                       SETU239 */
/*     and the NAMELIST file INPUT may be modified to contain free-      SETU240 */
/*     field input data as in the above list.                            SETU241 */
/* .....................................................................  SETU242 */
/* ...  Reset day, time of day, and location: */
    setup_05__1.myear = *iyear;
    setup_05__1.month = *imonth;
    setup_05__1.mday = *iday;
    setup_05__1.ihr = *ihour;
    setup_05__1.imin = *iminute;
    setup_05__1.sec = *csec;
    setup_05__1.fhgt = *chgt;
    setup_05__1.flat = *clat;
    setup_05__1.flon = *clon;
/* ...  Check that unit iup is in allowable range                         SETU243 */
    if (datacom_m05__1.iup >= 5 && datacom_m05__1.iup <= 12 || 
	    datacom_m05__1.iup >= 21 && datacom_m05__1.iup <= 29) {
	s_stop(" Unit iup conflict with another file", (ftnlen)36);
    }
    if (setup_05__1.flat < -90. || setup_05__1.flat > 90.) {
	io___20.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___20);
	e_wsfe();
	goto L9998;
    }
    if (setup_05__1.flon < 0.) {
	setup_05__1.flon += 360.;
    }
    if (setup_05__1.flon < 0. || setup_05__1.flon > 360.) {
	io___21.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___21);
	e_wsfe();
	goto L9998;
    }
/* ...  Store values for output arguments                                 SETU256 */
    *profnr = setup_05__1.profnear;
    *proffr = setup_05__1.proffar;
    *lnew = setup_05__1.lonew;
    if (*isload == 0) {
/* ...    Test profnear and proffar.  Read profile data if profnear > 0     SETU260 */
	if (setup_05__1.profnear > 0.) {
	    if (setup_05__1.proffar <= setup_05__1.profnear) {
		s_stop(" proffar must be > profnear", (ftnlen)27);
	    }
	    rdprof_m05__(setup_05__1.profile, nprof, &setup_05__1.lonew, (
		    ftnlen)60);
	}
    }
/* ...  Must use planeto-centric height and latitude if MOLAhgts = 1      SETU265 */
    if (datacom_m05__1.molahgts == 1) {
	datacom_m05__1.ipclat = 1;
    }
    *iulist = datacom_m05__1.iup;
/* ...  Convert Height above surface to km and insure proper range        SETU268 */
    if (setup_05__1.hgtasfcm < 0.) {
	setup_05__1.hgtasfcm = 0.;
    }
    if (setup_05__1.hgtasfcm > 4500.) {
	setup_05__1.hgtasfcm = 4500.;
    }
    *hgtasfc = setup_05__1.hgtasfcm / 1e3;
/* ...  Insure Wscale within proper range                                 SETU272 */
    if (wavecoef_m05__1.wscale <= 10.) {
	wavecoef_m05__1.wscale = 10.;
    }
    if (wavecoef_m05__1.wscale >= 1e4) {
	wavecoef_m05__1.wscale = 1e4;
    }
/* ...  Set traveling wave parameters if WaveDate le 0                    SETU275 */
    if (wavecoef_m05__1.wavedate <= 0.) {
	wavecoef_m05__1.wavedate = 0.;
	wavecoef_m05__1.phi1dot = 0.;
	wavecoef_m05__1.phi2dot = 0.;
	wavecoef_m05__1.phi3dot = 0.;
    }
/* ...  Open and read WaveFile and load wavedata array if iuwave>0        SETU282 */
    if (siload == 0.) {
	if (wavecoef_m05__1.iuwave > 0) {
	    o__1.oerr = 1;
	    o__1.ounit = wavecoef_m05__1.iuwave;
	    o__1.ofnmlen = 60;
	    o__1.ofnm = setup_05__1.wavefile;
	    o__1.orl = 0;
	    o__1.osta = 0;
	    o__1.oacc = 0;
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    *ierr1 = f_open(&o__1);
	    if (*ierr1 != 0) {
		s_stop("  Error opening WaveFile", (ftnlen)24);
	    }
	    wavecoef_m05__1.nwave = 1;
/* ...      Each WaveFile record contains: (1) time at which wave model     SETU287 */
/*           coefficients first apply (1st record must start at time=0),   SETU288 */
/*           (2) WaveA0, (3) WaveDate, (4) WaveA1, (5) Wavephi1, (6)       SETU289 */
/*           phi1dot, (6), WaveA2, (7)Wavephi2, (8) phi2dot, (9) WaveA3,   SETU290 */
/*           (10) Wavephi3, (11) phi3dot.  Times for wave model            SETU291 */
/*            coefficients must be in ascending order.                     SETU292 */
L5:
	    io___23.ciunit = wavecoef_m05__1.iuwave;
	    i__1 = s_rsle(&io___23);
	    if (i__1 != 0) {
		goto L6;
	    }
	    i__1 = do_lio(&c__5, &c__1, (char *)&wavecoef_m05__1.wavetime[
		    wavecoef_m05__1.nwave - 1], (ftnlen)sizeof(doublereal));
	    if (i__1 != 0) {
		goto L6;
	    }
	    for (i__ = 1; i__ <= 11; ++i__) {
		i__1 = do_lio(&c__5, &c__1, (char *)&wavecoef_m05__1.wavedata[
			wavecoef_m05__1.nwave + i__ * 100 - 101], (ftnlen)
			sizeof(doublereal));
		if (i__1 != 0) {
		    goto L6;
		}
	    }
	    i__1 = e_rsle();
	    if (i__1 != 0) {
		goto L6;
	    }
	    if (wavecoef_m05__1.nwave == 1) {
		if (wavecoef_m05__1.wavetime[0] > 0.) {
		    s_stop(" First wave data in file must be at time 0", (
			    ftnlen)42);
		}
	    } else {
		if (wavecoef_m05__1.wavetime[wavecoef_m05__1.nwave - 1] <= 
			wavecoef_m05__1.wavetime[wavecoef_m05__1.nwave - 2]) {
		    s_stop(" Wave data in file must in increasing order by t\
ime", (ftnlen)51);
		}
	    }
	    if (wavecoef_m05__1.wavedata[wavecoef_m05__1.nwave - 1] < .1 || 
		    wavecoef_m05__1.wavedata[wavecoef_m05__1.nwave - 1] > 12.)
		     {
		s_stop(" WaveA0 from input file is out of range", (ftnlen)39);
	    }
	    ++wavecoef_m05__1.nwave;
	    goto L5;
/* ...      Close wave data file                                            SETU305 */
L6:
	    cl__1.cerr = 0;
	    cl__1.cunit = wavecoef_m05__1.iuwave;
	    cl__1.csta = 0;
	    f_clos(&cl__1);
/* ...      Re-set nwave to number of wave data                             SETU307 */
	    --wavecoef_m05__1.nwave;
/* ...      Re-initialize wave coefficients                                 SETU309 */
	    wavecoef_m05__1.wavea0 = wavecoef_m05__1.wavedata[0];
	    wavecoef_m05__1.wavedate = wavecoef_m05__1.wavedata[100];
	    wavecoef_m05__1.wavea1 = wavecoef_m05__1.wavedata[200];
	    wavecoef_m05__1.wavephi1 = wavecoef_m05__1.wavedata[300];
	    wavecoef_m05__1.phi1dot = wavecoef_m05__1.wavedata[400];
	    wavecoef_m05__1.wavea2 = wavecoef_m05__1.wavedata[500];
	    wavecoef_m05__1.wavephi2 = wavecoef_m05__1.wavedata[600];
	    wavecoef_m05__1.phi2dot = wavecoef_m05__1.wavedata[700];
	    wavecoef_m05__1.wavea3 = wavecoef_m05__1.wavedata[800];
	    wavecoef_m05__1.wavephi3 = wavecoef_m05__1.wavedata[900];
	    wavecoef_m05__1.phi3dot = wavecoef_m05__1.wavedata[1000];
	}
    }
/* ...  Open and read TES dust optical depths                             SETU322 */
    if (*isload == 0) {
	readtes_m05__(setup_05__1.datadir, "1", (ftnlen)60, (ftnlen)1);
/* ...    Convert FLON, DUSTLON, DELLON to West if LonEW=1                  SETU324 */
	if (setup_05__1.lonew == 1) {
	    datacom_m05__1.dustlon = 360. - datacom_m05__1.dustlon;
/*          FLON = 360.0d0 - FLON                                           SETU327 */
	    setup_05__1.dellon = -setup_05__1.dellon;
	}
    }
    if (setup_05__1.lonew == 1) {
	setup_05__1.flon = 360. - setup_05__1.flon;
    }
/*      Write(6,*)'DUSTLON = ',DUSTLON */
/*      Write(6,*)'FLON = ',FLON */
/*      Write(6,*)'DELLON = ',DELLON */
/* ...  Insure corlmin input value within proper bounds                   SETU330 */
    if (setup_05__1.corlmin < 0.) {
	setup_05__1.corlmin = 0.;
    }
    if (setup_05__1.corlmin > 1.) {
	setup_05__1.corlmin = 1.;
    }
    datacom_m05__1.rpfactor = setup_05__1.rpscale;
    if (setup_05__1.rpscale < 0. || setup_05__1.rpscale > 2.) {
	s_stop(" Must have 0 <= rpscale <= 2", (ftnlen)28);
    }
/* ...  Insure Dustmin and Dustmax in proper ranges                       SETU336 */
    if (datacom_m05__1.dustmin < .1) {
	datacom_m05__1.dustmin = .1;
    }
    if (datacom_m05__1.dustmax > 1.) {
	datacom_m05__1.dustmax = 1.;
    }
    if (wavecoef_m05__1.wavea0 <= .1 || wavecoef_m05__1.wavea0 > 12.) {
	s_stop(" WaveA0 out of range", (ftnlen)20);
    }
    if (setup_05__1.nmonte < 1) {
	setup_05__1.nmonte = 1;
    }
/* ...  Pass corlmin value to output                                      SETU342 */
    *stepmin = setup_05__1.corlmin;
/* ...  Pass 1st random number and Number Monte Carlo runs to output      SETU344 */
    *nrn1 = setup_05__1.nr1;
    *nmcr1 = setup_05__1.nmonte;
/* ...  Check option values and pass to output or Common DATACOM          SETU347 */
    if (datacom_m05__1.wlscale < .1) {
	datacom_m05__1.wlscale = .1;
    }
    if (datacom_m05__1.wlscale > 10.) {
	datacom_m05__1.wlscale = 10.;
    }
    if (datacom_m05__1.blwinfac < 0.) {
	datacom_m05__1.blwinfac = 0.;
    }
    if (datacom_m05__1.rwscale < 0.) {
	datacom_m05__1.rwscale = 0.;
    }
    *inert = setup_05__1.iert;
    *inutc = setup_05__1.iutc;
    inpclat = datacom_m05__1.ipclat;
/* ...  Insure requa and rpole within bounds                              SETU355 */
    if (datacom_m05__1.requa < 3300. || datacom_m05__1.requa > 3500. || 
	    datacom_m05__1.rpole < 3300. || datacom_m05__1.rpole > 3500.) {
	s_stop(" requa or rpole out of bounds", (ftnlen)29);
    }
    requat = datacom_m05__1.requa;
    rpoles = datacom_m05__1.rpole;
/* ...  Insure MapYear within legal limits                                SETU364 */
    if (datacom_m05__1.mapyear < 0) {
	datacom_m05__1.mapyear = 0;
    }
    if (datacom_m05__1.mapyear > 2) {
	datacom_m05__1.mapyear = 2;
    }
    *dhgt = setup_05__1.delhgt;
    *dlat = setup_05__1.dellat;
    *dlon = setup_05__1.dellon;
    *dtime = setup_05__1.deltime;
/*                                                                       SETU371 */
/*     If output to the list file, output file, and plotable files       SETU372 */
/*     is not desired, the following statements may be removed.          SETU373 */
/*     Note, however, that the HEIGHTS.DAT file is required.             SETU374 */
/*     Note that output to list and other files can also be suppressed   SETU375 */
/*     by setting iup to 0 above, near line SETU153                      SETU376 */
/*                                                                       SETU377 */
    if (*isload == 0) {
	s_copy(files, filename_m05__1.lstfl, (ftnlen)60, (ftnlen)60);
	s_copy(files + 540, filename_m05__1.outfl, (ftnlen)60, (ftnlen)60);
	if (s_cmp(filename_m05__1.lstfl, "CON", (ftnlen)60, (ftnlen)3) != 0 &&
		 s_cmp(filename_m05__1.lstfl, "con", (ftnlen)60, (ftnlen)3) !=
		 0 && datacom_m05__1.iup > 0) {
	    o__1.oerr = 1;
	    o__1.ounit = datacom_m05__1.iup;
	    o__1.ofnmlen = 60;
	    o__1.ofnm = filename_m05__1.lstfl;
	    o__1.orl = 0;
	    o__1.osta = 0;
	    o__1.oacc = 0;
	    o__1.ofm = 0;
	    o__1.oblnk = 0;
	    *ierr1 = f_open(&o__1);
	}
/* ...    Write version number and file names to standard output            SETU383 */
/*        Write(iu0,1)version                                               SETU384 */
/*        If(iup.gt.0)Then                                                  SETU385 */
/*          Write(iu0,14)LSTFL,OUTFL                                        SETU386 */
/*        Else                                                              SETU387 */
/*          Write(iu0,14)'null','null'                                      SETU388 */
/*        Endif                                                             SETU389 */
/*        Write(iu0,15)DATADIR,GCMDIR,MapYear                               SETU390 */
/*        If (npos.eq.0)Write(iu0,16)TRAJFL                                 SETU391 */
	if (datacom_m05__1.npos > 0) {
	    goto L12;
	}
/* ...    If NPOS = 0 is entered, program reads position data from          SETU393 */
/*        unit 7, trajectory data file                                     SETU394 */
/* ...    Each trajectory file record contains time (seconds from initial   SETU395 */
/*        time), height (km), latitude (degrees, North positive), and      SETU396 */
/*        longitude (degrees, West positive if LonEW=0 or East positive    SETU397 */
/*        if LonEW=1).                                                     SETU398 */
	o__1.oerr = 1;
	o__1.ounit = 7;
	o__1.ofnmlen = 60;
	o__1.ofnm = setup_05__1.trajfl;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr7 = f_open(&o__1);
	if (*ierr7 != 0) {
	    io___28.ciunit = datacom_m05__1.iu0;
	    s_wsfe(&io___28);
	    e_wsfe();
	    goto L9998;
	}
    }
L12:
    *maxnum = datacom_m05__1.npos - 1;
    if (setup_05__1.profnear > 0.) {
	io___29.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___29);
	do_fio(&c__1, setup_05__1.profile, (ftnlen)60);
	e_wsfe();
    }
    if (wavecoef_m05__1.iuwave > 0) {
	io___30.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___30);
	do_fio(&c__1, setup_05__1.wavefile, (ftnlen)60);
	e_wsfe();
    }
    if (datacom_m05__1.npos <= 0) {
	*maxnum = 99999;
    }
/* ...  Write version number and file names to LIST file                  SETU409 */
    if (datacom_m05__1.iup > 0) {
	io___31.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___31);
	do_fio(&c__1, "1", (ftnlen)1);
	e_wsfe();
	io___32.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___32);
	do_fio(&c__1, filename_m05__1.lstfl, (ftnlen)60);
	do_fio(&c__1, filename_m05__1.outfl, (ftnlen)60);
	e_wsfe();
	io___33.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___33);
	do_fio(&c__1, setup_05__1.datadir, (ftnlen)60);
	do_fio(&c__1, setup_05__1.gcmdir, (ftnlen)60);
	e_wsfe();
	if (datacom_m05__1.npos == 0) {
	    io___34.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___34);
	    do_fio(&c__1, setup_05__1.trajfl, (ftnlen)60);
	    e_wsfe();
	}
	if (setup_05__1.profnear > 0.) {
	    io___35.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___35);
	    do_fio(&c__1, setup_05__1.profile, (ftnlen)60);
	    e_wsfe();
	}
	if (wavecoef_m05__1.iuwave > 0) {
	    io___36.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___36);
	    do_fio(&c__1, setup_05__1.wavefile, (ftnlen)60);
	    e_wsfe();
	}
    }
/* L15: */
/* ...  Files on units 21-27 contain parameters suitable for plotting.    SETU425 */
/* ...  Data are in either of two forms: (1)  X  Y1 Y2 ..., where X       SETU426 */
/* ...  is the variable to be plotted against (e.g. height), and Y1, Y2,  SETU427 */
/* ...  etc. are variables to be plotted, or (2) X Y Z1 Z2 ..., where X   SETU428 */
/* ...  and Y are two variables (e.g. latitude and height) to provide     SETU429 */
/* ...  position for plotting contour plots of one of the variables       SETU430 */
/* ...  Z1, Z2, etc.                                                      SETU431 */
    if (datacom_m05__1.iup > 0) {
/* ...    Unit 21 file = 'Density.txt': Headers for variables are -       SETU433 */
/*         DENSLO  =  low (-1 sigma) density                             SETU434 */
/*         DENSAV  =  average (mean plus wave-perturbed) density         SETU435 */
/*         DENSHI  =  high (+1 sigma) density                            SETU436 */
/*         DENSTOT =  total (average+perturbed) density                  SETU437 */
/*           (density units kg/m**3, log-10 scale, % from COSPAR, or     SETU438 */
/*             kg/km**3, depending on value of LOGSCALE input parameter) SETU439 */
/*         DustOD  =  dust optical depth                                 SETU440 */
/*         Radius  =  Radial distance from planetary center of mass to   SETU441 */
/*                    spacecraft position (areoid radius plus altitude)  SETU442 */
/*         Grav    =  local acceleration of gravity (m/s**2)             SETU443 */
/*         RadAU   =  Mars orbital radius (Astronomical Units)           SETU444 */
/*        LOGSCALE =  option controlling units of density output         SETU445 */
/*        hgtoffset=  local height offset (km) for MTGCM and MGCM data   SETU446 */
/*        ibougher =  input parameter controlling height offset option   SETU447 */
/*        MapYear  =  TES mapping year (0 for Mars-GRAM 2001 data)       SETU448 */
/*        profwgt  =  Weight factor for auxiliary input profile data     SETU449 */
/* ...                                                                    SETU450 */
	o__1.oerr = 1;
	o__1.ounit = 21;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 60;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr2 = f_open(&o__1);
	if (datacom_m05__1.nvary == 0) {
	    s_wsfe(&io___37);
	    e_wsfe();
	} else {
	    s_wsfe(&io___38);
	    e_wsfe();
	}
/* ...    Unit 22 file = 'Perturb.txt': Headers for variables are:        SETU463 */
/*         SigD     = Density standard deviation (% of unperturbed mean) SETU464 */
/*         DensRand = Random density perturbation (% of unpert. mean)    SETU465 */
/*         DensWave = Density wave perturbation (% of unperturbed mean)  SETU466 */
/*         DensP    = Total density perturbation (% of unperturbed mean) SETU467 */
/*         corlim   = Ratio of step size to correlation accuracy limit   SETU468 */
/*                     (ideally should be 1.0 or larger)                 SETU469 */
/*         SigU     = Standard deviation for wind perturbations (m/s)    SETU470 */
/*         SigW     = Standard deviation of vertical wind perturbations  SETU471 */
/*                    (m/s)                                              SETU472 */
/*         iupdate  = 1 if perturbations updated, 0 if perturbations not SETU473 */
/*                     updated but perturbation step updated, -1 if      SETU474 */
/*                     neither perturbations nor step updated            SETU475 */
/* ...                                                                    SETU476 */
	o__1.oerr = 1;
	o__1.ounit = 22;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 120;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr3 = f_open(&o__1);
	if (datacom_m05__1.nvary == 0) {
	    s_wsfe(&io___39);
	    e_wsfe();
	} else {
	    s_wsfe(&io___40);
	    e_wsfe();
	}
/* ...    Unit 23 file = 'Winds.txt' : Headers for variables are -        SETU487 */
/*         EWmean = mean eastward wind component (m/s)                   SETU488 */
/*         EWpert = perturbation in eastward wind component (m/s)        SETU489 */
/*         EWtot  = total (mean + perturbed) eastward wind (m/s)         SETU490 */
/*         NSmean = mean northward wind component (m/s)                  SETU491 */
/*         NSpert = perturbation in northward wind component (m/s)       SETU492 */
/*         NStot  = total (mean + perturbed) northward wind (m/s)        SETU493 */
/*         VWpert = vertical wind perturbation (m/s)                     SETU494 */
/*         iupdate  = 1 if perturbations updated, 0 if perturbations not SETU495 */
/*                     updated but perturbation step updated, -1 if      SETU496 */
/*                     neither perturbations nor step updated            SETU497 */
/* ...                                                                    SETU498 */
	o__1.oerr = 1;
	o__1.ounit = 23;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 180;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr4 = f_open(&o__1);
	if (datacom_m05__1.nvary == 0) {
	    s_wsfe(&io___41);
	    e_wsfe();
	} else {
	    s_wsfe(&io___42);
	    e_wsfe();
	}
/* ...    Unit 24 file = 'TPresHgt.txt' : Headers for variables are -     SETU509 */
/*         Temp    - Mean temperature (K)                                SETU510 */
/*         Pres    - Mean plus wave-perturbed pressure (N/m**2, log-10,  SETU511 */
/*                    or % from COSPAR, as determined by LOGSCALE)       SETU512 */
/*         TdegC   - Mean temperature (degrees C)                        SETU513 */
/*         Pres_mb - Mean plus wave-perturbed pressure (mb)              SETU514 */
/*         Hrho    - Density scale height (km)                           SETU515 */
/*         Hpres   - Pressure scale height (km)                          SETU516 */
/*         MolWt   - molecular weight (kg/kg-mole)                       SETU517 */
/*         Terhgt  - Altitude of local surface above MOLA 1/2-degree     SETU518 */
/*                   areoid                                              SETU519 */
/*         Tgrnd   - ground surface temperature (K)                      SETU520 */
/*         Areoid  - local radius (km) of MOLA 1/2-degree areoid         SETU521 */
/*         dAreoid - MOLA areoid minus radius of reference               SETU522 */
/*                   ellipsoid (km)                                      SETU523 */
/*         CO2%v   - mole fraction (%) Carbon Dioxide concentration (%   SETU524 */
/*                   by volume)                                          SETU525 */
/*          N2%v   - mole fraction (%) Nitrogen concentration (% by      SETU526 */
/*                   volume)                                             SETU527 */
/*          Ar%v   - mole fraction (%) Argon concentration (% by volume) SETU528 */
/*          O2%v   - mole fraction (%) Molecular Oxygen concentration    SETU529 */
/*                   (% by volume)                                       SETU530 */
/*          CO%v   - mole fraction (%) Carbon Monoxide concentration (%  SETU531 */
/*                   by volume)                                          SETU532 */
/*           O%v   - mole fraction (%) Atomic Oxygen concentration (% by SETU533 */
/*                   volume)                                             SETU534 */
/*          He%v   - mole fraction (%) Helium concentration (% by        SETU535 */
/*                   volume)                                             SETU536 */
/*          H2%v   - mole fraction (%) Molecular Hydrogen concentration  SETU537 */
/*                   (% by volume)                                       SETU538 */
/*           H%v   - mole fraction (%) Atomic Hydrogen concentration (%  SETU539 */
/*                   by volume)                                          SETU540 */
/*         H2O%v   - mole fraction (%) Water vapor concentration (% by   SETU541 */
/*                    volume)                                            SETU542 */
/*        LOGSCALE - option controlling units of pressure output         SETU543 */
/* ...                                                                    SETU544 */
	o__1.oerr = 1;
	o__1.ounit = 24;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 240;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr5 = f_open(&o__1);
	if (datacom_m05__1.nvary == 0) {
	    s_wsfe(&io___43);
	    e_wsfe();
	} else {
	    s_wsfe(&io___44);
	    e_wsfe();
	}
/* ...                                                                    SETU559 */
/* ...    Unit 25 file = 'DayData.txt' : Headers for variables are -      SETU560 */
/*         TempDay = Local daily average temperature (K)                 SETU561 */
/*         PresDay = Local daily average pressure (N/m**2)  (*)          SETU562 */
/*         DensDay = Local daily average density (kg/m**3)  (*)          SETU563 */
/*         EWwnDay = Local daily average Eastward wind (m/s)             SETU564 */
/*         NSwnDay = Local daily average Northward wind (m/s)            SETU565 */
/*         Tempmin = Local daily minimum temperature (K)                 SETU566 */
/*         Tempmax = Local daily maximum temperature (K)                 SETU567 */
/*         Densmin = Local daily minimum density (kg/m**3)  (*)          SETU568 */
/*         Densmax = Local daily maximum density (kg/m**3)  (*)          SETU569 */
/*        LOGSCALE = option controlling units of pressure and density    SETU570 */
/*         DensAv  = Local density (kg/m**3)  (*)                        SETU570a */
/*       -----                                                           SETU570b */
/*       (*) - or other units, as determined by LOGSCALE                 SETU570c */
/* ...                                                                    SETU571 */
	o__1.oerr = 1;
	o__1.ounit = 25;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 300;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr6 = f_open(&o__1);
	if (datacom_m05__1.nvary == 0) {
	    s_wsfe(&io___45);
	    e_wsfe();
	} else {
	    s_wsfe(&io___46);
	    e_wsfe();
	}
	if (setup_05__1.profnear > (float)0.) {
	    s_wsle(&io___47);
	    do_lio(&c__9, &c__1, " No output; profile being used", (ftnlen)30)
		    ;
	    e_wsle();
	}
/* ...                                                                    SETU585 */
/* ...    Unit 26 file = 'ThrmData.txt' : Headers for variables are -     SETU586 */
/*         Tbase     = temperature at 1.26 nbar level (K)                SETU587 */
/*         Zbase     = height of 1.26 nbar level (km)                    SETU588 */
/*         F1peak    = height of F1 peak layer (km)                      SETU589 */
/*         MolWgt    = molecular weight (kg/kg mole)                     SETU590 */
/*         Texos     = exospheric temperature (K)                        SETU591 */
/*         hgtoffset = local height offset (km) for MTGCM and MGCM data  SETU592 */
/*         ibougher  = input parameter controlling height offset option  SETU593 */
/* ...                                                                    SETU594 */
	o__1.oerr = 1;
	o__1.ounit = 26;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 360;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr7 = f_open(&o__1);
	if (datacom_m05__1.nvary == 0) {
	    s_wsfe(&io___48);
	    e_wsfe();
	} else {
	    s_wsfe(&io___49);
	    e_wsfe();
	}
/* ...                                                                    SETU605 */
/* ...    Unit 27 file = 'MarsRad.txt' : Headers for variables are -      SETU606 */
/*         alb      = surface albedo                                     SETU607 */
/*         mu0      = cosine of solar zenith angle                       SETU608 */
/*         Dareaden = dust column areal density (kg/m**2)                SETU609 */
/*         Dmixrat  = dust mixing ratio (kg dust / kg air)               SETU610 */
/*         Dmasden  = dust mass density (micrograms dust / m**3)         SETU611 */
/*         Dnumden  = dust number density (number dust particles / m**3) SETU612 */
/*         Ice      = surface polar ice indicator (0 = no, 1 = yes)      SETU613 */
/*                                                                       SETU614 */
/* ...                                                                    SETU615 */
	o__1.oerr = 1;
	o__1.ounit = 27;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 720;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr13 = f_open(&o__1);
	if (datacom_m05__1.nvary == 0) {
	    s_wsfe(&io___50);
	    e_wsfe();
	} else {
	    s_wsfe(&io___51);
	    e_wsfe();
	}
/*                                                                       SETU626 */
/* ...    Unit 29 file = OUTPUT file containing list of variables given   SETU627 */
/*       in Datastep_M05 routine (Format 800 or Format 810)              SETU628 */
/*                                                                       SETU629 */
/*       Headers for variables are:                                      SETU630 */
/*         Time    = time after initial input time (sec)                 SETU631 */
/*         Height  = planeto-centric height (km) above MOLA areoid       SETU632 */
/*                   (Height=HgtMOLA) OR planeto-centric height (km)     SETU633 */
/*                   above ellipsoid (Height=HgtELPS) OR planeto-centric SETU634 */
/*                   height (km) above local MOLA topographic surface    SETU635 */
/*                   (Height=HgtSFCM) OR planeto-graphic height (km)     SETU636 */
/*                   above ellipsoid (Height=HgtGRPH), as determined by  SETU637 */
/*                   input parameters MOLAhgts, NVARX, NVARY, and ipclat SETU638 */
/*          Lat    = planeto-centric latitude (Lat=LatPC) or planeto-    SETU639 */
/*                   graphic latitude (Lat=LatPG) in degrees (North      SETU640 */
/*                   positive)                                           SETU641 */
/*       LonW/LonE = longitude (degrees, West positive or East Positive) SETU642 */
/*        Denkgm3  = Average (mean plus wave=perturbed) density          SETU643 */
/*                   (kg/m**3) OR "Logkgm3" for Log10(kg/m**3) OR        SETU644 */
/*                   "Den%Avg" for percent deviation from COSPAR         SETU645 */
/*                   average, OR "Denkgkm3" for kg/km**3, depending on   SETU646 */
/*                   input value of LOGSCALE                             SETU647 */
/*         Temp    = average temperature (K)                             SETU648 */
/*        EWind    = eastward wind component (m/s, positive toward East) SETU649 */
/*        NWind    = northward wind component (m/s, positive toward      SETU650 */
/*                   North)                                              SETU651 */
/*         sigD    = standard deviation for density perturbations (% of  SETU652 */
/*                   unperturbed mean)                                   SETU653 */
/*          Ls     = areocentric longitude of Sun from Mars (degrees)    SETU654 */
/*         Dust    = dust optical depth                                  SETU655 */
/*        CO2%m    = Carbon Dioxide mass concentration (% by mass)       SETU656 */
/*         N2%m    = Nitrogen mass concentration (% by mass)             SETU657 */
/*         Ar%m    = Argon mass concentration (% by mass)                SETU658 */
/*         O2%m    = Molecular Oxygen mass concentration (% by mass)     SETU659 */
/*         CO%m    = Carbon Monoxide mass concentration (% by mass)      SETU660 */
/*          O%m    = Atomic Oxygen mass concentration (% by mass)        SETU661 */
/*         He%m    = Helium mass concentration (% by mass)               SETU662 */
/*         H2%m    = Molecular Hydrogen mass concentration (% by mass)   SETU663 */
/*          H%m    = Atomic Hydrogen mass concentration (% by mass)      SETU664 */
/*        H2O%m    = Water vapor mass concentration (% by mass)          SETU665 */
	o__1.oerr = 1;
	o__1.ounit = 29;
	o__1.ofnmlen = 60;
	o__1.ofnm = files + 540;
	o__1.orl = 0;
	o__1.osta = 0;
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr10 = f_open(&o__1);
	*(unsigned char *)ewlon = 'W';
	if (setup_05__1.lonew == 1) {
	    *(unsigned char *)ewlon = 'E';
	}
	if (datacom_m05__1.molahgts == 1) {
	    s_copy(hgtlbl, "HgtMOLA", (ftnlen)7, (ftnlen)7);
	    if (datacom_m05__1.nvarx == 2 || datacom_m05__1.nvary == 2) {
		s_copy(hgtlbl, "HgtSFCM", (ftnlen)7, (ftnlen)7);
	    }
	    s_copy(latlbl, "LatPC", (ftnlen)5, (ftnlen)5);
	} else {
	    s_copy(hgtlbl, "HgtELPS", (ftnlen)7, (ftnlen)7);
	    s_copy(latlbl, "LatPC", (ftnlen)5, (ftnlen)5);
	    if (datacom_m05__1.ipclat != 1) {
		s_copy(hgtlbl, "HgtGRPH", (ftnlen)7, (ftnlen)7);
		s_copy(latlbl, "LatPG", (ftnlen)5, (ftnlen)5);
	    }
	}
	if (datacom_m05__1.logscale == 0) {
	    s_copy(denslbl, " Denkgm3", (ftnlen)8, (ftnlen)8);
	}
	if (datacom_m05__1.logscale == 1) {
	    s_copy(denslbl, " Logkgm3", (ftnlen)8, (ftnlen)8);
	}
	if (datacom_m05__1.logscale == 2) {
	    s_copy(denslbl, " Den%Avg", (ftnlen)8, (ftnlen)8);
	}
	if (datacom_m05__1.logscale == 3) {
	    s_copy(denslbl, "Denkgkm3", (ftnlen)8, (ftnlen)8);
	}
	s_wsfe(&io___56);
	do_fio(&c__1, hgtlbl, (ftnlen)7);
	do_fio(&c__1, latlbl, (ftnlen)5);
	do_fio(&c__1, ewlon, (ftnlen)1);
	do_fio(&c__1, denslbl, (ftnlen)8);
	e_wsfe();
    }
    if (*isload == 0) {
/* ...    Compute character string length of DATADIR path name              SETU690 */
	lendir = i_indx(setup_05__1.datadir, " ", (ftnlen)60, (ftnlen)1) - 1;
	if (lendir < 1 || lendir > 60) {
	    lendir = 60;
	}
/* ...    Unit 9 molatoph.bin file contains MOLA 1/2 degree resolution      SETU693 */
/*       areoid radius and topographic heights above the areoid, versus    SETU694 */
/*       latitude and longitude                                            SETU695 */
	o__1.oerr = 1;
	o__1.ounit = 9;
	o__1.ofnmlen = lendir + 60;
/* Writing concatenation */
	i__2[0] = lendir, a__1[0] = setup_05__1.datadir;
	i__2[1] = 60, a__1[1] = files + 420;
	s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)120);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	*ierr8 = f_open(&o__1);
/* ...    Unit 10 COSPAR2.DAT file contains COSPAR atmosphere data values   SETU698 */
	o__1.oerr = 1;
	o__1.ounit = 10;
	o__1.ofnmlen = lendir + 60;
/* Writing concatenation */
	i__2[0] = lendir, a__1[0] = setup_05__1.datadir;
	i__2[1] = 60, a__1[1] = files + 480;
	s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)120);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr9 = f_open(&o__1);
/* ...    Unit 11 hgtoffst.dat file with MTGCM height offset array          SETU701 */
	o__1.oerr = 1;
	o__1.ounit = 11;
	o__1.ofnmlen = lendir + 60;
/* Writing concatenation */
	i__2[0] = lendir, a__1[0] = setup_05__1.datadir;
	i__2[1] = 60, a__1[1] = files + 600;
	s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)120);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = 0;
	o__1.oblnk = 0;
	*ierr11 = f_open(&o__1);
/* ...    Unit 12 albedo1.bin binary format albedo file                     SETU704 */
	o__1.oerr = 1;
	o__1.ounit = 12;
	o__1.ofnmlen = lendir + 60;
/* Writing concatenation */
	i__2[0] = lendir, a__1[0] = setup_05__1.datadir;
	i__2[1] = 60, a__1[1] = files + 660;
	s_cat(ch__1, a__1, i__2, &c__2, (ftnlen)120);
	o__1.ofnm = ch__1;
	o__1.orl = 0;
	o__1.osta = "old";
	o__1.oacc = 0;
	o__1.ofm = "unformatted";
	o__1.oblnk = 0;
	*ierr12 = f_open(&o__1);
/* ...    Test for file open error condition                                SETU707 */
	for (j = 1; j <= 13; ++j) {
	    if (ierr[j - 1] != 0) {
		io___59.ciunit = datacom_m05__1.iu0;
		s_wsfe(&io___59);
		do_fio(&c__1, (char *)&ierr[j - 1], (ftnlen)sizeof(integer));
		do_fio(&c__1, files + (j - 1) * 60, (ftnlen)60);
		e_wsfe();
		goto L9998;
	    }
/* L40: */
	}
/*  ...................................................................... SETU715 */
/* ...    Read COSPAR2.DAT atmosphere data                                  SETU716 */
	for (i__ = 1; i__ <= 164; ++i__) {
	    i__1 = s_rsle(&io___60);
	    if (i__1 != 0) {
		goto L65;
	    }
	    i__1 = do_lio(&c__5, &c__1, (char *)&cosparnh_m05__1.zc[i__ - 1], 
		    (ftnlen)sizeof(doublereal));
	    if (i__1 != 0) {
		goto L65;
	    }
	    i__1 = do_lio(&c__5, &c__1, (char *)&cosparnh_m05__1.tc[i__ - 1], 
		    (ftnlen)sizeof(doublereal));
	    if (i__1 != 0) {
		goto L65;
	    }
	    i__1 = do_lio(&c__5, &c__1, (char *)&cosparnh_m05__1.pc[i__ - 1], 
		    (ftnlen)sizeof(doublereal));
	    if (i__1 != 0) {
		goto L65;
	    }
	    i__1 = do_lio(&c__5, &c__1, (char *)&cosparnh_m05__1.dc[i__ - 1], 
		    (ftnlen)sizeof(doublereal));
	    if (i__1 != 0) {
		goto L65;
	    }
	    i__1 = e_rsle();
	    if (i__1 != 0) {
		goto L65;
	    }
/* L63: */
	}
	goto L70;
L65:
	s_stop(" Error or EOF on COSPAR2.DAT file!", (ftnlen)34);
/* ...    Read MTGCM and TES yr1 and yr 2 height offsets                    SETU722 */
L70:
	s_rsfe(&io___61);
	do_fio(&c__1, dummy, (ftnlen)1);
	e_rsfe();
	for (i__ = 0; i__ <= 12; ++i__) {
	    i__1 = s_rsle(&io___63);
	    if (i__1 != 0) {
		goto L72;
	    }
	    i__1 = do_lio(&c__3, &c__1, (char *)&ls, (ftnlen)sizeof(integer));
	    if (i__1 != 0) {
		goto L72;
	    }
	    for (j = 1; j <= 3; ++j) {
		i__1 = do_lio(&c__5, &c__1, (char *)&
			tgcmoffset_m05__1.offsets[i__ + j * 13 - 13], (ftnlen)
			sizeof(doublereal));
		if (i__1 != 0) {
		    goto L72;
		}
	    }
	    i__1 = e_rsle();
	    if (i__1 != 0) {
		goto L72;
	    }
	    if (ls == i__ * 30) {
		goto L67;
	    }
L72:
	    s_stop(" Error reading MTGCM height offsets", (ftnlen)35);
L67:
	    ;
	}
	s_rsfe(&io___65);
	do_fio(&c__1, dummy, (ftnlen)1);
	e_rsfe();
	for (i__ = 0; i__ <= 12; ++i__) {
	    i__1 = s_rsle(&io___66);
	    if (i__1 != 0) {
		goto L73;
	    }
	    i__1 = do_lio(&c__3, &c__1, (char *)&ls, (ftnlen)sizeof(integer));
	    if (i__1 != 0) {
		goto L73;
	    }
	    for (j = 1; j <= 2; ++j) {
		i__1 = do_lio(&c__5, &c__1, (char *)&
			tgcmoffset_m05__1.toffsets[i__ + j * 13 - 13], (
			ftnlen)sizeof(doublereal));
		if (i__1 != 0) {
		    goto L73;
		}
	    }
	    i__1 = e_rsle();
	    if (i__1 != 0) {
		goto L73;
	    }
	    if (ls == i__ * 30) {
		goto L68;
	    }
L73:
	    s_stop(" Error reading TES Yr1 and Yr2 height offsets", (ftnlen)
		    45);
L68:
	    ;
	}
/* ...    Read topographic height data file                                 SETU736 */
/*        Write(iu0,*)' Reading MOLA 1/2 degree areoid and topography'      SETU737 */
	s_rsue(&io___67);
	do_uio(&c_b166, (char *)&terhgt_m05__1.areorad[0], (ftnlen)sizeof(
		doublereal));
	do_uio(&c_b166, (char *)&terhgt_m05__1.topomola[0], (ftnlen)sizeof(
		doublereal));
	e_rsue();
/* ...    Read 1 degree resolution albedo file                              SETU739 */
/*        Write(iu0,*)' Reading 1 degree albedo data'                       SETU740 */
	s_rsue(&io___68);
	do_uio(&c__65884, (char *)&terhgt_m05__1.albedo[0], (ftnlen)sizeof(
		doublereal));
	e_rsue();
/*        Write(iu0,*)' Reading Mars GCM surface data files'                SETU742 */
	readsurf_m05__(setup_05__1.gcmdir, "1", (ftnlen)60, (ftnlen)1);
/*        Write(iu0,*)' Reading Mars GCM 0-80 km data files'                SETU744 */
	readmgcm_m05__(setup_05__1.gcmdir, "1", (ftnlen)60, (ftnlen)1);
/*        Write(iu0,*)' Reading Mars TGCM 80-170 km data files'             SETU746 */
	readtgcm_m05__(setup_05__1.gcmdir, "1", (ftnlen)60, (ftnlen)1);
/*        Write(iu0,*)' Reading TES Yr1 & Yr2 MGCM surface data files'      SETU748 */
	rdtessrf_m05__(setup_05__1.gcmdir, "1", (ftnlen)60, (ftnlen)1);
/*        Write(iu0,*)' Reading TES Yr1 & Yr2 MGCM -5 to 80 km data files'  SETU750 */
	rdtesmgcm_m05__(setup_05__1.gcmdir, "1", (ftnlen)60, (ftnlen)1);
/*        Write(iu0,*)' Reading TES Yr1 & Yr2 MTGCM 80-240 km data files'   SETU752 */
	rdtestgcm_m05__(setup_05__1.gcmdir, "1", (ftnlen)60, (ftnlen)1);
	datacom_m05__1.dtr = atan(1.) / 45.;
    }
/* ...  Check date; If error,write message and stop                       SETU755 */
    chkdt_m05__(&setup_05__1.myear, &setup_05__1.month, &setup_05__1.mday, &
	    setup_05__1.ihr, &setup_05__1.imin, &setup_05__1.sec, &idterr);
    if (idterr < -6) {
	io___70.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___70);
	e_wsfe();
    } else if (idterr < 0) {
	io___71.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___71);
	e_wsfe();
    }
    if (idterr < 0) {
	goto L9998;
    }
/* ...  Compute Julian day                                                SETU765 */
    caltojul_m05__(&setup_05__1.myear, &setup_05__1.month, &setup_05__1.mday, 
	    &setup_05__1.ihr, &setup_05__1.imin, &setup_05__1.sec, &
	    datacom_m05__1.day);
    *day0 = datacom_m05__1.day;
    if (datacom_m05__1.iup > 0) {
	if (setup_05__1.iert == 1) {
	    io___72.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___72);
	    e_wsfe();
	} else {
	    io___73.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___73);
	    e_wsfe();
	}
	if (setup_05__1.iutc == 1) {
	    io___74.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___74);
	    e_wsfe();
	} else {
	    io___75.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___75);
	    e_wsfe();
	}
	io___76.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___76);
	do_fio(&c__1, (char *)&setup_05__1.month, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&setup_05__1.mday, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&setup_05__1.myear, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&datacom_m05__1.day, (ftnlen)sizeof(doublereal))
		;
	do_fio(&c__1, (char *)&setup_05__1.ihr, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&setup_05__1.imin, (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&setup_05__1.sec, (ftnlen)sizeof(doublereal));
	e_wsfe();
    }
    datacom_m05__1.dtex = setup_05__1.deltatex;
    if (datacom_m05__1.iup > 0) {
	if (setup_05__1.deltatex != 0.) {
	    io___77.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___77);
	    do_fio(&c__1, (char *)&setup_05__1.deltatex, (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	}
	if (datacom_m05__1.molahgts == 1) {
	    io___78.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___78);
	    e_wsfe();
	} else {
	    if (datacom_m05__1.ipclat == 1) {
		io___79.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___79);
		e_wsfe();
	    } else {
		io___80.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___80);
		e_wsfe();
	    }
	}
	io___81.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___81);
	do_fio(&c__1, (char *)&datacom_m05__1.requa, (ftnlen)sizeof(
		doublereal));
	do_fio(&c__1, (char *)&datacom_m05__1.rpole, (ftnlen)sizeof(
		doublereal));
	e_wsfe();
	io___82.ciunit = datacom_m05__1.iup;
	s_wsfe(&io___82);
	e_wsfe();
    }
/* ...  Sun position in Mars latitude (areocentric latitude) and          SETU813 */
/* ...  longitude. ALS = Ls = areocentric longitude of sun in orbital     SETU814 */
/* ...  position (Ls = 0 at spring equinox). MARSAU = Mars orbital        SETU815 */
/* ...  radius in Astronomical Units                                      SETU816 */
    if (*dradau > 0.) {
	als = *dsunls;
	marsau = *dradau;
	owlt = *dowlt;
    } else {
/* ...    Use built-in Mars ephemeris routine                             SETU822 */
/* ...    Convert to Terrestrial (Dynamical) Time, if necessary           SETU823 */
	ttsec = 0.;
	if (setup_05__1.iutc == 1) {
/* ...      Get terrestrial dynamical time offset (seconds)               SETU826 */
	    dt = (*day0 - 2451545.) / 36525.;
/* ...      Terrestrial time offset (in seconds) TT = UTC + ttsec         SETU828 */
/* Computing 2nd power */
	    d__1 = dt;
	    ttsec = (dt * 95. + 64.184 + d__1 * d__1 * 35.) / 86400.;
	}
	d__1 = *day0 + ttsec;
	marsephm_m05__(&d__1, &sunlat, &sunlon, &als, &marsau, &owlt, &eot);
/* ...    Convert to Mars-Event Time, if necessary                        SETU832 */
	if (setup_05__1.iert == 1) {
	    d__1 = *day0 + ttsec - owlt / 6050.;
	    marsephm_m05__(&d__1, &sunlat, &sunlon, &als, &marsau, &owlt, &
		    eot);
	}
/* ...    Convert planetographic sun latitude to planetocentric           SETU835 */
	sunlat = atan(tan(sunlat * datacom_m05__1.dtr) / 1.0104941290313674) /
		 datacom_m05__1.dtr;
    }
    if (datacom_m05__1.als0 < 120. || datacom_m05__1.als0 > 320.) {
	if (datacom_m05__1.als0 > 0.) {
	    io___91.ciunit = datacom_m05__1.iu0;
	    s_wsle(&io___91);
	    do_lio(&c__9, &c__1, " ** Ls0 outside range. No dust storm assum\
ed.", (ftnlen)45);
	    e_wsle();
	    if (datacom_m05__1.iup > 0) {
		io___92.ciunit = datacom_m05__1.iup;
		s_wsle(&io___92);
		do_lio(&c__9, &c__1, " ** Ls0 outside range. ", (ftnlen)23);
		do_lio(&c__9, &c__1, "No dust storm assumed.", (ftnlen)22);
		e_wsle();
	    }
	}
	datacom_m05__1.als0 = -999.;
	datacom_m05__1.intens = 0.;
    } else {
	if (datacom_m05__1.intens < 0. || datacom_m05__1.intens > 3.) {
	    io___93.ciunit = datacom_m05__1.iu0;
	    s_wsle(&io___93);
	    do_lio(&c__9, &c__1, " Intensity must be between 0 and 3", (
		    ftnlen)34);
	    e_wsle();
	    goto L9998;
	}
	if (datacom_m05__1.radmax <= 0. || datacom_m05__1.radmax > 1e4) {
	    datacom_m05__1.radmax = 0.;
	}
    }
/* ...  Set ALSDUR within allowable range                                 SETU853 */
    if (datacom_m05__1.alsdur < 12.) {
	datacom_m05__1.alsdur = 12.;
    }
    if (datacom_m05__1.alsdur > 48.) {
	datacom_m05__1.alsdur = 48.;
    }
    if (therm_m05__1.f107 < 50. || therm_m05__1.f107 > 450.) {
	io___94.ciunit = datacom_m05__1.iu0;
	s_wsle(&io___94);
	do_lio(&c__9, &c__1, " F10.7 must be between 50 and 450", (ftnlen)33);
	e_wsle();
	goto L9998;
    }
    if (therm_m05__1.stdl < -3. || therm_m05__1.stdl > 3.) {
	io___95.ciunit = datacom_m05__1.iu0;
	s_wsle(&io___95);
	do_lio(&c__9, &c__1, " Std. deviations must be between -3 and +3", (
		ftnlen)42);
	e_wsle();
	goto L9998;
    }
    if (setup_05__1.nr1 <= 0 || setup_05__1.nr1 >= 30000) {
	io___96.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___96);
	e_wsfe();
	goto L9998;
    }
    randcom_m05__1.ix = setup_05__1.nr1;
    randcom_m05__1.iy = randcom_m05__1.ix % 176 * 172 - randcom_m05__1.ix / 
	    176 * 35;
    randcom_m05__1.iz = randcom_m05__1.ix % 178 * 170 - randcom_m05__1.ix / 
	    178 * 63;
    if (randcom_m05__1.iy < 0) {
	randcom_m05__1.iy += 30307;
    }
    if (randcom_m05__1.iz < 0) {
	randcom_m05__1.iz += 30323;
    }
    z1 = random_m05__(&l);
    *rhod = ppnd_m05__(&z1, &l);
    z1 = random_m05__(&l);
    *rhou = ppnd_m05__(&z1, &l);
    z1 = random_m05__(&l);
    *rhov = ppnd_m05__(&z1, &l);
    z1 = random_m05__(&l);
    *rhow = ppnd_m05__(&z1, &l);
    if (l == 1) {
	io___99.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___99);
	e_wsfe();
	goto L9998;
    }
    if (s_cmp(filename_m05__1.lstfl, "CON", (ftnlen)60, (ftnlen)3) != 0 && 
	    s_cmp(filename_m05__1.lstfl, "con", (ftnlen)60, (ftnlen)3) != 0 &&
	     datacom_m05__1.iup > 0) {
	if (datacom_m05__1.als0 > 0.) {
	    if (datacom_m05__1.radmax != 0.) {
		io___100.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___100);
		do_fio(&c__1, (char *)&datacom_m05__1.als0, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&datacom_m05__1.intens, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&datacom_m05__1.alsdur, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&datacom_m05__1.radmax, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&datacom_m05__1.dustlat, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&datacom_m05__1.dustlon, (ftnlen)sizeof(
			doublereal));
		d__1 = 360. - datacom_m05__1.dustlon;
		do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
		e_wsfe();
	    } else {
		io___101.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___101);
		do_fio(&c__1, (char *)&datacom_m05__1.als0, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&datacom_m05__1.intens, (ftnlen)sizeof(
			doublereal));
		do_fio(&c__1, (char *)&datacom_m05__1.alsdur, (ftnlen)sizeof(
			doublereal));
		e_wsfe();
	    }
	}
	if (datacom_m05__1.iup > 0) {
	    io___102.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___102);
	    do_fio(&c__1, (char *)&therm_m05__1.f107, (ftnlen)sizeof(
		    doublereal));
/* Computing 2nd power */
	    d__2 = marsau;
	    d__1 = therm_m05__1.f107 / (d__2 * d__2);
	    do_fio(&c__1, (char *)&d__1, (ftnlen)sizeof(doublereal));
	    do_fio(&c__1, (char *)&therm_m05__1.stdl, (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	    if (datacom_m05__1.mapyear == 0) {
		io___103.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___103);
		e_wsfe();
	    } else {
		io___104.ciunit = datacom_m05__1.iup;
		s_wsfe(&io___104);
		do_fio(&c__1, (char *)&datacom_m05__1.mapyear, (ftnlen)sizeof(
			integer));
		e_wsfe();
	    }
	    io___105.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___105);
	    do_fio(&c__1, (char *)&datacom_m05__1.dustnu, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.dustdiam, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.dustdens, (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	}
	if (datacom_m05__1.iup > 0) {
	    io___106.ciunit = datacom_m05__1.iup;
	    s_wsfe(&io___106);
	    do_fio(&c__1, (char *)&setup_05__1.nr1, (ftnlen)sizeof(integer));
	    do_fio(&c__1, (char *)&setup_05__1.rpscale, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&setup_05__1.corlmin, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.rwscale, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.wlscale, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.wmscale, (ftnlen)sizeof(
		    doublereal));
	    do_fio(&c__1, (char *)&datacom_m05__1.blwinfac, (ftnlen)sizeof(
		    doublereal));
	    e_wsfe();
	}
    }
/* L380: */
    if (datacom_m05__1.nvarx < 1 || datacom_m05__1.nvarx > 15) {
	io___107.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___107);
	e_wsfe();
	io___108.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___108);
	e_wsfe();
	goto L9998;
    }
    if (datacom_m05__1.nvary < 0 || datacom_m05__1.nvary > 15) {
	io___109.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___109);
	e_wsfe();
	io___110.ciunit = datacom_m05__1.iu0;
	s_wsfe(&io___110);
	e_wsfe();
	goto L9998;
    }
    if (datacom_m05__1.logscale < 0 || datacom_m05__1.logscale > 3) {
	datacom_m05__1.logscale = 0;
    }
/* ...  Initialize position data                                          SETU959 */
    *chgt = setup_05__1.fhgt;
    *clat = setup_05__1.flat;
    *clon = setup_05__1.flon;
/*     CSEC = 0.0d0                                                      SETU963 */
/*      Write(iu0,*)' Finished Setup_M05 - Starting computations'         SETU964 */
    return 0;
L9998:
    s_stop(" Error termination! Check the LIST file for messages.", (ftnlen)
	    53);
    return 0;
} /* setup_m05__ */

#undef ierr13
#undef ierr12
#undef ierr11
#undef ierr10
#undef ierr9
#undef ierr8
#undef ierr7
#undef ierr6
#undef ierr5
#undef ierr4
#undef ierr3
#undef ierr2
#undef ierr1
#undef ierr


/* ---------------------------------------------------------------------- SETU968 */
/* Subroutine */ int randinit_m05__(integer *j, integer *nr1, doublereal *
	rhod, doublereal *rhou, doublereal *rhov, doublereal *rhow, integer *
	iup, integer *iustdout)
{
    /* Format strings */
    static char fmt_10[] = "(\002   Random seed number\002,i6,\002 =\002,i6)";

    /* Builtin functions */
    integer s_wsfe(cilist *), do_fio(integer *, char *, ftnlen), e_wsfe();

    /* Local variables */
    static integer l;
    extern doublereal random_m05__(integer *), ppnd_m05__(doublereal *, 
	    integer *);

    /* Fortran I/O blocks */
    static cilist io___111 = { 0, 0, 0, fmt_10, 0 };
    static cilist io___112 = { 0, 0, 0, fmt_10, 0 };


/* ...  Re-initialize NR1, e.g. by reading from a file, or some algorithm RNDI  4 */
/*     from previous NR1 value, or by some computation on index J.       RNDI  5 */
/*     Note that it is not necessary to randomly select each seed value  RNDI  6 */
/*     NR1 in order to get a random sequence of output.  Any regular     RNDI  7 */
/*     progression of selected NR1 values will do for this process.      RNDI  8 */
    *nr1 += 11;
    if (*nr1 > 30000) {
	*nr1 %= 30000;
    }
/* ...  Write random seed value to list file                              RNDI 11 */
    if (*iup != 0) {
	io___111.ciunit = *iup;
	s_wsfe(&io___111);
	do_fio(&c__1, (char *)&(*j), (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&(*nr1), (ftnlen)sizeof(integer));
	e_wsfe();
    } else {
	io___112.ciunit = *iustdout;
	s_wsfe(&io___112);
	do_fio(&c__1, (char *)&(*j), (ftnlen)sizeof(integer));
	do_fio(&c__1, (char *)&(*nr1), (ftnlen)sizeof(integer));
	e_wsfe();
    }
    randcom_m05__1.ix = *nr1;
    randcom_m05__1.iy = randcom_m05__1.ix % 176 * 172 - randcom_m05__1.ix / 
	    176 * 35;
    randcom_m05__1.iz = randcom_m05__1.ix % 178 * 170 - randcom_m05__1.ix / 
	    178 * 63;
    if (randcom_m05__1.iy < 0) {
	randcom_m05__1.iy += 30307;
    }
    if (randcom_m05__1.iz < 0) {
	randcom_m05__1.iz += 30323;
    }
    *rhod = random_m05__(&l);
    *rhod = ppnd_m05__(rhod, &l);
    *rhou = random_m05__(&l);
    *rhou = ppnd_m05__(rhou, &l);
    *rhov = random_m05__(&l);
    *rhov = ppnd_m05__(rhov, &l);
    *rhow = random_m05__(&l);
    *rhow = ppnd_m05__(rhow, &l);
    return 0;
} /* randinit_m05__ */

/* ---------------------------------------------------------------------- RNDI 33 */
/* Subroutine */ int chkdt_m05__(integer *myear, integer *month, integer *
	iday, integer *ihour, integer *minutes, doublereal *sec, integer *err)
{
    /* Builtin functions */
    integer s_wsle(cilist *), do_lio(integer *, integer *, char *, ftnlen), 
	    e_wsle();

    /* Local variables */
    static logical leapyear, centyear;

    /* Fortran I/O blocks */
    static cilist io___116 = { 0, 6, 0, 0, 0 };


/*                                                                       CKDT  2 */
/*      CHecKs input Date and Time for validity and internal             CKDT  3 */
/*      consistency.  Returns error message(s) and prompts               CKDT  4 */
/*      user to re-enter inputs.                                         CKDT  5 */
/*                                                                       CKDT  6 */
    *err = 0;
    centyear = FALSE_;
    leapyear = FALSE_;
/* ...   Convert to 4-digit year, if necessary                            CKDT 14 */
    if (*myear >= 0 && *myear <= 69) {
	*myear += 2000;
    }
    if (*myear >= 70 && *myear <= 99) {
	*myear += 1900;
    }
    if (*myear < 1970 || *myear > 2069) {
	s_wsle(&io___116);
	do_lio(&c__9, &c__1, " Year must be 1970-2069", (ftnlen)23);
	e_wsle();
	*err = -1;
    }
    if (*myear % 100 == 0) {
	centyear = TRUE_;
    }
    if (*myear % 4 == 0) {
	leapyear = TRUE_;
    }
    if (centyear) {
	if (*myear % 400 != 0) {
	    leapyear = FALSE_;
	}
    }
    if (*month < 1 || *month > 12) {
/* ...     Write(*,*)' Month must be 1-12'                                CKDT 27 */
	*err = -2;
    }
    if (*month == 4 || *month == 6 || *month == 9 || *month == 11) {
	if (*iday < 1 || *iday > 30) {
/* ...       Write(*,*)' Day of month must be 1-30'                       CKDT 32 */
	    *err = -3;
	}
    } else if (*month == 2) {
	if (leapyear) {
	    if (*iday < 1 || *iday > 29) {
/* ...         Write(*,*)' Day of month must be 1-29 (Leap Year)'         CKDT 38 */
		*err = -4;
	    }
	} else if (*iday < 1 || *iday > 28) {
/* ...       Write(*,*)' Day of month must be 1-28 (Non-Leap Year)'       CKDT 42 */
	    *err = -5;
	}
    } else if (*iday < 1 || *iday > 31) {
/* ...       Write(*,*)' Day of month must be 1-31'                       CKDT 46 */
	*err = -6;
    }
    if (*ihour < 0 || *ihour > 24) {
/* ...     Write(*,*)' Hour must be 0-24'                                 CKDT 50 */
	*err = -7;
    }
    if (*ihour == 24 && (*minutes != 0 || *sec != 0.)) {
/* ...     Write(*,*)' Hour must be 23 or Time must be 24:00:00'          CKDT 54 */
	*err = -7;
    }
    if (*minutes < 0 || *minutes > 60) {
/* ...     Write(*,*)' Minutes must be 0-60'                              CKDT 58 */
	*err = -8;
    }
    if (*minutes == 60 && *sec != 0.) {
/* ...     Write(*,*)' Minutes must be 59 or Seconds must be 0'           CKDT 62 */
	*err = -8;
    }
    if (*sec < 0. || *sec > 60.) {
/* ...     Write(*,*)' Seconds must be 0.0-60.0'                          CKDT 66 */
	*err = -9;
    }
    return 0;
} /* chkdt_m05__ */

#ifdef __cplusplus
	}
#endif
