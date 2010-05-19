//$Id$
//------------------------------------------------------------------------------
//                                  SolarSystem
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/mm/dd
//
/**
 * This is the implementation of the SolarSystem class.
 *
 * @note It is assumed that no classes will be derived from this one.
 */
//------------------------------------------------------------------------------
#include <list>
#include <string>
#include "SolarSystem.hpp"              // class's header file
#include "SolarSystemException.hpp"
#include "UtilityException.hpp"
#include "CelestialBody.hpp"
#include "Star.hpp"
#include "Planet.hpp"
#include "Moon.hpp"
#include "StringUtil.hpp"               // for ToString()
#include "FileManager.hpp"              // for GetFullPathname()
#include "MessageInterface.hpp"         // for debugging
#include "CoordinateSystem.hpp"

//#define DEBUG_SS_COPY
//#define DEBUG_SS_CLONING
//#define DEBUG_SS_PLANETARY_FILE
//#define DEBUG_SS_FIND_BODY
//#define DEBUG_SS_ADD_BODY
//#define SS_CONSTRUCT_DESTRUCT
//#define DEBUG_SS_SPICE
//#define DEBUG_SS_CLOAKING
//#define DEBUG_SS_PARAM_EQUAL

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif
#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif

//---------------------------------
// static data
//---------------------------------
const std::string
SolarSystem::PARAMETER_TEXT[SolarSystemParamCount - GmatBaseParamCount] =
{
   "BodiesInUse",
   "NumberOfBodies",
   "Ephemeris",            // deprecated!!!!
   "EphemerisSource",
   "DEFilename",
   "SPKFilename",
   "LSKFilename",
   "UseTTForEphemeris",
   "EphemerisUpdateInterval",
};

const Gmat::ParameterType
SolarSystem::PARAMETER_TYPE[SolarSystemParamCount - GmatBaseParamCount] =
{
   Gmat::STRINGARRAY_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::STRINGARRAY_TYPE,            // deprecated!!!!
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::STRING_TYPE,
   Gmat::BOOLEAN_TYPE,
   Gmat::REAL_TYPE,
};



// define default names form solar system bodies
const std::string SolarSystem::SUN_NAME        = "Sun";

const std::string SolarSystem::MERCURY_NAME    = "Mercury";

const std::string SolarSystem::VENUS_NAME      = "Venus";

const std::string SolarSystem::EARTH_NAME      = "Earth";
const std::string SolarSystem::MOON_NAME       = "Luna";

const std::string SolarSystem::MARS_NAME       = "Mars";
const std::string SolarSystem::PHOBOS_NAME     = "Phobos";
const std::string SolarSystem::DEIMOS_NAME     = "Deimos";

const std::string SolarSystem::JUPITER_NAME    = "Jupiter";
const std::string SolarSystem::METIS_NAME      = "Metis";
const std::string SolarSystem::ADRASTEA_NAME   = "Adrastea";
const std::string SolarSystem::AMALTHEA_NAME   = "Amalthea";
const std::string SolarSystem::THEBE_NAME      = "Thebe";
const std::string SolarSystem::IO_NAME         = "Io";
const std::string SolarSystem::EUROPA_NAME     = "Europa";
const std::string SolarSystem::GANYMEDE_NAME   = "Ganymede";
const std::string SolarSystem::CALLISTO_NAME   = "Callisto";

const std::string SolarSystem::SATURN_NAME     = "Saturn";
const std::string SolarSystem::PAN_NAME        = "Pan";
const std::string SolarSystem::ATLAS_NAME      = "Atlas";
const std::string SolarSystem::PROMETHEUS_NAME = "Promethus";
const std::string SolarSystem::PANDORA_NAME    = "Pandora";
const std::string SolarSystem::EPIMETHEUS_NAME = "Epimetheus";
const std::string SolarSystem::JANUS_NAME      = "Janus";
const std::string SolarSystem::MIMAS_NAME      = "Mimas";
const std::string SolarSystem::ENCELADUS_NAME  = "Enceladus";
const std::string SolarSystem::TETHYS_NAME     = "Tethys";
const std::string SolarSystem::TELESTO_NAME    = "Telesto";
const std::string SolarSystem::CALYPSO_NAME    = "Calypso";
const std::string SolarSystem::DIONE_NAME      = "Dione";
const std::string SolarSystem::HELENE_NAME     = "Helene";
const std::string SolarSystem::RHEA_NAME       = "Rhea";
const std::string SolarSystem::TITAN_NAME      = "Titan";
const std::string SolarSystem::IAPETUS_NAME    = "Iapetus";
const std::string SolarSystem::PHOEBE_NAME     = "Phoebe";

const std::string SolarSystem::URANUS_NAME     = "Uranus";
const std::string SolarSystem::CORDELIA_NAME   = "Cordelia";
const std::string SolarSystem::OPHELIA_NAME    = "Ophelia";
const std::string SolarSystem::BIANCA_NAME     = "Bianca";
const std::string SolarSystem::CRESSIDA_NAME   = "Cressida";
const std::string SolarSystem::DESDEMONA_NAME  = "Desdemona";
const std::string SolarSystem::JULIET_NAME     = "Juliet";
const std::string SolarSystem::PORTIA_NAME     = "Portia";
const std::string SolarSystem::ROSALIND_NAME   = "Rosalind";
const std::string SolarSystem::BELINDA_NAME    = "Belinda";
const std::string SolarSystem::PUCK_NAME       = "Puck";
const std::string SolarSystem::MIRANDA_NAME    = "Miranda";
const std::string SolarSystem::ARIEL_NAME      = "Ariel";
const std::string SolarSystem::UMBRIEL_NAME    = "Umbriel";
const std::string SolarSystem::TITANIA_NAME    = "Titania";
const std::string SolarSystem::OBERON_NAME     = "Oberon";

const std::string SolarSystem::NEPTUNE_NAME    = "Neptune";
const std::string SolarSystem::NAIAD_NAME      = "Naiad";
const std::string SolarSystem::THALASSA_NAME   = "Thalassa";
const std::string SolarSystem::DESPINA_NAME    = "Despina";
const std::string SolarSystem::GALATEA_NAME    = "Galatea";
const std::string SolarSystem::LARISSA_NAME    = "Larissa";
const std::string SolarSystem::PROTEUS_NAME    = "Proteus";
const std::string SolarSystem::TRITON_NAME     = "Triton";

const std::string SolarSystem::PLUTO_NAME      = "Pluto";
const std::string SolarSystem::CHARON_NAME     = "Charon";


// default values for Planet data ------------------------ planets ------------------------
const std::string
SolarSystem::PLANET_NAMES[NumberOfDefaultPlanets] =
{
   SolarSystem::MERCURY_NAME,
   SolarSystem::VENUS_NAME,
   SolarSystem::EARTH_NAME,
   SolarSystem::MARS_NAME,
   SolarSystem::JUPITER_NAME,
   SolarSystem::SATURN_NAME,
   SolarSystem::URANUS_NAME,
   SolarSystem::NEPTUNE_NAME,
   SolarSystem::PLUTO_NAME,
};

const Gmat::PosVelSource    SolarSystem::PLANET_POS_VEL_SOURCE    = Gmat::DE405;
//const Gmat::AnalyticMethod  Planet::DEFAULT_ANALYTIC_METHOD   = Gmat::LOW_FIDELITY;

// Units for Equatorial radius are km
const Real                  SolarSystem::PLANET_EQUATORIAL_RADIUS[NumberOfDefaultPlanets]         =
{
   2.43970000000000e+003, // to match STK 2006.01.31 - was 2439.7,
   6.05190000000000e+003, // match to STK 2006.01.31 - was 6051.8,
   6.3781363E3, // to match STK 2006.01.31 - was 6378.1363,
   3.39700000000000e+003, // to match STK 2006.01.31 - was 3396.200,
   7.14920000000000e+004, // to match STK 2006.01.31 - was 71492.00,
   6.02680000000000e+004, // to match STK 2006.01.31 - was 60368.0,
   2.55590000000000e+004, // to match STK 2006.01.31 - was 25559.0,
   2.52690000000000e+004, // to match STK 2006.01.31 - was 24764.0,
   1162.0 // changed to match with STK. old:1195.0
};
const Real                  SolarSystem::PLANET_FLATTENING[NumberOfDefaultPlanets]         =
{
   0.0,
   0.0,
   0.00335270, // match to STK 2006.01.31 - was 0.0033528,
   0.00647630, // match to STK 2006.01.31 - was 0.0064763,
   0.06487439, // match to STK 2006.01.31 - was 0.0648744,
   0.09796243, // match to STK 2006.01.31 - was 0.0979624,
   0.02292734, // match to STK 2006.01.31 - was 0.0229273,
   0.01856029, // match to STK 2006.01.31 - was 0.0171,
   0.0
};
// Units for Mu are km^3/s^2
const Real                  SolarSystem::PLANET_MU[NumberOfDefaultPlanets]                        =
{
   22032.080486418,
   324858.59882646,
   398600.4415,
   42828.314258067,
   126712767.85780,
   37940626.061137,
   5794549.0070719,
   6836534.0638793,
   981.60088770700
};
const Integer               SolarSystem::PLANET_ORDER[NumberOfDefaultPlanets]               =
                            {0, 0, 4, 0, 0, 0, 0, 0, 0};

const Integer               SolarSystem::PLANET_DEGREE[NumberOfDefaultPlanets]              =
                            {0, 0, 4, 0, 0, 0, 0, 0, 0};
const Rmatrix               SolarSystem::PLANET_SIJ[NumberOfDefaultPlanets]                 =
{
   Rmatrix(5,5,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0),
   Rmatrix(5,5,
           0.0,                  0.0,                  0.0,
           0.0,                  0.0,
           0.0,                  0.0,                  0.0,
           0.0,                  0.0,
           0.0, 1.47467423600000E-08,-9.53141845209000E-08,
           0.0,                  0.0,
           0.0, 5.40176936891000E-07, 8.11618282044000E-07,
           2.11451354723000E-07, 0.0,
           0.0, 4.91465604098000E-07, 4.83653955909000E-07,
          -1.18564194898000E-07,      1.37586364127000E-06),
   Rmatrix(5,5,
           0.0,                  0.0,                  0.0,
           0.0,                  0.0,
           0.0,                  0.0,                  0.0,
           0.0,                  0.0,
           0.0, 1.19528010000000E-09,-1.40026639758800E-06,
           0.0,                  0.0,
           0.0, 2.48130798255610E-07,-6.18922846478490E-07,
           1.41420398473540E-06, 0.0,
           0.0,-4.73772370615970E-07, 6.62571345942680E-07,
          -2.00987354847310E-07,      3.08848036903550E-07),
   Rmatrix(5,5,
           0.0,                  0.0,                  0.0,
           0.0,                  0.0,
           0.0,                  0.0,                  0.0,
           0.0,                  0.0,
           0.0, 6.54655690000000E-09, 4.90611750000000E-05,
           0.0,                  0.0,
           0.0, 2.52926620000000E-05, 8.31603630000000E-06,
           2.55554990000000E-05, 0.0,
           0.0, 3.70906170000000E-06,-8.97764090000000E-06,
          -1.72832060000000E-07,     -1.28554120000000E-05),
   Rmatrix(5,5,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0),
   Rmatrix(5,5,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0),
   Rmatrix(5,5,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0),
   Rmatrix(5,5,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0),
   Rmatrix(5,5,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0),
};
const Rmatrix               SolarSystem::PLANET_CIJ[NumberOfDefaultPlanets]                 =
{
   Rmatrix(5,5,
                             1.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
           -2.68328157300000E-05, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0),
   Rmatrix(5,5,
                             1.0,                  0.0,                  0.0,
                             0.0,                  0.0,
                             0.0,                  0.0,                  0.0,
                             0.0,                  0.0,
           -1.95847963769000E-06, 2.87322988834000E-08, 8.52182522007000E-07,
                             0.0,                  0.0,
            7.98507258430000E-07, 2.34759127059000E-06,-9.45132723095000E-09,
           -1.84756674598000E-07,                  0.0,
            7.15385582249000E-07,-4.58736811774000E-07, 1.26875441402000E-07,
           -1.74034531883000E-07, 1.78438307106000E-07),
   Rmatrix(5,5,
                             1.0,                  0.0,                  0.0,
                             0.0,                   0.0,
                             0.0,                  0.0,                  0.0,
                             0.0,                   0.0,
           -4.84165374886470E-04,-1.86987640000000E-10, 2.43926074865630E-06,
                             0.0,                   0.0,
            9.57170590888000E-07, 2.03013720555300E-06, 9.04706341272910E-07,
            7.21144939823090E-07,                   0.0,
            5.39777068357300E-07,-5.36243554298510E-07, 3.50670156459380E-07,
            9.90868905774410E-07,-1.88481367425270E-07),
   Rmatrix(5,5,
                             1.0,                  0.0,                  0.0,
                             0.0,                   0.0,
                             0.0,                  0.0,                  0.0,
                             0.0,                   0.0,
           -8.75977040000000E-04, 3.69395770000000E-09,-8.46829230000000E-05,
                             0.0,                   0.0,
           -1.19062310000000E-05, 3.74737410000000E-06,-1.59844090000000E-05,
            3.51325710000000E-05,                   0.0,
            5.14919500000000E-06, 4.26122630000000E-06,-1.05467200000000E-06,
            6.47421510000000E-06, 2.97350070000000E-07),
   Rmatrix(5,5,
                             1.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
           -6.59640053360000E-03, 0.0,             0.0,             0.0,
                             0.0,
            2.19219394350000E-04, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0),
   Rmatrix(5,5,
                             1.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
           -7.35666364600000E-03, 0.0,             0.0,             0.0,
                             0.0,
           -3.77964473010000E-04, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0),
   Rmatrix(5,5,
                             1.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
           -5.36656314600000E-03, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0),
   Rmatrix(5,5,
                             1.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
           -1.78885438200000E-03, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0,
                             0.0, 0.0,             0.0,             0.0,
                             0.0),
   Rmatrix(5,5,
           1.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0,
           0.0, 0.0,             0.0,             0.0,             0.0),
};
const Real                  SolarSystem::PLANET_TWO_BODY_EPOCH[NumberOfDefaultPlanets]      =
{
   21544.500370768266, 21544.500370768266, 21544.500370768266,
   21544.500370768266, 21544.500370768266, 21544.500370768266,
   21544.500370768266, 21544.500370768266, 21544.500370768266
};
const Rvector6              SolarSystem::PLANET_TWO_BODY_ELEMENTS[NumberOfDefaultPlanets]   =
{
   Rvector6(57909212.938567216, 0.20562729774965544, 28.551674963293556,
            10.99100758149257, 67.548689584103984,  175.10396761800456),
   Rvector6(108208423.76486244, 0.0067572911404369688, 24.433051334216176,
            8.007373221205856, 124.55871063212626,     49.889845117140576),
   Rvector6(149653978.9783766,        0.01704556707314489, 23.439034090426388,
            0.00018646554487906264, 101.7416388084352,    358.12708491129),
   Rvector6(227939100.16983532,   0.093314935483163344, 24.677089965042784,
            3.3736838414054472, 333.01849018562076,     23.020633424007744),
   Rvector6(779362950.5867208, 0.049715759324379896, 23.235170252934984,
            3.253166212922,   12.959463238924978,    20.296667207322848),
   Rvector6(1433895241.1645338,  0.055944006117351672, 22.551333377462712,
            5.9451029086964872, 83.977808941927856,   316.23400767222348),
   Rvector6(2876804054.239868,   0.044369079419761096, 23.663364175915172,
            1.850441916938424, 168.86875273062818,    145.8502865552013),
   Rvector6(4503691751.2342816, 0.011211871260687014, 22.29780590076114,
            3.47555654789392,  33.957145210261132,   266.76236610390636),
   Rvector6(5909627293.567856,    0.24928777871911536, 23.4740184346088,
            43.998303104440304, 183.03164997859696,    25.513664216653164)
};

const Integer               SolarSystem::PLANET_NUM_GRAVITY_MODELS[NumberOfDefaultPlanets] =
   {  1,  2,  3,  1,  1,  1,  1,  1,  0 };
const Integer               SolarSystem::PLANET_NUM_ATMOSPHERE_MODELS[NumberOfDefaultPlanets] =
   {  0,  0,  3,  0,  0,  0,  0,  0,  0};
const Integer               SolarSystem::PLANET_NUM_MAGNETIC_MODELS[NumberOfDefaultPlanets]=
   {  0,  0,  0,  0,  0,  0,  0,  0,  0};
const Integer               SolarSystem::PLANET_NUM_SHAPE_MODELS[NumberOfDefaultPlanets] =
   {  0,  0,  0,  0,  0,  0,  0,  0,  0};

const std::string           SolarSystem::PLANET_GRAVITY_MODELS[] =
{
   "mercury2x0",
   "mgn75hsaap", "shg120",
   "JGM2",       "JGM3",       "EGM96",
   "GMM-1",
   "jupiter6x2",
   "saturn8x0",
   "uranus4x0",
   "neptune4x0",
   // none for Pluto at this time
};

const std::string           SolarSystem::PLANET_ATMOSPHERE_MODELS[] =
{
   // none for Mercury at this time
   // none for Venus at this time
   "Exponential", "MSISE90",  "Jacchia-Roberts",
   // none for Mars at this time
   // none for Jupiter at this time
   // none for Saturn at this time
   // none for Uranus at this time
   // none for Neptune at this time
   // none for Pluto at this time
};

const std::string           SolarSystem::PLANET_MAGNETIC_MODELS[] =
{
   "None"
   // none for Mercury at this time
   // none for Venus at this time
   // none for Earth at this time
   // none for Mars at this time
   // none for Jupiter at this time
   // none for Saturn at this time
   // none for Uranus at this time
   // none for Neptune at this time
   // none for Pluto at this time
};

const std::string          SolarSystem::PLANET_SHAPE_MODELS[] =
{
   "None"
   // none for Mercury at this time
   // none for Venus at this time
   // none for Earth at this time
   // none for Mars at this time
   // none for Jupiter at this time
   // none for Saturn at this time
   // none for Uranus at this time
   // none for Neptune at this time
   // none for Pluto at this time
};

//         SpinAxisRA  Rate        SpinAxisDEC  Rate        Rotation    Rate
const Rvector6              SolarSystem::PLANET_ORIENTATION_PARAMETERS[NumberOfDefaultPlanets]   =
{
   Rvector6(281.01,    -0.033,      61.45,    -0.005,        329.548,    6.1385025),
   Rvector6(272.76,     0.0   ,     67.16,     0.0,          160.20,    -1.4813688),
   Rvector6(  0.0,     -0.641,      90.00,    -0.557,        190.147,  360.9856235),
   Rvector6(317.68143, -0.1061,     52.88650, -0.0609,       176.630,  350.89198226),
   Rvector6(268.05,    -0.009,      64.49,     0.003,        284.95,   870.5366420),
   Rvector6( 40.589,   -0.036,      83.537,   -0.004,         38.90,   810.7939024),

   Rvector6(257.311,    0.0,       -15.175,    0.0,          203.81,  -501.1600928),
   Rvector6(299.36,     0.70,       43.46,    -0.51,         253.18,   536.3128492), // Neptune needs more, though
   Rvector6(313.02,     0.0,         9.09,     0.0,          236.77,   -56.3623195)
};

const Integer               SolarSystem::PLANET_NAIF_IDS[NumberOfDefaultPlanets] =
{
      199,
      299,
      399,
      499,
      599,
      699,
      799,
      899,
      999,
};

// default values for Planet data ------------------------  moons  ------------------------
const std::string           SolarSystem::MOON_NAMES[NumberOfDefaultMoons] =
{
   SolarSystem::MOON_NAME,
//   SolarSystem::PHOBOS_NAME,
//   SolarSystem::DEIMOS_NAME,
};

const std::string           SolarSystem::MOON_CENTRAL_BODIES[NumberOfDefaultMoons] =
{
   SolarSystem::EARTH_NAME,
//   SolarSystem::MARS_NAME,
//   SolarSystem::MARS_NAME,
};

const Gmat::PosVelSource    SolarSystem::MOON_POS_VEL_SOURCE[NumberOfDefaultMoons]    =
{
         Gmat::DE405,
//         Gmat::TWO_BODY_PROPAGATION,
//         Gmat::TWO_BODY_PROPAGATION,
//       Gmat::TWO_BODY_PROPAGATION,
};
//const Gmat::AnalyticMethod  Planet::DEFAULT_ANALYTIC_METHOD   = Gmat::LOW_FIDELITY;

// Units for Equatorial radius are km
const Real                  SolarSystem::MOON_EQUATORIAL_RADIUS[NumberOfDefaultMoons]         =
{
   1738.2000,
//   11.1,       // *** TEMPORARY ***
//   11.1,       // *** TEMPORARY ***
};
const Real                  SolarSystem::MOON_FLATTENING[NumberOfDefaultMoons]         =
{
   0.0,
//   0.0,
//   0.0,
};
// Units for Mu are km^3/s^2
const Real                  SolarSystem::MOON_MU[NumberOfDefaultMoons]                        =
{
   4902.8005821478,
//   7.22e-4,        // *** TEMPORARY ***
//   7.22e-4,        // *** TEMPORARY ***
};
const Integer               SolarSystem::MOON_ORDER[NumberOfDefaultMoons]               =
{
   0,
//   0,   // *** TEMPORARY ***
//   0,   // *** TEMPORARY ***
};

const Integer               SolarSystem::MOON_DEGREE[NumberOfDefaultMoons]              =
{
   0,
//   0, // *** TEMPORARY ***
//   0, // *** TEMPORARY ***
};
const Rmatrix               SolarSystem::MOON_SIJ[NumberOfDefaultMoons]                 =
{
   Rmatrix(5,5,
         0.0,                  0.0,                  0.0,                  0.0,
         0.0,
         0.0,                  0.0,                  0.0,                  0.0,
         0.0,
         0.0, 4.78976286742000E-09, 1.19043314469000E-08,                  0.0,
         0.0,
         0.0, 5.46564929895000E-06, 4.88875341590000E-06,-1.76416063010000E-06,
         0.0,
         0.0, 1.63304293851000E-06,-6.76012176494000E-06,-1.34287028168000E-05,
         3.94334642990000E-06),
//   Rmatrix(5,5,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0),
//   Rmatrix(5,5,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0),
};
const Rmatrix               SolarSystem::MOON_CIJ[NumberOfDefaultMoons]                 =
{
   Rmatrix(5,5,
         1.0,                 0.0,                  0.0,
         0.0,                 0.0,
         0.0,                 0.0,                  0.0,
         0.0,                 0.0,
-9.09314486280000E-05, 9.88441569067000E-09, 3.47139237760000E-05,
         0.0,                 0.0,
-3.17765981183000E-06, 2.63497832935000E-05, 1.42005317544000E-05,
1.22860504604000E-05,                 0.0,
3.21502582986000E-06,-6.01154071094000E-06,-7.10667037450000E-06,
-1.37041711834000E-06,-6.03652719918000E-06),
//   Rmatrix(5,5,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0),
//   Rmatrix(5,5,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0,
//         0.0,                  0.0,                  0.0,                  0.0,
//         0.0),
};
const Real                  SolarSystem::MOON_TWO_BODY_EPOCH[NumberOfDefaultMoons]      =
{
      21544.500370768266,
//      21544.500370768266,   // *** TEMPORARY ***
//      21544.500370768266,   // *** TEMPORARY ***
};
const Rvector6              SolarSystem::MOON_TWO_BODY_ELEMENTS[NumberOfDefaultMoons]   =
{
      Rvector6(      385494.90434829952,  0.055908943292024992,   20.940245433093748,
            12.233244412716252, 68.004298803147648,     137.94325682926458),
//      Rvector6(      385494.90434829952,  0.055908943292024992,   20.940245433093748,
//            12.233244412716252, 68.004298803147648,     137.94325682926458),           // *** TEMPORARY ***
//      Rvector6(      385494.90434829952,  0.055908943292024992,   20.940245433093748,
//            12.233244412716252, 68.004298803147648,     137.94325682926458),           // *** TEMPORARY ***
};

const Integer               SolarSystem::MOON_NUM_GRAVITY_MODELS[NumberOfDefaultMoons] =
   {  1,
         //0, 0,
   };
const Integer               SolarSystem::MOON_NUM_ATMOSPHERE_MODELS[NumberOfDefaultMoons] =
   {  0,
         //0, 0,
   };
const Integer               SolarSystem::MOON_NUM_MAGNETIC_MODELS[NumberOfDefaultMoons]=
   {  0,
         //0, 0,
   };
const Integer               SolarSystem::MOON_NUM_SHAPE_MODELS[NumberOfDefaultMoons] =
   {  0,
         //0, 0,
   };

const std::string           SolarSystem::MOON_GRAVITY_MODELS[] =
{
   "LP100",
};

const std::string           SolarSystem::MOON_ATMOSPHERE_MODELS[] =
{
   "None"
};

const std::string           SolarSystem::MOON_MAGNETIC_MODELS[] =
{
   "None"
};

const std::string          SolarSystem::MOON_SHAPE_MODELS[] =
{
   "None"
};

//         SpinAxisRA  Rate        SpinAxisDEC  Rate        Rotation    Rate
const Rvector6              SolarSystem::MOON_ORIENTATION_PARAMETERS[NumberOfDefaultMoons]   =
{
   Rvector6(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),  // for now, for default moons, calculations are done in the Moon class
//   Rvector6(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),  // for now, for default moons, calculations are done in the Moon class
//   Rvector6(0.0, 0.0, 0.0, 0.0, 0.0, 0.0),  // for now, for default moons, calculations are done in the Moon class
};
const Integer               SolarSystem::MOON_NAIF_IDS[NumberOfDefaultPlanets] =
{
      301,    // Luna
//      401,    // Phobos
//      402,    // Deimos
};

// default values for Planet data ------------------------  the Sun  ------------------------

const Gmat::PosVelSource    SolarSystem::STAR_POS_VEL_SOURCE      = Gmat::DE405;
const Real                  SolarSystem::STAR_EQUATORIAL_RADIUS   = 695990.0000;  // km
const Real                  SolarSystem::STAR_FLATTENING          = 0.0;
// Units for MU are km^3/s^2
const Real                  SolarSystem::STAR_MU                  = 132712440017.99;
//const Gmat::AnalyticMethod  STAR_ANALYTIC_METHOD     = Gmat::LOW_FIDELITY;
const Integer               SolarSystem::STAR_ORDER               = 0;
const Integer               SolarSystem::STAR_DEGREE              = 0;
const Rmatrix               SolarSystem::STAR_SIJ                 = Rmatrix(5,5); //zeroes
const Rmatrix               SolarSystem::STAR_CIJ                 = Rmatrix(5,5,
      1.0, 0.0,             0.0,             0.0,             0.0,
      0.0, 0.0,             0.0,             0.0,             0.0,
      0.0, 0.0,             0.0,             0.0,             0.0,
      0.0, 0.0,             0.0,             0.0,             0.0,
      0.0, 0.0,             0.0,             0.0,             0.0);

// NOTE - these must change when Earth's default values change!!!!!!!!!!!!
const Real                  SolarSystem::STAR_TWO_BODY_EPOCH           = 21544.500370768266;
const Rvector6              SolarSystem::STAR_TWO_BODY_ELEMENTS        = Rvector6(
      149653978.9783766,        0.01704556707314489,  23.439034090426388,
      0.00018646554487906264, 281.7416388084352,     358.12708491129);

const Integer               SolarSystem::STAR_NUM_GRAVITY_MODELS = 0;
const Integer               SolarSystem::STAR_NUM_ATMOSPHERE_MODELS = 0;
const Integer               SolarSystem::STAR_NUM_MAGNETIC_MODELS = 0;
const Integer               SolarSystem::STAR_NUM_SHAPE_MODELS = 0;
const std::string           SolarSystem::STAR_GRAVITY_MODELS = "None";
const std::string           SolarSystem::STAR_ATMOSPHERE_MODELS = "None";
const std::string           SolarSystem::STAR_MAGNETIC_MODELS = "None";
const std::string           SolarSystem::STAR_SHAPE_MODELS = "None"; // @todo add Shape Models
const Rvector6              SolarSystem::STAR_ORIENTATION_PARAMETERS = Rvector6(
      286.13, 0.0, 63.87, 0.0, 84.10, 14.1844000);

const Real                  SolarSystem::STAR_RADIANT_POWER       = 1358.0;
// Units for reference distance are km (1 AU)
const Real                  SolarSystem::STAR_REFERENCE_DISTANCE  = 1.49597870e8;
// Units for radius are meters
const Real                  SolarSystem::STAR_PHOTOSPHERE_RADIUS  = 695990000.0;
const Integer               SolarSystem::STAR_NAIF_IDS            = 10;


// add other moons, asteroids, comets, as needed
// what about libration points?

//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// static methods
//------------------------------------------------------------------------------
// none at this time


//------------------------------------------------------------------------------
//  SolarSystem(std::string withName)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the SolarSystem class (default constructor).
 *
 * @param <withName> optional parameter indicating the name of the solar
 *                   system (default is "").
 */
//------------------------------------------------------------------------------
SolarSystem::SolarSystem(std::string withName)
   : GmatBase(Gmat::SOLAR_SYSTEM, "SolarSystem", withName)
{
   objectTypes.push_back(Gmat::SOLAR_SYSTEM);
   objectTypeNames.push_back("SolarSystem");
   parameterCount      = SolarSystemParamCount;
   pvSrcForAll         = Gmat::DE405;
   thePlanetaryEphem   = NULL;
   overrideTimeForAll  = false;
   ephemUpdateInterval = 0.0;
#ifdef __USE_SPICE__
   planetarySPK   = new SpiceOrbitKernelReader();
   #ifdef DEBUG_SS_CREATE
   MessageInterface::ShowMessage
      ("SolarSystem::SolarSystem(default), this=<%p>, planetarySPK<%p> created\n",
       this, planetarySPK);
   #endif
#endif
   allowSpiceForDefaultBodies = true; // as of 2010.03.31, this is the default value

   // we want to cloak the Solar System data; i.e. we want to write only those
   // parameters that have been modified by the suer to a script; and we don't
   // want to include the Create line either
   cloaking = true;

   theSPKFilename             = "";
   lskKernelName              = "";

   FileManager *fm = FileManager::Instance();
   std::string upperCaseName;
   std::string filename, textureFile;

   // create and add the default bodies
   // Assume only one Star for now : )
   Star* theSun     = new Star(SUN_NAME);
   #ifdef DEBUG_MEMORY
   MemoryTracker::Instance()->Add
      (theSun, theSun->GetName(), "SolarSystem::SolarSystem()",
       "Star* theSun = new Star(SUN_NAME)");
   #endif
   theSun->SetCentralBody(EARTH_NAME);  // central body here is a reference body
   theSun->SetSolarSystem(this);
   theSun->SetSource(STAR_POS_VEL_SOURCE);
   theSun->SetEquatorialRadius(STAR_EQUATORIAL_RADIUS);
   theSun->SetFlattening(STAR_FLATTENING);
   theSun->SetGravitationalConstant(STAR_MU);
   theSun->SetOrder(STAR_ORDER);
   theSun->SetDegree(STAR_DEGREE);
   theSun->SetHarmonicCoefficientsSij(STAR_SIJ);
   theSun->SetHarmonicCoefficientsCij(STAR_CIJ);
   theSun->SetRadiantPower(STAR_RADIANT_POWER, STAR_REFERENCE_DISTANCE);
   theSun->SetPhotosphereRadius(STAR_PHOTOSPHERE_RADIUS);
   theSun->SetIntegerParameter(theSun->GetParameterID("NAIFId"),STAR_NAIF_IDS);

   theSun->SetTwoBodyEpoch(STAR_TWO_BODY_EPOCH);
   theSun->SetTwoBodyElements(STAR_TWO_BODY_ELEMENTS);
   theSun->AddValidModelName(Gmat::GRAVITY_FIELD,"Other");
   theSun->AddValidModelName(Gmat::ATMOSPHERE_MODEL,"Other");
   theSun->AddValidModelName(Gmat::MAGNETIC_FIELD,"Other");
// @todo - add Shape models or remove these if not necessary
   //         newPlanet->AddValidModelName(Gmat::SHAPE_MODEL,"Other");
   // Set the orientation parameters for the body
   theSun->SetOrientationParameters(STAR_ORIENTATION_PARAMETERS);
   // find and set the texture map
   upperCaseName = GmatStringUtil::ToUpper(SUN_NAME);
   filename      = upperCaseName + "_TEXTURE_FILE";
   try
   {
      textureFile   = fm->GetFullPathname(filename);
   }
   catch (UtilityException &ue)
   {
      MessageInterface::ShowMessage("Texture map file is missing or unknown for default body %s\n",
                                    SUN_NAME.c_str());
   }
   theSun->SetStringParameter(theSun->GetParameterID("TextureMapFileName"), textureFile);
   // add the body to the default SolarSystem
   AddBody(theSun);

   Planet *theEarth = NULL;
   Integer indexGravityModels = 0;
   Integer indexAtmosphereModels = 0;
   Integer indexMagneticModels = 0;
//   Integer indexShapeModels = 0;

   // Add default planets, assuming for now that they all orbit the Sun
   for (unsigned int ii = 0; ii < NumberOfDefaultPlanets; ii++)
   {
      Planet *newPlanet = new Planet(PLANET_NAMES[ii], SUN_NAME);
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (newPlanet, newPlanet->GetName(), "SolarSystem::SolarSystem()",
          "Planet *newPlanet = new Planet()");
      #endif
      if (PLANET_NAMES[ii] == EARTH_NAME) theEarth = newPlanet;
      newPlanet->SetCentralBody(SUN_NAME);
      newPlanet->SetSolarSystem(this);
      newPlanet->SetSource(PLANET_POS_VEL_SOURCE);
      newPlanet->SetEquatorialRadius(PLANET_EQUATORIAL_RADIUS[ii]);
      newPlanet->SetFlattening(PLANET_FLATTENING[ii]);
      newPlanet->SetGravitationalConstant(PLANET_MU[ii]);
      newPlanet->SetOrder(PLANET_ORDER[ii]);
      newPlanet->SetDegree(PLANET_DEGREE[ii]);
      newPlanet->SetHarmonicCoefficientsSij(PLANET_SIJ[ii]);
      newPlanet->SetHarmonicCoefficientsCij(PLANET_CIJ[ii]);
      // reference object must be set before setting TwoBodyEpoch or TwoBodyElements
      newPlanet->SetRefObject(theSun, Gmat::CELESTIAL_BODY, SUN_NAME);

      newPlanet->SetTwoBodyEpoch(PLANET_TWO_BODY_EPOCH[ii]);
      newPlanet->SetTwoBodyElements(PLANET_TWO_BODY_ELEMENTS[ii]);
      for (Integer jj = 0; jj < PLANET_NUM_GRAVITY_MODELS[ii]; jj++)
         newPlanet->AddValidModelName(Gmat::GRAVITY_FIELD,
                    PLANET_GRAVITY_MODELS[indexGravityModels++]);
      newPlanet->AddValidModelName(Gmat::GRAVITY_FIELD,"Other");
      for (Integer jj = 0; jj < PLANET_NUM_ATMOSPHERE_MODELS[ii]; jj++)
         newPlanet->AddValidModelName(Gmat::ATMOSPHERE_MODEL,
                    PLANET_ATMOSPHERE_MODELS[indexAtmosphereModels++]);
      newPlanet->AddValidModelName(Gmat::ATMOSPHERE_MODEL,"Other");
      for (Integer jj = 0; jj < PLANET_NUM_MAGNETIC_MODELS[ii]; jj++)
         newPlanet->AddValidModelName(Gmat::MAGNETIC_FIELD,
                    PLANET_MAGNETIC_MODELS[indexMagneticModels++]);
      newPlanet->AddValidModelName(Gmat::MAGNETIC_FIELD,"Other");
// @todo - add Shape models or remove these if not necessary
//      for (unsigned int jj = 0; jj < PLANET_NUM_SHAPE_MODELS[ii]; jj++)
//         newPlanet->AddValidModelName(Gmat::SHAPE_MODEL,
//                    PLANET_SHAPE_MODELS[indexShapeModels++]);
      //         newPlanet->AddValidModelName(Gmat::SHAPE_MODEL,"Other");
      // Set the orientation parameters for the body (Neptune is a special case - handled in the Planet class
      newPlanet->SetOrientationParameters(PLANET_ORIENTATION_PARAMETERS[ii]);
      newPlanet->SetIntegerParameter(newPlanet->GetParameterID("NAIFId"),PLANET_NAIF_IDS[ii]);
      // find and set the texture map
      upperCaseName = GmatStringUtil::ToUpper(PLANET_NAMES[ii]);
      filename      = upperCaseName + "_TEXTURE_FILE";
      try
      {
         textureFile   = fm->GetFullPathname(filename);
      }
      catch (UtilityException &ue)
      {
         MessageInterface::ShowMessage("Texture map file is missing or unknown for default body %s\n",
                                       (PLANET_NAMES[ii]).c_str());
      }
      newPlanet->SetStringParameter(newPlanet->GetParameterID("TextureMapFileName"), textureFile);
      // add the body to the default SolarSystem
      AddBody(newPlanet);
   }

   indexGravityModels = 0;
   indexAtmosphereModels = 0;
   indexMagneticModels = 0;
//   Integer indexShapeModels = 0;
   // Add default moons
   for (unsigned int ii = 0; ii < NumberOfDefaultMoons; ii++)
   {
      Moon *newMoon = new Moon(MOON_NAMES[ii], MOON_CENTRAL_BODIES[ii]);
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (newMoon, newMoon->GetName(), "SolarSystem::SolarSystem()",
          "Moon *newMoon = new Moon()");
      #endif
      newMoon->SetSource(MOON_POS_VEL_SOURCE[ii]);
      newMoon->SetEquatorialRadius(MOON_EQUATORIAL_RADIUS[ii]);
      newMoon->SetFlattening(MOON_FLATTENING[ii]);
      newMoon->SetGravitationalConstant(MOON_MU[ii]);
      newMoon->SetOrder(MOON_ORDER[ii]);
      newMoon->SetDegree(MOON_DEGREE[ii]);
      newMoon->SetHarmonicCoefficientsSij(MOON_SIJ[ii]);
      newMoon->SetHarmonicCoefficientsCij(MOON_CIJ[ii]);
      // reference object must be set before setting TwoBodyEpoch or TwoBodyElements
      CelestialBody *central = FindBody(MOON_CENTRAL_BODIES[ii]);
      if (!central)
      {
         std::string errMsg = "Central body " + MOON_CENTRAL_BODIES[ii];
         errMsg += " must be created before moon ";
         errMsg += MOON_NAMES[ii] + "\n";
         throw SolarSystemException(errMsg);
      }
      newMoon->SetCentralBody(MOON_CENTRAL_BODIES[ii]);
      newMoon->SetSolarSystem(this);
      newMoon->SetRefObject(central, Gmat::CELESTIAL_BODY, MOON_CENTRAL_BODIES[ii]);

      newMoon->SetTwoBodyEpoch(MOON_TWO_BODY_EPOCH[ii]);
      newMoon->SetTwoBodyElements(MOON_TWO_BODY_ELEMENTS[ii]);
      for (Integer jj = 0; jj < MOON_NUM_GRAVITY_MODELS[ii]; jj++)
         newMoon->AddValidModelName(Gmat::GRAVITY_FIELD,
                    MOON_GRAVITY_MODELS[indexGravityModels++]);
      for (Integer jj = 0; jj < MOON_NUM_ATMOSPHERE_MODELS[ii]; jj++)
         newMoon->AddValidModelName(Gmat::ATMOSPHERE_MODEL,
                    MOON_ATMOSPHERE_MODELS[indexAtmosphereModels++]);
      for (Integer jj = 0; jj < MOON_NUM_MAGNETIC_MODELS[ii]; jj++)
         newMoon->AddValidModelName(Gmat::MAGNETIC_FIELD,
                    MOON_MAGNETIC_MODELS[indexMagneticModels++]);
// @todo - add Shape models or remove these if not necessary
//      for (unsigned int jj = 0; jj < MOON_NUM_SHAPE_MODELS[ii]; jj++)
//         newMoon->AddValidModelName(Gmat::SHAPE_MODEL,
//                    MOON_SHAPE_MODELS[indexShapeModels++]);
      // Set the orientation parameters for the body (Neptune is a special case - handled in the Planet class
      newMoon->SetOrientationParameters(PLANET_ORIENTATION_PARAMETERS[ii]);
      newMoon->SetIntegerParameter(newMoon->GetParameterID("NAIFId"),MOON_NAIF_IDS[ii]);
      // find and set the texture map
      upperCaseName = GmatStringUtil::ToUpper(MOON_NAMES[ii]);
      filename      = upperCaseName + "_TEXTURE_FILE";
      try
      {
         textureFile   = fm->GetFullPathname(filename);
      }
      catch (UtilityException &ue)
      {
         MessageInterface::ShowMessage("Texture map file is missing or unknown for default body %s\n",
                                       (MOON_NAMES[ii]).c_str());
      }
      newMoon->SetStringParameter(newMoon->GetParameterID("TextureMapFileName"), textureFile);
      // add the body to the default SolarSystem
      AddBody(newMoon);
   }


   if (!theEarth)
      throw SolarSystemException("The Earth not defined.\n");

   // Set j2000body (LOJ: 2009.06.18)
   SetJ2000Body();
   
   // at least for now, give each body a pointer to its central body
   theSun->SetRefObject(theEarth, Gmat::CELESTIAL_BODY, SolarSystem::EARTH_NAME); // for reference!!!
   theDefaultDeFile    = NULL;


   // Set the Spice flags for the bodies
   #ifdef __USE_SPICE__
      spiceAvailable = true;
   #else
      spiceAvailable = false;
   #endif
   SetIsSpiceAllowedForDefaultBodies(true);

   CreatePlanetarySource();
   SaveAllAsDefault();
   // clear the modified flags for all default bodies
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      (*cbi)->SaveAllAsDefault();
      ++cbi;
   }

   #ifdef SS_CONSTRUCT_DESTRUCT
      MessageInterface::ShowMessage("Just constructed a new SolarSystem with name %s at <%p>\n",
            withName.c_str(), this);
   #endif
}


//------------------------------------------------------------------------------
//  SolarSystem(const SolarSystem &ss)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the SolarSystem class as a copy of the
 * specified SolarSystem class (copy constructor).
 *
 * @param <ss> SolarSystem object to copy.
 */
//------------------------------------------------------------------------------
SolarSystem::SolarSystem(const SolarSystem &ss) :
   GmatBase                          (ss),
   pvSrcForAll                       (ss.pvSrcForAll),
   thePlanetaryEphem                 (NULL),
   overrideTimeForAll                (ss.overrideTimeForAll),
   ephemUpdateInterval               (ss.ephemUpdateInterval),
   bodyStrings                       (ss.bodyStrings),
   defaultBodyStrings                (ss.defaultBodyStrings),
   userDefinedBodyStrings            (ss.userDefinedBodyStrings),
//   spiceKernelReader                 (ss.spiceKernelReader),
   allowSpiceForDefaultBodies        (ss.allowSpiceForDefaultBodies),
   spiceAvailable                    (ss.spiceAvailable),
   theSPKFilename                    (ss.theSPKFilename),
   lskKernelName                     (ss.lskKernelName),
   default_planetarySourceTypesInUse (ss.default_planetarySourceTypesInUse), // deprecated!!
   default_ephemerisSource           (ss.default_ephemerisSource),
   default_DEFilename                (ss.default_DEFilename),
   default_SPKFilename               (ss.default_SPKFilename),
   default_LSKFilename               (ss.default_LSKFilename),
   default_overrideTimeForAll        (ss.default_overrideTimeForAll),
   default_ephemUpdateInterval       (ss.default_ephemUpdateInterval)

{
   theDefaultDeFile  = NULL;
   parameterCount    = SolarSystemParamCount;

#ifdef __USE_SPICE__
   planetarySPK = (ss.planetarySPK)->Clone();
   #ifdef DEBUG_SS_CREATE
   MessageInterface::ShowMessage
      ("SolarSystem::SolarSystem(copy), this=<%p>, planetarySPK<%p> cloned\n",
       this, planetarySPK);
   #endif
#endif

   // create planetary source first, but do not create default
   thePlanetarySourceNames = ss.thePlanetarySourceNames;
   CreatePlanetarySource(false);

   // copy current planetary source is use
   thePlanetarySourceTypesInUse = ss.thePlanetarySourceTypesInUse;

   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage
      ("SolarSystem::copy constructor() num ephem source types=%d\n",
       thePlanetarySourceTypesInUse.size());
   for (UnsignedInt i=0; i<thePlanetarySourceTypesInUse.size(); i++)
      MessageInterface::ShowMessage
         ("   '%s'\n", thePlanetarySourceTypesInUse[i].c_str());
   #endif

   // set current planetary source
   SetPlanetarySourceTypesInUse(thePlanetarySourceTypesInUse);

   CloneBodiesInUse(ss);
   SetJ2000Body();
}


//------------------------------------------------------------------------------
//  SolarSystem& operator= (const SolarSystem& ss)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the SolarSystem class.
 *
 * @param <ss> the solar system object whose data to assign to "this"
 *             solar system.
 *
 * @return "this" solarsystem with data of input solarsystem ss.
 */
//------------------------------------------------------------------------------
SolarSystem& SolarSystem::operator=(const SolarSystem &ss)
{
   GmatBase::operator=(ss);
   pvSrcForAll                = ss.pvSrcForAll;
   thePlanetaryEphem          = NULL;
   overrideTimeForAll         = ss.overrideTimeForAll;
   ephemUpdateInterval        = ss.ephemUpdateInterval;
   bodyStrings                = ss.bodyStrings;
   defaultBodyStrings         = ss.defaultBodyStrings;
   userDefinedBodyStrings     = ss.userDefinedBodyStrings;
   allowSpiceForDefaultBodies = ss.allowSpiceForDefaultBodies;
   spiceAvailable             = ss.spiceAvailable;
   theSPKFilename             = ss.theSPKFilename;
   lskKernelName              = ss.lskKernelName;
   parameterCount             = SolarSystemParamCount;
   theDefaultDeFile           = NULL;
   default_planetarySourceTypesInUse = ss.default_planetarySourceTypesInUse; // deprecated!!
   default_ephemerisSource           = ss.default_ephemerisSource;
   default_DEFilename                = ss.default_DEFilename;
   default_SPKFilename               = ss.default_SPKFilename;
   default_LSKFilename               = ss.default_LSKFilename;
   default_overrideTimeForAll        = ss.default_overrideTimeForAll;
   default_ephemUpdateInterval       = ss.default_ephemUpdateInterval;

   // create planetary source first, but do not create default
   thePlanetarySourceNames = ss.thePlanetarySourceNames;
   CreatePlanetarySource(false);
#ifdef __USE_SPICE__
   planetarySPK          = ss.planetarySPK;
   #ifdef DEBUG_SS_CREATE
   MessageInterface::ShowMessage
      ("SolarSystem::operator=(), this=<%p>, planetarySPK<%p> copied\n",
       this, planetarySPK);
   #endif
#endif

   // copy current planetary source in use
   thePlanetarySourceTypesInUse = ss.thePlanetarySourceTypesInUse;

   // set current planetary source
   SetPlanetarySourceTypesInUse(thePlanetarySourceTypesInUse);

   // delete old bodies and clone bodies
   DeleteBodiesInUse();
   CloneBodiesInUse(ss);
   SetJ2000Body();

   return *this;
}


//------------------------------------------------------------------------------
//  ~SolarSystem()
//------------------------------------------------------------------------------
/**
 * Destructor for the SolarSystem class.
 */
//------------------------------------------------------------------------------
SolarSystem::~SolarSystem()
{
   #ifdef SS_CONSTRUCT_DESTRUCT
      MessageInterface::ShowMessage("Now destructing the SolarSystem with name %s at <%p>\n",
            instanceName.c_str(), this);
   #endif
      
   DeleteBodiesInUse();
   
   #ifdef DEBUG_SS_CLONING
   MessageInterface::ShowMessage
      ("SolarSystem::~SolarSystem() <%s> thePlanetaryEphem=%p, planetarySPK=<%p> \n",
       GetName().c_str(), thePlanetaryEphem, planetarySPK);
   #endif
   
//   if (theDefaultSlpFile != NULL)
//   {
//      #ifdef DEBUG_SS_CLONING
//      MessageInterface::ShowMessage
//         ("   deleting theDefaultSlpFile=%p\n",  theDefaultSlpFile);
//      #endif
//
//      delete theDefaultSlpFile;
//   }

   if (theDefaultDeFile != NULL)
   {
      #ifdef DEBUG_SS_CLONING
      MessageInterface::ShowMessage
         ("   deleting theDefaultDeFile=%p\n", theDefaultDeFile);
      #endif

      // This call crashes Linux
      /// @todo Track down and fix the solar system destructor call for DeFile
      #ifndef __linux
          delete theDefaultDeFile;
      #endif
   }

#ifdef __USE_SPICE__
   delete planetarySPK;
#endif
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
bool SolarSystem::Initialize()
{
   #ifdef DEBUG_SS_INIT
   MessageInterface::ShowMessage
      ("SolarSystem::Initialize() this=<%p> entered, planetarySPK=<%p>\n"
       "There are %d bodies in use\n", this, planetarySPK, bodiesInUse.size());
   for (UnsignedInt i = 0; i < bodiesInUse.size(); i++)
   {
      CelestialBody *cb = bodiesInUse[i];
      MessageInterface::ShowMessage
         ("   <%p>  %-9s %-10s\n", cb, cb->GetTypeName().c_str(), cb->GetName().c_str());
   }
   #endif

   #ifdef __USE_SPICE__
      try
      {
         planetarySPK->LoadKernel(theSPKFilename);
      }
      catch (UtilityException& ue)
      {
         // try again with path name if no path found
         std::string spkName = theSPKFilename;
         if (spkName.find("/") == spkName.npos &&
             spkName.find("\\") == spkName.npos)
         {
            std::string spkPath =
               FileManager::Instance()->GetFullPathname(FileManager::SPK_PATH);
            spkName = spkPath + spkName;
            try
            {
               planetarySPK->LoadKernel(spkName);
               #ifdef DEBUG_SS_SPICE
               MessageInterface::ShowMessage
                  ("   kernelReader has loaded file %s\n", spkName.c_str());
               #endif
            }
            catch (UtilityException& ue)
            {
               MessageInterface::ShowMessage("ERROR loading kernel %s\n",
                  spkName.c_str());
               throw; // rethrow the exception, for now
            }
         }
      }

      // Load the Leap Second Kernel
      try
      {
         planetarySPK->SetLeapSecondKernel(lskKernelName);
      }
      catch (UtilityException& ue)
      {
         // try again with path name if no path found
         std::string lskName = lskKernelName;
         if (lskName.find("/") == lskName.npos &&
             lskName.find("\\") == lskName.npos)
         {
            std::string lskPath =
               FileManager::Instance()->GetFullPathname(FileManager::TIME_PATH);
            lskName = lskPath + lskName;
            try
            {
               planetarySPK->LoadKernel(lskName);
               #ifdef DEBUG_SS_SPICE
               MessageInterface::ShowMessage
                  ("   kernelReader has loaded file %s\n", lskName.c_str());
               #endif
            }
            catch (UtilityException& ue)
            {
               MessageInterface::ShowMessage("ERROR loading kernel %s\n",
                  lskName.c_str());
               throw; // rethrow the exception, for now
            }
         }
      }
   #endif
   // Initialize bodies in use
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      #ifdef __USE_SPICE__
         // Set the kernel reader on the celestial bodies
         (*cbi)->SetSpiceOrbitKernelReader(planetarySPK);
         // Load the Planetary SPK Kernel
      #endif
      (*cbi)->Initialize();
      ++cbi;
   }
   return true;
}


//------------------------------------------------------------------------------
// void CreatePlanetarySource(bool setDefault)
//------------------------------------------------------------------------------
/*
 * Creates planetary epemeris source.
 *
 * @param <setDefault> set to true if default ephemeris is to be created (true)
 */
//------------------------------------------------------------------------------
void SolarSystem::CreatePlanetarySource(bool setDefault)
{
   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage("SolarSystem initializing planetary source...\n");
   #endif

   // initialize planetary source
   for (int i=0; i<Gmat::PosVelSourceCount; i++)
   {
      if ((i == Gmat::SPICE) && (!(allowSpiceForDefaultBodies) ||
            !spiceAvailable)) continue; // SPICE not selectable for entire solar system
      thePlanetarySourcePriority[i] = 0;
      isPlanetarySourceInUse[i] = false;

      thePlanetarySourceTypes.push_back(Gmat::POS_VEL_SOURCE_STRINGS[i]);
   }

   if (setDefault)
   {
      #ifdef DEBUG_SS_PLANETARY_FILE
      MessageInterface::ShowMessage(
            "SS::CreatePlanetarySource ... setting to defaults from file manager...\n");
      #endif
      FileManager *fm = FileManager::Instance();

      //initialize file names
      thePlanetarySourceNames.push_back("N/A");  // TwoBodyPropagation has no file associated with it

      thePlanetarySourceNames.push_back(fm->GetFullPathname("DE405_FILE"));
      if (spiceAvailable)
      {
         std::string spkFullPath = fm->GetFullPathname("PLANETARY_SPK_FILE");
         if (!(GmatStringUtil::IsBlank(spkFullPath)))
         {
            if (!(SetSPKFile(spkFullPath)))
               throw SolarSystemException("Unable to set SPK file on the Solar System.\n");
            thePlanetarySourceNames.push_back(theSPKFilename);
         }
         std::string lskFullPath = fm->GetFullPathname("LSK_FILE");
         if (!(GmatStringUtil::IsBlank(lskFullPath)))
         {
            if (!(SetLSKFile(lskFullPath)))
               throw SolarSystemException("Unable to set LSK file on the Solar System.\n");
         }
         else
            throw SolarSystemException("Unable to obtain Leap Second Kernel (SLK) full path name.\n");
      }
   }
   // Set planetary ephemeris source
   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage(" ... thePlanetarySourceNames are ... \n");
   for (unsigned int ii = 0; ii < thePlanetarySourceNames.size(); ii++)
      MessageInterface::ShowMessage(" ... %d    %s\n",
            (Integer) ii, (thePlanetarySourceNames.at(ii)).c_str());
   MessageInterface::ShowMessage(
         "SS::CreatePlanetarySource ... about to call SetDefaultPlanetarySource...\n");
   #endif
   SetDefaultPlanetarySource(); // do this each time ...
}


//------------------------------------------------------------------------------
// const StringArray& GetPlanetarySourceTypes()
//------------------------------------------------------------------------------
/**
 * @return a planetary source types
 */
//------------------------------------------------------------------------------
const StringArray& SolarSystem::GetPlanetarySourceTypes()
{
   return thePlanetarySourceTypes;
}

//------------------------------------------------------------------------------
// const StringArray& GetPlanetarySourceNames()
//------------------------------------------------------------------------------
/**
 * @return a planetary source file names
 */
//------------------------------------------------------------------------------
const StringArray& SolarSystem::GetPlanetarySourceNames()
{
   return thePlanetarySourceNames;
}

//------------------------------------------------------------------------------
// const StringArray& GetPlanetarySourceTypesInUse()
//------------------------------------------------------------------------------
/**
 * @return a planetary source types in use
 */
//------------------------------------------------------------------------------
const StringArray& SolarSystem::GetPlanetarySourceTypesInUse()
{
   theTempFileList.clear();
   for (unsigned int i=0; i<thePlanetarySourceTypesInUse.size(); i++)
      theTempFileList.push_back(thePlanetarySourceTypesInUse[i]);

   return theTempFileList;
}


//------------------------------------------------------------------------------
// bool SetPlanetarySourceName(const std::string &sourceType,
//                           const std::string &fileName)
//------------------------------------------------------------------------------
bool SolarSystem::SetPlanetarySourceName(const std::string &sourceType,
                                         const std::string &fileName)
{
   bool status = false;
   Integer id = GetPlanetarySourceId(sourceType);
   #ifdef DEBUG_SS_PLANETARY_FILE
      MessageInterface::ShowMessage(
            "In SS:SetPlanetarySourceName, sourceType = %s  and fileName = %s\n",
            sourceType.c_str(), fileName.c_str());
      MessageInterface::ShowMessage("   and id for that type = %d\n", id);
   #endif

   if (id >= 0)
   {
      if (id == Gmat::SPICE)
      {
         theSPKFilename = fileName;
         thePlanetarySourceNames[id] = fileName;
         status =  true;
      }
      else if (id == Gmat::DE405)
      {
         status = CreateDeFile(Gmat::DE405, fileName);
         if (status)
         {
            thePlanetarySourceNames[id] = fileName;
            SetSourceFile(theDefaultDeFile);
         }
      }
   }

   return status;
}


//------------------------------------------------------------------------------
// std::string GetPlanetarySourceName(const std::string &sourceType)
//------------------------------------------------------------------------------
std::string SolarSystem::GetPlanetarySourceName(const std::string &sourceType)
{
   Integer id = GetPlanetarySourceId(sourceType);

   if (id >= 0)
      return thePlanetarySourceNames[id];
   else
      return "Unknown Source Type";
}


//------------------------------------------------------------------------------
// Integer SetPlanetarySourceTypesInUse(const StringArray &sourceTypes)
//------------------------------------------------------------------------------
/*
 * @param <sourceTypes> list of file type in the priority order of use
 *
 * @return 0, if error setting any of planetary file in the list.
 *         1, if error setting first planetary file in the list, but set to
 *            next available file.
 *         2, if successfuly set to first planetary file in the list
 */
//------------------------------------------------------------------------------
Integer SolarSystem::SetPlanetarySourceTypesInUse(const StringArray &sourceTypes)
{
   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage
      ("SolarSystem::SetPlanetarySourceTypesInUse() num source types=%d\n",
       sourceTypes.size());
   for (UnsignedInt i=0; i<sourceTypes.size(); i++)
      MessageInterface::ShowMessage("   '%s'\n", sourceTypes[i].c_str());
   #endif

   bool status = false;
   Integer sourceTypeInUse = -1;
   Integer retCode = 0;

   // update planetary file types
   if (&thePlanetarySourceTypesInUse != &sourceTypes)
   {
      #ifdef DEBUG_SS_PLANETARY_FILE
      MessageInterface::ShowMessage
         ("SolarSystem::SetPlanetarySourceTypesInUse() updating planetary source\n");
      #endif

      thePlanetarySourceTypesInUse.clear();

      for (unsigned int i=0; i<sourceTypes.size(); i++)
      {
         #ifdef DEBUG_SS_PLANETARY_FILE
         MessageInterface::ShowMessage
            ("   and the input sourceTypes[%d] = %s\n",
                  (Integer) i, (sourceTypes.at(i)).c_str());
         #endif
         thePlanetarySourceTypesInUse.push_back(sourceTypes[i]);
      }
   }

   theTempFileList.clear();
   for (unsigned int i=0; i<thePlanetarySourceTypesInUse.size(); i++)
      theTempFileList.push_back(thePlanetarySourceTypesInUse[i]);

   // create planetary ephem file if non-analytic
   for (unsigned int i=0; i<thePlanetarySourceTypesInUse.size(); i++)
   {
      if (thePlanetarySourceTypesInUse[i] == Gmat::POS_VEL_SOURCE_STRINGS[Gmat::TWO_BODY_PROPAGATION])
      {
            thePlanetarySourcePriority[Gmat::TWO_BODY_PROPAGATION] = HIGHEST_PRIORITY - i;
            isPlanetarySourceInUse[Gmat::TWO_BODY_PROPAGATION] = true;
            sourceTypeInUse = Gmat::TWO_BODY_PROPAGATION;
            break;
      }
      else if (thePlanetarySourceTypesInUse[i] == Gmat::POS_VEL_SOURCE_STRINGS[Gmat::DE405])
      {
         #ifdef DEBUG_SS_PLANETARY_FILE
         MessageInterface::
            ShowMessage("SolarSystem::SetPlanetarySourceTypesInUse() create DE405 (%s)\n",
                  (thePlanetarySourceNames[Gmat::DE405]).c_str());
         #endif

         thePlanetarySourcePriority[Gmat::DE405] = 0;
         status = CreateDeFile(Gmat::DE405, thePlanetarySourceNames[Gmat::DE405]);
         if (status)
         {
            thePlanetarySourcePriority[Gmat::DE405] = HIGHEST_PRIORITY - i;
            isPlanetarySourceInUse[Gmat::DE405] = true;
            sourceTypeInUse = Gmat::DE405;
            break;
         }
      }
      else if (thePlanetarySourceTypesInUse[i] == Gmat::POS_VEL_SOURCE_STRINGS[Gmat::SPICE])
      {
         #ifdef DEBUG_SS_PLANETARY_FILE
            MessageInterface::ShowMessage(
                  "SolarSystem::SetPlanetarySourceTypesInUse() SPICE selected\n");
         #endif
         if ((!allowSpiceForDefaultBodies) || (!spiceAvailable))
         {
            std::string errMsg = "Cannot be selected for ephemeris source for entire solar system: \"";
            errMsg += thePlanetarySourceTypesInUse[i] + "\"\n";
            throw SolarSystemException(errMsg);
         }
         thePlanetarySourcePriority[Gmat::SPICE] = HIGHEST_PRIORITY - i;
         isPlanetarySourceInUse[Gmat::SPICE] = true;
         sourceTypeInUse = Gmat::SPICE;
         break;

      }
      else
      {
         std::string errMsg = "Unknown planetary ephemeris source \"";
         errMsg += thePlanetarySourceTypesInUse[i] + "\"\n";
         throw SolarSystemException(errMsg);
      }
   }

   // set SolarSystem to use the file
   if (sourceTypeInUse == -1)
   {
      MessageInterface::ShowMessage("SolarSystem::SetPlanetarySourceTypesInUse() NO "
                                    "Planetary file is set to use \n");
      retCode = 0;
   }
   else
   {
      #ifdef DEBUG_SS_PLANETARY_FILE
      MessageInterface::
         ShowMessage("SolarSystem::SetPlanetarySourceTypesInUse() "
                     "Set Planetary Source to use:%d\n", sourceTypeInUse);
      #endif
      switch (sourceTypeInUse)
      {
      case Gmat::TWO_BODY_PROPAGATION:
         if (SetSource(Gmat::TWO_BODY_PROPAGATION))
            retCode = 1;
         break;
      case Gmat::DE405:
         if (SetSource(Gmat::DE405))
            if (SetSourceFile(theDefaultDeFile))
               retCode = 1;
         break;
      case Gmat::SPICE:
         if (SetSource(Gmat::SPICE))
            if (theSPKFilename != "")
            {
               bool isOK = SetSPKFile(theSPKFilename);
               if (!isOK)
                  throw SolarSystemException("Unable to set SPK file on one or more of the default bodies.\n");
            }
            retCode = 1;
         break;
      default:
         break;
      }
   }

   // if planetary file is set to first type in the list
   if (retCode == 1 && Gmat::POS_VEL_SOURCE_STRINGS[sourceTypeInUse] == sourceTypes[0])
      retCode = 2;


   // if error setting given planetary file, re-arrange planetary file list
   if (retCode == 1)
   {
      thePlanetarySourceTypesInUse.clear();

      for (unsigned int i=0; i<theTempFileList.size(); i++)
      {
         if (theTempFileList[i] == Gmat::POS_VEL_SOURCE_STRINGS[Gmat::TWO_BODY_PROPAGATION])
         {
            if (thePlanetarySourcePriority[Gmat::TWO_BODY_PROPAGATION] > 0)
               thePlanetarySourceTypesInUse.push_back(Gmat::POS_VEL_SOURCE_STRINGS[Gmat::TWO_BODY_PROPAGATION]);
         }
         else if (theTempFileList[i] == Gmat::POS_VEL_SOURCE_STRINGS[Gmat::DE405])
         {
            if (thePlanetarySourcePriority[Gmat::DE405] > 0)
               thePlanetarySourceTypesInUse.push_back(Gmat::POS_VEL_SOURCE_STRINGS[Gmat::DE405]);
         }
      }

      #ifdef DEBUG_SS_PLANETARY_FILE
      for (unsigned int i=0; i<thePlanetarySourceTypesInUse.size(); i++)
      {
         MessageInterface::ShowMessage
            ("thePlanetarySourceTypesInUse[%d]=%s\n", i,
             thePlanetarySourceTypesInUse[i].c_str());
      }
      #endif
   }

   if (retCode > 0)
      MessageInterface::LogMessage
         ("Successfully set Planetary Source to use: %s\n",
          Gmat::POS_VEL_SOURCE_STRINGS[sourceTypeInUse].c_str());

   theCurrentPlanetarySource = Gmat::POS_VEL_SOURCE_STRINGS[sourceTypeInUse];

   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage
      ("SolarSystem::SetPlanetarySourceTypesInUse() returning %d, "
       "theCurrentPlanetarySource is set to '%s'\n", retCode,
       theCurrentPlanetarySource.c_str());
   #endif

   return retCode;
}

//------------------------------------------------------------------------------
// Integer GetPlanetarySourceId(const std::string &sourceType)
//------------------------------------------------------------------------------
Integer SolarSystem::GetPlanetarySourceId(const std::string &sourceType)
{
   for (int i=0; i<Gmat::PosVelSourceCount; i++)
   {
      if (sourceType == Gmat::POS_VEL_SOURCE_STRINGS[i])
         return i;
   }

   return -1;
}


//------------------------------------------------------------------------------
// std::string GetPlanetarySourceNameInUse()
//------------------------------------------------------------------------------
std::string SolarSystem::GetCurrentPlanetarySource()
{
   return theCurrentPlanetarySource;
}

//------------------------------------------------------------------------------
// void SetIsSpiceAllowedForDefaultBodies(const bool allowSpice)
//------------------------------------------------------------------------------
void SolarSystem::SetIsSpiceAllowedForDefaultBodies(const bool allowSpice)
{
   allowSpiceForDefaultBodies = allowSpice;
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
#ifdef DEBUG_SS_SPICE
   if ((*cbi)->IsUserDefined())
      MessageInterface::ShowMessage("In SS, body %s is user-defined\n",
            ((*cbi)->GetName()).c_str());
   else
      MessageInterface::ShowMessage("In SS, setting allowSpice flag on body %s\n",
            ((*cbi)->GetName()).c_str());
#endif
      if (!((*cbi)->IsUserDefined())) (*cbi)->SetAllowSpice(allowSpiceForDefaultBodies);
      ++cbi;
   }

}

//------------------------------------------------------------------------------
// bool IsSpiceAllowedForDefaultBodies() const
//------------------------------------------------------------------------------
bool SolarSystem::IsSpiceAllowedForDefaultBodies() const
{
   return allowSpiceForDefaultBodies;
}


//------------------------------------------------------------------------------
// void ResetToDefaults()
//------------------------------------------------------------------------------
void SolarSystem::ResetToDefaults()
{
   #ifdef DEBUG_SS_DEFAULTS
   MessageInterface::ShowMessage
      ("SolarSystem::ResetToDefaults() <%p> '%s' entered\n", this, GetName().c_str());
   #endif

   pvSrcForAll         = Gmat::DE405;
   thePlanetaryEphem   = NULL;
   overrideTimeForAll  = false;
   ephemUpdateInterval = 0.0;

   // Set it for each of the bodies
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      if (!((*cbi)->IsUserDefined())) (*cbi)->SetSource(pvSrcForAll);
      (*cbi)->SetOverrideTimeSystem(overrideTimeForAll);
      (*cbi)->SetEphemUpdateInterval(ephemUpdateInterval);
      (*cbi)->SetUsePotentialFile(false);
      ++cbi;
   }
}


//------------------------------------------------------------------------------
//  bool AddBody(CelestialBody* cb)
//------------------------------------------------------------------------------
/**
 * This method adds to the list of bodies in use for this solar system.
 *
 * @param <cb>  body to add to the list for this solar system.
 *
 * @return flag indicating success of the operation
 *
 */
//------------------------------------------------------------------------------
bool SolarSystem::AddBody(CelestialBody* cb)
{
   #ifdef DEBUG_SS_ADD_BODY
   MessageInterface::ShowMessage
      ("SolarSystem::AddBody() this=<%p> '%s' entered\n", this, GetName().c_str());
   #endif

   if (cb == NULL)
   {
      return false;    // write some kind of error or throw an exception?
   }

   bool userDef = cb->IsUserDefined();
   bodiesInUse.push_back(cb);
   bodyStrings.push_back(cb->GetName());
   if (userDef)  userDefinedBodyStrings.push_back(cb->GetName());
   else          defaultBodyStrings.push_back(cb->GetName());

   #ifdef DEBUG_SS_ADD_BODY
   MessageInterface::ShowMessage
      ("   %s, Added <%p> %s\n", GetName().c_str(), cb, cb->GetName().c_str());
   #endif

   // Set the source, source file, and override flag for the new body
   if (!userDef)
   {
      if (!cb->SetSource(pvSrcForAll))  return false;
      if (pvSrcForAll == Gmat::DE405)
      {
         if (thePlanetaryEphem)
            if (!cb->SetSourceFile(thePlanetaryEphem))  return false;
      }
      else if (pvSrcForAll == Gmat::SPICE)
      {
//         if (theSPKFilename != "")
//            if (!cb->SetStringParameter(cb->GetParameterID("OrbitSpiceKernelName"), theSPKFilename)) return false;
//         if (lskKernelName != "")
//            if (!cb->SetStringParameter(cb->GetParameterID("LeapSecondKernelName"), lskKernelName)) return false;
      }
   }
   else // set main SPICE file for all added bodies
   {
//     if (!cb->SetStringParameter(cb->GetParameterID("OrbitSpiceKernelName"), theSPKFilename)) return false;
//      if (!cb->SetStringParameter(cb->GetParameterID("LeapSecondKernelName"), lskKernelName)) return false;
   }
   if (!cb->SetOverrideTimeSystem(overrideTimeForAll))  return false;

   return true;
}

//------------------------------------------------------------------------------
//  CelestialBody* GetBody(std::string withName)
//------------------------------------------------------------------------------
/**
 * This method returns a pointer to the requested celestial body.
 *
 * @param <withName>  name of the requested body.
 *
 * @return a pointer to the requested body.
 *
 */
//------------------------------------------------------------------------------
CelestialBody* SolarSystem::GetBody(std::string withName)
{
   return FindBody(withName);
}

bool SolarSystem::DeleteBody(const std::string &withName)
{
   // Search through bodiesInUse for the body with the name withName
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      if ((*cbi)->GetName() == withName)
      {
         CelestialBody *bodyToDelete = (*cbi);
         bodiesInUse.erase(cbi);
         
         #ifdef DEBUG_MEMORY
         MemoryTracker::Instance()->Remove
            (bodyToDelete, bodyToDelete->GetName(), "SolarSystem::DeleteBody()",
             " delete bodyToDelete");
         #endif
         delete bodyToDelete;
         
         for (StringArray::iterator si = bodyStrings.begin(); si != bodyStrings.end(); ++si)
            if ((*si) == withName) bodyStrings.erase(si);
         for (StringArray::iterator si = defaultBodyStrings.begin(); si != defaultBodyStrings.end(); ++si)
            if ((*si) == withName) defaultBodyStrings.erase(si);
         for (StringArray::iterator si = userDefinedBodyStrings.begin(); si != userDefinedBodyStrings.end(); ++si)
            if ((*si) == withName) userDefinedBodyStrings.erase(si);
         return true;
      }
      ++cbi;
   }
   return false;
}

//------------------------------------------------------------------------------
//  bool IsBodyInUse(std::string theBody)
//------------------------------------------------------------------------------
/**
 * This method returns a flag indicating whether the specified celestial body
 * is in use.
 *
 * @param <theBody>  name of the body.
 *
 * @return a flag indicating whether the specified body is in use.
 *
 */
//------------------------------------------------------------------------------
bool SolarSystem::IsBodyInUse(std::string theBody)
{
   // Search through bodiesInUse for the body with the name withName
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      if ((*cbi)->GetName() == theBody)
         return true;
      ++cbi;
   }
   return false;
}

const StringArray& SolarSystem::GetDefaultBodies() const
{
   return defaultBodyStrings;
}

const StringArray& SolarSystem::GetUserDefinedBodies() const
{
   return userDefinedBodyStrings;
}


//------------------------------------------------------------------------------
//  Gmat::PosVelSource GetPosVelSource() const
//------------------------------------------------------------------------------
/**
 * This method returns the source of position and velocity for the bodies in
 * use (assuming all have the same source).
 *
 * @return position/velocity source for the bodies.
 *
 */
//------------------------------------------------------------------------------
Gmat::PosVelSource   SolarSystem::GetPosVelSource() const
{
   return pvSrcForAll;
}


//------------------------------------------------------------------------------
//  std::string GetSourceFileName() const
//------------------------------------------------------------------------------
/**
 * This method returns the source file name for the bodies in
 * use (assuming all have the same source).
 *
 * @return source file name for the bodies.
 *
 */
//------------------------------------------------------------------------------
std::string SolarSystem::GetSourceFileName() const
{
   if (thePlanetaryEphem == NULL) return "";
   return thePlanetaryEphem->GetName();
}


//------------------------------------------------------------------------------
// bool GetOverrideTimeSystem() const
//------------------------------------------------------------------------------
bool SolarSystem::GetOverrideTimeSystem() const
{
   return overrideTimeForAll;
}


//------------------------------------------------------------------------------
// Real GetEphemUpdateInterval() const
//------------------------------------------------------------------------------
Real SolarSystem::GetEphemUpdateInterval() const
{
   return ephemUpdateInterval;
}


//------------------------------------------------------------------------------
// StringArray GetValidModelList(Gmat::ModelType m, const std::string &forBody)
//------------------------------------------------------------------------------
StringArray SolarSystem::GetValidModelList(Gmat::ModelType m,
                         const std::string &forBody)
{
   for (std::vector<CelestialBody*>::iterator i = bodiesInUse.begin();
        i != bodiesInUse.end(); ++i)
   {
      if ((*i)->GetName() == forBody)
         return (*i)->GetValidModelList(m);
   }
   throw SolarSystemException("Model list requested for unknown body "
                               + forBody);
}


//------------------------------------------------------------------------------
//  bool SetSource(Gmat::PosVelSource pvSrc)
//------------------------------------------------------------------------------
/**
 * This method sets the source for the bodies in
 * use (assuming all have the same source).
 *
 * @param <pvSrc> source (for pos and vel) for all of the default bodies.
 *
* @return success flag for the operation.
 *
 */
//------------------------------------------------------------------------------
bool SolarSystem::SetSource(Gmat::PosVelSource pvSrc)
{
   #ifdef DEBUG_SS_PLANETARY_FILE
      MessageInterface::ShowMessage("Setting Solar System ephem source to %d (%s)\n",
            (Integer) pvSrc, (Gmat::POS_VEL_SOURCE_STRINGS[pvSrc]).c_str());
   #endif

   // Set the source flag on all of the default bodies
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      bool userDef = (*cbi)->IsUserDefined();
      if (!userDef)
      {
         if ((*cbi)->SetSource(pvSrc) == false)  return false;
      }
      ++cbi;
   }
   pvSrcForAll = pvSrc;
   std::string srcStr = "";
   for (Integer ii = 0; ii < Gmat::PosVelSourceCount; ii++)
      if (pvSrc == ii)
      {
         srcStr = Gmat::POS_VEL_SOURCE_STRINGS[ii];
         break;
      }
   if (srcStr == "")
      throw SolarSystemException("Error setting source for Solar System - unknown source\n");

   theCurrentPlanetarySource = srcStr;
   return true;
}

//------------------------------------------------------------------------------
//  bool SetSource(const std::string &pvSrc)
//------------------------------------------------------------------------------
/**
 * This method sets the source for the bodies in
 * use (assuming all have the same source).
 *
 * @param <pvSrc> source (for pos and vel) for all of the bodies.
 *
 * @return success flag for the operation.
 *
 */
//------------------------------------------------------------------------------
bool SolarSystem::SetSource(const std::string &pvSrc)
{
   Gmat::PosVelSource theSrc = Gmat::PosVelSourceCount;
   for (Integer i = 0; i < Gmat::PosVelSourceCount; i++)
   {
      if (pvSrc == Gmat::POS_VEL_SOURCE_STRINGS[i])
         theSrc = (Gmat::PosVelSource) i;
   }
   if (theSrc == Gmat::PosVelSourceCount)
      throw SolarSystemException("Unknown ephemeris source " + pvSrc);

   // Search through bodiesInUse and set the source for all
   return SetSource(theSrc);
}

//------------------------------------------------------------------------------
//  bool SetSourceFile(PlanetaryEphem *src)
//------------------------------------------------------------------------------
/**
 * This method sets the source (DE) file for the bodies in
 * use (assuming all have the same method).
 *
 * @param <src> planetary ephem - source for all of the bodies.
 *
 * @return success flag for the operation.
 *
 */
//------------------------------------------------------------------------------
bool SolarSystem::SetSourceFile(PlanetaryEphem *src)
{
   // check for null src
   if (src == NULL)
      return false;

   thePlanetaryEphem = src;

   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage
      ("SolarSystem::SetSourceFile() Setting source file to <%p>\n   %s\n", src,
       src->GetName().c_str(), src);
   #endif

   // Search through bodiesInUse for the body with the name withName
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      bool userDef = (*cbi)->IsUserDefined();
      if (!userDef)
         if ((*cbi)->SetSourceFile(thePlanetaryEphem) == false) return false;
      ++cbi;
   }

   return true;
}

bool SolarSystem::SetSPKFile(const std::string &spkFile)
{
   theSPKFilename = spkFile;
//   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
//   while (cbi != bodiesInUse.end())
//   {
//      bool userDef = (*cbi)->IsUserDefined(); // @todo - or to all of them?
//      if (!userDef)
//      {
//         if ((*cbi)->SetStringParameter((*cbi)->GetParameterID("OrbitSpiceKernelName"), theSPKFilename) == false) return false;
//      }
//      ++cbi;
//   }
   return true;
}


//------------------------------------------------------------------------------
bool SolarSystem::SetLSKFile(const std::string &lskFile)
{
   lskKernelName = lskFile;
//   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
//   while (cbi != bodiesInUse.end())
//   {
//      bool userDef = (*cbi)->IsUserDefined(); // @todo - or to all of them?
//      if (!userDef)
//      {
//         if ((*cbi)->SetStringParameter((*cbi)->GetParameterID("LeapSecondKernelName"), lskKernelName) == false) return false;
//      }
//      ++cbi;
//   }
   return true;
}


//  bool SetOverrideTimeSystem(bool overrideIt)
//------------------------------------------------------------------------------
/**
 * This method sets the override time system flag.
 *
 * @param <overrideIt> override time system selection for all of the bodies?.
 *
 * @return success flag for the operation.
 *
 */
//------------------------------------------------------------------------------
bool SolarSystem::SetOverrideTimeSystem(bool overrideIt)
{
   #ifdef DEBUG_SS_SET
   MessageInterface::ShowMessage
      ("SolarSystem::SetOverrideTimeSystem() <%p> '%s' entered, overrideIt=%d\n",
       this, GetName().c_str(), overrideIt);
   #endif

   // Set it for each of the bodies
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      if ((*cbi)->SetOverrideTimeSystem(overrideIt) == false)  return false;
      ++cbi;
   }
   overrideTimeForAll = overrideIt;
   return true;
}


//------------------------------------------------------------------------------
// bool SetEphemUpdateInterval(Real intvl)
//------------------------------------------------------------------------------
bool SolarSystem::SetEphemUpdateInterval(Real intvl)
{
   if (intvl < 0.0)
   {
      SolarSystemException sse;
      sse.SetDetails(errorMessageFormat.c_str(),
                     GmatStringUtil::ToString(intvl, GetDataPrecision()).c_str(),
                     "Ephemeris Update Interval", "Real Number >= 0.0");
      throw sse;
   }

   // Set it for each of the bodies
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      if ((*cbi)->SetEphemUpdateInterval(intvl) == false)  return false;
      ++cbi;
   }
   ephemUpdateInterval = intvl;
   return true;
}


//------------------------------------------------------------------------------
// bool AddValidModelName(Gmat::ModelType m, const std::string &forBody,
//                        const std::string &theModel)
//------------------------------------------------------------------------------
bool SolarSystem::AddValidModelName(Gmat::ModelType m,
                  const std::string &forBody,
                  const std::string &theModel)
{
   for (std::vector<CelestialBody*>::iterator i = bodiesInUse.begin();
        i != bodiesInUse.end(); ++i)
   {
      if ((*i)->GetName() == forBody)
         return (*i)->AddValidModelName(m, theModel);
   }
   throw SolarSystemException("Cannot set new model for unknown body "
                              + forBody);
}

//------------------------------------------------------------------------------
// bool SolarSystem::RemoveValidModelName(Gmat::ModelType m,
//                  const std::string & forBody,
//                  const std::string &theModel)
//------------------------------------------------------------------------------
bool SolarSystem::RemoveValidModelName(Gmat::ModelType m,
                  const std::string & forBody,
                  const std::string &theModel)
{
   for (std::vector<CelestialBody*>::iterator i = bodiesInUse.begin();
        i != bodiesInUse.end(); ++i)
   {
      if ((*i)->GetName() == forBody)
         return (*i)->RemoveValidModelName(m, theModel);
   }
   throw SolarSystemException("Cannot remove model for unknown body "
                              + forBody);
}

//------------------------------------------------------------------------------
// Rvector6 SolarSystem::GetCelestialBodyState(const std::string &bodyName,
//                       CoordinateSystem *cs, const A1Mjd &epoch)
//------------------------------------------------------------------------------
Rvector6 SolarSystem::GetCelestialBodyState(const std::string &bodyName,
                                           CoordinateSystem *cs, const A1Mjd &epoch)
{
   // check to see if the body is a valid celestial body in this solar system
   if (!IsBodyInUse(bodyName))
   {
      std::string errmsg = "GetCelestialBodyState:: Body \"";
      errmsg += bodyName + "\" is not in use.\n";
      throw SolarSystemException(errmsg);
   }
   CelestialBody *body  = GetBody(bodyName);
   Rvector6 mj2000State = body->GetMJ2000State(epoch);
   Rvector state        = cs->FromMJ2000Eq(epoch, mj2000State);
   Rvector6 cbState(state[0],state[1],state[2],state[3],state[4],state[5]);
   return cbState;
}


//------------------------------------------------------------------------------
//  const StringArray& GetBodiesInUse() const
//------------------------------------------------------------------------------
/**
 * This method returns a list of strings representing the bodies in use.
 *
 * @return list of strings representing the bodies in use.
 *
 */
//------------------------------------------------------------------------------
const StringArray& SolarSystem::GetBodiesInUse() const
{
   return bodyStrings;
}

//------------------------------------------------------------------------------
//  SolarSystem* Clone() const
//------------------------------------------------------------------------------
/**
* This method returns a clone of the SolarSystem.
 *
 * @return a pointer to a clone of the SolarSystem.
 *
 */
//------------------------------------------------------------------------------
SolarSystem* SolarSystem::Clone() const
{
   // clone all objects in the Solar System as well
   SolarSystem * clonedSS = new SolarSystem(*this);
   
   #ifdef SS_CONSTRUCT_DESTRUCT
      MessageInterface::ShowMessage("Now cloning a new SolarSystem from <%p> to <%p>\n",
            this, clonedSS);
   #endif

   return clonedSS;

//   return new SolarSystem(*this);
}


//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Set this instance to match the one passed in.
 *
 * @param <orig> The object that is being copied.
 */
//---------------------------------------------------------------------------
void SolarSystem::Copy(const GmatBase* orig)
{
   // We dont want to clone the bodies so, just copy

   SolarSystem *ss = (SolarSystem*)orig;

   pvSrcForAll            = ss->pvSrcForAll;
   overrideTimeForAll     = ss->overrideTimeForAll;
   ephemUpdateInterval    = ss->ephemUpdateInterval;
   bodyStrings            = ss->bodyStrings;
   defaultBodyStrings     = ss->defaultBodyStrings;
   userDefinedBodyStrings = ss->userDefinedBodyStrings;

   // The SolarSystem has its own PlnetaryEphem files, so do not copy pointers
   //thePlanetaryEphem   = ss->thePlanetaryEphem;
   //theDefaultSlpFile   = ss->theDefaultSlpFile;
   //theDefaultDeFile    = ss->theDefaultDeFile;

   // copy the SS bodies
   for (std::vector<CelestialBody*>::const_iterator cbi = ss->bodiesInUse.begin();
        cbi != ss->bodiesInUse.end(); ++cbi)
   {
      CelestialBody *cb = (*cbi);
      CelestialBody *body = FindBody(cb->GetName());
      if (body != NULL)
      {
         body->Copy(cb);

         #ifdef DEBUG_SS_COPY
         MessageInterface::ShowMessage
            ("   Copied %s from <%p> to <%p>\n", cb->GetName().c_str(), cb, body);
         #endif
      }
      else
      {
         SolarSystemException ss;
         ss.SetDetails("%s not found in the solar system named "
                       "\"%s\"\n", cb->GetName().c_str(), GetName().c_str());
         throw ss;
      }
   }
}


//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string SolarSystem::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < SolarSystemParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
   return GmatBase::GetParameterText(id);
}

//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Integer SolarSystem::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < SolarSystemParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}

//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Gmat::ParameterType SolarSystem::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < SolarSystemParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];

   return GmatBase::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type string, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string SolarSystem::GetParameterTypeString(const Integer id) const
{
   return SolarSystem::PARAM_TYPE_STRING[GetParameterType(id)];
}

//------------------------------------------------------------------------------
//  Integer  GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
* This method returns the Integer parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  Integer value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Integer SolarSystem::GetIntegerParameter(const Integer id) const
{
   if (id == NUMBER_OF_BODIES)          return bodiesInUse.size();

   return GmatBase::GetIntegerParameter(id);
}


//------------------------------------------------------------------------------
//  Integer  GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
* This method returns the Integer parameter value, given the input
 * parameter label.
 *
 * @param <label> label for the requested parameter.
 *
 * @return  Integer value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Integer SolarSystem::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
Real SolarSystem::GetRealParameter(const Integer id) const
{
   if (id == EPHEM_UPDATE_INTERVAL) return ephemUpdateInterval;
   return GmatBase::GetRealParameter(id);
}


//------------------------------------------------------------------------------
// Real GetRealParameter(const std::string &label) const
//------------------------------------------------------------------------------
Real SolarSystem::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// Real SetRealParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
Real SolarSystem::SetRealParameter(const Integer id, const Real value)
{
   if (id == EPHEM_UPDATE_INTERVAL)
   {
      SetEphemUpdateInterval(value);
      return true;
   }
   return GmatBase::SetRealParameter(id, value);
}


//------------------------------------------------------------------------------
// Real SetRealParameter(const std::string &label, const Real value)
//------------------------------------------------------------------------------
Real SolarSystem::SetRealParameter(const std::string &label, const Real value)
{
   return SetRealParameter(GetParameterID(label),value);
}


//------------------------------------------------------------------------------
// bool GetBooleanParameter(const Integer id) const
//------------------------------------------------------------------------------
bool SolarSystem::GetBooleanParameter(const Integer id) const
{
   if (id == OVERRIDE_TIME_SYSTEM) return overrideTimeForAll;
   return GmatBase::GetBooleanParameter(id);
}


//------------------------------------------------------------------------------
// bool GetBooleanParameter(const std::string &label) const
//------------------------------------------------------------------------------
bool SolarSystem::GetBooleanParameter(const std::string &label) const
{
   return GetBooleanParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetBooleanParameter(const Integer id, const bool value)
//------------------------------------------------------------------------------
bool SolarSystem::SetBooleanParameter(const Integer id, const bool value)
{
   #ifdef DEBUG_SS_SET
   MessageInterface::ShowMessage
      ("SolarSystem::SetBooleanParameter() this=<%p> entered\n", this);
   #endif

   if (id == OVERRIDE_TIME_SYSTEM)
   {
      SetOverrideTimeSystem(value);
      return true;
   }
   return GmatBase::SetBooleanParameter(id, value);
}


//------------------------------------------------------------------------------
// bool SetBooleanParameter(const std::string &label, const bool value)
//------------------------------------------------------------------------------
bool SolarSystem::SetBooleanParameter(const std::string &label, const bool value)
{
   return SetBooleanParameter(GetParameterID(label), value);
}

//------------------------------------------------------------------------------
//  std::string  GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value, given the input
 * parameter ID.
 *
 * @param <id>    ID for the requested parameter.
 *
 * @return  string value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string SolarSystem::GetStringParameter(const Integer id) const
{
   if (id == EPHEMERIS)        return theCurrentPlanetarySource; // deprecated!!!!
   if (id == EPHEMERIS_SOURCE) return theCurrentPlanetarySource;    // pvSrcForAll (string of)?
   if (id == DE_FILE_NAME)     return thePlanetarySourceNames[Gmat::DE405];
   if (id == SPK_FILE_NAME)    return theSPKFilename;
   if (id == LSK_FILE_NAME)    return lskKernelName;

   return GmatBase::GetStringParameter(id);
}


//------------------------------------------------------------------------------
//  std::string  GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * This method returns the string parameter value, given the input
 * parameter label.
 *
 * @param <label> label for the requested parameter.
 *
 * @return  string value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string SolarSystem::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool SolarSystem::SetStringParameter(const Integer id,
                                     const std::string &value)
{
   if (id == EPHEMERIS)
   {
      StringArray parts = GmatStringUtil::SeparateBy(value, "{}, ");

      #ifdef DEBUG_SS_SET
      MessageInterface::ShowMessage
         ("SolarSystem::SetStringParameter() Has %d ephemeris types\n", parts.size());
      for (UnsignedInt i=0; i<parts.size(); i++)
         MessageInterface::ShowMessage
            ("   ephemType[%d] = '%s'\n", i, parts[i].c_str());
      #endif
      std::string  validSrc      = "";
      bool         validSrcFound = false;
      unsigned int pos           = 0;
      while ((!validSrcFound) && pos < parts.size())
      {
         validSrc = GmatStringUtil::RemoveAll(parts[pos], '\'');
         for (unsigned int jj = 0; jj < Gmat::PosVelSourceCount; jj++)
         {
            if (validSrc == Gmat::POS_VEL_SOURCE_STRINGS[jj])
            {
               validSrcFound = true;
               break;
            }
         }
         ++pos;
      }
      if (validSrcFound)
      {
         MessageInterface::ShowMessage
            ("*** WARNING *** \"Ephemeris\" is deprecated and will be "
             "removed from a future build; please use \"EphemerisSource\" "
             "instead.\nSetting ephemeris source to first valid source in list - \"%s\".\n",
             validSrc.c_str());
         SetSource(validSrc);
         return true;
      }
      else
      {
         std::string errmsg = "*** WARNING *** \"Ephemeris\" is deprecated and will be "
             "removed from a future build; please use \"EphemerisSource\" "
             "instead.\n*** ERROR *** No valid source string found.\n";
         throw SolarSystemException(errmsg);
      }
   }
   if (id == EPHEMERIS_SOURCE)
   {
      SetSource(value);
      return true;
   }
   if (id == DE_FILE_NAME)
   {
      if (value != thePlanetarySourceNames[Gmat::DE405])
      {
         CreateDeFile(Gmat::DE405, value);
         thePlanetarySourceNames[Gmat::DE405] = value;
         SetSourceFile(theDefaultDeFile);
      }
      return true;
   }
   if (id == SPK_FILE_NAME)
   {
      bool isOK = SetSPKFile(value);
      if (!isOK)
         throw SolarSystemException("Unable to set SPK file on one or more of the default bodies.\n");
      return true;
   }

   if (id == LSK_FILE_NAME)
   {
      bool isOK = SetLSKFile(value);
      if (!isOK)
         throw SolarSystemException("Unable to set LSK file on one or more of the default bodies.\n");
      return true;
   }

   return GmatBase::SetStringParameter(id, value);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
bool SolarSystem::SetStringParameter(const std::string &label,
                                     const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
//  const StringArray&   GetStringArrayParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the StringArray parameter value, given the input
 * parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return  StringArray value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
const StringArray& SolarSystem::GetStringArrayParameter(const Integer id) const
{
   if (id == BODIES_IN_USE)
      return bodyStrings;
   else if (id == EPHEMERIS)                // deprecated!!!!
      return thePlanetarySourceTypes;

   return GmatBase::GetStringArrayParameter(id);
}

//------------------------------------------------------------------------------
//  const StringArray&   GetStringArrayParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * This method returns the StringArray parameter value, given the input
 * parameter label.
 *
 * @param <label> label for the requested parameter.
 *
 * @return  StringArray value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
const StringArray&
SolarSystem::GetStringArrayParameter(const std::string &label) const
{
   return GetStringArrayParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// Integer GetOwnedObjectCount()
//------------------------------------------------------------------------------
Integer SolarSystem::GetOwnedObjectCount()
{
   return bodiesInUse.size();
}


//------------------------------------------------------------------------------
// GmatBase* GetOwnedObject(Integer whichOne)
//------------------------------------------------------------------------------
GmatBase* SolarSystem::GetOwnedObject(Integer whichOne)
{
   Integer count = bodiesInUse.size();
   if (whichOne < count)
      return bodiesInUse[whichOne];

   return NULL;
}

//------------------------------------------------------------------------------
// bool IsParameterReadOnly(const Integer id) const
//------------------------------------------------------------------------------
bool SolarSystem::IsParameterReadOnly(const Integer id) const
{
   // do not write out these items
   if ((id == BODIES_IN_USE) || (id == NUMBER_OF_BODIES))
      return true;
   if (id == EPHEMERIS)     // deprecated!!!!
      return true;

//   if ((theCurrentPlanetarySource == "DE405") && (id == SPK_FILE_NAME))
//      return true;
//
//   if ((theCurrentPlanetarySource == "SPICE") && (id == DE_FILE_NAME))
//      return true;
//
   return GmatBase::IsParameterReadOnly(id);
}


bool SolarSystem::IsParameterCloaked(const Integer id) const
{
   #ifdef DEBUG_SS_CLOAKING
      MessageInterface::ShowMessage("In SS:IsParameterCloaked with id = %d (%s)\n",
            id, (GetParameterText(id)).c_str());
   #endif
   if (!cloaking) return false;
   // if it's read-only, we'll cloak it
   if (IsParameterReadOnly(id)) return true;

   if (id >= GmatBaseParamCount && id < SolarSystemParamCount)
      return IsParameterEqualToDefault(id);

   return GmatBase::IsParameterCloaked(id);
}


bool SolarSystem::IsParameterEqualToDefault(const Integer id) const
{
   #ifdef DEBUG_SS_CLOAKING
      MessageInterface::ShowMessage("Entering SS:IsParameterEqualToDefault: id = %d (%s)\n",
            id, (GetParameterText(id)).c_str());
   #endif
   if (id == EPHEMERIS)    // deprecated!!!!
   {
//      #ifdef DEBUG_SS_PARAM_EQUAL
//            MessageInterface::ShowMessage("Checking equality for ephem filenames for SS\n");
//            MessageInterface::ShowMessage("size of default list is %d;   size of list is %d\n",
//                  default_planetarySourceTypesInUse.size(), thePlanetarySourceTypesInUse.size());
//      #endif
//      if (default_planetarySourceTypesInUse.size() != thePlanetarySourceTypesInUse.size()) return false;
//      for (unsigned int ii = 0; ii < default_planetarySourceTypesInUse.size(); ii++)
//      {
//         #ifdef DEBUG_SS_PARAM_EQUAL
//               MessageInterface::ShowMessage("    %s     vs.     %s\n",
//                     (default_planetarySourceTypesInUse.at(ii)).c_str(), (thePlanetarySourceTypesInUse.at(ii)).c_str());
//         #endif
//         if (default_planetarySourceTypesInUse.at(ii) != thePlanetarySourceTypesInUse.at(ii)) return false;
//      }
      return true;
   }
   if (id == EPHEMERIS_SOURCE)
   {
      #ifdef DEBUG_SS_PARAM_EQUAL
            MessageInterface::ShowMessage("Checking equality for ephem source for SS\n");
            MessageInterface::ShowMessage("    %s     vs.     %s\n",
                  default_ephemerisSource.c_str(), theCurrentPlanetarySource.c_str());
      #endif
      return (default_ephemerisSource == theCurrentPlanetarySource);
   }
   if (id == DE_FILE_NAME)
   {
      return (default_DEFilename == thePlanetarySourceNames[Gmat::DE405]);
   }
   if (id == SPK_FILE_NAME)
   {
      return (default_SPKFilename == theSPKFilename);
   }
   if (id == LSK_FILE_NAME)
   {
      return (default_LSKFilename == lskKernelName);
   }
   if (id == OVERRIDE_TIME_SYSTEM)
   {
      return (default_overrideTimeForAll == overrideTimeForAll);
   }
   if (id == EPHEM_UPDATE_INTERVAL)
   {
      return GmatMathUtil::IsEqual(default_ephemUpdateInterval,ephemUpdateInterval);
   }
   return GmatBase::IsParameterEqualToDefault(id);
}

bool SolarSystem::SaveAllAsDefault()
{
   #ifdef DEBUG_SS_CLOAKING
      MessageInterface::ShowMessage("Entering SS:SaveAllAsDefault\n");
      MessageInterface::ShowMessage(" default_ephemerisSource = \"%s\", theCurrentPlanetarySource = \"%s\"\n",
            default_ephemerisSource.c_str(), theCurrentPlanetarySource.c_str());
   #endif
   default_planetarySourceTypesInUse = thePlanetarySourceTypesInUse;  // deprecated!!!!
   default_ephemerisSource           = theCurrentPlanetarySource;
   default_DEFilename                = thePlanetarySourceNames[Gmat::DE405];
   default_SPKFilename               = theSPKFilename;
   default_LSKFilename               = lskKernelName;
   default_overrideTimeForAll        = overrideTimeForAll;
   default_ephemUpdateInterval       = ephemUpdateInterval;
#ifdef DEBUG_SS_CLOAKING
   MessageInterface::ShowMessage("EXITING SS:SaveAllAsDefault\n");
   MessageInterface::ShowMessage(" default_ephemerisSource = \"%s\", theCurrentPlanetarySource = \"%s\"\n",
         default_ephemerisSource.c_str(), theCurrentPlanetarySource.c_str());
#endif
   return true;
}

bool SolarSystem::SaveParameterAsDefault(const Integer id)
{
   if (id == EPHEMERIS) // deprecated!!!!
   {
      default_planetarySourceTypesInUse = thePlanetarySourceTypesInUse;
      return true;
   }
   if (id == EPHEMERIS_SOURCE)
   {
      default_ephemerisSource = theCurrentPlanetarySource;
      return true;
   }
   if (id == DE_FILE_NAME)
   {
      default_DEFilename = thePlanetarySourceNames[Gmat::DE405];
      return true;
   }
   if (id == SPK_FILE_NAME)
   {
      default_SPKFilename = theSPKFilename;
      return true;
   }
   if (id == LSK_FILE_NAME)
   {
      default_LSKFilename = lskKernelName;
      return true;
   }
   if (id == OVERRIDE_TIME_SYSTEM)
   {
      default_overrideTimeForAll = overrideTimeForAll;
      return true;
   }
   if (id == EPHEM_UPDATE_INTERVAL)
   {
      default_ephemUpdateInterval = ephemUpdateInterval;
      return true;
   }

   return GmatBase::SaveParameterAsDefault(id);
}



//------------------------------------------------------------------------------
// private methods
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// CelestialBody* FindBody(std::string withName)
//------------------------------------------------------------------------------
/**
* Return a pointer to a CelestialBody with the name withName.
 *
 * @param <withName>  name of body requested.
 *
 * @return pointer to a CelestialBody wiith the requested name.
 */
//------------------------------------------------------------------------------
CelestialBody* SolarSystem::FindBody(std::string withName)
{
   #ifdef DEBUG_SS_FIND_BODY
      MessageInterface::ShowMessage("In SS::FindBody (%s) at location <%p>, there are %d bodiesInUse\n",
            withName.c_str(), (this), (Integer) bodiesInUse.size());
   #endif
   // Search through bodiesInUse for the body with the name withName
   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      if ((*cbi)->GetName() == withName)
      {
         return (*cbi);
      }
      ++cbi;
   }

   return NULL;
}


//------------------------------------------------------------------------------
// void SetJ2000Body()
//------------------------------------------------------------------------------
/*
 * Sets J2000Body used for Earth to all bodies in the solar system.
 */
//------------------------------------------------------------------------------
void SolarSystem::SetJ2000Body()
{
   CelestialBody *earth = FindBody(SolarSystem::EARTH_NAME);

   if (earth != NULL)
   {
      std::string j2000BodyName = earth->GetJ2000BodyName();
      CelestialBody *j2000Body = FindBody(j2000BodyName);
      if (j2000Body != NULL)
      {
         std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
         while (cbi != bodiesInUse.end())
         {
            (*cbi)->SetJ2000Body(j2000Body);
            ++cbi;
         }
      }
   }
}


//------------------------------------------------------------------------------
// void CloneBodiesInUse(const SolarSystem &ss)
//------------------------------------------------------------------------------
void SolarSystem::CloneBodiesInUse(const SolarSystem &ss)
{
   #ifdef DEBUG_SS_CLONING
   MessageInterface::ShowMessage("   ===> Cloning %d bodies\n", ss.bodiesInUse.size());
   #endif

   bodiesInUse.clear();

   // clone the SS bodies
   for (std::vector<CelestialBody*>::const_iterator cbi = ss.bodiesInUse.begin();
        cbi != ss.bodiesInUse.end(); ++cbi)
   {
      CelestialBody *cb = (CelestialBody*)((*cbi)->Clone());
      bodiesInUse.push_back(cb);
      
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (cb, cb->GetName(), "SolarSystem::CloneBodiesInUse()",
          "CelestialBody *cb = (CelestialBody*)((*cbi)->Clone())");
      #endif
      #ifdef DEBUG_SS_CLONING
      MessageInterface::ShowMessage("   Cloned <%p> %s to <%p> %s\n",
            (*cbi), ((*cbi)->GetName()).c_str(), cb, cb->GetName().c_str());
      #endif
   }
   
   // set references to cloned bodies
   #ifdef DEBUG_SS_CLONING
      MessageInterface::ShowMessage("   there are now %d cloned celestial bodies\n",
                                    ((Integer) bodiesInUse.size()));
   #endif

   std::string   cbName   = "";
   CelestialBody *cb      = NULL;
   CelestialBody *central = NULL;
   for (std::vector<CelestialBody*>::const_iterator cbi = bodiesInUse.begin();
        cbi != bodiesInUse.end(); ++cbi)
   {
      cb = (*cbi);
      cbName  = cb->GetCentralBody();
      central = FindBody(cbName);
      if (central == NULL)
      {
         std::string errmsg = "Central Body \"" + cbName;
         errmsg += "\" for body \"" + cb->GetName();
         errmsg += "\" cannot be found.";
         throw SolarSystemException(errmsg);
      }
      #ifdef DEBUG_SS_CLONING
      MessageInterface::ShowMessage("   setting reference objects on %s\n",
                        (cb->GetName()).c_str());
      #endif
      cb->SetRefObject(central, Gmat::CELESTIAL_BODY, cbName);
   }
}


//------------------------------------------------------------------------------
// void DeleteBodiesInUse()
//------------------------------------------------------------------------------
void SolarSystem::DeleteBodiesInUse()
{
   #ifdef DEBUG_SS_CLONING
   MessageInterface::ShowMessage("   ===> Deleting %d bodies\n", bodiesInUse.size());
   #endif

   std::vector<CelestialBody*>::iterator cbi = bodiesInUse.begin();
   while (cbi != bodiesInUse.end())
   {
      #ifdef DEBUG_SS_CLONING
      MessageInterface::ShowMessage
         ("   Deleting <%p> %s\n", (*cbi), (*cbi)->GetName().c_str());
      #endif
      
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (*cbi, (*cbi)->GetName(), "SolarSystem::DeleteBodiesInUse()", " deleting body");
      #endif
      delete (*cbi);       // delete each body first
      (*cbi) = NULL;
      ++cbi;
   }
   
   bodiesInUse.clear();
   bodyStrings.clear();
   defaultBodyStrings.clear();
   userDefinedBodyStrings.clear();
}


//------------------------------------------------------------------------------
// void SetDefaultPlanetarySource()
//------------------------------------------------------------------------------
void SolarSystem::SetDefaultPlanetarySource()
{
   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage("SolarSystem setting default planetary source...\n");
   #endif

   // initialize planetary file types/names in use
   // Set DE405 as default
   thePlanetarySourceTypesInUse.clear();
   // put DE405 first, so that it is the default
   thePlanetarySourceTypesInUse.push_back(Gmat::POS_VEL_SOURCE_STRINGS[Gmat::DE405]);
   thePlanetarySourceTypesInUse.push_back(Gmat::POS_VEL_SOURCE_STRINGS[Gmat::TWO_BODY_PROPAGATION]);
   if (spiceAvailable) thePlanetarySourceTypesInUse.push_back(Gmat::POS_VEL_SOURCE_STRINGS[Gmat::SPICE]);

   SetPlanetarySourceTypesInUse(thePlanetarySourceTypesInUse);
}


////------------------------------------------------------------------------------
//// bool CreateSlpFile(const std::string &fileName)
////------------------------------------------------------------------------------
//bool SolarSystem::CreateSlpFile(const std::string &fileName)
//{
//   #ifdef DEBUG_SS_PLANETARY_FILE
//   MessageInterface::ShowMessage
//      ("SolarSystem::CreateSlpFile() fileName=%s\n", fileName.c_str());
//   #endif
//
//   bool status = false;
//
//   if (isPlanetarySourceInUse[SLP])
//   {
//      MessageInterface::ShowMessage
//         ("SolarSystem::CreateSlpFile() SlpFile already created\n");
//      status = true;
//   }
//   else
//   {
//      theDefaultSlpFile = new SlpFile(fileName);
//
//      #ifdef DEBUG_SS_PLANETARY_FILE
//      MessageInterface::ShowMessage("SolarSystem::CreateSlpFile() SlpFile created\n");
//      #endif
//
//      if (theDefaultSlpFile != NULL)
//         status = true;
//   }
//
//   return status;
//}
//

//------------------------------------------------------------------------------
// bool CreateDeFile(const Integer id, const std::string &fileName,
//                   Gmat::DeFileFormat format = Gmat::DE_BINARY)
//------------------------------------------------------------------------------
bool SolarSystem::CreateDeFile(Integer id, const std::string &fileName,
                               Gmat::DeFileFormat format)
{
   bool status = false;
   Gmat::DeFileType deFileType;

   if (theDefaultDeFile != NULL)
   {
      if (theDefaultDeFile->GetName() == fileName)
      {
         status = true;
         return status;
      }
      else
      {
         #ifdef DEBUG_SS_PLANETARY_FILE
         MessageInterface::ShowMessage
            ("SolarSystem::CreateDeFile() deleting old DeFile %s\n",
             (theDefaultDeFile->GetName()).c_str());
         #endif
         delete theDefaultDeFile;
         theDefaultDeFile = NULL;
      }
   }
   switch (id)
   {
   case Gmat::DE405:
      deFileType = Gmat::DE_DE405;
      break;
   default:
      MessageInterface::PopupMessage
         (Gmat::WARNING_,
          "SolarSystem::CreateDeFile() unsupported DE file type");
      return false;
   }

   #ifdef DEBUG_SS_PLANETARY_FILE
   MessageInterface::ShowMessage
      ("SolarSystem::CreateDeFile() creating DeFile. type=%d,\n   "
       "fileName=%s, format=%d\n", deFileType, fileName.c_str(),
       format);
   #endif

   FILE *defile = fopen(fileName.c_str(), "rb");
   if (defile == NULL)
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_,
          "Error opening DE file:%s. \n"
          "Please check file path. "
          "The next filetype in the list will be used.\n", fileName.c_str());
   }
   else
   {
      fclose(defile);

      try
      {
         theDefaultDeFile = new DeFile(deFileType, fileName, format);
         thePlanetaryEphem = theDefaultDeFile;
         #ifdef DEBUG_SS_PLANETARY_FILE
         MessageInterface::ShowMessage
            ("SolarSystem::CreateDeFile() NOW creating DeFile %s\n",
             fileName.c_str());
         #endif

         if (theDefaultDeFile != NULL)
            status = true;
      }
      catch (...)
      {
         MessageInterface::PopupMessage
            (Gmat::WARNING_,
             "SolarSystem::CreateDeFile() Error creating %s. "
             "The next filetype in the list will "
             "be created.\n", fileName.c_str());
      }
   }
   return status;
}
