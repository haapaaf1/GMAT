/*
 * MarsGRAM2005.hpp
 *
 *  Created on: May 24, 2011
 *      Author: tdnguye2
 */

#ifndef MarsGRAM2005_hpp
#define MarsGRAM2005_hpp

#include "AtmosphereModel.hpp"
#include "marsgram_defs.hpp"

struct MarsGRAM_Outputs
{
	double temperature;			// temperature (K)
	double pressure;			// pressure (N/m**2)
	double density;				// mean density (kg/m**3)
	double density_low;			// nominal low density (kg/m**3)
	double density_hi;			// nominal high density (kg/m**3)
	double denisty_perturbation;	// density perturbation (% of unperturbed mean)
	double EWWIND;				// mean east-west wind component (m/s)
	double EWpert;				// east-west wind component perturbation (m/s)
	double NSWIND;				// mean north-south wind component (m/s)
	double NSpert;				// north-south wind component perturbation (m/s)
	double VWpert;				// upward wind component perturbation (m/s)
	double Hrho;				// density scale height (km)
	double Hpres;				// pressure scale height (km)
	double ALS; 				// Planeto-centric longitude of Sun (degrees)
	double SZA;					// Solar zenith angle (degrees)
	double owlt;				// Mars-Earth one-way light time (minutes)
	double sunlat;				// Sub-solar latitude (degrees)
	double sunlon; 				// Sub_solar longitude (degrees)
	double MarsAU;				// Mars orbital radius (AU)
	double TLOCAL;				// Local solar time (Mars hours)
};

class MARSGRAM_API MarsGRAM2005 : public AtmosphereModel
{
public:
   MarsGRAM2005(const std::string &typeStr, const std::string &name = "");
   virtual ~MarsGRAM2005();
   MarsGRAM2005(const MarsGRAM2005 &opt);
   MarsGRAM2005&    operator=(const MarsGRAM2005& opt);
   // inherited from GmatBase
   virtual GmatBase*    Clone() const;
   virtual void         Copy(const GmatBase* orig);
   virtual bool         Initialize();

   bool Temperature(Real *position, Real *temperature, Real epoch = GmatTimeConstants::MJD_OF_J2000,
                           Integer count = 1);
   bool Pressure(Real *position, Real *pressure, Real epoch = GmatTimeConstants::MJD_OF_J2000,
                           Integer count = 1);
   bool Density(Real *position, Real *density, Real epoch = GmatTimeConstants::MJD_OF_J2000,
                           Integer count = 1);
   bool EastWestWind(Real *position, Real *EWwind, Real epoch = GmatTimeConstants::MJD_OF_J2000,
                           Integer count = 1);
   bool NorthSouthWind(Real *position, Real *NSwind, Real epoch = GmatTimeConstants::MJD_OF_J2000,
                           Integer count = 1);


   // Access methods overriden from the base class

   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   virtual bool         IsParameterReadOnly(const Integer id) const;
   virtual bool         IsParameterReadOnly(const std::string &label) const;

   virtual Real         GetRealParameter(const Integer id) const;
   virtual Real         GetRealParameter(const std::string &label) const;


   virtual std::string  GetStringParameter(const Integer id) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);



private:
   bool MGRAMCalculation(Real *position, Real ep);
   void JulianToDay(Real epoch, int* yr, int* mon, int* d,
		   int* hr, int* min, double* s);
   void DayToJulian(int yr, int mon, int d,
		   int hr, int min, double s, Real* ep);
   void ConvertToBinaryDataFiles();
   bool MarsGRAM();

   void SetTime(int yr, int mon, int d, int hr, int min, double s);
   void SetPosition(double lat, double lon, double hgt);
   void SetInputFile(std::string filename);
   void GetOutputs(struct MarsGRAM_Outputs* result);

   // input parameters:
   std::string inputfile;		// name of the input file
   Real epoch;
   double latitude, longitude, height;

   // output parameters:
   struct MarsGRAM_Outputs outputs;

   bool isInitialized;
   bool hasResult;

   int year, month, day, hour, minute;
   double sec;

public:
   // Parameter IDs
   enum
   {
	  INPUTFILENAME = AtmosphereModelParamCount,
      TEMPERATURE,
      PRESSURE,
      DENSITY,
      DENSITY_PERT,
      DENSITY_HI,
      DENSITY_LOW,
      EASTWEST_WIND,
      EASTWEST_WIND_PERT,
      NORTHSOUTH_WIND,
      NORTHSOUTH_WIND_PERT,
      UPWARD_WIND_PERT,
      DESITY_SCALE_HEIGHT,
      PRESSURE_SCALE_HEIGHT,
      ALS,
      SOLAR_ZENITH_ANGLE,
      MARS_EARTH_ONEWAY_LIGHT,
      SUN_LAT,
      SUN_LON,
      MARS_ORBITAL_RADIUS,
      LOCAL_SOLAR_TIME,
      MarsGRAM2005ParamCount
   };

   static const std::string
   PARAMETER_TEXT[MarsGRAM2005ParamCount - AtmosphereModelParamCount];
   static const Gmat::ParameterType
   PARAMETER_TYPE[MarsGRAM2005ParamCount - AtmosphereModelParamCount];

};

#endif /* MarsGRAM2005_hpp */
