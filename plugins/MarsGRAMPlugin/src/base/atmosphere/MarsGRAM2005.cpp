/*
 * MarsGRAM2005.cpp
 *
 *  Created on: May 24, 2011
 *      Author: tdnguye2
 */

#include "MarsGRAM2005.hpp"
#include "f2c.h"


//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------

const std::string
MarsGRAM2005::PARAMETER_TEXT[
       MarsGRAM2005ParamCount - AtmosphereModelParamCount] =
{
   "InputFileName",
   "Temperature",
   "Pressure",
   "Density"
   "DensityPerturbation",
   "DensityHigh",
   "DensityLow",
   "EWwind",
   "EWwindPerturbation",
   "NSwind",
   "NSwindPerturbation",
   "UpwindPerturbation",
   "DensityScaleHeight",
   "PressureScaleHeight",
   "PlanetoCentricLongitudeOfSun",
   "SolarZenithAngle",
   "MarsEarthOnewayLightTime",
   "SubSolarLatitude",
   "SubSolarLongitude",
   "MarsOrbitalRadius",
   "LocalSolarTime",
};

const Gmat::ParameterType
MarsGRAM2005::PARAMETER_TYPE[
       MarsGRAM2005ParamCount - AtmosphereModelParamCount] =
{
   Gmat::STRING_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
};



MarsGRAM2005::MarsGRAM2005(const std::string &typeStr, const std::string &name):
	AtmosphereModel(typeStr, name)
{
	centralBody = "Mars";
	isInitialized = false;
	hasResult = false;

	inputfile = "../data/MGRAM_Data/inputstd0.txt";
}

MarsGRAM2005::~MarsGRAM2005()
{
	// TODO Auto-generated destructor stub
}

//------------------------------------------------------------------------------
// MarsGRAM2005(const MarsGRAM2005 &mgram)
//------------------------------------------------------------------------------
MarsGRAM2005::MarsGRAM2005(const MarsGRAM2005 &mgram) :
   AtmosphereModel       (mgram)
{
   // input file:
   inputfile = mgram.inputfile;

   // time:
   epoch = mgram.epoch;
   year = mgram.year;
   month = mgram.month;
   day = mgram.day;
   hour = mgram.hour;
   minute = mgram.minute;
   sec = mgram.sec;

   // position:
   latitude = mgram.latitude;
   longitude = mgram.longitude;
   height = mgram.height;

   // status and result:
   isInitialized = mgram.isInitialized;
   hasResult = mgram.hasResult;
}


//------------------------------------------------------------------------------
// MarsGRAM2005& operator=(const MarsGRAM2005& mgram)
//------------------------------------------------------------------------------
MarsGRAM2005&
MarsGRAM2005::operator=(const MarsGRAM2005& mgram)
{
   if (&mgram == this)
      return *this;

   AtmosphereModel::operator=(mgram);

   inputfile = mgram.inputfile;

   epoch = mgram.epoch;
   year = mgram.year;
   month = mgram.month;
   day = mgram.day;
   hour = mgram.hour;
   minute = mgram.minute;
   sec = mgram.sec;

   latitude = mgram.latitude;
   longitude = mgram.longitude;
   height = mgram.height;

   isInitialized = mgram.isInitialized;
   hasResult = mgram.hasResult;

   return *this;
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the MarsGRAM2005.
 *
 * @return clone of the MarsGRAM2005.
 */
//------------------------------------------------------------------------------
GmatBase* MarsGRAM2005::Clone() const
{
   return (new MarsGRAM2005(*this));
}

//---------------------------------------------------------------------------
//  void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 *
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void MarsGRAM2005::Copy(const GmatBase* orig)
{
   operator=(*((MarsGRAM2005 *)(orig)));
}

//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
bool MarsGRAM2005::Initialize()
{
   epoch = 0.0;
   year = 2000;
   month = 1;
   day = 1;
   hour = 0;
   minute = 0;
   sec = 0.0;
   latitude = 0.0;
   longitude = 0.0;
   height = 100;

   return (MarsGRAM());
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
 */
//------------------------------------------------------------------------------
std::string MarsGRAM2005::GetParameterText(const Integer id) const
{
   if ((id >= AtmosphereModelParamCount) && (id < MarsGRAM2005ParamCount))
      return PARAMETER_TEXT[id - AtmosphereModelParamCount];

   return AtmosphereModel::GetParameterText(id);
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
 */
//------------------------------------------------------------------------------
Integer MarsGRAM2005::GetParameterID(const std::string &str) const
{

   for(int i=AtmosphereModelParamCount; i < MarsGRAM2005ParamCount; ++i)
   {
	  if (PARAMETER_TEXT[i - AtmosphereModelParamCount] == str)
		 return i;
   }

   return AtmosphereModel::GetParameterID(str);
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
 */
//------------------------------------------------------------------------------
Gmat::ParameterType MarsGRAM2005::GetParameterType(const Integer id) const
{
   if ((id >= AtmosphereModelParamCount) && (id < MarsGRAM2005ParamCount))
      return PARAMETER_TYPE[id - AtmosphereModelParamCount];

   return AtmosphereModel::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type string, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 */
//------------------------------------------------------------------------------
std::string MarsGRAM2005::GetParameterTypeString(const Integer id) const
{
   return AtmosphereModel::PARAM_TYPE_STRING[GetParameterType(id)];
}

//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool MarsGRAM2005::IsParameterReadOnly(const Integer id) const
{
   if (id == INPUTFILENAME)
	  return false;

   if (id >= TEMPERATURE && id <= LOCAL_SOLAR_TIME)
      return true;

   return AtmosphereModel::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
// bool IsParameterReadOnly(const std::string &label) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not.
 */
//---------------------------------------------------------------------------
bool MarsGRAM2005::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


Real MarsGRAM2005::GetRealParameter(const Integer id) const
{
	switch (id)
	{
	case TEMPERATURE:
		return outputs.temperature;
		break;
	case PRESSURE:
		return outputs.pressure;
		break;
	case DENSITY:
		return outputs.density;
		break;
	case DENSITY_PERT:
		return outputs.denisty_perturbation;
		break;
	case DENSITY_HI:
		return outputs.density_hi;
		break;
	case DENSITY_LOW:
		return outputs.density_low;
		break;
	case EASTWEST_WIND:
		return outputs.EWWIND;
		break;
	case EASTWEST_WIND_PERT:
		return outputs.EWpert;
		break;
	case NORTHSOUTH_WIND:
		return outputs.NSWIND;
		break;
	case NORTHSOUTH_WIND_PERT:
		return outputs.NSpert;
		break;
	case UPWARD_WIND_PERT:
		return outputs.VWpert;
		break;
	case DESITY_SCALE_HEIGHT:
		return outputs.Hrho;
		break;
	case PRESSURE_SCALE_HEIGHT:
		return outputs.Hpres;
		break;
	case ALS:
		return outputs.ALS;
		break;
	case SOLAR_ZENITH_ANGLE:
		return outputs.SZA;
		break;
	case MARS_EARTH_ONEWAY_LIGHT:
		return outputs.owlt;
		break;
	case SUN_LAT:
		return outputs.sunlat;
		break;
	case SUN_LON:
		return outputs.sunlon;
		break;
	case MARS_ORBITAL_RADIUS:
		return outputs.MarsAU;
		break;
	case LOCAL_SOLAR_TIME:
		return outputs.TLOCAL;
		break;
	}

	return AtmosphereModel::GetRealParameter(id);
}


Real MarsGRAM2005::GetRealParameter(const std::string &label) const
{
	return GetRealParameter(GetParameterID(label));
}


std::string  MarsGRAM2005::GetStringParameter(const Integer id) const
{
	if (id == INPUTFILENAME)
	   return inputfile;

	return GmatBase::GetStringParameter(id);
}

bool MarsGRAM2005::SetStringParameter(const Integer id, const std::string &value)
{
	if (id == INPUTFILENAME)
	{
	   inputfile = value;
	   return true;
	}

	return GmatBase::SetStringParameter(id, value);
}

std::string  MarsGRAM2005::GetStringParameter(const std::string &label) const
{
	return GetStringParameter(GetParameterID(label));
}

bool MarsGRAM2005::SetStringParameter(const std::string &label, const std::string &value)
{
	return SetStringParameter(GetParameterID(label), value);
}


bool MarsGRAM2005::MGRAMCalculation(Real *position, Real ep)
{

	if ((hasResult == false) ||
		(latitude != position[0])||(longitude != position[1])||
		(height != position[3])||(epoch != ep))
	{
		latitude = position[0];
		longitude = position[1];
		height = position[2];
		epoch = ep;

		JulianToDay(epoch, &year, &month, &day, &hour, &minute, &sec);
//        printf("      year = %d, month = %d, day = %d\n", year, month, day);
//        printf("      hour = %d, minute = %d, second = %f\n\n", hour, minute, sec);

        hasResult = MarsGRAM();
	}
	return hasResult;
}


bool MarsGRAM2005::Temperature(Real *position,
		Real *temperature, Real ep, Integer count)
{
    if (MGRAMCalculation(position, ep))
    {
    	*temperature = outputs.temperature;
    	return true;
    }
    else
    	return false;
}

bool MarsGRAM2005::Pressure(Real *position,
		Real *pressure, Real ep, Integer count)
{
    if (MGRAMCalculation(position, ep))
    {
    	*pressure = outputs.pressure;
    	return true;
    }
    else
    	return false;
}

bool MarsGRAM2005::Density(Real *position,
		Real *density, Real ep, Integer count)
{
    if (MGRAMCalculation(position, ep))
    {
    	*density = outputs.density;
    	return true;
    }
    else
    	return false;
}


bool MarsGRAM2005::EastWestWind(Real *position,
		Real *EWwind, Real ep, Integer count)
{
    if (MGRAMCalculation(position, ep))
    {
    	*EWwind = outputs.EWWIND;
    	return true;
    }
    else
    	return false;
}

bool MarsGRAM2005::NorthSouthWind(Real *position,
		Real *NSwind, Real ep, Integer count)
{
    if (MGRAMCalculation(position, ep))
    {
    	*NSwind = outputs.NSWIND;
    	return true;
    }
    else
    	return false;
}




extern "C" int jultocal_(doublereal *jd, integer *year, integer *month,
	integer *day, doublereal *dayfrac);
void MarsGRAM2005::JulianToDay(double epoch, int* yr, int* mon, int* d, int* hr, int* min, double* s)
{
	integer y1,m1,d1;
	double dayfrac;
	double ep = epoch + 2429999.9995366395;
	jultocal_(&ep, &y1, &m1, &d1, &dayfrac);
	*yr = (int)y1;
	*mon = (int)m1;
	*d = (int)d1;
	*hr = (int)(dayfrac*24);
	*min = (int)((dayfrac*24 - (*hr))*60);
	*s = dayfrac*24*3600 - (*hr)*3600 - (*min)*60;

}

extern "C" int caltojul_(integer *iy, integer *im, integer *id, integer
	*ihour, integer *imin, doublereal *sec, doublereal *xjd);
void MarsGRAM2005::DayToJulian(int yr, int mon, int d, int hr, int min, double s, Real* ep)
{
	integer y1,m1,d1,h1,min1;
	doublereal sec;

	y1 = yr; m1 = mon; d1 = d; h1 = hr; min1 = min; sec = s;
	caltojul_(&y1, &m1, &d1, &h1, &min1, &sec, ep);

}


extern "C" int CreateMGRAMBinaryFiles();
extern "C" int CreateMOLADataFile();
extern "C" int CreateAlbFile();
void MarsGRAM2005::ConvertToBinaryDataFiles()
{
	CreateMGRAMBinaryFiles();
	CreateMOLADataFile();
	CreateAlbFile();
}

void MarsGRAM2005::SetInputFile(std::string filename)
{
	inputfile = filename;
}

void MarsGRAM2005::SetTime(int yr, int mon, int d, int hr, int min, double s)
{
	year = yr;
	month = mon;
	day = d;
	hour = hr;
	minute = min;
	sec = s;
}

void MarsGRAM2005::SetPosition(double lat, double lon, double hgt)
{
	latitude = lat;
	longitude = lon;
	height = hgt;
}

void MarsGRAM2005::GetOutputs(struct MarsGRAM_Outputs* result)
{
	result->ALS 		= outputs.ALS;
	result->EWWIND 		= outputs.EWWIND;
	result->EWpert 		= outputs.EWpert;
	result->Hpres 		= outputs.Hpres;
	result->Hrho 		= outputs.Hrho;
	result->MarsAU 		= outputs.MarsAU;
	result->NSWIND 		= outputs.NSWIND;
	result->NSpert 		= outputs.NSpert;
	result->VWpert		= outputs.VWpert;
	result->SZA			= outputs.SZA;
	result->TLOCAL 		= outputs.TLOCAL;
	result->denisty_perturbation 	= outputs.denisty_perturbation;
	result->density 	= outputs.density;
	result->density_hi	= outputs.density_hi;
	result->density_low	= outputs.density_low;
	result->owlt		= outputs.owlt;
	result->pressure	= outputs.pressure;
	result->sunlat		= outputs.sunlat;
	result->sunlon		= outputs.sunlon;
	result->temperature	= outputs.temperature;

}

extern "C" int mgram_m05__(integer *intyear, integer *intmonth, integer
		*intday, integer *inthour, integer *intmin, doublereal *rsec,
		doublereal *rlat, doublereal *rlon, doublereal *rhgt, doublereal *
		temp, doublereal *pres, doublereal *dens, doublereal *denslo,
		doublereal *denshi, doublereal *densp, doublereal *ewwind, doublereal
		*ewpert, doublereal *nswind, doublereal *nspert, doublereal *vwpert,
		doublereal *hrho, doublereal *hpres, doublereal *als, doublereal *sza,
		 doublereal *sunlat, doublereal *sunlon, doublereal *owlt, doublereal
		*marsau, doublereal *tlocal, integer *isload, char *inputfl, ftnlen
		inputfl_len);

bool MarsGRAM2005::MarsGRAM()
{
	integer isload = 0;
	char filename[100];
	strcpy(&filename[0], inputfile.c_str());
	ftnlen len = strlen(filename);
	if (isInitialized)
		isload = 1;

	mgram_m05__((integer*)&year, (integer*)&month, (integer*)&day, (integer*)&hour, (integer*)&minute,
			(doublereal*)&sec,
			(doublereal*)&latitude, (doublereal*)&longitude, (doublereal*)&height,
			(doublereal*)&outputs.temperature, (doublereal*)&outputs.pressure,
			(doublereal*)&outputs.density, (doublereal*)&outputs.density_low,
			(doublereal*)&outputs.density_hi, (doublereal*)&outputs.denisty_perturbation,
			(doublereal*)&outputs.EWWIND, (doublereal*)&outputs.EWpert,
			(doublereal*)&outputs.NSWIND, (doublereal*)&outputs.NSpert,
			(doublereal*)&outputs.VWpert,
			(doublereal*)&outputs.Hrho, (doublereal*)&outputs.Hpres,
			(doublereal*)&outputs.ALS, (doublereal*)&outputs.SZA,
			(doublereal*)&outputs.sunlat, (doublereal*)&outputs.sunlon,
			(doublereal*)&outputs.owlt, (doublereal*)&outputs.MarsAU, (doublereal*)&outputs.TLOCAL,
			&isload, &filename[0], len);

	isInitialized = true;

	return true;
}
