//$Id$
//------------------------------------------------------------------------------
//                                  Planet
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Wendy C. Shoan
// Created: 2004/01/29
//
/**
 * Implementation of the Planet class.
 *
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "SolarSystem.hpp"
#include "CelestialBody.hpp"
#include "Rmatrix.hpp"
#include "Planet.hpp"
#include "CoordUtil.hpp"
#include "MessageInterface.hpp"
#include "PhysicalConstants.hpp"
#include "RealUtilities.hpp"
#include "SolarSystemException.hpp"
#include "TimeTypes.hpp"
#include "TimeSystemConverter.hpp"
#include "AngleUtil.hpp"
#include "StringUtil.hpp"

//#define DEBUG_PLANET 1
//#define DEBUG_PLANET_TWO_BODY

using namespace GmatMathUtil;

//---------------------------------
// static data
//---------------------------------
const std::string
Planet::PARAMETER_TEXT[PlanetParamCount - CelestialBodyParamCount] =
{
   "NutationUpdateInterval",
};

const Gmat::ParameterType
Planet::PARAMETER_TYPE[PlanetParamCount - CelestialBodyParamCount] =
{
   Gmat::REAL_TYPE,
};



//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//  Planet(std::string name)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the Planet class
 * (default constructor).
 *
 * @param <name> optional parameter indicating the name of the celestial
 *               body (default is "Sun").
 */
//------------------------------------------------------------------------------
Planet::Planet(std::string name) :
   CelestialBody     ("Planet",name),
   nutationUpdateInterval    (60.0)
{   
   objectTypeNames.push_back("Planet");
   parameterCount = PlanetParamCount;

   theCentralBodyName  = SolarSystem::SUN_NAME;
   bodyType            = Gmat::PLANET;
   bodyNumber          = 1;
   referenceBodyNumber = 3;
   rotationSrc         = Gmat::IAU_SIMPLIFIED;
   if (name == SolarSystem::EARTH_NAME) 
      rotationSrc      = Gmat::FK5_IAU_1980;
   else if (name == SolarSystem::NEPTUNE_NAME)
      rotationSrc      = Gmat::IAU_2002;

   // defaults for now ...
   Rmatrix s(5,5,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0);
   Rmatrix c(5,5,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0,
         0.0, 0.0,             0.0,             0.0,             0.0);
   sij = s;
   cij = c;

   DeterminePotentialFileNameFromStartup();
   SaveAllAsDefault();
}

//------------------------------------------------------------------------------
//  Planet(std::string name, const std::string &cBody)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the Planet class
 * (constructor).
 *
 * @param <name> optional parameter indicating the name of the celestial
 *               body.
 * @param <cBody> pointer to a central body.
 */
//------------------------------------------------------------------------------
Planet::Planet(std::string name, const std::string &cBody) :
   CelestialBody     ("Planet",name),
   nutationUpdateInterval    (60.0)
{
   objectTypeNames.push_back("Planet");
   parameterCount = PlanetParamCount;
   
   theCentralBodyName  = cBody;
   bodyType            = Gmat::PLANET;
   bodyNumber          = 1;
   referenceBodyNumber = 3;
   rotationSrc         = Gmat::IAU_SIMPLIFIED;
   if (name == SolarSystem::EARTH_NAME) 
      rotationSrc      = Gmat::FK5_IAU_1980;
   else if (name == SolarSystem::NEPTUNE_NAME)
      rotationSrc      = Gmat::IAU_2002;
   
   DeterminePotentialFileNameFromStartup();
   SaveAllAsDefault();
}

//------------------------------------------------------------------------------
//  Planet(const Planet &pl)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the Planet class as a copy of the
 * specified Planet class (copy constructor).
 *
 * @param <pl> Planet object to copy.
 */
//------------------------------------------------------------------------------
Planet::Planet(const Planet &pl) :
   CelestialBody  (pl),
   nutationUpdateInterval (pl.nutationUpdateInterval),
   default_nutationUpdateInterval (pl.default_nutationUpdateInterval)
{
}

//------------------------------------------------------------------------------
//  Planet& operator= (const Planet& pl)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the Planet class.
 *
 * @param <pl> the Planet object whose data to assign to "this"
 *            solar system.
 *
 * @return "this" Planet with data of input Planet st.
 */
//------------------------------------------------------------------------------
Planet& Planet::operator=(const Planet &pl)
{
   if (&pl == this)
      return *this;

   CelestialBody::operator=(pl);
   nutationUpdateInterval          = pl.nutationUpdateInterval;
   default_nutationUpdateInterval  = pl.default_nutationUpdateInterval;
   return *this;
}

//------------------------------------------------------------------------------
//  ~Planet()
//------------------------------------------------------------------------------
/**
 * Destructor for the Planet class.
 */
//------------------------------------------------------------------------------
Planet::~Planet()
{
}

//------------------------------------------------------------------------------
//  Rvector GetBodyCartographicCoordinates(const A1Mjd &forTime) const
//------------------------------------------------------------------------------
/**
 * This method returns the cartographic coordinates for the planet.
 *
 * @param <forTime> time for which to compute the cartographic coordinates.
 *
 * @return vector containing alpha, delta, W, Wdot.
 *
 * @note currently only implemented for the (currently known) planets of our
 *       Solar System.  See "Report of the IAU/IAG Working Group on
 *       Cartographic Coordinates and Rotational Elements of the Planets
 *       and Satellites: 2000"
 */
//------------------------------------------------------------------------------
Rvector Planet::GetBodyCartographicCoordinates(const A1Mjd &forTime) const
{
   // Neptune is the special case for the planets
   if (instanceName == SolarSystem::NEPTUNE_NAME)
   {
      if (rotationSrc == Gmat::IAU_2002)
      {
      Real d = GetJulianDaysFromTCBEpoch(forTime); // interval in Julian days
      Real T = d / 36525;                        // interval in Julian centuries
      Real N    = 357.85 + 52.316 * T;
      //Real NDot = 52.316 * CelestialBody::TDot;
      Real NDot = 6.0551e-04;   // per new specs 2004.02.22
      Real alpha     = orientation[0] + orientation[1] * Sin(Rad(N));
      Real delta     = orientation[2] + orientation[3] * Cos(Rad(N));
      Real W         = orientation[4] + orientation[5] * d - 0.48 * Sin(Rad(N));
      Real Wdot      = orientation[5] * CelestialBody::dDot - 0.48 * NDot * Cos(Rad(N));
      return Rvector(4, alpha, delta, W, Wdot);
      }
      else if (rotationSrc == Gmat::IAU_SIMPLIFIED)
      {
         return CelestialBody::GetBodyCartographicCoordinates(forTime);
      }
      else
      {
         std::string errmsg = "Error computing cartographic coordinates for Neptune - ";
         errmsg += "unknown or invalid rotation data source\n";
         throw SolarSystemException(errmsg);
      }
   }
   else if (instanceName == SolarSystem::EARTH_NAME)
   {
      // TBD - actually, the FK5 stuff is handled in the AxesSystem class(es) for 
      // which it is appropriate (e.g. BodyFixedAxes)
      return CelestialBody::GetBodyCartographicCoordinates(forTime);
   }

   // by default, call the method that handles the IAU_SIMPLIFIED method
   return CelestialBody::GetBodyCartographicCoordinates(forTime);
   
}

//------------------------------------------------------------------------------
//  Real GetHourAngle(A1Mjd atTime)
//------------------------------------------------------------------------------
/**
 * This method returns the hour angle for the body, referenced from the
 * Prime Meridian, measured westward
 *
 * @param <atTime> time for which to compute the hour angle
 *
 * @return hour angle for the body, in degrees, from the Prime Meridian
 *
 * @note algorithm 15, Vallado p. 192
 * @todo move this to Planet?  Add generic calculation here.
 *
 */
//------------------------------------------------------------------------------
Real  Planet::GetHourAngle(A1Mjd atTime) 
{
   if (instanceName == SolarSystem::EARTH_NAME)
   {
      // Convert the time to a UT1 MJD
      // 20.02.06 - arg: changed to use enum types instead of strings
      Real mjdUT1 = TimeConverterUtil::Convert(atTime.Get(),
                                 TimeConverterUtil::A1MJD, TimeConverterUtil::UT1MJD,
                                 GmatTimeUtil::JD_JAN_5_1941);
      Real jdUT1    = mjdUT1 + GmatTimeUtil::JD_JAN_5_1941; // right?
                                                            // Compute elapsed Julian centuries (UT1)
      Real tUT1     = (jdUT1 - 2451545.0) / 36525.0;
      //Real timeUt1  = (ut1Jd - 2451545.0) / 36525;
      
      // compute mean sidereal time, in degrees
      // according to Vallado Eq. 3-45, converted to degrees, where
      // 1 hour = 15 degrees and 1 second = 1/240 of a second
      
      Real mst        = (67310.54841 / 240) +
         (((876600 * 15) + (8640184.812866 / 240)) * tUT1) +
         ((0.093104 / 240) * tUT1 * tUT1) -
         ((6.2e-06 / 240) * tUT1 * tUT1 * tUT1);
      
      // reduce to a quantity within one day (86400 seconds, 360.0 degrees)
      hourAngle = AngleUtil::PutAngleInDegRange(mst,0.0,360.0);
      return hourAngle;
   }
   return CelestialBody::GetHourAngle(atTime);
}


//------------------------------------------------------------------------------
//  bool SetTwoBodyEpoch(const A1Mjd &toTime)
//------------------------------------------------------------------------------
/**
 * This method sets the epoch to be used for Low Fidelity analytic modeling.
 *
 * @param <toTime> epoch
 *
 * @return success of the operation.
 *
 */
//------------------------------------------------------------------------------
bool Planet::SetTwoBodyEpoch(const A1Mjd &toTime)
{
   #ifdef DEBUG_PLANET_TWO_BODY
      MessageInterface::ShowMessage(
      "In Planet::SetTwoBodyEpoch with time = %.12f\n", toTime.Get());
   #endif
   if (!CelestialBody::SetTwoBodyEpoch(toTime)) return false;
   bool OK = true;
   // For the Earth, send the information to the Sun
   if (instanceName == SolarSystem::EARTH_NAME)
   {
      if (!theCentralBody) 
         throw SolarSystemException("Central body must be set for " 
                                    + instanceName);
      #ifdef DEBUG_PLANET_TWO_BODY
         MessageInterface::ShowMessage(
         "-------- and setting central body's epoch time to %.12f\n", 
         toTime.Get());
      #endif
      OK = theCentralBody->SetTwoBodyEpoch(toTime);
   }
   return OK;
}

//------------------------------------------------------------------------------
//  bool SetTwoBodyElements(const Rvector6 &kepl)
//------------------------------------------------------------------------------
/**
 * This method sets the elements to be used for Low Fidelity analytic modeling.
 *
 * @param <kepl> initial keplerian elements.
 *
 * @return success of the operation.
 *
 */
//------------------------------------------------------------------------------
bool Planet::SetTwoBodyElements(const Rvector6 &kepl)
{
   #ifdef DEBUG_PLANET_TWO_BODY
      MessageInterface::ShowMessage(
      "In Planet::SetTwoBodyElements, kepl = \n%.12f %.12f %.12f %.12f %.12f %.12f\n", 
      kepl[0], kepl[1], kepl[2], kepl[3], kepl[4], kepl[5]);
   #endif
   if (!CelestialBody::SetTwoBodyElements(kepl)) return false;
   bool OK = true;
   // For the Earth, send the information to the Sun
   if (instanceName == SolarSystem::EARTH_NAME)
   {
      if (!theCentralBody) 
         throw SolarSystemException("Central body must be set for " 
                                    + instanceName);
      Real     ma;
      Real     totalMu = mu + theCentralBody->GetGravitationalConstant();
      Rvector6 cart    = - (CoordUtil::KeplerianToCartesian(kepl,
                            totalMu, CoordUtil::TA)); 
      Rvector6 sunKepl = CoordUtil::CartesianToKeplerian(cart, totalMu, &ma);

      #ifdef DEBUG_PLANET_TWO_BODY
         MessageInterface::ShowMessage(
         "-------- and setting central body's elements to \n %.12f %.12f %.12f %.12f %.12f %.12f\n", 
         sunKepl[0], sunKepl[1], sunKepl[2], 
         sunKepl[3], sunKepl[4], sunKepl[5]);
      #endif
      OK = theCentralBody->SetTwoBodyElements(sunKepl);
   }
   return OK;
}

Real Planet::GetNutationUpdateInterval() const 
{
   return nutationUpdateInterval;
//   return updateInterval;
}

bool Planet::SetNutationUpdateInterval(Real val)
{
   if (val < 0.0)
   {
      SolarSystemException sse;
      sse.SetDetails(errorMessageFormat.c_str(),
                     GmatStringUtil::ToString(val, GetDataPrecision()).c_str(),
                     "NutationUpdateInterval", "Real Number >= 0.0");
      throw sse;
   }
   
   nutationUpdateInterval = val;
   return true;
//   if (val < 0.0) return false;
//   updateInterval = val;
//   return true;
}



//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Planet.
 *
 * @return clone of the Planet.
 *
 */
//------------------------------------------------------------------------------
GmatBase* Planet::Clone(void) const
{
   return (new Planet(*this));
}

//------------------------------------------------------------------------------
// public methods inherited from GmatBase
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
* This method returns the parameter text, given the input parameter ID.
 *
 * @param id Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string Planet::GetParameterText(const Integer id) const
{
   if (id >= CelestialBodyParamCount && id < PlanetParamCount)
      return PARAMETER_TEXT[id - CelestialBodyParamCount];
   return CelestialBody::GetParameterText(id);
}

//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
* This method returns the parameter ID, given the input parameter string.
 *
 * @param str string for the requested parameter.
 *
 * @return ID for the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Integer Planet::GetParameterID(const std::string &str) const
{
   for (Integer i = CelestialBodyParamCount; i < PlanetParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - CelestialBodyParamCount])
         return i;
   }
   
   return CelestialBody::GetParameterID(str);
}

//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
* This method returns the parameter type, given the input parameter ID.
 *
 * @param id ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Gmat::ParameterType Planet::GetParameterType(const Integer id) const
{
   if (id >= CelestialBodyParamCount && id < PlanetParamCount)
      return PARAMETER_TYPE[id - CelestialBodyParamCount];
   
   return CelestialBody::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
* This method returns the parameter type string, given the input parameter ID.
 *
 * @param id ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
std::string Planet::GetParameterTypeString(const Integer id) const
{
   return CelestialBody::PARAM_TYPE_STRING[GetParameterType(id)];
}


bool Planet::IsParameterReadOnly(const Integer id) const
{
//   if (id == UPDATE_INTERVAL)
   if (id == NUTATION_UPDATE_INTERVAL)
      return false;
   return CelestialBody::IsParameterReadOnly(id);
}

//------------------------------------------------------------------------------
//  Real  GetRealParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
* This method returns the real value, given the input parameter ID.
 *
 * @param id ID for the requested parameter.
 *
 * @return real value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Real Planet::GetRealParameter(const Integer id) const
{
//   if (id == UPDATE_INTERVAL) return updateInterval;
   if (id == NUTATION_UPDATE_INTERVAL) return nutationUpdateInterval;
   return CelestialBody::GetRealParameter(id);
}

//------------------------------------------------------------------------------
//  Real  SetRealParameter(const Integer id, const Real value) 
//------------------------------------------------------------------------------
/**
* This method sets the real value, given the input parameter ID.
 *
 * @param id ID for the requested parameter.
 * @param value to use to set the parameter.
 *
 * @return real value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Real Planet::SetRealParameter(const Integer id, const Real value)
{
//   if (id == UPDATE_INTERVAL)
//   {
//      updateInterval = value;
//      return true;
//   }
   if (id == NUTATION_UPDATE_INTERVAL)
   {
        SetNutationUpdateInterval(value);
      return true;
   }
   return CelestialBody::SetRealParameter(id,value);
}

//------------------------------------------------------------------------------
//  Real  GetRealParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
* This method returns the real value, given the input parameter label.
 *
 * @param label label for the requested parameter.
 *
 * @return real value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Real Planet::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
//  Real  SetRealParameter(const std::string &label, const Real value) 
//------------------------------------------------------------------------------
/**
* This method sets the real value, given the input parameter label.
 *
 * @param label label for the requested parameter.
 * @param value to use to set the parameter.
 *
 * @return real value of the requested parameter.
 *
 */
//------------------------------------------------------------------------------
Real Planet::SetRealParameter(const std::string &label, const Real value)
{
   return SetRealParameter(GetParameterID(label), value);
}

bool Planet::IsParameterCloaked(const Integer id) const
{
   if (!cloaking) return false;
   // if it's read-only, we'll cloak it
   if (IsParameterReadOnly(id)) return true;

   if (id >= CelestialBodyParamCount && id < PlanetParamCount)
      return IsParameterEqualToDefault(id);
   
   return CelestialBody::IsParameterCloaked(id);
}

bool Planet::IsParameterEqualToDefault(const Integer id) const
{
   if (id == NUTATION_UPDATE_INTERVAL)
   {
      if (default_nutationUpdateInterval == nutationUpdateInterval)     return true;
      else                                                              return false;
   }
   return CelestialBody::IsParameterEqualToDefault(id);
}

bool Planet::SaveAllAsDefault()
{
   CelestialBody::SaveAllAsDefault();
   default_nutationUpdateInterval = nutationUpdateInterval;
   return true;
}

bool Planet::SaveParameterAsDefault(const Integer id)
{
   if (id == NUTATION_UPDATE_INTERVAL)  
   {
      default_nutationUpdateInterval = nutationUpdateInterval;
      return true;
   }
   return CelestialBody::SaveParameterAsDefault(id);
}

//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------
// none at this time

//------------------------------------------------------------------------------
// private methods
//------------------------------------------------------------------------------
// none at this time
