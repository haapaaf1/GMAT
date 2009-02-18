//$Id: Asteroid.cpp 5553 2008-06-03 16:46:30Z djcinsb $
//------------------------------------------------------------------------------
//                                  Asteroid
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Wendy C. Shoan
// Created: 2009.01.12
//
/**
 * Implementation of the Asteroid class.
 *
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "SolarSystem.hpp"
#include "SolarSystemException.hpp"
#include "CelestialBody.hpp"
#include "Asteroid.hpp"
#include "PhysicalConstants.hpp"
#include "MessageInterface.hpp"
#include "RealUtilities.hpp"
#include "AngleUtil.hpp"
#include "StringUtil.hpp"

//#define DEBUG_ASTEROID 1

// initialize static default values
// default values for CelesitalBody data
//const Gmat::BodyType        Asteroid::BODY_TYPE           = Gmat::ASTEROID;
//const Gmat::PosVelSource    Asteroid::POS_VEL_SOURCE      = Gmat::SPICE; 
//const Integer               Asteroid::ORDER               = 0; 
//const Integer               Asteroid::DEGREE              = 0;  
// // 2006.01.31 Equatorial radius - to match STK; was 1738.1; 
//const Real                  Asteroid::LUNA_EQUATORIAL_RADIUS   = 1738.2000;// km
//const Real                  Asteroid::LUNA_FLATTENING          = 0.0;
//// Units for mu are km^3 / s^2
//const Real                  Asteroid::LUNA_MU                  = 4902.8005821478;
//const Integer               Asteroid::LUNA_BODY_NUMBER         = 2; 
//const Integer               Asteroid::LUNA_REF_BODY_NUMBER     = 3; 
//
//const Rmatrix               Asteroid::LUNA_SIJ                 = Rmatrix(5,5,
//   0.0,                  0.0,                  0.0,                  0.0,
//   0.0,
//   0.0,                  0.0,                  0.0,                  0.0,
//   0.0,
//   0.0, 4.78976286742000E-09, 1.19043314469000E-08,                  0.0,
//   0.0,
//   0.0, 5.46564929895000E-06, 4.88875341590000E-06,-1.76416063010000E-06,
//   0.0,
//   0.0, 1.63304293851000E-06,-6.76012176494000E-06,-1.34287028168000E-05,
//   3.94334642990000E-06);
//const Rmatrix               Asteroid::LUNA_CIJ                 = Rmatrix(5,5,
//                     1.0,                 0.0,                  0.0,
//                     0.0,                 0.0,
//                     0.0,                 0.0,                  0.0,
//                     0.0,                 0.0,
//   -9.09314486280000E-05, 9.88441569067000E-09, 3.47139237760000E-05,
//                     0.0,                 0.0,
//   -3.17765981183000E-06, 2.63497832935000E-05, 1.42005317544000E-05,
//    1.22860504604000E-05,                 0.0,
//    3.21502582986000E-06,-6.01154071094000E-06,-7.10667037450000E-06,
//   -1.37041711834000E-06,-6.03652719918000E-06);
//
//const Real                  Asteroid::TWO_BODY_EPOCH      = 21544.500370768266;
//const Rvector6              Asteroid::TWO_BODY_ELEMENTS   = Rvector6(
//      385494.90434829952,  0.055908943292024992,   20.940245433093748,
//      12.233244412716252, 68.004298803147648,     137.94325682926458);
//

/// @todo add other ones as needed

//---------------------------------
// static data
//---------------------------------
//const std::string
//Asteroid::PARAMETER_TEXT[AsteroidParamCount - CelestialBodyParamCount] =
//{
//  
//};
//
//const Gmat::ParameterType
//Asteroid::PARAMETER_TYPE[AsteroidParamCount - CelestialBodyParamCount] =
//{
//   
//};


//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//  Asteroid(std::string name)
//------------------------------------------------------------------------------
/**
* This method creates an object of the Asteroid class
 * (default constructor).
 *
 * @param <name> optional parameter indicating the name of the celestial
 *               body (default is "").
 */
//------------------------------------------------------------------------------
Asteroid::Asteroid(std::string name) :
CelestialBody     ("Asteroid",name)
{
   CelestialBody::InitializeBody();
   
   objectTypeNames.push_back("Asteroid"); 
   parameterCount = AsteroidParamCount;
   
   theCentralBodyName  = SolarSystem::SUN_NAME; 
   bodyType            = Gmat::ASTEROID;
   bodyNumber          = -1;
   referenceBodyNumber = -1;
   
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

   
   // @todo - add other default values here

   DeterminePotentialFileNameFromStartup();

}

//------------------------------------------------------------------------------
//  Asteroid(std::string name, const std::string &cBody)
//------------------------------------------------------------------------------
/**
* This method creates an object of the Asteroid class
 * (constructor).
 *
 * @param <name> optional parameter indicating the name of the celestial
 *               body.
 * @param <cBody> pointer to a central body.
 */
//------------------------------------------------------------------------------
Asteroid::Asteroid(std::string name, const std::string &cBody) :
CelestialBody     ("Asteroid",name)
{
   CelestialBody::InitializeBody();
   
   objectTypeNames.push_back("Asteroid");
   parameterCount = AsteroidParamCount;

   theCentralBodyName  = cBody; 
   bodyType            = Gmat::ASTEROID;
   bodyNumber          = -1;
   referenceBodyNumber = -1;

   // @todo - add other default values here

   DeterminePotentialFileNameFromStartup();

}

//------------------------------------------------------------------------------
//  Asteroid(const Asteroid &copy)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the Asteroid class as a copy of the
 * specified Asteroid class (copy constructor).
 *
 * @param <copy> Asteroid object to copy.
 */
//------------------------------------------------------------------------------
Asteroid::Asteroid(const Asteroid &copy) :
CelestialBody (copy)
{
}

//------------------------------------------------------------------------------
//  Asteroid& operator= (const Asteroid& copy)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the Asteroid class.
 *
 * @param <copy> the Asteroid object whose data to assign to "this"
 *            solar system.
 *
 * @return "this" Asteroid with data of input Asteroid copy.
 */
//------------------------------------------------------------------------------
Asteroid& Asteroid::operator=(const Asteroid &copy)
{
   if (&copy == this)
      return *this;

   CelestialBody::operator=(copy);
   return *this;
}

//------------------------------------------------------------------------------
//  ~Asteroid()
//------------------------------------------------------------------------------
/**
 * Destructor for the Asteroid class.
 */
//------------------------------------------------------------------------------
Asteroid::~Asteroid()
{
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Asteroid.
 *
 * @return clone of the Asteroid.
 *
 */
//------------------------------------------------------------------------------
GmatBase* Asteroid::Clone() const
{
   return (new Asteroid(*this));
}


//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// private methods
//------------------------------------------------------------------------------
// none at this time

