//$Id: Comet.cpp 5553 2008-06-03 16:46:30Z djcinsb $
//------------------------------------------------------------------------------
//                                  Comet
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Wendy C. Shoan
// Created: 2009.01.12
//
/**
 * Implementation of the Comet class.
 *
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "SolarSystem.hpp"
#include "SolarSystemException.hpp"
#include "CelestialBody.hpp"
#include "Comet.hpp"
#include "PhysicalConstants.hpp"
#include "MessageInterface.hpp"
#include "RealUtilities.hpp"
#include "AngleUtil.hpp"
#include "StringUtil.hpp"

//#define DEBUG_COMET 1

// initialize static default values
// default values for CelesitalBody data
//const Gmat::BodyType        Comet::BODY_TYPE           = Gmat::COMET;
//const Gmat::PosVelSource    Comet::POS_VEL_SOURCE      = Gmat::SPICE; 
//const Integer               Comet::ORDER               = 0; 
//const Integer               Comet::DEGREE              = 0;  
// // 2006.01.31 Equatorial radius - to match STK; was 1738.1; 
//const Real                  Comet::LUNA_EQUATORIAL_RADIUS   = 1738.2000;// km
//const Real                  Comet::LUNA_FLATTENING          = 0.0;
//// Units for mu are km^3 / s^2
//const Real                  Comet::LUNA_MU                  = 4902.8005821478;
//const Integer               Comet::LUNA_BODY_NUMBER         = 2; 
//const Integer               Comet::LUNA_REF_BODY_NUMBER     = 3; 
//
//const Rmatrix               Comet::LUNA_SIJ                 = Rmatrix(5,5,
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
//const Rmatrix               Comet::LUNA_CIJ                 = Rmatrix(5,5,
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
//const Real                  Comet::TWO_BODY_EPOCH      = 21544.500370768266;
//const Rvector6              Comet::TWO_BODY_ELEMENTS   = Rvector6(
//      385494.90434829952,  0.055908943292024992,   20.940245433093748,
//      12.233244412716252, 68.004298803147648,     137.94325682926458);
//

/// @todo add other ones as needed

//---------------------------------
// static data
//---------------------------------
//const std::string
//Comet::PARAMETER_TEXT[CometParamCount - CelestialBodyParamCount] =
//{
//  
//};
//
//const Gmat::ParameterType
//Comet::PARAMETER_TYPE[CometParamCount - CelestialBodyParamCount] =
//{
//   
//};


//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//  Comet(std::string name)
//------------------------------------------------------------------------------
/**
* This method creates an object of the Comet class
 * (default constructor).
 *
 * @param <name> optional parameter indicating the name of the celestial
 *               body (default is "").
 */
//------------------------------------------------------------------------------
Comet::Comet(std::string name) :
CelestialBody     ("Comet",name)
{
   CelestialBody::InitializeBody();
   
   objectTypeNames.push_back("Comet"); 
   parameterCount = CometParamCount;
   
   theCentralBodyName  = SolarSystem::SUN_NAME; 
   bodyType            = Gmat::COMET;
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
//  Comet(std::string name, const std::string &cBody)
//------------------------------------------------------------------------------
/**
* This method creates an object of the Comet class
 * (constructor).
 *
 * @param <name> optional parameter indicating the name of the celestial
 *               body.
 * @param <cBody> pointer to a central body.
 */
//------------------------------------------------------------------------------
Comet::Comet(std::string name, const std::string &cBody) :
CelestialBody     ("Comet",name)
{
   CelestialBody::InitializeBody();
   
   objectTypeNames.push_back("Comet");
   parameterCount = CometParamCount;

   theCentralBodyName  = cBody; 
   bodyType            = Gmat::COMET;
   bodyNumber          = -1;
   referenceBodyNumber = -1;

   // @todo - add other default values here

   DeterminePotentialFileNameFromStartup();

}

//------------------------------------------------------------------------------
//  Comet(const Comet &copy)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the Comet class as a copy of the
 * specified Comet class (copy constructor).
 *
 * @param <copy> Comet object to copy.
 */
//------------------------------------------------------------------------------
Comet::Comet(const Comet &copy) :
CelestialBody (copy)
{
}

//------------------------------------------------------------------------------
//  Comet& operator= (const Comet& copy)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the Comet class.
 *
 * @param <copy> the Comet object whose data to assign to "this"
 *            solar system.
 *
 * @return "this" Comet with data of input Comet copy.
 */
//------------------------------------------------------------------------------
Comet& Comet::operator=(const Comet &copy)
{
   if (&copy == this)
      return *this;

   CelestialBody::operator=(copy);
   return *this;
}

//------------------------------------------------------------------------------
//  ~Comet()
//------------------------------------------------------------------------------
/**
 * Destructor for the Comet class.
 */
//------------------------------------------------------------------------------
Comet::~Comet()
{
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Comet.
 *
 * @return clone of the Comet.
 *
 */
//------------------------------------------------------------------------------
GmatBase* Comet::Clone() const
{
   return (new Comet(*this));
}


//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// private methods
//------------------------------------------------------------------------------
// none at this time

