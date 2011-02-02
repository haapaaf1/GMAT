//$Id$
//------------------------------------------------------------------------------
//                           SimpleExponentialAtmosphere
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under FDSS contract
// task 28
//
// Author: Darrel J. Conway
// Created: 2011/02/01
//
/**
 * A simple exponentially modeled atmosphere based on input parameters in the
 * STK GUI.
 */
//------------------------------------------------------------------------------


#include "SimpleExponentialAtmosphere.hpp"
#include <cmath>
#include "MessageInterface.hpp"

//------------------------------------------------------------------------------
// SimpleExponentialAtmosphere(const std::string &name = "")
//------------------------------------------------------------------------------
/**
 * Default constructor.
 */
//------------------------------------------------------------------------------
SimpleExponentialAtmosphere::SimpleExponentialAtmosphere(const std::string &name) :
   AtmosphereModel      ("SimpleExponential", name),
   scaleHeight          (8.5),
   refHeight            (0.0),
   refDensity           (1.217),
   geocentricAltitude   (false)
{
}


//------------------------------------------------------------------------------
// ~SimpleExponentialAtmosphere()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
SimpleExponentialAtmosphere::~SimpleExponentialAtmosphere()
{
}


//------------------------------------------------------------------------------
// SimpleExponentialAtmosphere(const SimpleExponentialAtmosphere& atm)
//------------------------------------------------------------------------------
/**
 * Copy constructor. (private implementation)
 *
 * @param atm SimpleExponentialAtmosphere object to copy into the new one.
 */
//------------------------------------------------------------------------------
SimpleExponentialAtmosphere::SimpleExponentialAtmosphere(
      const SimpleExponentialAtmosphere& atm) :
   AtmosphereModel      (atm),
   scaleHeight          (atm.scaleHeight),
   refHeight            (atm.refHeight),
   refDensity           (atm.refDensity),
   geocentricAltitude   (atm.geocentricAltitude)
{
}

//------------------------------------------------------------------------------
// SimpleExponentialAtmosphere& operator=(
//       const SimpleExponentialAtmosphere& bary)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the SimpleExponentialAtmosphere class.
 *
 * @param bary the SimpleExponentialAtmosphere object whose data to assign to
 *             "this" calculated point.
 *
 * @return "this" SimpleExponentialAtmosphere with data of input
 *                SimpleExponentialAtmosphere ea.
 */
//------------------------------------------------------------------------------
SimpleExponentialAtmosphere& SimpleExponentialAtmosphere::operator=(
      const SimpleExponentialAtmosphere &atm)
{
   if (&atm != this)
   {
      AtmosphereModel::operator=(atm);

      scaleHeight        = atm.scaleHeight;
      refHeight          = atm.refHeight;
      refDensity         = atm.refDensity;
      geocentricAltitude = atm.geocentricAltitude;
   }
   return *this;
}


//------------------------------------------------------------------------------
// bool Density(Real *position, Real *density, Real epoch, Integer count)
//------------------------------------------------------------------------------
/**
 * Calculates the density at each of the states in the input vector using
 * Vallado's method to interpolate the densities.
 * 
 * @param pos      The input vector of spacecraft states
 * @param density  The array of output densities
 * @param epoch    The current TAIJulian epoch (unused here)
 * @param count    The number of spacecraft contained in pos
 *
 * @return true on success, throws on failure.
 */
//------------------------------------------------------------------------------
bool SimpleExponentialAtmosphere::Density(Real *position, Real *density, Real epoch, Integer count)
{
   if (centralBodyLocation == NULL)
      throw AtmosphereException("Exponential atmosphere: Central body vector "
            "was not initialized");
        
   Real loc[3], height;
   Integer i;
    
   for (i = 0; i < count; ++i)
   {
      loc[0] = position[ i*6 ] - centralBodyLocation[0];
      loc[1] = position[i*6+1] - centralBodyLocation[1];
      loc[2] = position[i*6+2] - centralBodyLocation[2];
        
      height = CalculateHeight(loc);
      if (height < 0.0)
         throw AtmosphereException("Exponential atmosphere: Position vector is "
               "inside central body");

      density[i] = refDensity * exp(-(height - refHeight) / scaleHeight);
      #ifdef DEBUG_DENSITY
         MessageInterface::ShowMessage("SEAtmos: [%lf %lf %lf] -> ht: %lf -> "
               "density: %.12le\n", loc[0], loc[1], loc[2], height, density[i]);
      #endif
   }
    
   return true;
}


//------------------------------------------------------------------------------
// Real CalculateHeight(Real *loc)
//------------------------------------------------------------------------------
/**
 * Calculates the altitude used for the density calculation.
 * 
 * @param loc  The position vector pointing from the body with the atmosphere
 *             to the point of interest (typically a spacecraft location).
 * 
 * @return The height above the body's reference ellipsoid.
 */
//------------------------------------------------------------------------------
Real SimpleExponentialAtmosphere::CalculateHeight(Real *loc)
{
   Real mag = sqrt(loc[0]*loc[0] + loc[1]*loc[1] + loc[2]*loc[2]);
   Real cbr = cbRadius;

   if (!geocentricAltitude)
   {
      Real phi, cos_phi;

      // Compute the geocentric latitude in radians
      // (atan2() is safer than atan() since it avoids division by 0)

      phi = atan2(loc[2], sqrt(loc[0]*loc[0] + loc[1]*loc[1]));

      // Compute the cosine of phi
      cos_phi = cos(phi);

      // Return the geodetic height
      cbr = mag - cbRadius * (1.0 - cbFlattening) /
            sqrt(1 - cbFlattening * (2.0 - cbFlattening) * cos_phi * cos_phi);
   }

   return mag - cbr;
}


//------------------------------------------------------------------------------
// GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Clone the object (inherited from GmatBase).
 *
 * @return a clone of "this" object.
 */
//------------------------------------------------------------------------------
GmatBase* SimpleExponentialAtmosphere::Clone() const
{
   return (new SimpleExponentialAtmosphere(*this));
}
