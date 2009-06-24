//$Header$
//------------------------------------------------------------------------------
//                              Msise00Atmosphere
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/20
//
/**
 * The Msise00 atmosphere
 */
//------------------------------------------------------------------------------

#ifndef Msise00Atmosphere_hpp
#define Msise00Atmosphere_hpp

#include "AtmosphereModel.hpp"
 
/**
 * Wraps the Msise00 atmosphere code into the GMAT atmosphere model format.
 */
class Msise00Atmosphere : public AtmosphereModel
{
public:
   Msise00Atmosphere();
   virtual ~Msise00Atmosphere();
   Msise00Atmosphere(const Msise00Atmosphere& msise);
   Msise00Atmosphere&      operator=(const Msise00Atmosphere& msise);
    
   bool                    Density(Real *position, Real *density,
                                    Real epoch=21545.0,
                                    Integer count = 1);

   virtual GmatBase*       Clone() const; // inherited from GmatBase
protected:
   /// Flag to indicate if data comes from a file
   bool                    fileData;
   /// Name of the file
   std::string             fluxfilename;
   /// Second of day
   Real                    sod;
   /// Year + Day of year, in the form YYYYDDD
   Integer                 yd;
   /// Value of F10.7 to use
   Real                    f107;
   /// 3 month average of the F10.7 data
   Real                    f107a;
   /// Geomagnetic index (Ap, not Kp)
   Real                    ap[7];
   
   Integer                 mass;

   void                    GetInputs(Real epoch);
};

#endif // Msise00Atmosphere_hpp
