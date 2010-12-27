//$Header$
//------------------------------------------------------------------------------
//                                  Comet
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Wendy C. Shoan
// Created: 2009.01.12
//
/**
 * The Comet class will contain all data and methods for any comet that exists in
 * the solar system.
 *
 */
//------------------------------------------------------------------------------


#ifndef Comet_hpp
#define Comet_hpp

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "Rmatrix.hpp"
#include "SolarSystem.hpp"
#include "CelestialBody.hpp"

class GMAT_API Comet : public CelestialBody
{
public:
   // default constructor, with optional name
   Comet(std::string name = "");
   // additional constructor
   Comet(std::string name, const std::string &cBody);
   // copy constructor
   Comet(const Comet &copy);
   // operator=
   Comet& operator=(const Comet &copy);
   // destructor
   virtual ~Comet();

   // inherited from GmatBase
   virtual GmatBase* Clone() const;
   // required method for all subclasses that can be copied in a script
   virtual void      Copy(const GmatBase* orig);


protected:
   enum
   {
      CometParamCount = CelestialBodyParamCount,
      
   };
   
   //static const std::string PARAMETER_TEXT[CometParamCount - CelestialBodyParamCount];
   
   //static const Gmat::ParameterType 
   //                         PARAMETER_TYPE[CometParamCount - CelestialBodyParamCount];


private:

};
#endif // Comet_hpp

