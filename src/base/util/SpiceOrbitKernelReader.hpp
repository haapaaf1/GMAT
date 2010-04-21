//$Id$
//------------------------------------------------------------------------------
//                              SpiceOrbitKernelReader
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under
// MOMS Task order 124.
//
// Author: Wendy C. Shoan
// Created: 2010.04.05
//
/**
 * Definition of the SpiceOrbitKernelReader, which reads SPICE SPK (kernel) files.
 * This class calls the JPL-supplied CSPICE routines to read the specified
 * SPICE file(s) and return the requested data.
 */
//------------------------------------------------------------------------------

#ifndef SpiceOrbitKernelReader_hpp
#define SpiceOrbitKernelReader_hpp

#include "gmatdefs.hpp"
#include "A1Mjd.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
#include "SpiceKernelReader.hpp"

class GMAT_API SpiceOrbitKernelReader : public SpiceKernelReader
{
public:
//   static      SpiceOrbitKernelReader* Instance();
   SpiceOrbitKernelReader();
   SpiceOrbitKernelReader(const SpiceOrbitKernelReader &reader);
   SpiceOrbitKernelReader& operator=(const SpiceOrbitKernelReader &reader);

   virtual ~SpiceOrbitKernelReader();

   SpiceOrbitKernelReader* Clone() const;

   /// method to return the state (position, velocity) of a planetary body or
   /// spacecraft at the specified time, with respect to the specified frame
   Rvector6    GetTargetState(const std::string &targetName,
                              const Integer     targetNAIFId,
                              const A1Mjd       &atTime,
                              const std::string &observingBodyName,
                              const std::string &referenceFrame = "J2000",
                              const std::string &aberration = "NONE");


protected:

   // data converted to SPICE types, to pass into SPICE methods
//   /// the kernel name
//   ConstSpiceChar  *kernelNameSPICE;
   /// the target body name (SPICE)
//   ConstSpiceChar  *targetBodyNameSPICE;
   /// the observing body name (SPICE)
   ConstSpiceChar  *observingBodyNameSPICE;
//   /// the observer epoch time (SPICE) in Ephemeris (TDB) Time
//   SpiceDouble     etSPICE;
   /// the aberration correction flag (SPICE)
   ConstSpiceChar  *aberrationSPICE;
//   /// the reference frame (SPICE)
//   ConstSpiceChar  *referenceFrameSPICE;

};

#endif // SpiceOrbitKernelReader_hpp
