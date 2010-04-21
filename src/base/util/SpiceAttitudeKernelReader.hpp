//$Id$
//------------------------------------------------------------------------------
//                              SpiceAttitudeKernelReader
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
 * Definition of the SpiceAttitudeKernelReader, which reads SPICE CK (kernel) files.
 * This class calls the JPL-supplied CSPICE routines to read the specified
 * SPICE file(s) and return the requested data.
 */
//------------------------------------------------------------------------------

#ifndef SpiceAttitudeKernelReader_hpp
#define SpiceAttitudeKernelReader_hpp

#include "gmatdefs.hpp"
#include "A1Mjd.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
#include "SpiceKernelReader.hpp"

class GMAT_API SpiceAttitudeKernelReader : public SpiceKernelReader
{
public:

   SpiceAttitudeKernelReader();
   SpiceAttitudeKernelReader(const SpiceAttitudeKernelReader &reader);
   SpiceAttitudeKernelReader& operator=(const SpiceAttitudeKernelReader &reader);

   virtual ~SpiceAttitudeKernelReader();

   SpiceAttitudeKernelReader* Clone() const;

   /// method to return the orientation (attitude) of an instrument or a
   /// spacecraft at the specified time, with respect to the specified frame
   /// (support for planetary bodies' orientation may be added later)
   void   GetTargetOrientation(const std::string &objectName,
                               Integer           naifID,
                               const A1Mjd       &atTime,
//                               Real              tolerance,
                               Rmatrix33         &r33,
                               Rvector3          &angVel,
                               const std::string &referenceFrame = "J2000");

protected:

   /// the spacecraft clock time
   SpiceDouble     sclkSPICE;
   /// the output spacecraft clock time
   SpiceDouble     sclkOutputSPICE;
   /// the tolerance
   SpiceDouble     toleranceSPICE;
   /// the found flag
   SpiceBoolean    foundSPICE;

};

#endif // SpiceAttitudeKernelReader_hpp
