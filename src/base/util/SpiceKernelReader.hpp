//$Id$
//------------------------------------------------------------------------------
//                              SpiceKernelReader
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under 
// MOMS Task order 124.
//
// Author: Wendy C. Shoan
// Created: 2008.10.22
//
/**
 * Definition of the SpiceKernelReader, which reads SPICE data (kernel) files.
 * This class calls the JPL-supplied CSPICE routines to read the specified
 * SPICE file(s) and return the requested data.  
 * Kernels that may be loaded (and read by child classes) are:
 *  planetary ephemeris (SPK)
 *  spacecraft ephemeris (SPK)
 *  spacecraft pointing (CK)
 *  planetary constants (PcK)   < future >
 *  instrument (IK)             < future >
 *  
 * This is the base class for readers.  Classes inheriting from this one handle
 * the reading of specific types of data (orbit, attitude, ...).
 */
//------------------------------------------------------------------------------

#ifndef SpiceKernelReader_hpp
#define SpiceKernelReader_hpp

#include "gmatdefs.hpp"
#include "A1Mjd.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
#include "SpiceInterface.hpp"

class GMAT_API SpiceKernelReader : public SpiceInterface
{
public:
   SpiceKernelReader();
   SpiceKernelReader(const SpiceKernelReader &reader);
   SpiceKernelReader& operator=(const SpiceKernelReader &reader);

   virtual ~SpiceKernelReader();
   
protected:

   // data converted to SPICE types, to pass into SPICE methods
   /// the target body name (SPICE)
   ConstSpiceChar  *objectNameSPICE;
   /// the NAIF ID
   SpiceInt        naifIDSPICE;
   /// the observer epoch time (SPICE) in Ephemeris (TDB) Time
   SpiceDouble     etSPICE;
   /// the reference frame (SPICE)
   ConstSpiceChar  *referenceFrameSPICE;

};

#endif // SpiceKernelReader_hpp
