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
 * This is the base class.  Classes inheriting from this one handle the reading of
 * specific types of data (orbit, attitude, ...).
 */
//------------------------------------------------------------------------------

#ifndef SpiceKernelReader_hpp
#define SpiceKernelReader_hpp

#include "gmatdefs.hpp"
#include "A1Mjd.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
// include the appropriate SPICE C header
extern "C"  
{
#include "SpiceUsr.h"    // for CSPICE routines
}

class GMAT_API SpiceKernelReader
{
public:
   SpiceKernelReader();
   SpiceKernelReader(const SpiceKernelReader &reader);
   SpiceKernelReader& operator=(const SpiceKernelReader &reader);

   virtual ~SpiceKernelReader();
   
   virtual SpiceKernelReader* Clone() const = 0;

   virtual bool        LoadKernel(const std::string &fileName);
   virtual bool        UnloadKernel(const std::string &fileName);
   virtual bool        UnloadAllKernels();
   virtual bool        IsLoaded(const std::string &fileName);
   
   virtual StringArray GetValidAberrationCorrectionFlags();
   virtual StringArray GetValidFrames();
   virtual void        SetLeapSecondKernel(const std::string &lsk);
   
   virtual Integer     GetNaifID(const std::string &forBody);
   
protected:

   /// the name (full path) of the leap second kernel to use
   std::string lsKernel;
   
   // data converted to SPICE types, to pass into SPICE methods
   /// the kernel name
   ConstSpiceChar  *kernelNameSPICE;
   
   static const std::string VALID_ABERRATION_FLAGS[9];
   static const Integer     NUM_VALID_FRAMES;
   static const std::string VALID_FRAMES[12];
   /// maximum number of characters for short, explanation of short, or
   /// long error message requested when calling CSPICE method getmsg_c
   static const Integer     MAX_SHORT_MESSAGE;
   static const Integer     MAX_EXPLAIN_MESSAGE;
   static const Integer     MAX_LONG_MESSAGE;
   
   /// array of files (kernels) currently loaded
   static StringArray    loadedKernels;
   /// counter of number of instances created
   static Integer        numInstances;

   static void InitializeReader();

};

#endif // SpiceKernelReader_hpp
