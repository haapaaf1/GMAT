//$Id$
//------------------------------------------------------------------------------
//                              SpiceInterface
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under 
// FDSS Task order 28.
//
// Author: Wendy C. Shoan
// Created: 2008.10.22
//
/**
 * Definition of the SpiceInterface, which manages SPICE data (kernel) files.
 * This class calls the JPL-supplied CSPICE routines to load and unload kernels
 * into and out of the kernel pool.
 * Kernels that may be loaded (and then read by child classes) are:
 *  planetary ephemeris (SPK)
 *  spacecraft ephemeris (SPK)
 *  spacecraft pointing (CK)
 *  spacecraft clock (SCLK)
 *  leap second  (LSK)
 *  planetary constants (PcK)   < future >
 *  instrument (IK)             < future >
 *  
 *  Kernels that may be created by child writer classes are:
 *   spacecraft ephemeris (SPK)
 *   spacecraft attitude (CK)  < future >
 *   spacecraft clock (SCLK)   < future >
 *
 * This is the base class.  Classes inheriting from this one handle the reading
 * or writing of specific types of data (orbit, attitude, ...).
 */
//------------------------------------------------------------------------------

#ifndef SpiceInterface_hpp
#define SpiceInterface_hpp

#include "gmatdefs.hpp"
#include "A1Mjd.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
// include the appropriate SPICE C header(s)
extern "C"  
{
#include "SpiceUsr.h"    // for CSPICE routines
}

class GMAT_API SpiceInterface
{
public:
   SpiceInterface();
   SpiceInterface(const SpiceInterface &copy);
   SpiceInterface& operator=(const SpiceInterface &copy);

   virtual ~SpiceInterface();
   
   virtual SpiceInterface* Clone() const = 0;

   virtual bool        LoadKernel(const std::string &fileName);
   virtual bool        UnloadKernel(const std::string &fileName);
   virtual bool        UnloadAllKernels();
   virtual bool        IsLoaded(const std::string &fileName);
   
   virtual StringArray GetValidAberrationCorrectionFlags();
   virtual StringArray GetValidFrames();
   virtual void        SetLeapSecondKernel(const std::string &lsk);
   
   virtual Integer     GetNaifID(const std::string &forBody);
   
   virtual Real        SpiceTimeToA1(SpiceDouble spiceTime);
   virtual SpiceDouble A1ToSpiceTime(Real a1Time);

protected:

   
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
   static const Integer     MAX_CHAR_COMMENT;
   
   /// array of files (kernels) currently loaded
   static StringArray    loadedKernels;
   /// counter of number of instances created
   static Integer        numInstances;
   /// the name (full path) of the leap second kernel to use
   static std::string lsKernel;

   //   static void InitializeReader();
   static void InitializeInterface();

};

#endif // SpiceInterface_hpp