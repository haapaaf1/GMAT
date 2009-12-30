//$Header$
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
 * Kernels that may be loaded are:
 *  planetary ephemeris (SPK)
 *  spacecraft ephemeris (SPK)
 *  spacecraft pointing (CK)
 *  planetary constants (PcK)   < future >
 *  instrument (IK)             < future >
 *  
 * These functions can be handled in one class; therefore, this is a singleton.
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
   static      SpiceKernelReader* Instance();
   ~SpiceKernelReader();
   
   bool        LoadKernel(const std::string &fileName);
   bool        UnloadKernel(const std::string &fileName);
   bool        UnloadAllKernels();
   bool        IsLoaded(const std::string &fileName);
   
   StringArray GetValidAberrationCorrectionFlags();
   StringArray GetValidFrames();
   void        SetLeapSecondKernel(const std::string &lsk);
   
   Integer     GetNaifID(const std::string &forBody);
   
   /// method to return the state (position, velocity) of a planetary body or 
   /// spacecraft at the specified time, with respect to the specified frame
   Rvector6    GetTargetState(const std::string &targetName,      
                              const A1Mjd       &atTime,
                              const std::string &observingBodyName,
                              const std::string &referenceFrame = "J2000",           
                              const std::string &aberration = "NONE");
   
   /// method to return the orientation (attitude) of an instrument or a 
   /// spacecraft at the specified time, with respect to the specified frame
   /// (support for planetary bodies' orientation may be added later)
   Rmatrix33   GetTargetOrientation(Integer           naifID,
                                    const A1Mjd       &atTime,
                                    Real              tolerance,
                                    const std::string &referenceFrame);
                                    // do I need an output s/c clock time here?
                                 
                                    

private:

   /// array of files (kernels) currently loaded
   StringArray    loadedKernels;
   /// the name (full path) of the leap second kernel to use
   std::string lsKernel;
   
   // data converted to SPICE types, to pass into SPICE methods
   /// the kernel name
   ConstSpiceChar  *kernelNameSPICE;
   /// the target body name (SPICE)
   ConstSpiceChar  *targetBodyNameSPICE;
   /// the observing body name (SPICE)
   ConstSpiceChar  *observingBodyNameSPICE;
   /// the observer epoch time (SPICE) in Ephemeris (TDB) Time
   SpiceDouble     etSPICE;
   /// the aberration correction flag (SPICE)
   ConstSpiceChar  *aberrationSPICE;
   /// the reference frame (SPICE)
   ConstSpiceChar  *referenceFrameSPICE;
   /// the NAIF ID
   SpiceInt        naifIDSPICE;
   /// the spacecraft clock time
   SpiceDouble     sclkSPICE;
   /// the output spacecraft clock time
   SpiceDouble     sclkOutputSPICE;
   /// the tolerance
   SpiceDouble     toleranceSPICE;
   /// the found flag
   SpiceBoolean    foundSPICE;
   
   static const std::string VALID_ABERRATION_FLAGS[9];
   static const Integer     NUM_VALID_FRAMES;
   static const std::string VALID_FRAMES[12];
   /// maximum number of characters for short, explanation of short, or
   /// long error message requested when calling CSPICE method getmsg_c
   static const Integer     MAX_SHORT_MESSAGE;
   static const Integer     MAX_EXPLAIN_MESSAGE;
   static const Integer     MAX_LONG_MESSAGE;
   
   static SpiceKernelReader *theInstance;
   SpiceKernelReader();
};

#endif // SpiceKernelReader_hpp
