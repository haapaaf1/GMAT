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
 * Definition of the SpiceAttitudeKernelReader, which reads SPICE SPK (kernel) files.
 * This class calls the JPL-supplied CSPICE routines to read the specified
 * SPICE file(s) and return the requested data.
 */
//------------------------------------------------------------------------------
#include "SpiceAttitudeKernelReader.hpp"
#include "gmatdefs.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
#include "A1Mjd.hpp"
#include "StringUtil.hpp"
#include "MessageInterface.hpp"
#include "TimeTypes.hpp"
#include "TimeSystemConverter.hpp"
#include "UtilityException.hpp"


//#define DEBUG_CK_READING

// -----------------------------------------------------------------------------
// static data
// -----------------------------------------------------------------------------
// none


// -----------------------------------------------------------------------------
// public methods
// -----------------------------------------------------------------------------
SpiceAttitudeKernelReader::SpiceAttitudeKernelReader() :
   SpiceKernelReader()
{
}

SpiceAttitudeKernelReader::SpiceAttitudeKernelReader(const SpiceAttitudeKernelReader &reader) :
   SpiceKernelReader(reader)
{
}

SpiceAttitudeKernelReader& SpiceAttitudeKernelReader::operator=(const SpiceAttitudeKernelReader &reader)
{
   if (&reader == this)
      return *this;

   SpiceKernelReader::operator=(reader);

   return *this;
}

SpiceAttitudeKernelReader::~SpiceAttitudeKernelReader()
{
}

SpiceAttitudeKernelReader* SpiceAttitudeKernelReader::Clone(void) const
{
   SpiceAttitudeKernelReader * clonedSKR = new SpiceAttitudeKernelReader(*this);

   return clonedSKR;
}

void SpiceAttitudeKernelReader::GetTargetOrientation(const std::string &objectName,
                                                     Integer           naifID,
                                                     const A1Mjd       &atTime,
//                                                     Real              tolerance,
                                                     Rmatrix33         &r33,
                                                     Rvector3          &angVel,
                                                     const std::string &referenceFrame)
{
   std::string objectNameToUse = objectName;

   objectNameToUse       = GmatStringUtil::ToUpper(objectNameToUse);
   objectNameSPICE       = objectNameToUse.c_str();
   naifIDSPICE           = naifID;
   referenceFrameSPICE   = referenceFrame.c_str();
   etSPICE               = A1ToSpiceTime(atTime.Get());

//   boddef_c(objectNameSPICE, naifIDSPICE);        // CSPICE method to set NAIF ID for an object - is this valid for spacecraft?
   // Convert the time (in TDB) to spacecaft ticks
   SpiceDouble scTime;
   sce2c_c(naifIDSPICE, etSPICE, &scTime);
   if (failed_c())
   {
      ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
      SpiceInt       numChar  = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting spacecraft time (ticks) for object \"";
      errmsg += objectName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      throw UtilityException(errmsg);
   }
   // get the tolerance in spacecraft clock ticks
   std::string    tolerance = "00:00:001";  // this should probably be user input, or set as a constant
   ConstSpiceChar *tol = tolerance.c_str();
   SpiceDouble    tolTicks;
   sctiks_c(naifIDSPICE, tol, &tolTicks);
   if (failed_c())
   {
      ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
      SpiceInt       numChar  = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting tolerance (ticks) for object \"";
      errmsg += objectName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      throw UtilityException(errmsg);
   }

   // Now get the C-matrix and angular velocity at the requested time
   SpiceDouble    cmat[3][3];
   SpiceDouble    av[3];
   SpiceBoolean   found;
   SpiceDouble    clkout;
   ckgpav_c(naifIDSPICE, scTime, tolTicks, referenceFrameSPICE, cmat, av, &clkout, &found);
   if (failed_c())
   {
      ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
      SpiceInt       numChar  = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting C-matrix and/or angular velocity for object \"";
      errmsg += objectName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
     throw UtilityException(errmsg);
   }
   // Set output values
   r33.Set(cmat[0][0], cmat[0][1], cmat[0][2],
           cmat[1][0], cmat[1][1], cmat[1][2],
           cmat[2][0], cmat[2][1], cmat[2][2]);
   angVel.Set(av[0], av[1], av[2]);

}
