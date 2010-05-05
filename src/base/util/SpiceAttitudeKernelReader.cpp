//$Id:$
//------------------------------------------------------------------------------
//                              SpiceAttitudeKernelReader
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under
// FDSS Task order 28.
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
//---------------------------------------------------------------------------
//  SpiceAttitudeKernelReader()
//---------------------------------------------------------------------------
/**
 * Creates a SpiceAttitudeKernelReader.
 *
 * @note This is the default constructor
 */
//---------------------------------------------------------------------------
SpiceAttitudeKernelReader::SpiceAttitudeKernelReader() :
   SpiceKernelReader()
{
}

//---------------------------------------------------------------------------
//  SpiceAttitudeKernelReader(const SpiceAttitudeKernelReader &reader)
//---------------------------------------------------------------------------
/**
 * Creates a SpiceAttitudeKernelReader, with data copied from the input
 * instance.
 *
 * @param reader the input reader whose data to copy.
 *
 * @note This is the copy constructor
 */
//---------------------------------------------------------------------------
SpiceAttitudeKernelReader::SpiceAttitudeKernelReader(const SpiceAttitudeKernelReader &reader) :
   SpiceKernelReader(reader)
{
}

//---------------------------------------------------------------------------
//  SpiceAttitudeKernelReader& operator=(const SpiceAttitudeKernelReader &reader)
//---------------------------------------------------------------------------
/**
 * Copies data from the input SpiceAttitudeKernelReader,into "this" instance.
 *
 * @param reader the input reader whose data to use.
 */
//---------------------------------------------------------------------------
SpiceAttitudeKernelReader& SpiceAttitudeKernelReader::operator=(const SpiceAttitudeKernelReader &reader)
{
   if (&reader == this)
      return *this;

   SpiceKernelReader::operator=(reader);

   return *this;
}

//---------------------------------------------------------------------------
//  ~SpiceAttitudeKernelReader()
//---------------------------------------------------------------------------
/**
 * Deletes "this" instance.
 *
 * @note This is the destructor.
 */
//---------------------------------------------------------------------------
SpiceAttitudeKernelReader::~SpiceAttitudeKernelReader()
{
}

//---------------------------------------------------------------------------
//  SpiceAttitudeKernelReader* Clone(void) const
//---------------------------------------------------------------------------
/**
 * Creates and returns a clone of "this" instance.
 *
 * @return Clone of "this" instance.
 */
//---------------------------------------------------------------------------
SpiceAttitudeKernelReader* SpiceAttitudeKernelReader::Clone(void) const
{
   SpiceAttitudeKernelReader * clonedSKR = new SpiceAttitudeKernelReader(*this);

   return clonedSKR;
}

//------------------------------------------------------------------------------
// void GetCoverageStartAndEnd(const StringArray &kernels, Integer forNaifId.
//                             Real              &start,   Real    &end)
//------------------------------------------------------------------------------
/**
 * This (pure virtual) method determines the coverage for the specified object
 * over the specified kernels.
 *
 * @param kernels   the array of kernels over which to check the coverage
 * @param forNaifId the NAIF ID of the object for which coverage should
 *                  be determined
 * @param start     (output) the earliest time of coverage for the object
 *                  included in the specified kernels
 * @param end       (output) the latest time of coverage for the object
 *                  included in the specified kernels
 *
 * @note An exception is thrown if any of the kernels listed are not currently
 *       loaded into the kernel pool, and an attempt to load it fails.
 * @note The interval between the returned start and end times is not
 *       necessarily continuous.  The method checks all intervals over which
 *       there is coverage for the specified object and returns the earliest
 *       time and the latest time of coverage.  There could be gaps in
 *       coverage over that span.
 * @note The method will ignore kernels of types not containing the type of
 *       data requested, i.e. this method will ignore kernels in the list that
 *       are not of type 'ck'.
 */
//------------------------------------------------------------------------------
void  SpiceAttitudeKernelReader::GetCoverageStartAndEnd(StringArray       &kernels,
                                                        Integer           forNaifId,
                                                        Real              &start,
                                                        Real              &end)
{
   throw UtilityException("GetCoverageStatAndEnd not yet implemented for CK kernels\n");
}

//---------------------------------------------------------------------------
//  void GetTargetOrientation(const std::string &objectName,
//                            Integer           naifID,    const A1Mjd       &atTime,
//                            Rmatrix33         &r33,      Rvector3          &angVel,
//                            const std::string &referenceFrame = "J2000")
//---------------------------------------------------------------------------
/**
 * Gets the target orientation (cosine matrix and angular velocity) for the
 * requested time, given the object name and NAIF ID.
 *
 * @param   objectName   name of the object for which to obtain the data
 * @param   naifID       NAIF ID of the object
 * @param   atTime       time for which to obtain the data
 * @param   r33          (output) cosine matrix at time atTime
 * @param   angVel       (output) angular velocity at time atTime
 * @param   referenceFrame reference frame for the data (default "J2000")
 */
//---------------------------------------------------------------------------
void SpiceAttitudeKernelReader::GetTargetOrientation(const std::string &objectName,
                                                     Integer           naifID,
                                                     const A1Mjd       &atTime,
//                                                     Real              tolerance,
                                                     Rmatrix33         &r33,
                                                     Rvector3          &angVel,
                                                     const std::string &referenceFrame)
{
   #ifdef DEBUG_CK_READING
      MessageInterface::ShowMessage("Entering GetTargetOrientation for object %s, with NAIF ID %d, at time %12,10f\n",
         objectName.c_str(), naifID, atTime.Get());
   #endif
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
      //SpiceChar      err[MAX_LONG_MESSAGE];
      SpiceChar      *err = new SpiceChar[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting spacecraft time (ticks) for object \"";
      errmsg += objectName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      delete [] err;
      throw UtilityException(errmsg);
   }
   // get the tolerance in spacecraft clock ticks
   std::string    tolerance = "01:00:000";  // this should probably be user input, or set as a constant
   ConstSpiceChar *tol = tolerance.c_str();
   SpiceDouble    tolTicks;
   sctiks_c(naifIDSPICE, tol, &tolTicks);
   if (failed_c())
   {
      ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
      SpiceInt       numChar  = MAX_LONG_MESSAGE;
      //SpiceChar      err[MAX_LONG_MESSAGE];
      SpiceChar      *err = new SpiceChar[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting tolerance (ticks) for object \"";
      errmsg += objectName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      delete [] err;
      throw UtilityException(errmsg);
   }

   // Now get the C-matrix and angular velocity at the requested time
   SpiceDouble    cmat[3][3];
   SpiceDouble    av[3];
   SpiceBoolean   found;
   SpiceDouble    clkout;
   #ifdef DEBUG_CK_READING
      MessageInterface::ShowMessage("about to call ckgpav: \n");
      MessageInterface::ShowMessage("   NAIF ID  = %d\n"
                                    "   etSPICE  = %12.10f\n"
                                    "   scTime   = %12.10fn"
                                    "   tolTicks = %12.10f\n"
                                    "   refFrame = %s\n",
         (Integer) naifIDSPICE, (Real) etSPICE, (Real) scTime, (Real) tolTicks,
         referenceFrame.c_str());
   #endif
   ckgpav_c(naifIDSPICE, scTime, tolTicks, referenceFrameSPICE, cmat, av, &clkout, &found);
   if (failed_c())
   {
      ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
      SpiceInt       numChar  = MAX_LONG_MESSAGE;
      //SpiceChar      err[MAX_LONG_MESSAGE];
      SpiceChar      *err = new SpiceChar[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting C-matrix and/or angular velocity for object \"";
      errmsg += objectName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      delete [] err;
      throw UtilityException(errmsg);
   }
   if (found == SPICEFALSE)
   {
      std::string errmsg = "Pointing data for object ";
      errmsg += objectName + " not found on loaded CK/SCLK kernels.\n";
      throw UtilityException(errmsg);
   }
   #ifdef DEBUG_CK_READING
      MessageInterface::ShowMessage("results from ckgpav: \n");
      MessageInterface::ShowMessage("   cosMat = %12.10f  %12.10f  %12.10f\n"
                                    "            %12.10f  %12.10f  %12.10f\n"
                                    "            %12.10f  %12.10f  %12.10f\n",
                                    (Real)cmat[0][0], (Real)cmat[0][1], (Real)cmat[0][2],
                                    (Real)cmat[1][0], (Real)cmat[1][1], (Real)cmat[1][2],
                                    (Real)cmat[2][0], (Real)cmat[2][1], (Real)cmat[2][2]);
      MessageInterface::ShowMessage("   angvel = %12.10f  %12.10f  %12.10f\n",
                                   (Real)av[0], (Real)av[1], (Real)av[2]);
      MessageInterface::ShowMessage("   and clkout = %12.10f\n", (Real) clkout);
   #endif
   // Set output values
   r33.Set(cmat[0][0], cmat[0][1], cmat[0][2],
           cmat[1][0], cmat[1][1], cmat[1][2],
           cmat[2][0], cmat[2][1], cmat[2][2]);
   angVel.Set(av[0], av[1], av[2]);

}
