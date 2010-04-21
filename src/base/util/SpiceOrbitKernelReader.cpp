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
#include "SpiceOrbitKernelReader.hpp"
#include "gmatdefs.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
#include "A1Mjd.hpp"
#include "StringUtil.hpp"
#include "MessageInterface.hpp"
#include "TimeTypes.hpp"
#include "TimeSystemConverter.hpp"
#include "UtilityException.hpp"


//#define DEBUG_SPK_READING
//#define DEBUG_SPK_PLANETS

// -----------------------------------------------------------------------------
// static data
// -----------------------------------------------------------------------------
// none


// -----------------------------------------------------------------------------
// public methods
// -----------------------------------------------------------------------------

//------------------------------------------------------------------------------
//  SpiceOrbitKernelReader()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the SpiceOrbitKernelReader class
 * (default constructor).
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelReader::SpiceOrbitKernelReader() :
   SpiceKernelReader(),
//   targetBodyNameSPICE     (NULL),
   observingBodyNameSPICE  (NULL),
   aberrationSPICE         (NULL)//,
//   referenceFrameSPICE     (NULL)
{
}

//------------------------------------------------------------------------------
//  SpiceOrbitKernelReader(const SpiceKernelReader &reader)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the SpiceOrbitKernelReader class, by copying
 * the input object.
 * (copy constructor).
 *
* @param <reader> SpiceOrbitKernelReader object to copy.
  */
//------------------------------------------------------------------------------
SpiceOrbitKernelReader::SpiceOrbitKernelReader(const SpiceOrbitKernelReader &reader) :
   SpiceKernelReader(reader),
//   targetBodyNameSPICE     (NULL),
   observingBodyNameSPICE  (NULL),
   aberrationSPICE         (NULL)//,
//   referenceFrameSPICE     (NULL)
{

}

//------------------------------------------------------------------------------
//  SpiceOrbitKernelReader& operator=(const SpiceOrbitKernelReader &reader)
//------------------------------------------------------------------------------
/**
 * This method copies the data from the input object to the object.
 *
 * @param <reader> the SpiceOrbitKernelReader object whose data to assign to "this"
 *                 SpiceOrbitKernelReader.
 *
 * @return "this" SpiceOrbitKernelReader with data of input SpiceOrbitKernelReader
 *         reader.
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelReader& SpiceOrbitKernelReader::operator=(const SpiceOrbitKernelReader &reader)
{
   if (&reader == this)
      return *this;

   SpiceKernelReader::operator=(reader);

//   targetBodyNameSPICE      = NULL;
   observingBodyNameSPICE   = NULL;
   aberrationSPICE          = NULL;
//   referenceFrameSPICE      = NULL;

   return *this;
}

//------------------------------------------------------------------------------
// ~SpiceOrbitKernelReader()
//------------------------------------------------------------------------------
/**
 * This method is the destructor for the SpiceOrbitKernelReader.
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelReader::~SpiceOrbitKernelReader()
{
}

//------------------------------------------------------------------------------
//  SpiceOrbitKernelReader* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method clones the object.
 *
 * @return new object, cloned from "this" object.
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelReader* SpiceOrbitKernelReader::Clone(void) const
{
   SpiceOrbitKernelReader * clonedSKR = new SpiceOrbitKernelReader(*this);

   return clonedSKR;
}

//------------------------------------------------------------------------------
//  Rvector6 GetTargetState(const std::string &targetName,
//                          const A1Mjd       &atTime,
//                          const std::string &observingBodyName,
//                          const std::string &referenceFrame,
//                          const std::string &aberration)
//------------------------------------------------------------------------------
/**
 * This method returns the state of the target with respect to the observing body
 * at the input time.
 *
 * @param <targetName>        name of the target object.
 * @param <atTime>            time at which the state is requested.
 * @param <observingBodyName> name of the observing body
 * @param <referenceFrame>    frame in which state should be returned
 * @param <aberration>        flag indicating aberration corrections, if any
 *
 * @return state at the input time
 *
 */
//------------------------------------------------------------------------------
Rvector6 SpiceOrbitKernelReader::GetTargetState(const std::string &targetName,
                                 const Integer     targetNAIFId,
                                 const A1Mjd       &atTime,
                                 const std::string &observingBodyName,
                                 const std::string &referenceFrame,
                                 const std::string &aberration)
{
   #ifdef DEBUG_SPK_READING
      MessageInterface::ShowMessage(
            "Entering SPKReader::GetTargetState with target = %s, naifId = %d, time = %12.10f, observer = %s\n",
            targetName.c_str(), targetNAIFId, atTime.Get(), observingBodyName.c_str());
   #endif
   std::string targetNameToUse = targetName;
   if ((targetName == "Luna") || (targetName == "LUNA"))  // Luna, instead of Moon, for GMAT
      targetNameToUse        = "Moon";
   targetNameToUse           = GmatStringUtil::ToUpper(targetNameToUse);
   objectNameSPICE           = targetNameToUse.c_str();
   observingBodyNameSPICE    = observingBodyName.c_str();
   referenceFrameSPICE       = referenceFrame.c_str();
   aberrationSPICE           = aberration.c_str();
   // convert time to Ephemeris Time (TDB)
   etSPICE                   = A1ToSpiceTime(atTime.Get());
//   SpiceDouble j2ET          = j2000_c();
//   Real        etMjdAtTime   = TimeConverterUtil::Convert(atTime.Get(), TimeConverterUtil::A1MJD,
//                               TimeConverterUtil::TDBMJD, GmatTimeUtil::JD_JAN_5_1941);
//   etSPICE                   = (etMjdAtTime + GmatTimeUtil::JD_JAN_5_1941 - j2ET) * GmatTimeUtil::SECS_PER_DAY;
   // set the association between the name and the NAIF Id
//   SpiceInt        itsNAIFId = targetNAIFId;
   naifIDSPICE               = targetNAIFId;
//   boddef_c(objectNameSPICE, itsNAIFId);        // CSPICE method to set NAIF ID for an object
   boddef_c(objectNameSPICE, naifIDSPICE);        // CSPICE method to set NAIF ID for an object

   #ifdef DEBUG_SPK_READING
      MessageInterface::ShowMessage("SET NAIF Id for object %s to %d\n",
            targetNameToUse.c_str(), targetNAIFId);
//      MessageInterface::ShowMessage("j2ET = %12.10f\n", (Real) j2ET);
      MessageInterface::ShowMessage(
            "In SPKReader::Converted (to TBD) time = %12.10f\n", etMjdAtTime);
      MessageInterface::ShowMessage("  then the full JD = %12.10f\n",
            (etMjdAtTime + GmatTimeUtil::JD_JAN_5_1941));
      MessageInterface::ShowMessage("So time passed to SPICE is %12.14f\n", (Real) etSPICE);
   #endif
   SpiceDouble state[6];
   SpiceDouble oneWayLightTime;
   spkezr_c(objectNameSPICE, etSPICE, referenceFrameSPICE, aberrationSPICE,
            observingBodyNameSPICE, state, &oneWayLightTime);
#ifdef DEBUG_SPK_PLANETS
   Real        ttMjdAtTime   = TimeConverterUtil::Convert(atTime.Get(), TimeConverterUtil::A1MJD,
                               TimeConverterUtil::TTMJD, GmatTimeUtil::JD_JAN_5_1941);
   Real etJd                 = etMjdAtTime + GmatTimeUtil::JD_JAN_5_1941;
   Real ttJd                 = ttMjdAtTime + GmatTimeUtil::JD_JAN_5_1941;
   MessageInterface::ShowMessage("Asking CSPICE for state of body %s, with observer %s, referenceFrame %s, and aberration correction %s\n",
         objectNameSPICE, observingBodyNameSPICE, referenceFrameSPICE, aberrationSPICE);
   MessageInterface::ShowMessage(
         "           Body: %s   TT Time:  %12.10f  TDB Time: %12.10f   state:  %12.10f  %12.10f  %12.10f  %12.10f  %12.10f  %12.10f\n",
         targetName.c_str(), ttJd, etJd, state[0], state[1], state[2], state[3], state[4], state[5]);
#endif
   if (failed_c())
   {
//      ConstSpiceChar option[] = "SHORT"; // retrieve short error message, for now
//      SpiceInt       numChar  = MAX_SHORT_MESSAGE;
//      SpiceChar      err[MAX_SHORT_MESSAGE];
      ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
      SpiceInt       numChar  = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting state for body \"";
      errmsg += targetName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      throw UtilityException(errmsg);
   }
   #ifdef DEBUG_SPK_READING
      MessageInterface::ShowMessage(
            "In SPKReader::Called spkezr_c and got state out\n");
   #endif


   Rvector6 r6(state[0],state[1],state[2],state[3],state[4],state[5]);
   return r6;
}


