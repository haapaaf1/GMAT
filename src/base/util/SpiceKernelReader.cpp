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
// Created: 2009.02.24
//
/**
 * Implementation of the SpicKernelReader, which reads SPICE data (kernel) files.
 * This class calls the JPL-supplied CSPICE routines to read the specified
 * SPICE file and return the requested data.
 */
//------------------------------------------------------------------------------


#include "SpiceKernelReader.hpp"
#include "A1Mjd.hpp"
#include "StringUtil.hpp"
#include "MessageInterface.hpp"
#include "TimeTypes.hpp"
#include "TimeSystemConverter.hpp"
#include "UtilityException.hpp"

//#define DEBUG_SPK_LOADING
//#define DEBUG_SPK_READING
//#define DEBUG_SPK_GETSTATE
//#define DEBUG_SPK_PLANETS

//---------------------------------
// static data
//---------------------------------
const std::string
SpiceKernelReader::VALID_ABERRATION_FLAGS[9] =
{
   "NONE",    // Apply no correction
   // the following 4 options apply to the 'reception' case, in which photons
   // depart from the target's location at time et-lt and arrive at the 
   // observer's location at et (input time)
   "LT",      // Correct for one-way light time
   "LT+S",    // Correct for one-way light time and stellar aberration
   "CN",      // Converged Newtonian light time correction
   "CN+S",    // Converged Newtonian light time and stellar aberration correction
   // the following 4 options apply to the 'transmission' case, in which photons
   // depart from the observer's location at time et and arrive at the 
   // target's location at et+lt (input time)
   "XLT",     // Correct for one-way light time
   "XLT+S",   // Correct for one-way light time and stellar aberration
   "XCN",     // Converged Newtonian light time correction
   "XCN+S",   // Converged Newtonian light time and stellar aberration correction
};

const std::string
SpiceKernelReader::VALID_FRAMES[9] =
{
   "NONE",   // TBD
   "NONE",   // TBD
   "NONE",   // TBD
   "NONE",   // TBD
   "NONE",   // TBD
   "NONE",   // TBD
   "NONE",   // TBD
   "NONE",   // TBD
   "NONE",   // TBD
};

const Integer SpiceKernelReader::MAX_SHORT_MESSAGE   = 320;
const Integer SpiceKernelReader::MAX_EXPLAIN_MESSAGE = 320;
const Integer SpiceKernelReader::MAX_LONG_MESSAGE    = 1840;


SpiceKernelReader* SpiceKernelReader::theInstance = NULL;

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// SpiceKernelReader* Instance()
//------------------------------------------------------------------------------
SpiceKernelReader* SpiceKernelReader::Instance()
{
   if (theInstance == NULL)
      theInstance = new SpiceKernelReader;
   return theInstance;
}


//------------------------------------------------------------------------------
// ~SpiceKernelReader()
//------------------------------------------------------------------------------
SpiceKernelReader::~SpiceKernelReader()
{
   UnloadAllKernels();
}

bool SpiceKernelReader::LoadKernel(const std::string &fileName)
{
   for (StringArray::iterator jj = loadedKernels.begin();
        jj != loadedKernels.end(); ++jj)
      if ((*jj) == fileName)
      {
         MessageInterface::ShowMessage("SPK kernel %s has already been loaded.\n",
               (*jj).c_str());
         return false;
      }
   kernelNameSPICE = fileName.c_str();
   furnsh_c(kernelNameSPICE);
   if (failed_c() == SPICETRUE)
   {
//      ConstSpiceChar option[] = "SHORT"; // retrieve short error message, for now
//      SpiceInt       numChar  = MAX_SHORT_MESSAGE;
//      SpiceChar      err[MAX_SHORT_MESSAGE];
      ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
      SpiceInt       numChar  = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numChar, err);
      std::string errStr(err);
      std::string errmsg = "Error loading kernel \"";
      errmsg += fileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      throw UtilityException(errmsg);
   }
#ifdef DEBUG_SPK_LOADING
   else
   {
      MessageInterface::ShowMessage("SpiceKernelReader Successfully loaded kernel %s <---------\n",
            fileName.c_str());
   }
#endif
   loadedKernels.push_back(fileName);
   
   return true;
}

bool SpiceKernelReader::UnloadKernel(const std::string &fileName)
{
   bool found = false;
   for (StringArray::iterator jj = loadedKernels.begin();
        jj != loadedKernels.end(); ++jj)
      if ((*jj) == fileName)
      {
         found = true;
         loadedKernels.erase(jj);
         break;
      }
   if (!found)
   {
//      MessageInterface::ShowMessage(
//            "SpiceKernelReader::UnloadKernel() - kernel %s is not currently loaded.\n",
//            fileName.c_str());
      return false;
   }
   #ifdef DEBUG_SPK_LOADING
      MessageInterface::ShowMessage("Now attempting to unload kernel %s\n",
            fileName.c_str());
   #endif
   kernelNameSPICE = fileName.c_str();
   unload_c(kernelNameSPICE);
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
      std::string errmsg = "Error unloading kernel \"";
      errmsg += fileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      throw UtilityException(errmsg);
   }

   return true; 
}

bool SpiceKernelReader::UnloadAllKernels()
{
   for (StringArray::iterator jj = loadedKernels.begin();
        jj != loadedKernels.end(); ++jj)
   {
      #ifdef DEBUG_SPK_LOADING
         MessageInterface::ShowMessage("Now attempting to unload kernel %s\n",
               (*jj).c_str());
      #endif
      kernelNameSPICE = (*jj).c_str();
      unload_c(kernelNameSPICE);
      if (failed_c())
      {
//         ConstSpiceChar option[] = "SHORT"; // retrieve short error message, for now
//         SpiceInt       numChar  = MAX_SHORT_MESSAGE;
//         SpiceChar      err[MAX_SHORT_MESSAGE];
         ConstSpiceChar option[] = "LONG"; // retrieve long error message, for now
         SpiceInt       numChar  = MAX_LONG_MESSAGE;
         SpiceChar      err[MAX_LONG_MESSAGE];
         getmsg_c(option, numChar, err);
         std::string errStr(err);
         std::string errmsg = "Error unloading kernel \"";
         errmsg += (*jj) + "\".  Message received from CSPICE is: ";
         errmsg += errStr + "\n";
         throw UtilityException(errmsg);
      }
      // @todo - handle exceptional conditions (SPICE signals) here ...
   }
   loadedKernels.clear();
   return true;
}

bool SpiceKernelReader::IsLoaded(const std::string &fileName) 
{
   #ifdef DEBUG_SPK_LOADING
      MessageInterface::ShowMessage("IsLoaded::Now attempting to find kernel name %s\n", fileName.c_str());
   #endif
   for (StringArray::iterator jj = loadedKernels.begin();
        jj != loadedKernels.end(); ++jj)
   {
      if ((*jj) == fileName) return true;
   }
   return false;
}



StringArray SpiceKernelReader::GetValidAberrationCorrectionFlags()
{
   StringArray aberr;
   for (unsigned int ii = 0; ii < 9; ii++)
      aberr.push_back(VALID_ABERRATION_FLAGS[ii]);
   return aberr;
}

StringArray SpiceKernelReader::GetValidFrames()
{
   StringArray frames;
   for (unsigned int ii = 0; ii < 9; ii++)  // ****** adjust this ******
      frames.push_back(VALID_FRAMES[ii]);
   return frames;
}

void SpiceKernelReader::SetLeapSecondKernel(const std::string &lsk)
{
   lsKernel = lsk;
   if (!IsLoaded(lsKernel))   LoadKernel(lsKernel);
}

Integer SpiceKernelReader::GetNaifID(const std::string &forBody)
{
   SpiceBoolean   found;
   SpiceInt       id;
   ConstSpiceChar *bodyName = forBody.c_str();
   bodn2c_c(bodyName, &id, &found);
   if (found == SPICEFALSE)
   {
      std::string warnmsg = "Cannot find NAIF ID for body ";
      warnmsg += forBody + ".  Insufficient data available.  Another SPICE Kernel may be necessary.";
      MessageInterface::PopupMessage(Gmat::WARNING_, warnmsg);
      return -1;
   }
   return (Integer) id;
}


Rvector6 SpiceKernelReader::GetTargetState(const std::string &targetName,      
                            const A1Mjd       &atTime,
                            const std::string &observingBodyName,
                            const std::string &referenceFrame,           
                            const std::string &aberration)
{
   #ifdef DEBUG_SPK_READING
      MessageInterface::ShowMessage(
            "Entering SPKReader::GetTargetState with target = %s, time = %12.10f, observer = %s\n",
            targetName.c_str(), atTime.Get(), observingBodyName.c_str());
   #endif
   std::string targetNameToUse = targetName;
   if ((targetName == "Luna") || (targetName == "LUNA"))  // Luna, instead of Moon, for GMAT
      targetNameToUse        = "Moon";
   targetNameToUse           = GmatStringUtil::ToUpper(targetNameToUse);
   targetBodyNameSPICE       = targetNameToUse.c_str();
   observingBodyNameSPICE    = observingBodyName.c_str();
   referenceFrameSPICE       = referenceFrame.c_str();
   aberrationSPICE           = aberration.c_str();
   // convert time to Ephemeris Time (TDB)
   SpiceDouble j2ET          = j2000_c();
   Real        etMjdAtTime   = TimeConverterUtil::Convert(atTime.Get(), TimeConverterUtil::A1MJD, 
                               TimeConverterUtil::TDBMJD, GmatTimeUtil::JD_JAN_5_1941);
   etSPICE                   = (etMjdAtTime + GmatTimeUtil::JD_JAN_5_1941 - j2ET) * GmatTimeUtil::SECS_PER_DAY;
   #ifdef DEBUG_SPK_READING
      MessageInterface::ShowMessage("j2ET = %12.10f\n", (Real) j2ET);
      MessageInterface::ShowMessage(
            "In SPKReader::Converted (to TBD) time = %12.10f\n", etMjdAtTime);
      MessageInterface::ShowMessage("  then the full JD = %12.10f\n",
            (etMjdAtTime + GmatTimeUtil::JD_JAN_5_1941));
      MessageInterface::ShowMessage("So time passed to SPICE is %12.14f\n", (Real) etSPICE);
   #endif
   SpiceDouble state[6];
   SpiceDouble oneWayLightTime;
   spkezr_c(targetBodyNameSPICE, etSPICE, referenceFrameSPICE, aberrationSPICE,
            observingBodyNameSPICE, state, &oneWayLightTime);
#ifdef DEBUG_SPK_PLANETS
   Real        ttMjdAtTime   = TimeConverterUtil::Convert(atTime.Get(), TimeConverterUtil::A1MJD, 
                               TimeConverterUtil::TTMJD, GmatTimeUtil::JD_JAN_5_1941);
   Real etJd                 = etMjdAtTime + GmatTimeUtil::JD_JAN_5_1941;
   Real ttJd                 = ttMjdAtTime + GmatTimeUtil::JD_JAN_5_1941;
   MessageInterface::ShowMessage("Asking CSPICE for state of body %s, with observer %s, referenceFrame %s, and aberration correction %s\n",
         targetBodyNameSPICE, observingBodyNameSPICE, referenceFrameSPICE, aberrationSPICE);
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
      throw UtilityException(errmsg);
   }
   #ifdef DEBUG_SPK_READING
      MessageInterface::ShowMessage(
            "In SPKReader::Called spkezr_c and got state out\n");
   #endif
            
   
   Rvector6 r6(state[0],state[1],state[2],state[3],state[4],state[5]);
   return r6; 
}

Rmatrix33 SpiceKernelReader::GetTargetOrientation(Integer           naifID,
                                                 const A1Mjd       &atTime,
                                                 Real              tolerance,
                                                 const std::string &referenceFrame)
{
   Rmatrix33 r33;
   // will need sce2c_c here, and a s/c sclk file  .........
   return r33;  // *********** TBD **************
}
   
//---------------------------------
// protected methods
//---------------------------------

//---------------------------------
// private methods
//---------------------------------

SpiceKernelReader::SpiceKernelReader() :
   kernelNameSPICE         (NULL),
   targetBodyNameSPICE     (NULL),
   observingBodyNameSPICE  (NULL),
   aberrationSPICE         (NULL),
   referenceFrameSPICE     (NULL)
{
   loadedKernels.clear();
   // set output file and action for cspice methods
   errdev_c("SET", 1840, "./cspice_error.txt"); // @todo this should be set in startup file 
   erract_c("SET", 1840, "RETURN");
}
                              

