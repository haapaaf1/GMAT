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

const Integer SpiceKernelReader::NUM_VALID_FRAMES = 1; // for now, only "J2000"

const std::string
SpiceKernelReader::VALID_FRAMES[12] =
{
   "J2000",   // default frame
   "NONE",   // TBD
   "NONE",   // TBD
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

/// array of files (kernels) currently loaded
StringArray    SpiceKernelReader::loadedKernels;
/// counter of number of instances created
Integer        SpiceKernelReader::numInstances = 0;



//SpiceKernelReader* SpiceKernelReader::theInstance = NULL;

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
//  SpiceKernelReader()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the SpiceKernelReader class
 * (default constructor).
 *
 */
//------------------------------------------------------------------------------
SpiceKernelReader::SpiceKernelReader() :
   kernelNameSPICE         (NULL)
{
   InitializeReader();
   numInstances++;
}

//------------------------------------------------------------------------------
//  SpiceKernelReader(const SpiceKernelReader &reader)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the SpiceKernelReader class, by copying
 * the input object.
 * (copy constructor).
 *
* @param <reader> SpiceKernelReader object to copy.
  */
//------------------------------------------------------------------------------
SpiceKernelReader::SpiceKernelReader(const SpiceKernelReader &reader) :
   lsKernel                (reader.lsKernel),
   kernelNameSPICE         (NULL)
{
   numInstances++;
}

//------------------------------------------------------------------------------
//  SpiceKernelReader& operator=(const SpiceKernelReader &reader)
//------------------------------------------------------------------------------
/**
 * This method copies the data from the input object to the object.
 *
 * @param <reader> the SpiceKernelReader object whose data to assign to "this"
 *                 SpiceKernelReader.
 *
 * @return "this" SpiceKernelReader with data of input SpiceKernelReader reader.
 */
//------------------------------------------------------------------------------
SpiceKernelReader& SpiceKernelReader::operator=(const SpiceKernelReader &reader)
{
   if (&reader == this)
      return *this;

   lsKernel                 = reader.lsKernel;
   kernelNameSPICE          = NULL;
   // don't modify numInstances - we are not creating a new instance here
   return *this;
}


//------------------------------------------------------------------------------
// ~SpiceKernelReader()
//------------------------------------------------------------------------------
/**
 * This method is the destructor for the SpiceKernelReader.
 *
 */
//------------------------------------------------------------------------------
SpiceKernelReader::~SpiceKernelReader()
{
   numInstances--;
   if (numInstances <= 0) UnloadAllKernels();
}

//------------------------------------------------------------------------------
//  bool LoadKernel(const std::string &fileName)
//------------------------------------------------------------------------------
/**
 * This method loads the input file into the SPICE kernel pool.
 *
 * @param <fileName>  full path of the file (kernel) to load.
 *
 * @return success flag.
 *
 */
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//  bool UnloadKernel(const std::string &fileName)
//------------------------------------------------------------------------------
/**
 * This method unloads the input file from the SPICE kernel pool.
 *
 * @param <fileName>  full path of the file (kernel) to unload.
 *
 * @return success flag.
 *
 */
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//  bool UnloadAllKernels()
//------------------------------------------------------------------------------
/**
 * This method unloads all loaded kernels from the SPICE kernel pool.
 *
 * @return success flag.
 *
 */
//------------------------------------------------------------------------------
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

//------------------------------------------------------------------------------
//  bool IsLoaded(const std::string &fileName)
//------------------------------------------------------------------------------
/**
 * This method checks to see if the input file is loaded into the kernel pool.
 *
 * @param <fileName>  full path of the file (kernel) to check for.
 *
 * @return true if the file (kernel) has been loaded;
 *         false otherwise
 *
 */
//------------------------------------------------------------------------------
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



//------------------------------------------------------------------------------
//  StringArray GetValidAberrationCorrectionFlags()
//------------------------------------------------------------------------------
/**
 * This method returns a string array containing all valid aberration flags.
 *
  * @return array of  strings representing valid SPICE aberration flags.
 *
 */
//------------------------------------------------------------------------------
StringArray SpiceKernelReader::GetValidAberrationCorrectionFlags()
{
   StringArray aberr;
   for (unsigned int ii = 0; ii < 9; ii++)
      aberr.push_back(VALID_ABERRATION_FLAGS[ii]);
   return aberr;
}

//------------------------------------------------------------------------------
//  StringArray GetValidFrames()
//------------------------------------------------------------------------------
/**
 * This method returns a string array containing all valid frames.
 *
  * @return array of  strings representing valid (built-in) SPICE frames.
 *
 */
//------------------------------------------------------------------------------
StringArray SpiceKernelReader::GetValidFrames()
{
   StringArray frames;
   for (Integer ii = 0; ii < NUM_VALID_FRAMES; ii++)
      frames.push_back(VALID_FRAMES[ii]);
   return frames;
}

//------------------------------------------------------------------------------
//  void SetLeapSecondKernel(const std::string &lsk)
//------------------------------------------------------------------------------
/**
 * This method sets the leap second kernel, loading it into the kernel pool.
 *
 * @param <lsk>  full path of the leap second  kernel to load.
 *
 *
 */
//------------------------------------------------------------------------------
void SpiceKernelReader::SetLeapSecondKernel(const std::string &lsk)
{
   lsKernel = lsk;
   if (!IsLoaded(lsKernel))   LoadKernel(lsKernel);
}

//------------------------------------------------------------------------------
//  Integer GetNaifID(const std::string &forBody)
//------------------------------------------------------------------------------
/**
 * This method returns the NAIF Id of an object, given its name.
 *
 * @param <forBody>  name of the object.
 *
 * @return correspoding NAIF id; or 0 if not found
 *
 */
//------------------------------------------------------------------------------
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
      return 0;
   }
   #ifdef DEBUG_SPK_READING
      MessageInterface::ShowMessage("NAIF ID for body %s has been found: it is %d\n",
                                    forBody.c_str(), (Integer) id);
   #endif
   return (Integer) id;
}

//---------------------------------
// protected methods
//---------------------------------

// ---------------------
// static methods
// ---------------------
//------------------------------------------------------------------------------
//  void InitializeReader()
//------------------------------------------------------------------------------
/**
 * This method initializes the static data for the reader, and sets up
 * SPICE error handling.
 *
 * @param <forBody>  name of the object.
 *
 * @return correspoding NAIF id; or 0 if not found
 *
 */
//------------------------------------------------------------------------------
void SpiceKernelReader::InitializeReader()
{
   if (numInstances == 0)
   {
      loadedKernels.clear();
      // set output file and action for cspice methods
      errdev_c("SET", 1840, "./GMATSpiceKernelReaderError.txt"); // @todo this should be set in startup file
      erract_c("SET", 1840, "RETURN");
   }
}


