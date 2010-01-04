//$Id:$
//------------------------------------------------------------------------------
//                              SpiceKernelWriter
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under
// MOMS Task order 124.
//
// Author: Wendy C. Shoan
// Created: 2009.12.07
//
/**
 * Implementation of the SpiceKernelWriter, which writes SPICE data (kernel) files.
 *
 * This code creates a temporary text file, required in order to include META-Data
 * (commentary) in the SPK file.  The file is deleted from the system after the
 * commentary is added to the SPK file.  The name of this temporary text file
 * takes the form
 *       GMATtmpSPKcmmnt<objName>.txt
 * where <objName> is the name of the object for whom the SPK file is created
 *
 * If the code is unable to create the temporary file (e.g., because of a permission
 * problem), the SPK file will still be generated but will contain no META-data.
 *
 */
//------------------------------------------------------------------------------
#include <stdio.h>
#include <sstream>

#include "SpiceKernelWriter.hpp"
#include "MessageInterface.hpp"
#include "StringUtil.hpp"
#include "TimeTypes.hpp"
#include "TimeSystemConverter.hpp"
#include "UtilityException.hpp"
#include "RealUtilities.hpp"

//#define DEBUG_SPK_WRITING

//---------------------------------
// static data
//---------------------------------

const Integer SpiceKernelWriter::NUM_VALID_FRAMES = 1; // for now, only "J2000"
const std::string
SpiceKernelWriter::VALID_FRAMES[12] =
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

const Integer SpiceKernelWriter::MAX_SHORT_MESSAGE   = 320;
const Integer SpiceKernelWriter::MAX_EXPLAIN_MESSAGE = 320;
const Integer SpiceKernelWriter::MAX_LONG_MESSAGE    = 1840;
const Integer SpiceKernelWriter::MAX_CHAR_COMMENT    = 4000;

std::string SpiceKernelWriter::TMP_TXT_FILE_NAME = "./GMATtmpSPKcmmnt";


//---------------------------------
// public methods
//---------------------------------
SpiceKernelWriter::SpiceKernelWriter(const std::string       &objName,   const std::string &centerName,
                                     Integer                 objNAIFId,  Integer           centerNAIFId,
                                     const std::string       &fileName,  Integer           deg,
                                     const std::string       &frame) :
   objectName      (objName),
   centralBodyName (centerName),
   kernelFileName  (fileName),
   frameName       (frame),
   fileOpen        (false),
   tmpTxtFile      (NULL)
{
   if (GmatMathUtil::IsEven(deg)) // degree must be odd for Data Type 13
   {
      std::string errmsg = "Error creating SpiceKernelWriter: degree must be odd for Data Type 13\n";
      throw UtilityException(errmsg);
   }
   /// set up CSPICE data
   objectNAIFId      = objNAIFId;
   if (centerNAIFId == 0) // need to find the NAIF Id for the central body  @todo - for object too??
      centralBodyNAIFId = GetNaifID(centralBodyName);
   else
      centralBodyNAIFId = centerNAIFId;
   kernelName        = kernelFileName.c_str();
   degree            = deg;
   referenceFrame    = frameName.c_str();
   handle            = -999;

   // @todo - do we need to call boddef_c here to set the NAIF ID for the spacecraft????

   // set output file and action for cspice methods
   errdev_c("SET", 1840, "./GMATSpiceKernelWriterError.txt"); // @todo this should be set in startup file
   erract_c("SET", 1840, "RETURN");

   // get a file handle here
   SpiceInt        maxChar = MAX_CHAR_COMMENT;
   std::string     internalFileName = "GMAT-generated SPK file for " + objectName;
   ConstSpiceChar  *internalSPKName  = internalFileName.c_str();
   spkopn_c(kernelName, internalSPKName, maxChar, &handle);
   if (failed_c())
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting file handle for SPK file \"";
      errmsg += kernelFileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      throw UtilityException(errmsg);
   }
   fileOpen = true;
   // set up the "basic" meta data here ...
   SetBasicMetaData();
   // Get Julian date of J2000 from CSPICE
   j2ET = j2000_c();

   // make sure that the NAIF Id is associated with the object name  @todo - need to set center's Id as well sometimes?
   ConstSpiceChar *itsName = objectName.c_str();
   boddef_c(itsName, objectNAIFId);
   if (failed_c())
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::stringstream ss("");
      ss << "Unable to set NAIF Id for object \"" << objectName << "\" to the value ";
      ss << objNAIFId << ".  Message received from CSPICE is: ";
      ss << errStr << std::endl;
      throw UtilityException(ss.str());
   }
}

SpiceKernelWriter::SpiceKernelWriter(const SpiceKernelWriter &copy) :
   objectName        (copy.objectName),
   centralBodyName   (copy.centralBodyName),
   kernelFileName    (copy.kernelFileName),
   frameName         (copy.frameName),
   objectNAIFId      (copy.objectNAIFId),
   centralBodyNAIFId (copy.centralBodyNAIFId),
   degree            (copy.degree),
   handle            (copy.handle),
   basicMetaData     (copy.basicMetaData),
   addedMetaData     (copy.addedMetaData),
   fileOpen          (copy.fileOpen),    // ??
   tmpTxtFile        (copy.tmpTxtFile)   // ??
{
   kernelName     = kernelFileName.c_str();
   referenceFrame = frameName.c_str();
}

SpiceKernelWriter& SpiceKernelWriter::operator=(const SpiceKernelWriter &copy)
{
   if (&copy != this)
   {
      objectName        = copy.objectName;
      centralBodyName   = copy.centralBodyName;
      kernelFileName    = copy.kernelFileName;
      frameName         = copy.frameName;
      objectNAIFId      = copy.objectNAIFId;
      centralBodyNAIFId = copy.centralBodyNAIFId;
      degree            = copy.degree;
      handle            = copy.handle;
      basicMetaData     = copy.basicMetaData;
      addedMetaData     = copy.addedMetaData;
      fileOpen          = copy.fileOpen; // ??
      tmpTxtFile        = copy.tmpTxtFile; // ??

      kernelName        = kernelFileName.c_str();
      referenceFrame    = frameName.c_str();
   }

   return *this;
}

SpiceKernelWriter::~SpiceKernelWriter()
{
   if (fileOpen) FinalizeKernel();
}

StringArray SpiceKernelWriter::GetValidFrames()
{
   StringArray frames;
   for (Integer ii = 0; ii < NUM_VALID_FRAMES; ii++)
      frames.push_back(VALID_FRAMES[ii]);
   return frames;
}

void SpiceKernelWriter::SetLeapSecondKernel(const std::string &lsk)
{
   ; // reserved for possible future use (will need LSK when writing CKs)
}

void SpiceKernelWriter::WriteSegment(const A1Mjd &start, const A1Mjd &end,
                                     const StateArray &states, const EpochArray &epochs)
{
   SpiceInt numStates = states.size();
   if ((Integer) epochs.size() != (Integer) numStates)
   {
      std::string errmsg = "Error writing segment to SPK file \"";
      errmsg += kernelFileName + "\" - size of epoch array does not match size of state array.\n";
      throw UtilityException(errmsg);
   }
   // do time conversions here, for start, end, and all epochs
   Real         startTDB = TimeConverterUtil::Convert(start.Get(), TimeConverterUtil::A1MJD,
                           TimeConverterUtil::TDBMJD, GmatTimeUtil::JD_JAN_5_1941);
   SpiceDouble  startSPICE = (startTDB + GmatTimeUtil::JD_JAN_5_1941 - j2ET) * GmatTimeUtil::SECS_PER_DAY;

   Real         endTDB = TimeConverterUtil::Convert(end.Get(), TimeConverterUtil::A1MJD,
                         TimeConverterUtil::TDBMJD, GmatTimeUtil::JD_JAN_5_1941);
   SpiceDouble  endSPICE = (endTDB + GmatTimeUtil::JD_JAN_5_1941 - j2ET) * GmatTimeUtil::SECS_PER_DAY;

//   SpiceDouble  epochArray[numStates];
   SpiceDouble  *epochArray;     // (deleted at end of method)
   epochArray = new SpiceDouble[numStates];
   Real         tmpTDB;
   for (Integer ii = 0; ii < numStates; ii++)
   {
      tmpTDB = TimeConverterUtil::Convert((epochs.at(ii))->Get(), TimeConverterUtil::A1MJD,
               TimeConverterUtil::TDBMJD, GmatTimeUtil::JD_JAN_5_1941);
      epochArray[ii] = (SpiceDouble) (tmpTDB + GmatTimeUtil::JD_JAN_5_1941 - j2ET) * GmatTimeUtil::SECS_PER_DAY;
   }

   // put states into SpiceDouble arrays
//   SpiceDouble         stateArray[numStates][6];  // MSVC compiler doesn't like this allocation
   SpiceDouble  *stateArray;
   stateArray = new SpiceDouble[numStates * 6];

   for (Integer ii = 0; ii < numStates; ii++)
   {
      for (Integer jj = 0; jj < 6; jj++)
      {
//         stateArray[ii][jj] = ((states.at(ii))->GetDataVector())[jj];
         stateArray[(ii*6) + jj] = ((states.at(ii))->GetDataVector())[jj];
      }
   }

   // create a segment ID
   std::string         segmentID = "SPK_SEGMENT";
   ConstSpiceChar      *segmentIDSPICE = segmentID.c_str();

   // pass data to CSPICE
   spkw13_c(handle, objectNAIFId, centralBodyNAIFId, referenceFrame, startSPICE,
            endSPICE, segmentIDSPICE, degree, numStates, stateArray, epochArray);

   if(failed_c())
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::string errmsg = "Error writing ephemeris data to SPK file \"";
      errmsg += kernelFileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      throw UtilityException(errmsg);
   }
   delete [] epochArray;
   delete [] stateArray;
}
void SpiceKernelWriter::AddMetaData(const std::string &line, bool done)
{
   if (!fileOpen)
   {
      std::string errmsg = "Unable to add meta data to SPK kernel \"";
      errmsg += kernelFileName + "\".  File has been finalized and closed.\n";
      throw UtilityException(errmsg);
   }
   addedMetaData.push_back(line);

   if (done)
      FinalizeKernel();
}

void SpiceKernelWriter::AddMetaData(const StringArray &lines, bool done)
{
   if (!fileOpen)
   {
      std::string errmsg = "Unable to add meta data to SPK kernel \"";
      errmsg += kernelFileName + "\".  File has been finalized and closed.\n";
      throw UtilityException(errmsg);
   }
   unsigned int sz = lines.size();
   for (unsigned int ii = 0; ii < sz; ii++)
      addedMetaData.push_back(lines.at(ii));

   if (done)
      FinalizeKernel();
}

//---------------------------------
// protected methods
//---------------------------------
// none at this time

//---------------------------------
// private methods
//---------------------------------
void SpiceKernelWriter::SetBasicMetaData()
{
   basicMetaData.clear();
   std::string metaDataLine = ("EPHEMERIS kernel for object " + objectName) + "\n";
   basicMetaData.push_back(metaDataLine);
   metaDataLine = "Generated by the General Mission Analysis Tool (GMAT) on ";
   metaDataLine += __DATE__;
   metaDataLine += " at ";
   metaDataLine += __TIME__;
   metaDataLine += "\n";
   basicMetaData.push_back(metaDataLine);
//   metaDataLine = "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=\n";
//   basicMetaData.push_back(metaDataLine);
}

void SpiceKernelWriter::FinalizeKernel()
{
   // write all the meta data to the file
   WriteMetaData();
   basicMetaData.clear();
   addedMetaData.clear();
   // close the SPK file
   spkcls_c(handle);
   fileOpen = false;
}


void SpiceKernelWriter::WriteMetaData()
{
   // create the temporary text file to hold the meta data
   std::string    tmpTxtFileName = TMP_TXT_FILE_NAME;
   tmpTxtFileName += objectName + ".txt";
   tmpTxtFile = fopen(tmpTxtFileName.c_str(), "w");

   if (!tmpTxtFile)
   {
      std::string errmsg = "Error opening temporary text file for SPK meta data, for object \"";
      errmsg += objectName + "\".  No meta data will be added to the file.\n";
   //      throw UtilityException(errmsg);    // should this be a warning message only?
      MessageInterface::PopupMessage(Gmat::WARNING_, errmsg);
      return;
   }

   // write the meta data to the temporary file
   unsigned int basicSize = basicMetaData.size();
   unsigned int addedSize = addedMetaData.size();
   for (unsigned int ii = 0; ii < basicSize; ii++)
      fprintf(tmpTxtFile, "%s", (basicMetaData[ii]).c_str());
   fprintf(tmpTxtFile,"\n");
   for (unsigned int ii = 0; ii < addedSize; ii++)
      fprintf(tmpTxtFile, "%s", (addedMetaData[ii]).c_str());
   fprintf(tmpTxtFile,"\n");
   fflush(tmpTxtFile);
   fclose(tmpTxtFile);

   // write the meta data to the SPK file comment area by telling it to read the
   // temporary text file
   Integer     txtLength = tmpTxtFileName.length();
//   char        tmpTxt[txtLength+1]; // MSVC doesn't like this allocation
   char        *tmpTxt;    // (deleted at end of method)
   tmpTxt = new char[txtLength + 1];
   for (Integer jj = 0; jj < txtLength; jj++)
      tmpTxt[jj] = tmpTxtFileName.at(jj);
   tmpTxt[txtLength] = '\0';
   integer     unit;
   ftnlen      txtLen = txtLength + 1;
   txtopr_(tmpTxt, &unit, txtLen);
   spcac_(&handle, &unit, " ", " ", 1, 1);
   if (failed_c())
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      SpiceChar      err[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::string errmsg = "Error writing meta data to SPK file \"";
      errmsg += kernelFileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      throw UtilityException(errmsg);
   }
   // close the text file
   ftncls_c(unit);
   // remove the temporary text file
   remove(tmpTxt);
   delete [] tmpTxt;
}

Integer SpiceKernelWriter::GetNaifID(const std::string &forBody)
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
   #ifdef DEBUG_SPK_WRITING
      MessageInterface::ShowMessage("NAIF ID for body %s has been found: it is %d\n",
                                    forBody.c_str(), (Integer) id);
   #endif
   return (Integer) id;
}


