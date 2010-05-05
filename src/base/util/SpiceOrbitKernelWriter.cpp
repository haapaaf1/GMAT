//$Id:$
//------------------------------------------------------------------------------
//                              SpiceOrbitKernelWriter
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under
// FDSS Task order 28.
//
// Author: Wendy C. Shoan
// Created: 2009.12.07
//
/**
 * Implementation of the SpiceOrbitKernelWriter, which writes SPICE data (kernel) files.
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

#include "SpiceOrbitKernelWriter.hpp"
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


std::string SpiceOrbitKernelWriter::TMP_TXT_FILE_NAME = "./GMATtmpSPKcmmnt";


//---------------------------------
// public methods
//---------------------------------
//------------------------------------------------------------------------------
//  SpiceOrbitKernelWriter(const std::string       &objName,
//                         const std::string       &centerName,
//                         Integer                 objNAIFId,
//                         Integer                 centerNAIFId,
//                         const std::string       &fileName,
//                         Integer                 deg,
//                         const std::string       &frame)
//------------------------------------------------------------------------------
/**
 * This method constructs a SpiceKernelWriter instance.
 * (constructor)
 *
 * @param    objName       name of the object for which to write the SPK kernel
 * @param    centerName    name of he central body of the object
 * @param    objNAIFId     NAIF ID for the object
 * @param    centerNAIFId  NAIF ID for the central body
 * @param    fileName      name of the kernel to generate
 * @param    deg           degree of interpolating polynomials
 * @param    frame         reference frame (default = "J2000")
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelWriter::SpiceOrbitKernelWriter(const std::string      &objName,   const std::string &centerName,
                                              Integer                 objNAIFId,  Integer           centerNAIFId,
                                              const std::string       &fileName,  Integer           deg,
                                              const std::string       &frame) :
   SpiceKernelWriter(),
   objectName      (objName),
   centralBodyName (centerName),
   kernelFileName  (fileName),
   frameName       (frame),
   fileOpen        (false),
   tmpTxtFile      (NULL)
{
   if (GmatMathUtil::IsEven(deg)) // degree must be odd for Data Type 13
   {
      std::string errmsg = "Error creating SpiceOrbitKernelWriter: degree must be odd for Data Type 13\n";
      throw UtilityException(errmsg);
   }
   /// set up CSPICE data
   objectNAIFId      = objNAIFId;
   if (centerNAIFId == 0) // need to find the NAIF Id for the central body  @todo - for object too??
      centralBodyNAIFId = GetNaifID(centralBodyName);
   else
      centralBodyNAIFId = centerNAIFId;
   kernelNameSPICE   = kernelFileName.c_str();
   degree            = deg;
   referenceFrame    = frameName.c_str();
   handle            = -999;

   // @todo - do we need to call boddef_c here to set the NAIF ID for the spacecraft????

   // get a file handle here
   SpiceInt        maxChar = MAX_CHAR_COMMENT;
   std::string     internalFileName = "GMAT-generated SPK file for " + objectName;
   ConstSpiceChar  *internalSPKName  = internalFileName.c_str();
   spkopn_c(kernelNameSPICE, internalSPKName, maxChar, &handle); // CSPICE method to create and open an SPK kernel
   if (failed_c()) // CSPICE method to detect failure of previous call to CSPICE
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      //SpiceChar      err[MAX_LONG_MESSAGE];
      SpiceChar      *err = new SpiceChar[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::string errmsg = "Error getting file handle for SPK file \"";
      errmsg += kernelFileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      delete [] err;
      throw UtilityException(errmsg);
   }
   fileOpen = true;
   // set up the "basic" meta data here ...
   SetBasicMetaData();

   // make sure that the NAIF Id is associated with the object name  @todo - need to set center's Id as well sometimes?
   ConstSpiceChar *itsName = objectName.c_str();
   boddef_c(itsName, objectNAIFId);        // CSPICE method to set NAIF ID for an object
   if (failed_c())
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      //SpiceChar      err[MAX_LONG_MESSAGE];
      SpiceChar      *err = new SpiceChar[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::stringstream ss("");
      ss << "Unable to set NAIF Id for object \"" << objectName << "\" to the value ";
      ss << objNAIFId << ".  Message received from CSPICE is: ";
      ss << errStr << std::endl;
      reset_c();
      delete [] err;
      throw UtilityException(ss.str());
   }
}

//------------------------------------------------------------------------------
//  SpiceOrbitKernelWriter(const SpiceOrbitKernelWriter &copy)
//------------------------------------------------------------------------------
/**
 * This method constructs a SpiceKernelWriter instance, copying data from the
 * input instance.
 * (copy constructor)
 *
 * @param    copy       object to copy
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelWriter::SpiceOrbitKernelWriter(const SpiceOrbitKernelWriter &copy) :
   SpiceKernelWriter(copy),
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
   kernelNameSPICE  = kernelFileName.c_str();
   referenceFrame   = frameName.c_str();
}

//------------------------------------------------------------------------------
//  SpiceOrbitKernelWriter& operator=(const SpiceOrbitKernelWriter &copy)
//------------------------------------------------------------------------------
/**
 * This method copies data from the input SpiceKernelWriter instance to
 * "this" instance.
 *
 * @param    copy       object whose data to copy
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelWriter& SpiceOrbitKernelWriter::operator=(const SpiceOrbitKernelWriter &copy)
{
   if (&copy != this)
   {
      SpiceKernelWriter::operator=(copy);
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

      kernelNameSPICE   = kernelFileName.c_str();
      referenceFrame    = frameName.c_str();
   }

   return *this;
}

//------------------------------------------------------------------------------
//  ~SpiceOrbitKernelWriter()
//------------------------------------------------------------------------------
/**
 * This method deletes "this" SpiceKernelWriter instance.
 * (destructor)
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelWriter::~SpiceOrbitKernelWriter()
{
   if (fileOpen) FinalizeKernel();
}


//------------------------------------------------------------------------------
//  SpiceOrbitKernelWriter* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method clones the object.
 *
 * @return new object, cloned from "this" object.
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelWriter* SpiceOrbitKernelWriter::Clone(void) const
{
   SpiceOrbitKernelWriter * clonedSKW = new SpiceOrbitKernelWriter(*this);

   return clonedSKW;
}

//------------------------------------------------------------------------------
//  void WriteSegment(const A1Mjd &start, const A1Mjd &end,
//                    const StateArray &states, const EpochArray &epochs)
//------------------------------------------------------------------------------
/**
 * This method writes a segment to the SPK kernel.
 *
 * @param   start    start time of the segment data
 * @param   end      end time of the segment data
 * @param   states   array of states to write to the segment
 * @param   epochs   array or corresponding epochs
 *
 */
//------------------------------------------------------------------------------
void SpiceOrbitKernelWriter::WriteSegment(const A1Mjd &start, const A1Mjd &end,
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
   SpiceDouble  startSPICE = A1ToSpiceTime(start.Get());

   SpiceDouble  endSPICE = A1ToSpiceTime(end.Get());

   SpiceDouble  *epochArray;     // (deleted at end of method)
   epochArray = new SpiceDouble[numStates];
   for (Integer ii = 0; ii < numStates; ii++)
   {
      epochArray[ii] = A1ToSpiceTime(epochs.at(ii)->Get());
   }

   // put states into SpiceDouble arrays
   SpiceDouble  *stateArray;
   stateArray = new SpiceDouble[numStates * 6];

   for (Integer ii = 0; ii < numStates; ii++)
   {
      for (Integer jj = 0; jj < 6; jj++)
      {
         stateArray[(ii*6) + jj] = ((states.at(ii))->GetDataVector())[jj];
      }
   }

   // create a segment ID
   std::string         segmentID = "SPK_SEGMENT";
   ConstSpiceChar      *segmentIDSPICE = segmentID.c_str();

   // pass data to CSPICE method that writes a segment to a Data Type 13 kernel
   spkw13_c(handle, objectNAIFId, centralBodyNAIFId, referenceFrame, startSPICE,
            endSPICE, segmentIDSPICE, degree, numStates, stateArray, epochArray);

   if(failed_c())
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      //SpiceChar      err[MAX_LONG_MESSAGE];
      SpiceChar      *err = new SpiceChar[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::string errmsg = "Error writing ephemeris data to SPK file \"";
      errmsg += kernelFileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      delete [] err;
      throw UtilityException(errmsg);
   }
   delete [] epochArray;
   delete [] stateArray;
}

//------------------------------------------------------------------------------
//  void AddMetaData(const std::string &line, bool done)
//------------------------------------------------------------------------------
/**
 * This method writes meta data (comments) to the SPK kernel.
 *
 * @param   line    line of comments to add.
 * @param   bool    indicates whether or not this is the last line to add
 *                  (if so, file can be finalized)
 */
//------------------------------------------------------------------------------
void SpiceOrbitKernelWriter::AddMetaData(const std::string &line, bool done)
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

//------------------------------------------------------------------------------
//  void AddMetaData(const StringArray &lines, bool done)
//------------------------------------------------------------------------------
/**
 * This method writes meta data (comments) to the SPK kernel.
 *
 * @param   lines   lines of comments to add.
 * @param   bool    indicates whether or not this is the last line to add
 *                  (if so, file can be finalized)
 */
//------------------------------------------------------------------------------
void SpiceOrbitKernelWriter::AddMetaData(const StringArray &lines, bool done)
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

//------------------------------------------------------------------------------
//  SetBasicMetaData()
//------------------------------------------------------------------------------
/**
 * This method sets the 'basic' (i.e. written to every kernel)  meta data
 * (comments).
 *
 */
//------------------------------------------------------------------------------
void SpiceOrbitKernelWriter::SetBasicMetaData()
{
   basicMetaData.clear();
   std::string metaDataLine = "--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---\n";
   basicMetaData.push_back(metaDataLine);
   metaDataLine = ("SPK EPHEMERIS kernel for object " + objectName) + "\n";
   basicMetaData.push_back(metaDataLine);
   metaDataLine = "Generated on ";
   metaDataLine += GmatTimeUtil::FormatCurrentTime();
   metaDataLine += "\n";
   basicMetaData.push_back(metaDataLine);
   metaDataLine = "Generated by the General Mission Analysis Tool (GMAT) [Build ";
   metaDataLine += __DATE__;
   metaDataLine += " at ";
   metaDataLine += __TIME__;
   metaDataLine += "]\n";
   basicMetaData.push_back(metaDataLine);
   metaDataLine = "--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---\n";
   basicMetaData.push_back(metaDataLine);
}

//------------------------------------------------------------------------------
//  FinalizeKernel()
//------------------------------------------------------------------------------
/**
 * This method writes the meta data (comments) to the kernel and then closes it.
 *
 */
//------------------------------------------------------------------------------
void SpiceOrbitKernelWriter::FinalizeKernel()
{
   // write all the meta data to the file
   WriteMetaData();
   basicMetaData.clear();
   addedMetaData.clear();
   // close the SPK file
   spkcls_c(handle);
   fileOpen = false;
}


//------------------------------------------------------------------------------
//  WriteMetaData()
//------------------------------------------------------------------------------
/**
 * This method writes the meta data (comments) to the kernel.
 *
 */
//------------------------------------------------------------------------------
void SpiceOrbitKernelWriter::WriteMetaData()
{
   // create the temporary text file to hold the meta data
   std::string    tmpTxtFileName = TMP_TXT_FILE_NAME;
   tmpTxtFileName += objectName + ".txt";
   tmpTxtFile = fopen(tmpTxtFileName.c_str(), "w");

   if (!tmpTxtFile)
   {
      std::string errmsg = "Error opening temporary text file for SPK meta data, for object \"";
      errmsg += objectName + "\".  No meta data will be added to the file.\n";
      MessageInterface::PopupMessage(Gmat::WARNING_, errmsg);
      return;
   }

   // write the meta data to the temporary file (according to SPICE dcs, must use regular C routines)
   // close the temporary file, when done
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
   txtopr_(tmpTxt, &unit, txtLen);         // CSPICE method to open test file for reading
   spcac_(&handle, &unit, " ", " ", 1, 1); // CSPICE method to write comments to kernel
   if (failed_c())
   {
      ConstSpiceChar option[]   = "LONG"; // retrieve long error message
      SpiceInt       numErrChar = MAX_LONG_MESSAGE;
      //SpiceChar      err[MAX_LONG_MESSAGE];
      SpiceChar      *err = new SpiceChar[MAX_LONG_MESSAGE];
      getmsg_c(option, numErrChar, err);
      std::string errStr(err);
      std::string errmsg = "Error writing meta data to SPK file \"";
      errmsg += kernelFileName + "\".  Message received from CSPICE is: ";
      errmsg += errStr + "\n";
      reset_c();
      delete [] err;
      throw UtilityException(errmsg);
   }
   // close the text file
   ftncls_c(unit);                         // CSPICE method to close the text file
   // remove the temporary text file
   remove(tmpTxt);
   delete [] tmpTxt;
}

//---------------------------------
// private methods
//---------------------------------

//------------------------------------------------------------------------------
//  SpiceOrbitKernelWriter()
//------------------------------------------------------------------------------
/**
 * This method constructs an instance of SpiceOrbitKernelWriter.
 * (default constructor)
 *
 */
//------------------------------------------------------------------------------
SpiceOrbitKernelWriter::SpiceOrbitKernelWriter()
{
};
