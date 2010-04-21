//$Id:$
//------------------------------------------------------------------------------
//                              SpiceOrbitKernelWriter
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under
// MOMS Task order 124.
//
// Author: Wendy C. Shoan
// Created: 2009.11.25
//
/**
 * Definition of the SpiceOrbitKernelWriter, which writes SPICE data (kernel) files.
 * This class calls the JPL-supplied CSPICE routines to write the specified
 * SPICE file.  Currently, this class write SPK files of Data Type 13 (Hermite
 * Interpolation with Unequal time steps; geometric - no aberration).  Currently,
 * each SPK file written by GMAT contains data for one and only one object
 * (currently only spacecraft objects are handled).
 *
 * Calling code must create a SpiceOrbitKernelWriter for each file that is to be
 * written.
 *
 * The output SPK file name takes the form
 *       <objName>-<yyyymmdd>-<data-type>-<n>.bsp
 * where <objName>  is the name of the object for which the SPK file is created
 *       <yyyymmdd> is the date of file creation
 *       <data-type>is the two-digit identifier for the SPK data type (see
 *                  SPK documentation at NAIF website)
 *       <n>        is the incremental file counter
 *       .bsp       is the standard file extension for binary SPK files
 *
 * This code creates a temporary text file, required in order to include META-Data
 * (commentary) in the SPK file.  The file is deleted from the system after the
 * commentary is added to the SPK file.  The name of this temporary text file
 * takes the form
 *       GMATtmpSPKcmmnt<objName>.txt
 * where <objName>  is the name of the object for which the SPK file is created
 *
 */
//------------------------------------------------------------------------------

#ifndef SpiceOrbitKernelWriter_hpp
#define SpiceOrbitKernelWriter_hpp

#include <stdio.h>
#include "gmatdefs.hpp"
#include "A1Mjd.hpp"
#include "Rvector6.hpp"
#include "Rmatrix33.hpp"
#include "SpiceKernelWriter.hpp"
// include the appropriate SPICE C header
extern "C"
{
//#include "SpiceUsr.h"    // for CSPICE routines
#include "SpiceZfc.h"    // for CSPICE routines to add meta data
}

class GMAT_API SpiceOrbitKernelWriter :public SpiceKernelWriter
{
public:
   SpiceOrbitKernelWriter(const std::string       &objName,   const std::string &centerName,
                     Integer                 objNAIFId,  Integer            centerNAIFId,
                     const std::string       &fileName,  Integer            deg = 7,
                     const std::string       &frame = "J2000");
   SpiceOrbitKernelWriter(const SpiceOrbitKernelWriter &copy);
   SpiceOrbitKernelWriter& operator=(const SpiceOrbitKernelWriter &copy);
   ~SpiceOrbitKernelWriter();

   virtual SpiceOrbitKernelWriter* Clone() const;


//   StringArray GetValidFrames();
//   String      GetSPKName() const;
//   void        SetLeapSecondKernel(const std::string &lsk);

   void        WriteSegment(const A1Mjd &start, const A1Mjd &end,
                            const StateArray &states, const EpochArray &epochs);
   void        AddMetaData(const std::string &line,  bool done = false);
   void        AddMetaData(const StringArray &lines, bool done = false);
   void        FinalizeKernel();

protected:
   /// the name of the spacecraft or body for which the SPK is created
   std::string     objectName;
   /// the name of the central body
   std::string     centralBodyName;
   /// the file (kernel) name
   std::string     kernelFileName;
   // the reference frame
   std::string     frameName;

   // data converted to SPICE types, to pass into SPICE methods
   /// the kernel name
//   ConstSpiceChar  *kernelName;
   /// the target body or spacecraft NAIF Id (SPICE)
   SpiceInt        objectNAIFId;
   /// the central body NAIF Id (SPICE)
   SpiceInt        centralBodyNAIFId;
   /// the observer epoch time (SPICE) in Ephemeris (TDB) Time
//   SpiceDouble     et;
   /// the degree of interpolating polynomials to pass to SPICE
   SpiceInt        degree;
   /// the reference frame (SPICE)
   ConstSpiceChar  *referenceFrame;
   /// handle to the SPK file to which to write the data
   SpiceInt        handle;
   /// "Basic" metadata
   StringArray     basicMetaData;
   /// Added metadata
   StringArray     addedMetaData;
   /// flag indicating whether or not a file handle has been obtained, and the file is
   /// open for writing
   bool            fileOpen;
   /// the temporary text file
   FILE            *tmpTxtFile;
//   /// SPICE Julian Date of 2000 JAN 01 12:00:00
//   SpiceDouble     j2ET;

//   static const Integer     NUM_VALID_FRAMES;
//   static const std::string VALID_FRAMES[12];

   /// maximum number of characters for short, explanation of short, or
   /// long error message requested when calling CSPICE method getmsg_c
//   static const Integer     MAX_SHORT_MESSAGE;
//   static const Integer     MAX_EXPLAIN_MESSAGE;
//   static const Integer     MAX_LONG_MESSAGE;
//   static const Integer     MAX_CHAR_COMMENT;

   static       std::string TMP_TXT_FILE_NAME;

   void        SetBasicMetaData();
   /// method used to create the temporary text file, to use to set metadata (comments)
   /// on the SPK file
   void        WriteMetaData();

//   Integer     GetNaifID(const std::string &forBody);


};

#endif // SpiceOrbitKernelWriter_hpp
