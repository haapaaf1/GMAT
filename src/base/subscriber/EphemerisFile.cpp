//$Id$
//------------------------------------------------------------------------------
//                                  EphemerisFile
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Author: Linda Jun / NASA
// Created: 2009/09/02
//
/**
 * Writes a spacecraft orbit states or attitude to an ephemeris file either
 * CCSDS or SPK format.
 */
//------------------------------------------------------------------------------

#include "EphemerisFile.hpp"
#include "Publisher.hpp"             // for Instance()
#include "FileManager.hpp"           // for GetPathname()
#include "SubscriberException.hpp"   // for exception
#include "StringUtil.hpp"            // for ToString()
#include "FileUtil.hpp"              // for ParseFileExtension()
#include "TimeSystemConverter.hpp"   // for ValidateTimeFormat()
#include "LagrangeInterpolator.hpp"  // for LagrangeInterpolator
#include "RealUtilities.hpp"         // for IsEven()
#include "MessageInterface.hpp"

//#define DEBUG_EPHEMFILE
//#define DEBUG_EPHEMFILE_SET
//#define DEBUG_EPHEMFILE_INIT
//#define DEBUG_EPHEMFILE_OPEN
//#define DEBUG_EPHEMFILE_SPICE
//#define DEBUG_EPHEMFILE_SPICE_BUFFER
//#define DEBUG_EPHEMFILE_TIME
//#define DEBUG_EPHEMFILE_ORBIT
//#define DEBUG_EPHEMFILE_WRITE
//#define DEBUG_EPHEMFILE_RESTART
//#define DEBUG_EPHEMFILE_FINISH
//#define DEBUG_EPHEMFILE_SOLVER_DATA
#define DEBUG_EPHEMFILE_TEXT
//#define DBGLVL_EPHEMFILE_DATA 1
//#define DBGLVL_EPHEMFILE_MANEUVER 1
//#define DBGLVL_EPHEMFILE_PROP_CHANGE 1

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif

#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif

//---------------------------------
// static data
//---------------------------------
StringArray EphemerisFile::fileFormatList;
StringArray EphemerisFile::epochFormatList;
StringArray EphemerisFile::initialEpochList;
StringArray EphemerisFile::finalEpochList;
StringArray EphemerisFile::stepSizeList;
StringArray EphemerisFile::stateTypeList;
StringArray EphemerisFile::writeEphemerisList;
StringArray EphemerisFile::interpolatorTypeList;

const std::string
EphemerisFile::PARAMETER_TEXT[EphemerisFileParamCount - SubscriberParamCount] =
{
   "Spacecraft",            // SPACECRAFT
   "FileName",              // FILE_NAME
   "FileFormat",            // FILE_FORMAT
   "EpochFormat",           // EPOCH_FORMAT
   "InitialEpoch",          // INITIAL_EPOCH
   "FinalEpoch",            // FINAL_EPOCH
   "StepSize",              // STEP_SIZE
   "Interpolator",          // INTERPOLATOR
   "InterpolationOrder",    // INTERPOLATION_ORDER
   "StateType",             // STATE_TYPE
   "CoordinateSystem",      // COORDINATE_SYSTEM
   "WriteEphemeris",        // WRITE_EPHEMERIS
};

const Gmat::ParameterType
EphemerisFile::PARAMETER_TYPE[EphemerisFileParamCount - SubscriberParamCount] =
{
   Gmat::OBJECT_TYPE,       // SPACECRAFT
   Gmat::STRING_TYPE,       // FILE_NAME
   Gmat::ENUMERATION_TYPE,  // FILE_FORMAT
   Gmat::ENUMERATION_TYPE,  // EPOCH_FORMAT
   Gmat::ENUMERATION_TYPE,  // INITIAL_EPOCH
   Gmat::ENUMERATION_TYPE,  // FINAL_EPOCH
   Gmat::ENUMERATION_TYPE,  // STEP_SIZE
   Gmat::OBJECT_TYPE,       // INTERPOLATOR
   Gmat::INTEGER_TYPE,      // INTERPOLATION_ORDER
   Gmat::ENUMERATION_TYPE,  // STATE_TYPE
   Gmat::OBJECT_TYPE,       // COORDINATE_SYSTEM
   Gmat::BOOLEAN_TYPE		// Gmat::ENUMERATION_TYPE,  // WRITE_EPHEMERIS
};


//------------------------------------------------------------------------------
// EphemerisFile(const std::string &name)
//------------------------------------------------------------------------------
EphemerisFile::EphemerisFile(const std::string &name) :
   Subscriber          ("EphemerisFile", name),
   spacecraft          (NULL),
   coordSystem         (NULL),
   interpolator        (NULL),
   spkWriter           (NULL),
   oututPath           (""),
   filePath            (""),
   spacecraftName      (""),
   fileName            (""),
   fileFormat          ("CCSDS-OEM"),
   epochFormat         ("UTCGregorian"),
   initialEpoch        ("InitialSpacecraftEpoch"),
   finalEpoch          ("FinalSpacecraftEpoch"),
   stepSize            ("IntegratorSteps"),
   interpolatorName    ("Lagrange"),
   stateType           ("Cartesian"),
   coordSystemName     ("EarthMJ2000Eq"),
   writeEphemeris      (true),
   prevPropName        (""),
   currPropName        (""),
   interpolationOrder  (7),
   initialCount        (0),
   waitCount           (0),
   stepSizeInA1Mjd     (-999.999),
   stepSizeInSecs      (-999.999),
   initialEpochA1Mjd   (-999.999),
   finalEpochA1Mjd     (-999.999),
   nextOutEpoch        (-999.999),
   nextReqEpoch        (-999.999),
   currEpochInDays     (-999.999),
   currEpochInSecs     (-999.999),
   prevEpoch           (-999.999),
   prevProcTime        (-999.999),
   maneuverEpochInDays (-999.999),
   firstTimeWriting    (true),
   writingNewSegment   (true),
   useStepSize         (false),
   writeOrbit          (false),
   writeAttitude       (false),
   writeDataInDataCS   (true),
   processingLargeStep (false),
   spkWriteFailed      (false),
   prevRunState        (Gmat::IDLE)
{
   #ifdef DEBUG_EPHEMFILE
   MessageInterface::ShowMessage
      ("EphemerisFile::EphemerisFile() <%p>'%s' entered\n", this, GetName().c_str());
   #endif
   
   objectTypes.push_back(Gmat::EPHEMERIS_FILE);
   objectTypeNames.push_back("EphemerisFile");
   parameterCount = EphemerisFileParamCount;
   
   // Should I give non-blank fileName?
   if (fileName == "")
      fileName = name + ".eph";
   
   // Available enumeration type list, since it is static data, clear it first
   fileFormatList.clear();
   fileFormatList.push_back("CCSDS-OEM");
   fileFormatList.push_back("CCSDS-AEM");
   fileFormatList.push_back("SPK");
   
   epochFormatList.clear();
   epochFormatList.push_back("UTCGregorian");
   epochFormatList.push_back("UTCModJulian");
   epochFormatList.push_back("TAIGregorian");
   epochFormatList.push_back("TAIModJulian");
   epochFormatList.push_back("TTGregorian");
   epochFormatList.push_back("TTModJulian");
   epochFormatList.push_back("A1Gregorian");
   epochFormatList.push_back("A1ModJulian");
   
   initialEpochList.clear();
   initialEpochList.push_back("InitialSpacecraftEpoch");
   
   finalEpochList.clear();
   finalEpochList.push_back("FinalSpacecraftEpoch");
   
   stepSizeList.clear();
   stepSizeList.push_back("IntegratorSteps");
   
   stateTypeList.clear();
   stateTypeList.push_back("Cartesian");
   stateTypeList.push_back("Quaternion");
   
   writeEphemerisList.clear();
   writeEphemerisList.push_back("Yes");
   writeEphemerisList.push_back("No");
   
   interpolatorTypeList.clear();
   interpolatorTypeList.push_back("Lagrange");
   interpolatorTypeList.push_back("Hermite");
   interpolatorTypeList.push_back("SLERP");
   
   #ifdef DEBUG_EPHEMFILE
   MessageInterface::ShowMessage
      ("EphemerisFile::EphemerisFile() <%p>'%s' leaving\n", this, GetName().c_str());
   #endif
}


//------------------------------------------------------------------------------
// ~EphemerisFile()
//------------------------------------------------------------------------------
EphemerisFile::~EphemerisFile()
{
   #ifdef DEBUG_EPHEMFILE
   MessageInterface::ShowMessage
      ("EphemerisFile::~EphemerisFile() <%p>'%s' entered\n", this, GetName().c_str());
   #endif
   
   dstream.flush();
   dstream.close();
   if (interpolator != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (interpolator, interpolator->GetName(), "EphemerisFile::~EphemerisFile()()",
          "deleting local interpolator");
      #endif
      delete interpolator;
   }
   if (spkWriter != NULL && !spkWriteFailed)
   {
      FinalizeSpkFile();
      
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (spkWriter, "SPK writer", "EphemerisFile::~EphemerisFile()()",
          "deleting local SPK writer");
      #endif
      delete spkWriter;
   }
   #ifdef DEBUG_EPHEMFILE
   MessageInterface::ShowMessage
      ("EphemerisFile::~EphemerisFile() <%p>'%s' leaving\n", this, GetName().c_str());
   #endif
}


//------------------------------------------------------------------------------
// EphemerisFile(const EphemerisFile &ef)
//------------------------------------------------------------------------------
EphemerisFile::EphemerisFile(const EphemerisFile &ef) :
   Subscriber          (ef),
   spacecraft          (ef.spacecraft),
   coordSystem         (ef.coordSystem),
   interpolator        (NULL),
   spkWriter           (NULL),
   oututPath           (ef.oututPath),
   filePath            (ef.filePath),
   spacecraftName      (ef.spacecraftName),
   fileName            (ef.fileName),
   fileFormat          (ef.fileFormat),
   epochFormat         (ef.epochFormat),
   initialEpoch        (ef.initialEpoch),
   finalEpoch          (ef.finalEpoch),
   stepSize            (ef.stepSize),
   interpolatorName    (ef.interpolatorName),
   stateType           (ef.stateType),
   coordSystemName     (ef.coordSystemName),
   writeEphemeris      (ef.writeEphemeris),
   prevPropName        (ef.prevPropName),
   currPropName        (ef.currPropName),
   interpolationOrder  (ef.interpolationOrder),
   initialCount        (ef.initialCount),
   waitCount           (ef.waitCount),
   stepSizeInA1Mjd     (ef.stepSizeInA1Mjd),
   stepSizeInSecs      (ef.stepSizeInSecs),
   initialEpochA1Mjd   (ef.initialEpochA1Mjd),
   finalEpochA1Mjd     (ef.finalEpochA1Mjd),
   nextOutEpoch        (ef.nextOutEpoch),
   nextReqEpoch        (ef.nextReqEpoch),
   currEpochInDays     (ef.currEpochInDays),
   currEpochInSecs     (ef.currEpochInSecs),
   prevEpoch           (ef.prevEpoch),
   prevProcTime        (ef.prevProcTime),
   maneuverEpochInDays (ef.maneuverEpochInDays),
   writingNewSegment   (ef.writingNewSegment),
   useStepSize         (ef.useStepSize),
   writeOrbit          (ef.writeOrbit),
   writeAttitude       (ef.writeAttitude),
   writeDataInDataCS   (ef.writeDataInDataCS),
   processingLargeStep (ef.processingLargeStep),
   spkWriteFailed      (ef.spkWriteFailed),
   prevRunState        (ef.prevRunState)
{
   coordConverter = ef.coordConverter;
}


//------------------------------------------------------------------------------
// EphemerisFile& EphemerisFile::operator=(const EphemerisFile& ef)
//------------------------------------------------------------------------------
/**
 * The assignment operator
 */
//------------------------------------------------------------------------------
EphemerisFile& EphemerisFile::operator=(const EphemerisFile& ef)
{
   if (this == &ef)
      return *this;
   
   Subscriber::operator=(ef);
   
   spacecraft          = ef.spacecraft;
   coordSystem         = ef.coordSystem;
   interpolator        = NULL;
   spkWriter           = NULL;
   oututPath           = ef.oututPath;
   filePath            = ef.filePath;
   spacecraftName      = ef.spacecraftName;
   fileName            = ef.fileName;
   fileFormat          = ef.fileFormat;
   epochFormat         = ef.epochFormat;
   initialEpoch        = ef.initialEpoch;
   finalEpoch          = ef.finalEpoch;
   stepSize            = ef.stepSize;
   interpolatorName    = ef.interpolatorName;
   stateType           = ef.stateType;
   coordSystemName     = ef.coordSystemName;
   writeEphemeris      = ef.writeEphemeris;
   prevPropName        = ef.prevPropName;
   currPropName        = ef.currPropName;
   interpolationOrder  = ef.interpolationOrder;
   initialCount        = ef.initialCount;
   waitCount           = ef.waitCount;
   stepSizeInA1Mjd     = ef.stepSizeInA1Mjd;
   stepSizeInSecs      = ef.stepSizeInSecs;
   initialEpochA1Mjd   = ef.initialEpochA1Mjd;
   finalEpochA1Mjd     = ef.finalEpochA1Mjd;
   nextOutEpoch        = ef.nextOutEpoch;
   nextReqEpoch        = ef.nextReqEpoch;
   currEpochInDays     = ef.currEpochInDays;
   currEpochInSecs     = ef.currEpochInSecs;
   prevEpoch           = ef.prevEpoch;
   prevProcTime        = ef.prevProcTime;
   maneuverEpochInDays = ef.maneuverEpochInDays;
   writingNewSegment   = ef.writingNewSegment;
   useStepSize         = ef.useStepSize;
   writeOrbit          = ef.writeOrbit;
   writeAttitude       = ef.writeAttitude;
   writeDataInDataCS   = ef.writeDataInDataCS;
   processingLargeStep = ef.processingLargeStep;
   spkWriteFailed      = ef.spkWriteFailed;
   prevRunState        = ef.prevRunState;
   coordConverter      = ef.coordConverter;
   
   return *this;
}

//---------------------------------
// methods for this class
//---------------------------------

//------------------------------------------------------------------------------
// std::string GetFileName()
//------------------------------------------------------------------------------
std::string EphemerisFile::GetFileName()
{
   std::string fname = fileName;

   #ifdef DEBUG_EPHEMFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::GetFileName() fname=%s\n", fname.c_str());
   #endif
   
   try
   {
      FileManager *fm = FileManager::Instance();
      oututPath = fm->GetPathname(FileManager::OUTPUT_PATH);
      
      if (fileName == "")
      {
         fname = oututPath + instanceName + "." + fileFormat + ".eph";
      }
      else
      {
         // add output path if there is no path
         if (fileName.find("/") == fileName.npos &&
             fileName.find("\\") == fileName.npos)
         {
            fname = oututPath + fileName;
         }
      }
   }
   catch (GmatBaseException &e)
   {
      if (fileName == "")
         fname = instanceName + ".eph";
      
      MessageInterface::ShowMessage(e.GetFullMessage());
   }
   
   // If SPK file, extension should be ".bsp"
   if (fileFormat == "SPK")
   {
      std::string fileExt = GmatFileUtil::ParseFileExtension(fname, true);
      if (fileExt != ".bsp")
      {
         std::string ofname = fname;
         fname = GmatStringUtil::Replace(fname, fileExt, ".bsp");
         MessageInterface::ShowMessage
            ("*** WARNING *** SPK file extension should be \".bsp\", so "
             "file name '%s' changed to '%s'\n", ofname.c_str(), fname.c_str());
      }
      
      #ifdef DEBUG_EPHEMFILE_OPEN
      // Add current time to file in debug mode, SpiceKernelWriter throws an exceptin
      // if writing to exiting file
      std::string currTime = GmatTimeUtil::FormatCurrentTime(3);
      MessageInterface::ShowMessage("   currTime='%s'\n", currTime.c_str());
      fname = GmatStringUtil::Replace(fname, ".bsp", currTime + ".bsp");
      #endif
   }
   
   #ifdef DEBUG_EPHEMFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::GetFileName() returning fname\n   %s\n", fname.c_str());
   #endif
   
   return fname;
}


//------------------------------------------------------------------------------
// void ValidateParameters()
//------------------------------------------------------------------------------
void EphemerisFile::ValidateParameters()
{
   if (fileFormat == "SPK")
   {
      if (stateType == "Quaternion")
         throw SubscriberException
            ("Currently GMAT only supports writing orbit states in SPK format");
      
      if (interpolatorName == "Hermite" && GmatMathUtil::IsEven(interpolationOrder))
         throw SubscriberException
            ("The SPK file interpolation order must be an odd number when using "
             "Hermite interpolator");
   }
   else
   {
      // check for FileFormat and StateType
      if ((fileFormat == "CCSDS-OEM" && stateType == "Quaternion") ||
          (fileFormat == "CCSDS-AEM" && stateType == "Cartesian"))
         throw SubscriberException
            ("FileFormat \"" + fileFormat + "\" and StateType " + "\"" + stateType +
             "\" does not match for the EphemerisFile \"" + GetName() + "\"");
      
      // check interpolator type
      if (stepSize != "IntegratorSteps")
      {
         // check for StateType Cartesion and Interpolator
         if (stateType == "Cartesian" && interpolatorName != "Lagrange")
            throw SubscriberException
               ("The Interpolator must be \"Lagrange\" for StateType of \"Cartesian\" for "
                "the EphemerisFile \"" + GetName() + "\"");
         
         // check for StateType Quaternion and Interpolator
         if (stateType == "Quaternion" && interpolatorName != "SLERP")
            throw SubscriberException
               ("The Interpolator must be \"SLERP\" for StateType of \"Quaternion\" for "
                "the EphemerisFile \"" + GetName() + "\"");
      }
   }
   
   // check for NULL pointers
   if (spacecraft == NULL)
      throw SubscriberException
         ("The Spacecraft \"" + spacecraftName + "\" has not been set for "
          "the EphemerisFile \"" + GetName() + "\"");
   
   if (coordSystem == NULL)
      throw SubscriberException
         ("The CoordinateSystem \"" + coordSystemName + "\" has not been set for "
          "the EphemerisFile \"" + GetName() + "\"");
   
   if (theDataCoordSystem == NULL)
      throw SubscriberException
         ("The internal CoordinateSystem which orbit data represents has not been set for "
          "the EphemerisFile \"" + GetName() + "\"");
   
}


//----------------------------------
// methods inherited from Subscriber
//----------------------------------

//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool EphemerisFile::Initialize()
{
   #ifdef DEBUG_EPHEMFILE_INIT
   MessageInterface::ShowMessage
      ("EphemerisFile::Initialize() <%p>'%s' entered, active=%d, isInitialized=%d\n"
       "   fileFormat='%s', stateType='%s'\n", this, GetName().c_str(), active,
       isInitialized, fileFormat.c_str(), stateType.c_str());
   #endif
   
   Subscriber::Initialize();
   
   // Do some validation, reset flags and clear buffers
   ValidateParameters();
   
   // Set FileType
   if (fileFormat == "CCSDS-OEM")
      fileType = CCSDS_OEM;
   else if (fileFormat == "CCSDS-AEM")
      fileType = CCSDS_AEM;
   else if (fileFormat == "SPK" && stateType == "Cartesian")
      fileType = SPK_ORBIT;
   else if (fileFormat == "SPK" && stateType == "Quaternion")
      fileType = SPK_ATTITUDE;
   else
      throw SubscriberException
         ("FileFormat \"" + fileFormat + "\" is not valid");
   
   // Initialize data
   firstTimeWriting = true;
   prevPropName = "";
   InitializeData();
   maneuversHandled.clear();
   
   #ifdef DEBUG_EPHEMFILE_INIT
   MessageInterface::ShowMessage
      ("   fileType=%d, spacecraft=<%p>'%s', coordSystem=<%p>'%s'\n", fileType,
       spacecraft, spacecraft->GetName().c_str(), coordSystem,
       coordSystem->GetName().c_str());
   #endif
   
   // If active and not initialized already, open report file
   if (active && !isInitialized)
   {
      if (!OpenEphemerisFile())
      {
         #ifdef DEBUG_EPHEMFILE_INIT
         MessageInterface::ShowMessage
            ("EphemerisFile::Initialize() <%p>'%s' returning false\n",
             this, GetName().c_str());
         #endif
         return false;
      }
      
      isInitialized = true;
   }
   
   // Create interpolator if needed
   CreateInterpolator();
   
   // Determine orbit or attitude, set to boolean to avoid string comparison
   if (stateType == "Cartesian")
      writeOrbit = true;
   else
      writeAttitude = true;
   
   // Determine output coordinate system, set to boolean to avoid string comparison
   // We don't need conversion for SPK_ORBIT. SpiceKernelWriter assumes it is in
   // J2000Eq frame for now
   if (fileType == CCSDS_OEM &&
       theDataCoordSystem->GetName() != coordSystemName)
      writeDataInDataCS = false;
   
   // Determine initial and final epoch in A1ModJulian, this format is what spacecraft
   // currently outputs.
   Real dummyA1Mjd = -999.999;
   std::string epochStr;
   
   if (initialEpoch != "InitialSpacecraftEpoch")
      TimeConverterUtil::Convert(epochFormat, dummyA1Mjd, initialEpoch,
                                 "A1ModJulian", initialEpochA1Mjd, epochStr);
   
   if (finalEpoch != "FinalSpacecraftEpoch")
      TimeConverterUtil::Convert(epochFormat, dummyA1Mjd, finalEpoch,
                                 "A1ModJulian", finalEpochA1Mjd, epochStr);
   
   // Set solver iteration option to none. We only writes solutions to a file
   mSolverIterOption = SI_NONE;
   
   #ifdef __USE_CCSDS_FILE__
   // Set CCSDS header and meta data pointer
   if (fileType == CCSDS_OEM)
   {
      ccsdsOemData.SetHeader(&ccsdsHeader);
      ccsdsOemData.SetMetaData(&ccsdsOemMetaData);
      #ifdef DEBUG_EPHEMFILE_INIT
      MessageInterface::ShowMessage
         ("   Setting ccsdsHeader and ccsdsOemMetaData to ccsdsOemData\n");
      #endif
   }
   #endif
   
   // Create SpiceKernelWriter
   if (fileType == SPK_ORBIT)
      CreateSpiceKernelWriter();
   
   // Clear manuvers handled array
   maneuversHandled.clear();
   
   #ifdef DEBUG_EPHEMFILE_INIT
   MessageInterface::ShowMessage
      ("EphemerisFile::Initialize() <%p>'%s' returning true, writeOrbit=%d, "
       "writeAttitude=%d, writeDataInDataCS=%d, initialEpochA1Mjd=%f, finalEpochA1Mjd=%f\n",
       this, GetName().c_str(), writeOrbit, writeAttitude, writeDataInDataCS,
       initialEpochA1Mjd, finalEpochA1Mjd);
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// virtual void SetProvider(GmatBase *provider)
//------------------------------------------------------------------------------
void EphemerisFile::SetProvider(GmatBase *provider)
{
   Subscriber::SetProvider(provider);
   HandlePropagatorChange(provider);
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the EphemerisFile.
 *
 * @return clone of the EphemerisFile.
 *
 */
//------------------------------------------------------------------------------
GmatBase* EphemerisFile::Clone(void) const
{
   return (new EphemerisFile(*this));
}


//---------------------------------------------------------------------------
// void Copy(const GmatBase* orig)
//---------------------------------------------------------------------------
/**
 * Sets this object to match another one.
 * 
 * @param orig The original that is being copied.
 */
//---------------------------------------------------------------------------
void EphemerisFile::Copy(const GmatBase* orig)
{
   operator=(*((EphemerisFile *)(orig)));
}


//------------------------------------------------------------------------------
// virtual bool TakeAction(const std::string &action,
//                         const std::string &actionData = "");
//------------------------------------------------------------------------------
/**
 * This method performs action.
 *
 * @param <action> action to perform
 * @param <actionData> action data associated with action
 * @return true if action successfully performed
 *
 */
//------------------------------------------------------------------------------
bool EphemerisFile::TakeAction(const std::string &action,
                            const std::string &actionData)
{
   #ifdef DEBUG_EPHEMFILE_ACTION
   MessageInterface::ShowMessage
      ("EphemerisFile::TakeAction() action=%s, actionData=%s\n", action.c_str(),
       actionData.c_str());
   #endif
   
   if (action == "Clear")
   {
      return true;
   }
   
   if (action == "Finalize")
   {
      return true;
   }
   
   return false;
}


//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool EphemerisFile::RenameRefObject(const Gmat::ObjectType type,
                                 const std::string &oldName,
                                 const std::string &newName)
{
   return Subscriber::RenameRefObject(type, oldName, newName);
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string EphemerisFile::GetParameterText(const Integer id) const
{
    if (id >= SubscriberParamCount && id < EphemerisFileParamCount)
        return PARAMETER_TEXT[id - SubscriberParamCount];
    else
        return Subscriber::GetParameterText(id);
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer EphemerisFile::GetParameterID(const std::string &str) const
{
   for (Integer i = SubscriberParamCount; i < EphemerisFileParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - SubscriberParamCount])
         return i;
   }
   return Subscriber::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType EphemerisFile::GetParameterType(const Integer id) const
{
    if (id >= SubscriberParamCount && id < EphemerisFileParamCount)
        return PARAMETER_TYPE[id - SubscriberParamCount];
    else
        return Subscriber::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string EphemerisFile::GetParameterTypeString(const Integer id) const
{
   if (id >= SubscriberParamCount && id < EphemerisFileParamCount)
      return EphemerisFile::PARAM_TYPE_STRING[GetParameterType(id)];
   else
      return Subscriber::GetParameterTypeString(id);

}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool EphemerisFile::IsParameterReadOnly(const Integer id) const
{
   if (id == SOLVER_ITERATIONS)
      return true;
   
   return Subscriber::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
// Gmat::ObjectType GetPropertyObjectType(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieves object type of parameter of given id.
 *
 * @param <id> ID for the parameter.
 *
 * @return parameter ObjectType
 */
//---------------------------------------------------------------------------
Gmat::ObjectType EphemerisFile::GetPropertyObjectType(const Integer id) const
{
   switch (id)
   {
   case SPACECRAFT:
      return Gmat::SPACECRAFT;
   case INTERPOLATOR:
      return Gmat::INTERPOLATOR;
   case COORDINATE_SYSTEM:
      return Gmat::COORDINATE_SYSTEM;
   default:
      return Subscriber::GetPropertyObjectType(id);
   }
}


//---------------------------------------------------------------------------
// const StringArray& GetPropertyEnumStrings(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Retrieves eumeration symbols of parameter of given id.
 *
 * @param <id> ID for the parameter.
 *
 * @return list of enumeration symbols
 */
//---------------------------------------------------------------------------
const StringArray& EphemerisFile::GetPropertyEnumStrings(const Integer id) const
{
   switch (id)
   {
   case FILE_FORMAT:
      return fileFormatList;
   case EPOCH_FORMAT:
      return epochFormatList;
   case INITIAL_EPOCH:
      return initialEpochList;
   case FINAL_EPOCH:
      return finalEpochList;
   case STEP_SIZE:
      return stepSizeList;
   case STATE_TYPE:
      return stateTypeList;
//   case WRITE_EPHEMERIS:			This parameter has boolean type, not string		// made a change
//      return writeEphemerisList;      
   case INTERPOLATOR:
      return interpolatorTypeList;
   default:
      return Subscriber::GetPropertyEnumStrings(id);
   }
}

//------------------------------------------------------------------------------
// bool GetBooleanParameter(const Integer id) const
//------------------------------------------------------------------------------
bool EphemerisFile::GetBooleanParameter(const Integer id) const
{
	switch (id)
	{
	case WRITE_EPHEMERIS:
		return writeEphemeris;
	default:
		return Subscriber::GetBooleanParameter(id);
	}
}


//------------------------------------------------------------------------------
// bool SetBooleanParameter(const Integer id, const bool value)
//------------------------------------------------------------------------------
bool EphemerisFile::SetBooleanParameter(const Integer id, const bool value)
{
	switch (id)
	{
	case WRITE_EPHEMERIS:
		writeEphemeris = value;
		return writeEphemeris;
	default:
      return Subscriber::SetBooleanParameter(id, value);
   }
}




//------------------------------------------------------------------------------
// Integer GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
Integer EphemerisFile::GetIntegerParameter(const Integer id) const
{
   switch (id)
   {
   case INTERPOLATION_ORDER:
      return interpolationOrder;
   default:
      return Subscriber::GetIntegerParameter(id);
   }
}


//------------------------------------------------------------------------------
// Integer SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
Integer EphemerisFile::SetIntegerParameter(const Integer id, const Integer value)
{
   switch (id)
   {
   case INTERPOLATION_ORDER:
      if (value >= 1 && value <= 10)
      {
         interpolationOrder = value;
         return value;
      }
      else
      {
         SubscriberException se;
         se.SetDetails(errorMessageFormat.c_str(),
                       GmatStringUtil::ToString(value, 1).c_str(),
                       GetParameterText(INTERPOLATION_ORDER).c_str(),
                       "1 <= Integer Number <= 10");
         throw se;
      }
   default:
      return Subscriber::SetIntegerParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string EphemerisFile::GetStringParameter(const Integer id) const
{
   switch (id)
   {
   case SPACECRAFT:
      return spacecraftName;
   case FILE_NAME:
      return fileName;
   case FILE_FORMAT:
      return fileFormat;
   case EPOCH_FORMAT:
      return epochFormat;
   case INITIAL_EPOCH:
      return initialEpoch;
   case FINAL_EPOCH:
      return finalEpoch;
   case STEP_SIZE:
      return stepSize;
   case INTERPOLATOR:
      return interpolatorName;
   case STATE_TYPE:
      return stateType;
   case COORDINATE_SYSTEM:
      return coordSystemName;
//   case WRITE_EPHEMERIS:
//      return writeEphemeris;
   default:
      return Subscriber::GetStringParameter(id);
   }
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
std::string EphemerisFile::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool EphemerisFile::SetStringParameter(const Integer id, const std::string &value)
{
   #ifdef DEBUG_EPHEMFILE_SET
   MessageInterface::ShowMessage
      ("EphemerisFile::SetStringParameter() this=<%p>'%s' entered, id=%d, "
       "value='%s'\n", this, GetName().c_str(), id, value.c_str());
   #endif
   
   switch (id)
   {
   case SPACECRAFT:
      spacecraftName = value;
      return true;
   case FILE_NAME:
      #ifdef DEBUG_EPHEMFILE_SET
      MessageInterface::ShowMessage
         ("EphemerisFile::SetStringParameter() Setting filename '%s' to "
          "EphemerisFile '%s'\n", value.c_str(), instanceName.c_str());
      #endif
      
      fileName = value;
      filePath = fileName;
      
      if (fileName.find("/") == fileName.npos &&
          fileName.find("\\") == fileName.npos)
         filePath = oututPath + fileName;
      
      return true;
   case FILE_FORMAT:
      if (find(fileFormatList.begin(), fileFormatList.end(), value) !=
          fileFormatList.end())
      {
         fileFormat = value;
         return true;
      }
      else
      {
         HandleError(FILE_FORMAT, value, fileFormatList);
      }
   case EPOCH_FORMAT:
      if (find(epochFormatList.begin(), epochFormatList.end(), value) !=
          epochFormatList.end())
      {
         epochFormat = value;
         return true;
      }
      else
      {
         HandleError(EPOCH_FORMAT, value, epochFormatList);
      }
   case INITIAL_EPOCH:
      if (find(initialEpochList.begin(), initialEpochList.end(), value) !=
          initialEpochList.end())
      {
         initialEpoch = value;
         return true;
      }
      else
      {
         if (SetEpoch(INITIAL_EPOCH, value, initialEpochList))
         {
            initialEpoch = value;
            return true;
         }
         else
            return false;
      }
   case FINAL_EPOCH:
      if (find(finalEpochList.begin(), finalEpochList.end(), value) !=
          finalEpochList.end())
      {
         finalEpoch = value;
         return true;
      }
      else
      {
         return SetEpoch(FINAL_EPOCH, value, finalEpochList);
      }
   case STEP_SIZE:
      if (find(stepSizeList.begin(), stepSizeList.end(), value) !=
          stepSizeList.end())
      {
         stepSize = value;
         return true;
      }
      else
      {
         return SetStepSize(STEP_SIZE, value, stepSizeList);
      }
   case INTERPOLATOR:
      interpolatorName = value;
      return true;
   case STATE_TYPE:
      if (find(stateTypeList.begin(), stateTypeList.end(), value) !=
          stateTypeList.end())
      {
         stateType = value;
         return true;
      }
      else
      {
         HandleError(STATE_TYPE, value, stateTypeList);
      }
   case COORDINATE_SYSTEM:
      coordSystemName = value;
      return true;
//   case WRITE_EPHEMERIS:
//      if (find(writeEphemerisList.begin(), writeEphemerisList.end(), value) !=
//          writeEphemerisList.end())
//      {
//         writeEphemeris = value;
//         return true;
//      }
//      else
//      {
//         HandleError(WRITE_EPHEMERIS, value, writeEphemerisList);
//      }
   default:
      return Subscriber::SetStringParameter(id, value);
   }
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const std::string &label,
//                         const std::string &value)
//------------------------------------------------------------------------------
bool EphemerisFile::SetStringParameter(const std::string &label,
                                       const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
// virtual GmatBase* GetRefObject(const Gmat::ObjectType type,
//                                const std::string &name)
//------------------------------------------------------------------------------
GmatBase* EphemerisFile::GetRefObject(const Gmat::ObjectType type,
                                      const std::string &name)
{
   if (type == Gmat::SPACECRAFT)
      return spacecraft;
   
   if (type == Gmat::COORDINATE_SYSTEM)
      return coordSystem;
   
   return Subscriber::GetRefObject(type, name);
}


//------------------------------------------------------------------------------
// virtual bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                           const std::string &name = "")
//------------------------------------------------------------------------------
bool EphemerisFile::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                 const std::string &name)
{
   #if DBGLVL_EPHEMFILE_REF_OBJ
   MessageInterface::ShowMessage
      ("EphemerisFile::SetRefObject() <%p>'%s' entered, obj=%p, name=%s, objtype=%s, "
       "objname=%s\n", this, GetName().c_str(), obj, name.c_str(), obj->GetTypeName().c_str(),
       obj->GetName().c_str());
   #endif
   
   if (type == Gmat::SPACECRAFT && name == spacecraftName)
   {
      spacecraft = (Spacecraft*)obj;
      return true;
   }
   else if (type == Gmat::COORDINATE_SYSTEM && name == coordSystemName)
   {
      coordSystem = (CoordinateSystem*)obj;
      return true;
   }
   
   return Subscriber::SetRefObject(obj, type, name);
}


//------------------------------------------------------------------------------
// virtual const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
const StringArray& EphemerisFile::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   refObjectNames.clear();
   
   if (type == Gmat::SPACECRAFT || type == Gmat::UNKNOWN_OBJECT)
      refObjectNames.push_back(spacecraftName);
   
   if (type == Gmat::COORDINATE_SYSTEM || type == Gmat::UNKNOWN_OBJECT)
      refObjectNames.push_back(coordSystemName);
   
   return refObjectNames;
}


//--------------------------------------
// protected methods
//--------------------------------------

//------------------------------------------------------------------------------
// void InitializeData()
//------------------------------------------------------------------------------
void EphemerisFile::InitializeData()
{
   #ifdef DEBUG_EPHEMFILE_RESTART
   MessageInterface::ShowMessage
      ("===== EphemerisFile::InitializeData() entered\n");
   #endif
   
   epochsOnWaiting.clear();
   
   if (interpolator != NULL)
      interpolator->Clear();
   
   initialCount        = 0;
   waitCount           = 0;
   nextOutEpoch        = -999.999;
   nextReqEpoch        = -999.999;
   currEpochInDays     = -999.999;
   currEpochInSecs     = -999.999;
   prevEpoch           = -999.999;
   prevProcTime        = -999.999;
   writingNewSegment   = true;
   
   #ifdef DEBUG_EPHEMFILE_RESTART
   MessageInterface::ShowMessage
      ("===== EphemerisFile::InitializeData() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// void CreateInterpolator()
//------------------------------------------------------------------------------
void EphemerisFile::CreateInterpolator()
{
   #ifdef DEBUG_EPHEMFILE_INTERPOLATOR
   MessageInterface::ShowMessage
      ("EphemerisFile::CreateInterpolator() entered, interpolator=<%p>'%s'\n",
       interpolator, interpolator ? interpolator->GetName().c_str() : "NULL");
   #endif

   // if not using step size just return
   if (!useStepSize)
      return;
   
   // If interpolator is not NULL, delete it first
   if (interpolator != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (interpolator, interpolator->GetName(), "EphemerisFile::CreateInterpolator()",
          "deleting local interpolator");
      #endif
      delete interpolator;
      interpolator = NULL;
   }
   
   // Create Interpolator
   if (interpolatorName == "Lagrange")
   {
      interpolator = new LagrangeInterpolator(instanceName+"_Lagrange", 6,
                                              interpolationOrder);
      
      // Set force interpolation to false to collect more data if needed
      interpolator->SetForceInterpolation(false);
      
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Add
         (interpolator, interpolator->GetName(), "EphemerisFile::CreateInterpolator()",
          "interpolator = new LagrangeInterpolator()");
      #endif
   }
   else if (interpolatorName == "SLERP")
   {
      throw SubscriberException("The SLERP Interpolator is not ready\n");
      //interpolator = new SLERP;
   }
   
   #ifdef DEBUG_EPHEMFILE_INTERPOLATOR
   MessageInterface::ShowMessage
      ("EphemerisFile::CreateInterpolator() leaving, interpolator=<%p>'%s'\n",
       interpolator, interpolator ? interpolator->GetName().c_str() : "NULL");
   #endif
}


//------------------------------------------------------------------------------
// void CreateSpiceKernelWriter()
//------------------------------------------------------------------------------
void EphemerisFile::CreateSpiceKernelWriter()
{
   #ifdef DEBUG_EPHEMFILE_SPICE
   MessageInterface::ShowMessage
      ("EphemerisFile::CreateSpiceKernelWriter() entered, spkWriter=<%p>\n",
       spkWriter);
   #endif
   
   //=======================================================
   #ifdef __USE_SPICE__
   //=======================================================
   // If spkWriter is not NULL, delete it first
   if (spkWriter != NULL)
   {
      #ifdef DEBUG_MEMORY
      MemoryTracker::Instance()->Remove
         (spkWriter, "spkWriter", "EphemerisFile::CreateSpiceKernelWriter()",
          "deleting local spkWriter");
      #endif
      delete spkWriter;
      spkWriter = NULL;
   }
   
   std::string name = instanceName;
   std::string centerName = spacecraft->GetOriginName();
   Integer objNAIFId = spacecraft->GetIntegerParameter("NAIFId");
   Integer centerNAIFId = (spacecraft->GetOrigin())->GetIntegerParameter("NAIFId");
   
   #ifdef DEBUG_EPHEMFILE_SPICE
   MessageInterface::ShowMessage
      ("   Creating SpiceKernelWriter with name='%s', centerName='%s', "
       "objNAIFId=%d, centerNAIFId=%d, fileName='%s', interpolationOrder=%d\n",
       name.c_str(), centerName.c_str(), objNAIFId, centerNAIFId,
       fileName.c_str(), interpolationOrder);
   #endif
   
   try
   {
      spkWriter =
         new SpiceKernelWriter(name, centerName, objNAIFId, centerNAIFId,
                               fileName, interpolationOrder, "J2000");
   }
   catch (BaseException &e)
   {
      #ifdef DEBUG_EPHEMFILE_SPICE
      MessageInterface::ShowMessage(e.GetFullMessage());
      #endif
      throw;
   }
   
   #ifdef DEBUG_MEMORY
   MemoryTracker::Instance()->Add
      (spkWriter, "spkWriter", "EphemerisFile::CreateSpiceKernelWriter()",
       "spkWriter = new SpiceKernelWriter()");
   #endif
   
   //=======================================================
   #else
   //=======================================================
   MessageInterface::ShowMessage
      ("*** WARNING *** Use of SpiceKernelWriter is turned off\n");
   //=======================================================
   #endif
   //=======================================================
   
   #ifdef DEBUG_EPHEMFILE_SPICE
   MessageInterface::ShowMessage
      ("EphemerisFile::CreateSpiceKernelWriter() leaving, spkWriter=<%p>\n",
       spkWriter);
   #endif
}


//------------------------------------------------------------------------------
// bool OpenEphemerisFile()
//------------------------------------------------------------------------------
bool EphemerisFile::OpenEphemerisFile()
{
   #ifdef DEBUG_EPHEMFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::OpenEphemerisFile() entered, fileName = %s\n", fileName.c_str());
   #endif
   
   fileName = GetFileName();
   bool retval = true;
   
   #ifdef __USE_CCSDS_FILE__
   // Open CCSDS output file
   if (fileType == CCSDS_OEM)
   {
      #ifdef DEBUG_EPHEMFILE_OPEN
      MessageInterface::ShowMessage("   About to open CCSDS output file\n");
      #endif
      
      CCSDSOEMDataFile ccsdsOutFile("theFile");
      ccsdsOutFile.SetReadWriteMode("w");
      ccsdsOutFile.SetFileName(fileName);
      ccsdsOutFile.Initialize();
   }
   #endif
   
   #ifdef DEBUG_EPHEMFILE_TEXT
   // Close the stream if it is open
   if (dstream.is_open())
      dstream.close();
   
   std::string debugFileName = fileName + ".txt";
   dstream.open(debugFileName.c_str());
   if (dstream.is_open())
   {
      retval = true;
      MessageInterface::ShowMessage("   '%s' is opened for debug\n", debugFileName.c_str());
   }
   else
   {
      retval = false;
      MessageInterface::ShowMessage("   '%s' was failed open\n", debugFileName.c_str());
   }
   #endif
   
   #ifdef DEBUG_EPHEMFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::OpenEphemerisFile() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool CheckInitialAndFinalEpoch()
//------------------------------------------------------------------------------
bool EphemerisFile::CheckInitialAndFinalEpoch()
{
   // Check initial and final epoch for writing, dat[0] is epoch
   bool writeData = false;
   
   // From InitialSpacecraftEpoch to FinalSpacecraftEpoch
   if (initialEpochA1Mjd == -999.999 && finalEpochA1Mjd == -999.999)
   {
      writeData = true;
   }
   // From InitialSpacecraftEpoch to user specified final epoch
   else if (initialEpochA1Mjd == -999.999 && finalEpochA1Mjd != -999.999)
   {
      if (currEpochInDays <= finalEpochA1Mjd)
         writeData = true;
   }
   // From user specified initial epoch to FinalSpacecraftEpoch
   else if (initialEpochA1Mjd != -999.999 && finalEpochA1Mjd == -999.999)
   {
      if (currEpochInDays >= initialEpochA1Mjd)
         writeData = true;
   }
   // From user specified initial epoch to user specified final epoch
   else
   {
      if (currEpochInDays >= initialEpochA1Mjd && currEpochInDays <= finalEpochA1Mjd)
         writeData = true;
   }
   
   return writeData;
}


//------------------------------------------------------------------------------
// void HandleCcsdsOrbitData(bool writeData)
//------------------------------------------------------------------------------
void EphemerisFile::HandleCcsdsOrbitData(bool writeData)
{   
   // Check if it is time to write
   bool timeToWrite = IsTimeToWrite(currEpochInSecs, currState);
   
   // LagrangeInterpolator's maximum buffer size is set to 80 which can hold
   // 80 min of data assuming average of 60 sec data interveval.
   // Check at least 10 min interval for large step size, since interpolater
   // buffer size is limited
   if (!timeToWrite)
   {
      if ((currEpochInSecs - prevProcTime) > 600.0)
         timeToWrite = true;
   }
   
   if (timeToWrite)
      prevProcTime = currEpochInSecs;
   
   //------------------------------------------------------------
   // write data to file
   //------------------------------------------------------------
   // Now actually write data
   if (writeData && timeToWrite)
   {
      if (firstTimeWriting)
         WriteHeader();
      
      if (writingNewSegment)
         WriteMetadata();
      
      if (fileType == CCSDS_AEM && (firstTimeWriting || writingNewSegment))
         WriteString("DATA_START\n");
      
      if (writeOrbit)
      {
         if (useStepSize)
            WriteCcsdsOrbitAt(nextReqEpoch, currState);
         else
            WriteCcsdsOrbit(currEpochInSecs, currState);
      }
      else if (writeAttitude)
      {
         WriteCcsdsAttitude();
      }
      
      if (firstTimeWriting)
         firstTimeWriting = false;
      
      if (writingNewSegment)
         writingNewSegment = false;
   }
}


//------------------------------------------------------------------------------
// void HandleSpkOrbitData(bool writeData)
//------------------------------------------------------------------------------
void EphemerisFile::HandleSpkOrbitData(bool writeData)
{
   if (writeData)
   {
      BufferSpkOrbitData(currEpochInDays, currState);
      
      #ifdef DEBUG_EPHEMFILE_TEXT
      DebugWriteOrbit(currEpochInSecs, currState);
      #endif
   }
}


//------------------------------------------------------------------------------
// void RestartInterpolation(const std::string &comments = "")
//------------------------------------------------------------------------------
/**
 * Resets interpolator to start new segments of data.
 */
//------------------------------------------------------------------------------
void EphemerisFile::RestartInterpolation(const std::string &comments)
{
   #ifdef DEBUG_EPHEMFILE_RESTART
   MessageInterface::ShowMessage
      ("===== EphemerisFile::RestartInterpolation() entered\n");
   #endif
   
   WriteComments(comments);
   InitializeData();
   
   #ifdef DEBUG_EPHEMFILE_RESTART
   MessageInterface::ShowMessage
      ("===== EphemerisFile::RestartInterpolation() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// bool IsTimeToWrite(Real epochInSecs, Real state[6])
//------------------------------------------------------------------------------
/*
 * Determines if it is time to write to ephemeris file based on the step size.
 *
 * @param epochInSecs Epoch in seconds
 */
//------------------------------------------------------------------------------
bool EphemerisFile::IsTimeToWrite(Real epochInSecs, Real state[6])
{
   #ifdef DEBUG_EPHEMFILE_TIME
   MessageInterface::ShowMessage
      ("EphemerisFile::IsTimeToWrite() entered, epochInSecs=%.16f, writingNewSegment=%d\n",
       epochInSecs, writingNewSegment);
   DebugWriteTime("current ", epochInSecs);
   #endif
   bool retval = true;
   
   // If writing at specified interval step, do checking
   if (useStepSize)
   {
      // Add data points
      if (writeOrbit)
      {
         // If staring new segment, we want add data to interpolator
         if (epochInSecs > prevEpoch)
         {
            #ifdef DEBUG_EPHEMFILE_TIME
            DebugWriteTime("===== Adding to interpolator ", epochInSecs);
            #endif
            
            interpolator->AddPoint(epochInSecs, state);
            prevEpoch = epochInSecs;
         }
         else
         {
            #ifdef DEBUG_EPHEMFILE_TIME
            MessageInterface::ShowMessage
               ("========== skipping epoch<=prevEpoch epochInSecs=%.16f, prevEpoch=%.16f\n",
                epochInSecs, prevEpoch);
            #endif
         }
      }
      else if (writeAttitude)
      {
         #ifdef DEBUG_EPHEMFILE_TIME
         MessageInterface::ShowMessage("Adding points to interpolator is todo work\n");
         #endif
      }
      
      #ifdef DEBUG_EPHEMFILE_TIME
      MessageInterface::ShowMessage
         ("===== processingLargeStep=%d, waitCount=%d\n", processingLargeStep, waitCount);
      #endif
      
      // If step size is to large, we may miss the data points since interpolator
      // buffer size is limited. So do additional process here.
      if (processingLargeStep)
      {
         waitCount++;
         
         if (waitCount >= interpolationOrder / 2)
         {
            #ifdef DEBUG_EPHEMFILE_TIME
            MessageInterface::ShowMessage
               ("   waitCount=%d, Calling ProcessEpochsOnWaiting()\n", waitCount);
            #endif
            
            ProcessEpochsOnWaiting(false);
            waitCount = 0;
            processingLargeStep = false;
         }
      }
      
      // compute next output time
      if (writingNewSegment)
      {
         nextOutEpoch = epochInSecs;
         nextReqEpoch = epochInSecs;
         retval = true;
      }
      else
      {         
         if (epochInSecs >= nextOutEpoch)
         {
            nextOutEpoch = nextOutEpoch + stepSizeInSecs;
            
            if (find(epochsOnWaiting.begin(), epochsOnWaiting.end(), nextOutEpoch) ==
                epochsOnWaiting.end())
            {
               epochsOnWaiting.push_back(nextOutEpoch);
               nextReqEpoch = nextOutEpoch;
               #ifdef DEBUG_EPHEMFILE_TIME
               DebugWriteTime("===== Adding 1 ", nextOutEpoch);
               #endif
            }
            
            // Handle step size less than integrator step size
            Real nextOut = nextOutEpoch;
            while (nextOut <= epochInSecs)
            {
               // Compute new output time
               nextOut = nextOut + stepSizeInSecs;
               
               if (find(epochsOnWaiting.begin(), epochsOnWaiting.end(), nextOut) ==
                   epochsOnWaiting.end())
               {
                  epochsOnWaiting.push_back(nextOut);
                  nextOutEpoch = nextOut;
                  #ifdef DEBUG_EPHEMFILE_TIME
                  DebugWriteTime("===== Adding 2 ", nextOut);
                  #endif
               }
            }
            retval = true;
         }
         else
         {
            retval = false;
         }
      }
   }
   
   #ifdef DEBUG_EPHEMFILE_TIME
   Real toMjd;
   std::string epochStr;
   // Convert current epoch to specified format
   TimeConverterUtil::Convert("A1ModJulian", nextOutEpoch/86400.0, "",
                              epochFormat, toMjd, epochStr);
   MessageInterface::ShowMessage
      ("EphemerisFile::IsTimeToWrite() returning %d, nextOutEpoch=%f, epochStr='%s'\n",
       retval, nextOutEpoch, epochStr.c_str());
   #endif
   return retval;
}


//------------------------------------------------------------------------------
// void WriteCcsdsOrbit(Real reqEpochInSecs, Real state[6])
//------------------------------------------------------------------------------
/**
 * Writes spacecraft orbit data to a ephemeris file.
 *
 * @param reqEpochInSecs Requested epoch to write in seconds 
 * @param state State to write 
 */
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsOrbit(Real reqEpochInSecs, Real state[6])
{
   #ifdef DEBUG_EPHEMFILE_WRITE
   MessageInterface::ShowMessage
      ("EphemerisFile::WriteCcsdsOrbit() entered, reqEpochInSecs=%f, state[0]=%f\n",
       reqEpochInSecs, state[0]);
   #endif
   
   #ifdef DEBUG_EPHEMFILE_TEXT
   DebugWriteOrbit(reqEpochInSecs, state);
   #endif
   
   #ifdef DEBUG_EPHEMFILE_WRITE
   MessageInterface::ShowMessage("EphemerisFile::WriteCcsdsOrbit() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// void WriteCcsdsOrbitAt(Real reqEpochInSecs, Real state[6])
//------------------------------------------------------------------------------
/**
 * Writes spacecraft orbit data to a ephemeris file at requested epoch
 *
 * @param reqEpochInSecs Requested epoch to write state in seconds
 * @param state State to write 
 */
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsOrbitAt(Real reqEpochInSecs, Real state[6])
{
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage
      ("EphemerisFile::WriteCcsdsOrbitAt() entered, reqEpochInSecs=%f\n",
       reqEpochInSecs);
   #endif
   
   if (writingNewSegment)
   {
      WriteCcsdsOrbit(reqEpochInSecs, state);
   }
   else
   {
      // Process epochs on waiting
      ProcessEpochsOnWaiting(false);
   }
   
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage("EphemerisFile::WriteCcsdsOrbitAt() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// void GetAttitude()
//------------------------------------------------------------------------------
void EphemerisFile::GetAttitude()
{
   // Get spacecraft attitude in direction cosine matrix
   attEpoch = spacecraft->GetEpoch();
   Rmatrix33 dcm = spacecraft->GetAttitude(attEpoch);
   Rvector quat = Attitude::ToQuaternion(dcm);
   for (int i = 0; i < 4; i++)
      attQuat[i] = quat[i];
}


//------------------------------------------------------------------------------
// void WriteCcsdsAttitude()
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsAttitude()
{
   GetAttitude();
   
   char strBuff[200];
   sprintf(strBuff, "%16.10f  %19.16f  %19.16f  %19.16f  %19.16f\n",
           attEpoch, attQuat[0], attQuat[1], attQuat[2], attQuat[3]);
   dstream << strBuff;
}


//------------------------------------------------------------------------------
// void FinishUpWriting()
//------------------------------------------------------------------------------
/*
 * Finishes up writing data at epochs on waiting
 */
//------------------------------------------------------------------------------
void EphemerisFile::FinishUpWriting()
{
   #ifdef DEBUG_EPHEMFILE_FINISH
   MessageInterface::ShowMessage
      ("EphemerisFile::FinishUpWriting() entered, isFinalized=%d\n", isFinalized);
   DebugWriteTime("   current ", currEpochInSecs);
   MessageInterface::ShowMessage
      ("   There are %d epochs waiting to be output\n", epochsOnWaiting.size());
   for (UnsignedInt i = 0; i < epochsOnWaiting.size(); i++)
      DebugWriteTime("      ", epochsOnWaiting[i]);
   MessageInterface::ShowMessage
      ("   There are %d data in the SPK buffer, spkWriter=<%p>\n",
       spkEpochArray.size(), spkWriter);
   #endif
   
   if (!isFinalized)
   {
      if (interpolator != NULL)
      {
         interpolator->SetForceInterpolation(true);
         ProcessEpochsOnWaiting(true);
         interpolator->SetForceInterpolation(false);
         
         // When running more than 5 days or so, the last epoch to precess is a few
         // milliseconds after the last epoch received, so the interpolator flags
         // as epoch after the last buffered epoch, so handle last data point here.
         // If there is 1 epoch left and the difference between the current epoch
         // is less than 1.e-6 then use the current epoch
         if (epochsOnWaiting.size() == 1)
         {
            Real lastEpoch = epochsOnWaiting.back();
            if (GmatMathUtil::Abs(lastEpoch - currEpochInSecs) < 1.e-6)
            {
               epochsOnWaiting.pop_back();
               epochsOnWaiting.push_back(currEpochInSecs);
               interpolator->SetForceInterpolation(true);
               ProcessEpochsOnWaiting(true);
               interpolator->SetForceInterpolation(false);
            }
         }
      }
      
      #ifdef DEBUG_EPHEMFILE_TEXT
      if (fileType == CCSDS_AEM)
         WriteString("DATA_STOP\n");
      #endif
      
      if (fileType == SPK_ORBIT)
      {
         if (spkWriter != NULL)
         {
            WriteSpkOrbitDataSegment();
         }
         else
         {
            #ifdef __USE_SPICE__
            if (spkEpochArray.size() > 0)
            {
               throw SubscriberException
                  ("*** INTERNANL ERROR *** SPK Writer is NULL in "
                   "EphemerisFile::FinishUpWriting()\n");
            }
            #endif
         }
      }
      
      isFinalized = true;
   }
   
   #ifdef DEBUG_EPHEMFILE_FINISH
   MessageInterface::ShowMessage
      ("EphemerisFile::FinishUpWriting() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// void ProcessEpochsOnWaiting(bool checkFinalEpoch = false)
//------------------------------------------------------------------------------
/*
 * Process epochs on waiting.
 *
 * @param checkFinalEpoch Set to true if checking for final epoch
 *
 */
//------------------------------------------------------------------------------
void EphemerisFile::ProcessEpochsOnWaiting(bool checkFinalEpoch)
{
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage
      ("EphemerisFile::ProcessEpochsOnWaiting() entered, checkFinalEpoch=%d, "
       "currEpochInSecs=%.16f\n   There are %d epochs waiting to be output\n",
       checkFinalEpoch, currEpochInSecs, epochsOnWaiting.size());
   for (UnsignedInt i = 0; i < epochsOnWaiting.size(); i++)
      MessageInterface::ShowMessage("      %.16f\n", epochsOnWaiting[i]);
   #endif
   
   Real estimates[6];
   Real reqEpochInSecs = 0.0;
   
   RealArray::iterator i = epochsOnWaiting.begin();
   while (i < epochsOnWaiting.end())
   {
      reqEpochInSecs = *i;
      
      // Do not write after the final epoch
      if (checkFinalEpoch)
      {
         if (reqEpochInSecs > currEpochInSecs)
         {
            #ifdef DEBUG_EPHEMFILE_ORBIT
            MessageInterface::ShowMessage
               ("   reqEpochInSecs %.16f > currEpochInSecs %.16f so exiting while "
                "loop\n", reqEpochInSecs, currEpochInSecs);
            #endif
            
            break;
         }
      }
      
      #ifdef DEBUG_EPHEMFILE_ORBIT
      DebugWriteTime("   Checking to see if it is feasible ", reqEpochInSecs);
      #endif
      
      Integer retval = interpolator->IsInterpolationFeasible(reqEpochInSecs);
      
      #ifdef DEBUG_EPHEMFILE_ORBIT
      MessageInterface::ShowMessage
         ("   =====> interpolation feasibility at reqEpochInSecs %f is %d\n",
          reqEpochInSecs, retval);
      #endif
      
      if (retval == 1)
      {
         // Now interpolate at epoch
         #ifdef DEBUG_EPHEMFILE_ORBIT
         MessageInterface::ShowMessage
            ("   =====> now try interpolating at epoch %.16f\n", reqEpochInSecs);
         #endif
         if (interpolator->Interpolate(reqEpochInSecs, estimates))
         {
            WriteCcsdsOrbit(reqEpochInSecs, estimates);
            #ifdef DEBUG_EPHEMFILE_ORBIT
            DebugWriteTime("   =====> now erasing ", reqEpochInSecs);
            #endif
            epochsOnWaiting.erase(i); // erase returns the next one
         }
         else
         {
            if (initialCount <= interpolationOrder/2)
            {
               initialCount++;
               
               #ifdef DEBUG_EPHEMFILE_ORBIT
               MessageInterface::ShowMessage
                  ("   =====> Forcing to interpolate at epoch %.16f\n", reqEpochInSecs);
               #endif
               
               // Since time should be in order, force process epochs on waiting.
               // First few request time can not be placed in the middle of the buffer.
               interpolator->SetForceInterpolation(true);
               ProcessEpochsOnWaiting(false);
               interpolator->SetForceInterpolation(false);
            }
            else
            {
               #ifdef DEBUG_EPHEMFILE_ORBIT
               MessageInterface::ShowMessage
                  ("   =====> epoch %.16f failed to interpolate so exiting the loop\n",
                   reqEpochInSecs);
               #endif
               break;
            }
         }
      }
      else
      {
         // If epoch is after the last data, collect number of order points
         // and process before epoch becomes out of the first data range
         if (retval ==  -3)
         {
            processingLargeStep = true;
         }
         
         // @todo Is there more checking here?
         #ifdef DEBUG_EPHEMFILE_ORBIT
         MessageInterface::ShowMessage
            ("   =====> epoch %.16f is not feasible so exiting the loop\n", reqEpochInSecs);
         #endif
         break;
      }
   }
   
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage
      ("EphemerisFile::ProcessEpochsOnWaiting() leaving\n   There are %d epochs "
       "waiting to be output\n", epochsOnWaiting.size());
   for (UnsignedInt i = 0; i < epochsOnWaiting.size(); i++)
      MessageInterface::ShowMessage("      %.16f\n", epochsOnWaiting[i]);
   #endif
}


//------------------------------------------------------------------------------
// bool SetEpoch(const std::string &value)
//------------------------------------------------------------------------------
bool EphemerisFile::SetEpoch(Integer id, const std::string &value,
                             const StringArray &allowedValues)
{
   #ifdef DEBUG_EPHEMFILE_SET
   MessageInterface::ShowMessage
      ("EphemerisFile::SetEpoch() entered, id=%d, value='%s', "
       "epochFormat='%s'\n", id, value.c_str(), epochFormat.c_str());
   #endif
   
   try
   {
      TimeConverterUtil::ValidateTimeFormat(epochFormat, value);
   }
   catch (BaseException &e)
   {
      HandleError(id, value, allowedValues, " or value in " + epochFormat);
   }
   
   if (id == INITIAL_EPOCH)
      initialEpoch = value;
   else if (id == FINAL_EPOCH)
      finalEpoch = value;
   
   return true;
}


//------------------------------------------------------------------------------
// bool SetStepSize(const std::string &value)
//------------------------------------------------------------------------------
/*
 * Sets real value step size.
 *
 * @param value step size value string
 *
 * @exception SubscriberException is thrown if value not converted to real number
 */
//------------------------------------------------------------------------------
bool EphemerisFile::SetStepSize(Integer id, const std::string &value,
                                const StringArray &allowedValues)
{
   #ifdef DEBUG_EPHEMFILE_SET
   MessageInterface::ShowMessage
      ("EphemerisFile::SetStepSize() entered, id=%d, value='%s'\n",
       id, value.c_str());
   #endif
   
   Real rval;
   if (GmatStringUtil::ToReal(value, rval) == false)
   {
      HandleError(id, value, allowedValues, " or Real Number");
   }
   
   stepSize = value;
   stepSizeInSecs = rval;
   stepSizeInA1Mjd = stepSizeInSecs / 86400.0;
   
   useStepSize = true;
   
   #ifdef DEBUG_EPHEMFILE_SET
   MessageInterface::ShowMessage
      ("EphemerisFile::SetStepSize() leaving, stepSize='%s', stepSizeInA1Mjd=%.16f, "
       "stepSizeInSecs=%.16f\n", stepSize.c_str(), stepSizeInA1Mjd, stepSizeInSecs);
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// void HandleError(Integer id, const std::string &value,
//                  const StringArray &allowedValues, const std::string &additionalMsg)
//------------------------------------------------------------------------------
void EphemerisFile::HandleError(Integer id, const std::string &value,
                                const StringArray &allowedValues,
                                const std::string &additionalMsg)
{
   std::string allowedList = ToString(allowedValues);
   SubscriberException se;
   se.SetDetails(errorMessageFormat.c_str(), value.c_str(),
                 GetParameterText(id).c_str(),
                 (allowedList + additionalMsg).c_str());
   throw se;
}


//------------------------------------------------------------------------------
// std::string ToString(const StringArray &strList)
//------------------------------------------------------------------------------
/**
 * Converts std::string array to std::string separated by comma.
 */
//------------------------------------------------------------------------------
std::string EphemerisFile::ToString(const StringArray &strList)
{
   std::string str = "";
   std::string delimiter = ", ";
   if (strList.size() > 0)
   {
      str = strList[0];
      
      for (unsigned int i=1; i<strList.size(); i++)
         str = str + delimiter + strList[i];
   }
   
   return str;
}


//------------------------------------------------------------------------------
// void WriteString(const std::string &str)
//------------------------------------------------------------------------------
void EphemerisFile::WriteString(const std::string &str)
{
   // For now write it text file
   dstream << str;
}


//------------------------------------------------------------------------------
// void WriteHeader()
//------------------------------------------------------------------------------
void EphemerisFile::WriteHeader()
{
   if (fileType == CCSDS_OEM || fileType == CCSDS_AEM)
      WriteCcsdsHeader();
   else if (fileType == SPK_ORBIT)
      WriteSpkHeader();
}


//------------------------------------------------------------------------------
// void WriteMetadata()
//------------------------------------------------------------------------------
void EphemerisFile::WriteMetadata()
{
   if (fileType == CCSDS_OEM)
      WriteCcsdsOemMetadata();
   else if (fileType == CCSDS_AEM)
      WriteCcsdsAemMetadata();
   else if (fileType == SPK_ORBIT)
      WriteSpkOrbitMetaData();
}


//------------------------------------------------------------------------------
// void WriteComments(const std::string &comments)
//------------------------------------------------------------------------------
/**
 * Writes comments to specific file.
 */
//------------------------------------------------------------------------------
void EphemerisFile::WriteComments(const std::string &comments)
{
   if (fileType == CCSDS_OEM || fileType == CCSDS_AEM)
      WriteCcsdsComments(comments);
   else if (fileType == SPK_ORBIT)
      WriteSpkComments(comments);
}


//------------------------------------------------------------------------------
// void WriteCcsdsHeader()
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsHeader()
{
   #ifdef DEBUG_EPHEMFILE_TEXT
   std::string creationTime = GmatTimeUtil::FormatCurrentTime(2);
   std::stringstream ss("");
   
   if (fileType == CCSDS_OEM)
      ss << "CCSDS_OEM_VERS = 1.0" << std::endl;
   else
      ss << "CCSDS_AEM_VERS = 1.0" << std::endl;
   
   ss << "CREATION_DATE  = " << creationTime << std::endl;
   ss << "ORIGINATOR     = GMAT USER" << std::endl;
   
   WriteString(ss.str());
   #endif
}


//------------------------------------------------------------------------------
// void WriteCcsdsOemMetadata()
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsOemMetadata()
{
   #ifdef DEBUG_EPHEMFILE_TEXT
   std::string objId  = spacecraft->GetStringParameter("Id");
   std::string origin = spacecraft->GetOriginName();
   std::string csType = "UNKNOWN";
   GmatBase *cs = (GmatBase*)(spacecraft->GetRefObject(Gmat::COORDINATE_SYSTEM, ""));
   if (cs)
      csType = (cs->GetRefObject(Gmat::AXIS_SYSTEM, ""))->GetTypeName();
   
   std::stringstream ss("");
   ss << std::endl;
   ss << "META_START" << std::endl;
   ss << "OBJECT_NAME           = " << spacecraftName << std::endl;
   ss << "OBJECT_ID             = " << objId << std::endl;
   ss << "CENTER_NAME           = " << origin << std::endl;
   ss << "REF_FRAME             = " << csType << std::endl;
   ss << "TIME_SYSTEM           = " << epochFormat << std::endl;
   ss << "START_TIME            = " << "@TODO_START" << std::endl;
   ss << "USEABLE_START_TIME    = " << "@TODO_USTART" << std::endl;
   ss << "USEABLE_STOP_TIME     = " << "@TODO_USTOP" << std::endl;
   ss << "STOP_TIME             = " << "@TODO_STOP" << std::endl;
   ss << "INTERPOLATION         = " << interpolatorName << std::endl;
   ss << "INTERPOLATION_DEGREE  = " << interpolationOrder << std::endl;
   ss << "META_STOP" << std::endl << std::endl;
   
   WriteString(ss.str());
   #endif
}


//------------------------------------------------------------------------------
// void WriteCcsdsAemMetadata()
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsAemMetadata()
{
   #ifdef DEBUG_EPHEMFILE_TEXT
   std::string objId  = spacecraft->GetStringParameter("Id");
   std::string origin = spacecraft->GetOriginName();
   std::string csType = "UNKNOWN";
   GmatBase *cs = (GmatBase*)(spacecraft->GetRefObject(Gmat::COORDINATE_SYSTEM, ""));
   if (cs)
      csType = cs->GetTypeName();
   
   std::stringstream ss("");
   ss << "META_START" << std::endl;
   ss << "OBJECT_NAME           = " << spacecraftName << std::endl;
   ss << "OBJECT_ID             = " << objId << std::endl;
   ss << "CENTER_NAME           = " << origin << std::endl;
   ss << "REF_FRAME_A           = " << csType << std::endl;
   ss << "REF_FRAME_B           = " << "@TODO_REFB" << std::endl;
   ss << "TIME_SYSTEM           = " << epochFormat << std::endl;
   ss << "START_TIME            = " << "@TODO_START" << std::endl;
   ss << "USEABLE_START_TIME    = " << "@TODO_USTART" << std::endl;
   ss << "USEABLE_STOP_TIME     = " << "@TODO_USTOP" << std::endl;
   ss << "STOP_TIME             = " << "@TODO_STOP" << std::endl;
   ss << "ATTITUDE_TYPE         = " << "@TODO_STOP" << std::endl;
   ss << "QUATERNION_TYPE       = " << "@TODO_STOP" << std::endl;
   ss << "INTERPOLATION_METHOD  = " << interpolatorName << std::endl;
   ss << "INTERPOLATION_DEGREE  = " << interpolationOrder << std::endl;
   ss << "META_STOP" << std::endl << std::endl;
   
   WriteString(ss.str());
   #endif
}


//------------------------------------------------------------------------------
// void WriteCcsdsOem(const std::string &epoch, Real state[6])
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsOem(const std::string &epoch, Real state[6])
{
}


//------------------------------------------------------------------------------
// void WriteCcsdsAem(const std::string &epoch, Real quat[4])
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsAem(const std::string &epoch, Real quat[4])
{
}


//------------------------------------------------------------------------------
// void WriteCcsdsComments(const std::string &comments)
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsComments(const std::string &comments)
{
   #ifdef DEBUG_EPHEMFILE_TEXT
   WriteString("\nCOMMENT  " + comments + "\n");
   #endif
}


//------------------------------------------------------------------------------
// void WriteSpkHeader()
//------------------------------------------------------------------------------
void EphemerisFile::WriteSpkHeader()
{
   #ifdef DEBUG_EPHEMFILE_TEXT
   std::string creationTime = GmatTimeUtil::FormatCurrentTime(2);
   std::stringstream ss("");
   
   ss << "SPK ORBIT DATA" << std::endl;
   ss << "CREATION_DATE  = " << creationTime << std::endl;
   ss << "ORIGINATOR     = GMAT USER" << std::endl;
   
   WriteString(ss.str());
   #endif
}


//------------------------------------------------------------------------------
// void BufferSpkOrbitData(Real epoch, Real state[6])
//------------------------------------------------------------------------------
void EphemerisFile::BufferSpkOrbitData(Real epoch, Real state[6])
{
   #ifdef DEBUG_EPHEMFILE_SPICE_BUFFER
   MessageInterface::ShowMessage
      ("==> BufferSpkOrbitData() entered, epoch=%f, state[0]=%f\n", epoch,
       state[0]);
   #endif
   
   //=======================================================
   #ifdef __USE_SPICE__
   //=======================================================
   if (spkEpochArray.size() > MAX_SEGMENT_SIZE)
   {
      WriteSpkOrbitDataSegment();
   }
   
   Rvector6 *rv6 = new Rvector6(state);
   A1Mjd *a1mjd = new A1Mjd(epoch);
   spkEpochArray.push_back(a1mjd);
   spkStateArray.push_back(rv6);
   //=======================================================
   #endif
   //=======================================================
   
   #ifdef DEBUG_EPHEMFILE_SPICE_BUFFER
   MessageInterface::ShowMessage
      ("==> BufferSpkOrbitData() leaving, there are %d data\n", spkEpochArray.size());
   #endif
}


//------------------------------------------------------------------------------
// void DeleteSpkOrbitData()
//------------------------------------------------------------------------------
void EphemerisFile::DeleteSpkOrbitData()
{
   EpochArray::iterator ei;
   for (ei = spkEpochArray.begin(); ei != spkEpochArray.end(); ++ei)
      delete (*ei);
   
   StateArray::iterator si;
   for (si = spkStateArray.begin(); si != spkStateArray.end(); ++si)
      delete (*si);
   
   spkEpochArray.clear();
   spkStateArray.clear();
}


//------------------------------------------------------------------------------
// void WriteSpkOrbitDataSegment()
//------------------------------------------------------------------------------
void EphemerisFile::WriteSpkOrbitDataSegment()
{
   #ifdef DEBUG_EPHEMFILE_SPICE
   MessageInterface::ShowMessage
      ("=====> WriteSpkOrbitDataSegment() entered, spkEpochArray.size()=%d, "
       "spkStateArray.size()=%d\n", spkEpochArray.size(), spkStateArray.size());
   #endif
   
   #ifdef __USE_SPICE__
   if (spkEpochArray.size() > 0)
   {
      A1Mjd *start = spkEpochArray.front();
      A1Mjd *end   = spkEpochArray.back();
      
      #ifdef DEBUG_EPHEMFILE_SPICE
      MessageInterface::ShowMessage
         ("   Writing start=%f, end=%f\n", start->GetReal(), end->GetReal());
      #endif

      spkWriteFailed = false;
      try
      {
         spkWriter->WriteSegment(*start, *end, spkStateArray, spkEpochArray);
         DeleteSpkOrbitData();
      }
      catch (BaseException &e)
      {
         DeleteSpkOrbitData();
         spkWriteFailed = true;
         #ifdef DEBUG_EPHEMFILE_SPICE
         MessageInterface::ShowMessage(e.GetFullMessage());
         #endif
         throw;
      }
   }
   #endif
   
   #ifdef DEBUG_EPHEMFILE_SPICE
   MessageInterface::ShowMessage
      ("=====> WriteSpkOrbitDataSegment() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// void WriteSpkOrbitMetaData()
//------------------------------------------------------------------------------
void EphemerisFile::WriteSpkOrbitMetaData()
{
   std::string objId  = spacecraft->GetStringParameter("Id");
   std::string origin = spacecraft->GetOriginName();
   std::string csType = "UNKNOWN";
   GmatBase *cs = (GmatBase*)(spacecraft->GetRefObject(Gmat::COORDINATE_SYSTEM, ""));
   if (cs)
      csType = (cs->GetRefObject(Gmat::AXIS_SYSTEM, ""))->GetTypeName();
   
   std::stringstream ss("");
   ss << std::endl;
   ss << "META_START" << std::endl;
   ss << "OBJECT_NAME           = " << spacecraftName << std::endl;
   ss << "OBJECT_ID             = " << objId << std::endl;
   ss << "CENTER_NAME           = " << origin << std::endl;
   ss << "REF_FRAME             = " << csType << std::endl;
   ss << "TIME_SYSTEM           = " << epochFormat << std::endl;
   ss << "START_TIME            = " << "@TODO_START" << std::endl;
   ss << "USEABLE_START_TIME    = " << "@TODO_USTART" << std::endl;
   ss << "USEABLE_STOP_TIME     = " << "@TODO_USTOP" << std::endl;
   ss << "STOP_TIME             = " << "@TODO_STOP" << std::endl;
   ss << "INTERPOLATION         = " << interpolatorName << std::endl;
   ss << "INTERPOLATION_DEGREE  = " << interpolationOrder << std::endl;
   ss << "META_STOP" << std::endl << std::endl;
   
   #ifdef DEBUG_EPHEMFILE_TEXT
   WriteString(ss.str());
   #endif
   
   WriteSpkComments(ss.str());
}


//------------------------------------------------------------------------------
// void WriteSpkComments(const std::string &comments)
//------------------------------------------------------------------------------
void EphemerisFile::WriteSpkComments(const std::string &comments)
{
   #ifdef DEBUG_EPHEMFILE_TEXT
   WriteString("\nCOMMENT  " + comments + "\n");
   #endif
   
   #ifdef __USE_SPICE__
   spkWriter->AddMetaData(comments);
   #endif
}


//------------------------------------------------------------------------------
// void FinalizeSpkFile()
//------------------------------------------------------------------------------
void EphemerisFile::FinalizeSpkFile()
{
   #ifdef __USE_SPICE__
   spkWriter->FinalizeKernel();
   #endif
}


//------------------------------------------------------------------------------
// void DebugWriteTime(const std::string &msg, Real epochInSecs)
//------------------------------------------------------------------------------
void EphemerisFile::DebugWriteTime(const std::string &msg, Real epochInSecs)
{
   Real toMjd;
   std::string epochStr;
   Real epochInDays = epochInSecs / 86400.0;
   
   // Convert current epoch to specified format
   TimeConverterUtil::Convert("A1ModJulian", epochInDays, "", epochFormat, toMjd, epochStr);
   
   MessageInterface::ShowMessage
      ("%sepoch = %.16f, %.16f, '%s'\n", msg.c_str(), epochInSecs, epochInDays,
       epochStr.c_str());
}


//------------------------------------------------------------------------------
// void DebugWriteOrbit(Real reqEpochInSecs, Real state[6], bool logOnly = false)
//------------------------------------------------------------------------------
void EphemerisFile::DebugWriteOrbit(Real reqEpochInSecs, Real state[6], bool logOnly)
{
   Real toMjd;
   std::string epochStr;
   Real reqEpochInDays = reqEpochInSecs / 86400.0;
   Rvector6 inState(state);
   Rvector6 outState(state);
   
   // Convert current epoch to specified format
   TimeConverterUtil::Convert("A1ModJulian", reqEpochInDays, "", epochFormat,
                              toMjd, epochStr);
   
   // Convert orbit data to output coordinate system
   if (writeDataInDataCS)
   {
      outState = inState;
   }
   else
   {
      coordConverter.Convert(A1Mjd(reqEpochInDays), inState, theDataCoordSystem,
                             outState, coordSystem, true);
   }
   
   if (logOnly)
   {
      MessageInterface::ShowMessage
         ("%s  %24.10f  %24.10f  %24.10f  %20.16f  %20.16f  %20.16f\n",
          epochStr.c_str(), outState[0], outState[1], outState[2], outState[3],
          outState[4], outState[5]);
   }
   else
   {
      #ifdef DEBUG_EPHEMFILE_TEXT
      char strBuff[200];
      sprintf(strBuff, "%s  %24.10f  %24.10f  %24.10f  %20.16f  %20.16f  %20.16f\n",
              epochStr.c_str(), outState[0], outState[1], outState[2], outState[3],
              outState[4], outState[5]);
      dstream << strBuff;
      #endif
   }
}


//--------------------------------------
// methods inherited from Subscriber
//--------------------------------------

//------------------------------------------------------------------------------
// bool Distribute(int len)
//------------------------------------------------------------------------------
bool EphemerisFile::Distribute(int len)
{
   return true;
}


//------------------------------------------------------------------------------
// bool Distribute(const Real * dat, Integer len)
//------------------------------------------------------------------------------
/*
 * Handles distributed data from Subscriber::ReceiveData() through
 * Publisher::Publish(). Asssumes first data dat[0] is data epoch in A1Mjd.
 *
 * @param dat Data received 
 */
//------------------------------------------------------------------------------
bool EphemerisFile::Distribute(const Real * dat, Integer len)
{
   #if DBGLVL_EPHEMFILE_DATA > 0
   MessageInterface::ShowMessage
      ("EphemerisFile::Distribute() this=<%p>'%s' called\n", this, GetName().c_str());
   MessageInterface::ShowMessage
      ("   len=%d, active=%d, isEndOfReceive=%d, runstate=%d, isManeuvering=%d, "
       "firstTimeWriting=%d\n", len, active, isEndOfReceive, runstate, isManeuvering,
       firstTimeWriting);
   #endif
   #if DBGLVL_EPHEMFILE_DATA > 1
   MessageInterface::ShowMessage("   fileName='%s'\n", fileName.c_str());
   #endif
   
   if (!active)
      return true;
   
   if (isEndOfReceive || len == 0)
   {
      #ifdef DEBUG_EPHEMFILE_FINISH
      MessageInterface::ShowMessage
         ("EphemerisFile::Distribute() Calling FinishUpWriting(), "
          "isEndOfReceive=%d, len=%d\n", isEndOfReceive, len);
      #endif
      
      FinishUpWriting();
      return true;
   }
   
   isFinalized = false;
   
   //------------------------------------------------------------
   // if solver is running, just return
   //------------------------------------------------------------
   if (runstate == Gmat::SOLVING)
   {
      #ifdef DEBUG_EPHEMFILE_SOLVER_DATA
      MessageInterface::ShowMessage
         ("EphemerisFile::Distribute() Just returning; solver is running\n");
      #endif
      
      return true;
   }
   
   currEpochInDays = dat[0];
   for (int i=0; i<6; i++)
      currState[i] = dat[i+1];
   
   // Internally all epochs are in seconds to avoid epoch drifting.
   // For long run epochs to process drifts behind the actual.
   currEpochInSecs = currEpochInDays * 86400.0;
   
   //------------------------------------------------------------
   // if solver is not running or solver has finished, write data
   //------------------------------------------------------------
   if (runstate == Gmat::RUNNING || runstate == Gmat::SOLVEDPASS)
   {
      #ifdef DEBUG_EPHEMFILE_SOLVER_DATA
      MessageInterface::ShowMessage
         ("EphemerisFile::Distribute() Writing out state with solver's final "
          "solution, runstate=%d, maneuverEpochInDays=%f\n", runstate,
          maneuverEpochInDays);
      DebugWriteOrbit(currEpochInSecs, currState, true);
      #endif
      
      // Check for epoch before maneuver epoch
      // Propagate publishes data with epoch before maneuver epoch
      if (runstate == Gmat::SOLVEDPASS && currEpochInDays < maneuverEpochInDays)
      {
         #ifdef DEBUG_EPHEMFILE_SOLVER_DATA
         MessageInterface::ShowMessage
            ("EphemerisFile::Distribute() Just returning; epoch (%f) < maneuver "
             "epoch (%f)solver is running\n", currEpochInDays, maneuverEpochInDays);
         #endif
         return true;
      }
      
      bool writeData = CheckInitialAndFinalEpoch();
      
      #if DBGLVL_EPHEMFILE_DATA > 0
      MessageInterface::ShowMessage
         ("   Start writing data, currEpochInDays=%.16f, currEpochInSecs=%.16f, "
          "writeData=%d, writeOrbit=%d, writeAttitude=%d\n", currEpochInDays,
          currEpochInSecs, writeData, writeOrbit, writeAttitude);
      #endif
      
      // For now we only writes Orbit data
      if (fileType == SPK_ORBIT)
         HandleSpkOrbitData(writeData);
      else
         HandleCcsdsOrbitData(writeData);
   }
   
   return true;
}


//------------------------------------------------------------------------------
// virtual void HandleManeuvering(GmatBase *originator, bool flag, Real epoch,
//                                const StringArray &satNames,
//                                const std::string &desc)
//------------------------------------------------------------------------------
/*
 * @see Subscriber
 */
//------------------------------------------------------------------------------
void EphemerisFile::HandleManeuvering(GmatBase *originator, bool flag, Real epoch,
                                      const StringArray &satNames,
                                      const std::string &desc)
{
   #if DBGLVL_EPHEMFILE_MANEUVER > 1
   MessageInterface::ShowMessage
      ("EphemerisFile::HandleManeuvering() entered, originator=<%p>, prevRunState=%d, "
       "runstate=%d, maneuversHandled.size()=%d\n",originator, prevRunState, runstate,
       maneuversHandled.size());
   #endif
   
   // SOLVEDPASS is passed multiple times, one from the solver loop another from
   // solver loop with converged data, so ignore second time on
   
   if (prevRunState == runstate && runstate == Gmat::SOLVEDPASS)
   {
      // Check if the originator already handled
      if (find(maneuversHandled.begin(), maneuversHandled.end(), originator) !=
          maneuversHandled.end())
      {
         #if DBGLVL_EPHEMFILE_MANEUVER
         MessageInterface::ShowMessage
            ("EphemerisFile::HandleManeuvering() leaving, prevRunState is "
             "SOLVEDPASS for the same originator\n");
         #endif
         return;
      }
   }
   
   if (runstate == Gmat::RUNNING || runstate == Gmat::SOLVEDPASS)
   {
      #if DBGLVL_EPHEMFILE_MANEUVER
      MessageInterface::ShowMessage
         ("EphemerisFile::HandleManeuvering() GMAT is not solving or solver has "
          "finished; prevRunState=%d, runstate=%d\n", prevRunState, runstate);
      #endif
      
      if (prevRunState != Gmat::IDLE)
      {
         // Check spacecraft name first   
         if (find(satNames.begin(), satNames.end(), spacecraftName) == satNames.end())
         {
            #if DBGLVL_EPHEMFILE_MANEUVER > 1
            MessageInterface::ShowMessage
               ("EphemerisFile::HandleManeuvering() leaving, the spacecraft '%s' is not maneuvering\n",
                spacecraftName.c_str());
            #endif
            return;
         }
         
         Real toMjd;
         std::string epochStr;
         
         // Added to maneuvers handled
         maneuversHandled.push_back(originator);
         maneuverEpochInDays = epoch;
         
         // Convert current epoch to specified format
         TimeConverterUtil::Convert("A1ModJulian", epoch, "", epochFormat, toMjd, epochStr);
         
         #if DBGLVL_EPHEMFILE_MANEUVER
         MessageInterface::ShowMessage
            ("=====> Restarting the interpolation at %s\n", epochStr.c_str());
         #endif
         
         // Restart interpolation
         RestartInterpolation("This block begins after " + desc + " at " + epochStr + "\n");
      }
   }
   else
   {
      #if DBGLVL_EPHEMFILE_MANEUVER > 1
      MessageInterface::ShowMessage
         ("EphemerisFile::HandleManeuvering() GMAT is solving\n");
      #endif
   }
   
   prevRunState = runstate;
   
   #if DBGLVL_EPHEMFILE_MANEUVER > 1
   MessageInterface::ShowMessage("EphemerisFile::HandleManeuvering() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// virtual void HandlePropagatorChange(GmatBase *provider)
//------------------------------------------------------------------------------
void EphemerisFile::HandlePropagatorChange(GmatBase *provider)
{
   #if DBGLVL_EPHEMFILE_PROP_CHANGE > 1
   MessageInterface::ShowMessage
      ("EphemerisFile::HandlePropagatorChange() entered, provider=<%p><%s>\n",
       provider, provider->GetTypeName().c_str());
   #endif
   
   if (runstate == Gmat::RUNNING || runstate == Gmat::SOLVEDPASS)
   {
      #if DBGLVL_EPHEMFILE_PROP_CHANGE > 1
      MessageInterface::ShowMessage
         ("EphemerisFile::HandlePropagatorChange() GMAT is not solving or solver has finished\n");
      #endif
      
      // Check if propagator name changed on ephemeris file spacecraft
      if (provider->GetTypeName() == "Propagate")
      {
         // Go through propagator list and check if spacecraft found
         StringArray propNames = provider->GetRefObjectNameArray(Gmat::PROP_SETUP);
         Integer scId = provider->GetParameterID("Spacecraft");
         for (UnsignedInt prop = 0; prop < propNames.size(); prop++)
         {
            StringArray satNames = provider->GetStringArrayParameter(scId, prop);
            for (UnsignedInt sat = 0; sat < satNames.size(); sat++)
            {
               if (spacecraftName == satNames[sat])
               {
                  if (currPropName != propNames[prop])
                  {
                     currPropName = propNames[prop];
                     
                     #if DBGLVL_EPHEMFILE_PROP_CHANGE
                     MessageInterface::ShowMessage
                        ("The propagator changed from '%s' to '%s'\n", prevPropName.c_str(),
                         currPropName.c_str());
                     #endif
                     
                     if (prevPropName != "")
                     {
                        #if DBGLVL_EPHEMFILE_PROP_CHANGE
                        MessageInterface::ShowMessage
                           ("=====> Restarting the interpolation\n");
                        #endif
                        
                        // Restart interpolation
                        RestartInterpolation("This block begins after propagator change from " +
                                             prevPropName + " to " + currPropName + "\n");
                     }
                     
                     prevPropName = currPropName;
                     
                  }
                  else
                  {
                     #if DBGLVL_EPHEMFILE_PROP_CHANGE > 1
                     MessageInterface::ShowMessage
                        ("The propagator is the same as '%s'\n", currPropName.c_str());
                     #endif
                  }
               }
            }
         }
      }
   }
   else
   {
      #if DBGLVL_EPHEMFILE_PROP_CHANGE > 1
      MessageInterface::ShowMessage
         ("EphemerisFile::HandlePropagatorChange() GMAT is solving\n");
      #endif
   }
   
   #if DBGLVL_EPHEMFILE_PROP_CHANGE > 1
   MessageInterface::ShowMessage
      ("EphemerisFile::HandlePropagatorChange() leaving, provider=<%p><%s>\n",
       provider, provider->GetTypeName().c_str());
   #endif
}
