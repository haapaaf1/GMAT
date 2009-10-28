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
#include "TimeSystemConverter.hpp"   // for ValidateTimeFormat()
#include "LagrangeInterpolator.hpp" 
#include "MessageInterface.hpp"

//#define DEBUG_EPHEMFILE_SET
//#define DEBUG_EPHEMFILE_INIT
//#define DEBUG_EPHEMFILE_OPEN
//#define DEBUG_EPHEMFILE_TIME
//#define DEBUG_EPHEMFILE_ORBIT
//#define DEBUG_EPHEMFILE_WRITE
//#define DBGLVL_EPHEMFILE_DATA 1

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
   Gmat::ENUMERATION_TYPE,  // WRITE_EPHEMERIS
};


//------------------------------------------------------------------------------
// EphemerisFile(const std::string &name)
//------------------------------------------------------------------------------
EphemerisFile::EphemerisFile(const std::string &name) :
   Subscriber          ("EphemerisFile", name),
   spacecraft          (NULL),
   coordSystem         (NULL),
   interpolator        (NULL),
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
   writeEphemeris      ("Yes"),
   interpolationOrder  (7),
   waitCount           (0),
   stepSizeInA1Mjd     (-999.999),
   initialEpochA1Mjd   (-999.999),
   finalEpochA1Mjd     (-999.999),
   nextOutEpoch        (-999.999),
   nextReqEpoch        (-999.999),
   currentEpoch        (-999.999),
   firstTimeWriting    (true),
   useStepSize         (false),
   writeOrbit          (false),
   writeAttitude       (false),
   writeDataInDataCS   (true),
   processingLargeStep (false)
{
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
}


//------------------------------------------------------------------------------
// ~EphemerisFile()
//------------------------------------------------------------------------------
EphemerisFile::~EphemerisFile()
{
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
}


//------------------------------------------------------------------------------
// EphemerisFile(const EphemerisFile &ef)
//------------------------------------------------------------------------------
EphemerisFile::EphemerisFile(const EphemerisFile &ef) :
   Subscriber          (ef),
   spacecraft          (ef.spacecraft),
   coordSystem         (ef.coordSystem),
   interpolator        (NULL),
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
   interpolationOrder  (ef.interpolationOrder),
   waitCount           (ef.waitCount),
   stepSizeInA1Mjd     (ef.stepSizeInA1Mjd),
   initialEpochA1Mjd   (ef.initialEpochA1Mjd),
   finalEpochA1Mjd     (ef.finalEpochA1Mjd),
   nextOutEpoch        (ef.nextOutEpoch),
   nextReqEpoch        (ef.nextReqEpoch),
   currentEpoch        (ef.currentEpoch),
   firstTimeWriting    (ef.firstTimeWriting),
   useStepSize         (ef.useStepSize),
   writeOrbit          (ef.writeOrbit),
   writeAttitude       (ef.writeAttitude),
   writeDataInDataCS   (ef.writeDataInDataCS),
   processingLargeStep (ef.processingLargeStep)
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
   interpolationOrder  = ef.interpolationOrder;
   waitCount           = ef.waitCount;
   stepSizeInA1Mjd     = ef.stepSizeInA1Mjd;
   initialEpochA1Mjd   = ef.initialEpochA1Mjd;
   finalEpochA1Mjd     = ef.finalEpochA1Mjd;
   nextOutEpoch        = ef.nextOutEpoch;
   nextReqEpoch        = ef.nextReqEpoch;
   currentEpoch        = ef.currentEpoch;
   firstTimeWriting    = ef.firstTimeWriting;
   useStepSize         = ef.useStepSize;
   writeOrbit          = ef.writeOrbit;
   writeAttitude       = ef.writeAttitude;
   writeDataInDataCS   = ef.writeDataInDataCS;
   processingLargeStep = ef.processingLargeStep;
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
         if (fileFormat == "SPK")
            fname = oututPath + instanceName + ".spk";
         else
            fname = oututPath + instanceName + "." + fileFormat + ".ephem";
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
         fname = instanceName + ".txt";
      
      MessageInterface::ShowMessage(e.GetFullMessage());
   }
   
   #ifdef DEBUG_EPHEMFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::GetFileName() returning fname=%s\n", fname.c_str());
   #endif
   
   return fname;
}


//------------------------------------------------------------------------------
// void ValidateParameters()
//------------------------------------------------------------------------------
void EphemerisFile::ValidateParameters()
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
      fileType = TEXT_FILE;
   
   firstTimeWriting = true;
   epochsOnWaiting.clear();
   
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
   if (theDataCoordSystem->GetName() != coordSystemName)
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
   case WRITE_EPHEMERIS:
      return writeEphemerisList;
   default:
      return Subscriber::GetPropertyEnumStrings(id);
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
   case WRITE_EPHEMERIS:
      return writeEphemeris;
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
   case WRITE_EPHEMERIS:
      if (find(writeEphemerisList.begin(), writeEphemerisList.end(), value) !=
          writeEphemerisList.end())
      {
         writeEphemeris = value;
         return true;
      }
      else
      {
         HandleError(WRITE_EPHEMERIS, value, writeEphemerisList);
      }
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
// bool OpenEphemerisFile()
//------------------------------------------------------------------------------
bool EphemerisFile::OpenEphemerisFile()
{
   #ifdef DEBUG_EPHEMFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::OpenEphemerisFile() entered, fileName = %s\n", fileName.c_str());
   #endif
   
   fileName = GetFileName();
   bool retval = false;
   
   // Close the stream if it is open
   if (dstream.is_open())
      dstream.close();
   
   dstream.open(fileName.c_str());
   if (dstream.is_open())
      retval = true;
   else
      MessageInterface::ShowMessage("   %s is not opened\n", fileName.c_str());
   
   #ifdef DEBUG_EPHEMFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::OpenEphemerisFile() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// bool IsTimeToWrite(Real epoch, Real *state)
//------------------------------------------------------------------------------
/*
 * Determines if it is time to write to ephemeris file based on the step size.
 */
//------------------------------------------------------------------------------
bool EphemerisFile::IsTimeToWrite(Real epoch, Real *state)
{
   #ifdef DEBUG_EPHEMFILE_TIME
   MessageInterface::ShowMessage
      ("EphemerisFile::IsTimeToWrite() entered, epoch=%f\n", epoch);
   DebugWriteTime("current", epoch);
   #endif
   bool retval = true;
   
   // If writing at specified interval step, do checking
   if (useStepSize)
   {
      // Add data points
      if (writeOrbit)
      {
         interpolator->AddPoint(epoch, state);
      }
      else if (writeAttitude)
      {
         #ifdef DEBUG_EPHEMFILE_TIME
         MessageInterface::ShowMessage("Adding points to interpolator is todo work\n");
         #endif
      }
      
      // If step size is to large, we may miss the data points since interpolator
      // buffer size is limited. So do additional process here.
      if (processingLargeStep)
      {
         waitCount++;
         
         if (waitCount > interpolationOrder / 2)
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
      if (firstTimeWriting)
      {
         nextOutEpoch = epoch;
         nextReqEpoch = epoch;
         retval = true;
      }
      else
      {         
         if (epoch >= nextOutEpoch)
         {
            nextOutEpoch = nextOutEpoch + stepSizeInA1Mjd;
            
            if (find(epochsOnWaiting.begin(), epochsOnWaiting.end(), nextOutEpoch) ==
                epochsOnWaiting.end())
            {
               epochsOnWaiting.push_back(nextOutEpoch);
               nextReqEpoch = nextOutEpoch;
               #ifdef DEBUG_EPHEMFILE_TIME
               DebugWriteTime("1", nextOutEpoch);
               #endif
            }
            
            // Handle step size less than integrator step size
            Real nextOut = nextOutEpoch;
            while (nextOut <= epoch)
            {
               // Compute new output time
               nextOut = nextOut + stepSizeInA1Mjd;
               if (find(epochsOnWaiting.begin(), epochsOnWaiting.end(), nextOut) ==
                   epochsOnWaiting.end())
               {
                  epochsOnWaiting.push_back(nextOut);
                  nextOutEpoch = nextOut;
                  #ifdef DEBUG_EPHEMFILE_TIME
                  DebugWriteTime("2", nextOut);
                  #endif
               }
            }
            retval = true;
         }
         else
            retval = false;
      }
   }
   
   #ifdef DEBUG_EPHEMFILE_TIME
   Real toMjd;
   std::string epochStr;
   // Convert current epoch to specified format
   TimeConverterUtil::Convert("A1ModJulian", nextOutEpoch, "", epochFormat, toMjd, epochStr);
   MessageInterface::ShowMessage
      ("EphemerisFile::IsTimeToWrite() returning %d, nextOutEpoch=%f, epochStr='%s'\n",
       retval, nextOutEpoch, epochStr.c_str());
   #endif
   return retval;
}


//------------------------------------------------------------------------------
// void WriteOrbit(Real reqEpoch, Real *state)
//------------------------------------------------------------------------------
/**
 * Writes spacecraft orbit data to a ephemeris file.
 *
 * @param reqEpoch Requested epoch to write
 * @param state State to write 
 */
//------------------------------------------------------------------------------
void EphemerisFile::WriteOrbit(Real reqEpoch, Real *state)
{
   #ifdef DEBUG_EPHEMFILE_WRITE
   MessageInterface::ShowMessage
      ("EphemerisFile::WriteOrbit() entered, reqEpoch=%f, state[0]=%f\n",
       reqEpoch, state[0]);
   #endif
   
   Rvector6 inState;
   inState.Set(state);
   Rvector6 outState;
   Real toMjd;
   std::string epochStr;
   
   // Convert current epoch to specified format
   TimeConverterUtil::Convert("A1ModJulian", reqEpoch, "", epochFormat, toMjd, epochStr);
   
   // Convert orbit data to output coordinate system
   if (writeDataInDataCS)
   {
      outState = inState;
   }
   else
   {
      coordConverter.Convert(A1Mjd(reqEpoch), inState, theDataCoordSystem,
                             outState, coordSystem, true);
   }
   
   char strBuff[200];
   sprintf(strBuff, "%s  %24.10f  %24.10f  %24.10f  %20.16f  %20.16f  %20.16f\n",
           epochStr.c_str(), outState[0], outState[1], outState[2], outState[3],
           outState[4], outState[5]);
   dstream << strBuff;
   
   #ifdef DEBUG_EPHEMFILE_WRITE
   MessageInterface::ShowMessage("EphemerisFile::WriteOrbit() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// void WriteOrbitAt(Real reqEpoch, Real *state)
//------------------------------------------------------------------------------
/**
 * Writes spacecraft orbit data to a ephemeris file at requested epoch
 *
 * @param reqEpoch Requested epoch to write state
 * @param state State to write 
 */
//------------------------------------------------------------------------------
void EphemerisFile::WriteOrbitAt(Real reqEpoch, Real *state)
{
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage
      ("EphemerisFile::WriteOrbitAt() entered, reqEpoch=%f\n", reqEpoch);
   #endif
   
   if (firstTimeWriting)
   {
      WriteOrbit(reqEpoch, state);
   }
   else
   {
      // Process epochs on waiting
      ProcessEpochsOnWaiting(false);
      
      // Check for interpolation feasibility
      Integer retval = interpolator->IsInterpolationFeasible(reqEpoch);
      #ifdef DEBUG_EPHEMFILE_ORBIT
      MessageInterface::ShowMessage
         ("   =====> interpolation feasibity at reqEpoch %f is %d\n", reqEpoch, retval);
      #endif
      
      // Interpolate if feasible
      if (retval == 1)
      {
         // Since time should be in order, force process epochs on waiting.
         // First few request time can not be placed in the middle of the buffer.
         interpolator->SetForceInterpolation(true);
         ProcessEpochsOnWaiting(false);
         interpolator->SetForceInterpolation(false);         
      }
      else
      {
         // If epoch is after the last data, collect number of order points
         // and process before epoch becomes out of the first data range
         if (retval ==  -3)
         {
            processingLargeStep = true;
         }
      }
   }
   
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage("EphemerisFile::WriteOrbitAt() leaving\n");
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
// void WriteAttitude()
//------------------------------------------------------------------------------
void EphemerisFile::WriteAttitude()
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
      ("EphemerisFile::FinishUpWriting() entered, currentEpoch=%18.12f\n", currentEpoch);
   #endif
   
   if (!isFinalized)
   {
      if (interpolator != NULL)
      {
         interpolator->SetForceInterpolation(true);
         ProcessEpochsOnWaiting(true);
         interpolator->SetForceInterpolation(false);      
      }
      
      if (fileType == CCSDS_AEM)
         WriteString("DATA_STOP\n");
      
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
   // Process any epochs on waiting
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage
      ("EphemerisFile::ProcessEpochsOnWaiting() entered, checkFinalEpoch=%d\n   "
       "There are %d epochs waiting to be output\n", checkFinalEpoch,
       epochsOnWaiting.size());
   for (UnsignedInt i = 0; i < epochsOnWaiting.size(); i++)
      MessageInterface::ShowMessage("      %18.12f\n", epochsOnWaiting[i]);
   #endif
   
   Real estimates[6];
   
   RealArray::iterator i = epochsOnWaiting.begin();
   while (i < epochsOnWaiting.end())
   {
      // Do not write after the final epoch
      if (checkFinalEpoch)
      {
         if ( (*i) > currentEpoch)
            break;
      }
      
      if (interpolator->IsInterpolationFeasible(*i) == 1)
      {
         // Now interpolate at epoch
         #ifdef DEBUG_EPHEMFILE_ORBIT
         MessageInterface::ShowMessage
            ("   =====> now try interpolating at epoch %18.12f\n", *i);
         #endif
         if (interpolator->Interpolate(*i, estimates))
         {
            WriteOrbit(*i, estimates);
            epochsOnWaiting.erase(i); // erase returns the next one
         }
         else
         {
            #ifdef DEBUG_EPHEMFILE_ORBIT
            MessageInterface::ShowMessage
               ("   =====> epoch %18.12f failed to interpolate so exiting the loop\n",
                *i);
            #endif
            break;
         }
      }
      else
      {
         // @todo Is there more checking here?
         #ifdef DEBUG_EPHEMFILE_ORBIT
         MessageInterface::ShowMessage
            ("   =====> epoch %18.12f is not feasible so exiting the loop\n", *i);
         #endif
         break;
      }
   }
   
   #ifdef DEBUG_EPHEMFILE_ORBIT
   MessageInterface::ShowMessage
      ("EphemerisFile::ProcessEpochsOnWaiting() leaving\n   There are %d epochs "
       "waiting to be output\n", epochsOnWaiting.size());
   for (UnsignedInt i = 0; i < epochsOnWaiting.size(); i++)
      MessageInterface::ShowMessage("      %18.12f\n", epochsOnWaiting[i]);
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
   stepSizeInA1Mjd = rval / 86400.0;
   useStepSize = true;
   
   #ifdef DEBUG_EPHEMFILE_SET
   MessageInterface::ShowMessage
      ("EphemerisFile::SetStepSize() leaving, stepSize='%s', stepSizeInA1Mjd=%f\n",
       stepSize.c_str(), stepSizeInA1Mjd);
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
}


//------------------------------------------------------------------------------
// void WriteCcsdsHeader()
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsHeader()
{
   // @todo reformat the time in 2009-10-27T14:26:30
   std::string creationTime = GmatTimeUtil::GetCurrentTime();
   std::stringstream ss("");
   
   if (fileType == CCSDS_OEM)
      ss << "CCSDS_OEM_VERS = 1.0" << std::endl;
   else
      ss << "CCSDS_AEM_VERS = 1.0" << std::endl;
   
   ss << "CREATION_DATE  = @TODO_FORMAT " << creationTime; // << std::endl;
   ss << "ORIGINATOR     = GMAT USER" << std::endl << std::endl;
   
   WriteString(ss.str());
}


//------------------------------------------------------------------------------
// void WriteCcsdsOemMetadata()
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsOemMetadata()
{
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
}


//------------------------------------------------------------------------------
// void WriteCcsdsAemMetadata()
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsAemMetadata()
{
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
// void WriteCcsdsComment(const std::string &comment)
//------------------------------------------------------------------------------
void EphemerisFile::WriteCcsdsComment(const std::string &comment)
{
}


//------------------------------------------------------------------------------
// void DebugWriteTime(const std::string &msg, Real epoch)
//------------------------------------------------------------------------------
void EphemerisFile::DebugWriteTime(const std::string &msg, Real epoch)
{
   Real toMjd;
   std::string epochStr;
   // Convert current epoch to specified format
   TimeConverterUtil::Convert("A1ModJulian", epoch, "", epochFormat,
                              toMjd, epochStr);
   MessageInterface::ShowMessage
      ("========== %s epoch = %f '%s'\n", msg.c_str(), epoch,
       epochStr.c_str());
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
bool EphemerisFile::Distribute(const Real * dat, Integer len)
{
   #if DBGLVL_EPHEMFILE_DATA > 0
   MessageInterface::ShowMessage
      ("EphemerisFile::Distribute() this=<%p>'%s' called len=%d\n""   fileName='%s'\n",
       this, GetName().c_str(), len, fileName.c_str());
   MessageInterface::ShowMessage
      ("   active=%d, isEndOfReceive=%d, mSolverIterOption=%d, runstate=%d\n",
       active, isEndOfReceive, mSolverIterOption, runstate);
   #endif
   
   if (!active)
      return true;
   
   if (isEndOfReceive || len == 0)
   {
      FinishUpWriting();
      return true;
   }
   
   if (len == 0)
      return true;
   
   //------------------------------------------------------------
   // if not writing solver data and solver is running, just return
   //------------------------------------------------------------
   if ((mSolverIterOption == SI_NONE) &&
       (runstate == Gmat::SOLVING || runstate == Gmat::SOLVEDPASS))
   {
      #if DBGLVL_EPHEMFILE_DATA > 0
      MessageInterface::ShowMessage
         ("   ===> Just returning; not writing solver data and solver is running\n");
      #endif
      
      return true;
   }
   
   // Check initial and final epoch for writing, dat[0] is epoch
   bool writeData = false;
   currentEpoch = dat[0];
   Real currState[6];
   for (int i=0; i<6; i++)
      currState[i] = dat[i+1];
   
   // From InitialSpacecraftEpoch to FinalSpacecraftEpoch
   if (initialEpochA1Mjd == -999.999 && finalEpochA1Mjd == -999.999)
   {
      writeData = true;
   }
   // From InitialSpacecraftEpoch to user specified final epoch
   else if (initialEpochA1Mjd == -999.999 && finalEpochA1Mjd != -999.999)
   {
      if (currentEpoch <= finalEpochA1Mjd)
         writeData = true;
   }
   // From user specified initial epoch to FinalSpacecraftEpoch
   else if (initialEpochA1Mjd != -999.999 && finalEpochA1Mjd == -999.999)
   {
      if (currentEpoch >= initialEpochA1Mjd)
         writeData = true;
   }
   // From user specified initial epoch to user specified final epoch
   else
   {
      if (currentEpoch >= initialEpochA1Mjd && currentEpoch <= finalEpochA1Mjd)
         writeData = true;
   }
   
   #if DBGLVL_EPHEMFILE_DATA > 0
   MessageInterface::ShowMessage
      ("   Start writing data, time=%f, writeData=%d, writeOrbit=%d, "
       "writeAttitude=%d\n", currentEpoch, writeData, writeOrbit, writeAttitude);
   #endif
   
   // Check if it is time to write
   bool timeToWrite = IsTimeToWrite(currentEpoch, currState);
   
   //------------------------------------------------------------
   // write data to file
   //------------------------------------------------------------
   // Now actually write data
   if (writeData && timeToWrite)
   {
      if (firstTimeWriting)
      {
         WriteHeader();
         WriteMetadata();
         if (fileType == CCSDS_AEM)
            WriteString("DATA_START\n");
      }
      
      if (writeOrbit)
      {
         if (useStepSize)
            WriteOrbitAt(nextReqEpoch, currState);
         else
            WriteOrbit(currentEpoch, currState);
      }
      else if (writeAttitude)
      {
         WriteAttitude();
      }
      
      if (firstTimeWriting)
         firstTimeWriting = false;
   }
   
   return true;
}

