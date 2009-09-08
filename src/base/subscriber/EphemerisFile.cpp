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
#include "Publisher.hpp"           // for Instance()
#include "FileManager.hpp"         // for GetPathname()
#include "SubscriberException.hpp" // for exception
#include "StringUtil.hpp"          // for GetArrayIndex()
#include "TimeSystemConverter.hpp" // for ValidateTimeFormat()
#include "MessageInterface.hpp"
#include <iomanip>
#include <sstream>

//#define DEBUG_EPHEMFILE_SET

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
   Subscriber         ("EphemerisFile", name),
   spacecraft         (NULL),
   coordSystem        (NULL),
   interpolator       (NULL),
   oututPath          (""),
   filePath           (""),
   spacecraftName     (""),
   fileName           (""),
   fileFormat         ("CCSDS-OEM"),
   epochFormat        ("UTCGregorian"),
   initialEpoch       ("InitialSpacecraftEpoch"),
   finalEpoch         ("FinalSpacecraftEpoch"),
   stepSize           ("IntegratorSteps"),
   interpolatorName   ("Lagrange"),
   stateType          ("Cartesian"),
   coordSystemName    ("EarthMJ2000Eq"),
   writeEphemeris     ("Yes"),
   interpolationOrder (7),
   stepSizeReal       (-99999.999),
   initialEpochReal   (-99999.999),
   finalEpochReal     (-99999.999)
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
// ~EphemerisFile(void)
//------------------------------------------------------------------------------
EphemerisFile::~EphemerisFile(void)
{
}


//------------------------------------------------------------------------------
// EphemerisFile(const EphemerisFile &ef)
//------------------------------------------------------------------------------
EphemerisFile::EphemerisFile(const EphemerisFile &ef) :
   Subscriber         (ef),
   spacecraft         (ef.spacecraft),
   coordSystem        (ef.coordSystem),
   interpolator       (ef.interpolator),
   oututPath          (ef.oututPath),
   filePath           (ef.filePath),
   spacecraftName     (ef.spacecraftName),
   fileName           (ef.fileName),
   fileFormat         (ef.fileFormat),
   epochFormat        (ef.epochFormat),
   initialEpoch       (ef.initialEpoch),
   finalEpoch         (ef.finalEpoch),
   stepSize           (ef.stepSize),
   interpolatorName   (ef.interpolatorName),
   stateType          (ef.stateType),
   coordSystemName    (ef.coordSystemName),
   writeEphemeris     (ef.writeEphemeris),
   interpolationOrder (ef.interpolationOrder),
   stepSizeReal       (ef.stepSizeReal),
   initialEpochReal   (ef.initialEpochReal),
   finalEpochReal     (ef.finalEpochReal)
{
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
   
   spacecraft         = ef.spacecraft;
   coordSystem        = ef.coordSystem;
   interpolator       = ef.interpolator;
   oututPath          = ef.oututPath;
   filePath           = ef.filePath;
   spacecraftName     = ef.spacecraftName;
   fileName           = ef.fileName;
   fileFormat         = ef.fileFormat;
   epochFormat        = ef.epochFormat;
   initialEpoch       = ef.initialEpoch;
   finalEpoch         = ef.finalEpoch;
   stepSize           = ef.stepSize;
   interpolatorName   = ef.interpolatorName;
   stateType          = ef.stateType;
   coordSystemName    = ef.coordSystemName;
   writeEphemeris     = ef.writeEphemeris;
   interpolationOrder = ef.interpolationOrder;
   stepSizeReal       = ef.stepSizeReal;
   initialEpochReal   = ef.initialEpochReal;
   finalEpochReal     = ef.finalEpochReal;
   
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

   #ifdef DEBUG_EPHEMERISFILE_OPEN
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
   
   #ifdef DEBUG_EPHEMERISFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::GetFileName() returning fname=%s\n", fname.c_str());
   #endif
   
   return fname;
}



//----------------------------------
// methods inherited from Subscriber
//----------------------------------

//------------------------------------------------------------------------------
// virtual bool Initialize()
//------------------------------------------------------------------------------
bool EphemerisFile::Initialize()
{
   MessageInterface::ShowMessage
      ("==> EphemerisFile::Initialize() <%p>'%s' entered\n", this, GetName().c_str());
   
   Subscriber::Initialize();
   
   // check for FileFormat and StateType
   if ((fileFormat == "CCSDS-OEM" && stateType == "Quaternion") ||
       (fileFormat == "CCSDS-AEM" && stateType == "Cartesian"))
      throw SubscriberException
         ("In EphemerisFile::Initialize(), FileFormat \"" + fileFormat + "\" and "
          "StateType " + "\"" + stateType + "\" does not match for the object"
          "named \"" + GetName() + "\"");
   
   // check for StateType and Interpolator
   if (stateType == "Cartesian" && interpolatorName != "Lagrange")
      throw SubscriberException
         ("In EphemerisFile::Initialize(), The Interpolator must be \"Lagrange\" "
          "for StateType of \"Cartesian\" for the object named \"" + GetName() + "\"");
   
   // if active and not initialized already, open report file (loj: 2008.08.20)
   if (active && !isInitialized)
   {
      if (!OpenEphemerisFile())
         return false;
      
      isInitialized = true;
   }
   
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
   #ifdef DEBUG_EPHEMERISFILE_ACTION
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
      #ifdef DEBUG_EPHEMERISFILE_SET
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
   return NULL;
}


//------------------------------------------------------------------------------
// virtual bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
//                           const std::string &name = "")
//------------------------------------------------------------------------------
bool EphemerisFile::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                 const std::string &name)
{
   #if DBGLVL_EPHEMERISFILE_REF_OBJ
   MessageInterface::ShowMessage
      ("EphemerisFile::SetRefObject() <%p>'%s' entered, obj=%p, name=%s, objtype=%s, "
       "objname=%s\n", this, GetName().c_str(), obj, name.c_str(), obj->GetTypeName().c_str(),
       obj->GetName().c_str());
   #endif
   
   return Subscriber::SetRefObject(obj, type, name);
}


//------------------------------------------------------------------------------
// virtual const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
const StringArray& EphemerisFile::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   refObjectNames.clear();
   return refObjectNames;
}


//--------------------------------------
// protected methods
//--------------------------------------

//------------------------------------------------------------------------------
// bool OpenEphemerisFile()
//------------------------------------------------------------------------------
bool EphemerisFile::OpenEphemerisFile()
{
   fileName = GetFileName();
   
   #ifdef DEBUG_EPHEMERISFILE_OPEN
   MessageInterface::ShowMessage
      ("EphemerisFile::OpenEphemerisFile() entered, fileName = %s\n", fileName.c_str());
   #endif
   
   #ifdef DEBUG_EPHEMERISFILE_OPEN
   MessageInterface::ShowMessage("EphemerisFile::OpenEphemerisFile() returning true\n");
   #endif
   
   return true;
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
   stepSizeReal = rval;
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
   return true;
}

