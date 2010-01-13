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
#ifndef EphemerisFile_hpp
#define EphemerisFile_hpp

#include "Subscriber.hpp"
#include "Spacecraft.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"
#include "Interpolator.hpp"
#include <iostream>
#include <fstream>

#ifdef __USE_SPICE__
#include "SpiceKernelWriter.hpp"
#endif

#ifdef __USE_CCSDS_FILE__
// CCSDS header
#include "CCSDSHeader.hpp"
// CCSDS Orbit Ephemeris Message
#include "OEMCCSDSObType.hpp"
#include "OEMCCSDSMetaData.hpp"
#include "OEMStateVectorCCSDSData.hpp"
//#include "ProcessCCSDSOEMDataFile.hpp"
// CCSDS Attitude Ephemeris Message (future work)
//#include "AEMCCSDSObType.hpp"
//#include "AEMCCSDSMetaData.hpp"
//#include "AEMQuaternionCCSDSData.hpp"
#endif

class EphemerisFile : public Subscriber
{
public:
   EphemerisFile(const std::string &name);
   virtual ~EphemerisFile();
   EphemerisFile(const EphemerisFile &);
   EphemerisFile& operator=(const EphemerisFile&);
   
   // methods for this class
   std::string          GetFileName();
   virtual void         ValidateParameters();
   
   // methods inherited from Subscriber
   virtual bool         Initialize();
   
   // methods inherited from GmatBase
   virtual GmatBase*    Clone(void) const;
   virtual void         Copy(const GmatBase* orig);
   
   virtual bool         TakeAction(const std::string &action,
                                   const std::string &actionData = "");
   
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   virtual bool         IsParameterReadOnly(const Integer id) const;
   
   virtual Gmat::ObjectType
                        GetPropertyObjectType(const Integer id) const;
   virtual const StringArray&
                        GetPropertyEnumStrings(const Integer id) const;
   
   virtual Integer      GetIntegerParameter(const Integer id) const;
   virtual Integer      SetIntegerParameter(const Integer id,
                                            const Integer value);
   
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   
   
protected:

   const static UnsignedInt MAX_SEGMENT_SIZE = 1000;
   
   enum FileType
   {
      CCSDS_OEM, CCSDS_AEM, SPK_ORBIT, SPK_ATTITUDE,
   };
   
   Spacecraft        *spacecraft;
   CoordinateSystem  *coordSystem;
   Interpolator      *interpolator; // owned object

#ifdef __USE_SPICE__
   SpiceKernelWriter *spkWriter;    // owned object
#else
   void              *spkWriter;
#endif
   
#ifdef __USE_CCSDS_FILE__
   /// CCSDS file
   CCSDSHeader             ccsdsHeader;
   OEMCCSDSMetaData        ccsdsOemMetaData;
   OEMCCSDSObType          ccsdsOemData;
   //ProcessCCSDSOEMDataFile ccsdsOutFile;
#endif
   
   // for SPK file
   EpochArray  spkEpochArray;
   StateArray  spkStateArray;
   
   /// ephemeris output path from the startup file
   std::string oututPath;
   /// ephmeris full file name including the path
   std::string filePath;
   std::string spacecraftName;
   std::string fileName;
   std::string fileFormat;
   std::string epochFormat;
   std::string initialEpoch;
   std::string finalEpoch;
   std::string stepSize;
   std::string interpolatorName;
   std::string stateType;
   std::string coordSystemName;
   std::string writeEphemeris;
   Integer     interpolationOrder;
   Integer     initialCount;
   Integer     waitCount;
   
   Real        stepSizeInA1Mjd;
   Real        stepSizeInSecs;
   Real        initialEpochA1Mjd;
   Real        finalEpochA1Mjd;
   Real        nextOutEpoch;
   Real        nextReqEpoch;
   Real        currEpochInDays;
   Real        currEpochInSecs;
   Real        currState[6];
   Real        prevEpoch;
   Real        prevProcTime;
   Real        attEpoch;
   Real        attQuat[4];
   Real        maneuverEpochInDays;
   RealArray   epochsOnWaiting;
   
   bool        firstTimeWriting;
   bool        writingNewSegment;
   bool        useStepSize;
   bool        writeOrbit;
   bool        writeAttitude;
   bool        writeDataInDataCS;
   bool        processingLargeStep;
   bool        spkWriteFailed;
   
   Gmat::RunState prevRunState;
   
   CoordinateConverter coordConverter;
   
   FileType    fileType;
   
   /// for maneuver handling
   ObjectArray maneuversHandled;
   
   /// output data stream
   std::ofstream      dstream;
   
   /// Available file format list
   static StringArray fileFormatList;   
   /// Available epoch format list
   static StringArray epochFormatList;   
   /// Available initial epoch list
   static StringArray initialEpochList;
   /// Available final epoch list
   static StringArray finalEpochList;   
   /// Available step size list
   static StringArray stepSizeList;   
   /// Available state type list
   static StringArray stateTypeList;   
   /// Available write ephemeris list
   static StringArray writeEphemerisList;
   /// Available interpolator type list
   static StringArray interpolatorTypeList;
   
   // Initialization
   void        InitializeData();
   void        CreateInterpolator();
   void        CreateSpiceKernelWriter();
   bool        OpenEphemerisFile();
   
   // Time and data
   bool        CheckInitialAndFinalEpoch();
   void        HandleCcsdsOrbitData(bool writeData);
   void        HandleSpkOrbitData(bool writeData);
   
   // Interpolation
   void        RestartInterpolation(const std::string &comments = "");
   bool        IsTimeToWrite(Real epochInSecs, Real state[6]);
   void        WriteCcsdsOrbit(Real reqEpochInSecs, Real state[6]);
   void        WriteCcsdsOrbitAt(Real reqEpochInSecs, Real state[6]);
   void        GetAttitude();
   void        WriteCcsdsAttitude();
   void        FinishUpWriting();
   void        ProcessEpochsOnWaiting(bool checkFinalEpoch = false);
   bool        SetEpoch(Integer id, const std::string &value,
                        const StringArray &allowedValues);
   bool        SetStepSize(Integer id, const std::string &value,
                           const StringArray &allowedValues);
   void        HandleError(Integer id, const std::string &value,
                           const StringArray &allowedValues,
                           const std::string &additionalMsg = "");
   std::string ToString(const StringArray &strList);
   
   // General writing
   void        WriteString(const std::string &str);
   void        WriteHeader();
   void        WriteMetadata();
   void        WriteComments(const std::string &comments);
   
   // CCSDS file writing
   void        WriteCcsdsHeader();
   void        WriteCcsdsOemMetadata();
   void        WriteCcsdsAemMetadata();
   void        WriteCcsdsOem(const std::string &epoch, Real state[6]);
   void        WriteCcsdsAem(const std::string &epoch, Real quat[4]);
   void        WriteCcsdsComments(const std::string &comments);
   
   // SPK file writing
   void        WriteSpkHeader(); // This is for debug
   void        BufferSpkOrbitData(Real reqEpoch, Real state[6]);
   void        DeleteSpkOrbitData();
   void        WriteSpkOrbitDataSegment();
   void        WriteSpkOrbitMetaData();
   void        WriteSpkComments(const std::string &comments);
   void        FinalizeSpkFile();
   
   // for debugging
   void        DebugWriteTime(const std::string &msg, Real epoch);
   void        DebugWriteOrbit(Real reqEpochInSecs, Real state[6], bool logOnly = false);
   
   // methods inherited from Subscriber
   virtual bool Distribute(Integer len);
   virtual bool Distribute(const Real * dat, Integer len);
   virtual void HandleManeuvering(GmatBase *originator, bool flag, Real epoch,
                                  const StringArray &satNames,
                                  const std::string &desc);
   
   enum
   {
      SPACECRAFT = SubscriberParamCount,
      FILE_NAME,
      FILE_FORMAT,
      EPOCH_FORMAT,
      INITIAL_EPOCH,
      FINAL_EPOCH,
      STEP_SIZE,
      INTERPOLATOR,
      INTERPOLATION_ORDER,
      STATE_TYPE,
      COORDINATE_SYSTEM,
      WRITE_EPHEMERIS,
      EphemerisFileParamCount  /// Count of the parameters for this class
   };
   
   static const std::string
      PARAMETER_TEXT[EphemerisFileParamCount - SubscriberParamCount];
   static const Gmat::ParameterType
      PARAMETER_TYPE[EphemerisFileParamCount - SubscriberParamCount];
   
};

#endif // EphemerisFile_hpp
