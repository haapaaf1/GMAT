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
#include "Interpolator.hpp"

class EphemerisFile : public Subscriber
{
public:
   EphemerisFile(const std::string &name);
   virtual ~EphemerisFile();
   EphemerisFile(const EphemerisFile &);
   EphemerisFile& operator=(const EphemerisFile&);
   
   // methods for this class
   std::string          GetFileName();
   
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
   
   Spacecraft       *spacecraft;
   CoordinateSystem *coordSystem;
   Interpolator     *interpolator;
   
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
   
   Real        stepSizeReal;
   Real        initialEpochReal;
   Real        finalEpochReal;
   
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
   
   bool        OpenEphemerisFile();
   bool        SetEpoch(Integer id, const std::string &value,
                        const StringArray &allowedValues);
   bool        SetStepSize(Integer id, const std::string &value,
                           const StringArray &allowedValues);
   void        HandleError(Integer id, const std::string &value,
                           const StringArray &allowedValues,
                           const std::string &additionalMsg = "");
   std::string ToString(const StringArray &strList);
   
   // methods inherited from Subscriber
   virtual bool         Distribute(Integer len);
   virtual bool         Distribute(const Real * dat, Integer len);
   
};

#endif // EphemerisFile_hpp
