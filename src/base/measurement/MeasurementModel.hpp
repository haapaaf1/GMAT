//$Id$
//------------------------------------------------------------------------------
//                          MeasurementModel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/06/24
//
/**
 * MeasurementModel declaration used in GMAT's estimators and simulator
 */
//------------------------------------------------------------------------------

#ifndef MeasurementModel_hpp
#define MeasurementModel_hpp

#include "GmatBase.hpp"
#include "EstimationDefs.hpp"
#include "MeasurementData.hpp"
#include "GeometricMeasurement.hpp"

class MeasurementModel : public GmatBase
{
public:
   MeasurementModel(const std::string &nomme = "");
   virtual ~MeasurementModel();
   MeasurementModel(const MeasurementModel &mm);
   MeasurementModel& operator=(const MeasurementModel &mm);

   virtual GmatBase* Clone() const;
   virtual bool Initialize();

   // Access methods derived classes can override
   virtual std::string  GetParameterText(const Integer id) const;
   virtual std::string  GetParameterUnit(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   virtual bool         IsParameterReadOnly(const Integer id) const;
   virtual bool         IsParameterReadOnly(const std::string &label) const;

   virtual Real         GetRealParameter(const Integer id) const;
   virtual Real         SetRealParameter(const Integer id,
                                         const Real value);
   virtual Real         GetRealParameter(const Integer id,
                                         const Integer index) const;
   virtual Real         GetRealParameter(const Integer id, const Integer row,
                                         const Integer col) const;
   virtual Real         SetRealParameter(const Integer id,
                                         const Real value,
                                         const Integer index);
   virtual Real         SetRealParameter(const Integer id, const Real value,
                                         const Integer row, const Integer col);
   virtual Real         GetRealParameter(const std::string &label) const;
   virtual Real         SetRealParameter(const std::string &label,
                                         const Real value);
   virtual Real         GetRealParameter(const std::string &label,
                                         const Integer index) const;
   virtual Real         SetRealParameter(const std::string &label,
                                         const Real value,
                                         const Integer index);
   virtual Real         GetRealParameter(const std::string &label,
                                         const Integer row,
                                         const Integer col) const;
   virtual Real         SetRealParameter(const std::string &label,
                                         const Real value, const Integer row,
                                         const Integer col);

   virtual std::string  GetStringParameter(const Integer id) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const Integer id,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index);
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const std::string &label,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index);

   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id,
                                                const Integer index) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label,
                                                const Integer index) const;



   // Access methods derived classes can override on reference objects
   virtual std::string  GetRefObjectName(const Gmat::ObjectType type) const;
   virtual const ObjectTypeArray&
                        GetRefObjectTypeArray();
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual bool         SetRefObjectName(const Gmat::ObjectType type,
                                         const std::string &name);
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name);
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name,
                                     const Integer index);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name,
                                     const Integer index);
   virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type);
   virtual ObjectArray& GetRefObjectArray(const std::string& typeString);

   virtual bool         SetMeasurement(CoreMeasurement *meas);
   virtual bool         IsOwnedObject(Integer id) const;
   virtual Integer      GetOwnedObjectCount();
   virtual GmatBase*    GetOwnedObject(Integer whichOne);

   Integer              GetModelID();
   void                 SetModelID(Integer newID);

   virtual const MeasurementData&
                        CalculateMeasurement();
   virtual const Rmatrix&
                        CalculateMeasurementDerivatives();
   virtual const MeasurementData&
                        GetMeasurement();
   virtual bool         WriteMeasurements();
   virtual bool         WriteMeasurement(Integer id);

protected:
   /// Name of the observation stream that supplied or receives data
   StringArray          observationStreamName;
   /// The observation stream
//   std::vector<Datafile*> observationStream;
   /// List of participants used in the contained measurement
   StringArray          participantNames;
   /// Pointers to the participants
   ObjectArray          participants;
   /// The type of measurement
   std::string          measurementType;
   /// The core measurement component
   CoreMeasurement      *measurement;
   /// Measurement data
   MeasurementData      *theData;
   /// And derivatives
   Rmatrix              *theDataDerivatives;

   /// Local storage element for ref objects
   StringArray          refObjectList;

   // Noise model parameters
   /// Measurement bias
   Real  measurementBias;
   /// Noise sigma
   Real noiseSigma;
   /// Time constant
   Real timeConstant;

   /// Internal ID used for this measurement model
   Integer modelID;

   enum
   {
       ObservationData = GmatBaseParamCount,
       MeasurementType,
       Participants,
       Bias,
       NoiseSigma,
       TimeConstant,
       MeasurementModelParamCount
   };

   // Start with the parameter IDs and associates strings
   static const std::string
                PARAMETER_TEXT[MeasurementModelParamCount - GmatBaseParamCount];
   static const Gmat::ParameterType
                PARAMETER_TYPE[MeasurementModelParamCount - GmatBaseParamCount];
};

#endif /* MeasurementModel_hpp */
