//$Id$
//------------------------------------------------------------------------------
//                         MeasurementManager
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
 * MeasurementManager declaration used in GMAT's estimators and simulator
 */
//------------------------------------------------------------------------------

#ifndef MeasurementManager_hpp
#define MeasurementManager_hpp

#include "gmatdefs.hpp"
#include "EstimationDefs.hpp"
#include "MeasurementData.hpp"
#include "ObservationData.hpp"
#include "Rmatrix.hpp"
#include "MeasurementModel.hpp"

// We'll want something like this eventually:
//#include "CoreMeasurement.hpp"
// But for now we go with this:
#include "GeometricMeasurement.hpp"
#include "Datafile.hpp"

/**
 * The mediator between the estimators/simulator and measurement models. 
 */
class MeasurementManager
{
public:
   MeasurementManager();
   virtual ~MeasurementManager();
   MeasurementManager(const MeasurementManager &mm);
   MeasurementManager& operator=(const MeasurementManager &mm);

   bool                    Initialize();
   bool                    PrepareForProcessing(bool simulating = false);
   bool                    ProcessingComplete();
   bool                    Finalize();
   
   bool                    CalculateMeasurements();
   bool                    CalculateMeasurementsAndDerivatives();
   bool                    WriteMeasurements();

   Integer                 AddMeasurement(MeasurementModel *meas);
   void                    AddMeasurementName(std::string measName);
   const StringArray&      GetMeasurementNames() const;
   const StringArray&      GetParticipantList();
   Integer                 Calculate(const Integer measurementToCalc);
   const MeasurementData*  GetMeasurement(const Integer measurementToGet);
   const StringArray&      GetStreamList();
   void                    SetStreamObject(Datafile *newStream);
   bool                    WriteMeasurement(const Integer measurementToWrite);

   // Observation reader methods needed for estimation
   void                    LoadObservations();
   GmatEpoch               GetEpoch();
   GmatEpoch               GetNextEpoch();
   const ObservationData * GetObsData(const Integer observationToGet = -1);
   void                    AdvanceObservation();

protected:
   /// List of the managed measurement models
   StringArray                      modelNames;
   /// List of all participants referenced in the measurement models
   StringArray                      participants;
   /// Pointers to the measurements
   std::vector<MeasurementModel*>   models;
   /// Current measurement epoch, ignoring event searching
   GmatEpoch                        anchorEpoch;
   /// Current measurement epoch, including event searching
   GmatEpoch                        currentEpoch;
   /// Measurement calculations
   std::vector<MeasurementData>     measurements;

   /// Observation data from all of the input observation data files
   std::vector<ObservationData>     observations;
   /// The current observation from the vector of observations
   std::vector<ObservationData>::iterator
                                    currentObs;

   /// Measurement derivatives
   std::vector<Rmatrix>             derivatives;
   /// Measurement stream objects
   StringArray                      streamNames;
   /// Measurement stream objects
   std::vector<Datafile*>           streamList;

   ///
   Integer                          idBase;
   Integer                          largestId;

   std::map<Integer,Datafile*>      idToStreamMap;
};

#endif /*MeasurementManager_hpp*/
