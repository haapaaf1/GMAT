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
 * MeasurementManager implementation used in GMAT's estimators and simulator
 */
//------------------------------------------------------------------------------

#include "MeasurementManager.hpp"
#include "MessageInterface.hpp"


//#define DEBUG_INITIALIZATION
//#define DEBUG_FILE_WRITE


//------------------------------------------------------------------------------
// MeasurementManager()
//------------------------------------------------------------------------------
/**
 * Constructor for the measurement manager
 */
//------------------------------------------------------------------------------
MeasurementManager::MeasurementManager() :
   anchorEpoch       (21545.0),
   currentEpoch      (21545.0),
   idBase            (10000),
   largestId         (10000)
{
}

//------------------------------------------------------------------------------
// ~MeasurementManager()
//------------------------------------------------------------------------------
/**
 * Destructor for the measurement manager
 */
//------------------------------------------------------------------------------
MeasurementManager::~MeasurementManager()
{
}

//------------------------------------------------------------------------------
// MeasurementManager(const MeasurementManager &mm)
//------------------------------------------------------------------------------
/**
 * Copy constructor for the measurement manager
 *
 * @param mm The manager that gets copied
 */
//------------------------------------------------------------------------------
MeasurementManager::MeasurementManager(const MeasurementManager &mm) :
   anchorEpoch       (mm.anchorEpoch),
   currentEpoch      (mm.currentEpoch),
   idBase            (mm.idBase),
   largestId         (mm.largestId)
{
   modelNames = mm.modelNames;

   for (std::vector<MeasurementModel*>::const_iterator i = mm.models.begin();
         i != mm.models.end(); ++i)
   {
      #ifdef DEBUG_INITIALIZATION
         MessageInterface::ShowMessage("Cloning %s MeasurementModel\n",
               (*i)->GetStringParameter("Type").c_str());
      #endif
      models.push_back((MeasurementModel*)((*i)->Clone()));
      MeasurementData md;
      measurements.push_back(md);
   }
}

//------------------------------------------------------------------------------
// MeasurementManager& operator=(const MeasurementManager &mm)
//------------------------------------------------------------------------------
/**
 * Measurement manager assignment operator
 *
 * @param mm The manager that gets copied
 *
 * @return This measurement manager
 */
//------------------------------------------------------------------------------
MeasurementManager& MeasurementManager::operator=(const MeasurementManager &mm)
{
   if (&mm != this)
   {
      anchorEpoch  = mm.anchorEpoch;
      currentEpoch = mm.currentEpoch;
      modelNames   = mm.modelNames;

      // Clone the measurements
      for (std::vector<MeasurementModel*>::iterator i = models.begin();
            i != models.end(); ++i)
         delete (*i);

      models.clear();
      measurements.clear();

      for (std::vector<MeasurementModel*>::const_iterator i = mm.models.begin();
            i != mm.models.end(); ++i)
         models.push_back((MeasurementModel*)(*i)->Clone());
   }

   return *this;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Verifies that the measuremetn models are ready to calculate measuremetns,
 * and builds internal data structures needed to manage these calculations.
 *
 * @return true is ready to go, false if not
 */
//------------------------------------------------------------------------------
bool MeasurementManager::Initialize()
{
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage(
            "Entered MeasurementManager::Initialize()\n");
   #endif

   bool retval = true;

   measurements.clear();
   for (UnsignedInt i = 0; i < models.size(); ++i)
   {
      if (models[i]->Initialize() == false)
         return false;
      MeasurementData md;
      measurements.push_back(md);
   }

   return retval;
}


bool MeasurementManager::PrepareForProcessing(bool simulating)
{
#ifdef DEBUG_INITIALIZATION
   MessageInterface::ShowMessage(
         "Entered MeasurementManager::PrepareForProcessing(%s)\n",
         (simulating ? "true" : "false"));
#endif

   bool retval = true;

   for (UnsignedInt i = 0; i < streamList.size(); ++i)
   {
      if (streamList[i]->OpenStream(simulating) == false)
         retval = false;
   }

   return retval;
}


bool MeasurementManager::ProcessingComplete()
{
   bool retval = true;

   for (UnsignedInt i = 0; i < streamList.size(); ++i)
   {
      if (streamList[i]->CloseStream() == false)
         retval = false;
   }

   return retval;
}


bool MeasurementManager::Finalize()
{
   bool retval = true;
   return retval;
}


//------------------------------------------------------------------------------
// Integer Calculate(const Integer measurementToCalc)
//------------------------------------------------------------------------------
/**
 * Calculate the selected measurement for the current state
 *
 * @param measurementToCalc The ID of the measurement that gets calculated.  If
 *                          the ID is -1, all measurements are attempted
 *
 * @return The number of measurements that were calculated
 */
//------------------------------------------------------------------------------
Integer MeasurementManager::Calculate(const Integer measurementToCalc)
{
   Integer successCount = 0;

   if (measurementToCalc == -1)
   {
      for (UnsignedInt j = 0; j < models.size(); ++j)
      {
         measurements[j] = models[j]->CalculateMeasurement();
         if (measurements[j].isFeasible)
            ++successCount;
      }
   }
   else
   {
      if ((measurementToCalc < (Integer)models.size()) && measurementToCalc >= 0)
      {
         measurements[measurementToCalc] =
               models[measurementToCalc]->CalculateMeasurement();
         if (measurements[measurementToCalc].isFeasible)
            successCount = 1;
      }
   }

   return successCount;
}

//------------------------------------------------------------------------------
// MeasurementData* GetMeasurement(const Integer measurementToGet)
//------------------------------------------------------------------------------
/**
 * Retrieves a calculated measurement
 *
 * @param measurementToGet ID of the desired measurement
 *
 * @return Pointer to the measurement, or NULL if the measuremetn was not
 *         available
 */
//------------------------------------------------------------------------------
const MeasurementData* MeasurementManager::GetMeasurement(
         const Integer measurementToGet)
{

   MeasurementData *theMeasurement = NULL;
   if ( (measurementToGet >= 0) &&
        (measurementToGet < (Integer)measurements.size()))
      theMeasurement = &measurements[measurementToGet];

   return theMeasurement;
}

//------------------------------------------------------------------------------
// bool WriteMeasurement(const Integer measurementToWrite)
//------------------------------------------------------------------------------
/**
 * Sends the selected measurement to a MeasurementStream
 *
 * @param measurementToWrite ID of the calculated measurement that is to be
 *                           written
 *
 * @return true if the measurement was sent to a MeasurementStream; false if
 *         not (either because the stream was not available or because the
 *         measurement is not possible)
 */
//------------------------------------------------------------------------------
bool MeasurementManager::WriteMeasurement(const Integer measurementToWrite)
{
   bool wasWritten = false;

   return wasWritten;
}


void MeasurementManager::LoadObservations()
{
   observations.clear();

   for (UnsignedInt i = 0; i < streamList.size(); ++i)
   {
      if (streamList[i]->IsOpen())
      {
         ObservationData *od;
         od = streamList[i]->ReadObservation();

         while (od != NULL)
         {
            observations.push_back(*od);
            od = streamList[i]->ReadObservation();
         }
      }
   }

   // Set the current data pointer to the first observation value
   currentObs = observations.begin();
}


GmatEpoch MeasurementManager::GetEpoch()
{
   if (currentObs == observations.end())
      return 0.0;
   return currentObs->epoch;
}


GmatEpoch MeasurementManager::GetNextEpoch()
{
   std::vector<ObservationData>::iterator nextObs = observations.end();
   if (currentObs != observations.end())
      nextObs = currentObs + 1;

   if (nextObs == observations.end())
      return 0.0;
   return nextObs->epoch;
}


const ObservationData *
   MeasurementManager::GetObsData(const Integer observationToGet)
{
   if (observationToGet == -1)
      return &(*currentObs);

   if ((observationToGet < 0) ||
       (observationToGet >= (Integer)observations.size()))
      // Should throw here
      return NULL;

   return &(observations[observationToGet]);
}


void MeasurementManager::AdvanceObservation()
{
   if (currentObs != observations.end())
      ++currentObs;
}

//------------------------------------------------------------------------------
// Integer AddMeasurement(MeasurementModel *meas)
//------------------------------------------------------------------------------
/**
 * Sets a new MeasurementModel on the MeasurementManager
 *
 * @param meas The new MeasurementModel
 *
 * @return The ID for the model during this run
 */
//------------------------------------------------------------------------------
Integer MeasurementManager::AddMeasurement(MeasurementModel *meas)
{
   meas->SetModelID(largestId++);
   models.push_back(meas);

   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage(
            "Added measurement of type %s with unique ID %d\n",
            meas->GetStringParameter("Type").c_str(),
            meas->GetModelID());
   #endif

   return meas->GetModelID();
}


//------------------------------------------------------------------------------
// const StringArray& GetParticipantList()
//------------------------------------------------------------------------------
/**
 * Accesses the complete list of measurement participants, avoiding duplicates.
 *
 * @return The list.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementManager::GetParticipantList()
{
   participants.clear();

#ifdef DEBUG_INITIALIZATION
   MessageInterface::ShowMessage("MeasurementManager knows about %d "
         "measurement models\n", models.size());
   MessageInterface::ShowMessage("MeasurementManager knows about %d "
         "measurement model names\n", modelNames.size());
#endif

   // Walk through the collection of measurement models...
   for (std::vector<MeasurementModel*>::iterator i =  models.begin();
         i !=  models.end(); ++i)
   {
      // Walk through the participant list for the model
      StringArray parts = (*i)->GetStringArrayParameter("Participants");
      for (StringArray::iterator j = parts.begin(); j != parts.end(); ++j)
      {
         // If the participant is not in the list yet, add it
         if (find(participants.begin(), participants.end(), (*j)) ==
               participants.end())
            participants.push_back(*j);
      }
   }

   return participants;
}


void MeasurementManager::AddMeasurementName(std::string measName)
{
   if (find(modelNames.begin(), modelNames.end(), measName) == modelNames.end())
      modelNames.push_back(measName);
}

const StringArray& MeasurementManager::GetMeasurementNames() const
{
   return modelNames;
}


GmatBase* MeasurementManager::GetClone(GmatBase *obj)
{
   GmatBase *retval = NULL;
   // Look for an object named same as obj
   std::string objname = obj->GetName();

   if (objname != "")
   {
      // Check the models
      for (UnsignedInt i = 0; i < models.size(); ++i)
         if (models[i]->GetName() == objname)
         {
            retval = models[i];
            break;
         }
   }

   return retval;
}

//------------------------------------------------------------------------------
// bool CalculateMeasurements()
//------------------------------------------------------------------------------
/**
 * Fires the calculation method of each owned MeasurementModel
 *
 * @return True if at least one measurement is feasible and calculated; false
 *         otherwise
 */
//------------------------------------------------------------------------------
bool MeasurementManager::CalculateMeasurements()
{
   bool retval = false;

   for (UnsignedInt j = 0; j < models.size(); ++j)
   {
      measurements[j] = models[j]->CalculateMeasurement();
      if (measurements[j].isFeasible)
         retval = true;
   }

   return retval;
}


//------------------------------------------------------------------------------
// const std::vector<RealArray>& MeasurementManager::CalculateDerivatives(
//                         GmatBase *obj, Integer wrt, Integer forMeasurement)
//------------------------------------------------------------------------------
/**
 * Fires the calculation method of each owned MeasurementModel, and on success
 * (i.e. if the measurement was feasible) then fires the derivative calculator.
 *
 * @param obj The object holding the with-respect-to parameters
 * @param wrt The with-respect-to parameters
 * @param forMeasurement Indix of the measurement that is being differentiated
 * 
 * @return The derivative data
 */
//------------------------------------------------------------------------------
const std::vector<RealArray>& MeasurementManager::CalculateDerivatives(
                           GmatBase *obj, Integer wrt, Integer forMeasurement)
{
   return models[forMeasurement]->CalculateMeasurementDerivatives(obj, wrt);
}


//------------------------------------------------------------------------------
// bool MeasurementManager::WriteMeasurements()
//------------------------------------------------------------------------------
/**
 * Writes the calculated data to a file.
 *
 * This method is used during measurement simulation to generate simulated data.
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool MeasurementManager::WriteMeasurements()
{
   #ifdef DEBUG_FILE_WRITE
      MessageInterface::ShowMessage("Entered MeasurementManager::"
            "WriteMeasurements()\n");
   #endif

   bool retval = false;

   for (UnsignedInt i = 0; i < measurements.size(); ++i)
   {
      if (measurements[i].isFeasible)
      {
         #ifdef DEBUG_FILE_WRITE
            MessageInterface::ShowMessage("Feasible\n   id = %d\n",
                  measurements[i].uniqueID);
            MessageInterface::ShowMessage("   Stream at <%p>\n",
                  idToStreamMap[measurements[i].uniqueID]);
         #endif
         idToStreamMap[measurements[i].uniqueID]->
               WriteMeasurement(&(measurements[i]));
         retval = true;
      }
   }

   return retval;
}


const StringArray& MeasurementManager::GetStreamList()
{
   #ifdef DEBUG_INITIALIZATION
      MessageInterface::ShowMessage("MeasurementManager::GetStreamList() "
            "Entered\n   %d models registered\n", models.size());
   #endif

   // Run through the measurements and build the list
   streamNames.clear();
   for (UnsignedInt i = 0; i < models.size(); ++i)
   {
      StringArray names = models[i]->GetStringArrayParameter("ObservationData");
      for (UnsignedInt j = 0; j < names.size(); ++j)
      {
         if(find(streamNames.begin(), streamNames.end(), names[j]) ==
               streamNames.end())
            streamNames.push_back(names[j]);
      }
   }

   return streamNames;
}


void MeasurementManager::SetStreamObject(Datafile *newStream)
{
   if (find(streamList.begin(), streamList.end(), newStream) ==
         streamList.end())
   {
      streamList.push_back(newStream);
      std::string streamName = newStream->GetName();

      // Walk through the models and set each model's index that refs the new
      // stream to point to it.
      for (UnsignedInt i = 0; i < models.size(); ++i)
      {
         StringArray od = models[i]->GetStringArrayParameter("ObservationData");
         for (UnsignedInt j = 0; j < od.size(); ++j)
         {
            // todo: Each model feeds only one stream with this code
            if (streamName == od[j])
            {
               idToStreamMap[models[i]->GetModelID()] = newStream;
               #ifdef DEBUG_INITIALIZATION
                  MessageInterface::ShowMessage("Stream id %d -> %s\n",
                        models[i]->GetModelID(), newStream->GetName().c_str());
               #endif
            }
         }
      }
   }
}

IntegerArray& MeasurementManager::GetValidMeasurementList()
{
   activeMeasurements.clear();
   Integer count = FindModelForObservation();
   #ifdef DEBUG_MODEL_MAPPING
      MessageInterface::ShowMessage("Found %d potential models for the current "
            "observation\n", count);
   #endif
   return activeMeasurements;
}

Integer MeasurementManager::FindModelForObservation()
{
   Integer retval = 0;

   Gmat::MeasurementType type =  currentObs->type;

   for (UnsignedInt i = 0; i < models.size(); ++i)
   {
      MeasurementData theMeas = models[i]->GetMeasurement();
      if (theMeas.type == type)
      {
         StringArray parts = currentObs->participantIDs;
         StringArray measParts = theMeas.participantIDs;
         bool missingParticipant = false;
         for (UnsignedInt j = 0; j < parts.size(); ++j)
            if (find(measParts.begin(), measParts.end(), parts[j]) == measParts.end())
               missingParticipant = true;

         if (!missingParticipant)
         {
            activeMeasurements.push_back(i);
            ++retval;
         }
      }
   }

   return retval;
}


void  MeasurementManager::Reset()
{
   currentObs = observations.begin();
}
