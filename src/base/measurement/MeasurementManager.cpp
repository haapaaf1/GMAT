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

//------------------------------------------------------------------------------
// MeasurementManager()
//------------------------------------------------------------------------------
/**
 * Constructor for the measurement manager
 */
//------------------------------------------------------------------------------
MeasurementManager::MeasurementManager() :
   anchorEpoch       (21545.0),
   currentEpoch      (21545.0)
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
   currentEpoch      (mm.currentEpoch)
{
   modelNames = mm.modelNames;

   for (std::vector<MeasurementModel*>::const_iterator i = mm.models.begin();
         i != mm.models.end(); ++i)
   {
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
   bool retval = true;

   measurements.clear();
   for (UnsignedInt i = 0; i < models.size(); ++i)
   {
      MeasurementData md;
      measurements.push_back(md);
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
   models.push_back((MeasurementModel*)meas->Clone());
   Integer measurementIndex = models.size() - 1;

   return measurementIndex;
}


//------------------------------------------------------------------------------
// const StringArray& GetParticipantList()
//------------------------------------------------------------------------------
/**
 * Accesses the complete list of measuremetn participants, avoiding duplicates.
 *
 * @return The list.
 */
//------------------------------------------------------------------------------
const StringArray& MeasurementManager::GetParticipantList()
{
   participants.clear();

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
// bool MeasurementManager::CalculateMeasurementsAndDerivatives()
//------------------------------------------------------------------------------
/**
 * Fires the calculation method of each owned MeasurementModel, and on success
 * (i.e. if the measurement was feasible) then fires the derivative calculator.
 *
 * @return True if at least one measurement is feasible and calculated; false
 *         otherwise
 */
//------------------------------------------------------------------------------
bool MeasurementManager::CalculateMeasurementsAndDerivatives()
{
   bool retval = false;

   for (UnsignedInt j = 0; j < models.size(); ++j)
   {
      measurements[j] = models[j]->CalculateMeasurement();
      if (measurements[j].isFeasible)
      {
         derivatives[j] = models[j]->CalculateMeasurementDerivatives();
         retval = true;
      }
   }

   return retval;
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
   return false;
}
