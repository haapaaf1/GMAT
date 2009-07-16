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
      for (std::vector<MeasurementModel*>::const_iterator i = mm.models.begin();
            i != mm.models.end(); ++i)
         models.push_back((MeasurementModel*)(*i)->Clone());
   }

   return *this;
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
const MeasurementData* MeasurementManager::GetMeasurement(const Integer measurementToGet)
{
   MeasurementData *theMeasurement = NULL;

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


Integer MeasurementManager::AddMeasurement(MeasurementModel *meas)
{
   MeasurementData md;

   models.push_back((MeasurementModel*)meas->Clone());
   measurements.push_back(md);
   Integer measurementIndex = models.size() - 1;

   return measurementIndex;
}


bool MeasurementManager::CalculateMeasurements()
{
   bool retval = false;
   MeasurementData theData;

   for (std::vector<MeasurementModel*>::iterator i = models.begin();
         i != models.end(); ++i)
   {
      theData = (*i)->CalculateMeasurement();
      if (theData.isFeasible)
      {
         retval = true;
      }
   }

   return retval;
}

bool MeasurementManager::CalculateMeasurementsAndDerivatives()
{
   return false;
}

bool MeasurementManager::WriteMeasurements()
{
   return false;
}
