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
MeasurementManager::MeasurementManager()
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
MeasurementManager::MeasurementManager(const MeasurementManager &mm)
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


Integer MeasurementManager::AddMeasurement(CoreMeasurement *meas)
{
   Integer measurementIndex = -1;
   return measurementIndex;
}


bool MeasurementManager::CalculateMeasurements()
{
   return false;
}

bool MeasurementManager::CalculateMeasurementsAndDerivatives()
{
   return false;
}

bool MeasurementManager::WriteMeasurements()
{
   return false;
}
