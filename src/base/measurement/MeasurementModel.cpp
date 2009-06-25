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
 * MeasurementModel implementation used in GMAT's estimators and simulator
 */
//------------------------------------------------------------------------------

#include "MeasurementModel.hpp"

MeasurementModel::MeasurementModel(const std::string &nomme) :
   GmatBase       (Gmat::MEASUREMENT_MODEL, "MeasurementModel", nomme)
{
   // TODO Auto-generated constructor stub

}

MeasurementModel::~MeasurementModel()
{
   // TODO Auto-generated destructor stub
}

MeasurementModel::MeasurementModel(const MeasurementModel &mm) :
   GmatBase       (mm)
{

}

MeasurementModel& MeasurementModel::operator=(const MeasurementModel &mm)
{
   if (&mm != this)
   {

   }

   return *this;
}
