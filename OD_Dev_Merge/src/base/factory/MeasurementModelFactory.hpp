//------------------------------------------------------------------------------
//                            MeasurementModelFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/23
//
/**
 *  Implementation code for the MeasurementModelFactory class, responsible
 *  for creating MeasurementModel objects.
 */
//------------------------------------------------------------------------------

#ifndef _MEASUREMENTMODELFACTORY_HPP
#define	_MEASUREMENTMODELFACTORY_HPP

#include "Factory.hpp"

class MeasurementModel;

class MeasurementModelFactory : public Factory
{
public:
   virtual MeasurementModel* CreateMeasurementModel(const std::string &ofType,
                                const std::string &withName /* = "" */);

   // default constructor
   MeasurementModelFactory();
   // constructor
   MeasurementModelFactory(StringArray createList);
   // copy constructor
   MeasurementModelFactory(const MeasurementModelFactory& fact);
   // assignment operator
   MeasurementModelFactory& operator=(const MeasurementModelFactory& fact);

   virtual ~MeasurementModelFactory();

};

#endif	/* _MEASUREMENTMODELFACTORY_HPP */
