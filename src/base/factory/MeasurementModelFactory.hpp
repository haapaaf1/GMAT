//$Id$
//------------------------------------------------------------------------------
//                         MeasurementModelFactory
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#ifndef MeasurementModelFactory_hpp
#define MeasurementModelFactory_hpp


#include "Factory.hpp"


/// Descriptor here
class MeasurementModelFactory : public Factory
{
public:
   MeasurementModelFactory();
   virtual ~MeasurementModelFactory();

   MeasurementModelFactory(StringArray createList);
   MeasurementModelFactory(const MeasurementModelFactory& fact);
   MeasurementModelFactory& operator= (const MeasurementModelFactory& fact);

   MeasurementModel *CreateMeasurementModel(const std::string &ofType,
         const std::string &withName = "");
};

#endif /* MeasurementModelFactory_hpp */
