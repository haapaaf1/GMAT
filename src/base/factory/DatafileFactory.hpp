//$Id$
//------------------------------------------------------------------------------
//                         ClassName
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


#ifndef DATAFILEFACTORY_HPP_
#define DATAFILEFACTORY_HPP_

#include "Factory.hpp"
#include "Datafile.hpp"

/// Descriptor here
class DatafileFactory : public Factory
{
public:
   DatafileFactory();
   virtual ~DatafileFactory();

   DatafileFactory(StringArray createList);
   DatafileFactory(const DatafileFactory& fact);
   DatafileFactory& operator= (const DatafileFactory& fact);

   virtual Datafile *CreateDatafile(const std::string &ofType,
         const std::string &withName = "");
};

#endif /* DATAFILEFACTORY_HPP_ */
