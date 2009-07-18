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


#include "DatafileFactory.hpp"
#include "Datafile.hpp"

DatafileFactory::DatafileFactory() :
   Factory     (Gmat::DATASTREAM)
{
   if (creatables.empty())
   {
      creatables.push_back("Datafile");
   }
}

DatafileFactory::~DatafileFactory()
{
}


DatafileFactory::DatafileFactory(StringArray createList) :
   Factory        (createList, Gmat::DATASTREAM)
{
   if (creatables.empty())
   {
      creatables.push_back("Datafile");
   }
}
DatafileFactory::DatafileFactory(const DatafileFactory& fact) :
   Factory        (fact)
{
   if (creatables.empty())
   {
      creatables.push_back("Datafile");
   }
}

DatafileFactory& DatafileFactory::operator= (const DatafileFactory& fact)
{
   if (&fact != this)
   {
      Factory::operator=(fact);

      if (creatables.empty())
      {
         creatables.push_back("Datafile");
      }
   }

   return *this;
}

Datafile* DatafileFactory::CreateDatafile(const std::string &ofType,
      const std::string &withName)
{
   Datafile *retval = NULL;

   if (ofType == "Datafile")
      retval = new Datafile(withName);

   return retval;
}
