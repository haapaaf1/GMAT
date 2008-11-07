//$Header$
//------------------------------------------------------------------------------
//                            DataFileFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/11/06
//
/**
 *  Implementation code for the DataFileFactory class, responsible
 *  for creating DataFile objects.
 */
//------------------------------------------------------------------------------


#ifndef DataFileFactory_hpp
#define DataFileFactory_hpp

#include "Factory.hpp"

// Forward References for the supported DataFiles
class ProcessB3Data;
class ProcessTLEData;
class ProcessSLRData;

class DataFileFactory : public Factory
{
public:
   virtual DataFile* CreateDataFile(const std::string &ofType,
                                const std::string &withName /* = "" */);

   // default constructor
   DataFileFactory();
   // constructor
   DataFileFactory(StringArray createList);
   // copy constructor
   DataFileFactory(const DataFileFactory& fact);
   // assignment operator
   DataFileFactory& operator=(const DataFileFactory& fact);

   virtual ~DataFileFactory();

};

#endif // DataFileFactory_hpp

