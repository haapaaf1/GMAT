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

#include "gmatdefs.hpp"
#include "DataFileFactory.hpp"
#include "MessageInterface.hpp"  // temporary

// Here are the supported classes
#include "DataFile.hpp"
#include "B3DataFile.hpp"
#include "SLRDataFile.hpp"
#include "TLEDataFile.hpp"
#include "TDMCCSDSDataFile.hpp"
#include "OPMCCSDSDataFile.hpp"
#include "OEMCCSDSDataFile.hpp"
#include "APMCCSDSDataFile.hpp"
#include "AEMCCSDSDataFile.hpp"

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  DataFile* CreateDataFile(const std::string &ofType, const std::string &withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested DataFile class.
 *
 * @param <ofType> type of DataFile object to create and return.
 * @param <withName> the name for the newly-created DataFile object.
 *
 * @return A pointer to the created object.
 */
//------------------------------------------------------------------------------
DataFile* DataFileFactory::CreateDataFile(const std::string &ofType,
                                    const std::string &withName)
{
MessageInterface::ShowMessage("DataFileFactory is creating a %s named %s\n",
      ofType.c_str(), withName.c_str());
   if (ofType == "B3DataFile")
      return new B3DataFile(withName);
   if (ofType == "SLRDataFile")
      return new SLRDataFile(withName);
   if (ofType == "TLEDataFile")
      return new TLEDataFile(withName);
   if (ofType == "TDMCCSDSDataFile")
      return new TDMCCSDSDataFile(withName);
   if (ofType == "OPMCCSDSDataFile")
      return new OPMCCSDSDataFile(withName);
   if (ofType == "OEMCCSDSDataFile")
      return new OEMCCSDSDataFile(withName);
   if (ofType == "APMCCSDSDataFile")
      return new APMCCSDSDataFile(withName);
   if (ofType == "AEMCCSDSDataFile")
      return new AEMCCSDSDataFile(withName);

   return NULL;
}


//------------------------------------------------------------------------------
//  DataFileFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class DataFileFactory.
 * (default constructor)
 */
//------------------------------------------------------------------------------
DataFileFactory::DataFileFactory() :
   Factory     (Gmat::DATASTREAM)
//   Factory     (Gmat::DATASTREAM)
{
   if (creatables.empty())
   {
      creatables.push_back("B3DataFile");
      creatables.push_back("SLRDataFile");
      creatables.push_back("TLEDataFile");
      creatables.push_back("TDMCCSDSDataFile");
      creatables.push_back("OPMCCSDSDataFile");
      creatables.push_back("OEMCCSDSDataFile");
      creatables.push_back("APMCCSDSDataFile");
      creatables.push_back("AEMCCSDSDataFile");
   }
}

//------------------------------------------------------------------------------
//  DataFileFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class DataFileFactory.
 *
 * @param <createList> list of creatable DataFile objects
 *
 */
//------------------------------------------------------------------------------
DataFileFactory::DataFileFactory(StringArray createList) :
Factory(createList, Gmat::DATASTREAM)
{
}


//------------------------------------------------------------------------------
//  DataFileFactory(const DataFileFactory& fact)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class DataFileFactory.  (copy constructor)
 *
 * @param <fact> the factory object to copy to "this" factory.
 */
//------------------------------------------------------------------------------
DataFileFactory::DataFileFactory(const DataFileFactory& fact) :
    Factory     (fact)
{
   if (creatables.empty())
   {
      creatables.push_back("B3DataFile");
      creatables.push_back("SLRDataFile");
      creatables.push_back("TLEDataFile");
      creatables.push_back("TDMCCSDSDataFile");
      creatables.push_back("OPMCCSDSDataFile");
      creatables.push_back("OEMCCSDSDataFile");
      creatables.push_back("APMCCSDSDataFile");
      creatables.push_back("AEMCCSDSDataFile");
   }
}


//------------------------------------------------------------------------------
//  CommandFactory& operator= (const CommandFactory& fact)
//------------------------------------------------------------------------------
/**
 * DataFileFactory operator for the DataFileFactory base class.
 *
 * @param <fact> the DataFileFactory object that is copied.
 *
 * @return "this" DataFileFactory with data set to match the input factory (fact).
 */
//------------------------------------------------------------------------------
DataFileFactory& DataFileFactory::operator=(const DataFileFactory& fact)
{
   Factory::operator=(fact);
   return *this;
}


//------------------------------------------------------------------------------
// ~DataFileFactory()
//------------------------------------------------------------------------------
/**
 * Destructor for the DataFileFactory base class.
 */
//------------------------------------------------------------------------------
DataFileFactory::~DataFileFactory()
{
}

//---------------------------------
//  protected methods
//---------------------------------

//---------------------------------
//  private methods
//---------------------------------

