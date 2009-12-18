//$Header$
//------------------------------------------------------------------------------
//                            SensorFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/11/30
//
/**
 *  Implementation code for the SensorFactory class, responsible
 *  for creating Sensor objects.
 */
//------------------------------------------------------------------------------


#include "gmatdefs.hpp"
#include "SensorFactory.hpp"
#include "MessageInterface.hpp"  // temporary

// Here are the supported classes
#include "Sensor.hpp"
#include "Receiver.hpp"
#include "Transmitter.hpp"
#include "Transceiver.hpp"

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  Sensor* CreateSensor(const std::string &ofType, const std::string &withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested Sensor class.
 *
 * @param <ofType> type of Sensor object to create and return.
 * @param <withName> the name for the newly-created Sensor object.
 *
 * @return A pointer to the created object.
 */
//------------------------------------------------------------------------------
Sensor* SensorFactory::CreateSensor(const std::string &ofType,
                                    const std::string &withName)
{
MessageInterface::ShowMessage("SensorFactory is creating a %s named %s\n",
      ofType.c_str(), withName.c_str());
   if (ofType == "Receiver")
      return new Receiver(withName);
   if (ofType == "Transmitter")
      return new Transmitter(withName);
   if (ofType == "Transceiver")
      return new Transceiver(withName);
   return NULL;
}


//------------------------------------------------------------------------------
//  SensorFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class SensorFactory.
 * (default constructor)
 */
//------------------------------------------------------------------------------
SensorFactory::SensorFactory() :
   Factory     (Gmat::DATA_FILE)
//   Factory     (Gmat::DATA_FILE)
{
   if (creatables.empty())
   {
      creatables.push_back("Receiver");
      creatables.push_back("Transmitter");
      creatables.push_back("Transceiver");
   }
}

//------------------------------------------------------------------------------
//  SensorFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class SensorFactory.
 *
 * @param <createList> list of creatable Sensor objects
 *
 */
//------------------------------------------------------------------------------
SensorFactory::SensorFactory(StringArray createList) :
Factory(createList, Gmat::DATA_FILE)
{
}


//------------------------------------------------------------------------------
//  SensorFactory(const SensorFactory& fact)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class SensorFactory.  (copy constructor)
 *
 * @param <fact> the factory object to copy to "this" factory.
 */
//------------------------------------------------------------------------------
SensorFactory::SensorFactory(const SensorFactory& fact) :
    Factory     (fact)
{
   if (creatables.empty())
   {
      creatables.push_back("Receiver");
      creatables.push_back("Transmitter");
      creatables.push_back("Transceiver");
   }
}


//------------------------------------------------------------------------------
//  CommandFactory& operator= (const CommandFactory& fact)
//------------------------------------------------------------------------------
/**
 * SensorFactory operator for the SensorFactory base class.
 *
 * @param <fact> the SensorFactory object that is copied.
 *
 * @return "this" SensorFactory with data set to match the input factory (fact).
 */
//------------------------------------------------------------------------------
SensorFactory& SensorFactory::operator=(const SensorFactory& fact)
{
   Factory::operator=(fact);
   return *this;
}


//------------------------------------------------------------------------------
// ~SensorFactory()
//------------------------------------------------------------------------------
/**
 * Destructor for the SensorFactory base class.
 */
//------------------------------------------------------------------------------
SensorFactory::~SensorFactory()
{
}

//---------------------------------
//  protected methods
//---------------------------------

//---------------------------------
//  private methods
//---------------------------------


