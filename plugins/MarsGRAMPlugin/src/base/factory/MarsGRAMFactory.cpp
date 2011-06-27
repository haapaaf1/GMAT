//$Id: MarsGRAMFactory.cpp 9490 2011-04-27 16:28:40Z tdnguye2 $
//------------------------------------------------------------------------------
//                            MarsGRAMFactory
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Tuan Nguyen
// Created: 2011/6/13
//
/**
 *  Implementation code for the MarsGRAMFactory class.
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "MarsGRAMFactory.hpp"
#include "MarsGRAM2005.hpp"

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  AtmosphereModel* CreateAtmosphereModel(const std::string &ofType, const std::string &withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested AtmosphereModel class.
 *
 * @param <ofType> type of AtmosphereModel object to create and return.
 * @param <withName> the name for the newly-created AtmosphereModel object.
 * 
 * @return A pointer to the created object.
 */
//------------------------------------------------------------------------------
AtmosphereModel* MarsGRAMFactory::CreateAtmosphereModel(
		const std::string &ofType,
        const std::string &withName,
        const std::string &forBody)
{
   if (forBody != "Mars")
	   return NULL;

   if (ofType == "MarsGRAM2005")
   {
	   AtmosphereModel* atm_model = (AtmosphereModel*)(new MarsGRAM2005(ofType, withName));
	   return atm_model;
   }
   else if (ofType == "MarsGRAM2001")
   {
//	   AtmosphereModel* atm_model = (AtmosphereModel*)(new MarsGRAM2001(ofType, withName));
//	   return atm_model;
	   return NULL;
   }
   else if (ofType == "MarsGRAM2000")
   {
//	   AtmosphereModel* atm_model = (AtmosphereModel*)(new MarsGRAM2000(ofType, withName));
//	   return atm_model;
	   return NULL;
   }
   else
   {
	   // no other MarsGRAM was found:
	   return NULL;
   }

}


//------------------------------------------------------------------------------
//  MarsGRAMFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class MarsGRAMFactory.
 * (default constructor)
 */
//------------------------------------------------------------------------------
MarsGRAMFactory::MarsGRAMFactory() :
   AtmosphereFactory()
{
   if (creatables.empty())
   {
      creatables.push_back("MarsGRAM");
   }
}


//------------------------------------------------------------------------------
//  MarsGRAMFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class MarsGRAMFactory.
 *
 * @param <createList> list of creatable AtmosphereModel objects
 *
 */
//------------------------------------------------------------------------------
MarsGRAMFactory::MarsGRAMFactory(StringArray createList) :
   AtmosphereFactory(createList)
{
}


//------------------------------------------------------------------------------
//  MarsGRAMFactory(const MarsGRAMFactory& fact)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class MarsGRAMFactory.
 * (copy constructor)
 *
 * @param <fact> the factory object to copy to "this" factory.
 */
//------------------------------------------------------------------------------
MarsGRAMFactory::MarsGRAMFactory(const MarsGRAMFactory& fact) :
   AtmosphereFactory (fact)
{
   if (creatables.empty())
   {
      creatables.push_back("MarsGRAM2005");
   }
}


//------------------------------------------------------------------------------
//  MarsGRAMFactory& operator= (const MarsGRAMFactory& fact)
//------------------------------------------------------------------------------
/**
 * MarsGRAMFactory operator for the MarsGRAMFactory base class.
 *
 * @param <fact> the MarsGRAMFactory object that is copied.
 *
 * @return "this" MarsGRAMFactory with data set to match the input factory (fact).
 */
//------------------------------------------------------------------------------
MarsGRAMFactory& MarsGRAMFactory::operator=(const MarsGRAMFactory& fact)
{
   AtmosphereFactory::operator=(fact);
   return *this;
}
    

//------------------------------------------------------------------------------
// ~MarsGRAMFactory()
//------------------------------------------------------------------------------
/**
 * Destructor for the FminconOptimizerFactory base class.
 */
//------------------------------------------------------------------------------
MarsGRAMFactory::~MarsGRAMFactory()
{
}

