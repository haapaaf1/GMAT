//$Header$
//------------------------------------------------------------------------------
//                            SpacecraftFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy Shoan
// Created: 2003/10/14
//
/**
 *  Implementation code for the SpacecraftFactory class, responsible for 
 *  creating Spacecraft objects.
 */
//------------------------------------------------------------------------------
#include "gmatdefs.hpp"
#include "Factory.hpp"
#include "SpacecraftFactory.hpp"
#include "Spacecraft.hpp" 
#include "Formation.hpp"

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  CreateSpacecraft(std::string ofType, std::string withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested Spacecraft class 
 *
 * @param <ofType> the Spacecraft object to create and return.
 * @param <withName> the name for the newly-created Spacecraft object.
 *
 * @note As of 2003/10/14, we are ignoring the ofType parameter.  Use of this
 *       parameter may be added later.
 */
//------------------------------------------------------------------------------
SpaceObject* SpacecraftFactory::CreateSpacecraft(const std::string &ofType,
                                                 const std::string &withName)
{
   if (ofType == "Spacecraft")
      return new Spacecraft(withName);
   if (ofType == "Formation")
      return new Formation(Gmat::FORMATION, "Formation", withName);
   return NULL;   
}


//------------------------------------------------------------------------------
//  SpacecraftFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class SpacecraftFactory
 *  (default constructor)
 */
//------------------------------------------------------------------------------
SpacecraftFactory::SpacecraftFactory() 
   :
   Factory(Gmat::SPACECRAFT)
{
   if (creatables.empty())
   {
      creatables.push_back("Spacecraft");
      creatables.push_back("Formation");
   }
}

//------------------------------------------------------------------------------
//  SpacecraftFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class SpacecraftFactory
 *  (constructor)
 *
 * @param <createList> the initial list of createble objects for this class
 */
//------------------------------------------------------------------------------
SpacecraftFactory::SpacecraftFactory(StringArray createList) 
   :
   Factory(createList, Gmat::SPACECRAFT)
{
   if (creatables.empty())
   {
      creatables.push_back("Spacecraft");
      creatables.push_back("Formation");
   }
}

//------------------------------------------------------------------------------
//  SpacecraftFactory(const SpacecraftFactory &fact)
//------------------------------------------------------------------------------
/**
   * This method creates an object of the (base) class SpacecraftFactory 
   * (copy constructor).
   *
   * @param <fact> the factory object to copy to "this" factory.
   */
//------------------------------------------------------------------------------
SpacecraftFactory::SpacecraftFactory(const SpacecraftFactory &fact) :
Factory(fact)
{
   if (creatables.empty())
   {
      creatables.push_back("Spacecraft");
      creatables.push_back("Formation");
   }
}

//------------------------------------------------------------------------------
//  SpacecraftFactory& operator= (const SpacecraftFactory &fact)
//------------------------------------------------------------------------------
/**
   * Assignment operator for the SpacecraftFactory base class.
   *
   * @param <fact> the SpacecraftFactory object whose data to assign
   *                 to "this" factory.
   *
   * @return "this" SpacecraftFactory with data of input factory fact.
   */
//------------------------------------------------------------------------------
SpacecraftFactory& SpacecraftFactory::operator= (const SpacecraftFactory &fact)
{
   Factory::operator=(fact);
   if (creatables.empty())
   {
      creatables.push_back("Spacecraft");
      creatables.push_back("Formation");
   }
   return *this;
}

//------------------------------------------------------------------------------
// ~SpacecraftFactory()
//------------------------------------------------------------------------------
/**
   * Destructor for the SpacecraftFactory base class.
   *
   */
//------------------------------------------------------------------------------
SpacecraftFactory::~SpacecraftFactory()
{
   // deletes handled by Factory destructor
}

//---------------------------------
//  protected methods
//---------------------------------

//---------------------------------
//  private methods
//---------------------------------