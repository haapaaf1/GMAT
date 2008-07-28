//$Header$
//------------------------------------------------------------------------------
//                            ObserverFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
//
/**
 *  Implementation code for the ObserverFactory class, responsible for 
 *  creating Observer objects.
 */
//------------------------------------------------------------------------------
#include "gmatdefs.hpp"
#include "Factory.hpp"
#include "ObserverFactory.hpp"
#include "Observer.hpp" 

//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  CreateObserver(std::string ofType, std::string withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested Observer class 
 *
 * @param <ofType> the Observer object to create and return.
 * @param <withName> the name for the newly-created Observer object.
 *
 * @note As of 2003/10/14, we are ignoring the ofType parameter.  Use of this
 *       parameter may be added later.
 */
//------------------------------------------------------------------------------
Observer* ObserverFactory::CreateObserver(const std::string &ofType,
                                                 const std::string &withName)
{
   if (ofType == "Observer")
      return new Observer(withName);
   return NULL;   
}


//------------------------------------------------------------------------------
//  ObserverFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class ObserverFactory
 *  (default constructor)
 */
//------------------------------------------------------------------------------
ObserverFactory::ObserverFactory() 
   :
   Factory(Gmat::Observer)
{
   if (creatables.empty())
   {
      creatables.push_back("Observer");
   }
}

//------------------------------------------------------------------------------
//  ObserverFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class ObserverFactory
 *  (constructor)
 *
 * @param <createList> the initial list of createble objects for this class
 */
//------------------------------------------------------------------------------
ObserverFactory::ObserverFactory(StringArray createList) 
   :
   Factory(createList, Gmat::Observer)
{
   if (creatables.empty())
   {
      creatables.push_back("Observer");
   }
}

//------------------------------------------------------------------------------
//  ObserverFactory(const ObserverFactory &fact)
//------------------------------------------------------------------------------
/**
   * This method creates an object of the (base) class ObserverFactory 
   * (copy constructor).
   *
   * @param <fact> the factory object to copy to "this" factory.
   */
//------------------------------------------------------------------------------
ObserverFactory::ObserverFactory(const ObserverFactory &fact) :
Factory(fact)
{
   if (creatables.empty())
   {
      creatables.push_back("Observer");
   }
}

//------------------------------------------------------------------------------
//  ObserverFactory& operator= (const ObserverFactory &fact)
//------------------------------------------------------------------------------
/**
   * Assignment operator for the ObserverFactory base class.
   *
   * @param <fact> the ObserverFactory object whose data to assign
   *                 to "this" factory.
   *
   * @return "this" ObserverFactory with data of input factory fact.
   */
//------------------------------------------------------------------------------
ObserverFactory& ObserverFactory::operator= (const ObserverFactory &fact)
{
   Factory::operator=(fact);
   if (creatables.empty())
   {
      creatables.push_back("Observer");
   }
   return *this;
}

//------------------------------------------------------------------------------
// ~ObserverFactory()
//------------------------------------------------------------------------------
/**
   * Destructor for the ObserverFactory base class.
   *
   */
//------------------------------------------------------------------------------
ObserverFactory::~ObserverFactory()
{
   // deletes handled by Factory destructor
}

//---------------------------------
//  protected methods
//---------------------------------

//---------------------------------
//  private methods
//---------------------------------
