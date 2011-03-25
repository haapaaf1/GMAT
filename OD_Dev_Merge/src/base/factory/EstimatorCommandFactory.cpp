//$Header$
//------------------------------------------------------------------------------
//                            EstimatorCommandFactory
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy Shoan
// Created: 2003/10/09
//
/**
 *  Implementation code for the EstimatorCommandFactory class, responsible for
 *  creating Command objects.
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "Factory.hpp"
#include "EstimatorCommandFactory.hpp"

#include "RunEstimator.hpp"   // for RunEstimator command


//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  GmatCommand* CreateCommand(const std::string &ofType,
//                             const std::string &withName)
//------------------------------------------------------------------------------
/**
 * This method creates and returns an object of the requested command class
 *
 * @param <ofType>   type of command object to create and return.
 * @param <withName> name of the command (currently not used).
 *
 * @return command object
 *
 * @note As of 2003/10/14, we are ignoring the withname parameter.  Use of this
 *       parameter may be added later.
 */
//------------------------------------------------------------------------------
GmatCommand* EstimatorCommandFactory::CreateCommand(const std::string &ofType,
                                           const std::string &withName)
{
    if (ofType == "RunEstimator")
        return new RunEstimator;
   // add more here .......
   else
   {
      return NULL;   // doesn't match any known type of command
   }

}


//------------------------------------------------------------------------------
//  EstimatorCommandFactory()
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class EstimatorCommandFactory.
 * (default constructor)
 *
 */
//------------------------------------------------------------------------------
EstimatorCommandFactory::EstimatorCommandFactory() :
    Factory(Gmat::COMMAND)
{
   if (creatables.empty())
   {
      creatables.push_back("RunEstimator");
   }
}

//------------------------------------------------------------------------------
//  EstimatorCommandFactory(StringArray createList)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the class EstimatorCommandFactory.
 *
 * @param <createList> list of creatable command objects
 *
 */
//------------------------------------------------------------------------------
EstimatorCommandFactory::EstimatorCommandFactory(StringArray createList) :
    Factory(createList,Gmat::COMMAND)
{
}

//------------------------------------------------------------------------------
//  EstimatorCommandFactory(const EstimatorCommandFactory& fact)
//------------------------------------------------------------------------------
/**
 * This method creates an object of the (base) class EstimatorCommandFactory (called by
 * copy constructors of derived classes).  (copy constructor)
 *
 * @param <fact> the factory object to copy to "this" factory.
 */
//------------------------------------------------------------------------------
EstimatorCommandFactory::EstimatorCommandFactory(const EstimatorCommandFactory& fact) :
    Factory(fact)
{
   if (creatables.empty())
   {
      creatables.push_back("RunEstimator");
   }
}

//------------------------------------------------------------------------------
//  EstimatorCommandFactory& operator= (const EstimatorCommandFactory& fact)
//------------------------------------------------------------------------------
/**
 * Assignment operator for the EstimatorCommandFactory base class.
 *
 * @param <fact> the EstimatorCommandFactory object whose data to assign to "this" factory.
 *
 * @return "this" EstimatorCommandFactory with data of input factory fact.
 */
//------------------------------------------------------------------------------
EstimatorCommandFactory& EstimatorCommandFactory::operator= (const EstimatorCommandFactory& fact)
{
   Factory::operator=(fact);
   return *this;
}

//------------------------------------------------------------------------------
// ~EstimatorCommandFactory()
//------------------------------------------------------------------------------
/**
 * Destructor for the EstimatorCommandFactory base class.
 */
//------------------------------------------------------------------------------
EstimatorCommandFactory::~EstimatorCommandFactory()
{
   // deletes handled by Factory destructor
}

//---------------------------------
//  protected methods
//---------------------------------

//---------------------------------
//  private methods
//---------------------------------

