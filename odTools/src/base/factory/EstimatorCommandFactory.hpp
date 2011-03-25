//$Header$
//------------------------------------------------------------------------------
//                         EstimatorCommandFactory
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
 *  This class is the factory class for commands.
 */
//------------------------------------------------------------------------------
#ifndef EstimatorCommandFactory_hpp
#define EstimatorCommandFactory_hpp

#include "gmatdefs.hpp"
#include "Factory.hpp"
#include "GmatCommand.hpp"

class GMAT_API EstimatorCommandFactory : public Factory
{
public:
   GmatCommand*  CreateCommand(const std::string &ofType,
                               const std::string &withName = "");

   // default constructor
   EstimatorCommandFactory();
   // constructor
   EstimatorCommandFactory(StringArray createList);
   // copy constructor
   EstimatorCommandFactory(const EstimatorCommandFactory& fact);
   // assignment operator
   EstimatorCommandFactory& operator= (const EstimatorCommandFactory& fact);

   // destructor
   ~EstimatorCommandFactory();

protected:
   // protected data

private:

   // private data


};

#endif // EstimatorCommandFactory_hpp

