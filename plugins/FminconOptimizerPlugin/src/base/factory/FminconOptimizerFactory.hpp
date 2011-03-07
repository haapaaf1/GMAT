//$Id$
//------------------------------------------------------------------------------
//                            FminconOptimizerFactory
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Linda Jun
// Created: 2010/03/30
//
/**
 *  Declaration code for the FminconOptimizerFactory class.
 */
//------------------------------------------------------------------------------
#ifndef FminconOptimizerFactory_hpp
#define FminconOptimizerFactory_hpp


#include "fmincon_defs.hpp"
#include "Factory.hpp"
#include "Solver.hpp"

class FMINCON_API FminconOptimizerFactory : public Factory
{
public:
   virtual Solver* CreateSolver(const std::string &ofType,
                                const std::string &withName);
   
   // default constructor
   FminconOptimizerFactory();
   // constructor
   FminconOptimizerFactory(StringArray createList);
   // copy constructor
   FminconOptimizerFactory(const FminconOptimizerFactory& fact);
   // assignment operator
   FminconOptimizerFactory& operator=(const FminconOptimizerFactory& fact);
   
   virtual ~FminconOptimizerFactory();
   
};

#endif // FminconOptimizerFactory_hpp
