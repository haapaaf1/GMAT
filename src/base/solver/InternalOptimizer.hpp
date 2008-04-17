//$Id$
//------------------------------------------------------------------------------
//                             InternalOptimizer
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2007/05/23
//
/**
 * Implementation for the steepest descent optimizer. 
 */
//------------------------------------------------------------------------------


#ifndef InternalOptimizer_hpp
#define InternalOptimizer_hpp

#include "Optimizer.hpp"

class GMAT_API InternalOptimizer : public Optimizer
{
public:
	InternalOptimizer(std::string type, std::string name);
	virtual ~InternalOptimizer();
   InternalOptimizer(const InternalOptimizer &opt);
   InternalOptimizer&      operator=(const InternalOptimizer& opt);

   virtual bool        Initialize();
   
protected:
   
   enum
   {
      InternalOptimizerParamCount = OptimizerParamCount
   };
};

#endif /*InternalOptimizer_hpp*/
