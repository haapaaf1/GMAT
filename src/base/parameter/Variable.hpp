//$Header$
//------------------------------------------------------------------------------
//                                Variable
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2004/09/15
//
/**
 * Declares Variable class which provides methods for general variable evaluation.
 *    Such as: 100 * Sat1.X; where Sat1.X is already defined
 *             Sat1.X + Sat1.X;
 *             A * B; where A and B are already defined
 */
//------------------------------------------------------------------------------
#ifndef Variable_hpp
#define Variable_hpp

#include "gmatdefs.hpp"
#include "RealVar.hpp"
#include "ParameterDatabase.hpp"
#include "ExpressionParser.hpp"

class GMAT_API Variable : public RealVar
{
public:

   Variable(const std::string &name, const std::string &desc = "",
            const std::string &unit = "");
   Variable(const Variable &copy);
   Variable& operator=(const Variable &right);
   virtual ~Variable();

   // methods inherited from RealVar
   virtual Real GetReal();
   virtual Real EvaluateReal();
  
   // methods inherited from GmatBase //loj: 9/10/04 added
   virtual std::string GetRefObjectName(const Gmat::ObjectType type) const;
   virtual bool SetRefObjectName(const Gmat::ObjectType type,
                                 const std::string &name);
   virtual GmatBase* GetRefObject(const Gmat::ObjectType type,
                                  const std::string &name);
   virtual bool SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                             const std::string &name = "");

protected:

   ParameterDatabase *mParamDb;
   ExpressionParser *mExpParser;
};

#endif // Variable_hpp
