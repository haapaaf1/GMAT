//$Header$
//------------------------------------------------------------------------------
//                                If 
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author:  Joey Gurgnaus
// Created: 2004/01/30
//
/**
 * Declaration for the IF command class
 */
//------------------------------------------------------------------------------


#ifndef If_hpp
#define If_hpp

#include "gmatdefs.hpp"
#include "BranchCommand.hpp"
#include "Parameter.hpp"

/**
 * Command that manages processing for entry to the IF statement 
 *
 * The If command manages the If statement.  
 *
 */
class GMAT_API If : public BranchCommand
{
public:
   // default constructor
   If();
   // copy constructor
   If(const If &ic);
   // operator =
   If& operator=(const If &ic);
   // destructor
   virtual ~If(void);
   
   // Inherited methods that need some enhancement from the base class
   virtual bool         Append(GmatCommand *cmd);
   
   // Methods used to run the command
   virtual bool         Initialize();
   virtual bool         Execute();
   
   // Method to set up the condition(s) for the If command
   virtual bool         SetCondition(std::string lhs, std::string operation,
                                       std::string rhs);
   
   // inherited from GmatBase
   virtual GmatBase*    Clone() const;
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                       const std::string &name,
                                       const Integer index);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                       const std::string &name,
                                       const Integer index);
    
    
protected:

   bool                 EvaluateCondition(Integer which);

   enum OpType
   {
      EQUAL_TO = 0,
      NOT_EQUAL,
      GREATER_THAN,
      LESS_THAN,
      GREATER_OR_EQUAL,
      LESS_OR_EQUAL,
      NumberOfOperators
   };

   static const std::string OPTYPE_TEXT[NumberOfOperators];

   /// Number of consitions for the If
   Integer                  numberOfConditions;
   /// Counter to track how deep the If nesting is
   Integer                  nestLevel;
   /// The object array used in GetRefObjectArray()
   ObjectArray              objectArray;
   /// Arrays representing conditions
   StringArray              lhsList;
   std::vector<OpType>      opList;
   StringArray              rhsList;
   /// list of parameter objects used by the conditions
   std::vector<Parameter*>   params;
   
};
#endif  // If_hpp
