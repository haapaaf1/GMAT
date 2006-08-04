//$Header$
//------------------------------------------------------------------------------
//                                ConditionalBranch 
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author:  Wendy Shoan
// Created: 2004/09/27
//
/**
 * Declaration for the ConditionalBranch command class
 */
//------------------------------------------------------------------------------


#ifndef ConditionalBranch_hpp
#define ConditionalBranch_hpp

#include "gmatdefs.hpp"
#include "BranchCommand.hpp"
#include "Parameter.hpp"

/**
 * Command that manages processing for entry to the conditional
 * branch commands. 
 *
 * The ConditionalBranch command manages the conditional branch
 * commands.  
 *
 */
class GMAT_API ConditionalBranch : public BranchCommand
{
public:
   // default constructor
   ConditionalBranch(const std::string &typeStr);
   // copy constructor
   ConditionalBranch(const ConditionalBranch &cb);
   // operator =
   ConditionalBranch& operator=(const ConditionalBranch &cb);
   // destructor
   virtual ~ConditionalBranch();
         
   // Method to set up the condition(s) for the conditional branch commands
   virtual bool         SetCondition(const std::string &lhs, 
                                     const std::string &operation,
                                     const std::string &rhs,
                                     Integer atIndex = -999);
   virtual bool         SetConditionOperator(const std::string &op,
                                             Integer atIndex = -999);
   virtual bool         RemoveCondition(Integer atIndex);
   virtual bool         RemoveConditionOperator(Integer atIndex);
   
   
   virtual bool         Initialize();
   
   // inherited from GmatBase
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                     const std::string &oldName,
                                     const std::string &newName);
   
   virtual GmatBase*    GetRefObject(const Gmat::ObjectType type,
                                     const std::string &name,
                                     const Integer index);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name,
                                     const Integer index);
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   
   virtual Integer      GetIntegerParameter(const Integer id) const;
//   virtual Integer      SetIntegerParameter(const Integer id,
//                                            const Integer value);
   virtual Integer      GetIntegerParameter(const std::string &label) const;
//   virtual Integer      SetIntegerParameter(const std::string &label,
//                                            const Integer value);
   virtual std::string  GetStringParameter(const Integer id,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const Integer id, 
                                           const std::string &value,
                                           const Integer index);
   virtual std::string  GetStringParameter(const std::string &label,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const std::string &label, 
                                           const std::string &value,
                                           const Integer index);
   virtual const StringArray& 
                        GetStringArrayParameter(const Integer id) const; 
   //virtual const StringArray& 
   //                     GetStringArrayParameter(const Integer id, 
   //                           const Integer index) const; 
   virtual const StringArray& 
                        GetStringArrayParameter(const std::string &label) const;
   //virtual const StringArray& 
   //                     GetStringArrayParameter(const std::string &label, 
   //                                             const Integer index) const; 
   
   
protected:

   virtual bool         EvaluateCondition(Integer which);
   virtual bool         EvaluateAllConditions();
   
   virtual bool         SetStringArrayValue(Integer forArray, 
                                            const std::string &toValue,
                                            Integer forIndex);
                                            
   std::string          GetConditionalString();
   
   enum
   {
      NUMBER_OF_CONDITIONS = BranchCommandParamCount,
      NUMBER_OF_LOGICAL_OPS,
      LEFT_HAND_STRINGS,
      OPERATOR_STRINGS,
      RIGHT_HAND_STRINGS,
      LOGICAL_OPERATORS,
      NUMBER_OF_REF_PARAMS,
      //REF_PARAMETER_NAMES,
      ConditionalBranchParamCount
   };

   static const std::string
   PARAMETER_TEXT[ConditionalBranchParamCount - BranchCommandParamCount];
   
   static const Gmat::ParameterType
   PARAMETER_TYPE[ConditionalBranchParamCount - BranchCommandParamCount];

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
   
   enum LogicalOpType
   {
      AND = 0,
      OR,
      NumberOfLogicalOperators
   };

   static const std::string OPTYPE_TEXT[NumberOfOperators];
   static const std::string LOGICAL_OPTYPE_TEXT[NumberOfLogicalOperators];

   /// Number of conditions for the conditional branch commands
   Integer                    numberOfConditions;
   /// Number  of separating logical operators (should be numberOfConditions - 1)
   Integer                    numberOfLogicalOps;
   /// Arrays representing conditions
   StringArray                lhsList;
   StringArray                lhsParamList;
   StringArray                opStrings;
   std::vector<OpType>        opList;
   StringArray                rhsList;
   StringArray                rhsParamList;
   StringArray                logicalOpStrings;
   std::vector<LogicalOpType> logicalOpList;
   /// list of parameter objects used by the conditions
   std::vector<Parameter*>    params;
   //StringArray                paramStrings;
   /// Row of lhs Array parameter
   std::vector<Integer>       lhsParamRows;
   /// Col of lhs Array parameter
   std::vector<Integer>       lhsParamCols;
   /// Row of rhs Array parameter
   std::vector<Integer>       rhsParamRows;
   /// Col of rhs Array parameter
   std::vector<Integer>       rhsParamCols;
   
};
#endif  // ConditionalBranch_hpp
