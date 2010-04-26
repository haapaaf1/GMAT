//$Id$
//------------------------------------------------------------------------------
//                                   MathParser
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under MOMS task
// 124.
//
// Author: Darrel J. Conway
// Created: 2006/03/16
// Modified: 2006/04/10 Linda Jun /NASA/GSFC
//   - Added actual code
//
/**
 * Class definition for the MathParser.
 *
 * The MathParser class takes a line of script that evaluates to inline math,
 * and breaks that line apart into its component elements using a recursive 
 * descent algorithm.  The resulting representation is stored in a binary tree 
 * structure, which is calculated, depth first, when the expression needs to be
 * evaluated during execution of a script.
 */
//------------------------------------------------------------------------------

#ifndef MathParser_hpp
#define MathParser_hpp


//#include "MathTree.hpp"

#include "MathNode.hpp"
#include "MathException.hpp"
#include <map>

class GMAT_API MathParser
{
public:
   
   MathParser();
   MathParser(const MathParser &copy);
   MathParser& operator=(const MathParser &right);
   virtual ~MathParser();
   
   bool IsEquation(const std::string &str);
   std::string FindLowestOperator(const std::string &str, Integer &opIndex,
                                  Integer start = 0);
   
   MathNode* Parse(const std::string &str);
   
   StringArray GetGmatFunctionNames();
   
protected:
   
   MathNode* ParseNode(const std::string &str);
   MathNode* CreateNode(const std::string &type,
                        const std::string &exp);
   
   StringArray Decompose(const std::string &str);
   
private:
   
   std::string theEquation;
   Integer theGmatFuncCount;
   
   StringArray ParseParenthesis(const std::string &str);
   StringArray ParseAddSubtract(const std::string &str);
   StringArray ParseMultDivide(const std::string &str);
   StringArray ParseMatrixOps(const std::string &str);
   StringArray ParsePower(const std::string &str);
   StringArray ParseUnary(const std::string &str);
   StringArray ParseMathFunctions(const std::string &str);
   StringArray ParseUnitConversion(const std::string &str);
   
   bool HasFunctionName(const std::string &str, const StringArray &fnList);
   bool IsParenPartOfFunction(const std::string &str);
   bool IsGmatFunction(const std::string &name);
   std::string GetFunctionName(UnsignedInt functionType, const std::string &str,
                               std::string &leftStr);
   std::string FindOperatorFrom(const std::string &str, std::string::size_type start,
                                std::string &left, std::string &right,
                                std::string::size_type &opIndex);
   std::string::size_type FindSubtract(const std::string &str, std::string::size_type start);
   std::string GetOperator(const std::map<std::string, Integer>::iterator &pos1,
                           const std::map<std::string, Integer>::iterator &pos2,
                           const std::map<std::string, Integer> &opIndexMap,
                           Integer &opIndex);
   std::string FindOperator(const std::string &str, Integer &opIndex);
   
   std::string GetOperatorName(const std::string &op, bool &opFound);
   void BuildAllFunctionList();
   void BuildGmatFunctionList(const std::string &str);
   void BuildFunction(const std::string &str, const StringArray &fnList,
                      std::string &fnName, std::string &leftStr);
   std::string::size_type FindMatchingParen(const std::string &str, std::string::size_type start);
   void FillItems(StringArray &items, const std::string &op,
                  const std::string &left, const std::string &right);
   
   void WriteItems(const std::string &msg, StringArray &items);
   void WriteNode(MathNode *node, UnsignedInt level);
   
   enum
   {
      MATH_FUNCTION,
      MATRIX_FUNCTION,
      MATRIX_OP,
      UNIT_CONVERSION,
      GMAT_FUNCTION,
   };
   
   StringArray realFuncList;
   StringArray matrixFuncList;
   StringArray matrixOpList;
   StringArray unitConvList;
   StringArray gmatFuncList;
};


#endif //MathParser_hpp

