//$Id$
//------------------------------------------------------------------------------
//                                  Transpose
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Allison Greene
// Created: 2006/04/20
//
/**
 * Implements Transpose class.
 */
//------------------------------------------------------------------------------

#include "Transpose.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_TRANSPOSE 1
//#define DEBUG_INPUT_OUTPUT

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// Transpose()
//------------------------------------------------------------------------------
/**
 * Constructor.
 */
//------------------------------------------------------------------------------
Transpose::Transpose(const std::string &nomme)
   : MathFunction("Transpose", nomme)
{
}


//------------------------------------------------------------------------------
// ~Transpose()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
Transpose::~Transpose()
{
}


//------------------------------------------------------------------------------
//  Transpose(const Transpose &copy)
//------------------------------------------------------------------------------
/**
 * Constructs the Transpose object (copy constructor).
 * 
 * @param <copy> Object that is copied
 */
//------------------------------------------------------------------------------
Transpose::Transpose(const Transpose &copy) :
   MathFunction      (copy)
{
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Clone of the Transpose operation.
 *
 * @return clone of the Transpose operation.
 *
 */
//------------------------------------------------------------------------------
GmatBase* Transpose::Clone() const
{
   return (new Transpose(*this));
}


//------------------------------------------------------------------------------
// void GetOutputInfo(Integer &type, Integer &rowCount, Integer &colCount)
//------------------------------------------------------------------------------
void Transpose::GetOutputInfo(Integer &type, Integer &rowCount, Integer &colCount)
{
   #ifdef DEBUG_INPUT_OUTPUT
   MessageInterface::ShowMessage
      ("Transpose::GetOutputInfo() exp=%s, leftNode=<%p><%s>, rightNode=<%p><%s>\n",
       GetName().c_str(), leftNode, leftNode ? leftNode->GetTypeName().c_str() : "NULL",
       rightNode, rightNode ? rightNode->GetTypeName().c_str() : "NULL");
   #endif
   
   if (!leftNode)
      throw MathException("Transpose::GetOutputInfo() The left node is NULL");
   
   Integer type1, row1, col1; // Left node
   
   // Get the type(Real or Matrix), # rows and # columns of the left node
   leftNode->GetOutputInfo(type1, row1, col1);
   
   if (type1 != Gmat::RMATRIX_TYPE)
      throw MathException("Left is not a matrix, so cannot do Transpose().\n");
   
   // output row and col is transpose of leftNode's row and col
   type = type1;
   rowCount = col1;
   colCount = row1;
   
   #ifdef DEBUG_INPUT_OUTPUT
   MessageInterface::ShowMessage
      ("Transpose::GetOutputInfo() returning type=%d, rowCount=%d, colCount=%d\n",
       type, rowCount, colCount);
   #endif
}


//------------------------------------------------------------------------------
// bool ValidateInputs()
//------------------------------------------------------------------------------
/**
 * This method calls its subnodes and checks to be sure that the subnodes return
 * compatible data for the function.
 */
//------------------------------------------------------------------------------
bool Transpose::ValidateInputs()
{
   Integer type1, row1, col1; // Left node
   bool retval = false;
   
   #ifdef DEBUG_INPUT_OUTPUT
   MessageInterface::ShowMessage
      ("Transpose::ValidateInputs() left=%s, %s\n",
       leftNode->GetTypeName().c_str(), leftNode->GetName().c_str());
   #endif
   
   // Get the type(Real or Matrix), # rows and # columns of the left node
   leftNode->GetOutputInfo(type1, row1, col1);
   
   if (type1 == Gmat::RMATRIX_TYPE)
      retval = true;
   else if (type1 == Gmat::REAL_TYPE && row1 == 1 && col1 == 1)
      retval = true;
   else
      retval = false;
   
   #ifdef DEBUG_INPUT_OUTPUT
   MessageInterface::ShowMessage
      ("Transpose::ValidateInputs() returning %d\n", retval);
   #endif
   
   return retval;
}


//------------------------------------------------------------------------------
// Rmatrix MatrixEvaluate()
//------------------------------------------------------------------------------
/**
 * @return the product of left and right nodes
 *
 */
//------------------------------------------------------------------------------
Rmatrix Transpose::MatrixEvaluate()
{
   //=======================================================
   #ifdef DEBUG_EVALUATE
   //=======================================================

   Rmatrix rmat = leftNode->MatrixEvaluate();
   MessageInterface::ShowMessage
      ("Transpose::MatrixEvaluate() = %s\n", rmat.ToString(12).c_str());
   try
   {
      Rmatrix result = rmat.Transpose();
      MessageInterface::ShowMessage
         ("Transpose::MatrixEvaluate() returning\n%s\n", result.ToString(12).c_str());
      return result;
   }
   catch (BaseException &be)
   {
      MessageInterface::ShowMessage
         ("Transpose::MatrixEvaluate() %s for %s\n", be.GetFullMessage().c_str(),
          GetName().c_str());
      throw;
   }
   
   //=======================================================
   #else
   //=======================================================
   
   return (leftNode->MatrixEvaluate()).Transpose();
   
   //=======================================================
   #endif
   //=======================================================
}


