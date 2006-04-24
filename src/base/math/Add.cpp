//------------------------------------------------------------------------------
//                                  Add
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: LaMont Ruley
// Created: 2006/03/31
//
/**
 * Implements Add class.
 */
//------------------------------------------------------------------------------

#include "Add.hpp"
#include "MessageInterface.hpp"

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// Add()
//------------------------------------------------------------------------------
/**
 * Constructor.
 */
//------------------------------------------------------------------------------
Add::Add(const std::string &nomme)
   : MathFunction("Add", nomme)
{
}


//------------------------------------------------------------------------------
// ~Add()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
Add::~Add()
{
}


//------------------------------------------------------------------------------
//  Add(const Add &copy)
//------------------------------------------------------------------------------
/**
 * Constructs the Add object (copy constructor).
 * 
 * @param <copy> Object that is copied
 */
//------------------------------------------------------------------------------
Add::Add(const Add &copy) :
   MathFunction      (copy)
{
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Clone of the Add operation.
 *
 * @return clone of the Add operation.
 *
 */
//------------------------------------------------------------------------------
GmatBase* Add::Clone() const
{
   return (new Add(*this));
}


//------------------------------------------------------------------------------
// Real Evaluate()
//------------------------------------------------------------------------------
/**
 * @return the sum of left node(real) and right node(real)
 *
 */
//------------------------------------------------------------------------------
Real Add::Evaluate()
{
   return leftNode->Evaluate() + rightNode->Evaluate();
}


//------------------------------------------------------------------------------
// Rmatrix MatrixEvaluate()
//------------------------------------------------------------------------------
/**
 * @return the sum of left node(matrix) and right node(matrix)
 *
 */
//------------------------------------------------------------------------------
Rmatrix Add::MatrixEvaluate()
{
   return leftNode->MatrixEvaluate() + rightNode->MatrixEvaluate();
}

