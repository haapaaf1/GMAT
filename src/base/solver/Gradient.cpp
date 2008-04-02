//$Id$
//------------------------------------------------------------------------------
//                         Optimizer
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway/Thinking Systems, Inc.
// Created: 2008.03.26
//
/**
 * Cladsss used to calculate gradients.  
 * 
 * Currently only supports finite differences using forward differencing.
 */

#include "Gradient.hpp"

#include "SolverException.hpp"
#include "MessageInterface.hpp"


// #define DEBUG_GRADIENT

Gradient::Gradient() : 
   DerivativeModel   (),
   nominal           (9876.54321)
{
}

Gradient::~Gradient()
{
}

Gradient::Gradient(const Gradient &grad) : 
   DerivativeModel   (grad),
   nominal           (grad.nominal),
   gradient          (grad.gradient)
{
}

Gradient& Gradient::operator=(const Gradient &grad)
{
   if (&grad != this)
   {
      DerivativeModel::operator=(grad);
      nominal = grad.nominal;
      gradient = grad.gradient;
      calcMode = grad.calcMode;
   }
   
   return *this;
}

bool Gradient::Initialize(UnsignedInt varCount, UnsignedInt componentCount)
{
   DerivativeModel::Initialize(varCount);
   gradient.assign(varCount, 0.0);
   
   #ifdef DEBUG_GRADIENT
      MessageInterface::ShowMessage(
         "Gradient initialized in mode %d with %d variables\n", calcMode, 
         varCount);
   #endif

   return true;
}


void Gradient::Achieved(Integer pertNumber, Integer componentId, Real dx, 
                        Real value, bool plusEffect)
{
   if (pertNumber == -1)
   {
      #ifdef DEBUG_GRADIENT
         MessageInterface::ShowMessage(
            "Setting Gradient Nominal Value to %.12lf\n", value);
      #endif
      nominal = value;
   }
   else
   {
      DerivativeModel::Achieved(pertNumber, componentId, dx, value, plusEffect);
   }
}


bool Gradient::Calculate(std::vector<Real> &grad)
{
   if (calcMode == USER_SUPPLIED)
      return true;
 
   UnsignedInt gradSize = pert.size();
   for (UnsignedInt i = 0; i < gradSize; ++i)
   {
      if (pert[i] == 0.0)
         throw SolverException(
               "Perturbation of size 0.0 found in gradient calculation");

      #ifdef DEBUG_GRADIENT
         MessageInterface::ShowMessage(
            "   Finding Gradient in mode %d\n", calcMode);
      #endif

      switch (calcMode) 
      {
         case FORWARD_DIFFERENCE:
            gradient[i] = (plusPertEffect[i] - nominal) / pert[i];
            break;
            
         case CENTRAL_DIFFERENCE:
//            gradient[i] = (plusPertEffect[i] - minusPertEffect[i]) / 
//                          (2.0 * pert[i]);
//            break;
            
         case BACKWARD_DIFFERENCE:
//            gradient[i] = (nominal - minusPertEffect[i]) / pert[i];
//            break;
            
         default:
            throw SolverException(
                  "Gradient differencing mode is not available");
      }
   }

   #ifdef DEBUG_GRADIENT
      MessageInterface::ShowMessage(
         "      Gradient = [");
      for (UnsignedInt i = 0; i < gradSize; ++i)
      {
         MessageInterface::ShowMessage("%.12lf", gradient[i]);
         if (i < gradSize - 1)
            MessageInterface::ShowMessage(", ");
      }
      MessageInterface::ShowMessage("]\n");
   #endif

   grad = gradient;
   return true;
}
