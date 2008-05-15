//$Id: Gradient.cpp,v 1.2 2008/05/15 21:26:13 djc Exp $
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
 * Class used to calculate gradients.  
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
   
   gradient.clear();   
   for (UnsignedInt i = 0; i < varCount; ++i)
      gradient.push_back(0.0);

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
      #ifdef DEBUG_GRADIENT
            MessageInterface::ShowMessage(
                  "   Component %d of %d, pert = %.12lf\n", i, gradSize, 
                  pert.at(i));
      #endif   

            if (pert.at(i) == 0.0)
         throw SolverException(
               "Perturbation of size 0.0 found in gradient calculation");

      #ifdef DEBUG_GRADIENT
         MessageInterface::ShowMessage(
            "   Finding Gradient in mode %d\n", calcMode);
      #endif

      switch (calcMode) 
      {
         case FORWARD_DIFFERENCE:
            #ifdef DEBUG_GRADIENT
            MessageInterface::ShowMessage("   FD[%d]:  %.15lf - %.15lf / %.15lf\n", 
                  i, plusPertEffect[i], nominal, pert[i]);
            #endif

            gradient.at(i) = (plusPertEffect.at(i) - nominal) / pert.at(i);
            break;
            
         case CENTRAL_DIFFERENCE:
            #ifdef DEBUG_GRADIENT
            MessageInterface::ShowMessage("   CD[%d]:  %.15lf - %.15lf / 2 * %.15lf\n", 
                  i, plusPertEffect[i], minusPertEffect[i], pert[i]);
            #endif

            gradient[i] = (plusPertEffect[i] - minusPertEffect[i]) / 
                          (2.0 * pert[i]);
            break;
            
         case BACKWARD_DIFFERENCE:
            #ifdef DEBUG_GRADIENT
            MessageInterface::ShowMessage("   BD[%d]:  %.15lf - %.15lf / %.15lf\n", 
                  i, nominal, minusPertEffect[i], pert[i]);
            #endif

            gradient[i] = (nominal - minusPertEffect[i]) / pert[i];
            break;
            
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
