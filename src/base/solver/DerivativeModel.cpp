//$Id$
/**
 * Base class for gradients, Jacobians, Hessians, and so forth.
 */

#include "DerivativeModel.hpp"

#include "SolverException.hpp"
#include "MessageInterface.hpp"


//#define DEBUG_DERIVMODEL


DerivativeModel::DerivativeModel() :
   calcMode          (FORWARD_DIFFERENCE),
   variableCount     (0)
{
}

DerivativeModel::~DerivativeModel()
{
}

DerivativeModel::DerivativeModel(const DerivativeModel& dm) :
   calcMode          (dm.calcMode),
   variableCount     (dm.variableCount),
   pert              (dm.pert),
   plusPertEffect    (dm.plusPertEffect),
   minusPertEffect   (dm.minusPertEffect)
{
   
}

DerivativeModel& DerivativeModel::operator=(const DerivativeModel& dm)
{
   if (&dm != this)
   {
      calcMode = dm.calcMode;
      variableCount = dm.variableCount;
      pert = dm.pert;
      plusPertEffect = dm.plusPertEffect;
      minusPertEffect = dm.minusPertEffect;
   }
   
   return *this;
}


void DerivativeModel::SetDifferenceMode(derivativeMode mode)
{
   calcMode = mode;
}


bool DerivativeModel::Initialize(UnsignedInt varCount, 
                                 UnsignedInt componentCount)
{
   UnsignedInt elementCount = varCount * componentCount;
   
   if (elementCount == 0)
      throw SolverException(
            "DerivativeModel cannot initialize because elementCount == 0");
   
   variableCount = varCount;
   pert.assign(varCount, 0.0);
   plusPertEffect.assign(elementCount, 0.0);
   minusPertEffect.assign(elementCount, 0.0);
   
   #ifdef DEBUG_DERIVMODEL
      MessageInterface::ShowMessage(
         "Derivative Model initialized in mode %d with %d elements\n", 
         calcMode, elementCount);
   #endif

   return true;
}


void DerivativeModel::Achieved(Integer pertNumber, Integer componentId, Real dx, 
                               Real value, bool plusEffect)
{
   if (pertNumber == -1)
      throw SolverException(
         "Setting a nominal value in the DerivativeModel base class Achieved() "
         "method is not allowed.");
   else
   {
      if (pertNumber >= (Integer)pert.size())
         throw SolverException(
               "Invalid pert element when setting an achieved value.");

      #ifdef DEBUG_DERIVMODEL
         MessageInterface::ShowMessage(
            "   %s perturbation #%d, size %.12lf gives %.12lf for id %d\n", 
            (plusEffect ? "Positive" : "Negative"), pertNumber, dx, value, 
            componentId);
      #endif
      
      pert[pertNumber] = dx;
      if (plusEffect)
         plusPertEffect[pertNumber + componentId * variableCount] = value;
      else
         minusPertEffect[pertNumber + componentId * variableCount] = value;
   }
}


