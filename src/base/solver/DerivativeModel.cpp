#include "DerivativeModel.hpp"

#include "SolverException.hpp"
#include "MessageInterface.hpp"


#define DEBUG_DERIVMODEL


DerivativeModel::DerivativeModel() :
   calcMode          (FORWARD_DIFFERENCE)
{
}

DerivativeModel::~DerivativeModel()
{
}

DerivativeModel::DerivativeModel(const DerivativeModel& dm) :
   calcMode          (dm.calcMode),
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
   
   pert.assign(elementCount, 0.0);
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
            "   Perturbation #%d, size %.12lf gives %.12lf\n", pertNumber, dx, 
            value);
      #endif
      
      pert[pertNumber] = dx;
      if (plusEffect)
         plusPertEffect[pertNumber] = value;
      else
         minusPertEffect[pertNumber] = value;
   }
}


