#include "Jacobian.hpp"

#include "SolverException.hpp"
#include "MessageInterface.hpp"


#define DEBUG_JACOBIAN

Jacobian::Jacobian() :
   DerivativeModel         (),
   numVariables            (0),
   numComponents           (0)
{
}

Jacobian::~Jacobian()
{
}

Jacobian::Jacobian(const Jacobian &jac) :
   DerivativeModel         (jac),
   numVariables            (jac.numVariables),
   numComponents           (jac.numComponents)
{
   
}

Jacobian& Jacobian::operator=(const Jacobian &jac)
{
   if (&jac != this)
   {
      
   }
   
   return *this;
}

bool Jacobian::Initialize(UnsignedInt varCount, UnsignedInt componentCount)
{
   numVariables = varCount;
   numComponents = componentCount;
   UnsignedInt elementCount = numVariables * numComponents;
   
   jacobian.assign(elementCount, 0.0);
   
   return true;
}

void Jacobian::Achieved(Integer pertNumber, Integer componentId, 
                        Real dx, Real value, bool plusEffect)
{
   
}

bool Jacobian::Calculate(std::vector<Real> &jac)
{
   if (calcMode == USER_SUPPLIED)
      return true;
 
   UnsignedInt pertSize = pert.size();
   UnsignedInt compSize = jacobian.size() / pertSize;
   for (UnsignedInt i = 0; i < pertSize; ++i)
   {
      if (pert[i] == 0.0)
         throw SolverException(
               "Perturbation of size 0.0 found in gradient calculation");

      #ifdef DEBUG_JACOBIAN
         MessageInterface::ShowMessage(
            "   Finding Gradient in mode %d\n", calcMode);
      #endif
         
      for (UnsignedInt j = 0; j < compSize; ++j)
      {
         UnsignedInt rowStart = j * compSize;
         switch (calcMode) 
         {
            case FORWARD_DIFFERENCE:
               jacobian[rowStart+i] = (plusPertEffect[rowStart+i] - nominal[i])/ 
                                       pert[i];
               break;
               
            case CENTRAL_DIFFERENCE:
//               jacobian[i] = (plusPertEffect[rowStart+i] - minusPertEffect[rowStart+i]) / 
//                             (2.0 * pert[i]);
//               break;
               
            case BACKWARD_DIFFERENCE:
//               jacobian[i] = (nominal - minusPertEffect[rowStart+i]) / pert[i];
//               break;
               
            default:
               throw SolverException(
                     "Gradient differencing mode is not available");
         }
      }
   }

   #ifdef DEBUG_JACOBIAN
      MessageInterface::ShowMessage("      Jacobian = \n");
      for (UnsignedInt i = 0; i < compSize; ++i)
      {
         MessageInterface::ShowMessage(
            "                 [");
         for (UnsignedInt j = 0; j < pertSize; ++j)
         {
            MessageInterface::ShowMessage("%.12lf", jacobian[i]);
            if (i < pertSize - 1)
               MessageInterface::ShowMessage(", ");
         }
         MessageInterface::ShowMessage("]\n");
      }
      MessageInterface::ShowMessage("\n");
   #endif

   jac = jacobian;
   return true;
}
