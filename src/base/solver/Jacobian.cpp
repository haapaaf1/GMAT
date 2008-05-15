#include "Jacobian.hpp"

#include "SolverException.hpp"
#include "MessageInterface.hpp"


// #define DEBUG_JACOBIAN
// #define DEBUG_JACOBIAN_DETAILS

Jacobian::Jacobian() :
   DerivativeModel         (),
   numComponents           (0)
{
}

Jacobian::~Jacobian()
{
}

Jacobian::Jacobian(const Jacobian &jac) :
   DerivativeModel         (jac),
   numComponents           (jac.numComponents)
{
   
}

Jacobian& Jacobian::operator=(const Jacobian &jac)
{
   if (&jac != this)
   {
      DerivativeModel::operator=(jac);
      
      numComponents = jac.numComponents;
      
      jacobian      = jac.jacobian;
      nominal       = jac.nominal;
   }
   
   return *this;
}

bool Jacobian::Initialize(UnsignedInt varCount, UnsignedInt componentCount)
{
   DerivativeModel::Initialize(varCount, componentCount);

   numComponents = componentCount;
   UnsignedInt elementCount = variableCount * numComponents;
   
   for (UnsignedInt i = 0; i < numComponents; ++i)
      nominal.push_back(0.0);
   
   for (UnsignedInt i = 0; i < elementCount; ++i)
      jacobian.push_back(0.0);
   
   #ifdef DEBUG_JACOBIAN
      MessageInterface::ShowMessage(
         "Jacobian initialized in mode %d with %d variables and %d components "
         "giving %d entries\n", calcMode, varCount, componentCount, 
         elementCount);
   #endif

   return true;
}

void Jacobian::Achieved(Integer pertNumber, Integer componentId, 
                        Real dx, Real value, bool plusEffect)
{
   if (pertNumber == -1)
   {
      #ifdef DEBUG_JACOBIAN
         MessageInterface::ShowMessage(
               "   Nominal data[%d], gives %.12lf\n", componentId, 
               value);
      #endif

      nominal.at(componentId) = value;
   }
   else
   {
      DerivativeModel::Achieved(pertNumber, componentId, dx, value, plusEffect);
   }
}

bool Jacobian::Calculate(std::vector<Real> &jac)
{
   if (calcMode == USER_SUPPLIED)
      return true;
 
   for (UnsignedInt i = 0; i < (UnsignedInt)variableCount; ++i)
   {
      if (pert[i] == 0.0)
         throw SolverException(
               "Perturbation of size 0.0 found in Jacobian calculation");

      #ifdef DEBUG_JACOBIAN
         MessageInterface::ShowMessage(
            "   Finding Jacobian in mode %d\n", calcMode);
      #endif
         
      for (UnsignedInt j = 0; j < numComponents; ++j)
      {
         UnsignedInt rowStart = j * variableCount;
         switch (calcMode) 
         {
            case FORWARD_DIFFERENCE:
               jacobian.at(rowStart+i) = (plusPertEffect.at(rowStart+i) - nominal.at(j))/ 
                                       pert.at(i);

               #ifdef DEBUG_JACOBIAN_DETAILS      
                  MessageInterface::ShowMessage(
                     "         FD[%d]: (%.12lf - %.12lf) / %.12lf = %.12lf\n", 
                     rowStart+i, plusPertEffect[rowStart+i], nominal[j], 
                     pert[i], jacobian[rowStart+i]);
               #endif

               break;
               
            case CENTRAL_DIFFERENCE:
               jacobian[rowStart+i] = (plusPertEffect[rowStart+i] - 
                             minusPertEffect[rowStart+i]) / 
                             (2.0 * pert[i]);

               #ifdef DEBUG_JACOBIAN_DETAILS      
                  MessageInterface::ShowMessage(
                     "         CD[%d]: (%.12lf - %.12lf) / 2 * %.12lf = %.12lf\n", 
                     rowStart+i, plusPertEffect[rowStart+i], 
                     minusPertEffect[rowStart+i], pert[i], 
                     jacobian[rowStart+i]);
               #endif
               
               break;
               
            case BACKWARD_DIFFERENCE:
               jacobian[rowStart+i] = (nominal[j] - 
                              minusPertEffect[rowStart+i]) / 
                              pert[i];

               #ifdef DEBUG_JACOBIAN_DETAILS      
                  MessageInterface::ShowMessage(
                     "         CD[%d]: (%.12lf - %.12lf) / %.12lf = %.12lf\n", 
                     rowStart+i, minusPertEffect[rowStart+i], 
                     nominal[j], pert[i], 
                     jacobian[rowStart+i]);
               #endif
               
               break;
               
            default:
               throw SolverException(
                     "Jacobian differencing mode is not available");
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
            MessageInterface::ShowMessage("%.12lf", jacobian[i * pertSize + j]);
            if (j < pertSize - 1)
               MessageInterface::ShowMessage(", ");
         }
         MessageInterface::ShowMessage("]\n");
      }
      MessageInterface::ShowMessage("\n");
   #endif

   jac = jacobian;
   return true;
}
