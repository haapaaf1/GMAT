//$Id$
//------------------------------------------------------------------------------
//                             EphemerisPropagator
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: Mar 26, 2010 by Darrel Conway (Thinking Systems)
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under the FDSS 
// contract, Task 28
//
/**
 * Implementation for the EphemerisPropagator class
 */
//------------------------------------------------------------------------------


#include "EphemerisPropagator.hpp"

#include "MessageInterface.hpp"


/// EphemerisPropagator parameter labels
const std::string
EphemerisPropagator::PARAMETER_TEXT[EphemerisPropagatorParamCount - PropagatorParamCount] =
{
      "StepSize",       // EPHEM_STEP_SIZE
      "CentralBody",    // EPHEM_CENTRAL_BODY
      "EpochFormat",    // EPHEM_EPOCH_FORMAT
      "StartEpoch"      // EPHEM_START_EPOCH
};

/// EphemerisPropagator parameter types
const Gmat::ParameterType
EphemerisPropagator::PARAMETER_TYPE[EphemerisPropagatorParamCount - PropagatorParamCount] =
{
      Gmat::REAL_TYPE,        // EPHEM_STEP_SIZE
      Gmat::OBJECT_TYPE,      // EPHEM_CENTRAL_BODY
      Gmat::STRING_TYPE,      // EPHEM_EPOCH_FORMAT
      Gmat::STRING_TYPE       // EPHEM_START_EPOCH
};


EphemerisPropagator::EphemerisPropagator(const std::string & typeStr,
      const std::string & name) :
   Propagator           (typeStr, name)
{
   parameterCount = EphemerisPropagatorParamCount;
}


EphemerisPropagator::~EphemerisPropagator()
{
}


EphemerisPropagator::EphemerisPropagator(const EphemerisPropagator & ep) :
   Propagator           (ep)
{
}


EphemerisPropagator& EphemerisPropagator::operator=(
      const EphemerisPropagator& ep)
{
   if (this != &ep)
   {
      Propagator::operator=(ep);
   }

   return *this;
}


std::string EphemerisPropagator::GetParameterText(const Integer id) const
{
   if (id >= PropagatorParamCount && id < EphemerisPropagatorParamCount)
      return PARAMETER_TEXT[id - PropagatorParamCount];
   return Propagator::GetParameterText(id);
}


std::string EphemerisPropagator::GetParameterUnit(const Integer id) const
{
   return Propagator::GetParameterUnit(id);
}


Integer EphemerisPropagator::GetParameterID(const std::string &str) const
{
   for (Integer i = PropagatorParamCount;
         i < EphemerisPropagatorParamCount; ++i)
   {
      if (str == PARAMETER_TEXT[i - PropagatorParamCount])
         return i;
   }

   return Propagator::GetParameterID(str);
}


Gmat::ParameterType EphemerisPropagator::GetParameterType(
      const Integer id) const
{
   if (id >= PropagatorParamCount && id < EphemerisPropagatorParamCount)
      return PARAMETER_TYPE[id - PropagatorParamCount];
   return Propagator::GetParameterType(id);
}


std::string EphemerisPropagator::GetParameterTypeString(const Integer id) const
{
   if (id >= PropagatorParamCount && id < EphemerisPropagatorParamCount)
      return Propagator::PARAM_TYPE_STRING[GetParameterType(id)];
   return Propagator::GetParameterTypeString(id);
}


bool EphemerisPropagator::IsParameterReadOnly(const Integer id) const
{
   if (id == INITIAL_STEP_SIZE)
      return true;
   return Propagator::IsParameterReadOnly(id);
}


bool EphemerisPropagator::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


Real EphemerisPropagator::GetRealParameter(const Integer id) const
{
   if (id == EPHEM_STEP_SIZE)
   {
      return ephemStep;
   }

   return Propagator::GetRealParameter(id);
}


Real EphemerisPropagator::SetRealParameter(const Integer id, const Real value)
{
   if (id == EPHEM_STEP_SIZE)
   {
      if (value != 0.0)
         ephemStep = value;
      return ephemStep;
   }

   return Propagator::SetRealParameter(id, value);
}


Real EphemerisPropagator::GetRealParameter(const Integer id,
      const Integer index) const
{
   return Propagator::GetRealParameter(id, index);
}


Real EphemerisPropagator::GetRealParameter(const Integer id, const Integer row,
      const Integer col) const
{
   return Propagator::GetRealParameter(id, row, col);
}


Real EphemerisPropagator::SetRealParameter(const Integer id, const Real value,
      const Integer index)
{
   return Propagator::SetRealParameter(id, value, index);
}


Real EphemerisPropagator::SetRealParameter(const Integer id, const Real value,
      const Integer row, const Integer col)
{
   return Propagator::SetRealParameter(id, value, row, col);
}


Real EphemerisPropagator::GetRealParameter(const std::string &label) const
{
   return GetRealParameter(GetParameterID(label));
}


Real EphemerisPropagator::SetRealParameter(const std::string &label,
      const Real value)
{
   return SetRealParameter(GetParameterID(label), value);
}


Real EphemerisPropagator::GetRealParameter(const std::string &label,
      const Integer index) const
{
   return GetRealParameter(GetParameterID(label), index);
}


Real EphemerisPropagator::SetRealParameter(const std::string &label,
      const Real value, const Integer index)
{
   return SetRealParameter(GetParameterID(label), value, index);
}


Real EphemerisPropagator::GetRealParameter(const std::string &label,
      const Integer row, const Integer col) const
{
   return GetRealParameter(GetParameterID(label), row, col);
}


Real EphemerisPropagator::SetRealParameter(const std::string &label,
      const Real value, const Integer row, const Integer col)
{
   return SetRealParameter(GetParameterID(label), value, row, col);
}


std::string  EphemerisPropagator::GetStringParameter(const Integer id) const
{
   switch (id)
   {
      case EPHEM_CENTRAL_BODY:
         return centralBody;

      case EPHEM_EPOCH_FORMAT:
         return epochFormat;

      case EPHEM_START_EPOCH:
         return startEpoch;

      default:
         break;
   }

   return Propagator::GetStringParameter(id);
}


bool EphemerisPropagator::SetStringParameter(const Integer id,
      const std::string &value)
{
   switch (id)
   {
      case EPHEM_CENTRAL_BODY:
         centralBody = value;
         return true;

      case EPHEM_EPOCH_FORMAT:
         epochFormat = value;
         return true;

      case EPHEM_START_EPOCH:
         startEpoch = value;
         return true;

      default:
         break;
   }

   return Propagator::SetStringParameter(id, value);
}


std::string EphemerisPropagator::GetStringParameter(const Integer id,
      const Integer index) const
{
   return Propagator::GetStringParameter(id, index);
}


bool EphemerisPropagator::SetStringParameter(const Integer id,
      const std::string &value, const Integer index)
{
   return Propagator::SetStringParameter(id, value, index);
}


std::string EphemerisPropagator::GetStringParameter(
      const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


bool EphemerisPropagator::SetStringParameter(const std::string &label,
      const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


std::string EphemerisPropagator::GetStringParameter(const std::string &label,
      const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}


bool EphemerisPropagator::SetStringParameter(const std::string &label,
      const std::string &value, const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


//------------------------------------------------------------------------------
// bool UsesODEModel()
//------------------------------------------------------------------------------
/**
 * Used to tell the PropSetup if an ODE model is needed for the propagator
 *
 * @return true if an ODEModel is required, false if not
 */
//------------------------------------------------------------------------------
bool EphemerisPropagator::UsesODEModel()
{
   return false;
}

