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


#ifndef EphemerisPropagator_hpp
#define EphemerisPropagator_hpp

/**
 * EphemerisPropagator ...
 */
#include "Propagator.hpp"

class EphemerisPropagator : public Propagator
{
public:
   EphemerisPropagator(const std::string &typeStr, const std::string &name = "");
   virtual ~EphemerisPropagator();
   EphemerisPropagator(const EphemerisPropagator& ep);
   EphemerisPropagator& operator=(const EphemerisPropagator& ep);

   // Access methods for the scriptable parameters
   virtual std::string  GetParameterText(const Integer id) const;
   virtual std::string  GetParameterUnit(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   // Access methods for the scriptable parameters
   virtual bool         IsParameterReadOnly(const Integer id) const;
   virtual bool         IsParameterReadOnly(const std::string &label) const;

   virtual Real         GetRealParameter(const Integer id) const;
   virtual Real         SetRealParameter(const Integer id,
                                         const Real value);
   virtual Real         GetRealParameter(const Integer id,
                                         const Integer index) const;
   virtual Real         GetRealParameter(const Integer id, const Integer row,
                                         const Integer col) const;
   virtual Real         SetRealParameter(const Integer id,
                                         const Real value,
                                         const Integer index);
   virtual Real         SetRealParameter(const Integer id, const Real value,
                                         const Integer row, const Integer col);
   virtual Real         GetRealParameter(const std::string &label) const;
   virtual Real         SetRealParameter(const std::string &label,
                                         const Real value);
   virtual Real         GetRealParameter(const std::string &label,
                                         const Integer index) const;
   virtual Real         SetRealParameter(const std::string &label,
                                         const Real value,
                                         const Integer index);
   virtual Real         GetRealParameter(const std::string &label,
                                         const Integer row,
                                         const Integer col) const;
   virtual Real         SetRealParameter(const std::string &label,
                                         const Real value, const Integer row,
                                         const Integer col);


   virtual std::string  GetStringParameter(const Integer id) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const Integer id,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index);
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const std::string &label,
                                           const Integer index) const;
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value,
                                           const Integer index);


protected:
   /// Step used to propagate through the ephemeris
   Real                 ephemStep;
   /// Name of the central body
   std::string          centralBody;
   /// Format used for the start epoch data
   std::string          epochFormat;
   /// Start epoch
   std::string          startEpoch;

   /// Parameter IDs
   enum
   {
      EPHEM_STEP_SIZE = PropagatorParamCount,
      EPHEM_CENTRAL_BODY,
      EPHEM_EPOCH_FORMAT,
      EPHEM_START_EPOCH,
      EphemerisPropagatorParamCount
   };

   /// EphemerisPropagator parameter types
   static const Gmat::ParameterType
         PARAMETER_TYPE[EphemerisPropagatorParamCount - PropagatorParamCount];
   /// EphemerisPropagator parameter labels
   static const std::string
         PARAMETER_TEXT[EphemerisPropagatorParamCount - PropagatorParamCount];

};

#endif /* EphemerisPropagator_hpp */
