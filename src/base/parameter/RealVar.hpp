//$Header$
//------------------------------------------------------------------------------
//                                  RealVar
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2004/01/08
//
/**
 * Declares base class of parameters returning Real.
 */
//------------------------------------------------------------------------------
#ifndef RealVar_hpp
#define RealVar_hpp

#include "gmatdefs.hpp"
#include "Parameter.hpp"

class GMAT_API RealVar : public Parameter
{
public:

   RealVar(const std::string &name = "", const std::string &valStr = "",
           const std::string &typeStr = "RealVar",
           GmatParam::ParameterKey key = GmatParam::USER_PARAM,
           GmatBase *obj = NULL, const std::string &desc = "",
           const std::string &unit = "",
           GmatParam::DepObject depObj = GmatParam::NO_DEP,
           Gmat::ObjectType ownerType = Gmat::UNKNOWN_OBJECT,
           bool isTimeParam = false);
   RealVar(const RealVar &copy);
   RealVar& operator= (const RealVar& right);
   virtual ~RealVar();

   bool operator==(const RealVar &right) const;
   bool operator!=(const RealVar &right) const;

   // methods inherited from Parameter
   virtual bool Initialize();
   virtual std::string ToString();
   
   virtual Real GetReal() const;
   virtual void SetReal(Real val);
   
   virtual bool SetStringParameter(const Integer id, const std::string &value);
   virtual bool SetStringParameter(const std::string &label,
                                   const std::string &value);
protected:

   Real mRealValue;
       
private:

};
#endif // RealVar_hpp
