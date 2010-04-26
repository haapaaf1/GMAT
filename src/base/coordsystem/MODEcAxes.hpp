//$Header$
//------------------------------------------------------------------------------
//                                  MODEcAxes
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under 
// MOMS Task order 124.
//
// Author: Wendy C. Shoan/GSFC/MAB
// Created: 2005/05/11
//
/**
 * Definition of the MODEcAxes class.
 *
 */
//------------------------------------------------------------------------------

#ifndef MODEcAxes_hpp
#define MODEcAxes_hpp

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "AxisSystem.hpp"
#include "MeanOfDateAxes.hpp"

class GMAT_API MODEcAxes : public MeanOfDateAxes
{
public:

   // default constructor
   MODEcAxes(const std::string &itsName = "");
   // copy constructor
   MODEcAxes(const MODEcAxes &mod);
   // operator = for assignment
   const MODEcAxes& operator=(const MODEcAxes &mod);
   // destructor
   virtual ~MODEcAxes();
   
   // method to initialize the data
   virtual bool Initialize();
   
   // all classes derived from GmatBase must supply this Clone method;
   // this must be implemented in the 'leaf' classes
   virtual GmatBase*       Clone() const;

   // Parameter access methods - overridden from GmatBase
   /* placeholder - may be needed later
   virtual std::string     GetParameterText(const Integer id) const;     
   virtual Integer         GetParameterID(const std::string &str) const; 
   virtual Gmat::ParameterType
                           GetParameterType(const Integer id) const;
   virtual std::string     GetParameterTypeString(const Integer id) const;
    */
   
protected:

   enum
   {
      MODEcAxesParamCount = MeanOfDateAxesParamCount,
   };
   
   //static const std::string PARAMETER_TEXT[MODEcAxesParamCount - 
   //                                        MeanOfDateAxesParamCount];
   
   //static const Gmat::ParameterType PARAMETER_TYPE[MODEcAxesParamCount - 
   //                                                MeanOfDateAxesParamCount];
   
   virtual void CalculateRotationMatrix(const A1Mjd &atEpoch,
                                        bool forceComputation = false);

   // no additional data at this time
};
#endif // MODEcAxes_hpp