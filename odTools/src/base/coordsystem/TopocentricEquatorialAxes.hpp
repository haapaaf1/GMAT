//$Header$
//------------------------------------------------------------------------------
//                                  TopocentricEquatorialAxes
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/30
//
/**
 * Definition of the TopocentricEquatorialAxes class.
 *
 * @note There are three data files currently needed:
 *    EOP file containing polar motion (x,y) and UT1-UTC offset
 *    coefficient file containing nutation and planetary coeffifients
 */
//------------------------------------------------------------------------------

#ifndef TopocentricEquatorialAxes_hpp
#define TopocentricEquatorialAxes_hpp

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "AxisSystem.hpp"
#include "DynamicAxes.hpp"
#include "EopFile.hpp"
#include "DeFile.hpp"
#include "ItrfCoefficientsFile.hpp"

class GMAT_API TopocentricEquatorialAxes : public DynamicAxes
{
public:

   // default constructor
   TopocentricEquatorialAxes(const std::string &itsName = "");
   // copy constructor
   TopocentricEquatorialAxes(const TopocentricEquatorialAxes &teaxes);
   // operator = for assignment
   const TopocentricEquatorialAxes& operator=(const TopocentricEquatorialAxes &teaxes);
   // destructor
   virtual ~TopocentricEquatorialAxes();
   
   // methods to set files for reading
   // 3 are needed:
   //    leap second file (NOTE - this should be done in the utiltities!!)
   //    EOP file containing polar motion (x,y) and UT1-UTC offset
   //    coefficient file containing nutation and planetary coeffifients
   
   virtual GmatCoordinate::ParameterUsage UsesEopFile() const;
   virtual GmatCoordinate::ParameterUsage UsesItrfFile() const;
   virtual GmatCoordinate::ParameterUsage UsesNutationUpdateInterval() const;
   
// method to initialize the data
   virtual bool Initialize();
   
   // all classes derived from GmatBase must supply this Clone method;
   // this must be implemented in the 'leaf' classes
   virtual GmatBase*       Clone(void) const;

   // Parameter access methods - overridden from GmatBase
   /*
   virtual std::string     GetParameterText(const Integer id) const;     
   virtual Integer         GetParameterID(const std::string &str) const; 
   virtual Gmat::ParameterType
                           GetParameterType(const Integer id) const;
   virtual std::string     GetParameterTypeString(const Integer id) const;
   virtual std::string     GetStringParameter(const Integer id) const;
   virtual bool            SetStringParameter(const Integer id, 
                                              const std::string &value);
   virtual std::string     GetStringParameter(const std::string &label) const;
   virtual bool            SetStringParameter(const std::string &label, 
                                              const std::string &value);
    */
   
   
protected:

   enum
   {
      TopocentricEquatorialAxesParamCount = DynamicAxesParamCount,
   };
   
   //static const std::string PARAMETER_TEXT[TopocentricEquatorialAxesParamCount - 
   //                                        DynamicAxesParamCount];
   
   //static const Gmat::ParameterType PARAMETER_TYPE[TopocentricEquatorialAxesParamCount - 
   //                                                DynamicAxesParamCount];
   
   virtual void CalculateRotationMatrix(const A1Mjd &atEpoch,
                                        bool forceComputation = false);
   
   //Rmatrix33      precT, nutT, stT, stDerivT, pmT;
   
   //const Real *precData;  // moved to AxisSystem
   //const Real *nutData;
   //const Real *stData;
   //const Real *stDerivData;
   //const Real *pmData;
   
   DeFile     *de;
};
#endif // TopocentricEquatorialAxes_hpp
