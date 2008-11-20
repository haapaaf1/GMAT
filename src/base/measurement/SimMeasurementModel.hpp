//$Header$
//------------------------------------------------------------------------------
//                         SimMeasurementModel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/07/23
//
/**
 * Defines the simulated measurement models used for observer objects.
 */
//------------------------------------------------------------------------------
#ifndef _SIMMEASUREMENTMODEL_HPP
#define	_SIMMEASUREMENTMODEL_HPP

#include "GmatBase.hpp"
#include "MeasurementModelException.hpp"

class GMAT_API SimMeasurementModel : public MeasurementModel
{

public:

  SimMeasurementModel(const std::string typeName, const std::string name = "");
  SimMeasurementModel(const SimMeasurementModel &SimMeasurementModel);
  SimMeasurementModel& operator=(const SimMeasurementModel &SimMeasurementModel);
  virtual ~SimMeasurementModel();
  
  // Friend function
  friend std::ostream& operator<<(std::ostream& output, SimMeasurementModel &mm);
  friend std::istream& operator>>(std::istream& input, SimMeasurementModel &mm);

  // Methods overridden from the GmatBase clase
  virtual GmatBase *Clone() const;
  virtual void      Copy(const GmatBase* orig);      

  
  virtual GmatBase* 
                    GetRefObject(const Gmat::ObjectType type,
				 const std::string &name);
  virtual const StringArray&
                   GetRefObjectNameArray(const Gmat::ObjectType type);
  virtual bool     SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
     
  virtual void Initialize() const;
  
  const std::string* GetModelDescriptions() const;
  std::string GetModelNameText(const Integer &id) const;
  Integer GetModelID(const std::string &label);

  // Compute measurements
  virtual bool ComputeMeasurement(ObjectArray participants, 
				  LaVectorDouble &myMeasurements);
  virtual bool ComputeMeasurement(GroundStation* theStation,
        Spacecraft* theSat, LaVectorDouble &myMeasurements);
 
  // Compute partial derivatives
  virtual bool ComputeCartesianPartialDerivative(ObjectArray participants, 
				  LaGenMatDouble &myMeasurements);

  virtual bool ComputeCartesianPartialDerivative(
        GroundStation* theStation, Spacecraft* theSat,
        LaGenMatDouble &myCartDerivatives);
   
private:

  static const Integer NUM_MODELS = 21;
  static const std::string MODEL_DESCRIPTIONS[NUM_MODELS];

protected:
   
   enum
   {
      FILENAME_ID   = MeasurementModelParamCount,
      FILEFORMAT_ID,
      DATATYPESALLOWED_ID,
      NUMLINES_ID,
      SimMeasurementModelParamCount
   };

   static const std::string    PARAMETER_TEXT[SimMeasurementModelParamCount -
                                              MeasurementModelParamCount];
   static const Gmat::ParameterType
                               PARAMETER_TYPE[SimMeasurementModelParamCount -
                                              MeasurementModelParamCount];

};


#endif	/* _SIMMEASUREMENTMODEL_HPP */

