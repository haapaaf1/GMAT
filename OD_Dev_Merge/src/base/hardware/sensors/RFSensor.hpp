//$Header$
//------------------------------------------------------------------------------
//                             RFSensor
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/12/15
//
// Based in part upon elements of the Thruster class.
//
/**
 *
 * Implements the RFSensor base class to provide for modeling of various
 * RF capabilities like receivers, transmitters, transceivers, emitters, etc
 *
 */
//------------------------------------------------------------------------------

#ifndef _RFSENSOR_HPP
#define	_RFSENSOR_HPP

#include "Sensor.hpp"

class GMAT_API RFSensor : public Sensor
{
public:

    RFSensor(std::string nomme);
    virtual ~RFSensor();
    RFSensor(const RFSensor &rf);
    RFSensor&            operator=(const RFSensor &rf);

    // required method for all subclasses
    virtual GmatBase*    Clone() const;
    virtual void         Copy(const GmatBase* inst);

    // Parameter access methods - overridden from GmatBase
    virtual std::string  GetParameterText(const Integer id) const;
    virtual Integer      GetParameterID(const std::string &str) const;
    virtual Gmat::ParameterType
                         GetParameterType(const Integer id) const;
    virtual std::string  GetParameterTypeString(const Integer id) const;
    virtual bool         IsParameterReadOnly(const Integer id) const;

    virtual Real         GetRealParameter(const Integer id) const;
    virtual Real         SetRealParameter(const Integer id,
                                          const Real value);
    virtual std::string  GetStringParameter(const Integer id) const;
    virtual bool         SetStringParameter(const Integer id,
                                            const std::string &value);
    virtual bool         GetBooleanParameter(const Integer id) const;
    virtual bool         SetBooleanParameter(const Integer id,
                                             const bool value);

    virtual const StringArray&
                         GetStringArrayParameter(const Integer id) const;

    virtual bool         Initialize();

protected:

    /// Flag used to determine if the configuration needs updating
    bool                       initialized;

    // RFSensor ID for identification purposes
    Integer                     RFSensorID;

    // Minimum and maximum range for this RFSensor
    Real                        maxRange;
    Real                        minRange;

    // Minimum and maximum range rate for this RFSensor
    Real                        maxRangeRate;
    Real                        minRangeRate;

    // The bore sight vector indication where the RFSensor is pointed in
    // the local coordinate frame
    Real                        boreSightVector[3];

    // Minimum and maximum co-elevation for this RFSensor
    Real                        maxCoElevation;
    Real                        minCoElevation;

   /// Published parameters for RFSensors
   enum
   {
      RFSENSOR_ACTIVE = HardwareParamCount,
      RFSENSOR_ID,
      MAX_RANGE,
      MIN_RANGE,
      MAX_RANGERATE,
      MIN_RANGERATE,
      BORESIGHTVECTOR_1,
      BORESIGHTVECTOR_2,
      BORESIGHTVECTOR_3,
      MIN_COELEVATION,
      MAX_COELEVATION,
      RFSensorParamCount
   };

   /// RFSensor parameter labels
   static const std::string
                        PARAMETER_TEXT[RFSensorParamCount - HardwareParamCount];
   /// RFSensor parameter types
   static const Gmat::ParameterType
                        PARAMETER_TYPE[RFSensorParamCount - HardwareParamCount];

};

#endif	/* _RFSENSOR_HPP */

