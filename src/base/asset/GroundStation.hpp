//$Id$
//------------------------------------------------------------------------------
//                            GroundStation
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/08/01
// Modified: 
//    2010.03.15 Thomas Grubb 
//      - Changed visiblity of PARAMETER_TEXT, PARAMETER_TYPE, and enum from
//        protected to public
//      - Overrode Copy method
//
/**
 * Defines the Groundstation class used to model ground based tracking stations.
 */
//------------------------------------------------------------------------------

#ifndef GroundStation_hpp
#define GroundStation_hpp

#include "SpacePoint.hpp"
#include "BodyFixedPoint.hpp"
#include "LatLonHgt.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateConverter.hpp"


class GroundStation : public BodyFixedPoint
{
public:
   GroundStation(const std::string &itsName);
   virtual ~GroundStation();
   GroundStation(const GroundStation& gs);
   GroundStation& operator=(const GroundStation& gs);

   // all leaf classes derived from GmatBase must supply this Clone method
   virtual GmatBase*       Clone() const;
   virtual void            Copy(const GmatBase* orig);

   // Access methods derived classes can override
   virtual std::string  GetParameterText(const Integer id) const;
   virtual std::string  GetParameterUnit(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   virtual bool         IsParameterReadOnly(const Integer id) const;
   virtual bool         IsParameterReadOnly(const std::string &label) const;

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

   virtual bool         Initialize();

//   virtual Integer         GetEstimationParameterID(const std::string &param);
//   virtual Integer         SetEstimationParameter(const std::string &param);
   virtual bool            IsEstimationParameterValid(const Integer id);
   virtual Integer         GetEstimationParameterSize(const Integer id);
   virtual Real*           GetEstimationParameterValue(const Integer id);
   
protected:
   /// Ground station ID
   std::string          stationId;
   
   // Override GetGenString to handle the changeable names for the parameters
   virtual const std::string&  
                        GetGeneratingString(
                           Gmat::WriteMode mode = Gmat::SCRIPTING,
                           const std::string &prefix = "",
                           const std::string &useName = "");
   virtual void         WriteParameters(Gmat::WriteMode mode, 
                           std::string &prefix, std::stringstream &stream);

public:
   /// Published parameters for ground stations
   enum
   {
      STATION_ID = BodyFixedPointParamCount,
      GroundStationParamCount,
   };
   
   static const std::string
      PARAMETER_TEXT[GroundStationParamCount - BodyFixedPointParamCount];
   static const Gmat::ParameterType
      PARAMETER_TYPE[GroundStationParamCount - BodyFixedPointParamCount];

};

#endif /*GroundStation_hpp*/
