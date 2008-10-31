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


// Forward reference
class MeasurementModel;


class GroundStation : public BodyFixedPoint
{
public:
   GroundStation(const std::string &itsName);
   virtual ~GroundStation();
   GroundStation(const GroundStation& gs);
   GroundStation& operator=(const GroundStation& gs);

   // all leaf classes derived from GmatBase must supply this Clone method
   virtual GmatBase*       Clone() const;


   virtual bool            Initialize();
   
   void SetMeasurementModel(MeasurementModel* mm);
   MeasurementModel* GetMeasurementModel();
   
protected:
   
   /// Published parameters for ground stations
   enum
   {
      GroundStationParamCount = SpacePointParamCount,
   };
   
//   static const std::string 
//      PARAMETER_TEXT[GroundStationParamCount - SpacePointParamCount];
   /// burn parameter types
//   static const Gmat::ParameterType 
//      PARAMETER_TYPE[GroundStationParamCount - SpacePointParamCount];

   // Override GetGenString to handle the changeable names for the parameters
   virtual const std::string&  
                        GetGeneratingString(
                           Gmat::WriteMode mode = Gmat::SCRIPTING,
                           const std::string &prefix = "",
                           const std::string &useName = "");
   virtual void         WriteParameters(Gmat::WriteMode mode, 
                           std::string &prefix, std::stringstream &stream);
   
   MeasurementModel  *measModel;
   
};

#endif /*GroundStation_hpp*/
