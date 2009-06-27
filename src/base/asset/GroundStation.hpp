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
// For matrix and vector definitions
#include "lapackpp.h"

#include "MeasurementModel.hpp"
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

   virtual GmatBase*   GetRefObject(const Gmat::ObjectType type,
                                    const std::string &name);
   virtual bool        SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                    const std::string &name = "");


   void SetMeasurementModel(MeasurementModel* mm);
   MeasurementModel* GetMeasurementModel();

  // This function takes a spacecraft and computes a measurement
  // between the associated ground station a space point
  bool GetTheMeasurements(SpacePoint* theSpacePoint,
                          const A1Mjd &atTime,
                          LaGenMatDouble &theMeasurements);

  // Obtain the partials
  bool GetThePartials(const std::string &param,
                      SpacePoint* theSpacePoint,
                      const A1Mjd &atTime,
                      LaGenMatDouble &theDerivatives);
  bool GetThePartials(const Integer &paramID,
                      SpacePoint* theSpacePoint,
                      const A1Mjd &atTime,
                      LaGenMatDouble &theDerivatives);

   void SetSpinRate(Real &sr);
   Real GetSpinRate();

   void SetEquatorialRadius(Real &er);
   Real GetEquatorialRadius();

   void SetFlattening(Real &flat);
   Real GetFlattening();

   void SetBody(CelestialBody &body);
   CelestialBody* GetBody();
   
protected:
   
   /// Published parameters for ground stations
   enum
   {
      MEASUREMENTMODEL = BodyFixedPointParamCount,
      GroundStationParamCount
   };
   
   static const std::string
        PARAMETER_TEXT[GroundStationParamCount - BodyFixedPointParamCount];
   static const Gmat::ParameterType 
        PARAMETER_TYPE[GroundStationParamCount - BodyFixedPointParamCount];

   // Override GetGenString to handle the changeable names for the parameters
   virtual const std::string&  
                        GetGeneratingString(
                           Gmat::WriteMode mode = Gmat::SCRIPTING,
                           const std::string &prefix = "",
                           const std::string &useName = "");
   virtual void         WriteParameters(Gmat::WriteMode mode, 
                           std::string &prefix, std::stringstream &stream);

   MeasurementModel  *measModel;
   Real equatorialRadius;
   Real flattening;
   Real bodySpinRate;
   
};

#endif /*GroundStation_hpp*/
