//$Header$
//------------------------------------------------------------------------------
//                              ObserverObject
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2008/05/28
//
/**
 * Defines the base class used for ground station objects. 
 */
//------------------------------------------------------------------------------


#ifndef ObserverObject_hpp
#define ObserverObject_hpp

#include "GmatBase.hpp"
#include "SpacePoint.hpp"
#include "ObserverPropState.hpp"
#include "ObserverObjectException.hpp"

class GMAT_API ObserverObject : public SpacePoint
{
public:
   ObserverObject(Gmat::ObjectType typeId, const std::string &typeStr,
               const std::string &instName);
   virtual ~ObserverObject();
   ObserverObject(const ObserverObject& so);
   ObserverObject&         operator=(const ObserverObject& so);
   
   virtual PropState&   GetState();
   virtual Real         GetEpoch();
   virtual Real         SetEpoch(const Real ep);
   virtual bool         IsGroundBased();
   virtual bool         IsGroundBased(bool groundFlag);
   virtual bool         IsSpaceBased();
   virtual bool         IsSpaceBased(bool spaceFlag);
   virtual bool         IsShipBased();
   virtual bool         IsShipBased(bool shipFlag);
   virtual bool         IsAirplaneBased();
   virtual bool         IsAirplaneBased(bool planeFlag);
   virtual bool         IsManeuvering();
   virtual void         IsManeuvering(bool mnvrFlag);
   virtual bool         ParametersHaveChanged();
   virtual void         ParametersHaveChanged(bool flag);
   
   virtual Integer GetParameterID(const std::string &str) const;
   virtual Real GetRealParameter(const Integer id) const;
   virtual Real GetRealParameter(const std::string &label) const;
   virtual Real SetRealParameter(const Integer id, const Real value);
   virtual Real SetRealParameter(const std::string &label, const Real value);

   /// @todo Waiting for CoordinateSystems in Spacecraft, then see if needed
   virtual void SetOriginName(std::string cbName);
   virtual const std::string GetOriginName();
   virtual void SetOrigin(SpacePoint *cb);
   
   virtual const Rvector6 GetMJ2000State(const A1Mjd &atTime);
   virtual const Rvector3 GetMJ2000Position(const A1Mjd &atTime);
   virtual const Rvector3 GetMJ2000Velocity(const A1Mjd &atTime);
   
   virtual std::string GetParameterText(const Integer id) const;
   virtual Gmat::ParameterType
                       GetParameterType(const Integer id) const;
   virtual std::string GetParameterTypeString(const Integer id) const;

  //virtual void ClearLastStopTriggered();
  //virtual void SetLastStopTriggered(const std::string &stopCondName);
  //virtual bool WasLastStopTriggered(const std::string &stopCondName);
   
protected:
   /// The Observer state
   ObserverPropState         state;
   /// true when the observer is located on the surface of the Earth
   bool                  isGroundBased;
   /// true when the observer is located on a space object
   bool                  isSpaceBased;
   /// true when the observer is located on a ship
   bool                  isShipBased;
   /// true when the observer is located on an airplane
   bool                  isAirplaneBased;
   /// true when the space object that the Observer is attached to is maneuvering
   bool                  isManeuvering;
   /// Reference SpacePoint for the data
   std::string       originName;
   /// Reference SpacePoint for the data
   SpacePoint        *origin;
   /// Flag indicating if the force model parms have changed
   bool              parmsChanged;
   /// The names of the last set of stopping conditions met
   //StringArray       lastStopTriggered;

   /// Enumerated parameter IDs   
   enum
   {
       EPOCH_PARAM = SpacePointParamCount,
       ObserverObjectParamCount
   };
   /// Array of supported parameters
   static const std::string PARAMETER_TEXT[ObserverObjectParamCount - 
                                           SpacePointParamCount];
   /// Array of parameter types
   static const Gmat::ParameterType PARAMETER_TYPE[ObserverObjectParamCount - 
                                                   SpacePointParamCount];
   
};

#endif // ObserverObject_hpp
