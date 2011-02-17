//$Id$
//------------------------------------------------------------------------------
//                                   Burn
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2003/12/17
//
/**
 * Defines the Burn base class used for impulsive and finite maneuvers. 
 */
//------------------------------------------------------------------------------


#ifndef Burn_hpp
#define Burn_hpp


#include "GmatBase.hpp"
#include "Spacecraft.hpp"
#include "TimeTypes.hpp"

/**
 * All maneuver classes are derived from this base class.
 */
class GMAT_API Burn : public GmatBase
{
public:
   Burn(Gmat::ObjectType type, const std::string &typeStr,
        const std::string &nomme);
   virtual ~Burn();
   Burn(const Burn &b);
   Burn&                operator=(const Burn &b);

   bool                 IsUsingLocalCoordSystem();
   
   // Inherited (GmatBase) methods
   // for parameters
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
   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value,
                                           const Integer index);
   
   // for enumerated strings
   virtual const StringArray&
                        GetPropertyEnumStrings(const Integer id) const;
   virtual const StringArray&
                        GetPropertyEnumStrings(const std::string &label) const;
   
   // for Ref. objects
   virtual const ObjectTypeArray&
                        GetRefObjectTypeArray();
   virtual const StringArray&
                        GetRefObjectNameArray(const Gmat::ObjectType type);
   virtual bool         SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                                     const std::string &name = "");
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   
   // Accessor method used by Maneuver to pass in the spacecraft pointer
   virtual void         SetSpacecraftToManeuver(Spacecraft *sat);
   
   virtual void         SetSolarSystem(SolarSystem *ss);
   virtual bool         Initialize();
   
   //---------------------------------------------------------------------------
   // bool Fire(Real *burnData = NULL, Real epoch = GmatTimeUtil::MJD_OF_J2000)
   //---------------------------------------------------------------------------
   /**
    * Applies the burn.  
    * 
    * Derived classes implement this method to provide the mathematics that 
    * model the burn.  The parameter is provided so that the derived classes 
    * have an interface to pass in additional data as needed.
    * 
    * @param <burnData>    Array of data specific to the derived burn class. 
    *
    * @return true on success, false or throw on failure.
    */
   //---------------------------------------------------------------------------
   virtual bool         Fire(Real *burnData = NULL, Real epoch = GmatTimeUtil::MJD_OF_J2000) = 0;
    
protected:

   /// Solar system used to find the J2000 body, etc.
   SolarSystem          *solarSystem;
   /// Local Coordinate system
   CoordinateSystem     *localCoordSystem;
   /// Coordinate system
   CoordinateSystem     *coordSystem;
   /// Ponter to the burn origin
   SpacePoint           *localOrigin;
   /// Pointer to the J2000 body
   CelestialBody        *j2000Body;
   /// Pointer to the spacecraft that maneuvers
   Spacecraft           *spacecraft;
   /// Text description of the GMAT coordinate system, if used
   std::string          coordSystemName;
   /// Name of the Spacepoint used as the origin of the burn
   std::string          localOriginName;
   /// Text description of the (internal) coordinate axis type
   std::string          localAxesName;
   /// Name of the J2000 body
   std::string          j2000BodyName;
   /// Name of the Spacecraft that gets maneuvered
   std::string          satName;
   /// Orientation vector for maneuver; includes magnitude for impulsive burns
   Real                 deltaV[3];
   /// Orientation vector for maneuver in inertial system
   Real                 deltaVInertial[3];
   /// Matrix of maneuver frame vectors
   Real                 frameBasis[3][3];
   /// String array that holds ref. object names
   StringArray          refObjectNames;
   /// Text description of the vector format (deprecated)
   std::string          vectorFormat;
   /// Flag indicating if local coordinate system is used
   bool                 usingLocalCoordSys;
   /// Flag indicating if axes is MJ2000Eq
   bool                 isMJ2000EqAxes;
   /// Flag indicating if axes is SpacecrftBody
   bool                 isSpacecraftBodyAxes;
   /// Flag used to determine if the configuration needs updating
   bool                 initialized;
   
   /// Available local axes labels
   static StringArray   localAxesLabels;
   
   CoordinateSystem*    CreateLocalCoordinateSystem();
   void                 TransformJ2kToBurnOrigin(const Real *scState, 
                                                 Real *state, Real epoch);
   void                 ConvertDeltaVToInertial(Real *dv, Real *dvInertial,
                                                Real epoch);
   
   /// Published parameters for burns
   enum
   {
      COORDINATESYSTEM = GmatBaseParamCount,
      BURNORIGIN,
      BURNAXES,
      VECTORFORMAT,
      DELTAV1,
      DELTAV2,
      DELTAV3,
      SATNAME,
      BurnParamCount
   };
   
   /// burn parameter labels
   static const std::string 
      PARAMETER_TEXT[BurnParamCount - GmatBaseParamCount];
   /// burn parameter types
   static const Gmat::ParameterType 
      PARAMETER_TYPE[BurnParamCount - GmatBaseParamCount];
};


#endif // Burn_hpp
