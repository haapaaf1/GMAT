//$Header$
//------------------------------------------------------------------------------
//                                  CelestialBody
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy C. Shoan
// Created: 2004/01/28
//
/**
 * This is the base class for celestial bodies.
 *
 * @note This is an abstract class.
 */
//------------------------------------------------------------------------------


// Class (initial shell only) automatically generated by Dev-C++ New Class wizard

#ifndef CelestialBody_hpp
#define CelestialBody_hpp

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "A1Mjd.hpp"
#include "ForceModel.hpp"  // what about Drag? 

// Add needed things to Gmat namespace
namespace Gmat  // do all of these things exist somewhere else already?
{
   // possible sources of position and velocity data for celestial bodies
   enum PosVelSource
   {
      ANALYTIC = 0,
      SLP,
      DE,                 // different types of DE files?
      EPHEMERIS           // do we need more?
   };

   // if using an analytical method, which one?
   enum AnalyticMethod
   {
      NONE = 0,
      TWO_BODY,
      EARTH_ANALYTIC,
      MOON_ANALYTIC,
      NUM_ANALYTIC
   };

   // possible types of celestial bodies
   enum BodyType
   {
      STAR = 0,
      PLANET,
      MOON,
      ASTEROID,
      COMET,
      BodyTypeCount
   };
};

/**
 * CelestialBody base class, from which all types of celestial bodeis will derive.
 *
 * The CelestialBody class is primarily a base class, from which all types of
 * celestial bodies will derive.  CelestialBody itself derives from GmatBase.
 */
class GMAT_API CelestialBody : public GmatBase
{
public:
   // default constructor, with optional name
   //CelestialBody(std::string name = SolarSystem::EARTH_NAME);
   CelestialBody(std::string name = "Earth");
   // additional constructor, specifying body type (as string) and name
   CelestialBody(std::string itsBodyType, std::string name);
   // additional constructor, specifying type (as Gmat::BodyType) and name
   CelestialBody(Gmat::BodyType itsBodyType, std::string name);
   // copy constructor
   CelestialBody(const CelestialBody &cb);
   // operator=
   CelestialBody& operator=(const CelestialBody &cb);
   // destructor
   virtual ~CelestialBody();

   // method to return the state (position and velocity) of the body at
   // the specified time, using the specified method
   virtual RealArray            GetState(A1Mjd atTime);  // should this be pure virtual?
   // methods to return the body type, central body, gravitational constant,
   // radius, mass, posvel source, and analytic method - ****need methods for other data as well
   virtual Gmat::BodyType       GetBodyType() const;
   virtual CelestialBody*       GetCentralBody() const;
   virtual Real                 GetGravitationalConstant() const;
   virtual Real                 GetEquatorialRadius() const;
   virtual Real                 GetMass() const;
   virtual Gmat::PosVelSource   GetPosVelSource() const;
   virtual Gmat::AnalyticMethod GetAnalyticMethod() const;

   // methods to return the body type, central body,
   // posvel source, and analytic method
   virtual bool           SetBodyType(Gmat::BodyType bType);
   virtual bool           SetCentralBody(CelestialBody* cBody);
   virtual bool           SetSource(Gmat::PosVelSource pvSrc);
   virtual bool           SetAnalyticMethod(Gmat::AnalyticMethod aM);

   
   // Parameter access methods - overridden from GmatBase
   virtual std::string    GetParameterText(const Integer id) const;     // const?
   virtual Integer        GetParameterID(const std::string &str) const; // const?
   virtual Gmat::ParameterType
                          GetParameterType(const Integer id) const;
   virtual std::string    GetParameterTypeString(const Integer id) const;

   virtual Real           GetRealParameter(const Integer id) const;
   virtual Real           SetRealParameter(const Integer id,
                                           const Real value);
   virtual Integer        GetIntegerParameter(const Integer id) const; // const?
   virtual Integer        SetIntegerParameter(const Integer id,
                                              const Integer value); // const?
   virtual std::string    GetStringParameter(const Integer id) const; // const?
   virtual bool           SetStringParameter(const Integer id, 
                                             const std::string &value); // const?
   virtual bool           GetBooleanParameter(const Integer id) const; // const?
   virtual bool           SetBooleanParameter(const Integer id,
                                              const bool value); // const?

   //------------------------------------------------------------------------------
   // virtual CelestialBody* Clone(void) const
   //------------------------------------------------------------------------------
   /**
     * Method used to create a copy of the object
     * Each class that can be instantiated provides an implementation of this
     * method so that copies can be made from a base class pointer.
     */
   //------------------------------------------------------------------------------
   virtual CelestialBody* Clone(void) const = 0;

   // strings representing the possible celestial body types
   static const std::string BODY_TYPE_STRINGS[Gmat::BodyTypeCount];

protected:

   // body type of the body
   Gmat::BodyType         bodyType;
   // mass
   Real                   mass;
   /// radius of the body at the equator
   Real                   equatorialRadius;
   /// radius of the body from center to the pole
   Real                   polarRadius;
   /// gravitational constant in km^3/s^2
   Real                   mu;
   /// source for position and velocity
   Gmat::PosVelSource     posVelSrc;
   /// analytic method to use
   Gmat::AnalyticMethod   analyticMethod;
   /// flattening coefficient
   Real                   flatCoeff;
   /// rotation rate in rad/sec
   Real                   rotationRate;
   /// zonals
   RealArray              zonals;
   /// state of the body 0-2 position 3-5 velocity
   RealArray              state;
   /// maximum distance from the body at which oblateness effects matter
   Real                   effectiveRange;
   /// central body around which this body revolves
   CelestialBody          *centralBody;
   /// potential model
   ForceModel             *potentialModel;   // - is this right??
   /// drag model
   //Drag                   *dragModel;
   ForceModel                   *dragModel;  // should be drag
   // order of the gravity model
   Integer                order;
   // degree of the gravity model
   Integer                degree;
   
   /// flag indicting whether or not this body has been initialized
   bool                   isInitialized;
   /// body number for the SLP file
   Integer                bodyNumber;
   /// body number of origin of coordinate system for file
   Integer                referenceBodyNumber;
   /// name of file that is the source of position and velocity for this body
   std::string            sourceFilename;
   /// date and time of start of source file
   A1Mjd                  sourceStart;
   /// date and time of end of sourcce file
   A1Mjd                  sourceEnd;

   /// IDs for the parameters
   Integer                bodyTypeID;
   Integer                massID;
   Integer                eqRadiusID;
   Integer                polarRadiusID;
   Integer                muID;
   Integer                posVelSourceID;
   Integer                analyticMethodID;
   Integer                flatCoeffID;
   Integer                rotationRateID;
   Integer                zonals1ID;
   Integer                zonals2ID;
   Integer                zonals3ID;
   Integer                zonals4ID;
   Integer                zonals5ID;
   Integer                state1ID;
   Integer                state2ID;
   Integer                state3ID;
   Integer                state4ID;
   Integer                state5ID;
   Integer                state6ID;
   Integer                effRangeID;
   Integer                centralBodyID;    // ID for pointer? is this even needed?
   Integer                potentialModelID; // ???
   Integer                dragModelID;      // ???
   Integer                orderID;
   Integer                degreeID;
   Integer                isInitializedID;
   Integer                bodyNumberID;
   Integer                refBodyNumberID;
   Integer                sourceFilenameID;
   Integer                sourceStartID; // ???????????????
   Integer                sourceEndID;   // ???????????????

   void Initialize(std::string withBodyType = "Planet");

private:

};
#endif // CelestialBody_hpp

