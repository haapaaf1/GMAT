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
#include "PlanetaryEphem.hpp"
//#include "ForceModel.hpp"  // what about Drag?
#include "AtmosphereManager.hpp"
#include "AtmosphereModel.hpp"
#include "Rmatrix.hpp"
#include "Rvector6.hpp"

// Add needed things to Gmat namespace
namespace Gmat  // do all of these things exist somewhere else already?
{
   // possible sources of position and velocity data for celestial bodies
   enum PosVelSource
   {
      ANALYTIC = 0,
      SLP,
      DE_102,
      DE_200,
      DE_202,
      DE_403,
      DE_405,
      DE_406,
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
   virtual const Rvector6&      GetState(A1Mjd atTime); 
   
   // methods to return the body type, central body, gravitational constant,
   // radius, mass, posvel source, and analytic method - ****need methods for other data as well
   virtual Gmat::BodyType       GetBodyType() const;
   virtual CelestialBody*       GetCentralBody() const;
   virtual Real                 GetGravitationalConstant();
   virtual Real                 GetEquatorialRadius();
   virtual Real                 GetPolarRadius() const;
   virtual Real                 GetMass() const;
   virtual Gmat::PosVelSource   GetPosVelSource() const;
   virtual Gmat::AnalyticMethod GetAnalyticMethod() const;
   virtual bool                 GetUsePotentialFile() const;
   virtual Real*                GetAngularVelocity();
   virtual Real                 GetHourAngle() const;            // const??  -> do I need a time here?
   virtual Rmatrix              GetHarmonicCoefficientsSij(); // const??
   virtual Rmatrix              GetHarmonicCoefficientsCij(); // const??
   virtual const StringArray&   GetSupportedAtmospheres() const;
   virtual std::string          GetAtmosphereModelType();
   virtual AtmosphereModel*     GetAtmosphereModel(void);
   

   // methods to return the body type, central body,
   // posvel source, and analytic method
   virtual bool           SetBodyType(Gmat::BodyType bType);
   virtual bool           SetCentralBody(CelestialBody* cBody);
   virtual bool           SetSource(Gmat::PosVelSource pvSrc);
   virtual bool           SetSourceFile(PlanetaryEphem *src);
   virtual bool           SetAnalyticMethod(Gmat::AnalyticMethod aM);
   virtual bool           SetUsePotentialFile(bool useIt);
   // need to set the filename here as well????
   virtual bool           SetAtmosphereModel(std::string toAtmModel);

   
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
   virtual const StringArray& GetStringArrayParameter(const Integer id) const;

   
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
   /// state of the body 0-2 position 3-5 velocity
   Rvector6               state ;
   // time of the state
   A1Mjd                  stateTime;
   // order of the gravity model
   Integer                order;     // are these the same as coefficientSize?
   // degree of the gravity model
   Integer                degree;     // are these the same as coefficientSize?

   /// central body around which this body revolves
   CelestialBody          *centralBody;
   /// body number for the SLP file
   Integer                bodyNumber;
   /// body number of origin of coordinate system for file
   Integer                referenceBodyNumber;
   /// name of file that is the source of position and velocity for this body
   std::string            sourceFilename;
   /// date and time of start of source file
   A1Mjd                  sourceStart;       // do I need this?
   /// date and time of end of sourcce file
   A1Mjd                  sourceEnd;         // do I need this?
   // the source file
   PlanetaryEphem*        theSourceFile;

   bool                   usePotentialFile;
   std::string            potentialFileName;   // do I need a pointer to a file type/class for the potential file?
   Real                   angularVelocity[3];
   Integer                coefficientSize;      // n   // same as degree, order above?
   Rmatrix                sij;                  // n x n
   Rmatrix                cij;                  // n x n
   Real                   hourAngle;
   AtmosphereManager*     atmManager;
   AtmosphereModel*       atmModel;

   /// IDs for the parameters
   Integer                bodyTypeID;
   Integer                massID;
   Integer                eqRadiusID;
   Integer                polarRadiusID;
   Integer                muID;
   Integer                posVelSourceID;
   Integer                analyticMethodID;
   Integer                state1ID;
   Integer                state2ID;
   Integer                state3ID;
   Integer                state4ID;
   Integer                state5ID;
   Integer                state6ID;
   Integer                stateTimeID;
   Integer                orderID;
   Integer                degreeID;
   Integer                bodyNumberID;
   Integer                refBodyNumberID;
   Integer                sourceFilenameID;
   Integer                sourceStartID; // ???????????????
   Integer                sourceEndID;   // ???????????????
   Integer                usePotentialFileID;
   Integer                potentialFileNameID;
   Integer                angularVelocityID1;
   Integer                angularVelocityID2;
   Integer                angularVelocityID3;
   Integer                coefficientSizeID;
   Integer                sijID;        // ?????????????????
   Integer                cijID;        // ?????????????????
   Integer                hourAngleID;
   Integer                atmModelID;
   Integer                supportedAtmModelsID;

   // initialze the body
   void Initialize(std::string withBodyType = "Planet");
   // read the potential file, if requested
   bool ReadPotentialFile();

   // has the potential file been read already?
   bool                   potentialFileRead;

   // defaults if potential file is not used
   Real                   defaultMu;
   Real                   defaultEqRadius;
   Integer                defaultCoefSize;
   Rmatrix                defaultSij;
   Rmatrix                defaultCij;

private:

};
#endif // CelestialBody_hpp

