//$Id$
//------------------------------------------------------------------------------
//                                  SolarSystem
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy C. Shoan
// Created: 2004/01/28
//
/**
 * This is the SolarSystem class.
 *
 * @note The SolarSystem will contain pointers to all of the celestial bodies
 *       currently in use; NOTE *** for Build 2, this is Sun/Earth/Moon only***
 *       For Build 3, all planets (except Sedna?) are included.
 * @note It is assumed no classes will be derived from this one.
 */
//------------------------------------------------------------------------------


// Class (initial shell only) automatically generated by Dev-C++ New Class wizard

#ifndef SolarSystem_hpp
#define SolarSystem_hpp

#include <list>
#include <string>
#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "CelestialBody.hpp"
#include "SpecialCelestialPoint.hpp"
#include "PlanetaryEphem.hpp"
#include "DeFile.hpp"
#include "GmatDefaults.hpp"
#ifdef __USE_SPICE__
#include "SpiceOrbitKernelReader.hpp"
#endif

class CoordinateSystem;

/**
 * SolarSystem class, containing pointers to all of the objects currently in
 * use.
 *
 * The SolarSystem class manages all objects that are currently defined for the
 * specified Solar System.  NOTE - For Build 2, the default Solar System
 * contains only the Sun, Earth, and Moon.
 */
class GMAT_API SolarSystem : public GmatBase
{
public:
   // class default constructor - creates default solar system
   // for Build 2 - this is Sun, Earth, Moon only
   SolarSystem(std::string withName = "");
   // copy constructor
   SolarSystem(const SolarSystem &ss);
   // operator=
   SolarSystem& operator=(const SolarSystem &ss);
   // class destructor
   ~SolarSystem();
   
   virtual bool Initialize();
   
   // method for planetary ephemeris files
   void CreatePlanetarySource(bool setDefault = true);
   
   const StringArray& GetPlanetarySourceTypes();
   const StringArray& GetPlanetarySourceNames();
   const StringArray& GetPlanetarySourceTypesInUse();
   bool  SetPlanetarySourceName(const std::string &sourceType,
                               const std::string &fileName);
   Integer SetPlanetarySourceTypesInUse(const StringArray &sourceTypes); 
   Integer GetPlanetarySourceId(const std::string &sourceType);
   std::string GetPlanetarySourceName(const std::string &sourceType);
   std::string GetCurrentPlanetarySource();
   void        SetIsSpiceAllowedForDefaultBodies(const bool allowSpice);
   bool        IsSpiceAllowedForDefaultBodies() const;

   PlanetaryEphem* GetPlanetaryEphem();
#ifdef __USE_SPICE__
   void        LoadSpiceKernels();
   SpiceOrbitKernelReader* GetSpiceOrbitKernelReader();
#endif

   
   void ResetToDefaults();
   
   // method to add a body to the solar system
   bool                 AddBody(CelestialBody* cb);
   // method to return a body of the solar system, given its name
   CelestialBody*       GetBody(std::string withName);
   // method to remove a body from the solar system
   bool                 DeleteBody(const std::string &withName);
   /// methods to return a pointer to a specific SpecialCelestialPoint
   SpecialCelestialPoint*
                        GetSpecialPoint(const std::string &withName);
   
   // method to return an array of the names of the bodies included in
   // this solar system
   const StringArray&   GetBodiesInUse() const;
   // method to return a flag indicating whether or not the specified
   // body is in use for this solar system
   bool                 IsBodyInUse(std::string theBody);
   const StringArray&   GetDefaultBodies() const;
   const StringArray&   GetUserDefinedBodies() const;
   
   // methods to get the source and analytic model flags
   Gmat::PosVelSource   GetPosVelSource() const;
   std::string          GetSourceFileName() const;
   bool                 GetOverrideTimeSystem() const;
   Real                 GetEphemUpdateInterval() const;
   StringArray          GetValidModelList(Gmat::ModelType m, 
                                          const std::string &forBody);
   
   // methods to set the source, source file, and analytic method for each
   // of the bodies in use
   bool SetSource(Gmat::PosVelSource pvSrc);
   bool SetSource(const std::string &pvSrc);
   bool SetSourceFile(PlanetaryEphem *src);
   bool SetSPKFile(const std::string &spkFile);
   bool SetLSKFile(const std::string &lskFile);
   
   bool SetOverrideTimeSystem(bool overrideIt);
   bool SetEphemUpdateInterval(Real intvl);
   bool AddValidModelName(Gmat::ModelType m, const std::string &forBody,
                          const std::string &theModel);
   bool RemoveValidModelName(Gmat::ModelType m, const std::string &forBody,
                             const std::string &theModel);
   
   
   // Parameter access methods
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   
   virtual Integer      GetIntegerParameter(const Integer id) const;
   virtual Integer      GetIntegerParameter(const std::string &label) const;
   virtual Real         GetRealParameter(const Integer id) const;
   virtual Real         GetRealParameter(const std::string &label) const;
   virtual Real         SetRealParameter(const Integer id,
                                         const Real value);
   virtual Real         SetRealParameter(const std::string &label,
                                         const Real value);
   virtual bool         GetBooleanParameter(const Integer id) const; 
   virtual bool         GetBooleanParameter(const std::string &label) const; 
   virtual bool         SetBooleanParameter(const Integer id,
                                            const bool value);
   virtual bool         SetBooleanParameter(const std::string &label,
                                            const bool value);
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
   virtual bool         SetStringParameter(const Integer id, 
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label, 
                                           const std::string &value);
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const;
   virtual const StringArray&
                        GetStringArrayParameter(const std::string &label) const;
   
   virtual Integer      GetOwnedObjectCount();
   virtual GmatBase*    GetOwnedObject(Integer whichOne);

   virtual bool         IsParameterReadOnly(const Integer id) const;
   virtual bool         IsParameterCloaked(const Integer id) const;
   virtual bool         IsParameterEqualToDefault(const Integer id) const;
   virtual bool         SaveAllAsDefault();
   virtual bool         SaveParameterAsDefault(const Integer id);

   // all classes derived from GmatBase must supply this Clone method
   virtual SolarSystem* Clone(void) const;
   
   // required method for all subclasses that can be copied in a script
   virtual void         Copy(const GmatBase* orig);
   
   /// default names for each of the possible celestial bodies in the solar system
   static const std::string SOLAR_SYSTEM_BARYCENTER_NAME;


   static const std::string SUN_NAME;

   static const std::string MERCURY_NAME;
   
   static const std::string VENUS_NAME;
   
   static const std::string EARTH_NAME;
   static const std::string MOON_NAME;
   
   static const std::string MARS_NAME;
   static const std::string PHOBOS_NAME;
   static const std::string DEIMOS_NAME;
   
   static const std::string JUPITER_NAME;
   static const std::string METIS_NAME;
   static const std::string ADRASTEA_NAME;
   static const std::string AMALTHEA_NAME;
   static const std::string THEBE_NAME;
   static const std::string IO_NAME;
   static const std::string EUROPA_NAME;
   static const std::string GANYMEDE_NAME;
   static const std::string CALLISTO_NAME;
   
   static const std::string SATURN_NAME;
   static const std::string PAN_NAME;
   static const std::string ATLAS_NAME;
   static const std::string PROMETHEUS_NAME;
   static const std::string PANDORA_NAME;
   static const std::string EPIMETHEUS_NAME;
   static const std::string JANUS_NAME;
   static const std::string MIMAS_NAME;
   static const std::string ENCELADUS_NAME;
   static const std::string TETHYS_NAME;
   static const std::string TELESTO_NAME;
   static const std::string CALYPSO_NAME;
   static const std::string DIONE_NAME;
   static const std::string HELENE_NAME;
   static const std::string RHEA_NAME;
   static const std::string TITAN_NAME;
   static const std::string IAPETUS_NAME;
   static const std::string PHOEBE_NAME;
   
   static const std::string URANUS_NAME;
   static const std::string CORDELIA_NAME;
   static const std::string OPHELIA_NAME;
   static const std::string BIANCA_NAME;
   static const std::string CRESSIDA_NAME;
   static const std::string DESDEMONA_NAME;
   static const std::string JULIET_NAME;
   static const std::string PORTIA_NAME;
   static const std::string ROSALIND_NAME;
   static const std::string BELINDA_NAME;
   static const std::string PUCK_NAME;
   static const std::string MIRANDA_NAME;
   static const std::string ARIEL_NAME;
   static const std::string UMBRIEL_NAME;
   static const std::string TITANIA_NAME;
   static const std::string OBERON_NAME;

   static const std::string NEPTUNE_NAME;
   static const std::string NAIAD_NAME;
   static const std::string THALASSA_NAME;
   static const std::string DESPINA_NAME;
   static const std::string GALATEA_NAME;
   static const std::string LARISSA_NAME;
   static const std::string PROTEUS_NAME;
   static const std::string TRITON_NAME;

   static const std::string PLUTO_NAME;
   static const std::string CHARON_NAME;
   
   // add other moons, asteroids, comets, as needed
   // what do we do about libration points??

   DEFAULT_TO_NO_CLONES
   DEFAULT_TO_NO_REFOBJECTS

protected:
   enum
   {
      BODIES_IN_USE = GmatBaseParamCount,
      NUMBER_OF_BODIES,
      EPHEMERIS,   // deprecated!!!!!!
      EPHEMERIS_SOURCE, 
      DE_FILE_NAME,
      SPK_FILE_NAME,
      LSK_FILE_NAME,
      OVERRIDE_TIME_SYSTEM,
      EPHEM_UPDATE_INTERVAL,
      SolarSystemParamCount
   };
      

   static const std::string
      PARAMETER_TEXT[SolarSystemParamCount - GmatBaseParamCount];
   
   static const Gmat::ParameterType
      PARAMETER_TYPE[SolarSystemParamCount - GmatBaseParamCount];
      
   Gmat::PosVelSource    pvSrcForAll;
   PlanetaryEphem*       thePlanetaryEphem;
   bool                  overrideTimeForAll;
   Real                  ephemUpdateInterval;

private:
   
   std::string theCurrentPlanetarySource;
   Integer thePlanetarySourcePriority[Gmat::PosVelSourceCount];
   bool isPlanetarySourceInUse[Gmat::PosVelSourceCount];
   static const Integer HIGHEST_PRIORITY = 10;
   
   // list for planetary source
   StringArray thePlanetarySourceTypes;
   StringArray thePlanetarySourceNames;
   StringArray thePlanetarySourceTypesInUse;
   StringArray theTempFileList;
   
   DeFile *theDefaultDeFile;
   
   /// list of the celestial bodies that are included in this solar system
   std::vector<CelestialBody*> bodiesInUse;
   
   /// the names of the bodies in use
   StringArray bodyStrings;  // is this needed, or just a convenience?
   StringArray defaultBodyStrings;
   StringArray userDefinedBodyStrings;

   /// map of SpecialCelestialPoints that the SolarSystem knows about
   std::map<std::string, SpecialCelestialPoint*>  specialPoints;

#ifdef __USE_SPICE__
   SpiceOrbitKernelReader *planetarySPK;
#endif
   /// flag indicating whether or not SPICE is allowed as a position/velocity 
   /// source for default bodies
   bool        allowSpiceForDefaultBodies;
   bool        spiceAvailable;
   /// name of the SPK file for the default bodies
   std::string theSPKFilename;
   /// name of the leap second kernel
   std::string lskKernelName;
   
   // default values for parameters
   StringArray default_planetarySourceTypesInUse;  // deprecated!!
   std::string default_ephemerisSource;
   std::string default_DEFilename;
   std::string default_SPKFilename;
   std::string default_LSKFilename;
   bool        default_overrideTimeForAll;
   Real        default_ephemUpdateInterval;
   
   // method to find a body in the solar system, given its name
   CelestialBody* FindBody(std::string withName);
   void SetJ2000Body();
   void CloneBodiesInUse(const SolarSystem &ss, bool cloneSpecialPoints = true);
   void DeleteBodiesInUse(bool deleteSpecialPoints = true);
   
   // methods to create planetary source file
   void SetDefaultPlanetarySource();
   bool CreateDeFile(const Integer id, const std::string &fileName,
                     Gmat::DeFileFormat format = Gmat::DE_BINARY);
   
//   // default values for CelestialBody data
   static const Gmat::PosVelSource    PLANET_POS_VEL_SOURCE;
   static const Integer               PLANET_ORDER[GmatSolarSystemDefaults::NumberOfDefaultPlanets];
   static const Integer               PLANET_DEGREE[GmatSolarSystemDefaults::NumberOfDefaultPlanets];
   static const Integer               PLANET_NUM_GRAVITY_MODELS[GmatSolarSystemDefaults::NumberOfDefaultPlanets];
   static const Integer               PLANET_NUM_ATMOSPHERE_MODELS[GmatSolarSystemDefaults::NumberOfDefaultPlanets];
   static const Integer               PLANET_NUM_MAGNETIC_MODELS[GmatSolarSystemDefaults::NumberOfDefaultPlanets];
   static const Integer               PLANET_NUM_SHAPE_MODELS[GmatSolarSystemDefaults::NumberOfDefaultPlanets];
   static const std::string           PLANET_GRAVITY_MODELS[];
   static const std::string           PLANET_ATMOSPHERE_MODELS[];
   static const std::string           PLANET_MAGNETIC_MODELS[];
   static const std::string           PLANET_SHAPE_MODELS[]; // @todo add Shape Models
   static const Gmat::PosVelSource    MOON_POS_VEL_SOURCE[GmatSolarSystemDefaults::NumberOfDefaultMoons];
   static const Integer               MOON_ORDER[GmatSolarSystemDefaults::NumberOfDefaultMoons];
   static const Integer               MOON_DEGREE[GmatSolarSystemDefaults::NumberOfDefaultMoons];
   static const Integer               MOON_NUM_GRAVITY_MODELS[GmatSolarSystemDefaults::NumberOfDefaultMoons];
   static const Integer               MOON_NUM_ATMOSPHERE_MODELS[GmatSolarSystemDefaults::NumberOfDefaultMoons];
   static const Integer               MOON_NUM_MAGNETIC_MODELS[GmatSolarSystemDefaults::NumberOfDefaultMoons];
   static const Integer               MOON_NUM_SHAPE_MODELS[GmatSolarSystemDefaults::NumberOfDefaultMoons];
   static const std::string           MOON_GRAVITY_MODELS[];
   static const std::string           MOON_ATMOSPHERE_MODELS[];
   static const std::string           MOON_MAGNETIC_MODELS[];
   static const std::string           MOON_SHAPE_MODELS[]; // @todo add Shape Models
   static const Gmat::PosVelSource    STAR_POS_VEL_SOURCE;
   static const Integer               STAR_ORDER;
   static const Integer               STAR_DEGREE;
   static const Integer               STAR_NUM_GRAVITY_MODELS;
   static const Integer               STAR_NUM_ATMOSPHERE_MODELS;
   static const Integer               STAR_NUM_MAGNETIC_MODELS;
   static const Integer               STAR_NUM_SHAPE_MODELS;
   static const std::string           STAR_GRAVITY_MODELS;
   static const std::string           STAR_ATMOSPHERE_MODELS;
   static const std::string           STAR_MAGNETIC_MODELS;
   static const std::string           STAR_SHAPE_MODELS; // @todo add Shape Models

};

#endif // SolarSystem_hpp

