//$Id$
//------------------------------------------------------------------------------
//                             FactoryManager
//------------------------------------------------------------------------------

// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Wendy Shoan
// Created: 2003/08/27
//
/**
 * This class is the interface between the Moderator and Factories.  It is a
 * singleton - only one instance of this class can be created.
 */
//------------------------------------------------------------------------------
#ifndef FactoryManager_hpp
#define FactoryManager_hpp

#include <list>
#include <string>
#include "gmatdefs.hpp"
#include "Factory.hpp"
#include "Spacecraft.hpp"
#include "Parameter.hpp"
#include "Propagator.hpp"
#include "ODEModel.hpp"
#include "PhysicalModel.hpp"
#include "PropSetup.hpp"
#include "StopCondition.hpp"
#include "CalculatedPoint.hpp"
#include "CelestialBody.hpp"
#include "SolarSystem.hpp"
#include "Solver.hpp"
#include "Subscriber.hpp"
#include "GmatCommand.hpp"
#include "Burn.hpp"
#include "AtmosphereModel.hpp"
#include "Function.hpp"
#include "AxisSystem.hpp"
#include "CoordinateSystem.hpp"
#include "MathNode.hpp"
#include "Attitude.hpp"

#include "MeasurementModel.hpp"
#include "GeometricMeasurement.hpp"  // Change to "CoreMeasurement.hpp" when ready
#include "Datafile.hpp"
#include "Obtype.hpp"


/**
 * GMAT Factory Manager Class, the interface between the Moderator and the
 * factories.
 *
 * The Factory Manager manages all of the factories that are needed for
 * execution of the system for a particular run.  Each factory must be
 * registered with the FactoryManager in order for its objects to be able to be
 * created.  The Moderator will register all of the predefined factories;
 * user-defined factories must be registered as they are added to the system.
 */
class GMAT_API FactoryManager
{
public:
   // class instance method (this is a singleton class)
   static FactoryManager* Instance();
   // all factories must be registered via this method
   bool                   RegisterFactory(Factory* fact);

   // Generic method to create an object
   GmatBase*              CreateObject(const Gmat::ObjectType generalType,
                                       const std::string &ofType,
                                       const std::string &withName = "");
   
   // methods to create and return objects of the various types
   SpaceObject*           CreateSpacecraft(const std::string &ofType,
                                           const std::string &withName = "");
   Parameter*             CreateParameter(const std::string &ofType,
                                          const std::string &withName = "");
   Propagator*            CreatePropagator(const std::string &ofType,
                                           const std::string &withName = "");
   PhysicalModel*         CreatePhysicalModel(const std::string &ofType,
                                              const std::string &withName = "");
   StopCondition*         CreateStopCondition(const std::string &ofType,
                                              const std::string &withName = "");
   CalculatedPoint*       CreateCalculatedPoint(const std::string &ofType,
                                                const std::string &withName = "");
   CelestialBody*         CreateCelestialBody(const std::string &ofType,
                                              const std::string &withName = "");
   Solver*                CreateSolver(const std::string &ofType,
                                       const std::string &withName = "");
   Subscriber*            CreateSubscriber(const std::string &ofType,
                                           const std::string &withName = "",
                                           const std::string &fileName = "");
   GmatCommand*           CreateCommand(const std::string &ofType,
                                        const std::string &withName = "");
   Burn*                  CreateBurn(const std::string &ofType,
                                     const std::string &withName = "");
   AtmosphereModel*       CreateAtmosphereModel(const std::string &ofType,
                                                const std::string &withName = "",
                                                const std::string &forBody = "Earth");
   Function*              CreateFunction(const std::string &ofType,
                                         const std::string &withName = "");
   Hardware*              CreateHardware(const std::string &ofType,
                                         const std::string &withName = "");
   AxisSystem*            CreateAxisSystem(const std::string &ofType,
                                           const std::string &withName = "");
   MathNode*              CreateMathNode(const std::string &ofType,
                                         const std::string &withName = "");
   Attitude*              CreateAttitude(const std::string &ofType,
                                         const std::string &withName = "");
   SpacePoint*            CreateSpacePoint(const std::string &ofType,
                                           const std::string &withName = "");

   CoreMeasurement*       CreateMeasurement(const std::string &ofType,
                                            const std::string &withName = "");

   Obtype*                CreateObtype(const std::string &ofType,
                                            const std::string &withName = "");

   //----- Just container
   SolarSystem*           CreateSolarSystem(const std::string &withName = "");
   PropSetup*             CreatePropSetup(const std::string &withName = "");
   ODEModel*              CreateODEModel(const std::string &ofType,
                                         const std::string &withName = "");
   CoordinateSystem*      CreateCoordinateSystem(const std::string &withName = "");

   MeasurementModel*      CreateMeasurementModel(const std::string &withName);
   Datafile*              CreateDatafile(const std::string &withName);

   // method to return a list of strings representing the objects of the input
   // type that may be created in the system
   const StringArray&     GetListOfItems(Gmat::ObjectType byType);
   const StringArray&     GetListOfAllItems();
   
   // method to return the base type for the input string
   Gmat::ObjectType       GetBaseTypeOf(const std::string &typeName);
   
protected:
   StringArray            entireList;
   
private:

   // private class data
   /// the list of factories that have been registered and which are available
   /// to create objects
   std::list<Factory*> factoryList;
   /// pointer to the only instance allowed for this singleton class
   static FactoryManager* onlyInstance;
   
   // private methods 
   Factory*               FindFactory(Gmat::ObjectType ofType, const std::string &forType);
   const StringArray&     GetList(Gmat::ObjectType ofType);
   
   // Hide the default constructor and destructor to preserve singleton status
   // default constructor
   FactoryManager();
   // class destructor
   ~FactoryManager();
};

#endif // FactoryManager_hpp
