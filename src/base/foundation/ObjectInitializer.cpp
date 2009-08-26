//$Id$
//------------------------------------------------------------------------------
//                                  ObjectInitializer
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CCA54C
//
// Author: Wendy Shoan
// Created: 2008.06.11
//
// Original code from the Sandbox.
// Author: Darrel J. Conway
//
/**
 * Implementation for the ObjectInitializer class.
 * This class initializes objects of the specified LocalObjectStore and ,
 * on option, GlobalObjectStore.
 */
//------------------------------------------------------------------------------
#include "gmatdefs.hpp"
#include "Moderator.hpp"
#include "ObjectInitializer.hpp"
#include "SolarSystem.hpp"
#include "CoordinateSystem.hpp"
#include "GmatBaseException.hpp"   // need a specific one here?  ObjectExecption?
#include "SubscriberException.hpp"
#include "Publisher.hpp"

//#define DEBUG_OBJECT_INITIALIZER
//#define DEBUG_OBJECT_INITIALIZER_DETAILED
//#define DEBUG_SUBSCRIBER
//#define DEBUG_INITIALIZE_OBJ
//#define DEBUG_INITIALIZE_CS
//#define DEBUG_BUILD_ASSOCIATIONS

// Cloning the hardware in the spacecraft is now good to go (LOJ: 2009.08.25)
// But if we want to have the old code, just uncomment the lines here and in
// the Spacecraft
//#ifndef __CLONE_HARDWARE_IN_OBJ_INITIALIZER__
//#define __CLONE_HARDWARE_IN_OBJ_INITIALIZER__
//#endif

//#ifndef DEBUG_MEMORY
//#define DEBUG_MEMORY
//#endif
//#ifndef DEBUG_TRACE
//#define DEBUG_TRACE
//#endif

#ifdef DEBUG_MEMORY
#include "MemoryTracker.hpp"
#endif
#ifdef DEBUG_TRACE
#include <ctime>                 // for clock()
#endif

//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// constructor
//------------------------------------------------------------------------------
ObjectInitializer::ObjectInitializer(SolarSystem *solSys, ObjectMap *objMap,
                                     ObjectMap *globalObjMap, CoordinateSystem *intCS, 
                                     bool useGOS) :
   ss         (solSys),
   LOS        (objMap),
   GOS        (globalObjMap),
   mod        (NULL),
   internalCS (intCS),
   includeGOS (useGOS),
   registerSubscribers (false)
{
   mod = Moderator::Instance();
   publisher = Publisher::Instance();
}

//------------------------------------------------------------------------------
// ObjectInitializer(const ObjectInitializer &objInit)
//------------------------------------------------------------------------------
ObjectInitializer::ObjectInitializer(const ObjectInitializer &objInit) :
   ss          (objInit.ss),
   LOS         (objInit.LOS),
   GOS         (objInit.GOS),
   mod         (NULL),
   internalCS  (objInit.internalCS),
   includeGOS  (objInit.includeGOS),
   registerSubscribers (objInit.registerSubscribers)
{
   mod = Moderator::Instance();
   publisher = Publisher::Instance();
}

//------------------------------------------------------------------------------
// ObjectInitializer& operator= (const ObjectInitializer &objInit)
//------------------------------------------------------------------------------
ObjectInitializer& ObjectInitializer::operator= (const ObjectInitializer &objInit)
{
   if (&objInit != this)
   {
      ss          = objInit.ss;
      LOS         = objInit.LOS;
      GOS         = objInit.GOS;
      mod         = objInit.mod;
      internalCS  = objInit.internalCS;
      publisher   = objInit.publisher;
      includeGOS  = objInit.includeGOS;
      registerSubscribers = objInit.registerSubscribers;
   }
   
   return *this;
}


//------------------------------------------------------------------------------
// destructor
//------------------------------------------------------------------------------
ObjectInitializer::~ObjectInitializer()
{
}

//------------------------------------------------------------------------------
// void SetSolarSystem(SolarSystem *solSys)
//------------------------------------------------------------------------------
void ObjectInitializer::SetSolarSystem(SolarSystem *solSys)
{
   ss = solSys;
}

//------------------------------------------------------------------------------
// void SetObjectMap(ObjectMap *objMap)
//------------------------------------------------------------------------------
void ObjectInitializer::SetObjectMap(ObjectMap *objMap)
{
   LOS = objMap;
}

//------------------------------------------------------------------------------
// void SetInternalCoordinateSystem(CoordinateSystem* intCS)
//------------------------------------------------------------------------------
void ObjectInitializer::SetInternalCoordinateSystem(CoordinateSystem* intCS)
{
   #ifdef DEBUG_OI_SET
   MessageInterface::ShowMessage
      ("in OI:SetInternalCoordinateSystem(), intCS=<%p>\n", intCS);
   #endif
   if (intCS != NULL)
      internalCS = intCS;
}

//------------------------------------------------------------------------------
// bool ObjectInitializer::InitializeObjects(bool registerSubs)
//------------------------------------------------------------------------------
/*
 * Initializes objects
 *
 * @param  registerSubs  registers subscribers if set to true (false)
 */
//------------------------------------------------------------------------------
bool ObjectInitializer::InitializeObjects(bool registerSubs)
{   
   #ifdef DEBUG_TRACE
   static Integer callCount = 0;
   callCount++;      
   clock_t t1 = clock();
   MessageInterface::ShowMessage
      ("=== ObjectInitializer::InitializeObjects() entered, Count = %d\n",
       callCount);
   #endif
   
   registerSubscribers =  registerSubs;
   std::map<std::string, GmatBase *>::iterator omIter;
   std::map<std::string, GmatBase *>::iterator omi;
   std::string oName;
   std::string j2kName;
   
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("About to Initialize Internal Objects ...\n");
   #endif
   InitializeInternalObjects();
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("Internal Objects Initialized ...\n");
   #endif
   // Set J2000 Body for all SpacePoint derivatives before anything else
   // NOTE - at this point, everything should be in the SandboxObjectMap,
   // and the GlobalObjectMap should be empty
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("--- Right before setting the J2000 body ---\n");
      MessageInterface::ShowMessage("The Object Map contains:\n");
      for (omIter = LOS->begin(); omIter != LOS->end(); ++omIter)
         MessageInterface::ShowMessage("   %s of type %s\n",
               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
      MessageInterface::ShowMessage("The Global Object Map contains:\n");
      for (omIter = GOS->begin(); omIter != GOS->end(); ++omIter)
         MessageInterface::ShowMessage("   %s of type %s\n",
               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
   #endif
      
   SetObjectJ2000Body(LOS);
   
   if (includeGOS)
      SetObjectJ2000Body(LOS);
                                 
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("J2000 Body set ...\n");
   #endif
   
   // The initialization order is:
   //
   //  1. CoordinateSystems
   //  2. Spacecrafts
   //  3. Measurement Models
   //  4. System Parameters
   //  5. Parameters.
   //  6. Subscribers
   //  7. Remaining Objects
   
   // Coordinate Systems
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage("--- Initialize CoordinateSystems in LOS\n");
   #endif
   InitializeObjectsInTheMap(LOS, Gmat::COORDINATE_SYSTEM, "");
   
   if (includeGOS)
   {
      #ifdef DEBUG_INITIALIZE_OBJ
      MessageInterface::ShowMessage("--- Initialize CoordinateSystems in GOS\n");
      #endif
      InitializeObjectsInTheMap(GOS, Gmat::COORDINATE_SYSTEM, "");
   }
   
   // Spacecraft
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage("--- Initialize Spacecrafts in LOS\n");
   #endif
   InitializeObjectsInTheMap(LOS, Gmat::SPACECRAFT, "");
   
   if (includeGOS)
   {
      #ifdef DEBUG_INITIALIZE_OBJ
      MessageInterface::ShowMessage("--- Initialize Spacecrafts in GOS\n");
      #endif
      InitializeObjectsInTheMap(GOS, Gmat::SPACECRAFT, "");
   }
   
   // MeasurementModel
   // Measurement Models must init before the Estimators/Simulator, so do next
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage("--- Initialize MeasurementModels in LOS\n");
   #endif
   InitializeObjectsInTheMap(LOS, Gmat::MEASUREMENT_MODEL, "");
   
   if (includeGOS)
   {
      #ifdef DEBUG_INITIALIZE_OBJ
      MessageInterface::ShowMessage("--- Initialize MeasurementModels in GOS\n");
      #endif
      InitializeObjectsInTheMap(GOS, Gmat::MEASUREMENT_MODEL, "");
   }
   
   // System Parameters
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage("--- Initialize Sytem Parameters in LOS\n");
   #endif
   InitializeSystemParamters(LOS);
   
   if (includeGOS)
   {
      #ifdef DEBUG_INITIALIZE_OBJ
      MessageInterface::ShowMessage("--- Initialize Sytem Parameters in LOS\n");
      #endif
      InitializeSystemParamters(GOS);
   }
   
   // Parameters
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage("--- Initialize Parameters in LOS\n");
   #endif
   InitializeObjectsInTheMap(LOS, Gmat::PARAMETER, "");
   
   if (includeGOS)
   {
      #ifdef DEBUG_INITIALIZE_OBJ
      MessageInterface::ShowMessage("--- Initialize Parameters in GOS\n");
      #endif
      InitializeObjectsInTheMap(GOS, Gmat::PARAMETER, "");
   }
   
   // Subscribers
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage("--- Initialize Subscriber in LOS\n");
   #endif
   InitializeObjectsInTheMap(LOS, Gmat::SUBSCRIBER, "");
   
   if (includeGOS)
   {
      #ifdef DEBUG_INITIALIZE_OBJ
      MessageInterface::ShowMessage("--- Initialize Subscriber in GOS\n");
      #endif
      InitializeObjectsInTheMap(GOS, Gmat::SUBSCRIBER, "");
   }
   
   // All other objects
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage("--- Initialize All other objects in LOS\n");
   #endif
   InitializeAllOtherObjects(LOS);
   
   if (includeGOS)
   {
      #ifdef DEBUG_INITIALIZE_OBJ
      MessageInterface::ShowMessage("--- Initialize All other objects in GOS\n");
      #endif
      InitializeAllOtherObjects(GOS);
   }
   
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("ObjectInitializer: Objects Initialized ...\n");
   #endif
      
   #ifdef DEBUG_TRACE
   clock_t t2 = clock();
   MessageInterface::ShowMessage
      ("=== ObjectInitializer::InitializeObjects() Count = %d, Run Time: %f seconds\n",
       callCount, (Real)(t2-t1)/CLOCKS_PER_SEC);
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// void SetObjectJ2000Body(ObjectMap *objMap)
//------------------------------------------------------------------------------
/*
 * Sets J2000Body pointer to SpacePoint objects in the map
 */
//------------------------------------------------------------------------------
void ObjectInitializer::SetObjectJ2000Body(ObjectMap *objMap)
{
   std::map<std::string, GmatBase *>::iterator omi;
   for (omi = objMap->begin(); omi != objMap->end(); ++omi)
   {
      GmatBase *obj = omi->second;
      if (obj->IsOfType(Gmat::SPACE_POINT))
      {
         #ifdef DEBUG_OBJECT_INITIALIZER
            MessageInterface::ShowMessage(
               "Setting J2000 Body for %s in the ObjectInitializer\n",
               obj->GetName().c_str());
         #endif
         SpacePoint *spObj = (SpacePoint *)obj;
         SpacePoint* j2k = FindSpacePoint(spObj->GetJ2000BodyName());
         if (j2k)
            spObj->SetJ2000Body(j2k);
         else
            throw GmatBaseException("ObjectInitializer did not find the Spacepoint \"" +
               spObj->GetJ2000BodyName() + "\"");
      }
   }
}


//------------------------------------------------------------------------------
// void InitializeObjectsInTheMap(ObjectMap *objMap, ...)
//------------------------------------------------------------------------------
/*
 * Initializes specific types of objects in the map. if objType is UNDEFINED_OBJECT,
 * it will check for objTypeName to retrieve objects.
 *
 * @param objMap the object map to be used for retrieving objects
 * @param objType the objec type to be used for retrieving objects
 * @param objTypeName the objec type name to be used for retrieving objects
 */
//------------------------------------------------------------------------------
void ObjectInitializer::InitializeObjectsInTheMap(ObjectMap *objMap,
                                                  Gmat::ObjectType objType,
                                                  const std::string objTypeName)
{
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage
      ("InitializeObjectsInTheMap() entered, objMap=<%p>, objType=%d, objTypeName='%s'\n"
       "   registerSubs=%d\n", objMap, objType, objTypeName.c_str(), registerSubscribers);
   #endif
   
   std::map<std::string, GmatBase *>::iterator omi;
   for (omi = objMap->begin(); omi != objMap->end(); ++omi)
   {
      GmatBase *obj = omi->second;
      if (objType != Gmat::UNKNOWN_OBJECT)
      {
         if (obj->IsOfType(objType))
         {
            #ifdef DEBUG_OBJECT_INITIALIZER
            MessageInterface::ShowMessage
               ("Initializing <%p><%s>'%s'\n", obj, obj->GetTypeName().c_str(),
                obj->GetName().c_str());
            #endif
            if (obj->IsOfType(Gmat::COORDINATE_SYSTEM))
            {
               obj->SetSolarSystem(ss);
               BuildReferences(obj);
               InitializeCoordinateSystem((CoordinateSystem *)obj);
               obj->Initialize();
            }
            else if (obj->IsOfType(Gmat::SPACECRAFT))
            {
               obj->SetSolarSystem(ss);
               ((Spacecraft *)obj)->SetInternalCoordSystem(internalCS);
               BuildReferences(obj);
               
               // Setup spacecraft hardware
               BuildAssociations(obj);
               obj->Initialize();
            }
            else
            {
               BuildReferencesAndInitialize(obj);    
            }
            
            // Check if we need to register subscribers to the publisher
            if (objType == Gmat::SUBSCRIBER && registerSubscribers)
            {
               #ifdef DEBUG_SUBSCRIBER
               MessageInterface::ShowMessage
                  ("ObjectInitializer registering global subscriber '%s' of type '%s' "
                   "to publisher\n", obj->GetName().c_str(), obj->GetTypeName().c_str());
               #endif
               publisher->Subscribe((Subscriber*)obj);
            }
         }
         else if (obj->IsOfType(objTypeName))
         {
            BuildReferencesAndInitialize(obj);
         }
      }
   }
}


//------------------------------------------------------------------------------
// void InitializeSystemParamters(ObjectMap *objMap)
//------------------------------------------------------------------------------
void ObjectInitializer::InitializeSystemParamters(ObjectMap *objMap)
{
   std::map<std::string, GmatBase *>::iterator omi;
   for (omi = objMap->begin(); omi != objMap->end(); ++omi)
   {
      GmatBase *obj = omi->second;
      
      // Treat parameters as a special case -- because system parameters have
      // to be initialized before other parameters.
      if (obj->IsOfType(Gmat::PARAMETER))
      {
         Parameter *param = (Parameter *)obj;
         // Make sure system parameters are configured before others
         if (param->GetKey() == GmatParam::SYSTEM_PARAM)
         {
            #ifdef DEBUG_OBJECT_INITIALIZER
               MessageInterface::ShowMessage(
                  "ObjectInitializer::Initialize objTypeName = %s, objName = %s\n",
                  obj->GetTypeName().c_str(), obj->GetName().c_str());
            #endif
            
            BuildReferencesAndInitialize(obj);
         }
      }
   }
}


//------------------------------------------------------------------------------
// void InitializeAllOtherObjects(ObjectMap *objMap)
//------------------------------------------------------------------------------
/*
 * Initializes the rest of objects in the map.
 */
//------------------------------------------------------------------------------
void ObjectInitializer::InitializeAllOtherObjects(ObjectMap *objMap)
{
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage
      ("ObjectInitializer::InitializeAllOtherObjects() entered\n");
   #endif
   
   std::map<std::string, GmatBase *>::iterator omi;
   for (omi = objMap->begin(); omi != objMap->end(); ++omi)
   {
      GmatBase *obj = omi->second;
      if ((obj->GetType() != Gmat::COORDINATE_SYSTEM) &&
          (obj->GetType() != Gmat::SPACECRAFT) &&
          (obj->GetType() != Gmat::MEASUREMENT_MODEL) &&
          (obj->GetType() != Gmat::PARAMETER) &&
          (obj->GetType() != Gmat::SUBSCRIBER))
      {
         #ifdef DEBUG_OBJECT_INITIALIZER
         MessageInterface::ShowMessage
            ("ObjectInitializer::Initialize objTypeName = %s, objName = %s\n",
             obj->GetTypeName().c_str(), obj->GetName().c_str());
         #endif
         if (obj->GetType() == Gmat::PROP_SETUP)
         {
            // PropSetup initialization is handled by the commands, since the
            // state that is propagated may change as spacecraft are added or
            // removed.
            BuildReferences(obj);
         }
         else
         {
            if (obj->IsOfType(Gmat::SPACE_POINT)       ||
                obj->IsOfType(Gmat::BURN)              ||
                obj->IsOfType(Gmat::HARDWARE)          ||
                obj->IsOfType("Estimator")             ||
                obj->IsOfType("Simulator"))
            {
               BuildReferencesAndInitialize(obj);
            }
         }
      }
   }
}


//------------------------------------------------------------------------------
// void InitializeInternalObjects()
//------------------------------------------------------------------------------
/**
 *  Initializes internal objects in the sandbox.
 */
//------------------------------------------------------------------------------
void ObjectInitializer::InitializeInternalObjects()
{
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("Entering InitializeInternalObjects ...\n");
   #endif
   SpacePoint *sp, *j2kBod;
   std::string j2kName, oName;
   
   if (ss == NULL)
      throw GmatBaseException("ObjectInitializer::InitializeInternalObjects() "
                              "The Solar System pointer is NULL");
   
   if (internalCS == NULL)
      throw GmatBaseException("ObjectInitializer::InitializeInternalObjects() "
                              "The Internal Coordinate System pointer is NULL");
   
   ss->Initialize();
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage
      ("--- The object <%p><%s>'%s' initialized\n",  ss, ss->GetTypeName().c_str(),
       ss->GetName().c_str());
   #endif
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage(" ... and solar system is initialized  ...\n");
   #endif
   
   // Set J2000 bodies for solar system objects -- should this happen here?
   const StringArray biu = ss->GetBodiesInUse();
   for (StringArray::const_iterator i = biu.begin(); i != biu.end(); ++i)
   {
      #ifdef DEBUG_OBJECT_INITIALIZER
         MessageInterface::ShowMessage(" ... initializing body %s ...\n", (*i).c_str());
      #endif
      sp = ss->GetBody(*i);
      j2kName = sp->GetStringParameter("J2000BodyName");
      j2kBod = FindSpacePoint(j2kName);
      sp->SetJ2000Body(j2kBod);
   }

   // set ref object for internal coordinate system
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage(" ... solar system about to be set on coordinate system  ...\n");
      //if (!internalCS) MessageInterface::ShowMessage(" but solar system is NULL!!!!!!\n");
   #endif
   
   internalCS->SetSolarSystem(ss);
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage(" ... and solar system is set on coordinate system  ...\n");
      MessageInterface::ShowMessage(" ... about to call BuildReferences  ...\n");
   #endif

   BuildReferences(internalCS);

   // Set reference origin for internal coordinate system.
   oName = internalCS->GetStringParameter("Origin");
   sp = FindSpacePoint(oName);
   if (sp == NULL)
      throw GmatBaseException("Cannot find SpacePoint named \"" +
         oName + "\" used for the internal coordinate system origin");
   internalCS->SetRefObject(sp, Gmat::SPACE_POINT, oName);


   // Set J2000 body for internal coordinate system
   oName = internalCS->GetStringParameter("J2000Body");
   sp = FindSpacePoint(oName);
   if (sp == NULL)
      throw GmatBaseException("Cannot find SpacePoint named \"" +
         oName + "\" used for the internal coordinate system J2000 body");
   internalCS->SetRefObject(sp, Gmat::SPACE_POINT, oName);

   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage(" ... about to call Initialize on internalCS  ...\n");
   #endif
      
   internalCS->Initialize();
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage
      ("--- The object <%p><%s>'%s' initialized\n",  internalCS,
       internalCS->GetTypeName().c_str(), internalCS->GetName().c_str());
   #endif
}


//------------------------------------------------------------------------------
// void InitializeCoordinateSystem(GmatBase *obj)
//------------------------------------------------------------------------------
/*
 * Sets reference objects of CoordianteSystem object and owned objects.
 */
//------------------------------------------------------------------------------
void ObjectInitializer::InitializeCoordinateSystem(GmatBase *obj)
{
   #ifdef DEBUG_INITIALIZE_CS
   MessageInterface::ShowMessage
      ("Entering ObjectInitializer::InitializeCoordinateSystem(), obj=<%p><%s>'%s'\n",
       obj, obj->GetTypeName().c_str(), obj->GetName().c_str() );
   #endif
   
   if (!obj->IsOfType(Gmat::COORDINATE_SYSTEM))
   {
      #ifdef DEBUG_INITIALIZE_CS
      MessageInterface::ShowMessage
         ("The object '%s' is not of CoordinateSystem\n", obj->GetName().c_str());
      #endif
      return;
   }
   
   SpacePoint *sp;
   CoordinateSystem *cs = (CoordinateSystem*)obj;
   std::string oName;   
   
   // Set the reference objects for the coordinate system
   BuildReferences(cs);
   
   oName = cs->GetStringParameter("Origin");
   
   GmatBase *axes = cs->GetOwnedObject(0);
   BuildReferences(axes);
   
   sp = FindSpacePoint(oName);
   if (sp == NULL)
      throw GmatBaseException("Cannot find SpacePoint named \"" +
         oName + "\" used for the coordinate system " +
         cs->GetName() + " origin");
   
   cs->SetRefObject(sp, Gmat::SPACE_POINT, oName);
   
   oName = cs->GetStringParameter("J2000Body");
   
   sp = FindSpacePoint(oName);
   if (sp == NULL)
      throw GmatBaseException("Cannot find SpacePoint named \"" +
         oName + "\" used for the coordinate system " +
         cs->GetName() + " J2000 body");
   
   cs->SetRefObject(sp, Gmat::SPACE_POINT, oName);
   #ifdef DEBUG_INITIALIZE_CS
   MessageInterface::ShowMessage
      ("Exiting ObjectInitializer::InitializeCoordinateSystem()\n");
   #endif
}


//------------------------------------------------------------------------------
// void BuildReferencesAndInitialize(GmatBase *obj)
//------------------------------------------------------------------------------
/**
 *  Sets all reference objects for the input object and initialize
 */
//------------------------------------------------------------------------------
void ObjectInitializer::BuildReferencesAndInitialize(GmatBase *obj)
{   
   BuildReferences(obj);
   obj->Initialize();
   
   #ifdef DEBUG_INITIALIZE_OBJ
   MessageInterface::ShowMessage
      ("--- The object <%p><%s>'%s' initialized\n",  obj,
       obj->GetTypeName().c_str(), obj->GetName().c_str());
   #endif
}


//------------------------------------------------------------------------------
// void BuildReferences()
//------------------------------------------------------------------------------
/**
 *  Sets all reference objects for the input object.
 */
//------------------------------------------------------------------------------
void ObjectInitializer::BuildReferences(GmatBase *obj)
{
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage
         ("Entering ObjectInitializer::BuildReferences, object type = '%s'\n",
          obj->GetTypeName().c_str());
   #endif
   std::string oName;
   
   obj->SetSolarSystem(ss);
   obj->SetInternalCoordSystem(internalCS);
   
   // PropSetup probably should do this...
   if ((obj->IsOfType(Gmat::PROP_SETUP)) ||
       (obj->IsOfType(Gmat::ODE_MODEL)))
   {
      #ifdef DEBUG_OBJECT_INITIALIZER
         MessageInterface::ShowMessage("--- it is a PropSetup or ODEModel ...\n");
      #endif
      ODEModel *fm = ((PropSetup *)obj)->GetODEModel();
      fm->SetSolarSystem(ss);
      
      // Handle the coordinate systems
      StringArray csList = fm->GetStringArrayParameter("CoordinateSystemList");
      
      #ifdef DEBUG_OBJECT_INITIALIZER
         MessageInterface::ShowMessage("Coordinate system list for '%s':\n",
            fm->GetName().c_str());
         for (StringArray::iterator i = csList.begin(); i != csList.end(); ++i)
            MessageInterface::ShowMessage("   %s\n", i->c_str());
      #endif
      // Set CS's on the objects
      for (StringArray::iterator i = csList.begin(); i != csList.end(); ++i)
      {
         CoordinateSystem *fixedCS = NULL;

         if (LOS->find(*i) != LOS->end())
         {
            GmatBase *ref = (*LOS)[*i];
            if (ref->IsOfType("CoordinateSystem") == false)
               throw GmatBaseException("Object named " + (*i) + 
                  " was expected to be a Coordinate System, but it has type " +
                  ref->GetTypeName());
            fixedCS = (CoordinateSystem*)ref;
            fm->SetRefObject(fixedCS, fixedCS->GetType(), *i); 
         }
         else if (GOS->find(*i) != GOS->end())
         {
            GmatBase *ref = (*GOS)[*i];
            if (ref->IsOfType("CoordinateSystem") == false)
               throw GmatBaseException("Object named " + (*i) + 
                  " was expected to be a Coordinate System, but it has type " +
                  ref->GetTypeName());
            fixedCS = (CoordinateSystem*)ref;
            fm->SetRefObject(fixedCS, fixedCS->GetType(), *i); 
         }
         else
         {
            //MessageInterface::ShowMessage("===> create BodyFixed here\n");
            fixedCS = mod->CreateCoordinateSystem("", false);
            AxisSystem *axes = mod->CreateAxisSystem("BodyFixed", "");
            fixedCS->SetName(*i);
            fixedCS->SetRefObject(axes, Gmat::AXIS_SYSTEM, "");
            fixedCS->SetOriginName(fm->GetStringParameter("CentralBody"));
            
            // Since CoordinateSystem clones AxisSystem, delete it from here
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Remove
               (axes, "localAxes", "ObjectInitializer::BuildReferences()",
                "deleting localAxes");
            #endif
            delete axes;
            
            fm->SetRefObject(fixedCS, fixedCS->GetType(), *i);
            
            fixedCS->SetSolarSystem(ss);
            BuildReferences(fixedCS); 
            InitializeCoordinateSystem(fixedCS);
            fixedCS->Initialize();
            #ifdef DEBUG_INITIALIZE_OBJ
            MessageInterface::ShowMessage
               ("--- The object <%p><%s>'%s' initialized\n",  internalCS,
                internalCS->GetTypeName().c_str(), internalCS->GetName().c_str());
            #endif
            // if things have already been moved to the globalObjectStore, put it there
            if ((GOS->size() > 0) && (fixedCS->GetIsGlobal()))
               (*GOS)[*i] = fixedCS;
            // otherwise, put it in the Sandbox object map - it will be moved to the GOS later
            else
               (*LOS)[*i] = fixedCS;
            
            #ifdef DEBUG_OBJECT_INITIALIZER
               MessageInterface::ShowMessage(
                  "Coordinate system %s has body %s\n",
                  fixedCS->GetName().c_str(), fixedCS->GetOriginName().c_str());
            #endif
         }
      }
      
      #ifdef DEBUG_OBJECT_INITIALIZER
         MessageInterface::ShowMessage(
            "Initializing force model references for '%s'\n",
            (fm->GetName() == "" ? obj->GetName().c_str() :
                                   fm->GetName().c_str()) );
      #endif

      try
      {
         StringArray fmRefs = fm->GetRefObjectNameArray(Gmat::UNKNOWN_OBJECT);
         #ifdef DEBUG_OBJECT_INITIALIZER
            MessageInterface::ShowMessage("fmRefs are:\n");
            for (unsigned int ii=0; ii < fmRefs.size(); ii++)
               MessageInterface::ShowMessage(" --- %s\n", (fmRefs.at(ii)).c_str());
         #endif
         for (StringArray::iterator i = fmRefs.begin();
              i != fmRefs.end(); ++i)
         {
            oName = *i;
            try
            {
               SetRefFromName(fm, oName);
            }
            catch (GmatBaseException &ex)  // *******
            {
               // Handle SandboxExceptions first.
               #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
                  MessageInterface::ShowMessage(
                     "RefObjectName " + oName + " not found; ignoring " +
                     ex.GetFullMessage() + "\n");
               #endif
               //throw ex;
            }
            catch (BaseException &ex)
            {
               // Post a message and -- otherwise -- ignore the exceptions
               // Handle SandboxExceptions first.
               #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
                  MessageInterface::ShowMessage(
                     "RefObjectName not found; ignoring " +
                     ex.GetFullMessage() + "\n");
               #endif
            }
         }
      }
      catch (GmatBaseException &ex) // **********
      {
         // Handle SandboxExceptions first.
         #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
            MessageInterface::ShowMessage(
               "RefObjectNameArray not found; ignoring " +
               ex.GetFullMessage() + "\n");
         #endif
         //throw ex;
      }
      catch (BaseException &ex) // Handles no refObject array
      {
         // Post a message and -- otherwise -- ignore the exceptions
         #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
            MessageInterface::ShowMessage(
               "RefObjectNameArray not found; ignoring " +
               ex.GetFullMessage() + "\n");
         #endif
      }

      if (obj->IsOfType(Gmat::ODE_MODEL))
         return;
   }


   try
   {
      // First set the individual reference objects
      #ifdef DEBUG_OBJECT_INITIALIZER
         MessageInterface::ShowMessage(
               "Attempting to set individual reference objects ...\n");
      #endif
      oName = obj->GetRefObjectName(Gmat::UNKNOWN_OBJECT);
      #ifdef DEBUG_OBJECT_INITIALIZER
         MessageInterface::ShowMessage(
               "Attempting to set individual reference objects with name = %s...\n",
               oName.c_str());
      #endif
      SetRefFromName(obj, oName);
   }
   catch (SubscriberException &ex)
   {
      throw ex;
   }
   catch (GmatBaseException &ex) // ************
   {
      // Handle SandboxExceptions first.
      // For now, post a message and -- otherwise -- ignore exceptions
      #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
         MessageInterface::ShowMessage("RefObjectName not found; ignoring " +
            ex.GetFullMessage() + "\n");
      #endif
      //throw ex;
   }
   catch (BaseException &ex)
   {
      // Post a message and -- otherwise -- ignore the exceptions
      #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
         MessageInterface::ShowMessage("RefObjectName not found; ignoring " +
            ex.GetFullMessage() + "\n");
      #endif
   }
   
   
   // Next handle the array version
   try
   {
      StringArray oNameArray =
         obj->GetRefObjectNameArray(Gmat::UNKNOWN_OBJECT);
      for (StringArray::iterator i = oNameArray.begin();
           i != oNameArray.end(); ++i)
      {
         oName = *i;
         #ifdef DEBUG_OBJECT_INITIALIZER
            MessageInterface::ShowMessage(
                  "Attempting to set ARRAY object with name = %s...\n",
                  oName.c_str());
         #endif
         try
         {
            SetRefFromName(obj, oName);
         }
         catch (GmatBaseException &ex) // ***********
         {
            // Handle SandboxExceptions first.
            #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
               MessageInterface::ShowMessage(
                  "RefObjectName " + oName + " not found; ignoring " +
                  ex.GetFullMessage() + "\n");
            #endif
            //throw ex;
         }
         catch (SubscriberException &ex)
         {
            throw ex;
         }
         catch (BaseException &ex)
         {
            // Post a message and -- otherwise -- ignore the exceptions
            // Handle SandboxExceptions first.
            #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
               MessageInterface::ShowMessage(
                  "RefObjectName not found; ignoring " +
                  ex.GetFullMessage() + "\n");
            #endif
         }
      }
   }
   catch (GmatBaseException &ex)  // ***********
   {
      // Handle SandboxExceptions first.
      #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
         MessageInterface::ShowMessage(
            "RefObjectNameArray not found; ignoring " +
            ex.GetFullMessage() + "\n");
      #endif
      //throw ex;
   }
   catch (SubscriberException &ex)
   {
      throw ex;
   }
   catch (BaseException &ex) // Handles no refObject array
   {
      // Post a message and -- otherwise -- ignore the exceptions
      #ifdef DEBUG_OBJECT_INITIALIZER_DETAILED
         MessageInterface::ShowMessage(
            "RefObjectNameArray not found; ignoring " +
            ex.GetFullMessage() + "\n");
      #endif
   }
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("Exiting BuildReferences ...\n");
   #endif
}



//------------------------------------------------------------------------------
// void SetRefFromName(GmatBase *obj, const std::string &oName)
//------------------------------------------------------------------------------
/**
 *  Sets a reference object on an object.
 *
 *  @param <obj>   The object that needs to set the reference.
 *  @param <oName> The name of the reference object.
 */
//------------------------------------------------------------------------------
void ObjectInitializer::SetRefFromName(GmatBase *obj, const std::string &oName)
{
   #ifdef DEBUG_OBJECT_INITIALIZER
      MessageInterface::ShowMessage("Setting reference '%s' on '%s'\n", 
         oName.c_str(), obj->GetName().c_str());
   #endif
      
   GmatBase *refObj = NULL;
   
   if ((refObj = FindObject(oName)) != NULL)
      obj->SetRefObject(refObj, refObj->GetType(), refObj->GetName());
   else
   {
      // look in the SolarSystem
      refObj = FindSpacePoint(oName);
      
      if (refObj == NULL)
         throw GmatBaseException("Unknown object " + oName + " requested by " +
                                  obj->GetName());

      obj->SetRefObject(refObj, refObj->GetType(), refObj->GetName());
   }
}


//------------------------------------------------------------------------------
// void BuildAssociations(GmatBase * obj)
//------------------------------------------------------------------------------
/**
 *  Assigns clones of objects to their owners.
 *
 *  This method finds referenced objects that need to be associated with the
 *  input object through cloning, creates the clones, and hands the cloned
 *  object to the owner.
 *
 *  An example of the associations that are made here are hardware elements that
 *  get associated with spacecraft.  Users configure a single element -- for
 *  example, a tank, and then can assign that element to many different
 *  spacecraft.  In order to avoid multiple objects using the same instance of
 *  the tank, clones are made for each spacecraft that has the tank associated
 *  with it.
 *
 *  @param <obj> The owner of the clones.
 */
//------------------------------------------------------------------------------
void ObjectInitializer::BuildAssociations(GmatBase * obj)
{
   std::string objName = obj->GetName();
   std::string objType = obj->GetTypeName();
   
   #ifdef DEBUG_BUILD_ASSOCIATIONS
   MessageInterface::ShowMessage
      ("ObjectInitializer::BuildAssociations() entered, obj=<%p><%s>'%s'\n",
       obj, objType.c_str(), objName.c_str());
   #endif
   // Spacecraft receive clones of the associated hardware objects
   if (obj->IsOfType(Gmat::SPACECRAFT))
   {
      StringArray hw = obj->GetRefObjectNameArray(Gmat::HARDWARE);
      for (StringArray::iterator i = hw.begin(); i < hw.end(); ++i)
      {
         #ifdef DEBUG_BUILD_ASSOCIATIONS
            MessageInterface::ShowMessage
               ("ObjectInitializer::BuildAssociations() setting \"%s\" on \"%s\"\n",
                i->c_str(), objName.c_str());
         #endif
         
         GmatBase *elem = NULL;
         if ((elem = FindObject(*i)) == NULL)
            throw GmatBaseException("ObjectInitializer::BuildAssociations: Cannot find "
                                    "hardware element \"" + (*i) + "\"\n");
         
         // To handle Spacecraft hardware setting inside the function,
         // all hardware are cloned in the SetRefObject() method. (LOJ: 2009.07.24)
         #ifdef __CLONE_HARDWARE_IN_OBJ_INITIALIZER__
            GmatBase *newElem = elem->Clone();
            #ifdef DEBUG_MEMORY
            MemoryTracker::Instance()->Add
               (newElem, newElem->GetName(), "ObjectInitializer::BuildAssociations()",
                "newElem = elem->Clone()");
            #endif         
            #ifdef DEBUG_BUILD_ASSOCIATIONS
               MessageInterface::ShowMessage
                  ("ObjectInitializer::BuildAssociations() created clone \"%s\" of type \"%s\"\n",
                  newElem->GetName().c_str(), newElem->GetTypeName().c_str());
            #endif
         #else
            // Spacecraft will clone the hardware in SetRefObject()
            GmatBase *newElem = elem;
         #endif
         
         // now set Hardware to Spacecraft
         if (!obj->SetRefObject(newElem, newElem->GetType(), newElem->GetName()))
         {
            MessageInterface::ShowMessage
               ("ObjectInitializer::BuildAssociations() failed to set %s\n",
                newElem->GetName().c_str());
            // Should we throw an exception here?
         }
         else
         {
            #ifdef DEBUG_BUILD_ASSOCIATIONS
            MessageInterface::ShowMessage
               ("ObjectInitializer::BuildAssociations() successfully set <%p>'%s' to '%s'\n",
                newElem, newElem->GetName().c_str(), objName.c_str());
            #endif
            
            // Set SolarSystem and Spacecraft to Thruster since it needs coordinate
            // conversion during Thruster initialization (LOJ: 2009.03.05)
            // Set CoordinateSystem (LOJ: 2009.05.05)
            if (newElem->IsOfType(Gmat::THRUSTER))
            {
               newElem->SetSolarSystem(ss);
               newElem->SetRefObject(obj, Gmat::SPACECRAFT, objName);
               std::string csName = newElem->GetRefObjectName(Gmat::COORDINATE_SYSTEM);
               if (csName != "")
                  newElem->SetRefObject(FindObject(csName), Gmat::COORDINATE_SYSTEM, csName);
               newElem->Initialize();
               
               // Now add Thruster's CoordinateSystem to Spacecraft so that it can set it
               // to cloned Thruster
               StringArray objRefs = obj->GetRefObjectNameArray(Gmat::COORDINATE_SYSTEM);
               for (StringArray::iterator i = objRefs.begin(); i != objRefs.end(); ++i)
               {
                  csName = *i;
                  GmatBase *refObj = FindObject(csName);
                  if (refObj == NULL)
                     throw GmatBaseException("Unknown object " + csName + " requested by " +
                                             obj->GetName());
                  obj->TakeAction("ThrusterSettingMode", "On");
                  obj->SetRefObject(refObj, refObj->GetType(), refObj->GetName());                  
                  obj->TakeAction("ThrusterSettingMode", "Off");
               }
            }
            
            #ifdef DEBUG_BUILD_ASSOCIATIONS
            MessageInterface::ShowMessage
               ("ObjectInitializer::BuildAssociations() Calling <%p>'%s'->"
                "TakeAction(SetupHardware)\n", obj, objName.c_str());
            #endif
            obj->TakeAction("SetupHardware");
            
         }
      }
   }
   
   #ifdef DEBUG_BUILD_ASSOCIATIONS
   MessageInterface::ShowMessage
      ("ObjectInitializer::BuildAssociations() leaving\n");
   #endif
}


//------------------------------------------------------------------------------
// SpacePoint* FindSpacePoint(const std::string &spname)
//------------------------------------------------------------------------------
/**
 *  Finds a SpacePoint by name.
 *
 *  @param <spname> The name of the SpacePoint.
 *
 *  @return A pointer to the SpacePoint, or NULL if it does not exist in the
 *          stores.
 */
//------------------------------------------------------------------------------
SpacePoint * ObjectInitializer::FindSpacePoint(const std::string &spName)
{
   SpacePoint *sp = ss->GetBody(spName);


   if (sp == NULL)
   {
      GmatBase *spObj;
      if ((spObj = FindObject(spName)) != NULL)
      {
         if (spObj->IsOfType(Gmat::SPACE_POINT))
            sp = (SpacePoint*)(spObj);
      }
   }

   return sp;
}

//------------------------------------------------------------------------------
// GmatBase* ObjectInitializer::FindObject(const std::string &name)
//------------------------------------------------------------------------------
/**
 *  Finds an object by name, searching through the LOS first,
 *  then the GOS
 *
 *  @param <name> The name of the object.
 *
 *  @return A pointer to the object, or NULL if it does not exist in the
 *          object stores.
 */
//------------------------------------------------------------------------------
GmatBase* ObjectInitializer::FindObject(const std::string &name)
{
   if (LOS->find(name) == LOS->end())
   {
     // If not found in the LOS, check the Global Object Store (GOS)
      if (includeGOS)
      {
         if (GOS->find(name) == GOS->end())
            return NULL;
         else return (*GOS)[name];
      }
      else return NULL;
   }
   else
      return (*LOS)[name];
}

//------------------------------------------------------------------------------
// ObjectInitializer::ObjectInitializer()
//------------------------------------------------------------------------------
ObjectInitializer::ObjectInitializer() :
   ss         (NULL),
   LOS        (NULL),
   GOS        (NULL),
   mod        (NULL),
   internalCS (NULL),
   includeGOS (false)
{
}

