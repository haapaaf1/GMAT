//$Header$
//------------------------------------------------------------------------------
//                                ConfigManager
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/10/27
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Configuration manager used to manage configured (i.e. named) GMAT objects.
 */
//------------------------------------------------------------------------------


#include "ConfigManager.hpp"
#include "ConfigManagerException.hpp"

//#define DEBUG_RENAME 1
//#define DEBUG_CONFIG_MEMORY

//---------------------------------
// static members
//---------------------------------


ConfigManager* ConfigManager::theConfigManager = NULL;


//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// ConfigManager* Instance()
//------------------------------------------------------------------------------
/**
 * Accessor method used to obtain the singleton.
 *
 * @return the singleton instance of the configuration manager.
 */
//------------------------------------------------------------------------------
ConfigManager* ConfigManager::Instance()
{
   if (!theConfigManager)
      theConfigManager = new ConfigManager;
        
   return theConfigManager;
}


//------------------------------------------------------------------------------
// ConfigManager()
//------------------------------------------------------------------------------
/**
 * Constructor.
 */
//------------------------------------------------------------------------------
ConfigManager::ConfigManager()
{
}

// class destructor
//------------------------------------------------------------------------------
// ~ConfigManager()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
ConfigManager::~ConfigManager()
{
   RemoveAllItems();
}

//------------------------------------------------------------------------------
// void AddPhysicalModel(PhysicalModel *pm)
//------------------------------------------------------------------------------
/**
 * Adds a PhysicalModel to the configuration.
 *
 * @param pm Pointer to the PhysicalModel instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddPhysicalModel(PhysicalModel *pm)
{
   std::string name = pm->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!pm->IsOfType(Gmat::PHYSICAL_MODEL))
      throw ConfigManagerException(name + " is not a PhysicalModel");

   AddObject(pm);
}

//------------------------------------------------------------------------------
// void ConfigManager::AddPropagator(Propagator *prop)
//------------------------------------------------------------------------------
/**
 * Adds a Propagator to the configuration.
 *
 * @param prop Pointer to the Propagator instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddPropagator(Propagator *prop)
{
   std::string name = prop->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!prop->IsOfType(Gmat::PROPAGATOR))
      throw ConfigManagerException(name + " is not a Propagator");

   AddObject(prop);
}


//------------------------------------------------------------------------------
// void AddForceModel(ForceModel *fm)
//------------------------------------------------------------------------------
/**
 * Adds a ForceModel to the configuration.
 *
 * @param fm Pointer to the ForceModel instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddForceModel(ForceModel *fm)
{
   std::string name = fm->GetName();

   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!fm->IsOfType(Gmat::FORCE_MODEL))
      throw ConfigManagerException(name + " is not a ForceModel");

   AddObject(fm);
}


//------------------------------------------------------------------------------
// void AddSubscriber(Subscriber *subs)
//------------------------------------------------------------------------------
/**
 * Adds a Subscriber to the configuration.
 *
 * @param subs Pointer to the Subscriber.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddSubscriber(Subscriber *subs)
{
   std::string name = subs->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!subs->IsOfType(Gmat::SUBSCRIBER))
      throw ConfigManagerException(name + " is not a Subscriber");

   AddObject(subs);
}


//------------------------------------------------------------------------------
// void AddSolarSystem(SolarSystem *solarSys)
//------------------------------------------------------------------------------
/**
 * Adds a SolarSystem to the configuration.
 *
 * @param solarSys Pointer to the SolarSystem instance.
 *
 * @todo Add solar systems to the ConfigManager
 */
//------------------------------------------------------------------------------
void ConfigManager::AddSolarSystem(SolarSystem *solarSys)
{
   throw ConfigManagerException("SolarSystem objects are not yet managed");
}


//------------------------------------------------------------------------------
// void AddPropSetup(PropSetup* propSetup)
//------------------------------------------------------------------------------
/**
 * Adds a PropSetup to the configuration.
 *
 * @param propSetup Pointer to the PropSetup instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddPropSetup(PropSetup* propSetup)
{
   std::string name = propSetup->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!propSetup->IsOfType(Gmat::PROP_SETUP))
      throw ConfigManagerException(name + " is not a PropSetup");

   AddObject(propSetup);
}


//------------------------------------------------------------------------------
// void AddSpacecraft(Spacecraft *sc)
//------------------------------------------------------------------------------
/**
 * Adds a Spacecraft to the configuration.
 *
 * @param sc Pointer to the Spacecraft instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddSpacecraft(SpaceObject *sc)
{
   std::string name = sc->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!sc->IsOfType(Gmat::SPACECRAFT))
      throw ConfigManagerException(name + " is not a Spacecraft");

   AddObject(sc);
}


//------------------------------------------------------------------------------
// void AddHardware(Hardware *hw)
//------------------------------------------------------------------------------
/**
 * Adds a Hardware object to the configuration.
 *
 * @param hw Pointer to the Hardware object.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddHardware(Hardware *hw)
{
   std::string name = hw->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!hw->IsOfType(Gmat::HARDWARE))
      throw ConfigManagerException(name + " is not Hardware");

   AddObject(hw);
}


//------------------------------------------------------------------------------
// void AddStopCondition(StopCondition* stopCond)
//------------------------------------------------------------------------------
/**
 * Adds a StopCondition to the configuration.
 *
 * @param stopCond Pointer to the StopCondition instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddStopCondition(StopCondition* stopCond)
{
   std::string name = stopCond->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!stopCond->IsOfType(Gmat::STOP_CONDITION))
      throw ConfigManagerException(name + " is not a StopCondition");

   AddObject(stopCond);
}


//------------------------------------------------------------------------------
// void AddParameter(Parameter* parameter)
//------------------------------------------------------------------------------
/**
 * Adds a Parameter to the configuration.
 *
 * @param parameter Pointer to the Parameter instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddParameter(Parameter* parameter)
{
   std::string name = parameter->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!parameter->IsOfType(Gmat::PARAMETER))
      throw ConfigManagerException(name + " is not a Parameter");

   AddObject(parameter);
}


//------------------------------------------------------------------------------
// void AddBurn(Burn* burn)
//------------------------------------------------------------------------------
/**
 * Adds a Burn to the configuration.
 *
 * @param burn Pointer to the Burn instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddBurn(Burn* burn)
{
   std::string name = burn->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!burn->IsOfType(Gmat::BURN))
      throw ConfigManagerException(name + " is not a Burn");

   AddObject(burn);
}

//------------------------------------------------------------------------------
// void AddSolver(Solver* solver)
//------------------------------------------------------------------------------
/**
 * Adds a Solver to the configuration.
 *
 * @param solver Pointer to the Solver instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddSolver(Solver* solver)
{
   std::string name = solver->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!solver->IsOfType(Gmat::SOLVER))
      throw ConfigManagerException(name + " is not a Solver");

   AddObject(solver);
}

//------------------------------------------------------------------------------
// void AddAtmosphereModel(AtmosphereModel* atmosModel)
//------------------------------------------------------------------------------
/**
 * Adds an AtmosphereModel to the configuration.
 *
 * @param pm atmosModel to the AtmosphereModel instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddAtmosphereModel(AtmosphereModel* atmosModel)
{
   std::string name = atmosModel->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!atmosModel->IsOfType(Gmat::ATMOSPHERE))
      throw ConfigManagerException(name + " is not an AtmosphereModel");

   AddObject(atmosModel);
}

//------------------------------------------------------------------------------
// void AddFunction(Function* function)
//------------------------------------------------------------------------------
/**
 * Adds a Function to the configuration.
 *
 * @param function Pointer to the Function instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddFunction(Function* function)
{
   std::string name = function->GetName();
   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");
   
   if (!function->IsOfType(Gmat::FUNCTION))
      throw ConfigManagerException(name + " is not a Function");

   AddObject(function);
}

//------------------------------------------------------------------------------
// void AddCoordinateSystem(CoordinateSystem *cs)
//------------------------------------------------------------------------------
/**
 * Adds a CoordinateSystem to the configuration.
 *
 * @param cs Pointer to the CoordinateSystem instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddCoordinateSystem(CoordinateSystem *cs)
{
   std::string name = cs->GetName();

   if (name == "")
      throw ConfigManagerException("Unnamed objects cannot be managed");

   if (!cs->IsOfType(Gmat::COORDINATE_SYSTEM))
      throw ConfigManagerException(name + " is not a CoordinateSystem");

   AddObject(cs);
}

//------------------------------------------------------------------------------
// void AddObject(GmatBase *obj)
//------------------------------------------------------------------------------
/**
 * Adds a CoordinateSystem to the configuration.
 *
 * @param cs Pointer to the CoordinateSystem instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddObject(GmatBase *obj)
{
   std::string name = obj->GetName();

   #ifdef DEBUG_CONFIG_MEMORY
      MessageInterface::ShowMessage("Adding %s\n", name.c_str());
   #endif

   if (mapping.find(name) != mapping.end())
   {
      name += " is already in the configuration table";
      throw ConfigManagerException(name);
   }
   else
   {
      objects.push_back(obj);
      mapping[name] = obj;
   }
}

//------------------------------------------------------------------------------
// bool SetSolarSystemInUse(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Sets the name for the current SolarSystem.
 *
 * @param name The SolarSystem name.
 *
 * @note This method is not yet used in GMAT.
 */
//------------------------------------------------------------------------------
bool ConfigManager::SetSolarSystemInUse(const std::string &name)
{
   return false;
}

//------------------------------------------------------------------------------
// StringArray& GetListOfAllItems()
//------------------------------------------------------------------------------
/**
 * Retrieves a list of all configured objects.
 *
 * @return The list of objects.
 */
//------------------------------------------------------------------------------
StringArray& ConfigManager::GetListOfAllItems()
{
   listOfItems.erase(listOfItems.begin(), listOfItems.end());
    
   std::vector<GmatBase*>::iterator current =
      (std::vector<GmatBase*>::iterator)(objects.begin());
   
   while (current != (std::vector<GmatBase*>::iterator)(objects.end()))
   {
      listOfItems.push_back((*current)->GetName());
      ++current;
   }
   return listOfItems;
}


//------------------------------------------------------------------------------
// StringArray& GetListOfItems(Gmat::ObjectType itemType)
//------------------------------------------------------------------------------
/**
 * Retrieves a list of all configured objects of a given type.
 *
 * @param itemType The type of object requested.
 *
 * @return The list of objects.
 */
//------------------------------------------------------------------------------
StringArray& ConfigManager::GetListOfItems(Gmat::ObjectType itemType)
{
   listOfItems.erase(listOfItems.begin(), listOfItems.end());
    
   std::vector<GmatBase*>::iterator current =
      (std::vector<GmatBase*>::iterator)(objects.begin());
   while (current != (std::vector<GmatBase*>::iterator)(objects.end()))
   {
      if ((*current)->GetType() == itemType)
         listOfItems.push_back((*current)->GetName());
      ++current;
   }
   return listOfItems;
}


//------------------------------------------------------------------------------
// GmatBase* GetItem(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves an object by name.
 *
 * @param name The name of the object requested.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
GmatBase* ConfigManager::GetItem(const std::string &name)
{   
   GmatBase *obj = NULL;
    
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetName() == name)
      {
         obj = mapping[name];
      }
   }
    
   return obj;
}

//------------------------------------------------------------------------------
// bool RenameItem(Gmat::ObjectType type, const std::string &oldName,
//                 const std::string &newName)
//------------------------------------------------------------------------------
/**
 * Changes the name for a configured object.
 *
 * @param type The type of object that is renamed.
 * @param oldName The current name for the object.
 * @param newName The new name for the object.
 *
 * @return true if the object was renamed, false if not.
 */
//------------------------------------------------------------------------------
bool ConfigManager::RenameItem(Gmat::ObjectType type,
                               const std::string &oldName,
                               const std::string &newName)
{
#if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("ConfigManager::RenameItem() type=%d, oldName=%s, newName=%s\n",
       type, oldName.c_str(), newName.c_str());
#endif
   bool renamed = false;
    
   if (mapping.find(oldName) != mapping.end())
   {
      GmatBase *obj = mapping[oldName];
      if (obj->GetType() == type)
      {
         // if newName does not exist, change name
         if (mapping.find(newName) == mapping.end())
         {
            mapping.erase(oldName);
            mapping[newName] = obj;
            obj->SetName(newName);
            renamed = true;
         }
         else
         {
            MessageInterface::PopupMessage
               (Gmat::WARNING_, "%s already exist, Please enter different name.\n",
                newName.c_str());
         }
      }
   }
   
   if (!renamed)
   {
      #if DEBUG_RENAME
         MessageInterface::ShowMessage
            ("ConfigManager::RenameItem() Unable to rename: oldName not found.\n");
      #endif
      return false;
   }
   
   //--------------------------------------------------
   // rename ref. object name used in parameters
   //--------------------------------------------------
   
   if (type == Gmat::SPACECRAFT)
   {
      StringArray forms = GetListOfItems(Gmat::FORMATION);
      StringArray subs = GetListOfItems(Gmat::SUBSCRIBER);
      StringArray params = GetListOfItems(Gmat::PARAMETER);
      Subscriber *sub;
      Parameter *param;
      std::string oldParamName;
      std::string newParamName;
      std::string::size_type pos1;
      
      //loj: 2/22/05 - Added
      //------------------------------------------
      // Formation has spacecraft name
      //------------------------------------------
      for (unsigned int i=0; i<forms.size(); i++)
      {
         GetSpacecraft(forms[i])->RenameRefObject(type, oldName, newName);
      }
      
      //------------------------------------------
      // OpenGLPlot has spacecraft name
      //------------------------------------------
      for (unsigned int i=0; i<subs.size(); i++)
      {
         sub = GetSubscriber(subs[i]);
         if (sub->GetTypeName() == "OpenGLPlot")
         {
            sub->RenameRefObject(type, oldName, newName);
         }
      }

      //------------------------------------------
      // Parameter name consists of spacecraft name
      //------------------------------------------
      for (unsigned int i=0; i<params.size(); i++)
      {
         #if DEBUG_RENAME
         MessageInterface::ShowMessage("params[%d]=%s\n", i, params[i].c_str());
         #endif
         
         param = GetParameter(params[i]);
         
         // if system parameter
         if (param->GetKey() == GmatParam::SYSTEM_PARAM)
         {
            oldParamName = param->GetName();
            // if parameter name has old name
            if (oldParamName.find(oldName) != oldParamName.npos)
            {
               // rename ref. object name
               param->RenameRefObject(type, oldName, newName);
               
               // rename actual parameter name
               newParamName = oldParamName;
               pos1 = newParamName.find(".");
               newParamName.replace(0, pos1, newName);
               
               // rename configured parameter name
               renamed = RenameItem(Gmat::PARAMETER, oldParamName, newParamName);
               
               #if DEBUG_RENAME
               MessageInterface::ShowMessage
                  ("newParamName=%s\n", param->GetName().c_str());
               MessageInterface::ShowMessage
                  ("===> Change Subscriber ref object names\n");
               #endif
               
               //--------------------------------------------------
               // rename ref. objects used in subscribers
               //--------------------------------------------------
               for (unsigned int i=0; i<subs.size(); i++)
               {
                  sub = GetSubscriber(subs[i]);
                  
                  // Subscribers other than OpenGLPlot has paramter name
                  if (sub->GetTypeName() != "OpenGLPlot")
                     sub->RenameRefObject(Gmat::PARAMETER, oldParamName, newParamName);
                  
               }
            }
         }
         else if (param->GetTypeName() == "Variable")
         {
            // if parameter name has oldName replace with newName
            oldParamName = param->GetName();
            newParamName = oldParamName;
            std::string::size_type pos = newParamName.find(oldName);
            
            if (pos != newParamName.npos)
               newParamName.replace(pos, oldName.size(), newName);
        
            // if parameter expression has oldName replace with newName
            std::string newExp = param->GetStringParameter("Expression");
            pos = newExp.find(oldName);
            
            if (pos != newExp.npos)
            {
               newExp.replace(pos, oldName.size(), newName);
               param->SetStringParameter("Expression", newExp);
               
               // rename ref. parameter name
               //loj: 2/22/05 "RefParams" no longer used
               //StringArray refParamNames = param->GetStringArrayParameter("RefParams");
               StringArray refParamNames = param->GetRefObjectNameArray(Gmat::PARAMETER);
               for (unsigned int i=0; i<refParamNames.size(); i++)
               {
                  oldParamName = refParamNames[i];
                  newParamName = oldParamName;
                  pos = oldParamName.find(oldName);
                  if (pos != oldParamName.npos)
                  {
                     newParamName.replace(pos, oldName.size(), newName);
                     param->RenameRefObject(Gmat::PARAMETER, oldParamName, newParamName);
                  }
               }
            }
         }//if (param->GetKey() == Parameter::SYSTEM_PARAM)
      } //for (unsigned int i=0; i<params.size(); i++)
   }
   
   
   return renamed;
}


//------------------------------------------------------------------------------
// bool RemoveAllItems()
//------------------------------------------------------------------------------
/**
 * Removes all configured objects from memory
 *
 * @return true on success, false on failure.
 */
//------------------------------------------------------------------------------
bool ConfigManager::RemoveAllItems()
{
   // delete objects
   #ifdef DEBUG_CONFIG_MEMORY
      MessageInterface::ShowMessage("Deleting %d objects\n", objects.size());
   #endif
   
   for (unsigned int i=0; i<objects.size(); i++)
   {
      std::string objName = objects[i]->GetName();

      #ifdef DEBUG_CONFIG_MEMORY
         MessageInterface::ShowMessage("  Deleting %s\n", objects[i]->GetName().c_str());
      #endif

//      delete objects[i];
      objects[i] = NULL;
   }

   objects.clear();
   mapping.clear();

   return true;
}

//------------------------------------------------------------------------------
// bool RemoveItem(Gmat::ObjectType type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Removes a specific item from memory.
 *
 * @param type The type of the object that is being removed.
 * @param name The name of the object.
 *
 * @return true on success, false on failure.
 */
//------------------------------------------------------------------------------
bool ConfigManager::RemoveItem(Gmat::ObjectType type, const std::string &name)
{
   bool status = false;

   // remove from objects
   std::vector<GmatBase*>::iterator currentIter =
      (std::vector<GmatBase*>::iterator)(objects.begin());
    
   while (currentIter != (std::vector<GmatBase*>::iterator)(objects.end()))
   {
      if ((*currentIter)->GetType() == type)
      {
         if ((*currentIter)->GetName() == name)
         {
            objects.erase(currentIter);
            break;
         }
      }
      ++currentIter;
   }
    
   // remove from mapping
   if (mapping.find(name) != mapping.end())
   {
      GmatBase *obj = mapping[name];
      if (obj->GetType() == type)
      {
         mapping.erase(name);
         delete obj;
         status = true;
      }
   }
    
   return status;
}


//------------------------------------------------------------------------------
// PhysicalModel* GetPhysicalModel(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a PhysicalModel object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
PhysicalModel* ConfigManager::GetPhysicalModel(const std::string &name)
{
   PhysicalModel *physicalModel = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::PHYSICAL_MODEL)
      {
         physicalModel = (PhysicalModel *)mapping[name];
      }
   }
   return physicalModel;
}

//------------------------------------------------------------------------------
// Propagator* GetPropagator(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Propagator object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
Propagator* ConfigManager::GetPropagator(const std::string &name)
{
   Propagator *prop = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::PROPAGATOR)
      {
         prop = (Propagator *)mapping[name];
      }
   }
   return prop;
}


//------------------------------------------------------------------------------
// ForceModel* GetForceModel(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a ForceModel object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
ForceModel* ConfigManager::GetForceModel(const std::string &name)
{
   ForceModel *fm = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::FORCE_MODEL)
      {
         fm = (ForceModel *)mapping[name];
      }
   }
   return fm;
}


//------------------------------------------------------------------------------
// SpaceObject* GetSpacecraft(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Spacecraft or Formation object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
SpaceObject* ConfigManager::GetSpacecraft(const std::string &name)
{
   SpaceObject *sc = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if ((mapping[name]->GetType() == Gmat::SPACECRAFT) ||
          (mapping[name]->GetType() == Gmat::FORMATION))
      {
         sc = (SpaceObject *)mapping[name];
      }
   }
   return sc;
}


//------------------------------------------------------------------------------
// Hardware* GetHardware(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Hardware object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
Hardware* ConfigManager::GetHardware(const std::string &name)
{
   Hardware *hw = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::HARDWARE)
      {
         hw = (Hardware *)mapping[name];
      }
   }
   return hw;
}


//------------------------------------------------------------------------------
// PropSetup* GetPropSetup(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a PropSetup object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
PropSetup* ConfigManager::GetPropSetup(const std::string &name)
{
   PropSetup *ps = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::PROP_SETUP)
      {
         ps = (PropSetup *)mapping[name];
      }
   }
   return ps;
}


//------------------------------------------------------------------------------
// Subscriber* GetSubscriber(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Subscriber object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
Subscriber* ConfigManager::GetSubscriber(const std::string &name)
{
   Subscriber *sub = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::SUBSCRIBER)
      {
         sub = (Subscriber *)mapping[name];
      }
   }
   return sub;
}


//------------------------------------------------------------------------------
// SolarSystem* GetDefaultSolarSystem()
//------------------------------------------------------------------------------
/**
 * Retrieves the default SolarSystem object.
 *
 * @return A pointer to the object.
 *
 * @note This method is not yet used in GMAT.
 */
//------------------------------------------------------------------------------
SolarSystem* ConfigManager::GetDefaultSolarSystem()
{
   throw ConfigManagerException
      ("ConfigManager::GetDefaultSolarSystem() has not been implemented.\n");
}


//------------------------------------------------------------------------------
// SolarSystem* GetSolarSystemInUse()
//------------------------------------------------------------------------------
/**
 * Retrieves the current SolarSystem object.
 *
 * @return A pointer to the object.
 *
 * @note This method is not yet used in GMAT.
 */
//------------------------------------------------------------------------------
SolarSystem* ConfigManager::GetSolarSystemInUse()
{
   throw ConfigManagerException
      ("ConfigManager::GetSolarSystemInUse() has not been implemented.\n");
}


//------------------------------------------------------------------------------
// StopCondition* GetStopCondition(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a StopCondition object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
StopCondition* ConfigManager::GetStopCondition(const std::string &name)
{
   StopCondition *sc = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::STOP_CONDITION)
      {
         sc = (StopCondition *)mapping[name];
      }
   }
   return sc;
}


//------------------------------------------------------------------------------
// Parameter* GetParameter(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Parameter object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
Parameter* ConfigManager::GetParameter(const std::string &name)
{
   Parameter *param = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::PARAMETER)
      {
         param = (Parameter *)mapping[name];
      }
   }
   return param;
}

//------------------------------------------------------------------------------
// Burn* GetBurn(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Burn object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
Burn* ConfigManager::GetBurn(const std::string &name)
{
   Burn *burn = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::BURN)
      {
         burn = (Burn *)mapping[name];
      }
   }
   return burn;
}

//------------------------------------------------------------------------------
// Solver* GetSolver(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Solver object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
Solver* ConfigManager::GetSolver(const std::string &name)
{
   Solver *solver = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::SOLVER)
      {
         solver = (Solver *)mapping[name];
      }
   }
   return solver;
}

//------------------------------------------------------------------------------
// AtmosphereModel* GetAtmosphereModel(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves an Atmosphere object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
AtmosphereModel* ConfigManager::GetAtmosphereModel(const std::string &name)
{
   AtmosphereModel *atmosModel = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::ATMOSPHERE)
      {
         atmosModel = (AtmosphereModel *)mapping[name];
      }
   }
   return atmosModel;
}

//------------------------------------------------------------------------------
// Function* GetFunction(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a Function object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
Function* ConfigManager::GetFunction(const std::string &name)
{
   Function *function = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::FUNCTION)
      {
         function = (Function *)mapping[name];
      }
   }
   return function;
}

//------------------------------------------------------------------------------
// CoordinateSystem* GetCoordinateSystem(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a CoordinateSystem object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
CoordinateSystem* ConfigManager::GetCoordinateSystem(const std::string &name)
{
   CoordinateSystem *cs = NULL;
   if (mapping.find(name) != mapping.end())
   {
      if (mapping[name]->GetType() == Gmat::COORDINATE_SYSTEM)
      {
         cs = (CoordinateSystem *)mapping[name];
      }
   }
   return cs;
}

//=================================
// Methods I'm not sure we need
//=================================

//------------------------------------------------------------------------------
// void AddCelestialBody(CelestialBody* body)
//------------------------------------------------------------------------------
/**
 * Adds a CelestialBody to the configuration.
 *
 * @param body Pointer to the CelestialBody instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddCelestialBody(CelestialBody* body)
{
   throw ConfigManagerException
      ("ConfigManager::AddCelestialBody() has not been implemented.\n");
}

//------------------------------------------------------------------------------
// void AddCommand(GmatCommand *cmd)
//------------------------------------------------------------------------------
/**
 * Adds a GmatCommand to the configuration.
 *
 * @param cmd Pointer to the PhysicalModel instance.
 */
//------------------------------------------------------------------------------
void ConfigManager::AddCommand(GmatCommand *cmd)
{
   throw ConfigManagerException
      ("ConfigManager::AddCommand() has not been implemented.\n");
}

//------------------------------------------------------------------------------
// CelestialBody* GetCelestialBody(const std::string &name)
//------------------------------------------------------------------------------
/**
 * Retrieves a CelestialBody object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
CelestialBody* ConfigManager::GetCelestialBody(const std::string &name)
{
   throw ConfigManagerException
      ("ConfigManager::GetCelestialBody() has not been implemented.\n");
}

//------------------------------------------------------------------------------
// GmatCommand* GetCommand(const std::string name)
//------------------------------------------------------------------------------
/**
 * Retrieves a GmatCommand object.
 *
 * @param name The name of the object.
 *
 * @return A pointer to the object.
 */
//------------------------------------------------------------------------------
GmatCommand* ConfigManager::GetCommand(const std::string name)
{
   throw ConfigManagerException
      ("ConfigManager::GetCommand() has not been implemented.\n");
}
