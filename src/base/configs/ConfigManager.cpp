//$Header$
//------------------------------------------------------------------------------
//                                  ClassName
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/10/24
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Insert descriptive text here.
 *
 * @note Any notes here.
 */
//------------------------------------------------------------------------------

// Class automatically generated by Dev-C++ New Class wizard

#include "ConfigManager.hpp"
#include "ConfigManagerException.hpp"

ConfigManager* ConfigManager::theConfigManager = NULL;


//------------------------------------------------------------------------------
// ConfigManager* Instance(void)
//------------------------------------------------------------------------------
ConfigManager* ConfigManager::Instance(void)
{
    if (!theConfigManager)
        theConfigManager = new ConfigManager;
        
    return theConfigManager;
}


// class constructor
//------------------------------------------------------------------------------
// ConfigManager()
//------------------------------------------------------------------------------
ConfigManager::ConfigManager()
{
        // insert your code here
}

// class destructor
//------------------------------------------------------------------------------
// ~ConfigManager()
//------------------------------------------------------------------------------
ConfigManager::~ConfigManager()
{
        // insert your code here
}


//------------------------------------------------------------------------------
// void AddForceModel(ForceModel *fm)
//------------------------------------------------------------------------------
void ConfigManager::AddForceModel(ForceModel *fm)
{
    std::string name = fm->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(fm);
        mapping[name] = fm;
    }
}


//------------------------------------------------------------------------------
// void AddSubscriber(Subscriber *subs)
//------------------------------------------------------------------------------
void ConfigManager::AddSubscriber(Subscriber *subs)
{
//    throw ConfigManagerException("Subscribers cannot be managed yet");
    std::string name = subs->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(subs);
        mapping[name] = subs;
    }
}


//------------------------------------------------------------------------------
// void AddSolarSystem(SolarSystem *solarSys)
//------------------------------------------------------------------------------
void ConfigManager::AddSolarSystem(SolarSystem *solarSys)
{
    throw ConfigManagerException("SolarSystem objects not managed in build 1");
}


//------------------------------------------------------------------------------
// void AddPropSetup(PropSetup* propSetup)
//------------------------------------------------------------------------------
void ConfigManager::AddPropSetup(PropSetup* propSetup)
{
    std::string name = propSetup->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(propSetup);
        mapping[name] = propSetup;
    }
}


//------------------------------------------------------------------------------
// void AddSpacecraft(Spacecraft *sc)
//------------------------------------------------------------------------------
void ConfigManager::AddSpacecraft(Spacecraft *sc)
{
    std::string name = sc->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(sc);
        mapping[name] = sc;
    }
}


//------------------------------------------------------------------------------
// void AddStopCondition(StopCondition* stopCond)
//------------------------------------------------------------------------------
void ConfigManager::AddStopCondition(StopCondition* stopCond)
{
    std::string name = stopCond->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(stopCond);
        mapping[name] = stopCond;
    }
}


//------------------------------------------------------------------------------
// void AddParameter(Parameter* parameter)
//------------------------------------------------------------------------------
void ConfigManager::AddParameter(Parameter* parameter)
{
    std::string name = parameter->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(parameter);
        mapping[name] = parameter;
    }
}


//------------------------------------------------------------------------------
// void AddBurn(Burn* burn)
//------------------------------------------------------------------------------
void ConfigManager::AddBurn(Burn* burn)
{
    std::string name = burn->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(burn);
        mapping[name] = burn;
    }
}


//------------------------------------------------------------------------------
// void AddSolver(Solver* solver)
//------------------------------------------------------------------------------
void ConfigManager::AddSolver(Solver* solver)
{
    std::string name = solver->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(solver);
        mapping[name] = solver;
    }
}


//------------------------------------------------------------------------------
// bool SetSolarSystemInUse(const std::string &name)
//------------------------------------------------------------------------------
bool ConfigManager::SetSolarSystemInUse(const std::string &name)
{
    return false;
}

//loj: 2/13/04 added
//------------------------------------------------------------------------------
// StringArray& GetListOfAllItems()
//------------------------------------------------------------------------------
StringArray& ConfigManager::GetListOfAllItems()
{
    listOfItems.erase(listOfItems.begin(), listOfItems.end());
    
    std::vector<GmatBase*>::iterator current = (std::vector<GmatBase*>::iterator)(objects.begin());
    while (current != (std::vector<GmatBase*>::iterator)(objects.end())) {
        listOfItems.push_back((*current)->GetName());
        ++current;
    }
    return listOfItems;
}


//------------------------------------------------------------------------------
// StringArray& GetListOfItems(Gmat::ObjectType itemType)
//------------------------------------------------------------------------------
StringArray& ConfigManager::GetListOfItems(Gmat::ObjectType itemType)
{
    listOfItems.erase(listOfItems.begin(), listOfItems.end());
    
    std::vector<GmatBase*>::iterator current = (std::vector<GmatBase*>::iterator)(objects.begin());
    while (current != (std::vector<GmatBase*>::iterator)(objects.end())) {
        if ((*current)->GetType() == itemType)
            listOfItems.push_back((*current)->GetName());
        ++current;
    }
    return listOfItems;
}


//------------------------------------------------------------------------------
// GmatBase* GetItem(const std::string &name)
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
// bool RenameItem(Gmat::ObjectType itemType, const std::string &oldName,
//                 const std::string &newName)
//------------------------------------------------------------------------------
bool ConfigManager::RenameItem(Gmat::ObjectType itemType,
                               const std::string &oldName,
                               const std::string &newName)
{
    bool status = false;
    
    if (mapping.find(oldName) != mapping.end())
    {
        GmatBase *obj = mapping[oldName];
        if (obj->GetType() == itemType)
        {
            mapping.erase(oldName);
            mapping[newName] = obj;
            obj->SetName(newName);
            status = true;
        }
    }
    
    return status;
}


//------------------------------------------------------------------------------
// bool RemoveAllItems()
//------------------------------------------------------------------------------
bool ConfigManager::RemoveAllItems()
{
    // delete objects

    //loj: 3/10/04 added
    for (int i=0; i<objects.size(); i++)
    {
        delete objects[i];
        objects[i] = NULL;
    }
    
    objects.clear();
    mapping.clear();

    return true;
}

//loj: 2/13/04 added
//------------------------------------------------------------------------------
// bool RemoveItem(Gmat::ObjectType type, const std::string &name)
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
            
            // delete object loj:3/10/04 added
            delete obj;
            
            status = true;
        }
    }
    
    return status;
}


//------------------------------------------------------------------------------
// ForceModel* GetForceModel(const std::string &name)
//------------------------------------------------------------------------------
ForceModel* ConfigManager::GetForceModel(const std::string &name)
{
    ForceModel *fm = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::FORCE_MODEL) {
            //std::string str = mapping[name]->GetName() +
            //                  " is not a force model";
            //throw ConfigManagerException(str);
            return NULL;
        }
        fm = (ForceModel *)mapping[name];
    }
    return fm;
}


//------------------------------------------------------------------------------
// Spacecraft* GetSpacecraft(const std::string &name)
//------------------------------------------------------------------------------
Spacecraft* ConfigManager::GetSpacecraft(const std::string &name)
{
    Spacecraft *sc = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::SPACECRAFT) {
            //std::string str = mapping[name]->GetName() +
            //                  " is not a spacecraft";
            //throw ConfigManagerException(str);
            return NULL;
        }
        sc = (Spacecraft *)mapping[name];
    }
    return sc;
}


//------------------------------------------------------------------------------
// PropSetup* GetPropSetup(const std::string &name)
//------------------------------------------------------------------------------
PropSetup* ConfigManager::GetPropSetup(const std::string &name)
{
    PropSetup *ps = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::PROP_SETUP) {
            //std::string str = mapping[name]->GetName() +
            //                  " is not a PropSetup";
            //throw ConfigManagerException(str);
            return NULL;
        }
        ps = (PropSetup *)mapping[name];
    }
    return ps;
}


//------------------------------------------------------------------------------
// Subscriber* GetSubscriber(const std::string &name)
//------------------------------------------------------------------------------
Subscriber* ConfigManager::GetSubscriber(const std::string &name)
{
    Subscriber *sub = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::SUBSCRIBER) {
            //std::string str = mapping[name]->GetName() +
            //                  " is not a PropSetup";
            //throw ConfigManagerException(str);
            return NULL;
        }
        sub = (Subscriber *)mapping[name];
    }
    return sub;
}


//------------------------------------------------------------------------------
// SolarSystem* GetDefaultSolarSystem()
//------------------------------------------------------------------------------
SolarSystem* ConfigManager::GetDefaultSolarSystem()
{
    return NULL;
}


//------------------------------------------------------------------------------
// SolarSystem* GetSolarSystemInUse()
//------------------------------------------------------------------------------
SolarSystem* ConfigManager::GetSolarSystemInUse()
{
    return NULL;
}


//------------------------------------------------------------------------------
// StopCondition* GetStopCondition(const std::string &name)
//------------------------------------------------------------------------------
StopCondition* ConfigManager::GetStopCondition(const std::string &name)
{
    StopCondition *sc = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::STOP_CONDITION) {
            std::string str = mapping[name]->GetName() +
                              " is not a spacecraft";
            throw ConfigManagerException(str);
        }
        sc = (StopCondition *)mapping[name];
    }
    return sc;
}


// Methods I'm not sure we need
void ConfigManager::AddCelestialBody(CelestialBody* body)
{
}

//loj: 3/3/04 added implementation
//------------------------------------------------------------------------------
// void AddPhysicalModel(PhysicalModel *pm)
//------------------------------------------------------------------------------
void ConfigManager::AddPhysicalModel(PhysicalModel *pm)
{
    std::string name = pm->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(pm);
        mapping[name] = pm;
    }
}

//loj: 2/13/04 added implementation
//------------------------------------------------------------------------------
// void ConfigManager::AddPropagator(Propagator *prop)
//------------------------------------------------------------------------------
void ConfigManager::AddPropagator(Propagator *prop)
{
    std::string name = prop->GetName();
    if (name == "")
        throw ConfigManagerException("Unnamed objects cannot be managed");
    if (mapping.find(name) != mapping.end()) {
        name += " is already in the configuration table";
        throw ConfigManagerException(name);
    }
    else {
        objects.push_back(prop);
        mapping[name] = prop;
    }
}


CelestialBody* ConfigManager::GetCelestialBody(const std::string &name)
{
    return NULL;
}


//loj: 3/3/04 added implementation
//------------------------------------------------------------------------------
// PhysicalModel* GetPhysicalModel(const std::string &name)
//------------------------------------------------------------------------------
PhysicalModel* ConfigManager::GetPhysicalModel(const std::string &name)
{
    PhysicalModel *physicalModel = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::PHYSICAL_MODEL) {
            std::string str = mapping[name]->GetName() +
                              " is not a Propagator";
            throw ConfigManagerException(str);
        }
        physicalModel = (PhysicalModel *)mapping[name];
    }
    return physicalModel;
}

//loj: 3/3/04 added implementation
//------------------------------------------------------------------------------
// Propagator* GetPropagator(const std::string &name)
//------------------------------------------------------------------------------
Propagator* ConfigManager::GetPropagator(const std::string &name)
{
    Propagator *prop = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::PROPAGATOR) {
            std::string str = mapping[name]->GetName() +
                              " is not a Propagator";
            throw ConfigManagerException(str);
        }
        prop = (Propagator *)mapping[name];
    }
    return prop;
}


void ConfigManager::AddCommand(GmatCommand *cmd)
{
}


GmatCommand* ConfigManager::GetCommand(const std::string name)
{
    return NULL;
}


Parameter* ConfigManager::GetParameter(const std::string &name)
{
    Parameter *param = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::PARAMETER) {
            std::string str = mapping[name]->GetName() +
                              " is not a spacecraft";
            throw ConfigManagerException(str);
        }
        param = (Parameter *)mapping[name];
    }
    return param;
}

Burn* ConfigManager::GetBurn(const std::string &name)
{
    Burn *burn = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::BURN) {
            std::string str = mapping[name]->GetName() +
                              " is not a burn";
            throw ConfigManagerException(str);
        }
        burn = (Burn *)mapping[name];
    }
    return burn;
}

Solver* ConfigManager::GetSolver(const std::string &name)
{
    Solver *solver = NULL;
    if (mapping.find(name) != mapping.end()) {
        if (mapping[name]->GetType() != Gmat::SOLVER) {
            std::string str = mapping[name]->GetName() +
                              " is not a solver";
            throw ConfigManagerException(str);
        }
        solver = (Solver *)mapping[name];
    }
    return solver;
}

