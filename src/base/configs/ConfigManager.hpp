//$Header$
//------------------------------------------------------------------------------
//                                  ClassName
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/mm/dd
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

#ifndef CONFIGMANAGER_HPP
#define CONFIGMANAGER_HPP

#include <vector>
#include <map>

#include "ForceModel.hpp"
#include "Subscriber.hpp"
#include "SolarSystem.hpp"
#include "CelestialBody.hpp"
#include "PropSetup.hpp"
#include "Spacecraft.hpp"
#include "StopCondition.hpp"
#include "PhysicalModel.hpp"
#include "Propagator.hpp"
#include "Parameter.hpp"
#include "Command.hpp"
#include "Burn.hpp"
#include "Solver.hpp"


/**
 * Configuration Manager Stub -- a hack until the real code is ready
 */
class ConfigManager
{
public:
    static ConfigManager*   Instance(void);
        
    void                AddForceModel(ForceModel *fm);
    void                AddSubscriber(Subscriber *subs);
    void                AddSolarSystem(SolarSystem *solarSys);
    void                AddPropSetup(PropSetup *propSetup);
    void                AddSpacecraft(SpaceObject *sc);
    void                AddStopCondition(StopCondition* stopCond);
    void                AddParameter(Parameter* parameter);
    void                AddBurn(Burn* burn);
    void                AddSolver(Solver *solver);
    
    bool                SetSolarSystemInUse(const std::string &name);
    
    StringArray&        GetListOfAllItems();
    StringArray&        GetListOfItems(Gmat::ObjectType itemType);// const;
    GmatBase*           GetItem(const std::string &name);
    
    bool                RenameItem(Gmat::ObjectType itemType,
                                   const std::string &oldName,
                                   const std::string &newName);
    
    bool                RemoveAllItems();
    bool                RemoveItem(Gmat::ObjectType type, const std::string &name);
        
    ForceModel*         GetForceModel(const std::string &name);
    SpaceObject*        GetSpacecraft(const std::string &name);
    PropSetup*          GetPropSetup(const std::string &name);
    Subscriber*         GetSubscriber(const std::string &name);
    SolarSystem*        GetDefaultSolarSystem();
    SolarSystem*        GetSolarSystemInUse();
    StopCondition*      GetStopCondition(const std::string &name);
    Parameter*          GetParameter(const std::string &name);
    Burn*               GetBurn(const std::string &name);
    Solver*             GetSolver(const std::string &name);
        
    // Methods I'm not sure we need
    void                AddCelestialBody(CelestialBody* body);
    void                AddPhysicalModel(PhysicalModel *pm);
    void                AddPropagator(Propagator *prop);
                                             
    CelestialBody*      GetCelestialBody(const std::string &name);
    PhysicalModel*      GetPhysicalModel(const std::string &name);
    Propagator*         GetPropagator(const std::string &name);
        
    void                AddCommand(GmatCommand *cmd);
    GmatCommand*        GetCommand(const std::string name);
private:
    /// The singleton instance
    static ConfigManager*   theConfigManager;
    ConfigManager();
    ~ConfigManager();
                
    std::vector<GmatBase*>  objects;
    StringArray  listOfItems;
    std::map<std::string, GmatBase *>  mapping;
};


#endif // CONFIGMANAGER_HPP

