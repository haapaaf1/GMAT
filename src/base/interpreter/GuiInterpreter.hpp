//$Header$
//------------------------------------------------------------------------------
//                              GuiInterpreter
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2003/08/25
//
/**
 * Declares the operations between GUI subsystem and the Moderator.
 */
//------------------------------------------------------------------------------
#ifndef GuiInterpreter_hpp
#define GuiInterpreter_hpp

#include "gmatdefs.hpp"
#include "PhysicalModel.hpp"
#include "ForceModel.hpp"
#include "Propagator.hpp"
#include "Spacecraft.hpp"
#include "Parameter.hpp"
#include "StopCondition.hpp"
#include "SolarSystem.hpp"
#include "CelestialBody.hpp"
#include "PropSetup.hpp"
#include "Command.hpp"
#include "Subscriber.hpp"

class Moderator;

class GMAT_API GuiInterpreter
{
public:

    static GuiInterpreter* Instance();
    ~GuiInterpreter();

    bool IsInitialized();
    void Initialize();

    //----- factory
    //loj:Do we need this?
    //StringArray GetListOfContainer();
    StringArray GetListOfFactoryItems(Gmat::ObjectType type);

    //----- config
    StringArray GetListOfConfiguredItems(Gmat::ObjectType type);
    bool RemoveConfiguredItem(Gmat::ObjectType type, const std::string &name);

    // Spacecraft
    Spacecraft* CreateSpacecraft(const std::string&type, const std::string &name);
    Spacecraft* GetSpacecraft(const std::string &name);
    //future build:GroundStation* CreateGroundStation(const std::string &name);
    //future build:GroundStation* GetGroundStation(const std::string &name);

    // Propagator
    Propagator* CreatePropagator(const std::string &type, const std::string &name);
    Propagator* GetPropagator(const std::string &name);

    // PhysicalModel
    PhysicalModel* CreatePhysicalModel(const std::string &type, const std::string &name);
    PhysicalModel* GetPhysicalModel(const std::string &name);

    // Parameter
    Parameter* CreateParameter(const std::string &type, const std::string &name);
    Parameter* GetParameter(const std::string &name);

    // Celestial body
    CelestialBody* CreateCelestialBody(const std::string &type, const std::string &name);
    CelestialBody* GetCelestialBody(const std::string &name);

    // SolarSystem
    SolarSystem* GetDefaultSolarSystem();

    // Subscriber
    Subscriber* CreateSubscriber(const std::string &type, const std::string &name,
                                 const std::string &filename = "");
    Subscriber* GetSubscriber(const std::string &name);

    // Command
    Command* CreateCommand(const std::string&type, const std::string &name);
    Command* GetCommand(const std::string &name);
    Command* GetNextCommand(Integer sandboxNum = 1);
    bool DeleteCommand(const std::string &name, Integer position,
                       Integer sandboxNum = 1);
    Command* InsertCommand(const std::string&type, const std::string &name,
                           Integer position, bool addAbove = true,
                           Integer sandboxNum = 1);
    Command* AppendCommand(const std::string&type, const std::string &name,
                           Integer sandboxNum = 1);

    // Sandbox
    void ClearAllSandboxes();
    Integer RunMission(Integer sandboxNum = 1);

    // Script
    bool InterpretScript(const std::string &scriptFilename);
    bool SaveScript(const std::string &scriptFilename);
    Integer RunScript(Integer sandboxNum = 1);
    
private:

    GuiInterpreter();
    GuiInterpreter(const GuiInterpreter&);
    GuiInterpreter& operator=(const GuiInterpreter &guiInterpreter);

    // member data
    bool isInitialized;
    Moderator *theModerator;

    static GuiInterpreter *instance;
};


#endif // GuiInterpreter_hpp

