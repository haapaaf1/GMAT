//$Id$
//------------------------------------------------------------------------------
//                                 Moderator
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
 * Declares opeartions of the GMAT executive. It is a singleton class -
 * only one instance of this class can be created.
 */
//------------------------------------------------------------------------------
#ifndef Moderator_hpp
#define Moderator_hpp

#include "gmatdefs.hpp"
// executive
#include "Sandbox.hpp"
#include "GuiInterpreter.hpp"
#include "ScriptInterpreter.hpp"
#include "FactoryManager.hpp"
#include "ConfigManager.hpp"
#include "Publisher.hpp"
#include "FileManager.hpp"
// dynamic libraries
#include "DynamicLibrary.hpp"
// core
#include "AtmosphereModel.hpp"
#include "Attitude.hpp"
#include "AxisSystem.hpp"
#include "Burn.hpp"
#include "GmatCommand.hpp"
#include "CoordinateSystem.hpp"
#include "Function.hpp"
#include "Hardware.hpp"
#include "PhysicalModel.hpp"
#include "ForceModel.hpp"
#include "Propagator.hpp"
#include "Spacecraft.hpp"
#include "Formation.hpp"
#include "Parameter.hpp"
#include "StopCondition.hpp"
#include "Solver.hpp"
#include "SolarSystem.hpp"
#include "CelestialBody.hpp"
#include "PropSetup.hpp"
#include "Subscriber.hpp"
#include "Interpolator.hpp"
#include "CalculatedPoint.hpp"
#include "MathNode.hpp"
// files
#include "EopFile.hpp"
#include "ItrfCoefficientsFile.hpp"
#include "LeapSecsFileReader.hpp"

namespace Gmat
{
   const Integer MAX_SANDBOX = 4;
};

class GMAT_API Moderator
{
public:

   static Moderator* Instance();
   bool Initialize(bool isFromGui = false);
   void Finalize();
   void SetRunReady(bool flag = true);
   void SetShowFinalState(bool flag = true);
   
   //----- ObjectType
   std::string GetObjectTypeString(Gmat::ObjectType type);  
   
   //----- interpreter
   static GuiInterpreter* GetGuiInterpreter();
   static ScriptInterpreter* GetScriptInterpreter();
   static void SetGuiInterpreter(GuiInterpreter *guiInterp);
   static void SetScriptInterpreter(ScriptInterpreter *scriptInterp);
   
   //----- factory
   const StringArray& GetListOfFactoryItems(Gmat::ObjectType type);
   
   //----- configuration
   const StringArray& GetListOfObjects(Gmat::ObjectType type);
   GmatBase* GetConfiguredObject(const std::string &name);
   std::string GetNewName(const std::string &name, Integer startCount);
   std::string AddClone(const std::string &name);
   bool RenameObject(Gmat::ObjectType type, const std::string &oldName,
                     const std::string &newName);
   bool RemoveObject(Gmat::ObjectType type, const std::string &name,
                     bool delOnlyIfNotUsed);
   bool HasConfigurationChanged(Integer sandboxNum = 1);
   void ConfigurationChanged(GmatBase *obj, bool tf);
   void ResetConfigurationChanged(bool resetResource = true,
                                  bool resetCommands = true,
                                  Integer sandboxNum = 1);
   
   // SolarSystem
   SolarSystem* GetDefaultSolarSystem();
   SolarSystem* CreateSolarSystem(const std::string &name);
   SolarSystem* GetSolarSystemInUse();
   bool SetSolarSystemInUse(const std::string &name);
   void SetSolarSystemInUse(SolarSystem *ss);
   
   // CalculatedPoint
   CalculatedPoint* CreateCalculatedPoint(const std::string &type,
                                          const std::string &name,
                                          bool addDefaultBodies = true);
   CalculatedPoint* GetCalculatedPoint(const std::string &name);
   
   // CelestialBody
   CelestialBody* CreateCelestialBody(const std::string &type,
                                      const std::string &name);
   CelestialBody* GetCelestialBody(const std::string &name);

   // Spacecraft
   SpaceObject* CreateSpacecraft(const std::string &type,
                                 const std::string &name);
   SpaceObject* GetSpacecraft(const std::string &name);
   std::string GetSpacecraftNotInFormation();
   
   // Hardware
   Hardware* CreateHardware(const std::string &type,
                            const std::string &name);
   Hardware* GetHardware(const std::string &name);
   
   // Propagator
   Propagator* CreatePropagator(const std::string &type,
                                const std::string &name);
   Propagator* GetPropagator(const std::string &name);
   
   // PhysicalModel
   PhysicalModel* CreatePhysicalModel(const std::string &type,
                                      const std::string &name);
   PhysicalModel* GetPhysicalModel(const std::string &name);
   
   // AtmosphereModel
   AtmosphereModel* CreateAtmosphereModel(const std::string &type,
                                          const std::string &name,
                                          const std::string &body = "Earth");
   AtmosphereModel* GetAtmosphereModel(const std::string &name);
   
   // Burn
   Burn* CreateBurn(const std::string &type,
                    const std::string &name);
   Burn* GetBurn(const std::string &name);
   
   // Parameter
   bool IsParameter(const std::string &type);
   Parameter* CreateParameter(const std::string &type,
                              const std::string &name,
                              const std::string &ownerName = "",
                              const std::string &depName = "",
                              bool manage = true);
   Parameter* GetParameter(const std::string &name);
   
   // ForceModel
   ForceModel* CreateForceModel(const std::string &name);
   ForceModel* GetForceModel(const std::string &name);
   bool AddToForceModel(const std::string &forceModelName,
                        const std::string &forceName);
   bool ReconfigureItem(GmatBase *newobj, const std::string &name);
   
   // Solver
   Solver* CreateSolver(const std::string &type,
                        const std::string &name);
   Solver* GetSolver(const std::string &name);
   
   // PropSetup
   PropSetup* CreateDefaultPropSetup(const std::string &name);
   PropSetup* CreatePropSetup(const std::string &name,
                              const std::string &propagatorName = "",
                              const std::string &forceModelName = "");
   PropSetup* GetPropSetup(const std::string &name);
   
   // Interpolator
   Interpolator* CreateInterpolator(const std::string &type,
                                    const std::string &name);
   Interpolator* GetInterpolator(const std::string &name);
   
   // CoordinateSystem
   CoordinateSystem* CreateCoordinateSystem(const std::string &name,
                                            bool createDefault = false,
                                            bool internal = false);
   CoordinateSystem* GetCoordinateSystem(const std::string &name);
   
   // Subscriber
   Subscriber* CreateSubscriber(const std::string &type,
                                const std::string &name,
                                const std::string &fileName = "",
                                bool createDefault = false);
   Subscriber* GetSubscriber(const std::string &name);
   
   // Function
   Function* CreateFunction(const std::string &type,
                            const std::string &name);
   Function* GetFunction(const std::string &name);
   
   //----- Non-Configurable Items
   // StopCondition
   StopCondition* CreateStopCondition(const std::string &type,
                                      const std::string &name);
   
   // AxisSystem
   AxisSystem* CreateAxisSystem(const std::string &type,
                                const std::string &name);
   
   // MathNode
   MathNode* CreateMathNode(const std::string &type,
                            const std::string &name = "");
   
   // AxisSystem
   Attitude* CreateAttitude(const std::string &type,
                            const std::string &name);
   
   // GmatCommand
   GmatCommand* InterpretGmatFunction(const std::string &fileName);
   GmatCommand* InterpretGmatFunction(Function *funct);
   GmatCommand* CreateCommand(const std::string &type,
                              const std::string &name, bool &retFlag);
   GmatCommand* CreateDefaultCommand(const std::string &type,
                                     const std::string &name = "",
                                     GmatCommand *refCmd = NULL);
   GmatCommand* AppendCommand(const std::string &type,
                              const std::string &name, bool &retFlag,
                              Integer sandboxNum = 1);
   GmatCommand* DeleteCommand(GmatCommand *cmd, Integer sandboxNum = 1);
   GmatCommand* GetFirstCommand(Integer sanboxNum = 1);
   bool AppendCommand(GmatCommand *cmd, Integer sandboxNum = 1);
   bool InsertCommand(GmatCommand *cmd, GmatCommand *prevCmd,
                      Integer sandboxNum = 1);
   void SetCommandsUnchanged(Integer whichList = 0); 
   
   // CoordinateSystem
   CoordinateSystem* GetInternalCoordinateSystem();
   
   // Planetary files
   const StringArray& GetPlanetarySourceTypes();
   const StringArray& GetPlanetarySourceNames();
   const StringArray& GetPlanetarySourceTypesInUse();
   const StringArray& GetAnalyticModelNames();
   bool SetAnalyticModelToUse(const std::string &modelName);
   bool SetPlanetarySourceName(const std::string &sourceType,
                               const std::string &fileName);
   Integer SetPlanetarySourceTypesInUse(const StringArray &sourceTypes); 
   Integer GetPlanetarySourceId(const std::string &sourceType);
   std::string GetPlanetarySourceName(const std::string &sourceType);
   std::string GetCurrentPlanetarySource();
   
   // Potential field files
   std::string GetPotentialFileName(const std::string &fileType);
   
   // Getting file names
   // This will eventually replace Get*FileName() above (loj: 7/7/05)
   std::string GetFileName(const std::string &fileType);
   
   // Mission
   bool LoadDefaultMission();
   
   // Resource
   bool ClearResource();
   
   // Mission sequence
   bool ClearCommandSeq(Integer sandboxNum = 1);
   
   // Sandbox
   void ClearAllSandboxes();
   GmatBase* GetInternalObject(const std::string &name, Integer sandboxNum = 1);
   Integer RunMission(Integer sandboxNum = 1);
   Integer ChangeRunState(const std::string &state, Integer sandboxNum = 1);
   Gmat::RunState GetUserInterrupt();
   Gmat::RunState GetRunState();
   
   // Script
   bool InterpretScript(const std::string &filename, bool readBack = false,
                        const std::string &newPath = "");
   bool InterpretScript(std::istringstream *ss, bool clearObjs);
   bool SaveScript(const std::string &filename,
                   Gmat::WriteMode mode = Gmat::SCRIPTING);
   std::string GetScript(Gmat::WriteMode mode = Gmat::SCRIPTING);
   Integer RunScript(Integer sandboxNum = 1);
   
   // Dynamic library access code
   void   LoadPlugins();
   void   LoadAPlugin(std::string pluginName);
   DynamicLibrary *LoadLibrary(const std::string &libraryName);
   bool   IsLibraryLoaded(const std::string &libName);
   void   (*GetDynamicFunction(const std::string &funName, 
         const std::string &libraryName))();
   
private:

   // initialization
   void CreatePlanetaryCoeffFile();
   void CreateTimeFile();
   
   void CreateSolarSystemInUse();
   void CreateInternalCoordSystem();
   void CreateDefaultCoordSystems();
   void CreateDefaultMission();
   
   // default objects
   Spacecraft* GetDefaultSpacecraft();
   PropSetup*  GetDefaultPropSetup();
   Burn*       GetDefaultBurn(const std::string &type);
   Hardware*   GetDefaultHardware(const std::string &type);
   Solver*     GetDefaultSolver();
   Subscriber* GetDefaultSubscriber(const std::string &type,
                                    bool addObjects = true);
   Parameter*  GetDefaultX();
   Parameter*  GetDefaultY();
   StopCondition* CreateDefaultStopCondition();
   
   // sandbox
   void AddSolarSystemToSandbox(Integer index);
   void AddInternalCoordSystemToSandbox(Integer index);
   void AddPublisherToSandbox(Integer index);
   void AddCoordSystemToSandbox(Integer index);
   void AddSpacecraftToSandbox(Integer index);
   void AddFormationToSandbox(Integer index);
   void AddPropSetupToSandbox(Integer index);
   void AddPropagatorToSandbox(Integer index);
   void AddForceModelToSandbox(Integer index);
   void AddBurnToSandbox(Integer index);
   void AddSolverToSandbox(Integer index);
   void AddSubscriberToSandbox(Integer index);
   void AddParameterToSandbox(Integer index);
   void AddFunctionToSandbox(Integer index);
   void AddCommandToSandbox(Integer index);
   void InitializeSandbox(Integer index);
   void ExecuteSandbox(Integer index);
   
   // for Debug
   void ShowCommand(const std::string &title1, GmatCommand *cmd1,
                    const std::string &title2 = "", GmatCommand *cmd2 = NULL);
   
   Moderator();
   virtual ~Moderator();
   
   // member data
   bool isSlpAlreadyInUse;
   bool isRunReady;
   bool isFromGui;
   bool endOfInterpreter;
   bool showFinalState;
   std::vector<Sandbox*> sandboxes;
   std::vector<GmatCommand*> commands;
   
   static Moderator *instance;
   static GuiInterpreter *theGuiInterpreter;
   static ScriptInterpreter *theScriptInterpreter;
   ConfigManager *theConfigManager;
   FactoryManager *theFactoryManager;
   FileManager *theFileManager;
   Publisher *thePublisher;
   
   SolarSystem *theDefaultSolarSystem;
   SolarSystem *theSolarSystemInUse;
   CoordinateSystem *theInternalCoordSystem;
   StringArray theSpacePointList;
   EopFile *theEopFile;
   ItrfCoefficientsFile *theItrfFile;
   LeapSecsFileReader *theLeapSecsFile;
   Gmat::RunState runState;
   
   // Dynamic library data structures
   std::map<std::string, DynamicLibrary*>   userLibraries;
};

#endif // Moderator_hpp

