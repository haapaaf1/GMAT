//$Header$
//------------------------------------------------------------------------------
//                                  GmatCommand
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Darrel J. Conway
// Created: 2003/09/23
//
/**
 * Definition for the GmatCommand base class
 */
//------------------------------------------------------------------------------


// Class automatically generated by Dev-C++ New Class wizard


#ifndef Command_hpp
#define Command_hpp


#include <map>              // for mapping between object names and types
#include <algorithm>        // for find()
#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "CommandException.hpp"


// Headers for the referenced classes
#include "SolarSystem.hpp"   // for SolarSystem
#include "Subscriber.hpp"    // For the Publisher and base Subscribers


/**
 * GMAT GmatCommand Base Class, used for timeline elements in the script
 *
 * The GMAT GmatCommands all follow a "late-binding" philosophy, in that they do not
 * set object associations until the Sandbox has been populated with both the 
 * objects that are used in the model and with the complete GmatCommand sequence.
 * Once the Sandbox is populated, it initializes the GmatCommand sequence by calling
 * Initialize() on each GmatCommand, and then runs the sequence by calling Execute()
 * on the first GmatCommand in the sequence.
 */
class GmatCommand : public GmatBase
{
public:
   // class constructor
   GmatCommand(const std::string &typeStr);
   // class destructor
   virtual ~GmatCommand();
   // Copy constructor
   GmatCommand(const GmatCommand &c);
   // Assignment operator
   GmatCommand&         operator=(const GmatCommand &c);
   
   void                 SetGeneratingString(const std::string &gs);
   virtual const std::string&  
                        GetGeneratingString();
   
   // Methods used to setup objects
   virtual bool         SetObject(const std::string &name,
                                  const Gmat::ObjectType type,
                                  const std::string &associate = "",
                                  const Gmat::ObjectType associateType =
                                       Gmat::UNKNOWN_OBJECT);
   virtual bool         SetObject(GmatBase *obj,
                                  const Gmat::ObjectType type);
   virtual GmatBase*    GetObject(const Gmat::ObjectType type, 
                                  const std::string objName = "");
   
   virtual void         SetSolarSystem(SolarSystem *ss);
   virtual void         SetObjectMap(std::map<std::string, GmatBase *> *map);
   virtual void         SetPublisher(Publisher *p);
   
   // Sequence methods
   virtual bool         Initialize();
   virtual GmatCommand* GetNext();
   virtual bool         Append(GmatCommand *cmd);
   virtual bool         Insert(GmatCommand *cmd, GmatCommand *prev);
   virtual GmatCommand* Remove(GmatCommand *cmd);
   
   virtual GmatCommand* GetChildCommand(Integer whichOne = 0);
   
   virtual bool         InterpretAction()
   {
      return false;
   }
            
   /** 
   * The method that is fired to perform the GmatCommand.
   *
   * Derived classes implement this method to perform their actions on 
   * GMAT objects.
   *
   * @return true if the GmatCommand runs to completion, false if an error 
   *         occurs. 
   */
   virtual bool        Execute() = 0;

protected:
   // no additional parameters to add at this time
   enum
   {
      GmatCommandParamCount = GmatBaseParamCount,
   };
   
   /// Script string that was used to build the GmatCommand
   std::string           generatingString;
   /// Map containing names and associated types
   std::map<std::string, Gmat::ObjectType> 
                         association;
   /// List of the associated objects
   StringArray           objects;
   /// Flag used to determine if associations have been made
   bool                  initialized;
   /// Pointer to the next GmatCommand in the sequence; NULL at the end
   GmatCommand           *next;
   /// Indicator of the current nesting level
   Integer               level;
   /// Object store obtained from the Sandbox
   std::map<std::string, GmatBase *>
                         *objectMap;
   /// Solar System, set by the local Sandbox
   SolarSystem           *solarSys;
   /// Publisher for data generated by this GmatCommand
   Publisher             *publisher;
   
   virtual bool          AssignObjects(); 
   virtual bool          ClearObjects();    
};

#endif // Command_hpp
