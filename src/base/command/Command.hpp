//$Header$
//------------------------------------------------------------------------------
//                                  Command
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
 * Definition for the Command base class
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


// Forward references for classes that we'll have eventually
class SolarSystem;
class Publisher;

// Headers for the referenced classes
//#include "Asset.hpp"        // for Spacecraft and GroundStations
//#include "SolarSystem.hpp"  // for SolarSystem
#include "Subscriber.hpp"    // For the Publisher


/**
 * GMAT Command Base Class, used for timeline elements in the script
 *
 * The GMAT Commands all follow a "late-binding" philosophy, in that they do not
 * set object associations until the Sandbox has been populated with both the 
 * objects that are used in the model and with the complete command sequence.
 * Once the Sandbox is populated, it initializes the command sequence by calling
 * Initialize() on each command, and then runs the sequence by calling Execute()
 * on the first command in the sequence.
 */
class Command : public GmatBase
{
	public:
        // class constructor
        Command(const std::string &typeStr);
        // class destructor
        virtual ~Command(void);
        // Copy constructor
        Command(const Command &c);
        // Assignment operator
        Command&            operator=(const Command &c);
        
        void                SetGeneratingString(const std::string &gs);
        const std::string&  GetGeneratingString(void) const;

        // Methods used to setup objects
        virtual bool        SetObject(const std::string &name,
                                      const Gmat::ObjectType type,
                                      const std::string &associate = "",
                                      const Gmat::ObjectType associateType =
                                            Gmat::UNKNOWN_OBJECT);
        virtual bool        SetObject(GmatBase *obj,
                                      const Gmat::ObjectType type);
        virtual GmatBase*   GetObject(const Gmat::ObjectType type, 
                                      const std::string objName = "");
        virtual void        SetSolarSystem(SolarSystem *ss);
        virtual void        SetObjectMap(std::map<std::string, GmatBase *> *map);
        virtual void        SetPublisher(Publisher *p);
        
        // Sequence methods
        virtual bool        Initialize(void);
        virtual Command*    GetNext(void);
        virtual bool        Append(Command *cmd);
        virtual bool        Insert(Command *cmd, Command *prev);

        virtual void        InterpretAction(void)
        {}
                
        /** 
         * The method that is fired to perform the command.
         *
         * Derived classes implement this method to perform their actions on 
         * GMAT objects.
         *
         * @return true if the Command runs to completion, false if an error 
         *         occurs. 
         */
        virtual bool        Execute(void) = 0;
        
    protected:
        /// Script string that was used to build the command
        std::string         generatingString;
        /// Map containing names and associated types
        std::map<std::string, Gmat::ObjectType> 
                            association;
        /// List of the associated objects
        StringArray         objects;
        /// Flag used to determine if associations have been made
        bool                initialized;
        /// Pointer to the next command in the sequence; NULL at the end
        Command             *next;
        /// Indicator of the current nesting level
        Integer             level;
        /// Object store obtained from the Sandbox
        std::map<std::string, GmatBase *>
                            *objectMap;
        /// Solar System, set by the local Sandbox
        SolarSystem         *solarSys;
        /// Publisher for data generated by this command
        Publisher           *publisher;

        virtual bool        AssignObjects(void); 
        virtual bool        ClearObjects(void);    
};

#endif // Command_hpp
