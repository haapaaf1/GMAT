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
 * Implementation code for the GmatCommand base class
 */
//------------------------------------------------------------------------------


#include "Command.hpp"          // class's header file
#include "MessageInterface.hpp" // MessageInterface
#include "Spacecraft.hpp"

#include <sstream>              // for command summary generation
#include <cmath>                // for Cartesian to Keplerian conversion; 
                                // remove it when the real conversions are
                                // used.


//#define DEBUG_COMMAND_DEALLOCATION


//---------------------------------
//  static members
//---------------------------------
const std::string 
   GmatCommand::PARAMETER_TEXT[GmatCommandParamCount - GmatBaseParamCount] =
   {
      "Comment",
      "Summary"
   };


const Gmat::ParameterType 
   GmatCommand::PARAMETER_TYPE[GmatCommandParamCount - GmatBaseParamCount] =
   {
      Gmat::STRING_TYPE,
      Gmat::STRING_TYPE
   };


//---------------------------------
//  public methods
//---------------------------------

//------------------------------------------------------------------------------
//  GmatCommand(const std::string &typeStr)
//------------------------------------------------------------------------------
/**
 * Constructs GmatCommand core structures (default constructor).
 */
//------------------------------------------------------------------------------
GmatCommand::GmatCommand(const std::string &typeStr) :
   GmatBase             (Gmat::COMMAND, typeStr),
   initialized          (false),
   next                 (NULL),
   level                (-1),   // Not set
   objectMap            (NULL),
   solarSys             (NULL),
   publisher            (NULL),
   streamID             (-1),
   depthChange          (0),
   commandChangedState  (false),
   comment              ("")
{
   generatingString = "";
   parameterCount = GmatCommandParamCount;
   objectTypes.push_back(Gmat::COMMAND);
   
   BuildCommandSummary(false);
}


//------------------------------------------------------------------------------
//  ~GmatCommand()
//------------------------------------------------------------------------------
/**
 * Destructs GmatCommand core structures.
 *
 * The GmatCommand destructor calls the destructor for the next GmatCommand in
 * the sequence.  Therefore, when a GmatCommand sequence is cleared, all the
 * Sandbox needs to do is call the destructor on the first GmatCommand in the
 * sequence, and the remaining GmatCommands are all destructed.
 *
 * @note The branching GmatCommands will also destroy all of the GmatCommands in
 *       the subbranch(es)
 */
//------------------------------------------------------------------------------
GmatCommand::~GmatCommand()
{
   #ifdef DEBUG_COMMAND_DEALLOCATION
      std::string nextStr = "NULL";
      if (next)
         nextStr = next->GetTypeName();
   
      MessageInterface::ShowMessage
         ("In GmatCommand::~GmatCommand() this=%s, next=%s\n",
          this->GetTypeName().c_str(), nextStr.c_str());
   #endif
      
   if (this->IsOfType("BranchEnd")) //loj: 6/29/05 Added
      return;
   
   // Delete the subsequent GmatCommands
   if (next) {
      #ifdef DEBUG_COMMAND_DEALLOCATION
         MessageInterface::ShowMessage("Deleting %s\n", 
                                       next->GetTypeName().c_str());
      #endif
      delete next;   
   }
}


//------------------------------------------------------------------------------
//  GmatCommand(const GmatCommand &c)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * The copy constructor for GmatCommands copies object lists and associations
 * from the "original" GmatCommand, c, but not its navigation parameters (next, 
 * level), nor does it initialize the GmatCommand.
 *
 * @param c GmatCommand object whose values to use to make "this" copy.
 */
//------------------------------------------------------------------------------
GmatCommand::GmatCommand(const GmatCommand &c) :
   GmatBase             (c),
   association          (c.association),
   objects              (c.objects),
   initialized          (false),
   next                 (NULL),
   level                (-1),   // Not set
   objectMap            (c.objectMap),
   solarSys             (c.solarSys),
   publisher            (c.publisher),
   streamID             (c.streamID),
   depthChange          (c.depthChange),
   commandChangedState  (c.commandChangedState)
{
   generatingString = c.generatingString;
   parameterCount = GmatCommandParamCount;
}


//------------------------------------------------------------------------------
//  GmatCommand operator=(const GmatCommand &c)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 *
 * The assignment operator for GmatCommands copies the object lists and
 * associations from the "other" GmatCommand, c, but not its navigation
 * parameters (next, level, depthChange, commandChangedState).   This operator
 * clears all object associations, and leaves "this" object in an uninitialized
 * state.
 *
 * @param c GmatCommand object whose values to use to set "this" GmatCommand
 *          object.
 *
 * @return GmatCommand object.
 */
//------------------------------------------------------------------------------
GmatCommand& GmatCommand::operator=(const GmatCommand &c)
{
   if (&c == this)
      return *this;

   GmatBase::operator=(c);
   initialized = false;
   objects.clear();
   association.clear();
   ClearObjects();             // Drop any previously set object pointers
   
   objects = c.objects;
   association = c.association;
   
   objectMap = c.objectMap;
   solarSys = c.solarSys;
   publisher = c.publisher;
   generatingString = c.generatingString;
   streamID = c.streamID;   
   return *this;
}


//------------------------------------------------------------------------------
//  void SetGeneratingString(const std::string &gs)
//------------------------------------------------------------------------------
/**
 * Method used to store the string that was parsed to build this GmatCommand.
 *
 * This method is used to store the GmatCommand string so that the script can be
 * written to a file without inverting the steps used to set up the internal
 * object data.
 *
 * @param <gs> The script line that was interpreted to define this GmatCommand.
 */
//------------------------------------------------------------------------------
void GmatCommand::SetGeneratingString(const std::string &gs)
{
   // Drop the leading white space
   UnsignedInt start = 0, end;
   while ((start < gs.length()) && (gs[start] == ' '))
      ++start;
   generatingString = gs.substr(start);
   
   end = gs.length()-1;
   while ((end > 0) && (gs[end] == ' '))
      --end;

   generatingString = gs.substr(0, end+1);
}

//------------------------------------------------------------------------------
//  const std::string& GetGeneratingString()
//------------------------------------------------------------------------------
/**
 * Method used to retrieve the string that was parsed to build this GmatCommand.
 *
 * This method is used to retrieve the GmatCommand string from the script that
 * was parsed to build the GmatCommand.  It is used to save the script line, so
 * that the script can be written to a file without inverting the steps taken to
 * set up the internal object data.  As a side benefit, the script line is
 * available in the GmatCommand structure for debugging purposes.
 *
 * @param mode    Specifies the type of serialization requested. (Not yet used
 *                in commands)
 * @param prefix  Optional prefix appended to the object's name.  (Not yet used
 *                in commands)
 * @param useName Name that replaces the object's name.  (Not yet used in
 *                commands)
 *
 * @return The script line that was interpreted to define this GmatCommand.
 */
//------------------------------------------------------------------------------
const std::string& GmatCommand::GetGeneratingString(Gmat::WriteMode mode,
                                            const std::string &prefix,
                                            const std::string &useName)
{
   static std::string empty;
   if (generatingString == "") {
      if (typeName == "NoOp")
         return generatingString;
      empty = "% Generating string not set for " + typeName + " command.";
      return empty;
   }
   return generatingString;
}


//------------------------------------------------------------------------------
//  bool SetObject(const std::string &name, const Gmat::ObjectType type,
//                 const std::string &associate, 
//                 const Gmat::ObjectType associateType)
//------------------------------------------------------------------------------
/**
 * Stores the object names and types required for the GmatCommand.
 *
 * Objects associated with a GmatCommand are not necessarily available at the
 * time the GmatCommand is parsed from the script.  This GmatCommand builds the
 * data store of object names that will be used to execute the GmatCommand when
 * the script is run.
 * 
 * @param name The name of the object.
 * @param type The (enumerated) base type assigned to the object.
 * @param associate Optional second object associated with the named object.
 *                  Defaults to the empty string ("").
 * @param associateType Type for the second object; defaults to
 *                      Gmat::UNKNOWN_OBJECT.
 *
 * @return true object is stored successfully, or it it is already registered
 *         with the same type, false otherwise.
 *
 * @note associate and associateType are provided to allow derived classes to
 *       override this method for GmatCommands that link two types of objects
 *       together -- for example, the Propagate GmatCommand links a propagator
 *       setup to a spacecraft.  These parameters are not used in this default
 *       implementation.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetObject(const std::string &name,
                        const Gmat::ObjectType type,
                        const std::string &associate,
                        const Gmat::ObjectType associateType)
{
   // Check to see if it is already in the object list
   StringArray::iterator item = std::find(objects.begin(), objects.end(), name);
   if (item != objects.end()) 
   {
      if (type != association[*item])  // make sure the types match
         return false;
   }
   else
   {
      // Add it to the lists
      objects.push_back(name);
      association[name] = type; 
   }
   
   return true;
}


//------------------------------------------------------------------------------
//  GmatBase* GetObject(const Gmat::ObjectType type, const std::string objName)
//------------------------------------------------------------------------------
/**
 * Retrieves the objects used by the GmatCommand.
 *
 * Objects associated with a GmatCommand are not necessarily available at the
 * time the GmatCommand is parsed from the script.  This GmatCommand builds the
 * data store of object names that will be used to execute the GmatCommand when
 * the script is run.
 * 
 * @param type    The (enumerated) base type assigned to the object.
 * @param objName The name of the object.
 *
 * @return A pointer to the requested object, obtained from the Configuration 
 *         Manager if it is a configured object, or from the local pointer if it
 *         is an internal object.
 */
//------------------------------------------------------------------------------
GmatBase* GmatCommand::GetObject(const Gmat::ObjectType type, 
                             const std::string objName)
{
   return NULL;
}


//------------------------------------------------------------------------------
//  bool SetObject(GmatBase *obj, const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Stores pointers to objects required exclusively for the current GmatCommand.
 *
 * Some GmatCommands require helper objects to execute successfully -- for
 * example, the Propagate GmatCommand uses a StoppingCondition object to
 * evaluate when the GmatCommand finishes execution.  Use this method to assign
 * these GmatCommand specific objects.
 *
 * @param obj Pointer to the object.
 * @param type The (enumerated) base type assigned to the object.
 *
 * @return true when object is stored successfully, false otherwise.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetObject(GmatBase *obj, const Gmat::ObjectType type)
{
   return false;
}


//------------------------------------------------------------------------------
//  void SetSolarSystem(SolarSystem *ss)
//------------------------------------------------------------------------------
/**
 * Called by the Sandbox to set the local solar system for the GmatCommand
 * 
 * @param <ss> Pointer to the local solar system
 */
//------------------------------------------------------------------------------
void GmatCommand::SetSolarSystem(SolarSystem *ss)
{
   solarSys = ss;
}


//------------------------------------------------------------------------------
//  void SetAssetMap(std::map<std::string, GmatBase *> *map)
//------------------------------------------------------------------------------
/**
 * Called by the Sandbox to set the local asset store used by the GmatCommand
 * 
 * @param <map> Pointer to the local object map
 */
//------------------------------------------------------------------------------
void GmatCommand::SetObjectMap(std::map<std::string, GmatBase *> *map)
{
   objectMap = map;
}


//------------------------------------------------------------------------------
//  void SetTransientForces(std::vector<PhysicalModel*> *tf)
//------------------------------------------------------------------------------
/**
 * Passes the transient force vector into the commands that need them
 * 
 * @param tf The vector of transient forces
 * 
 * @note The default behavior in the GmatCommands is to ignore the vector.
 */
//------------------------------------------------------------------------------
void GmatCommand::SetTransientForces(std::vector<PhysicalModel*> *tf)
{
}


//------------------------------------------------------------------------------
//  void SetPublisher(Publisher *p)
//------------------------------------------------------------------------------
/**
 * Sets the Publisher used for data generated by the GmatCommand
 * 
 * @param <p> Pointer to the Publisher
 */
//------------------------------------------------------------------------------
void GmatCommand::SetPublisher(Publisher *p)
{
   publisher = p;
}


//------------------------------------------------------------------------------
//  std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the name of the parameter with the input id.
 * 
 * @param <id> Integer id for the parameter.
 * 
 * @return The string name of the parameter.
 */
//------------------------------------------------------------------------------
std::string GmatCommand::GetParameterText(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < GmatCommandParamCount)
      return PARAMETER_TEXT[id - GmatBaseParamCount];
      
   return GmatBase::GetParameterText(id);
}


//------------------------------------------------------------------------------
//  Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * Gets the id corresponding to a named parameter.
 * 
 * @param <str> Name of the parameter.
 * 
 * @return The ID.
 */
//------------------------------------------------------------------------------
Integer GmatCommand::GetParameterID(const std::string &str) const
{
   for (Integer i = GmatBaseParamCount; i < GmatCommandParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatBaseParamCount])
         return i;
   }

   return GmatBase::GetParameterID(str);
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve the enumerated type of the object.
 *
 * @param <id> The integer ID for the parameter.
 *
 * @return The enumeration for the type of the parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType GmatCommand::GetParameterType(const Integer id) const
{
   if (id >= GmatBaseParamCount && id < GmatCommandParamCount)
      return PARAMETER_TYPE[id - GmatBaseParamCount];
      
   return GmatBase::GetParameterType(id);
}



//------------------------------------------------------------------------------
//  std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Gets the text description for the type of a parameter.
 * 
 * @param <id> Integer ID of the parameter.
 * 
 * @return The text description of the type of the parameter.
 */
//------------------------------------------------------------------------------
std::string GmatCommand::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 * 
 * For GmatCommands, this is a bit of a misnomer because the comment field is 
 * not read-only.  It is accessed only in special circumstances, though -- 
 * comment lines come before the command in the script -- and therefore gets
 * special treatment in the Interpreters.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool GmatCommand::IsParameterReadOnly(const Integer id) const
{
   if ((id >= GmatBaseParamCount) && (id < GmatCommandParamCount))
      return true;

   return GmatBase::IsParameterReadOnly(id);
}


//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 * 
 * @param <label> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool GmatCommand::IsParameterReadOnly(const std::string &label) const
{
   return IsParameterReadOnly(GetParameterID(label));
}


//------------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <id>    The integer ID for the parameter.
 *
 * @return The string stored for this parameter.
 */
//------------------------------------------------------------------------------
std::string GmatCommand::GetStringParameter(const Integer id) const
{
   if (id == COMMENT)
   {
      return comment;
   }
         
   if (id == SUMMARY)
   {
      return commandSummary;
   }
         
   return GmatBase::GetStringParameter(id);
}

   
//------------------------------------------------------------------------------
//  std::string GetStringParameter(const Integer id, const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <id>    The integer ID for the parameter.
 * @param <index> Array index for parameters.
 *
 * @return The string stored for this parameter.
 */
//------------------------------------------------------------------------------
std::string GmatCommand::GetStringParameter(const Integer id, 
                                            const Integer index) const
{
   return GmatBase::GetStringParameter(id, index);
}

   
//------------------------------------------------------------------------------
//  Real SetStringParameter(const Integer id, const Real value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetStringParameter(const Integer id, const std::string &value)
{
   if (id == COMMENT)
   {
      comment = value;
      return true;
   }
         
   if (id == SUMMARY)
   {
      return false;
   }
         
   return GmatBase::SetStringParameter(id, value);
}


//------------------------------------------------------------------------------
//  Real SetStringParameter(const Integer id, const Real value, 
//                          const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 * 
 * @param <id>    Integer ID of the parameter.
 * @param <value> New value for the parameter.
 * @param <index> array index for the parameter.
 * 
 * @return The value of the parameter.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetStringParameter(const Integer id, const std::string &value,
                                     const Integer index)
{
   return GmatBase::SetStringParameter(id, value, index);
}


//------------------------------------------------------------------------------
//  std::string GetStringParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <label> The (string) label for the parameter.
 *
 * @return The string stored for this parameter.
 */
//------------------------------------------------------------------------------
std::string GmatCommand::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}


//------------------------------------------------------------------------------
//  std::string GetStringParameter(const std::string &label,
//                                 const Integer index) const
//------------------------------------------------------------------------------
/**
 * Retrieve a string parameter.
 *
 * @param <label> The (string) label for the parameter.
 * @param <index> array index for the parameter.
 *
 * @return The string stored for this parameter.
 */
//------------------------------------------------------------------------------
std::string GmatCommand::GetStringParameter(const std::string &label,
                                            const Integer index) const
{
   return GetStringParameter(GetParameterID(label), index);
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const std::string &label, const std::string &value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 *
 * @param <label> The (string) label for the parameter.
 * @param <value> New value for the parameter.
 *
 * @return The string stored for this parameter.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetStringParameter(const std::string &label, 
                                     const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}


//------------------------------------------------------------------------------
//  bool SetStringParameter(const std::string &label, const std::string &value,
//                          const Integer index)
//------------------------------------------------------------------------------
/**
 * Sets the value for a std::string parameter.
 *
 * @param <label> The (string) label for the parameter.
 * @param <value> New value for the parameter.
 * @param <index> array index for the parameter.
 *
 * @return The string stored for this parameter.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetStringParameter(const std::string &label, 
                                     const std::string &value,
                                     const Integer index)
{
   return SetStringParameter(GetParameterID(label), value, index);
}


//------------------------------------------------------------------------------
//  bool SetCondition(const std::string &lhs, 
//                    const std::string &operation, 
//                    const std::string &rhs, 
//                    Integer atIndex)
//------------------------------------------------------------------------------
/**
 * Sets a condition for the command, at index atIndex (-999 to add to the end
 * of the list).
 * 
 * @param lhs       string for the left hand side of the condition
 * @param operation string for the operator
 * @param rhs       string for the right hand side of the condition
 * @param atIndex   where in the list to place the condition (-999 means to
 *                    add it to the end of the list (default))
 *
 * @return true if the condition was set, false if not.  The default
 *         (implemented here) returns false.
 *
 * @note See subclasses for more meaningful implementations.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetCondition(const std::string &lhs, 
                               const std::string &operation, 
                               const std::string &rhs, 
                               Integer atIndex)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool SetConditionOperator(const std::string &op, 
//                            Integer atIndex)
//------------------------------------------------------------------------------
/**
 * Sets a logical operator for the command, at index atIndex (-999 to add to the
 * end of the list) - this is the operator connecting conditions, when there are
 * multiple conditions.
 * 
 * @param op      string for the logical operator
 * @param atIndex where in the list to place the logical operator (-999 means
 *                to add it to the end of the list (default))
 *
 * @return true if the operator was set, false if not.  The default
 *         (implemented here) returns false.
 *
 * @note See subclasses for more meaningful implementations.
 */
//------------------------------------------------------------------------------
bool GmatCommand::SetConditionOperator(const std::string &op, 
                                       Integer atIndex)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool RemoveCondition(Integer atIndex)
//------------------------------------------------------------------------------
/**
 * Removes the condition for the command, at index atIndex.
 * 
 * @param atIndex where in the list to remove the condition from.
 *
 * @return true if the condition was removed, false if not.  The default
 *         (implemented here) returns false.
 *
 * @note See subclasses for more meaningful implementations.
 */
//------------------------------------------------------------------------------
bool GmatCommand::RemoveCondition(Integer atIndex)
{
   return false;
}

//------------------------------------------------------------------------------
//  bool RemoveConditionOperator(Integer atIndex)
//------------------------------------------------------------------------------
/**
 * Removes the logical operator for the command, at index atIndex.
 * 
 * @param atIndex where in the list to remove the logical operator from.
 *
 * @return true if the operator was removed, false if not.  The default
 *         (implemented here) returns false.
 *
 * @note See subclasses for more meaningful implementations.
 */
//------------------------------------------------------------------------------
bool GmatCommand::RemoveConditionOperator(Integer atIndex)
{
   return false;
}


//------------------------------------------------------------------------------
//  bool Initialize()
//------------------------------------------------------------------------------
/**
 * Initializes the GmatCommand structures at the start of a run.
 *
 * This method is called by the Sandbox when it receives a run GmatCommand from
 * the Moderator.  This method calls AssignObjects to set the local object
 * pointers prior to execution.
 *
 * Derived GmatCommand objects can use this method to set up any internal data 
 * structures needed to run the GmatCommand.  If this method is overridden, the
 * base class method provided here should be called to ensure that AssignObjects
 * is called at the start of the run.
 *
 * @return true if the GmatCommand is initialized, false if an error occurs.
 */
//------------------------------------------------------------------------------
bool GmatCommand::Initialize()
{
   // Check to be sure the basic infrastructure is in place
   if (objectMap == NULL)
   {
      std::string errorstr("Object map has not been initialized for ");
      errorstr += GetTypeName();
      throw CommandException(errorstr);
   }
   
   if (solarSys == NULL) {
      std::string errorstr("Solar system has not been initialized for ");
      errorstr += GetTypeName();
      throw CommandException(errorstr);
   }
   
   initialized = AssignObjects();
   if (publisher == NULL)
      publisher = Publisher::Instance();
   return initialized;
}


//------------------------------------------------------------------------------
//  GmatCommand* GetNext()
//------------------------------------------------------------------------------
/**
 * Accesses the next GmatCommand to be executed in the GmatCommand sequence
 * 
 * @return Pointer to the next GmatCommand
 */
//------------------------------------------------------------------------------
GmatCommand* GmatCommand::GetNext()
{
   return next;
}

bool GmatCommand::ForceSetNext(GmatCommand *toCmd) // dangerous!
{
   next = toCmd;
   return true;
}


//------------------------------------------------------------------------------
//  bool Append(GmatCommand *cmd)
//------------------------------------------------------------------------------
/**
 * Adds GmatCommand at the end of the GmatCommand sequence
 * 
 * @param cmd Pointer to GmatCommand that is added at the end
 *
 * @return true on success, false on failure.
 */
//------------------------------------------------------------------------------
bool GmatCommand::Append(GmatCommand *cmd)
{
   if (cmd == this)
      throw CommandException("Attempting to add GmatCommand already in list");
   if (next)
      next->Append(cmd);
   else
      next = cmd;
      
   return true;
}


//------------------------------------------------------------------------------
//  bool Insert(GmatCommand *cmd, GmatCommand *prev)
//------------------------------------------------------------------------------
/**
 * Inserts GmatCommand into the GmatCommand sequence
 * 
 * @param cmd  Pointer to GmatCommand that is inserted
 * @param prev Pointer to GmatCommand preceding this GmatCommand
 *
 * @return true on success, false on failure.
 */
//------------------------------------------------------------------------------
bool GmatCommand::Insert(GmatCommand *cmd, GmatCommand *prev)
{
   if (this == prev)
   {
      GmatCommand *temp = next;
      if (!next) return Append(cmd);
      next = cmd;
      return next->Append(temp); // assumes cmd->next is NULL and next != NULL
   }
   
   if (next == NULL)
      return false;
   return next->Insert(cmd, prev);
}

//------------------------------------------------------------------------------
//  GmatCommand* Remove(GmatCommand *cmd)
//------------------------------------------------------------------------------
/**
 * Removes GmatCommand from the GmatCommand sequence
 * 
 * @param cmd Pointer to GmatCommand that is inserted
 * 
 * @return Pointer to the removed GmatCommand or NULL if the GmatCommand was not
 *         found.
 */
//------------------------------------------------------------------------------
GmatCommand* GmatCommand::Remove(GmatCommand *cmd)
{
   if (this->IsOfType("BranchEnd")) //loj: 6/29/05 Added
      return NULL;
   
   if (this == cmd)
   {  // NULL the next pointer
      next = NULL;
      return this;
   }
   
   if (next == NULL)
      return NULL;
      
   if (next == cmd)
   {
      GmatCommand *temp = next;
      next = next->GetNext();
      temp->Remove(cmd);
      return temp;            
   }
   
   return next->Remove(cmd);
}


//------------------------------------------------------------------------------
//  GmatCommand* GetChildCommand(Integer whichOne)
//------------------------------------------------------------------------------
/**
 * Returns nested commands.
 *
 * This method returns the entry point for the commands in a branch off of the
 * main command sequence.  The base implementation here always returns NULL; 
 * see the BranchCommand implementation for a less trivial example.
 *
 * @param whichOne Determines which child command entry point is returned.  
 * 
 * @return The pointer to the first GmatCommand in the branch.
 */
//------------------------------------------------------------------------------
GmatCommand* GmatCommand::GetChildCommand(Integer whichOne)
{
   return NULL;
}


//------------------------------------------------------------------------------
//  Integer GetVariableCount()
//------------------------------------------------------------------------------
/**
 * Counts up variables.
 * 
 * @return The number of variables in this command and its children.
 */
//------------------------------------------------------------------------------
Integer GmatCommand::GetVariableCount()
{
   return 0;
}


//------------------------------------------------------------------------------
//  Integer GetGoalCount()
//------------------------------------------------------------------------------
/**
 * Counts up goals.
 * 
 * @return The number of goals in this command and its children.
 */
//------------------------------------------------------------------------------
Integer GmatCommand::GetGoalCount()
{
   return 0;
}


//------------------------------------------------------------------------------
//  bool AssignObjects()
//------------------------------------------------------------------------------
/**
 * Sets the internal object pointers prior to a run.
 *
 * This default implementation doesn't do anything.  
 *
 * Derived classes should override this method to set the internal data pointers 
 * needed to execute the GmatCommand by accessing the various maps of objects
 * and setting corresponding internal pointers.
 * 
 * @return true if the pointers are assigned "correctly", false otherwise.
 */
//------------------------------------------------------------------------------
bool GmatCommand::AssignObjects()
{
   return true;
}


//------------------------------------------------------------------------------
//  bool ClearObjects()
//------------------------------------------------------------------------------
/**
 * Clears the internal object pointers.
 *
 * This default implementation doesn't do anything.  
 *
 * Derived classes should override this method to "uninitialize" internal 
 * pointers when the assignment operator is called on this GmatCommand.
 * 
 * @return true if the pointers are assigned "correctly", false otherwise.
 */
//------------------------------------------------------------------------------
bool GmatCommand::ClearObjects()
{
   return true;
}


//------------------------------------------------------------------------------
//  bool GmatCommand::InterpretAction()
//------------------------------------------------------------------------------
/**
 * Performs command actions.
 *
 * This default implementation always returns false because the base class does
 * not perform any actions.
 *
 * @return true if an action was taken, false otherwise.
 */
//------------------------------------------------------------------------------
bool GmatCommand::InterpretAction()
{
   return false;
}


//------------------------------------------------------------------------------
//  Integer DepthIncrement()
//------------------------------------------------------------------------------
/**
 * Indicates the change in branch depth for subsequent commands.
 *
 * This default implementation always returns false because the base class does
 * not perform any actions.
 *
 * @return 0 if the depth stays the same, 1 if the depth increases, or -1 if
 *           it decreases 
 */
//------------------------------------------------------------------------------
Integer GmatCommand::DepthIncrement()
{
   return depthChange;
}


//------------------------------------------------------------------------------
//  bool HasPropStateChanged()
//------------------------------------------------------------------------------
/**
 * Indicates that the propagation data was changed by the command.
 *
 * Some commands can change propagation data in a manner that requires that the
 * propagators and force models need to rebuild their data structures, or that 
 * multistep integrators need to perform a restart before proceeding.  The
 * Sandbox manages the communication to perform propagator restarts.  It checks
 * for state changes of this type by calling this method, and then using the 
 * reference object methods to determine which object (or objects) is affected 
 * by the change.
 *
 * @return true if propagation state has changed, false if there was no change.
 */
//------------------------------------------------------------------------------
bool GmatCommand::HasPropStateChanged()
{
   return commandChangedState;
}


//------------------------------------------------------------------------------
//  void RunComplete()
//------------------------------------------------------------------------------
/**
 * Tells the sewuence that the run was ended, possibly before reaching the end.
 *
 * Some commands -- for instance, the BranchCommands -- track state during a 
 * run.  This method is called when the sequence is terminated by the Moderator
 * to reset the commands to an idle state.
 */
//------------------------------------------------------------------------------
void GmatCommand::RunComplete()
{
   #ifdef DEBUG_COMMAND_DEALLOCATION
      MessageInterface::ShowMessage("RunComplete for %s\n", typeName.c_str());
   #endif
   
   // By default, just call RunComplete on the next command.
   if (next)
      // Branch command ends point back to start; this line prevents looping
      if (!next->IsOfType("BranchEnd"))
         next->RunComplete();
}


//------------------------------------------------------------------------------
//  void BuildCommandSummary(bool commandCompleted)
//------------------------------------------------------------------------------
/**
 * Generates the summary string for the command
 *
 * Inherited commands override this method for specialized summary data.
 * 
 * @param <commandCompleted> True if the command ran successfully.
 */
//------------------------------------------------------------------------------
void GmatCommand::BuildCommandSummary(bool commandCompleted)
{
   std::stringstream data;
   
   StateConverter    stateConverter;
   
      
   data << "Command Summary: " << typeName << " Command\n";
   if (!commandCompleted)
      data << "Execute the script to generate command summary data\n";
   else
   {
      data << "-------------------------------------------"
           << "-------------------------------------------\n";

      // Build summary data for each spacecraft in the object list
      GmatBase *obj;
      for (std::map<std::string, GmatBase *>::iterator i = objectMap->begin();
           i != objectMap->end(); ++i)
      {
         obj = i->second;
         if (obj->GetTypeName() == "Spacecraft")
         {
            Rvector6 rawState = ((Spacecraft*)obj)->GetState().GetState();
            Rvector6 newState = rawState;
                                
            data.precision(16);                 
            data << "  Spacecraft " << obj->GetName() << "\n"
                 << "     A.1 Modified Julian Epoch: " 
                 << obj->GetRealParameter("Epoch") << "\n\n"
                 << "    Coordinate System: EarthMJ2000Eq \n\n"
                 << "    Cartesian State:\n"
                 << "        X  = " << newState[0] << " km\n"
                 << "        Y  = " << newState[1] << " km\n"
                 << "        Z  = " << newState[2] << " km\n"
                 << "        VX = " << newState[3] << " km/s\n"
                 << "        VY = " << newState[4] << " km/s\n"
                 << "        VZ = " << newState[5] << " km/s\n";

            obj->SetStringParameter("StateType", "Keplerian");
            
            CartToKep(rawState, newState);
                             
            data << "\n    Keplerian State:\n"
                 << "        SMA  = " << newState[0] << " km\n"
                 << "        ECC  = " << newState[1] << "\n"
                 << "        INC  = " << newState[2] << " deg\n"
                 << "        RAAN = " << newState[3] << " deg\n"
                 << "        AOP  = " << newState[4] << " deg\n"
                 << "        TA   = " << newState[5] << " deg\n";
                 
            data << "\n\n    Spacecraft properties:\n"
                 << "        Cd = " 
                 << obj->GetRealParameter("Cd") << "\n"
                 << "        Drag area = " 
                 << obj->GetRealParameter("DragArea") << " m^2\n"
                 << "        Cr = " 
                 << obj->GetRealParameter("Cr") << "\n"
                 << "        Reflective (SRP) area = " 
                 << obj->GetRealParameter("SRPArea") << " m^2\n";
                 
            StringArray tanks = obj->GetStringArrayParameter("Tanks");
            if (tanks.size() > 0)
            {
               data << "        Dry mass = "
                    << obj->GetRealParameter("DryMass") << " kg\n";
               data << "        Tanks:\n";
               for (StringArray::iterator i = tanks.begin();
                    i != tanks.end(); ++i)
                  data << "           " << (*i) << "\n";
            }
                 
            data << "        Total mass = " 
                 << obj->GetRealParameter("TotalMass") << " kg\n";

            data << "-------------------------------------------"
                 << "-------------------------------------------\n";
         }
      }
   }

   commandSummary = data.str();
}


////------------------------------------------------------------------------------
////  void BuildCommandSummary(bool commandCompleted)
////------------------------------------------------------------------------------
///**
// * Generates the summary string for the command
// *
// * Inherited commands override this method for specialized summary data.
// * 
// * @param <commandCompleted> True if the command ran successfully.
// */
////------------------------------------------------------------------------------
//std::string GmatCommand::GetCommandSummary()
//{
//   return commandSummary;
//}


// Temporary -- need to figure out how we're supposed to do transformations 
// generically
void GmatCommand::CartToKep(const Rvector6 in, Rvector6 &out)
{
   Real mu = 398600.4415;
   Real r = sqrt(in[0]*in[0]+in[1]*in[1]+in[2]*in[2]);
   Real v2 = in[3]*in[3]+in[4]*in[4]+in[5]*in[5];
   Real rdotv = in[0]*in[3] + in[1]*in[4] + in[2]*in[5];
   Real energy = 0.5*v2 - mu / r;
   /// SMA
   out[0] = -mu / (2.0 * energy);
   
   Rvector3 h, ecc;

   // angular vel = r cross v
   h[0] = in[1]*in[5] - in[2]*in[4];
   h[1] = in[2]*in[3] - in[0]*in[5];
   h[2] = in[0]*in[4] - in[1]*in[3];
   // eccentricity vector
   ecc[0] = (in[4]*h[2] - in[5]*h[1]) / mu - in[0] / r;
   ecc[1] = (in[5]*h[0] - in[3]*h[2]) / mu - in[1] / r;
   ecc[2] = (in[3]*h[1] - in[4]*h[0]) / mu - in[2] / r;
   
   // eccentricity
   out[1] = sqrt(ecc[0]*ecc[0] + ecc[1]*ecc[1] + ecc[2]*ecc[2]);
   
   // inclination
   Real hMag = sqrt(h[0]*h[0] + h[1]*h[1] + h[2]*h[2]);
   out[2] = acos(h[2]/hMag) * 180.0 / M_PI;
   
   Rvector3 nodevec;
   nodevec[0] = -h[1];
   nodevec[1] = h[0];
   nodevec[2] = 0.0;

   Real magnodevec = sqrt(nodevec[0]*nodevec[0] + nodevec[1]*nodevec[1]);

   // RAAN
   out[3] = atan2(nodevec[1], nodevec[0]) * 180.0 / M_PI;

   // AOP
   out[4] = acos((ecc[0]*nodevec[0] + ecc[1]*nodevec[1]) / (out[1]*magnodevec));
   if (ecc[2] < 0)
      out[4] = 2.0 * M_PI - out[4];
   out[4] *= 180.0 / M_PI;

   // TA
   out[5] = acos((ecc[0]*in[0] + ecc[1]*in[1] + ecc[2]*in[2]) / 
                 (out[1] * r));
   if (rdotv < 0)
      out[5] = 2.0 * M_PI - out[5];
   out[5] *= 180.0 / M_PI;
}
