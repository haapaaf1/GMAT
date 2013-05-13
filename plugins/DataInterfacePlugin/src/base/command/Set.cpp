//$Id$
//------------------------------------------------------------------------------
//                                  Set
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Author: Darrel J. Conway
// Created: 2004/02/26
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Class implementation for the Set command
 */
//------------------------------------------------------------------------------

#include "Set.hpp"
#include "FileManager.hpp"      // for GetPathname()
#include "MessageInterface.hpp"
#include "GmatGlobal.hpp"       // for GetDataPrecision()

#define DEBUG_INITIALIZATION
#define DEBUG_SET_EXEC
#define DEBUG_SET_OUTPUT
#define DEBUG_PARSING

////---------------------------------
////  static data
////---------------------------------
//const std::string
//Set::PARAMETER_TEXT[SetParamCount - GmatCommandParamCount] =
//{
//   "ObjectNames",
//};
//
//const Gmat::ParameterType
//Set::PARAMETER_TYPE[SetParamCount - GmatCommandParamCount] =
//{
//   Gmat::STRINGARRAY_TYPE,   // "ObjectNames",
//};


//------------------------------------------------------------------------------
// Set()
//------------------------------------------------------------------------------
/**
 * Default constructor.
 */
//------------------------------------------------------------------------------
Set::Set() :
   GmatCommand       ("Set"),
   targetName        (""),
   target            (NULL),
   interfaceName     (""),
   theInterface      (NULL),
   loadAll           (true)
{
}


//------------------------------------------------------------------------------
// ~Set()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
Set::~Set()
{
}


//------------------------------------------------------------------------------
// Set(const Set& sv)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 * 
 * @param sv The instance that is copied.
 */
//------------------------------------------------------------------------------
Set::Set(const Set& sv) :
   GmatCommand       (sv),
   targetName        (sv.targetName),
   target            (NULL),
   interfaceName     (sv.interfaceName),
   theInterface      (NULL),
   loadAll           (sv.loadAll),
   selections        (sv.selections)
{
}


//------------------------------------------------------------------------------
// Set& operator=(const Set& sv)
//------------------------------------------------------------------------------
/**
 * Assignmant operator.
 * 
 * @param sv The instance that is copied.
 *
 * @return this instance, with internal data set to match the input command.
 */
//------------------------------------------------------------------------------
Set& Set::operator=(const Set& sv)
{
   if (this != &sv)
   {
      targetName    = sv.targetName;
      target        = NULL;
      interfaceName = sv.interfaceName;
      theInterface  = NULL;
      loadAll       = sv.loadAll;
      selections    = sv.selections;
   }
   
   return *this;
}


////------------------------------------------------------------------------------
//// std::string GetParameterText(const Integer id) const
////------------------------------------------------------------------------------
//std::string Set::GetParameterText(const Integer id) const
//{
//   if (id >= GmatCommandParamCount && id < SetParamCount)
//      return PARAMETER_TEXT[id - GmatCommandParamCount];
//   else
//      return GmatCommand::GetParameterText(id);
//}
//
//
////------------------------------------------------------------------------------
//// Integer GetParameterID(const std::string &str) const
////------------------------------------------------------------------------------
//Integer Set::GetParameterID(const std::string &str) const
//{
//   for (int i=GmatCommandParamCount; i<SetParamCount; i++)
//   {
//      if (str == PARAMETER_TEXT[i - GmatCommandParamCount])
//         return i;
//   }
//
//   return GmatCommand::GetParameterID(str);
//}
//
//
////------------------------------------------------------------------------------
//// Gmat::ParameterType GetParameterType(const Integer id) const
////------------------------------------------------------------------------------
//Gmat::ParameterType Set::GetParameterType(const Integer id) const
//{
//   if (id >= GmatCommandParamCount && id < SetParamCount)
//      return PARAMETER_TYPE[id - GmatCommandParamCount];
//   else
//      return GmatCommand::GetParameterType(id);
//}
//
//
////------------------------------------------------------------------------------
//// std::string GetParameterTypeString(const Integer id) const
////------------------------------------------------------------------------------
//std::string Set::GetParameterTypeString(const Integer id) const
//{
//   if (id >= GmatCommandParamCount && id < SetParamCount)
//      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
//   else
//      return GmatCommand::GetParameterTypeString(id);
//}
//
//
////------------------------------------------------------------------------------
////  bool  SetStringParameter(const Integer id, const std::string value)
////------------------------------------------------------------------------------
///**
// * This method sets the string parameter value, given the input
// * parameter ID.
// *
// * @param <id> ID for the requested parameter.
// * @param <value> string value for the requested parameter.
// *
// * @exception <CommandException> thrown if value is already in the list.
// *
// * @return  success flag.
// *
// */
////------------------------------------------------------------------------------
//bool Set::SetStringParameter(const Integer id, const std::string &value)
//{
//   if (id == OBJECT_NAMES)
//   {
//      Integer sz = objNameArray.size();
//      for (Integer ii = 0; ii < sz; ii++)
//      {
//         if (objNameArray[ii] == value)
//         {
//            std::string ex = "Attempting to add """;
//            ex += value + """ more than once to list of objects.\n";
//            throw CommandException(ex);
//         }
//      }
//
//      objNameArray.push_back(value);
//      return true;
//   }
//
//   return GmatCommand::SetStringParameter(id, value);
//}
//
//
////------------------------------------------------------------------------------
////  bool SetStringParameter(const std::string &label, const std::string &value)
////------------------------------------------------------------------------------
///**
// * Sets the value for a std::string parameter.
// *
// * @param <label> The (string) label for the parameter.
// * @param <value> New value for the parameter.
// *
// * @return The string stored for this parameter.
// */
////------------------------------------------------------------------------------
//bool Set::SetStringParameter(const std::string &label, const std::string &value)
//{
//   return SetStringParameter(GetParameterID(label), value);
//}
//
//
////------------------------------------------------------------------------------
////  std::string  GetStringParameter(const Integer id, const Integer index)
////------------------------------------------------------------------------------
///**
// * This method returns the string parameter value, given the input
// * parameter ID and the index into the array.
// *
// * @param <id> ID for the requested parameter.
// * @param <index> index into the StringArray parameter.
// *
// * @exception <CommandException> thrown if value is out of range
// *
// * @return  string value at index 'index'.
// *
// */
////------------------------------------------------------------------------------
//std::string Set::GetStringParameter(const Integer id,
//                                     const Integer index) const
//{
//   if (id == OBJECT_NAMES)
//   {
//      if ((index < 0) || (index >= ((Integer) objNameArray.size())))
//         throw CommandException
//            ("Index out of bounds when attempting to return object name\n");
//      return objNameArray.at(index);
//   }
//
//   return GmatCommand::GetStringParameter(id, index);
//}
//
//
////------------------------------------------------------------------------------
////  std::string GetStringParameter(const std::string &label,
////                                 const Integer index) const
////------------------------------------------------------------------------------
///**
// * Retrieve a string parameter.
// *
// * @param <label> The (string) label for the parameter.
// * @param <index> array index for the parameter.
// *
// * @return The string stored for this parameter.
// */
////------------------------------------------------------------------------------
//std::string Set::GetStringParameter(const std::string &label,
//                                     const Integer index) const
//{
//   return GetStringParameter(GetParameterID(label), index);
//}
//
//
////------------------------------------------------------------------------------
////  const StringArray&  GetStringArrayParameter(const Integer id)
////------------------------------------------------------------------------------
///**
// * This method returns the string array value, given the input
// * parameter ID .
// *
// * @param <id> ID for the requested parameter.
// *
//  * @return  string array.
// *
// */
////------------------------------------------------------------------------------
//const StringArray& Set::GetStringArrayParameter(const Integer id) const
//{
//   if (id == OBJECT_NAMES)
//      return objNameArray;
//
//   return GmatCommand::GetStringArrayParameter(id);
//}


//------------------------------------------------------------------------------
// bool GetRefObjectName(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Retrieves the reference object names.
 * 
 * @param type The type of the reference object.
 * 
 * @return the name of the object.
 */
//------------------------------------------------------------------------------
std::string Set::GetRefObjectName(const Gmat::ObjectType type) const
{
   if (type == Gmat::INTERFACE)
      return interfaceName;

   return targetName;
}


//------------------------------------------------------------------------------
// const StringArray& GetRefObjectNameArray(const Gmat::ObjectType type)
//------------------------------------------------------------------------------
/**
 * Accesses arrays of names for referenced objects.
 * 
 * @param type Type of object requested.
 * 
 * @return the StringArray containing the referenced object names.
 */
//------------------------------------------------------------------------------
const StringArray& Set::GetRefObjectNameArray(const Gmat::ObjectType type)
{
   refObjectNames.clear();
   refObjectNames.push_back(interfaceName);
   if (type != Gmat::INTERFACE)
      refObjectNames.push_back(targetName);

   // it can be any object, so ignore object type
   return refObjectNames;
}


//------------------------------------------------------------------------------
// bool SetRefObjectName(const Gmat::ObjectType type, const std::string &name)
//------------------------------------------------------------------------------
/**
 * Sets the reference objects that get Set and their sources.
 * 
 * @param type The type of the reference object.
 * @param name The name of the reference object.
 * 
 * @return true on success, false on failure.
 */
//------------------------------------------------------------------------------
bool Set::SetRefObjectName(const Gmat::ObjectType type,
                            const std::string &name)
{
//   if (name == "")
//      return false;
//
//   // Set works for all types, so we don't check the type parameter
//   objNameArray.push_back(name);
   return true;
}


//------------------------------------------------------------------------------
// bool InterpretAction()
//------------------------------------------------------------------------------
/**
 * Parses the scripting for the Set command
 *
 * @return true on success, false on failure
 */
//------------------------------------------------------------------------------
bool Set::InterpretAction()
{
   #ifdef DEBUG_SET_ASSEMBLE
      MessageInterface::ShowMessage
         ("%s::InterpretAction() genString = \"%s\"\n", typeName.c_str(),
          generatingString.c_str());
   #endif

   StringArray blocks = parser.DecomposeBlock(generatingString);

   StringArray chunks = parser.SeparateBrackets(blocks[0], "{}", " ", false);

   #ifdef DEBUG_PARSING
      MessageInterface::ShowMessage("Chunks from \"%s\":\n",
            blocks[0].c_str());
      for (StringArray::iterator i = chunks.begin(); i != chunks.end(); ++i)
         MessageInterface::ShowMessage("   \"%s\"\n", i->c_str());
   #endif

   if (chunks.size() < 3)
      throw CommandException(typeName + "::InterpretAction() cannot identify "
            "either the target or the data source -- is one missing? -- in "
            "line\n" + generatingString);

   if (chunks.size() > 4)
      throw CommandException(typeName +
            "::InterpretAction() found too many components to parse in the "
            "line\n" + generatingString);

   if (chunks[0] != typeName)
      throw CommandException(typeName + "::InterpretAction() does not identify "
            "the correct command type in line\n" + generatingString);

   targetName = chunks[1];
   interfaceName = chunks[2];

//   if (chunks.size() == 3)
//      CheckForOptions(chunks[2]);

   #ifdef DEBUG_PARSING
      MessageInterface::ShowMessage("%s::InterpretAction for \"%s\", type = %s\n",
            typeName.c_str(), generatingString.c_str(), typeName.c_str());
   #endif

   return true;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Connects up the object associations prior to saving them.
 * 
 * @return true on success
 */
//------------------------------------------------------------------------------
bool Set::Initialize()
{
   bool retval = GmatCommand::Initialize();
   
   target = FindObject(targetName);

   GmatBase *obj = FindObject(interfaceName);
   if (obj->IsOfType("DataInterface"))
      theInterface = (DataInterface*)obj;

   if ((theInterface == NULL) || (target == NULL))
      throw CommandException("The Set command could not find objects "
            "needed to initialize");
   
   return retval;
}


//------------------------------------------------------------------------------
// bool Execute()
//------------------------------------------------------------------------------
/**
 * Write objects to a text file.
 *
 * @return true if the Command runs to completion, false if an error
 *         occurs.
 */
//------------------------------------------------------------------------------
bool Set::Execute()
{
   #ifdef DEBUG_SET_EXEC
      MessageInterface::ShowMessage("Set::Execute() entered\n");
   #endif
   
   bool retval = false;
   
   if (theInterface->Open() == 0)
   {
      if (theInterface->LoadData())
         retval = true;
      #ifdef DEBUG_SET_EXEC
         else
            MessageInterface::ShowMessage("Set::Execute() LoadData returned "
                  "false\n");
      #endif

      if (theInterface->Close() != 0)
      {
         MessageInterface::ShowMessage("The interface failed to close\n");
      }
   }
   #ifdef DEBUG_SET_EXEC
      else
         MessageInterface::ShowMessage("Set::Execute(): The interface did not "
               "open\n");
   #endif

   
   return retval;
}


//------------------------------------------------------------------------------
// void RunComplete()
//------------------------------------------------------------------------------
void Set::RunComplete()
{
   GmatCommand::RunComplete();
}


//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Set.
 *
 * @return clone of the Set.
 */
//------------------------------------------------------------------------------
GmatBase* Set::Clone() const
{
   return (new Set(*this));
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
 * @param mode    Specifies the type of serialization requested.  (Not yet used
 *                in commands)
 * @param prefix  Optional prefix appended to the object's name.  (Not yet used
 *                in commands)
 * @param useName Name that replaces the object's name.  (Not yet used in
 *                commands)
 *
 * @return The script line that, when interpreted, defines this Set command.
 */
//------------------------------------------------------------------------------
const std::string& Set::GetGeneratingString(Gmat::WriteMode mode,
                                            const std::string &prefix,
                                            const std::string &useName)
{
   // Build the local string
   generatingString = prefix + "Set " + targetName + " " + interfaceName;

   /// @todo Add selections

   generatingString += ";";

   // Then call the base class method
   return GmatCommand::GetGeneratingString(mode, prefix, useName);
}


//------------------------------------------------------------------------------
// virtual bool TakeAction(const std::string &action,  
//                         const std::string &actionData = "");
//------------------------------------------------------------------------------
/**
 * This method performs action.
 *
 * @param <action> action to perform
 * @param <actionData> action data associated with action
 * @return true if action successfully performed
 *
 */
//------------------------------------------------------------------------------
bool Set::TakeAction(const std::string &action, const std::string &actionData)
{
   #if DEBUG_TAKE_ACTION
   MessageInterface::ShowMessage
      ("Set::TakeAction() action=%s, actionData=%s\n",
       action.c_str(), actionData.c_str());
   #endif
   
   if (action == "Clear")
   {
      selections.clear();
      return true;
   }

   return false;
}


//------------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//------------------------------------------------------------------------------
/**
 * This method updates object names when the user changes them.
 *
 * @param type Type of object that is renamed.
 * @param oldName Old name for the object.
 * @param newName New name for the object.
 *
 * @return true on success.
 */
//------------------------------------------------------------------------------
bool Set::RenameRefObject(const Gmat::ObjectType type,
                           const std::string &oldName,
                           const std::string &newName)
{
   if (targetName == oldName)
      targetName = newName;
   if (interfaceName == oldName)
      interfaceName = newName;
   
   return true;
}
