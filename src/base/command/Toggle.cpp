//$Header$
//------------------------------------------------------------------------------
//                                  Toggle
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

#include "Toggle.hpp" // class's header file
#include "Publisher.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_RENAME 1

// class constructor
//------------------------------------------------------------------------------
// Toggle()
//------------------------------------------------------------------------------
Toggle::Toggle() :
   GmatCommand("Toggle"),
   toggleState(true),
   subscriberID(parameterCount),
   toggleStateID(++parameterCount)
{
   ++parameterCount;
}

// class destructor
//------------------------------------------------------------------------------
// ~Toggle()
//------------------------------------------------------------------------------
Toggle::~Toggle()
{
   // insert your code here
}


//------------------------------------------------------------------------------
// Toggle(const Toggle& t)
//------------------------------------------------------------------------------
Toggle::Toggle(const Toggle& t) :
   GmatCommand(t),
   subscriberID(t.subscriberID)
{
   parameterCount = t.parameterCount;
}


//------------------------------------------------------------------------------
// Toggle& operator=(const Toggle& t)
//------------------------------------------------------------------------------
Toggle& Toggle::operator=(const Toggle& t)
{
   return *this;
}


//------------------------------------------------------------------------------
// bool InterpretAction()
//------------------------------------------------------------------------------
bool Toggle::InterpretAction()
{
   /// @todo: Clean up this hack for the Toggle::InterpretAction method
   // Sample string:  "Toggle Report On"
    
   Integer loc = generatingString.find("Toggle", 0) + 6, end;
   const char *str = generatingString.c_str();
   while (str[loc] == ' ')
      ++loc;
    
   Integer cmd = generatingString.find("On", loc);
   while (generatingString.find("On", cmd+1) != std::string::npos)
      cmd = generatingString.find("On", cmd+1);
        
   if ((cmd == (Integer)std::string::npos) || (cmd <= (Integer)(generatingString.length() - 5))) {
      cmd = generatingString.find("Off", loc);
      while (generatingString.find("Off", cmd+1) != std::string::npos)
         cmd = generatingString.find("Off", cmd+1);
            
      if (cmd == (Integer)std::string::npos)
         throw CommandException("Must Toggle either 'On' or 'Off'");
      if (cmd > (Integer)(generatingString.length() - 6)) {
         toggleState = false;
      }
   }
   else
      if (cmd > (Integer)(generatingString.length() - 5)) {
         toggleState = true;
      }
            
   // Find the Subscriber list
   end = generatingString.find(" ", loc);
   std::string sName = generatingString.substr(loc, end-loc);
   subNames.push_back(sName);
    
   // Register with the publisher
   if (publisher == NULL)
      publisher = Publisher::Instance();
   streamID = publisher->RegisterPublishedData(subNames, subNames);
        
   return true;
}


//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
bool Toggle::Initialize()
{
   #ifdef DEBUG_TOGGLE
      MessageInterface::ShowMessage("Toggle::Initialize() entered\n");
   #endif
   Subscriber *sub;
   
   subs.clear();
    
   for (StringArray::iterator s = subNames.begin(); s != subNames.end(); ++s) {
      if ((*objectMap).find(*s) != objectMap->end()) {
         sub = (Subscriber *)(*objectMap)[*s];
         if (sub) {
            subs.push_back(sub);
         }
      }
      else {
         MessageInterface::ShowMessage
            ("Toggle command cannot find subscriber %s; command has no effect for that object\n",
             s->c_str());
      }
   }
   return true;
}


//------------------------------------------------------------------------------
// bool Execute()
//------------------------------------------------------------------------------
bool Toggle::Execute()
{
   #ifdef DEBUG_TOGGLE
      MessageInterface::ShowMessage("Toggle::Execute() entered\n");
   #endif

   for (std::list<Subscriber *>::iterator s = subs.begin(); s != subs.end(); ++s) {
      (*s)->Activate(toggleState);
   }
    
   char data[] = "Toggle executed\n\n";
   publisher->Publish(streamID, data, strlen(data));
   return true;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Toggle.
 *
 * @return clone of the Toggle.
 *
 */
//------------------------------------------------------------------------------
GmatBase* Toggle::Clone(void) const
{
   return (new Toggle(*this));
}


//loj: 11/22/04 added
//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool Toggle::RenameRefObject(const Gmat::ObjectType type,
                             const std::string &oldName,
                             const std::string &newName)
{
#if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Toggle::RenameConfiguredItem() type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
#endif
   
   if (type != Gmat::SUBSCRIBER)
      return true;

   for (unsigned int i=0; i<subNames.size(); i++)
   {
      if (subNames[i] == oldName)
         subNames[i] = newName;
   }

   return true;
}


//------------------------------------------------------------------------------
// std::string GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
std::string Toggle::GetParameterText(const Integer id) const
{
   if (id == subscriberID)
      return "Subscriber";
   else if (id == toggleStateID)
      return "ToggleState";
   return GmatCommand::GetParameterText(id);
}


//------------------------------------------------------------------------------
// Integer GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
Integer Toggle::GetParameterID(const std::string &str) const
{
   if (str == "Subscriber")
      return subscriberID;
   else if (str == "ToggleState")
      return toggleStateID;
   return GmatCommand::GetParameterID(str);
}


//------------------------------------------------------------------------------
// Gmat::ParameterType GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
Gmat::ParameterType Toggle::GetParameterType(const Integer id) const
{
   if (id == subscriberID)
      return Gmat::STRING_TYPE;
   return GmatCommand::GetParameterType(id);
}


//------------------------------------------------------------------------------
// std::string GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
std::string Toggle::GetParameterTypeString(const Integer id) const
{
   if (id == subscriberID)
      return PARAM_TYPE_STRING[Gmat::STRING_TYPE];
   return GmatCommand::GetParameterTypeString(id);
}


//------------------------------------------------------------------------------
// std::string GetStringParameter(const Integer id) const
//------------------------------------------------------------------------------
std::string Toggle::GetStringParameter(const Integer id) const
{
   //if (id == subscriberID)
   //   return "List of subscribers";
   
   //loj: 10/20/04 added new code
   if (id == subscriberID)
   {
      return subNames[0]; //loj: return first subscriber
   }
   else if (id == toggleStateID)
   {
      if (toggleState == true)
         return "On";
      else
         return "Off";
   }
   
   return GmatCommand::GetStringParameter(id);
}


//------------------------------------------------------------------------------
// bool SetStringParameter(const Integer id, const std::string &value)
//------------------------------------------------------------------------------
bool Toggle::SetStringParameter(const Integer id, const std::string &value)
{
   //if (id == subscriberID) {
   //   if (find(subNames.begin(), subNames.end(), value) == subNames.end()) {
   //      subNames.push_back(value);
   //      return true;
   //   }
   //   return false;
   //}

   //loj: 10/20/04 added new code
   if (id == subscriberID)
   {
      if (find(subNames.begin(), subNames.end(), value) == subNames.end())
      {
         subNames.push_back(value);
         return true;
      }
   }
   else if (id == toggleStateID)
   {
      if (value == "On")
         toggleState = true;
      else
         toggleState = false;

      return true;
   }
   
   return GmatCommand::SetStringParameter(id, value);
}

