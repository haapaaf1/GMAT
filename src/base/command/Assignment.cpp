//$Header$
//------------------------------------------------------------------------------
//                                Assignment
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Darrel J. Conway
// Created: 2004/02/03
//
/**
 * Implementation for the Assignment command class.
 * 
 * The assignment class is the Command class that handles commands of the form
 * 
 *     GMAT object.parameter = value;
 * 
 * Eventually (currently scheduled for build 4) this class will be extended to 
 * include numerical operators on the right side of the assignment line.
 */
//------------------------------------------------------------------------------


#include "Assignment.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_RENAME 1
//#define DEBUG_PARM_ASSIGNMENT

Assignment::Assignment  () :
   GmatCommand          ("GMAT"),
   ownerName            (""),
   parmName             (""),
   parmOwner            (NULL),
   rhsObject            (NULL),
   objToObj             (false),
   parmID               (-1),
   parmType             (Gmat::UNKNOWN_PARAMETER_TYPE),
   value                ("Not_Set")
{
}


Assignment::~Assignment()
{
}


Assignment::Assignment  (const Assignment& a) :
   GmatCommand          (a),
   ownerName            (a.ownerName),
   parmName             (a.parmName),
   parmOwner            (a.parmOwner),
   rhsObject            (a.rhsObject),
   parmID               (a.parmID),
   parmType             (a.parmType),
   value                (a.value)
{
}


Assignment& Assignment::operator=(const Assignment& a)
{
   if (this == &a)
      return *this;
        
   ownerName = a.ownerName;
   parmName  = a.parmName;
   parmOwner = a.parmOwner;
   rhsObject = a.rhsObject;
   objToObj  = a.objToObj;
   parmID    = a.parmID;
   parmType  = a.parmType;
   value     = a.value;
    
   return *this;
}


bool Assignment::InterpretAction()
{
   /// @todo: Clean up this hack for the Assignment::InterpretAction method
   // Assignment lines have the form GMAT Sat.Element1 = 7654.321; or
   // GMAT object1 = object2;
   Integer loc = generatingString.find("GMAT", 0) + 4, end;
   const char *str = generatingString.c_str();
   while (str[loc] == ' ')
      ++loc;
    
   end = generatingString.find(".", loc);
   if (end == (Integer)std::string::npos) {
      // Must be object = object assignment or Variable = value assignment
      Integer eqloc = generatingString.find("=", loc);
      if (eqloc == (Integer)std::string::npos)
         throw CommandException("Assignment string does not contain an '='");
      end = eqloc;
      while ((str[end] == ' ') || (str[end] == '='))
         --end;
      std::string component = generatingString.substr(loc, end-loc+1);
      ownerName = component;

      loc = eqloc;
      while ((str[loc] == ' ') || (str[loc] == '='))
         ++loc;
      end = loc;
      value     = &str[loc];
      end = value.find(";");
      value = value.substr(0, end);

      objToObj = true;
       
      return true;
   }
    
   std::string component = generatingString.substr(loc, end-loc);
   if (component == "")
      throw CommandException("Assignment string does not identify object");
   ownerName = component;
    
   loc = end + 1;
   end = generatingString.find("=", loc);
   if (end == (Integer)std::string::npos)
      throw CommandException("Assignment string does not set value");
    
   Integer strend = end;
   while (str[strend] == ' ')
      --strend;
   component = generatingString.substr(loc, strend-loc-1);
   if (component == "")
      throw CommandException("Assignment string does not identify parameter");
   parmName = component;
    
   loc = end + 1;
   while (str[loc] == ' ')
      ++loc;

   value     = &str[loc];
    
   end = value.find(";");
   value = value.substr(0, end);
    
   return true;
}


bool Assignment::Initialize()
{
   // Find the object
   if (objectMap->find(ownerName) == objectMap->end())
      throw CommandException("Assignment command cannot find object \"" +
                             ownerName + "\" for line \n" + generatingString);

   parmOwner = (*objectMap)[ownerName];
   if (objToObj) {
      if (objectMap->find(value) != objectMap->end()) {
         rhsObject = (*objectMap)[value];
      }
      else {
         if (parmOwner->GetTypeName() == "Variable") {
            parmName = "Expression";
            objToObj = false;
            #ifdef DEBUG_PARM_ASSIGNMENT
               MessageInterface::ShowMessage(
                  "Assignment::Initialize has owner %s, name %s, and val %s\n",
                  ownerName.c_str(), parmName.c_str(), value.c_str());
            #endif
         }
         else
            throw CommandException("Assignment command cannot find object \"" +
                     value + "\" for line \n" + generatingString);
      }
   }
   return true;
}


/**
 * The method that is fired to perform the command.
 *
 * Derived classes implement this method to perform their actions on 
 * GMAT objects.
 *
 * @return true if the GmatCommand runs to completion, false if an error 
 *         occurs. 
 */
bool Assignment::Execute()
{
   #ifdef DEBUG_PARM_ASSIGNMENT
      MessageInterface::ShowMessage("Assignment::Execute entered\n");
   #endif
   bool retval = false;

   // Get the parameter ID and ID type
   try {
      if (parmOwner == NULL)
         throw CommandException("Parameter Owner Not Initialized");
       
      if (objToObj) {
      #ifdef DEBUG_PARM_ASSIGNMENT
         MessageInterface::ShowMessage("Assignment::Execute running object to "
            "object\n");
      #endif
         if (!rhsObject)
            throw CommandException("Assignment command cannot find object \"" +
               value + "\"");
         if (parmOwner->GetTypeName() != rhsObject->GetTypeName())
            throw CommandException("Mismatched object types between \"" +
               parmOwner->GetName() + "\" and \"" + rhsObject->GetName() +
               "\"");
         parmOwner->Copy(rhsObject);
         return true;
      }

      parmID    = parmOwner->GetParameterID(parmName);
      parmType  = parmOwner->GetParameterType(parmID);

      #ifdef DEBUG_PARM_ASSIGNMENT
         MessageInterface::ShowMessage("Assignment::Execute Parameter %s has "
            "type %s\n", parmName.c_str(),
            parmOwner->GetParameterTypeString(parmID).c_str());
      #endif

      switch (parmType) {
         case Gmat::INTEGER_TYPE:
            parmOwner->SetIntegerParameter(parmID, atoi(value.c_str()));
            retval = true;
            break;
               
         case Gmat::REAL_TYPE:
            parmOwner->SetRealParameter(parmID, atof(value.c_str()));
            retval = true;
            break;
               
         case Gmat::STRING_TYPE:
         case Gmat::STRINGARRAY_TYPE:
            #ifdef DEBUG_PARM_ASSIGNMENT
               MessageInterface::ShowMessage("Assignment::Execute setting "
                  "string to %s\n", value.c_str());
            #endif
            parmOwner->SetStringParameter(parmID, value);
            retval = true;
            break;
               
         case Gmat::BOOLEAN_TYPE:
            bool tf;
            if (value == "true")
               tf = true;
            else
               tf = false;
            parmOwner->SetBooleanParameter(parmID, tf);
            retval = true;
            break;
   
         default:
            break;
      }

       // "Add" parameters could also mean to set reference objects
      if (parmName == "Add") {
         if (objectMap->find(value) != objectMap->end())
         {
            GmatBase *obj = (*objectMap)[value];
            if (obj)
               parmOwner->SetRefObject(obj, obj->GetType(), value);
         }
      }
   }
   catch (BaseException& ex)
   {
      if (parmOwner == NULL)
         throw;
      // Could be an action rather than a parameter
      if (!parmOwner->TakeAction(parmName, value))
         throw;
       
      retval = true;
   }

   #ifdef DEBUG_PARM_ASSIGNMENT
      MessageInterface::ShowMessage("Assignment::Execute finished\n");
   #endif
   return retval;
}

//loj: 2/22/05 added
//---------------------------------------------------------------------------
//  bool RenameRefObject(const Gmat::ObjectType type,
//                       const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
bool Assignment::RenameRefObject(const Gmat::ObjectType type,
                                 const std::string &oldName,
                                 const std::string &newName)
{
   #if DEBUG_RENAME
   MessageInterface::ShowMessage
      ("Assignment::RenameRefObject() type=%s, oldName=%s, newName=%s\n",
       GetObjectTypeString(type).c_str(), oldName.c_str(), newName.c_str());
   #endif

   // Assignment needs to know about spacecraft, formation
   if (type != Gmat::SPACECRAFT && type != Gmat::FORMATION )
      return true;

   if (ownerName == oldName)
      ownerName = newName;

   if (parmName == oldName)
      parmName = newName;

   if (value == oldName)
      value = newName;
   
   return true;
}


//------------------------------------------------------------------------------
//  GmatBase* Clone(void) const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Assignment.
 *
 * @return clone of the Assignment.
 *
 */
//------------------------------------------------------------------------------
GmatBase* Assignment::Clone() const
{
   return (new Assignment(*this));
}

