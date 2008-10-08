//$Id$
//------------------------------------------------------------------------------
//                                 Create
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Wendy C. Shoan
// Created: 2008.03.14
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CCA54C
//
/**
 * Class implementation for the Create command
 */
//------------------------------------------------------------------------------


#include "Create.hpp"
#include "MessageInterface.hpp"
#include "CommandException.hpp"
#include "StringUtil.hpp"
#include "Array.hpp"

//#define DEBUG_CREATE
//#define DEBUG_CREATE_INIT

//---------------------------------
// static data
//---------------------------------
const std::string
Create::PARAMETER_TEXT[CreateParamCount - ManageObjectParamCount] =
{
      "ObjectType",
};

const Gmat::ParameterType
Create::PARAMETER_TYPE[CreateParamCount - ManageObjectParamCount] =
{
      Gmat::STRING_TYPE,
};

//------------------------------------------------------------------------------
// Create()
//------------------------------------------------------------------------------
/**
 * Constructor
 */
//------------------------------------------------------------------------------
Create::Create() :
   ManageObject("Create"),
   refObj       (NULL)
{
   //creations.clear();
}


//------------------------------------------------------------------------------
// ~Create()
//------------------------------------------------------------------------------
/**
 * Destructor
 */
//------------------------------------------------------------------------------
Create::~Create()
{
   //creations.clear();
   if (refObj) delete refObj;
   // assuming objects inseted into a valid Object Store (Sandbox, Function, or Global)
   // will be deleted as members of those stores at the appropriate time; objects created 
   // in the Create command that were not successfully inserted into an object store will
   // need to be deleted in the Execute method; therefore, no objects, other than the
   // reference object, need to be deleted here
}


//------------------------------------------------------------------------------
// Create(const Create &cr)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 * 
 * @param <cr> The instance that gets copied.
 */
//------------------------------------------------------------------------------
Create::Create(const Create &cr) :
   ManageObject(cr),
   objType  (cr.objType)//,
   //creations(cr.creations)
{
   if (cr.refObj)  refObj = (cr.refObj)->Clone();
   else            refObj = NULL;
}


//------------------------------------------------------------------------------
// Create& operator=(const Create &cr)
//------------------------------------------------------------------------------
/**
 * Assignment operator
 * 
 * @param <cr> The Create command that gets copied.
 * 
 * @return A reference to this instance.
 */
//------------------------------------------------------------------------------
Create& Create::operator=(const Create &cr)
{
   if (&cr != this)
   {
      ManageObject::operator=(cr);
      objType    = cr.objType;
      if (refObj) delete refObj;
      refObj     = (cr.refObj)->Clone();
      //creations.clear();
      //creations  = cr.creations; // do I want to do this? I don't think so
   }
   
   return *this;
}

// Parameter access methods - overridden from GmatBase
std::string Create::GetParameterText(const Integer id) const
{
   if (id >= ManageObjectParamCount && id < CreateParamCount)
      return PARAMETER_TEXT[id - ManageObjectParamCount];
   return ManageObject::GetParameterText(id);
}

Integer Create::GetParameterID(const std::string &str) const
{
   for (Integer i = ManageObjectParamCount; i < CreateParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - ManageObjectParamCount])
         return i;
   }
   
   return ManageObject::GetParameterID(str);
}

Gmat::ParameterType Create::GetParameterType(const Integer id) const
{
   if (id >= ManageObjectParamCount && id < CreateParamCount)
      return PARAMETER_TYPE[id - ManageObjectParamCount];
      
   return ManageObject::GetParameterType(id);
}

std::string Create::GetParameterTypeString(const Integer id) const
{
   return ManageObject::PARAM_TYPE_STRING[GetParameterType(id)];
}


std::string Create::GetStringParameter(const Integer id) const
{
   if (id == OBJECT_TYPE)
   {
      return objType;
   }
   return ManageObject::GetStringParameter(id);
}

std::string Create::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}

bool Create::SetStringParameter(const Integer id, 
                                const std::string &value)
{
   if (id == OBJECT_TYPE)
   {
      #ifdef DEBUG_CREATE
         MessageInterface::ShowMessage(
               "Create::SetStringParameter() setting object type to:  %s\n",
               value.c_str());
      #endif
      objType = value;
      return true;
   }
   return ManageObject::SetStringParameter(id, value);
}

bool Create::SetStringParameter(const std::string &label, 
                                const std::string &value)
{
   return SetStringParameter(GetParameterID(label),value);
}


GmatBase* Create::GetRefObject(const Gmat::ObjectType type,
                               const std::string &name)
{
   if ((refObj->GetType() == type) &&
       (refObj->GetName() == name))
      return refObj;
   return NULL;
}

bool Create::SetRefObject(GmatBase *obj, const Gmat::ObjectType type,
                          const std::string &name)
{
   #ifdef DEBUG_CREATE
      MessageInterface::ShowMessage("Create::SetRefObject() entered, and expecting an object of  "
            "type: '%s'\n", obj->GetTypeName().c_str());
      MessageInterface::ShowMessage("Create::SetRefObject() entered, with object "
            "type: '%s', name: '%s'\n", obj->GetTypeName().c_str(), name.c_str());
   #endif
   if ((!GmatStringUtil::IsBlank(objType)) &&
       !(obj->IsOfType(objType)))
      throw CommandException(
            "Reference object for Create command is not of expected type of \"" +
            objType + "\"");
   if (refObj)
   {
      throw CommandException(
            "Reference object for Create command already set.\n"); 
   }
   refObj = obj;
   return true;
}

//------------------------------------------------------------------------------
// GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Override of the GmatBase clone method.
 * 
 * @return A new copy of this instance.
 */
//------------------------------------------------------------------------------
GmatBase* Create::Clone() const
{
   return new Create(*this);
}



//------------------------------------------------------------------------------
// bool Initialize()
//------------------------------------------------------------------------------
/**
 * Method that initializes the internal data structures.
 * 
 * @return true if initialization succeeds.
 */
//------------------------------------------------------------------------------
bool Create::Initialize()
{
   #ifdef DEBUG_CREATE_INIT
      MessageInterface::ShowMessage("Create::Initialize() entered, for object type %s\n",
            objType.c_str());
      ShowObjectMaps("object maps at the start");
      MessageInterface::ShowMessage("   Create command has %d objectName(s):\n", objectNames.size());
      for (unsigned int ii = 0; ii < objectNames.size(); ii++)
         MessageInterface::ShowMessage("   ........ %s\n", (objectNames.at(ii)).c_str());
      MessageInterface::ShowMessage("   Create command has %d arrayName(s):\n", arrayNames.size());
      for (unsigned int ii = 0; ii < arrayNames.size(); ii++)
         MessageInterface::ShowMessage("   ........ %s\n", (arrayNames.at(ii)).c_str());
   #endif
      
   ManageObject::Initialize();
   
   // throw an exception if the object type or reference object has not been set
   if (GmatStringUtil::IsBlank(objType))
      throw CommandException("Object type not set for Create command.\n");
   if (!refObj) 
   {
      std::string ex = "No reference object of type """ + objType;
      ex += """ set for Create command.\n";
      throw CommandException(ex);
   }
   
   //---------------------------------- debug
   #ifdef DEBUG_CREATE_INIT
   MessageInterface::ShowMessage("   Create command has a reference object of type %s \n", 
         (refObj->GetTypeName()).c_str());
   #endif
   //---------------------------------- debug
   
   // Clone the reference object to create as many of the requested type of 
   // object as needed (reuse the array names if it is an Array)
   StringArray useNames = objectNames;
   if (objType == "Array")
   {
      SetArrayInfo();
      useNames = arrayNames;
   }
   Integer numNames = (Integer) useNames.size();
   for (Integer jj = 0; jj < numNames; jj++)
   {
      GmatBase *newObj = refObj->Clone();
      newObj->SetName(useNames.at(jj));
      if (refObj->GetType() == Gmat::COORDINATE_SYSTEM)
         newObj->SetSolarSystem(((CoordinateBase*)refObj)->GetSolarSystem());
      #ifdef DEBUG_CREATE_INIT
      MessageInterface::ShowMessage("   Creating object of type %s with name '%s' \n", 
            (newObj->GetTypeName()).c_str(), (useNames.at(jj)).c_str());
      #endif
      //creations.push_back(newObj);
      //creations.at(jj)->SetName(useNames.at(jj));
      if (objType == "Array") 
         ((Array*) (newObj))->SetSize(rows.at(jj), columns.at(jj));
         //((Array*) (creations.at(jj)))->SetSize(rows.at(jj), columns.at(jj));
      InsertIntoObjectStore(newObj, useNames.at(jj));
   }
   #ifdef DEBUG_CREATE_INIT
      MessageInterface::ShowMessage("Exiting Create::Initialize()\n");
      ShowObjectMaps("object maps at the end");
   #endif
   return true;
}


//---------------------------------------------------------------------------
//  bool GmatCommand::Execute()
//---------------------------------------------------------------------------
/**
 * The method that is fired to perform this Create command.
 *
 * @return true if the Create runs to completion, false if an error
 *         occurs.
 */
//---------------------------------------------------------------------------
bool Create::Execute()
{
//   #ifdef DEBUG_CREATE
//      MessageInterface::ShowMessage("Create::Execute() entered, for object type %s\n",
//            objType.c_str());
//      std::map<std::string, GmatBase *>::iterator omi;
//      MessageInterface::ShowMessage("   and at the start,  LOS contains:\n");
//      for (omi = objectMap->begin(); omi != objectMap->end(); ++omi)
//         MessageInterface::ShowMessage("    <%p>  %s of type %s\n", omi->second,
//               (omi->first).c_str(), ((omi->second)->GetTypeName()).c_str());
//      MessageInterface::ShowMessage("   and at the start, GOS contains:\n");
//      for (omi = globalObjectMap->begin(); omi != globalObjectMap->end(); ++omi)
//         MessageInterface::ShowMessage("    %s of type %s\n",
//               (omi->first).c_str(), ((omi->second)->GetTypeName()).c_str());
//      MessageInterface::ShowMessage("Create::Execute() executing for objects (objectNames):\n");
//      for (unsigned int ii = 0; ii < objectNames.size(); ii++)
//         MessageInterface::ShowMessage(" ........ %s\n", (objectNames.at(ii)).c_str());
//      MessageInterface::ShowMessage("Create::Execute() executing for objects (arrayNames):\n");
//      for (unsigned int ii = 0; ii < arrayNames.size(); ii++)
//         MessageInterface::ShowMessage(" ........ %s\n", (arrayNames.at(ii)).c_str());
//   #endif
//
//   bool isGlobalObj = false;
//   StringArray useNames = objectNames;
//   // If the type is Array, use the names without the indices
//   if (objType == "Array")  useNames = arrayNames;
//
//   //put the objects onto the LOS or GOS, as appropriate, if not already there
//   for (Integer ii = 0; ii < (Integer) creations.size(); ii++)
//   {
//      InsertIntoObjectStore(creations.at(ii), useNames.at(ii));
//   }
//   #ifdef DEBUG_CREATE
//      std::map<std::string, GmatBase *>::iterator omIter;
//      MessageInterface::ShowMessage("Exiting Create::Execute()\n");
//      MessageInterface::ShowMessage("   and LOS contains:\n");
//      for (omIter = objectMap->begin(); omIter != objectMap->end(); ++omIter)
//         MessageInterface::ShowMessage("    %s of type %s\n",
//               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
//      MessageInterface::ShowMessage("   and GOS contains:\n");
//      for (omIter = globalObjectMap->begin(); omIter != globalObjectMap->end(); ++omIter)
//         MessageInterface::ShowMessage("    %s of type %s\n",
//               (omIter->first).c_str(), ((omIter->second)->GetTypeName()).c_str());
//   #endif
//   creations.clear();
   return true;
}

void Create::SetArrayInfo()
{
   // Extract the array names, number of rows, and number of columns from the
   // objectNames array
   arrayNames.clear();
   rows.clear();
   columns.clear();
   std::string itsName;
   Integer r = -99;
   Integer c = -99;
   unsigned int sz = objectNames.size();
   for (unsigned int ii = 0; ii < sz; ii++)
   {
      GmatStringUtil::GetArrayIndex(objectNames.at(ii), r, c, itsName, "[]");
      #ifdef DEBUG_CREATE
         MessageInterface::ShowMessage(
               "Create::SetArrayInfo() setting array name, row, column to:  %s  %d  %d\n",
               itsName.c_str(), r, c);
      #endif
      arrayNames.push_back(itsName);
      rows.push_back(r);
      columns.push_back(c);
   }
}

//------------------------------------------------------------------------------
// bool InsertIntoLOS(GmatBase *obj, const std::string &withName)
//------------------------------------------------------------------------------
bool Create::InsertIntoLOS(GmatBase *obj, const std::string &withName)
{
   #ifdef DEBUG_CREATE_INIT
      MessageInterface::ShowMessage("   InsertIntoLOS: entered with obj = <%p> "
         "and name = %s\n", obj, withName.c_str());
   #endif
   GmatBase *mapObj;
   std::string ex;
   // if it is already in the LOS, make sure the types match
   if (objectMap->find(withName) != objectMap->end())
   {
      mapObj = (*objectMap)[withName];
      
      // if mapObj is NULL, throw exception (loj: 2008.09.23)
      if (mapObj == NULL)
      {
         ex = "Create::InsertIntoLOS() '" + withName +
            "' has NULL pointer in Local Object Store";
         throw CommandException(ex);
      }
      else
      {
         if (!mapObj->IsOfType(objType))
         {
            ex = "Object of name """ + withName;
            ex += """, but of a different type, already exists in Local Object Store\n";
            throw CommandException(ex);
         }
         if (objType == "Array")
         {
            Integer r1, r2, c1, c2;
            ((Array*) mapObj)->GetSize(r1, c1);
            ((Array*) obj)->GetSize(r2, c2);
            if ((r1 != r2) || (c1 != c2))
            {
               ex = "Array of name """ + withName;
               ex += """, but with different dimensions already exists in Local Object Store\n";
               throw CommandException(ex);
            }
         }
      }
      #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage("   InsertIntoLOS: object %s was already "
             "in object store ...\n", withName.c_str());
         MessageInterface::ShowMessage("   InsertIntoLOS: pointer for obj = <%p> "
             "and pointer for mapObj = <%p>\n", obj, mapObj);
      #endif
      // it is already in there, so we do not need to put this one in; clean it up
      if (mapObj != obj)  
      {
         #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage("   InsertIntoLOS: returning false, "
            "object '%s' is not the same\n", withName.c_str());
         #endif
         return false;
      }
   }
   else
   {
      #ifdef DEBUG_CREATE_INIT
      MessageInterface::ShowMessage
         ("   InsertIntoLOS: inserting '%s' into objectMap\n", withName.c_str());
      #endif
      // put it into the LOS
      objectMap->insert(std::make_pair(withName, obj));
   }
   #ifdef DEBUG_CREATE_INIT
   MessageInterface::ShowMessage("   InsertIntoLOS: returning true\n");
   #endif
   return true;
}

//------------------------------------------------------------------------------
// bool InsertIntoObjectStore(GmatBase *obj, const std::string &withName)
//------------------------------------------------------------------------------
bool Create::InsertIntoObjectStore(GmatBase *obj, const std::string &withName)
{
   // check to see if the object is a(n automatic) global
   bool isGlobalObj = obj->GetIsGlobal();
   if (!isGlobalObj)
   {
      #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage("   InsertIntoObjectStore: object "
            "'%s' is NOT global\n", withName.c_str());
      #endif
      // insert the object into the LOS; if not successful, delete this object
      // to avoid a memory leak
      if (!(InsertIntoLOS(obj, withName)))
      {
         #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage
            ("   InsertIntoObjectStore: '%s' was not successfully inserted to LOS, so deleting...\n",
             withName.c_str());
         #endif
         delete obj;
      }
      else
      {
         #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage("   InsertIntoObjectStore: '%s' was put into the LOS\n", 
               (withName).c_str());
         #endif
      }
   }
   else
   {
      #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage("   InsertIntoObjectStore, object "
            "'%s' IS global\n", withName.c_str());
      #endif
      // insert the object into the GOS; if not successful, delete this object
      // to avoid a memory leak
      if (!(InsertIntoGOS(obj, withName)))
      {
         #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage
            ("   InsertIntoObjectStore: '%s' was not successfully inserted to GOS, so deleting...\n",
             withName.c_str());
         #endif
         delete obj;
      }
      else
      {
         #ifdef DEBUG_CREATE_INIT
         MessageInterface::ShowMessage("   InsertIntoObjectStore: '%s' was put into the GOS\n", 
               (withName).c_str());
         #endif
      }
   }
   
   #ifdef DEBUG_CREATE_INIT
   MessageInterface::ShowMessage("   InsertIntoObjectStore: returning true\n");
   #endif
   return true;
}

