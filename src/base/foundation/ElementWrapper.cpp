//$Id$
//------------------------------------------------------------------------------
//                                  ElementWrapper
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Wendy C. Shoan
// Created: 2007.01.17
//
/**
 * Implementation of the ElementWrapper class.  This is the base class for wrappers 
 * for various element types.
 *
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "GmatBaseException.hpp"
#include "ElementWrapper.hpp"
#include "StringUtil.hpp"          // for ReplaceName()
#include "TextParser.hpp"          // for SeparateBrackets()
#include "SolarSystem.hpp"         // for GetBody()
#include "MessageInterface.hpp"

//#define DEBUG_RENAME
//#define DEBUG_EW_SET_VALUE
//#define DEBUG_FIND_OBJECT

//---------------------------------
// static data
//---------------------------------
const Real ElementWrapper::UNDEFINED_REAL = -999.99;

//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

//---------------------------------------------------------------------------
//  ElementWrapper();
//---------------------------------------------------------------------------
/**
 * Constructs base ElementWrapper structures used in derived classes
 * (default constructor).
 *
 */
//---------------------------------------------------------------------------
ElementWrapper::ElementWrapper() :
   description  (""),
   wrapperType  (Gmat::NUMBER_WT)
{
}

//---------------------------------------------------------------------------
//  ElementWrapper(const ElementWrapper &er);
//---------------------------------------------------------------------------
/**
 * Constructs base ElementWrapper structures used in derived classes, by copying 
 * the input instance (copy constructor).
 *
 * @param <er>  ElementWrapper instance to copy to create "this" instance.
 */
//---------------------------------------------------------------------------
ElementWrapper::ElementWrapper(const ElementWrapper &er) :
   description    (er.description),
   refObjectNames (er.refObjectNames),
   wrapperType    (er.wrapperType)
{
}

//---------------------------------------------------------------------------
//  ElementWrapper& operator=(const ElementWrapper &er)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ElementWrapper structures.
 *
 * @param <er> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const ElementWrapper& ElementWrapper::operator=(const ElementWrapper &er)
{
   if (&er == this)
      return *this;

   description    = er.description;
   refObjectNames = er.refObjectNames;
   wrapperType    = er.wrapperType;

   return *this;
}

//---------------------------------------------------------------------------
//  ~ElementWrapper()
//---------------------------------------------------------------------------
/**
 * Destructor.
 */
//---------------------------------------------------------------------------
ElementWrapper::~ElementWrapper()
{
   refObjectNames.clear();
}

//------------------------------------------------------------------------------
// std::string ToString()
//------------------------------------------------------------------------------
/**
 * @return ElementWrapper value converted to std::string.
 *
 * @exception <GmatBaseException> thrown if this method is called.
 */
//------------------------------------------------------------------------------
std::string ElementWrapper::ToString()
{
   GmatBaseException be;
   be.SetDetails
      ("ElementWrapper::ToString() has not been implemented for wrapper "
       "type %d, description of \"%s\"", wrapperType, description.c_str());
   throw be;
}

//------------------------------------------------------------------------------
// virtual ElementWrapper* Clone() const
//------------------------------------------------------------------------------
/**
 * Method used to create a copy of the object
 */
//------------------------------------------------------------------------------
ElementWrapper* ElementWrapper::Clone() const
{
   GmatBaseException be;
   be.SetDetails
      ("ElementWrapper::Clone() has not been implemented for wrapper "
       "type %d, description of \"%s\"", wrapperType, description.c_str());
   throw be;
}

//------------------------------------------------------------------------------
//  void SetDescription(const std::string &str)
//------------------------------------------------------------------------------
/**
 * Sets the description for the ElementWrapper object.
 */
//------------------------------------------------------------------------------
void ElementWrapper::SetDescription(const std::string &str)
{
   description = str;
   SetupWrapper();
}


//------------------------------------------------------------------------------
//  std::string  GetDescription() const
//------------------------------------------------------------------------------
/**
 * This method returns the description for the ElementWrapper object.
 *
 * @return description string for the object.
 *
 */
//------------------------------------------------------------------------------
std::string ElementWrapper::GetDescription() const
{
   return description;
}

//------------------------------------------------------------------------------
//  Gmat::WrapperDataType  GetWrapperType() const
//------------------------------------------------------------------------------
/**
 * This method returns the wrapper type for the ElementWrapper object.
 *
 * @return wrapper type for the object.
 *
 */
//------------------------------------------------------------------------------
Gmat::WrapperDataType ElementWrapper::GetWrapperType() const
{
   return wrapperType;
}

//------------------------------------------------------------------------------
// virtual void ClearRefObjectNames()
//------------------------------------------------------------------------------
void ElementWrapper::ClearRefObjectNames()
{
   refObjectNames.clear();
}


//------------------------------------------------------------------------------
//  const StringArray&  GetRefObjectNames() 
//------------------------------------------------------------------------------
/**
 * This method returns the list of reference object names for the ElementWrapper 
 * object.
 *
 * @return list of reference object names for the object.
 *
 */
//------------------------------------------------------------------------------
const StringArray& ElementWrapper::GetRefObjectNames()
{
   return refObjectNames;
}


//------------------------------------------------------------------------------
// virtual bool SetRefObjectName(const std::string &name, Integer index)
//------------------------------------------------------------------------------
/**
 * This method sets a reference object name for the ElementWrapper 
 * object.
 * 
 * @param <name> name of the ref object to set
 * @param <index> index of ref object name to set (0)
 *
 * @return true for success; false for failure.
 *
 */
//------------------------------------------------------------------------------
bool ElementWrapper::SetRefObjectName(const std::string &name, Integer index)
{
   return false;
}


//------------------------------------------------------------------------------
//  GmatBase* GetRefObject(const std::string &name = "")
//------------------------------------------------------------------------------
/**
 * This method retrives a reference object for the wrapper name
 * 
 * @param <name> name of the wrapper
 *
 * @return reference for success; NULL if name not found
 *
 */
//------------------------------------------------------------------------------
GmatBase* ElementWrapper::GetRefObject(const std::string &name)
{
   return NULL;
}


//------------------------------------------------------------------------------
//  bool SetRefObject(GmatBase *obj)
//------------------------------------------------------------------------------
/**
 * This method sets a reference object for the ElementWrapper 
 * object.
 * 
 * @param <obj> pointer to the object.
 *
 * @return true for success; false for failure.
 *
 */
//------------------------------------------------------------------------------
bool ElementWrapper::SetRefObject(GmatBase *obj)
{
   return false;
}


//---------------------------------------------------------------------------
//  bool RenameObject(const std::string &oldName, const std::string &newName)
//---------------------------------------------------------------------------
/**
 * Method to rename a reference object for the wrapper.
 *
 * @return true if successful; false otherwise.
 */
//---------------------------------------------------------------------------
bool ElementWrapper::RenameObject(const std::string &oldName, 
                                  const std::string &newName)
{
   #ifdef DEBUG_RENAME
      MessageInterface::ShowMessage(
      "Now entering EW::RenameObject with oldName = %s\n and newName = %s\n",
      oldName.c_str(), newName.c_str());
   #endif
   // replace the old name with the new name in the list of ref objects
   Integer sz = refObjectNames.size();
   for (Integer j=0; j < sz; j++)
   {
      if (refObjectNames[j].find(oldName) != oldName.npos)
      {
         #ifdef DEBUG_RENAME
         MessageInterface::ShowMessage
            ("   old refObjectNames[%d]=<%s>\n", j, refObjectNames[j].c_str());
         #endif
         
         refObjectNames[j] =
            GmatStringUtil::ReplaceName(refObjectNames[j], oldName, newName);
         
         #ifdef DEBUG_RENAME
         MessageInterface::ShowMessage
            ("   new refObjectNames[%d]=<%s>\n", j, refObjectNames[j].c_str());
         #endif
      }
   }
   return true;
}

//---------------------------------------------------------------------------
// const Rmatrix& EvaluateArray() const
//---------------------------------------------------------------------------
const Rmatrix& ElementWrapper::EvaluateArray() const
{
   throw GmatBaseException(
      "In ElementWrapper, EvaluateArray() method not valid for wrapper of non-Array type.\n");
}

//---------------------------------------------------------------------------
// bool SetArray(const Rmatrix &toValue)
//---------------------------------------------------------------------------
bool ElementWrapper::SetArray(const Rmatrix &toValue)
{
   throw GmatBaseException(
      "In ElementWrapper, SetArray() method not valid for wrapper of non-Array type.\n");
}

//---------------------------------------------------------------------------
// std::string EvaluateString() const
//---------------------------------------------------------------------------
std::string ElementWrapper::EvaluateString() const
{
   throw GmatBaseException(
      "In ElementWrapper, EvaluateString() method not valid for wrapper of non-String type.\n");
}

//---------------------------------------------------------------------------
// bool SetString(const std::string &toValue)
//---------------------------------------------------------------------------
bool ElementWrapper::SetString(const std::string &toValue)
{
   throw GmatBaseException(
      "In ElementWrapper, SetString() method not valid for wrapper of non-String type.\n");
}

//---------------------------------------------------------------------------
// std::string EvaluateOnOff() const
//---------------------------------------------------------------------------
std::string ElementWrapper::EvaluateOnOff() const
{
   throw GmatBaseException(
      "In ElementWrapper, EvaluateOnOff() method not valid for wrapper of non-OnOff type.\n");
}

//---------------------------------------------------------------------------
// bool SetOnOff(const std::string &toValue)
//---------------------------------------------------------------------------
bool ElementWrapper::SetOnOff(const std::string &toValue)
{
   throw GmatBaseException(
      "In ElementWrapper, SetOnOff() method not valid for wrapper of non-OnOff type.\n");
}

//---------------------------------------------------------------------------
// bool EvaluateBoolean() const
//---------------------------------------------------------------------------
bool ElementWrapper::EvaluateBoolean() const
{
   throw GmatBaseException(
      "In ElementWrapper, EvaluateBoolean() method not valid for wrapper of non-Boolean type.\n");
}

//---------------------------------------------------------------------------
// bool SetBoolean(const bool toValue)
//---------------------------------------------------------------------------
bool ElementWrapper::SetBoolean(const bool toValue)
{
   throw GmatBaseException(
      "In ElementWrapper, SetBoolean() method not valid for wrapper of non-Boolean type.\n");
}

//---------------------------------------------------------------------------
// Integer EvaluateInteger() const
//---------------------------------------------------------------------------
Integer ElementWrapper::EvaluateInteger() const
{
   throw GmatBaseException(
      "In ElementWrapper, EvaluateInteger() method not valid for wrapper of non-Integer type.\n");
}

//---------------------------------------------------------------------------
// bool SetInteger(const Integer toValue)
//---------------------------------------------------------------------------
bool ElementWrapper::SetInteger(const Integer toValue)
{
   throw GmatBaseException(
      "In ElementWrapper, SetInteger() method not valid for wrapper of non-Integer type.\n");
}

//---------------------------------------------------------------------------
// GmatBase* EvaluateObject() const
//---------------------------------------------------------------------------
GmatBase* ElementWrapper::EvaluateObject() const
{
   throw GmatBaseException(
      "In ElementWrapper, EvaluateObject() method not valid for wrapper of non-Object type.\n");
}


//---------------------------------------------------------------------------
// bool SetObject(GmatBase *obj)
//---------------------------------------------------------------------------
bool ElementWrapper::SetObject(GmatBase *obj)
{
   throw GmatBaseException(
      "In ElementWrapper, SetObject() method not valid for wrapper of non-Object type.\n");
}


//---------------------------------------------------------------------------
// static bool SetValue(ElementWrapper *lhsWrapper, ElementWrapper *rhsWrapper,
//                      SolarSystem *solarSys, ObjectMap *objMap,
//                      ObjectMap *globalObjMap, bool setRefObj = true)
//---------------------------------------------------------------------------
/*
 * static function to set value from rhs wrapper to lhs wrapper.
 */
//---------------------------------------------------------------------------
bool ElementWrapper::SetValue(ElementWrapper *lhsWrapper, ElementWrapper *rhsWrapper,
                              SolarSystem *solarSys, ObjectMap *objMap,
                              ObjectMap *globalObjMap, bool setRefObj)
{
   #ifdef DEBUG_EW_SET_VALUE
   MessageInterface::ShowMessage
      ("ElemementWrapper::SetValue() entered, lhsWrapper=<%p>, rhsWrapper=<%p>\n   "
       "solarSys=<%p> objMap=<%p>, globalObjMap=<%p>, setRefObj=%d\n", lhsWrapper,
       rhsWrapper, solarSys, objMap, globalObjMap, setRefObj);
   #endif
   
   if (lhsWrapper == NULL || rhsWrapper == NULL)
      return false;
   
   std::string lhs = lhsWrapper->GetDescription();
   std::string rhs = rhsWrapper->GetDescription();
   
   #ifdef DEBUG_EW_SET_VALUE
   MessageInterface::ShowMessage("   lhs=\"%s\"\n   rhs=\"%s\"\n", lhs.c_str(), rhs.c_str());
   #endif
   
   Real rval = -99999.999;
   Integer ival = -99999;
   bool bval = false;
   std::string sval = "UnknownValue";
   Rmatrix rmat;
   GmatBase *rhsObj = NULL;
   
   Gmat::ParameterType lhsDataType = lhsWrapper->GetDataType();
   Gmat::ParameterType rhsDataType = Gmat::UNKNOWN_PARAMETER_TYPE;
   std::string lhsTypeStr = "UnknownDataType";
   if (lhsDataType != Gmat::UNKNOWN_PARAMETER_TYPE)
      lhsTypeStr = GmatBase::PARAM_TYPE_STRING[lhsDataType];
   std::string rhsTypeStr = "UnknownDataType";
   Gmat::WrapperDataType lhsWrapperType = lhsWrapper->GetWrapperType();
   Gmat::WrapperDataType rhsWrapperType = Gmat::UNKNOWN_WRAPPER_TYPE;
   
   #ifdef DEBUG_EW_SET_VALUE
   MessageInterface::ShowMessage
      ("   lhsWrapperType=%2d, lhsDataType=%s\n", lhsWrapperType, lhsTypeStr.c_str());
   #endif
   
   try
   {
      rhsDataType = rhsWrapper->GetDataType();
      rhsTypeStr = GmatBase::PARAM_TYPE_STRING[rhsDataType];
      rhsWrapperType = rhsWrapper->GetWrapperType();
      
      #ifdef DEBUG_EW_SET_VALUE
      MessageInterface::ShowMessage
         ("   rhsWrapperType=%2d, rhsDataType=%s\n", rhsWrapperType, rhsTypeStr.c_str());
      #endif
      
      // If lhs is a String, it must be String Object and STRING_OBJECT_WT,
      // so check it first for error condition. ex) UnknownObj1 = str1
      if (lhsDataType == Gmat::STRING_TYPE && lhsWrapperType == Gmat::STRING_WT)
      {
         GmatBaseException ex;
         ex.SetDetails("ElementWrapper::SetValue() Cannot set \"%s\" to unknown "
                       "object \"%s\"", rhs.c_str(), lhs.c_str());
         throw ex;
      }
      
      switch (rhsDataType)
      {
      case Gmat::BOOLEAN_TYPE:
         bval = rhsWrapper->EvaluateBoolean();
         break;
      case Gmat::INTEGER_TYPE:
         ival = rhsWrapper->EvaluateInteger();
         break;
      case Gmat::REAL_TYPE:
         rval = rhsWrapper->EvaluateReal();
         #ifdef DEBUG_EW_SET_VALUE
         MessageInterface::ShowMessage("   REAL_TYPE rhs rval=%f\n", rval);
         #endif
         break;
      case Gmat::RMATRIX_TYPE:
         rmat = rhsWrapper->EvaluateArray();
         break;
      case Gmat::STRING_TYPE:
      case Gmat::ENUMERATION_TYPE:
         sval = rhsWrapper->EvaluateString();
         sval = GmatStringUtil::RemoveEnclosingString(sval, "'");
         break;
      case Gmat::ON_OFF_TYPE:
         sval = rhsWrapper->EvaluateOnOff();
         break;
      case Gmat::OBJECT_TYPE:
         rhsObj = rhsWrapper->EvaluateObject();
         break;
      default:
         throw GmatBaseException("Unknown RHS data type");
      }
      
      #ifdef DEBUG_EW_SET_VALUE
      MessageInterface::ShowMessage
         ("   ==> Now assign \"%s\" to \"%s\", rhsObj=<%p>, sval='%s'\n",
          rhs.c_str(), lhs.c_str(), rhsObj, sval.c_str());
      #endif
      
      // Now assign to LHS
      switch (lhsDataType)
      {
      case Gmat::BOOLEAN_TYPE:
         lhsWrapper->SetBoolean(bval);
         break;
      case Gmat::INTEGER_TYPE:
         {
            // Since it always creates NumberWrapper for numbers,
            // check both Integer and Real types
            if (rhsDataType == Gmat::INTEGER_TYPE)
            {
               lhsWrapper->SetInteger(ival);
            }
            else if (rhsDataType == Gmat::REAL_TYPE)
            {
               Integer itempval;
               std::string desc = rhs;
               if (GmatStringUtil::ToInteger(desc, itempval))
                  lhsWrapper->SetInteger(itempval);
               else
                  throw GmatBaseException
                     ("ElementWrapper::SetValue() Cannot set Real number to Integer");
            }
            break;
         }
      case Gmat::UNSIGNED_INTARRAY_TYPE:
         {
            if (rhsDataType == Gmat::STRING_TYPE)
               lhsWrapper->SetString(rhs);
            else
               throw GmatBaseException
                  ("ElementWrapper::SetValue() Cannot set Non-Integer value to "
                   "UnsignedInt Array");
            break;
         }
      case Gmat::RVECTOR_TYPE:
         {
            if (rhsDataType == Gmat::STRING_TYPE)
               lhsWrapper->SetString(rhs);
            else
               throw GmatBaseException
                  ("ElementWrapper::SetValue() Cannot set Non-Rvector value to "
                   "Rvector");
            break;
         }
      case Gmat::REAL_TYPE:
         {
            #ifdef DEBUG_EW_SET_VALUE
            MessageInterface::ShowMessage("   setting rhs rval=%f to lhs\n", rval);
            #endif
            bool valueSet = false;
            if (rval != -99999.999)
            {
               lhsWrapper->SetReal(rval);
               valueSet = true;
            }
            else if (rhsDataType == Gmat::RMATRIX_TYPE)
            {
               if (rmat.GetNumRows() == 1 && rmat.GetNumColumns() == 1)
               {
                  Real val = rmat.GetElement(0, 0);
                  lhsWrapper->SetReal(val);
                  valueSet = true;
               }
            }
            
            if (!valueSet)
            {
               throw GmatBaseException
                  ("ElementWrapper::SetValue() Cannot set Non-Real value to Real");
            }
            break;
         }
      case Gmat::RMATRIX_TYPE:
         lhsWrapper->SetArray(rmat);
         break;
      case Gmat::STRING_TYPE:
      case Gmat::ENUMERATION_TYPE:
         // Object to String is needed for Remove for Formation
         if (rhsObj != NULL)
         {
            lhsWrapper->SetString(rhsObj->GetName());
         }
         else if ((rhsDataType == Gmat::STRING_TYPE ||
                   rhsDataType == Gmat::ENUMERATION_TYPE ||
                   rhsDataType == Gmat::ON_OFF_TYPE))
         {
            lhsWrapper->SetString(sval);
         }
         // We don't want to allow Variable or Array element to STRING assignment
         else if (rhsDataType == Gmat::REAL_TYPE &&
                  rhsWrapperType != Gmat::VARIABLE_WT &&
                  rhsWrapperType != Gmat::ARRAY_ELEMENT_WT)
         {
            lhsWrapper->SetString(rhs);
         }
         else
         {
            // Handle setting real value to string here
            // This fixes Bug 1340 (LOJ: 2009.10.19)
            if (rhsDataType == Gmat::REAL_TYPE)
            {
               sval = GmatStringUtil::ToString(rval, 16);
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage
                  ("   %f converted to string '%s'\n", rval, sval.c_str());
               #endif
               lhsWrapper->SetString(sval);
               break;
            }
            else if (rhsDataType == Gmat::INTEGER_TYPE)
            {
               sval = GmatStringUtil::ToString(ival);
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage
                  ("   %d converted to string '%s'\n", ival, sval.c_str());
               #endif
               lhsWrapper->SetString(sval);
               break;
            }
            else if (rhsDataType == Gmat::BOOLEAN_TYPE)
            {
               sval = GmatStringUtil::ToString(bval);
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage
                  ("   %s converted to string '%s'\n", bval ? "true" : "false",
                   sval.c_str());
               #endif
               lhsWrapper->SetString(sval);
               break;
            }
            else
            {
               GmatBaseException ex;
               if (rhsObj != NULL)
                  ex.SetDetails("ElementWrapper::SetValue() Cannot set object of "
                                "type \"%s\" to an undefined object \"%s\"",
                                rhsObj->GetTypeName().c_str(), lhs.c_str());
               else if (lhsWrapperType == Gmat::STRING_OBJECT_WT &&
                        rhsWrapperType == Gmat::VARIABLE_WT)
                  ex.SetDetails("ElementWrapper::SetValue() Cannot set objet of "
                                "type \"Variable\" to object of type \"String\"");
               else
                  ex.SetDetails("ElementWrapper::SetValue() Cannot set \"%s\" to "
                                "an undefined object \"%s\"", rhs.c_str(), lhs.c_str());
               throw ex;
            }
         }
         break;
      case Gmat::ON_OFF_TYPE:
         lhsWrapper->SetOnOff(sval);
         break;
      case Gmat::OBJECT_TYPE:
         if (rhsObj == NULL)
         {
            // Handle special case for "DefaultFM.Drag = None;"
            if (rhsDataType == Gmat::STRING_TYPE)
            {
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage("   calling lhsWrapper->SetString(rhs)\n");
               #endif
               
               lhsWrapper->SetString(rhs);
            }
            // Handle case like "XYPlot1.IndVar = sat.A1ModJulian;"
            else if (rhsWrapperType == Gmat::PARAMETER_WT)
            {
               lhsWrapper->SetObject(rhsWrapper->GetRefObject());
            }
            else
               throw GmatBaseException
                  ("ElementWrapper::SetValue() Cannot set Non-Object type to object");
         }
         else
         {            
            // check if ref object can be set to lhs
            if (setRefObj)
            {
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage("   calling lhsWrapper->SetObject(rhsObj)\n");
               #endif
               lhsWrapper->SetObject(rhsObj);
            }
            else
            {
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage("   calling lhsWrapper->SetString(rhsObjName)\n");
               #endif
               lhsWrapper->SetString(rhsObj->GetName());
            }
         }
         break;
      case Gmat::STRINGARRAY_TYPE:                  
         if (rhsObj != NULL)
         {
            #ifdef DEBUG_EW_SET_VALUE
            MessageInterface::ShowMessage("   calling lhsWrapper->SetString(rhsObj->GetName())\n");
            #endif
            lhsWrapper->SetString(rhsObj->GetName());
         }
         else
         {
            #ifdef DEBUG_EW_SET_VALUE
            MessageInterface::ShowMessage("   calling lhsWrapper->SetString(rhs or sval)\n");
            #endif
            if (sval == "UnknownValue")
               lhsWrapper->SetString(rhs);
            else
               lhsWrapper->SetString(sval);
            
            // Commented out to handle SolarSystem.Ephemeris = {SLP} (loj: 2008.07.16)            
            //GmatBaseException ex;
            //ex.SetDetails("ElementWrapper::SetValue() Cannot set \"%s\" to \"%s\"",
            //              rhs.c_str(), lhs.c_str());
            //throw ex;
         }
         break;
      case Gmat::OBJECTARRAY_TYPE:
         // Object to String is needed for Add for Subscribers/Formation
         if (rhsObj != NULL)
            lhsWrapper->SetObject(rhsObj);
         else
         {
            bool errorCond = true;
            
            // Handle case like "GMAT XYPlot1.Add = {sat.X sat.Y};"
            // Set individually if RHS has more than one object
            StringArray rhsValues;
            TextParser tp;
            rhsValues = tp.SeparateBrackets(rhs, "{}", " ,", false);
            
            #ifdef DEBUG_EW_SET_VALUE
            MessageInterface::ShowMessage("   rhs {} has %d items\n", rhsValues.size());
            #endif
            
            std::string firstTypeStr, currTypeStr;
            for (UnsignedInt i=0; i<rhsValues.size(); i++)
            {
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage("   rhsValues[%d]=<%s>\n", i, rhsValues[i].c_str());
               #endif
               
               // Remove enclosing single quotes (LOJ: 2009.10.09)
               rhsValues[i] = GmatStringUtil::RemoveEnclosingString(rhsValues[i], "'");
               
               GmatBase *obj = FindObject(rhsValues[i], solarSys, objMap, globalObjMap);
               
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage
                  ("   obj=<%p><%s>'%s'\n", obj, obj->GetTypeName().c_str(), obj->GetName().c_str());
               #endif
               
               if (obj == NULL)
               {
                  errorCond = true;
                  break;
               }
               
               #ifdef DEBUG_EW_SET_VALUE
               MessageInterface::ShowMessage
                  ("   calling lhsWrapper->SetObject(obj), wrapperType=%d\n",
                   lhsWrapper->GetWrapperType());
               #endif
               
               lhsWrapper->SetObject(obj);
               errorCond = false;
            }
            
            // To handle Earth2Body.PointMasses = {}, check for empty items
            if (errorCond && rhsValues.size() > 0)
            {
               GmatBaseException ex;
               ex.SetDetails
                  ("ElementWrapper::SetValue() Cannot set \"%s\" to \"%s\"",
                   rhs.c_str(), lhs.c_str());
               throw ex;
            }
         }
         break;
      default:
         throw GmatBaseException("Unknown LHS type");
      }
      
      #ifdef DEBUG_EW_SET_VALUE
      MessageInterface::ShowMessage("ElemementWrapper::SetValue() (1)returning true\n");
      #endif
      return true;
   }
   catch (BaseException &e)
   {
      // anyting to add here?
      throw;
   }
   
   #ifdef DEBUG_EW_SET_VALUE
   MessageInterface::ShowMessage("ElemementWrapper::SetValue() (2)returning true\n");
   #endif
   
   return true;
}


//---------------------------------------------------------------------------
// static GmatBase* FindObject(const std::string &name, SolarSystem *solarSys,
//                             ObjectMap *objMap, ObjectMap *globalObjMap)
//---------------------------------------------------------------------------
GmatBase* ElementWrapper::FindObject(const std::string &name, SolarSystem *solarSys,
                                     ObjectMap *objMap, ObjectMap *globalObjMap)
{
   std::string newName = name;
   
   // Ignore array indexing of Array
   std::string::size_type index = name.find('(');
   if (index != name.npos)
      newName = name.substr(0, index);
   
   #ifdef DEBUG_FIND_OBJECT
   MessageInterface::ShowMessage
      ("ElementWrapper::FindObject() entered, name='%s', newName='%s'\n", name.c_str(),
       newName.c_str());
   #endif
   
   #ifdef DEBUG_FIND_OBJECT
   ShowObjectMaps(objMap, globalObjMap);
   #endif
   
   // Check for the object in the Local Object Store (LOS) first
   if (objMap && objMap->find(newName) != objMap->end())
      return (*objMap)[newName];
   
   // If not found in the LOS, check the Global Object Store (GOS)
   if (globalObjMap && globalObjMap->find(newName) != globalObjMap->end())
      return (*globalObjMap)[newName];
   
   // Let's try SolarSystem
   if (solarSys && solarSys->GetBody(newName))
      return (GmatBase*)(solarSys->GetBody(newName));
   
   return NULL;
}


//------------------------------------------------------------------------------
// static void ShowObjectMaps(ObjectMap *objMap, ObjectMap *globalObjMap)
//------------------------------------------------------------------------------
void ElementWrapper::ShowObjectMaps(ObjectMap *objMap, ObjectMap *globalObjMap)
{
   MessageInterface::ShowMessage
      ("ElementWrapper::ShowObjectMaps() objMap=<%p>, globalObjMap=<%p>\n",
       objMap, globalObjMap);
   
   if (objMap)
   {
      MessageInterface::ShowMessage("Here is the local object map:\n");
      for (std::map<std::string, GmatBase *>::iterator i = objMap->begin();
           i != objMap->end(); ++i)
         MessageInterface::ShowMessage
            ("   %30s  <%p><%s>\n", i->first.c_str(), i->second,
             i->second == NULL ? "NULL" : (i->second)->GetTypeName().c_str());
   }
   if (globalObjMap)
   {
      MessageInterface::ShowMessage("Here is the global object map:\n");
      for (std::map<std::string, GmatBase *>::iterator i = globalObjMap->begin();
           i != globalObjMap->end(); ++i)
         MessageInterface::ShowMessage
            ("   %30s  <%p><%s>\n", i->first.c_str(), i->second,
             i->second == NULL ? "NULL" : (i->second)->GetTypeName().c_str());
   }
}

