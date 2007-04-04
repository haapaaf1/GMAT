//$Header$
//------------------------------------------------------------------------------
//                                  StringObjectWrapper
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P
//
// Author: Wendy C. Shoan
// Created: 2007.04.04
//
/**
 * Implementation of the StringObjectWrapper class.
 *
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "StringObjectWrapper.hpp"
#include "ParameterException.hpp"
#include "RealUtilities.hpp"
#include "StringUtil.hpp"
#include "UtilityException.hpp"

#include "MessageInterface.hpp"

//#define DEBUG_STRING_WRAPPER

//---------------------------------
// static data
//---------------------------------
// none at this time

//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

//---------------------------------------------------------------------------
//  StringObjectWrapper();
//---------------------------------------------------------------------------
/**
 * Constructs StringObjectWrapper structures
 * (default constructor).
 *
 */
//---------------------------------------------------------------------------
StringObjectWrapper::StringObjectWrapper() :
   ElementWrapper(),
   stringVar     (NULL),
   stringName    ("")
{
   wrapperType = Gmat::STRING_OBJECT;
}

//---------------------------------------------------------------------------
//  StringObjectWrapper(const StringObjectWrapper &sow);
//---------------------------------------------------------------------------
/**
 * Constructs base StringObjectWrapper structures used in derived classes, 
 * by copying the input instance (copy constructor).
 *
 * @param <sow>  StringObjectWrapper instance to copy to create "this" 
 * instance.
 */
//---------------------------------------------------------------------------
StringObjectWrapper::StringObjectWrapper(const StringObjectWrapper &sow) :
   ElementWrapper(sow),
   stringVar     (NULL),
   stringName    (sow.stringName)
{
}

//---------------------------------------------------------------------------
//  StringObjectWrapper& operator=(const StringObjectWrapper &sow)
//---------------------------------------------------------------------------
/**
 * Assignment operator for StringObjectWrapper structures.
 *
 * @param <sow> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const StringObjectWrapper& StringObjectWrapper::operator=(
                             const StringObjectWrapper &sow)
{
   if (&sow == this)
      return *this;

   ElementWrapper::operator=(sow);
   stringVar     = NULL;  
   stringName    = sow.stringName;

   return *this;
}
//---------------------------------------------------------------------------
//  ~StringObjectWrapper()
//---------------------------------------------------------------------------
/**
 * Destructor.
 */
//---------------------------------------------------------------------------
StringObjectWrapper::~StringObjectWrapper()
{
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
const StringArray& StringObjectWrapper::GetRefObjectNames()
{
   refObjectNames.clear();
   // start with the stringVar name ...
   refObjectNames.push_back(stringName);
   
          
   #ifdef DEBUG_STRING_WRAPPER
      MessageInterface::ShowMessage("StringObjectWrapper:: Returning ref object names:\n");
      for (Integer ii = 0; ii < (Integer) refObjectNames.size(); ii++)
         MessageInterface::ShowMessage("      %s\n", refObjectNames[ii].c_str());
   #endif
   
   return refObjectNames;
}

//---------------------------------------------------------------------------
//  bool SetRefObject(GmatBase *obj)
//---------------------------------------------------------------------------
/**
 * Method to set the reference object (ObjectProperty) pointer on the wrapped 
 * object.
 *
 * @return true if successful; false otherwise.
 */
//---------------------------------------------------------------------------
bool StringObjectWrapper::SetRefObject(GmatBase *obj)
{
   bool isOk   = false;
   if ( (obj->IsOfType("String")) && (obj->GetName() == stringName) )
   {
      stringVar = (StringVar*) obj;
      #ifdef DEBUG_STRING_WRAPPER
         MessageInterface::ShowMessage("StringObjectWrapper:: Setting stringVar object %s\n",
            stringName.c_str());
      #endif
      isOk = true;
   }
   
   if (!isOk) return false;
   #ifdef DEBUG_STRING_WRAPPER
      MessageInterface::ShowMessage(
         "StringObjectWrapper:: Returning true from SetRefObject\n");
   #endif
   
   return true;
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
bool StringObjectWrapper::RenameObject(const std::string &oldName, 
                                       const std::string &newName)
{
   ElementWrapper::RenameObject(oldName, newName);
   // now rebuild the description string from the refObjectNames
   // first, rename things in the row and column wrappers
   // then put it all back together
   stringName  = refObjectNames[0];
   description = stringName;
   return true;
}

//---------------------------------------------------------------------------
//  Real EvaluateReal() const
//---------------------------------------------------------------------------
/**
 * Method to return the Real value of the wrapped object.
 *
 * @return Real value of the wrapped number object.
 * 
 * @note This method is not value for Strings.
 * 
 */
//---------------------------------------------------------------------------
Real StringObjectWrapper::EvaluateReal() const
{
   throw GmatBaseException(
      "EvaluateReal() method not valid for wrapper of String Object type.\n");
}
   
//---------------------------------------------------------------------------
//  bool SetReal(const Real toValue)
//---------------------------------------------------------------------------
/**
 * Method to set the Real value of the wrapped object.
 *
 * @return true if successful; false otherwise.
 * 
 * @note This method is not value for Strings.
 */
//---------------------------------------------------------------------------
bool StringObjectWrapper::SetReal(const Real toValue)
{
   throw GmatBaseException(
      "SetReal() method not valid for wrapper of String Object type.\n");
}

//---------------------------------------------------------------------------
//  std::string EvaluateString() const
//---------------------------------------------------------------------------
/**
 * Method to return the String (Rmatrix) value of the wrapped object.
 *
 * @return Real value of the wrapped String object.
 * 
 */
//---------------------------------------------------------------------------
std::string StringObjectWrapper::EvaluateString() const
{
   #ifdef DEBUG_STRING_WRAPPER
      MessageInterface::ShowMessage(
         "StringObjectWrapper::EvaluateString called on stringVar %s\n", 
         stringName.c_str());
   #endif
   if (stringVar == NULL)
      throw ParameterException(
      "Cannot return value of String - object pointer is NULL\n");
   return stringVar->EvaluateString();
}

//---------------------------------------------------------------------------
//  bool SetString(const Rmatrix &toValue) const
//---------------------------------------------------------------------------
/**
 * Method to return the String (Rmatrix) value of the wrapped object.
 *
 * @return bool true if successful; otherwise, false.
 * 
 */
//---------------------------------------------------------------------------
bool StringObjectWrapper::SetString(const std::string &toValue)
{
   #ifdef DEBUG_STRING_WRAPPER
      MessageInterface::ShowMessage(
         "StringObjectWrapper::SetString called on stringVar %s\n", 
         stringName.c_str());
   #endif
   if (stringVar == NULL)
      throw ParameterException(
      "Cannot set value of String - object pointer is NULL\n");
   try
   {
      stringVar->SetStringParameter("Value",toValue);
   }
   catch (BaseException &be)
   {
      std::string errmsg = "Error setting String value : " + be.GetFullMessage();
      errmsg += "\n";
      throw ParameterException(errmsg);
   }
   return true;
}

//---------------------------------------------------------------------------
//  void SetupWrapper()
//---------------------------------------------------------------------------
/**
 * Method to set up the String Element Wrapper.
 *
 */
//---------------------------------------------------------------------------
void StringObjectWrapper::SetupWrapper()
{
   stringName = description;
   // for now, put the stringVar name in the list of reference objects
   refObjectNames.push_back(stringName);
}
