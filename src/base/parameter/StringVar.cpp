//$Header$
//------------------------------------------------------------------------------
//                                  StringVar
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2005/1/7
//
/**
 * Implements StringVar class which handles std::string value. The string value
 * is stored in Parameter::mExpr.
 */
//------------------------------------------------------------------------------

#include "StringVar.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_STRINGVAR 1

//---------------------------------
// public methods
//---------------------------------

//------------------------------------------------------------------------------
// StringVar(const std::string &name, const std::string &typeStr, 
//         GmatParam::ParameterKey key, GmatBase *obj, const std::string &desc,
//         const std::string &unit, GmatParam::DepObject depObj, Gmat::ObjectType,
//         bool isTimeParam)
//------------------------------------------------------------------------------
/**
 * Constructor.
 *
 * @param <name> parameter name
 * @param <typeStr>  parameter type string
 * @param <key>  parameter key (SYSTEM_PARAM, USER_PARAM, etc)
 * @param <obj>  reference object pointer
 * @param <desc> parameter description
 * @param <unit> parameter unit
 * @param <depObj> object which parameter is dependent on (COORD_SYS, ORIGIN, NONE)
 * @param <ownerType> object type who owns this parameter as property
 * @param <isTimeParam> true if parameter is time related, false otherwise
 */
//------------------------------------------------------------------------------
StringVar::StringVar(const std::string &name, const std::string &typeStr, 
                     GmatParam::ParameterKey key, GmatBase *obj, const std::string &desc,
                     const std::string &unit, GmatParam::DepObject depObj,
                     Gmat::ObjectType ownerType, bool isTimeParam)
   : Parameter(name, typeStr, key, obj, desc, unit, depObj, ownerType, isTimeParam,
               false, true)
{  
   mStringValue = STRING_PARAMETER_UNDEFINED;
   mReturnType = Gmat::STRING_TYPE;
}


//------------------------------------------------------------------------------
// StringVar(const StringVar &copy)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * @param <copy> the object being copied.
 */
//------------------------------------------------------------------------------
StringVar::StringVar(const StringVar &copy)
   : Parameter(copy)
{
}


//------------------------------------------------------------------------------
// StringVar& operator= (const StringVar& right)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 *
 * @param <right> the object being copied.
 *
 * @return reference to this object
 */
//------------------------------------------------------------------------------
StringVar& StringVar::operator= (const StringVar& right)
{
   if (this != &right)
   {
      Parameter::operator=(right);
   }

   return *this;
}


//------------------------------------------------------------------------------
// ~StringVar()
//------------------------------------------------------------------------------
/**
 * Destructor
 */
//------------------------------------------------------------------------------
StringVar::~StringVar()
{
}


//------------------------------------
// methods inherited from Parameter
//------------------------------------

//------------------------------------------------------------------------------
// bool operator==(const StringVar &right) const
//------------------------------------------------------------------------------
/**
 * Equal operator.
 *
 * @return true if input object's type and name are the same as this object.
 */
//------------------------------------------------------------------------------
bool StringVar::operator==(const StringVar &right) const
{
   return Parameter::operator==(right);
}


//------------------------------------------------------------------------------
// bool operator!=(const StringVar &right) const
//------------------------------------------------------------------------------
/**
 * Not equal operator.
 *
 * @return true if input object's type and name are not the same as this object.
 */
//------------------------------------------------------------------------------
bool StringVar::operator!=(const StringVar &right) const
{
   return Parameter::operator!=(right);
}


//------------------------------------------------------------------------------
// std::string ToString()
//------------------------------------------------------------------------------
/**
 * Retrieves string value of parameter.
 *
 * @return string value of parameter.
 */
//------------------------------------------------------------------------------
std::string StringVar::ToString()
{
   return mExpr;
}


//------------------------------------------------------------------------------
// std::string GetString() const
//------------------------------------------------------------------------------
/**
 * Retrieves string value of parameter.
 *
 * @return string value.
 */
//------------------------------------------------------------------------------
std::string StringVar::GetString() const
{
   return mExpr;
}


//------------------------------------------------------------------------------
// std::string EvaluateString()
//------------------------------------------------------------------------------
/**
 * Retrieves string value of parameter.
 *
 * @return string value.
 */
//------------------------------------------------------------------------------
std::string StringVar::EvaluateString()
{
   return mExpr;
}


//------------------------------------
// methods inherited from GmatBase
//------------------------------------

//------------------------------------------------------------------------------
// virtual GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * Method used to create a copy of the object.
 *
 * @return cloned object pointer.
 */
//------------------------------------------------------------------------------
GmatBase* StringVar::Clone() const
{
   return new StringVar(*this);
}

