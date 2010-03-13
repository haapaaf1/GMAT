//$Id: FlowerConstellation.hpp 5835 2008-09-12 00:01:15Z djcinsb $
//------------------------------------------------------------------------------
//                              FlowerConstellation
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2010/03/12
//
/**
 *
 * Implements the Flower Constellation Lattice Theory using the Formation
 * class to contain the satellite objects that are generated.
 *
 */
//------------------------------------------------------------------------------

#include "FlowerConstellation.hpp"

//---------------------------------
// static data
//---------------------------------
const std::string
FlowerConstellation::PARAMETER_TEXT[FCParamCount - FormationParamCount] =
{
   "NumOrbitPlanes",
   "NumSatellites"
};


const Gmat::ParameterType
FlowerConstellation::PARAMETER_TYPE[FCParamCount - FormationParamCount] =
{
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE
};


//------------------------------------------------------------------------------
// FlowerConstellation(Gmat::ObjectType typeId, const std::string &typeStr,
//           const std::string &instName)
//------------------------------------------------------------------------------
/**
 * Default constructor.
 *
 * @param <typeId>   Gmat::ObjectType of the constructed object.
 * @param <typeStr>  String describing the type of object created.
 * @param <instName> Name of the constructed instance.
 */
//------------------------------------------------------------------------------
FlowerConstellation::FlowerConstellation(Gmat::ObjectType typeId, const std::string &typeStr,
                     const std::string &instName) :
   Formation    (typeId, typeStr, instName),
   no      (1),
   ns      (1)
{
   objectTypes.push_back(Gmat::FORMATION);
   objectTypeNames.push_back("Formation");

   parameterCount = FormationParamCount;
}

//------------------------------------------------------------------------------
// FlowerConstellation(const FlowerConstellation& orig)
//------------------------------------------------------------------------------
/**
 * Copy constructor.
 *
 * @param <orig> Formation that is copied onto this one.
 */
//------------------------------------------------------------------------------
FlowerConstellation::FlowerConstellation(const FlowerConstellation& orig)  :
   Formation    (orig),
   no (orig.no),
   ns (orig.ns)
{
    parameterCount = FCParamCount;
}

//------------------------------------------------------------------------------
// FlowerConstellation& operator=(const FlowerConstellation& orig)
//------------------------------------------------------------------------------
/**
 * Assignment operator.
 *
 * @param <orig> Formation that is copied onto this one.
 *
 * @return this instance, configured like the input instance.
 */
//------------------------------------------------------------------------------
FlowerConstellation& FlowerConstellation::operator=(const FlowerConstellation& orig)
{
   if (this != &orig)
   {
      FlowerConstellation::operator=(orig);

      no = orig.no;
      ns = orig.ns;
   }
   return *this;
}

//------------------------------------------------------------------------------
// ~FlowerConstellation()
//------------------------------------------------------------------------------
/**
 * Destructor.
 */
//------------------------------------------------------------------------------
FlowerConstellation::~FlowerConstellation()
{
}

bool FlowerConstellation::Initialize()
{
    return true;
}

//------------------------------------------------------------------------------
//  std::string  GetParameterText(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter text, given the input parameter ID.
 *
 * @param <id> Id for the requested parameter text.
 *
 * @return parameter text for the requested parameter.
 */
//------------------------------------------------------------------------------
std::string FlowerConstellation::GetParameterText(const Integer id) const
{
   if (id >= FormationParamCount && id < FCParamCount)
      return PARAMETER_TEXT[id - FormationParamCount];

   return FlowerConstellation::GetParameterText(id);
}


//------------------------------------------------------------------------------
//  Integer  GetParameterID(const std::string &str) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter ID, given the input parameter string.
 *
 * @param <str> string for the requested parameter.
 *
 * @return ID for the requested parameter.
 */
//------------------------------------------------------------------------------
Integer FlowerConstellation::GetParameterID(const std::string &str) const
{
   for (Integer i = FormationParamCount; i < FCParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - FormationParamCount])
         return i;
   }

   return FlowerConstellation::GetParameterID(str);
}


//------------------------------------------------------------------------------
//  Gmat::ParameterType  GetParameterType(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type of the requested parameter.
 */
//------------------------------------------------------------------------------
Gmat::ParameterType FlowerConstellation::GetParameterType(const Integer id) const
{
   if (id >= FormationParamCount && id < FCParamCount)
      return PARAMETER_TYPE[id - FormationParamCount];

   return FlowerConstellation::GetParameterType(id);
}

//------------------------------------------------------------------------------
//  std::string  GetParameterTypeString(const Integer id) const
//------------------------------------------------------------------------------
/**
 * This method returns the parameter type string, given the input parameter ID.
 *
 * @param <id> ID for the requested parameter.
 *
 * @return parameter type string of the requested parameter.
 */
//------------------------------------------------------------------------------
std::string FlowerConstellation::GetParameterTypeString(const Integer id) const
{
   return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
}

//---------------------------------------------------------------------------
//  bool IsParameterReadOnly(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is read only.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default) if not,
 *         throws if the parameter is out of the valid range of values.
 */
//---------------------------------------------------------------------------
bool FlowerConstellation::IsParameterReadOnly(const Integer id) const
{
   return FlowerConstellation::IsParameterReadOnly(id);
}
