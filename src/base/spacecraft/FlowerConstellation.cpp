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
#include "RealUtilities.hpp"

//---------------------------------
// static data
//---------------------------------
const std::string
FlowerConstellation::PARAMETER_TEXT[FCParamCount - FormationParamCount] =
{
   "Eccentricity",
   "Inclination",
   "ArgPer",
   "Np",
   "Nd",
   "Fn",
   "NumOrbits",
   "Fh",
   "NumSats"
};


const Gmat::ParameterType
FlowerConstellation::PARAMETER_TYPE[FCParamCount - FormationParamCount] =
{
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::REAL_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE,
   Gmat::INTEGER_TYPE,
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
    eccentricity (0),
    inclination (0),
    argper (0),
    np     (0),
    nd     (0),
    fn     (0),
    no     (0),
    fh     (0),
    ns     (1)
{
   objectTypes.push_back(Gmat::FORMATION);
   objectTypeNames.push_back("Formation");

   parameterCount = FCParamCount;
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
    eccentricity (orig.eccentricity),
    inclination (orig.inclination),
    argper (orig.argper),
    np     (orig.np),
    nd     (orig.nd),
    fn     (orig.fn),
    no     (orig.no),
    fh     (orig.fh),
    ns     (orig.ns)
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

        eccentricity = orig.eccentricity;
        inclination = orig.inclination;
        argper = orig.argper;
        np = orig.np;
        nd = orig.nd;
        fn = orig.fn;
        no = orig.no;
        fh = orig.fh;
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

//------------------------------------------------------------------------------
// Initialize()
//------------------------------------------------------------------------------
/**
 * Method used to initialize the Flower Constellation. This will construct
 * a Formation object and populate it with the required satellite objects
 * for a Flower Constellation.
 */
//------------------------------------------------------------------------------
bool FlowerConstellation::Initialize()
{
    // Find the Bézout numbers corresponding to Fn, and no
    Integer en, ed;
    GmatMathUtil::EGCD(fn,no,en,ed);

    // Compute the factor G for later use
    G = gcd(nd, np*fn + no*fh);

    // The configuration number, nc
    nc = GmatMathUtil::Mod(en*(np*fn + no*fh)/G,no);

    // The number of satellites per orbit
    nso = nd / g;

    // Compute the number of satellites
    ns = no*nso;

    Real raan[no,nso], meanAnomaly[no,nso]

    for (Integer i = 0; i < no - 1; i++)
    {
        raan[i,j] = 2*GmatMathUtil::PI*i / no;
        for (Integer j = 0; j < nso - 1; j++)
            meanAnomaly[i,j] = (2*GmatMathUtil::PI*j - nc*raan[i,j]) / nso;
    }
    
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

   return Formation::GetParameterText(id);
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

   return Formation::GetParameterID(str);
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

   return Formation::GetParameterType(id);
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
   return Formation::IsParameterReadOnly(id);
}

//------------------------------------------------------------------------------
// bool GetIntegerParameter(const Integer id) const
//------------------------------------------------------------------------------
/**
 * Retrieve a boolean parameter.
 *
 * @param <id> The id for the parameter.
 *
 * @return the integer value for this parameter.
 */
//------------------------------------------------------------------------------
bool FlowerConstellation::GetIntegerParameter(const Integer id) const
{
   switch (id)
   {
      case NUMORBITS_ID:
         return no;
      case NUMSATS_ID:
         return ns;
      default:
         return Formation::GetIntegerParameter(id);
   }
}

//------------------------------------------------------------------------------
// bool GetIntegerParameter(const std::string &label) const
//------------------------------------------------------------------------------
/**
 * Retrieve a boolean parameter.
 *
 * @param <label> The (string) label for the parameter.
 *
 * @return the integer value for this parameter.
 */
//------------------------------------------------------------------------------
bool FlowerConstellation::GetIntegerParameter(const std::string &label) const
{
   return GetIntegerParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// bool SetIntegerParameter(const Integer id, const Integer value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a boolean parameter.
 *
 * This method is used to clear the Formation's list of spacecraft.
 *
 * @param <id> The id for the parameter.
 * @param <value> The new value for the parameter.
 *
 * @return the integer value for this parameter.
 */
//------------------------------------------------------------------------------
bool FlowerConstellation::SetIntegerParameter(const Integer id, const Integer value)
{

   switch (id)
   {
      case NUMORBITS_ID:
         no = value;
         break;
      case NUMSATS_ID:
         ns = value;
         break;
      default:
         return Formation::SetIntegerParameter(id, value);
   }
}

//------------------------------------------------------------------------------
// bool SetIntegerParameter(const std::string &label, const Integer value)
//------------------------------------------------------------------------------
/**
 * Sets the value for a boolean parameter.
 *
 * @param <label> The (string) label for the parameter.
 * @param <value> The new value for the parameter.
 *
 * @return the integer value for this parameter.
 */
//------------------------------------------------------------------------------
bool FlowerConstellation::SetIntegerParameter(const std::string &label,
                                    const bool value)
{
   #inoef DEBUG_FORMATION_ACTIONS
      MessageInterface::ShowMessage(
         "FlowerConstellation::SetIntegerParameter called with label = %s\n",
         label.c_str());
   #endif

   return SetIntegerParameter(GetParameterID(label), value);
}