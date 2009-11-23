//$Header$
//------------------------------------------------------------------------------
//                             AEMEulerAngleCCSDSData
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed by Dr. Matthew P. Wilkins, Schafer Corporation
//
// Author: Matthew P. Wilkins
// Created: 2009/10/22
//
/**
 *
 * This class specifies the CCSDS Attitude Ephemeris implementation of
 * the Euler Angle data construct.
 *
 */
//------------------------------------------------------------------------------

#include "AEMEulerAngleCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const bool AEMEulerAngleCCSDSData::CCSDS_IS_REQUIRED[EndEulerAngleCCSDSDataDataReps] =
{
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false,
    false
};

//------------------------------------------------------------------------------
//  AEMEulerAngleCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
AEMEulerAngleCCSDSData::AEMEulerAngleCCSDSData() : EulerAngleCCSDSData()
{
}

//------------------------------------------------------------------------------
//  AEMEulerAngleCCSDSData(const AEMEulerAngleCCSDSData &aemEA)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
AEMEulerAngleCCSDSData::AEMEulerAngleCCSDSData(const AEMEulerAngleCCSDSData &aemEA) :
        EulerAngleCCSDSData(aemEA)
{
}

//---------------------------------------------------------------------------
//  AEMEulerAngleCCSDSData& operator=(const AEMEulerAngleCCSDSData &aemEA)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <aemEA> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const AEMEulerAngleCCSDSData& AEMEulerAngleCCSDSData::operator=
                                               (const AEMEulerAngleCCSDSData &aemEA)
{
   if (&aemEA == this)
      return *this;

   EulerAngleCCSDSData::operator=(aemEA);

   return *this;
}

//------------------------------------------------------------------------------
//  ~AEMEulerAngleCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
AEMEulerAngleCCSDSData::~AEMEulerAngleCCSDSData()
{
}

//---------------------------------------------------------------------------
//  bool IsParameterRequired(const Integer id) const
//---------------------------------------------------------------------------
/**
 * Checks to see if the requested parameter is required by the data format.
 *
 * @param <id> Description for the parameter.
 *
 * @return true if the parameter is read only, false (the default)
 */
//---------------------------------------------------------------------------
bool AEMEulerAngleCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndEulerAngleCCSDSDataDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}


//---------------------------------------------------------------------------
//  Integer CountRequiredNumberAEMEulerAngleParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAEMEulerAngleParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < AEMEulerAngleCCSDSData::EndEulerAngleCCSDSDataDataReps; id++)
        if (AEMEulerAngleCCSDSData::CCSDS_IS_REQUIRED[id])
            num++;

    return num;
}

//---------------------------------------------------------------------------
//  bool Validate() const
//---------------------------------------------------------------------------
/**
 * Checks to see if the header is valid
 *
 * @return True if the header is valid, false otherwise (the default)
 */
//---------------------------------------------------------------------------
bool AEMEulerAngleCCSDSData::Validate() const
{

    if (!IsParameterDefined(eulerAngleType))
    {
        MessageInterface::ShowMessage("Error: Euler angle type must be defined!\n");
        return false;
    }
    
    for (unsigned int i = 0; i < EndEulerAngleCCSDSDataDataReps; i++ )
    {

        if (IsParameterRequired(i))
        {
            switch (GetDataParameterType(i))
            {
                case Gmat::INTEGER_TYPE:
                    if (!IsParameterDefined(GetIntegerDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Integer parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::REAL_TYPE:
                    if (!IsParameterDefined(GetRealDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required Real parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::STRING_TYPE:
                    if (!IsParameterDefined(GetStringDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required String parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                case Gmat::STRINGARRAY_TYPE:
                    if (!IsParameterDefined(GetStringArrayDataParameter(i)))
                    {
                        MessageInterface::ShowMessage("Error: Required String parameter " + GetDataParameterText(i) + " not defined!\n");
                        return false;
                    }
                    break;
                default:
                    return false;
                    break;
            }
        }
    }

    return true;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const AEMEulerAngleCCSDSData *myAEMEulerAngle)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAEMEulerAngle>    CCSDS Euler angle data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const AEMEulerAngleCCSDSData *myAEMEulerAngle)
{
    using namespace std;

    if (!myAEMEulerAngle->Validate()) return output;

    switch (myAEMEulerAngle->eulerAngleType)
    {

        case CCSDSData::CCSDS_EULER_ANGLE_ID:
        {
            output << myAEMEulerAngle->timeTag
                   << " " << myAEMEulerAngle->angle1
                   << " " << myAEMEulerAngle->angle2
                   << " " << myAEMEulerAngle->angle3 << endl;

            return output;
        }

        break;

        case CCSDSData::CCSDS_EULER_ANGLE_RATE_ID:
        {
            output << myAEMEulerAngle->timeTag
                   << " " << myAEMEulerAngle->angle1
                   << " " << myAEMEulerAngle->angle2
                   << " " << myAEMEulerAngle->angle3
                   << " " << myAEMEulerAngle->angleRate1
                   << " " << myAEMEulerAngle->angleRate2
                   << " " << myAEMEulerAngle->angleRate3 << endl;

            return output;
        }

        break;

        default:

            break;
    }

   return output;
}