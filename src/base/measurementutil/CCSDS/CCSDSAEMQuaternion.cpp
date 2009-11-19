//$Header$
//------------------------------------------------------------------------------
//                             CCSDSAEMQuaternion
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
 * the Quaternion data construct.
 *
 */
//------------------------------------------------------------------------------

#include "CCSDSAEMQuaternion.hpp"

//---------------------------------
//  static data
//---------------------------------
const bool CCSDSAEMQuaternion::CCSDS_IS_REQUIRED[EndCCSDSQuaternionDataReps] =
{
    true,
    false,
    false,
    false,
    true,
    true,
    true,
    true,
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
//  CCSDSAEMQuaternion()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::CCSDSAEMQuaternion() : CCSDSQuaternion()
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ) :
    CCSDSQuaternion(aemQ)
{
}

//---------------------------------------------------------------------------
//  CCSDSAEMQuaternion& operator=(const CCSDSAEMQuaternion &aemQ)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <AEMQ> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMQuaternion& CCSDSAEMQuaternion::operator=
                                                (const CCSDSAEMQuaternion &aemQ)
{
   if (&aemQ == this)
      return *this;

   CCSDSQuaternion::operator=(aemQ);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAEMQuaternion()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::~CCSDSAEMQuaternion()
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
bool CCSDSAEMQuaternion::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSQuaternionDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}



//---------------------------------------------------------------------------
//  Integer CountRequiredNumberAEMQuaternionParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAEMQuaternionParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSAEMQuaternion::EndCCSDSQuaternionDataReps; id++)
        if (CCSDSAEMQuaternion::CCSDS_IS_REQUIRED[id])
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
bool CCSDSAEMQuaternion::Validate() const
{

    if (!IsParameterDefined(attitudeType))
    {
        MessageInterface::ShowMessage("Error: Quaternion must have attitudeType defined.\n");
        return false;
    }

    if (!IsParameterDefined(quaternionType))
    {
        MessageInterface::ShowMessage("Error: Quaternion must have quaternionType defined.\n");
        return false;
    }

    for (unsigned int i = 0; i < EndCCSDSQuaternionDataReps; i++ )
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
// std::ostream& operator<< (std::ostream &output, const CCSDSAEMQuaternion *myAEMQuaternion)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAEMQuaternion>    CCSDS quaternion data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAEMQuaternion *myAEMQuaternion)
{
    using namespace std;

    if (!myAEMQuaternion->Validate()) return output;

    switch (myAEMQuaternion->attitudeType)
    {
        case CCSDSData::CCSDS_QUATERNION_ID:
        {
            if (myAEMQuaternion->quaternionType == CCSDSAEMQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << " " << myAEMQuaternion->qC
                       << " " << myAEMQuaternion->q1
                       << " " << myAEMQuaternion->q2
                       << " " << myAEMQuaternion->q3 << endl;

                return output;
            }
            else if (myAEMQuaternion->quaternionType == CCSDSAEMQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << " " << myAEMQuaternion->q1
                       << " " << myAEMQuaternion->q2
                       << " " << myAEMQuaternion->q3
                       << " " << myAEMQuaternion->qC << endl;

                return output;
            }
            else
                return output;
        }

        break;

        case CCSDSData::CCSDS_QUATERNION_DERIVATIVE_ID:
        {
            if (myAEMQuaternion->quaternionType == CCSDSAEMQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << " " << myAEMQuaternion->qC
                       << " " << myAEMQuaternion->q1
                       << " " << myAEMQuaternion->q2
                       << " " << myAEMQuaternion->q3
                       << " " << myAEMQuaternion->qCDot
                       << " " << myAEMQuaternion->q1Dot
                       << " " << myAEMQuaternion->q2Dot
                       << " " << myAEMQuaternion->q3Dot << endl;

                return output;
            }
            else if (myAEMQuaternion->quaternionType == CCSDSAEMQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << " " << myAEMQuaternion->q1
                       << " " << myAEMQuaternion->q2
                       << " " << myAEMQuaternion->q3
                       << " " << myAEMQuaternion->qC
                       << " " << myAEMQuaternion->q1Dot
                       << " " << myAEMQuaternion->q2Dot
                       << " " << myAEMQuaternion->q3Dot
                       << " " << myAEMQuaternion->qCDot << endl;

                return output;
            }
            else
                return output;
        }

        break;

        case CCSDSData::CCSDS_QUATERNION_RATE_ID:
        {
            if (myAEMQuaternion->quaternionType == CCSDSAEMQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << " " << myAEMQuaternion->qC
                       << " " << myAEMQuaternion->q1
                       << " " << myAEMQuaternion->q2
                       << " " << myAEMQuaternion->q3
                       << " " << myAEMQuaternion->xRate
                       << " " << myAEMQuaternion->yRate
                       << " " << myAEMQuaternion->zRate << endl;

                return output;
            }
            else if (myAEMQuaternion->quaternionType == CCSDSAEMQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << " " << myAEMQuaternion->q1
                       << " " << myAEMQuaternion->q2
                       << " " << myAEMQuaternion->q3
                       << " " << myAEMQuaternion->qC
                       << " " << myAEMQuaternion->xRate
                       << " " << myAEMQuaternion->yRate
                       << " " << myAEMQuaternion->zRate << endl;

                return output;
            }
            else
                return output;
        }

        break;

        default:

            break;
    }

    return output;
}