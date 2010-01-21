//$Header$
//------------------------------------------------------------------------------
//                             APMQuaternionCCSDSData
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
 * This class specifies the CCSDS Attitude Parameter implementation of
 * the Quaternion data construct.
 *
 */
//------------------------------------------------------------------------------

#include "APMQuaternionCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const bool APMQuaternionCCSDSData::CCSDS_IS_REQUIRED[EndQuaternionCCSDSDataReps] =
{
    true,
    true,
    true,
    true,
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
//  APMQuaternionCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMQuaternionCCSDSData::APMQuaternionCCSDSData() : QuaternionCCSDSData()
{
}

//------------------------------------------------------------------------------
//  APMQuaternionCCSDSData(const APMQuaternionCCSDSData &apmQ)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMQuaternionCCSDSData::APMQuaternionCCSDSData(const APMQuaternionCCSDSData &apmQ) : QuaternionCCSDSData(apmQ)
{
}

//---------------------------------------------------------------------------
//  APMQuaternionCCSDSData& operator=(const APMQuaternionCCSDSData &apmQ)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apmQ> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const APMQuaternionCCSDSData& APMQuaternionCCSDSData::operator=(const APMQuaternionCCSDSData &apmQ)
{
   if (&apmQ == this)
      return *this;

   QuaternionCCSDSData::operator=(apmQ);

   return *this;
}

//------------------------------------------------------------------------------
//  ~APMQuaternionCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
APMQuaternionCCSDSData::~APMQuaternionCCSDSData()
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
bool APMQuaternionCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndQuaternionCCSDSDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}



//---------------------------------------------------------------------------
//  Integer CountRequiredNumberAPMQuaternionParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAPMQuaternionParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < APMQuaternionCCSDSData::EndQuaternionCCSDSDataReps; id++)
        if (APMQuaternionCCSDSData::CCSDS_IS_REQUIRED[id])
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
bool APMQuaternionCCSDSData::Validate() const
{

    if (!IsParameterDefined(attitudeType))
    {
        MessageInterface::ShowMessage("Error: Quaternion must have attitudeType defined.\n");
        return false;
    }

    for (unsigned int i = 0; i < EndQuaternionCCSDSDataReps; i++ )
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
//                           const APMQuaternionCCSDSData *myAPMQuaternion)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAPMQuaternion>    CCSDS quaternion data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const APMQuaternionCCSDSData *myAPMQuaternion)
{
    using namespace std;

    if (!myAPMQuaternion->Validate()) return output;

    for (unsigned int i = 0; i < myAPMQuaternion->comments.size(); i++ )
    {
        output << "COMMENT " << myAPMQuaternion->comments[i] << endl;
    }
    
    output << "EPOCH = " << myAPMQuaternion->timeTag << endl;
    output << "Q_FRAME_A = " << myAPMQuaternion->frameA << endl;
    output << "Q_FRAME_B = " << myAPMQuaternion->frameB << endl;
    output << "Q_DIR = " << myAPMQuaternion->GetAttitudeDirText(myAPMQuaternion->direction) << endl;

    switch (myAPMQuaternion->attitudeType)
    {
        case CCSDSData::CCSDS_QUATERNION_ID:
        {
            output << "Q1 = " << myAPMQuaternion->q1 << endl;
            output << "Q2 = " << myAPMQuaternion->q2 << endl;
            output << "Q3 = " << myAPMQuaternion->q3 << endl;
            output << "QC = " << myAPMQuaternion->qC << endl;
            return output;
        }

        break;

        case CCSDSData::CCSDS_QUATERNION_DERIVATIVE_ID:
        {
            output << "Q1 = " << myAPMQuaternion->q1 << endl;
            output << "Q2 = " << myAPMQuaternion->q2 << endl;
            output << "Q3 = " << myAPMQuaternion->q3 << endl;
            output << "QC = " << myAPMQuaternion->qC << endl;
            output << "Q1_DOT = " << myAPMQuaternion->q1Dot << endl;
            output << "Q2_DOT = " << myAPMQuaternion->q2Dot << endl;
            output << "Q3_DOT = " << myAPMQuaternion->q3Dot << endl;
            output << "QC_DOT = " << myAPMQuaternion->qCDot << endl;
            return output;
        }

        break;

        case CCSDSData::CCSDS_QUATERNION_RATE_ID:
        {
            output << "Q1 = " << myAPMQuaternion->q1 << endl;
            output << "Q2 = " << myAPMQuaternion->q2 << endl;
            output << "Q3 = " << myAPMQuaternion->q3 << endl;
            output << "QC = " << myAPMQuaternion->qC << endl;
            output << "X_RATE = " << myAPMQuaternion->xRate << endl;
            output << "Y_RATE = " << myAPMQuaternion->yRate << endl;
            output << "Z_RATE = " << myAPMQuaternion->zRate << endl;
            return output;
        }

        break;

        default:

            break;
    }

   output << endl;

   return output;
}