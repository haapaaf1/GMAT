//$Header$
//------------------------------------------------------------------------------
//                             APMEulerAngleCCSDSData
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
 * the Euler Angle data construct.
 *
 */
//------------------------------------------------------------------------------

#include "APMEulerAngleCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const bool APMEulerAngleCCSDSData::CCSDS_IS_REQUIRED[EndEulerAngleCCSDSDataDataReps] =
{
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
    false
};

//------------------------------------------------------------------------------
//  APMEulerAngleCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMEulerAngleCCSDSData::APMEulerAngleCCSDSData() : EulerAngleCCSDSData()
{
}

//------------------------------------------------------------------------------
//  APMEulerAngleCCSDSData(const APMEulerAngleCCSDSData &apmEA)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMEulerAngleCCSDSData::APMEulerAngleCCSDSData(const APMEulerAngleCCSDSData &apmEA) :
    EulerAngleCCSDSData(apmEA)
{
}

//---------------------------------------------------------------------------
//  APMEulerAngleCCSDSData& operator=(const APMEulerAngleCCSDSData &apmEA)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apmEA> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const APMEulerAngleCCSDSData& APMEulerAngleCCSDSData::operator=
                                               (const APMEulerAngleCCSDSData &apmEA)
{
   if (&apmEA == this)
      return *this;

   EulerAngleCCSDSData::operator=(apmEA);

   return *this;
}

//------------------------------------------------------------------------------
//  ~APMEulerAngleCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
APMEulerAngleCCSDSData::~APMEulerAngleCCSDSData()
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
bool APMEulerAngleCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndEulerAngleCCSDSDataDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}


//---------------------------------------------------------------------------
//  Integer CountRequiredNumberAPMEulerAngleParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAPMEulerAngleParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < APMEulerAngleCCSDSData::EndEulerAngleCCSDSDataDataReps; id++)
        if (APMEulerAngleCCSDSData::CCSDS_IS_REQUIRED[id])
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
bool APMEulerAngleCCSDSData::Validate() const
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
                    MessageInterface::ShowMessage("Error: Invalid Data Parameter Type in Euler Angles!\n");
                    return false;
                    break;
            }
        }
    }

    return true;
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const APMEulerAngleCCSDSData *myAPMEulerAngle)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAPMEulerAngle>    CCSDS Euler angle data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const APMEulerAngleCCSDSData *myAPMEulerAngle)
{
    using namespace std;

    if (!myAPMEulerAngle->Validate()) return output;

    for (unsigned int i = 0; i < myAPMEulerAngle->comments.size(); i++ )
    {
           output << "COMMENT " << myAPMEulerAngle->comments[i] << endl;
    }

    output << "EULER_FRAME_A = " << myAPMEulerAngle->frameA << endl;
    output << "EULER_FRAME_B = " << myAPMEulerAngle->frameB << endl;
    output << "EULER_DIR = " << myAPMEulerAngle->GetAttitudeDirText(myAPMEulerAngle->direction) << endl;
    output << "EULER_ROT_SEQ = " << myAPMEulerAngle->GetEulerSequenceText(myAPMEulerAngle->rotationSequence) << endl;

    switch (myAPMEulerAngle->eulerAngleType)
    {
        case CCSDSData::CCSDS_EULER_ANGLE_ID:
        {
            switch(myAPMEulerAngle->rotationSequence)
            {
            
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_123:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_132:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_213:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_231:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_312:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_321:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_121:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_131:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_212:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_232:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_313:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;
                
                case EulerAngleCCSDSData::CCSDS_EULERANGLE_323:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;

                    break;

                default:

                    break;
            }

        }

        break;

        case CCSDSData::CCSDS_EULER_RATE_ID:
        {

            output << "RATE_FRAME = " << myAPMEulerAngle->GetRateFrameText(myAPMEulerAngle->rateFrame) << endl;

            switch(myAPMEulerAngle->rotationSequence)
            {

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_123:

                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_132:

                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_213:

                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_231:

                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_312:

                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_321:

                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_121:

                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_131:

                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_212:

                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_232:

                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_313:

                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_323:

                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                default:

                    break;
            }
        }

        break;

        case CCSDSData::CCSDS_EULER_ANGLE_RATE_ID:
        {

            output << "RATE_FRAME = " << myAPMEulerAngle->GetRateFrameText(myAPMEulerAngle->rateFrame) << endl;

            switch(myAPMEulerAngle->rotationSequence)
            {

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_123:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_132:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_213:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_231:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_312:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_321:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_121:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_131:

                    output << "X_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_212:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_232:

                    output << "Y_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_313:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "X_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "X_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                case EulerAngleCCSDSData::CCSDS_EULERANGLE_323:

                    output << "Z_ANGLE = " << myAPMEulerAngle->angle1 << endl;
                    output << "Y_ANGLE = " << myAPMEulerAngle->angle2 << endl;
                    output << "Z_ANGLE = " << myAPMEulerAngle->angle3 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate1 << endl;
                    output << "Y_RATE = " << myAPMEulerAngle->angleRate2 << endl;
                    output << "Z_RATE = " << myAPMEulerAngle->angleRate3 << endl;

                    break;

                default:

                    break;
            }
        }

        break;

        default:

            break;
    }
    
    return output;
    
}