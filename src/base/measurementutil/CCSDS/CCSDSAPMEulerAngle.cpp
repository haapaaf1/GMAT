#include "CCSDSAPMEulerAngle.hpp"
//---------------------------------
//  static data
//---------------------------------
const bool CCSDSAPMEulerAngle::CCSDS_IS_REQUIRED[EndCCSDSEulerAngleDataReps] =
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
//  CCSDSAPMEulerAngle()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMEulerAngle::CCSDSAPMEulerAngle() : CCSDSEulerAngle()
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMEulerAngle(const CCSDSAPMEulerAngle &apmEA)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMEulerAngle::CCSDSAPMEulerAngle(const CCSDSAPMEulerAngle &apmEA) :
    CCSDSEulerAngle(apmEA)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMEulerAngle& operator=(const CCSDSAPMEulerAngle &apmEA)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apmEA> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMEulerAngle& CCSDSAPMEulerAngle::operator=
                                               (const CCSDSAPMEulerAngle &apmEA)
{
   if (&apmEA == this)
      return *this;

   CCSDSEulerAngle::operator=(apmEA);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMEulerAngle()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMEulerAngle::~CCSDSAPMEulerAngle()
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
bool CCSDSAPMEulerAngle::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSEulerAngleDataReps)
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

    for (Integer id = 0; id < CCSDSAPMEulerAngle::EndCCSDSEulerAngleDataReps; id++)
        if (CCSDSAPMEulerAngle::CCSDS_IS_REQUIRED[id])
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
bool CCSDSAPMEulerAngle::Validate() const
{

    if (!IsParameterDefined(eulerAngleType))
        return false;

    for (unsigned int i = 0; i < EndCCSDSEulerAngleDataReps; i++ )
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
//                           const CCSDSAPMEulerAngle *myAPMEulerAngle)
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
                          const CCSDSAPMEulerAngle *myAPMEulerAngle)
{
    using namespace std;

    if (!myAPMEulerAngle->Validate()) return output;

    unsigned int i;
    for (i = 0; i < myAPMEulerAngle->comments.size(); i++ )
    {
           output << "COMMENT " << myAPMEulerAngle->comments[i] << endl;
    }
    if (i > 0) output << endl;

    output << "EULER_FRAME_A = " << myAPMEulerAngle->frameA << endl;
    output << "EULER_FRAME_B = " << myAPMEulerAngle->frameB << endl;
    output << "EULER_DIR = " << myAPMEulerAngle->GetAttitudeDirText(myAPMEulerAngle->direction) << endl;
    output << "EULER_ROT_SEQ = " << myAPMEulerAngle->rotationSequence << endl;

    switch (myAPMEulerAngle->eulerAngleType)
    {
        case CCSDSData::CCSDS_EULER_ANGLE_ID:
        {
            output << "X_ANGLE = " << myAPMEulerAngle->xAngle << endl;
            output << "Y_ANGLE = " << myAPMEulerAngle->yAngle << endl;
            output << "Z_ANGLE = " << myAPMEulerAngle->zAngle << endl;
            output << endl;
        }

        break;

        case CCSDSData::CCSDS_EULER_ANGLE_RATE_ID:
        {
            output << "RATE_FRAME = " << myAPMEulerAngle->GetRateFrameText(myAPMEulerAngle->rateFrame) << endl;
            output << "X_RATE = " << myAPMEulerAngle->xRate << endl;
            output << "Y_RATE = " << myAPMEulerAngle->yRate << endl;
            output << "Z_RATE = " << myAPMEulerAngle->zRate << endl;
            output << endl;
        }

        break;

        default:

            break;
    }

   return output;
}