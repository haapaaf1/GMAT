#include "CCSDSAEMSpinStabilized.hpp"
//---------------------------------
//  static data
//---------------------------------
const bool CCSDSAEMSpinStabilized::CCSDS_IS_REQUIRED[EndCCSDSSpinStabilizedDataReps] =
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
    false
};

//------------------------------------------------------------------------------
//  CCSDSAEMSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMSpinStabilized::CCSDSAEMSpinStabilized() : CCSDSSpinStabilized()
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMSpinStabilized(const CCSDSAEMSpinStabilized &aemSS)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMSpinStabilized::CCSDSAEMSpinStabilized
              (const CCSDSAEMSpinStabilized &aemSS) : CCSDSSpinStabilized(aemSS)
{
}

//---------------------------------------------------------------------------
//  CCSDSAEMSpinStabilized& operator=(const CCSDSAEMSpinStabilized &aemSS)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <aemSS> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMSpinStabilized& CCSDSAEMSpinStabilized::operator=
               (const CCSDSAEMSpinStabilized &aemSS)
{
   if (&aemSS == this)
      return *this;

   CCSDSSpinStabilized::operator=(aemSS);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAEMSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMSpinStabilized::~CCSDSAEMSpinStabilized()
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
bool CCSDSAEMSpinStabilized::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndCCSDSSpinStabilizedDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}


//---------------------------------------------------------------------------
//  Integer CountRequiredNumberAEMSpinStabilizedParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAEMSpinStabilizedParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < CCSDSAEMSpinStabilized::EndCCSDSSpinStabilizedDataReps; id++)
        if (CCSDSAEMSpinStabilized::CCSDS_IS_REQUIRED[id])
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
bool CCSDSAEMSpinStabilized::Validate() const
{

    if (!IsParameterDefined(attitudeType))
        return false;

    for (unsigned int i = 0; i < EndCCSDSSpinStabilizedDataReps; i++ )
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
//                       const CCSDSAEMSpinStabilized *mySS)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <mySS>    CCSDS spin stabilized data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                         const CCSDSAEMSpinStabilized *mySS)
{
    using namespace std;

    if (!mySS->Validate()) return output;

    switch (mySS->attitudeType)
    {
        case CCSDSData::CCSDS_SPIN_ID:
        {

            output << mySS->timeTag
                   << " " << mySS->spinAlpha
                   << " " << mySS->spinDelta
                   << " " << mySS->spinAngle
                   << " " << mySS->spinAngleVelocity << endl;
            return output;
        }

        break;

        case CCSDSData::CCSDS_SPIN_NUTATION_ID:
        {

            output << mySS->timeTag
                   << " " << mySS->spinAlpha
                   << " " << mySS->spinDelta
                   << " " << mySS->spinAngle
                   << " " << mySS->spinAngleVelocity
                   << " " << mySS->nutation
                   << " " << mySS->nutationPeriod
                   << " " << mySS->nutationPhase
                   << endl;

            return output;
        }
        break;

        default:

            break;

    }

    return output;
}
