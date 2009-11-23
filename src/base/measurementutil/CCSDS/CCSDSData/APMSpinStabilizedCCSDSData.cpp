//$Header$
//------------------------------------------------------------------------------
//                             APMSpinStabilizedCCSDSData
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
 * the Spin Stabilized data construct.
 *
 */
//------------------------------------------------------------------------------

#include "APMSpinStabilizedCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const bool APMSpinStabilizedCCSDSData::CCSDS_IS_REQUIRED[EndSpinStabilizedCCSDSDataDataReps] =
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
    false
};

//------------------------------------------------------------------------------
//  APMSpinStabilizedCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMSpinStabilizedCCSDSData::APMSpinStabilizedCCSDSData() : SpinStabilizedCCSDSData()
{
}

//------------------------------------------------------------------------------
//  APMSpinStabilizedCCSDSData(const APMSpinStabilizedCCSDSData &apmSS)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
APMSpinStabilizedCCSDSData::APMSpinStabilizedCCSDSData
              (const APMSpinStabilizedCCSDSData &apmSS) : SpinStabilizedCCSDSData(apmSS)
{
}

//---------------------------------------------------------------------------
//  APMSpinStabilizedCCSDSData& operator=(const APMSpinStabilizedCCSDSData &apmSS)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apmSS> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const APMSpinStabilizedCCSDSData& APMSpinStabilizedCCSDSData::operator=
                (const APMSpinStabilizedCCSDSData &apmSS)
{
   if (&apmSS == this)
      return *this;

   SpinStabilizedCCSDSData::operator=(apmSS);

   return *this;
}

//------------------------------------------------------------------------------
//  ~APMSpinStabilizedCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
APMSpinStabilizedCCSDSData::~APMSpinStabilizedCCSDSData()
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
bool APMSpinStabilizedCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndSpinStabilizedCCSDSDataDataReps)
        return CCSDS_IS_REQUIRED[id];
    else
        return false;
}


//---------------------------------------------------------------------------
//  Integer CountRequiredNumberAPMSpinStabilizedParameters()
//---------------------------------------------------------------------------
/**
 * Count the number of required variables.
 *
 * @return The number of required variables.
 */
//---------------------------------------------------------------------------
Integer CountRequiredNumberAPMSpinStabilizedParameters()
{

    Integer num = 0;

    for (Integer id = 0; id < APMSpinStabilizedCCSDSData::EndSpinStabilizedCCSDSDataDataReps; id++)
        if (APMSpinStabilizedCCSDSData::CCSDS_IS_REQUIRED[id])
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
bool APMSpinStabilizedCCSDSData::Validate() const
{

    if (!IsParameterDefined(attitudeType))
        return false;

    for (unsigned int i = 0; i < EndSpinStabilizedCCSDSDataDataReps; i++ )
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
//                       const ccsdsAPMSpinStabilized *mySS)
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
                         const APMSpinStabilizedCCSDSData *mySS)
{
    using namespace std;

    if (!mySS->Validate()) return output;

    unsigned int i;
    for (i = 0; i < mySS->comments.size(); i++ )
    {
        output << "COMMENT " << mySS->comments[i] << endl;
    }
    if (i > 0) output << endl;

    switch (mySS->attitudeType)
    {
        case CCSDSData::CCSDS_SPIN_ID:

            output << "SPIN_FRAME_A = " << mySS->frameA << endl;
            output << "SPIN_FRAME_B = " << mySS->frameB << endl;
            output << "SPIN_DIR = " << mySS->direction << endl;
            output << "SPIN_ALPHA = " << mySS->spinAlpha << endl;
            output << "SPIN_DELTA = " << mySS->spinDelta << endl;
            output << "SPIN_ANGLE = " << mySS->spinAngle << endl;
            output << "SPIN_ANGLE_VEL = " << mySS->spinAngleVelocity << endl;

            break;

        case CCSDSData::CCSDS_SPIN_NUTATION_ID:

            output << "SPIN_FRAME_A = " << mySS->frameA << endl;
            output << "SPIN_FRAME_B = " << mySS->frameB << endl;
            output << "SPIN_DIR = " << mySS->direction << endl;
            output << "SPIN_ALPHA = " << mySS->spinAlpha << endl;
            output << "SPIN_DELTA = " << mySS->spinDelta << endl;
            output << "SPIN_ANGLE = " << mySS->spinAngle << endl;
            output << "SPIN_ANGLE_VEL = " << mySS->spinAngleVelocity << endl;
            output << "NUTATION = " << mySS->nutation << endl;
            output << "NUTATION_PER = " << mySS->nutationPeriod << endl;
            output << "NUTATION_PHASE = " << mySS->nutationPhase << endl;

            break;

        default:

            break;

    }

    output << endl;

    return output;

}