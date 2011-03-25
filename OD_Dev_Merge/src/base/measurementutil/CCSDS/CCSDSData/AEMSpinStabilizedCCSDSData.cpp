//$Header$
//------------------------------------------------------------------------------
//                             AEMSpinStabilizedCCSDSData
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
 * the Spin Stabilized data construct.
 *
 */
//------------------------------------------------------------------------------

#include "AEMSpinStabilizedCCSDSData.hpp"

//---------------------------------
//  static data
//---------------------------------
const bool AEMSpinStabilizedCCSDSData::CCSDS_IS_REQUIRED[EndSpinStabilizedCCSDSDataDataReps] =
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
//  AEMSpinStabilizedCCSDSData()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
AEMSpinStabilizedCCSDSData::AEMSpinStabilizedCCSDSData() : SpinStabilizedCCSDSData()
{
}

//------------------------------------------------------------------------------
//  AEMSpinStabilizedCCSDSData(const AEMSpinStabilizedCCSDSData &aemSS)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
AEMSpinStabilizedCCSDSData::AEMSpinStabilizedCCSDSData
              (const AEMSpinStabilizedCCSDSData &aemSS) : SpinStabilizedCCSDSData(aemSS)
{
}

//---------------------------------------------------------------------------
//  AEMSpinStabilizedCCSDSData& operator=(const AEMSpinStabilizedCCSDSData &aemSS)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <aemSS> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const AEMSpinStabilizedCCSDSData& AEMSpinStabilizedCCSDSData::operator=
               (const AEMSpinStabilizedCCSDSData &aemSS)
{
   if (&aemSS == this)
      return *this;

   SpinStabilizedCCSDSData::operator=(aemSS);

   return *this;
}

//------------------------------------------------------------------------------
//  ~AEMSpinStabilizedCCSDSData()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
AEMSpinStabilizedCCSDSData::~AEMSpinStabilizedCCSDSData()
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
bool AEMSpinStabilizedCCSDSData::IsParameterRequired(const Integer id) const
{
    if (id >= 0 && id <= EndSpinStabilizedCCSDSDataDataReps)
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

    for (Integer id = 0; id < AEMSpinStabilizedCCSDSData::EndSpinStabilizedCCSDSDataDataReps; id++)
        if (AEMSpinStabilizedCCSDSData::CCSDS_IS_REQUIRED[id])
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
bool AEMSpinStabilizedCCSDSData::Validate() const
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
//                       const AEMSpinStabilizedCCSDSData *mySS)
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
                         const AEMSpinStabilizedCCSDSData *mySS)
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
