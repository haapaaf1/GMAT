#include "CCSDSAEMSpinStabilized.hpp"

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

    for (unsigned int i = 0; i < mySS->comments.size(); i++)
    {
        output << "COMMENT " << mySS->comments[i] << endl;
    }

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
