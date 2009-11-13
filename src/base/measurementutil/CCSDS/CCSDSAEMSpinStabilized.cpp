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
CCSDSAEMSpinStabilized::CCSDSAEMSpinStabilized(const CCSDSAEMSpinStabilized &aemSS) : CCSDSSpinStabilized(aemSS)
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
const CCSDSAEMSpinStabilized& CCSDSAEMSpinStabilized::operator=(const CCSDSAEMSpinStabilized &aemSS)
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
//                       const CCSDSAEMSpinStabilized *myAEMSpinStabilized)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAEMSpinStabilized>    CCSDS spin stabilized data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                         const CCSDSAEMSpinStabilized *myAEMSpinStabilized)
{
    using namespace std;

    for (unsigned int i = 0; i < myAEMSpinStabilized->comments.size(); i++)
    {
        output << "COMMENT " << myAEMSpinStabilized->comments[i] << endl;
    }

    switch (myAEMSpinStabilized->attitudeType)
    {
        case CCSDSObType::CCSDS_SPIN_ID:
        {
            output << myAEMSpinStabilized->timeTag
                   << " " << myAEMSpinStabilized->spinAlpha
                   << " " << myAEMSpinStabilized->spinDelta
                   << " " << myAEMSpinStabilized->spinAngle
                   << " " << myAEMSpinStabilized->spinAngleVelocity << endl;
            return output;
        }

        break;

        case CCSDSObType::CCSDS_SPIN_NUTATION_ID:
        {
            output << myAEMSpinStabilized->timeTag
                   << " " << myAEMSpinStabilized->spinAlpha
                   << " " << myAEMSpinStabilized->spinDelta
                   << " " << myAEMSpinStabilized->spinAngle
                   << " " << myAEMSpinStabilized->spinAngleVelocity
                   << " " << myAEMSpinStabilized->nutation
                   << " " << myAEMSpinStabilized->nutationPeriod
                   << " " << myAEMSpinStabilized->nutationPhase
                   << endl;

            return output;
        }
        break;

        default:

            break;

    }

    return output;
}
