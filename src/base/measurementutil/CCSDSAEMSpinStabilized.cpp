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
//                       const CCSDSAEMSpinStabilized *myCCSDSAEMSpinStabilized)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myCCSDSAEMSpinStabilized>    CCSDS spin stabilized data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                         const CCSDSAEMSpinStabilized *myCCSDSAEMSpinStabilized)
{
    using namespace std;

    for (unsigned int i = 0; i < myCCSDSAEMSpinStabilized->comments.size(); i++)
    {
        output << "COMMENT " << myCCSDSAEMSpinStabilized->comments[i] << endl;
    }

    switch (myCCSDSAEMSpinStabilized->attitudeType)
    {
        case CCSDSObType::CCSDS_SPIN_ID:

            output << "SPIN_FRAME_A = " << myCCSDSAEMSpinStabilized->frameA << endl;
            output << "SPIN_FRAME_B = " << myCCSDSAEMSpinStabilized->frameB << endl;
            output << "SPIN_DIR = " << myCCSDSAEMSpinStabilized->direction << endl;
            output << "SPIN_ALPHA = " << myCCSDSAEMSpinStabilized->spinAlpha << endl;
            output << "SPIN_DELTA = " << myCCSDSAEMSpinStabilized->spinDelta << endl;
            output << "SPIN_ANGLE = " << myCCSDSAEMSpinStabilized->spinAngle << endl;
            output << "SPIN_ANGLE_VEL = " << myCCSDSAEMSpinStabilized->spinAngleVelocity << endl;

            break;

        case CCSDSObType::CCSDS_SPIN_NUTATION_ID:

            output << "SPIN_FRAME_A = " << myCCSDSAEMSpinStabilized->frameA << endl;
            output << "SPIN_FRAME_B = " << myCCSDSAEMSpinStabilized->frameB << endl;
            output << "SPIN_DIR = " << myCCSDSAEMSpinStabilized->direction << endl;
            output << "SPIN_ALPHA = " << myCCSDSAEMSpinStabilized->spinAlpha << endl;
            output << "SPIN_DELTA = " << myCCSDSAEMSpinStabilized->spinDelta << endl;
            output << "SPIN_ANGLE = " << myCCSDSAEMSpinStabilized->spinAngle << endl;
            output << "SPIN_ANGLE_VEL = " << myCCSDSAEMSpinStabilized->spinAngleVelocity << endl;
            output << "NUTATION = " << myCCSDSAEMSpinStabilized->nutation << endl;
            output << "NUTATION_PER = " << myCCSDSAEMSpinStabilized->nutationPeriod << endl;
            output << "NUTATION_PHASE = " << myCCSDSAEMSpinStabilized->nutationPhase << endl;

            break;

        default:

            break;

    }

    return output;
}
