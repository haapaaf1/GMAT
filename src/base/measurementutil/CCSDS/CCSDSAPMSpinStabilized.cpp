#include "CCSDSAPMSpinStabilized.hpp"

//------------------------------------------------------------------------------
//  CCSDSAPMSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMSpinStabilized::CCSDSAPMSpinStabilized() : CCSDSSpinStabilized()
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMSpinStabilized(const CCSDSAPMSpinStabilized &apmSS)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMSpinStabilized::CCSDSAPMSpinStabilized
              (const CCSDSAPMSpinStabilized &apmSS) : CCSDSSpinStabilized(apmSS)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMSpinStabilized& operator=(const CCSDSAPMSpinStabilized &apmSS)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apmSS> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMSpinStabilized& CCSDSAPMSpinStabilized::operator=
                (const CCSDSAPMSpinStabilized &apmSS)
{
   if (&apmSS == this)
      return *this;

   CCSDSSpinStabilized::operator=(apmSS);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMSpinStabilized::~CCSDSAPMSpinStabilized()
{
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
                         const CCSDSAPMSpinStabilized *mySS)
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