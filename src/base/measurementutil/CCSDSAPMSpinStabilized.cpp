#include "CCSDSAPMSpinStabilized.hpp"

//------------------------------------------------------------------------------
//  CCSDSAPMSpinStabilized()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMSpinStabilized::CCSDSAPMSpinStabilized() : CCSDSSpinStabilized(),
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMSpinStabilized(const CCSDSAPMSpinStabilized &apmSS)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMSpinStabilized::CCSDSAPMSpinStabilized(const CCSDSAPMSpinStabilized &apmSS) : CCSDSSpinStabilized(apmSS),
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
const CCSDSAPMSpinStabilized& CCSDSAPMSpinStabilized::operator=(const CCSDSAPMSpinStabilized &apmSS)
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
//                       const ccsdsAPMSpinStabilized *myCCSDSAPMSpinStabilized)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myCCSDSAPMSpinStabilized>    CCSDS spin stabilized data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                         const CCSDSAPMSpinStabilized *myCCSDSAPMSpinStabilized)
{
   using namespace std;

   switch(myCCSDSAPMSpinStabilized->attitudeType)
   {
       case CCSDSObType::CCSDS_QUATERNION_ID:

           break;
       default:
           break;
   }

   for (unsigned int i = 0; i < myCCSDSAPMSpinStabilized->comments.size(); i++)
   {
       output << "COMMENT " << myCCSDSAPMSpinStabilized->comments[i] << endl;
   }
   output << "SPIN_FRAME_A = " << myCCSDSAPMSpinStabilized->frameA << endl;
   output << "SPIN_FRAME_B = " << myCCSDSAPMSpinStabilized->frameB << endl;
   output << "SPIN_DIR = " << myCCSDSAPMSpinStabilized->direction << endl;
   output << "SPIN_ALPHA = " << myCCSDSAPMSpinStabilized->spinAlpha << endl;
   output << "SPIN_DELTA = " << myCCSDSAPMSpinStabilized->spinDelta << endl;
   output << "SPIN_ANGLE = " << myCCSDSAPMSpinStabilized->spinAngle << endl;
   output << "SPIN_ANGLE_VEL = " << myCCSDSAPMSpinStabilized->spinAngleVelocity << endl;
   output << "NUTATION = " << myCCSDSAPMSpinStabilized->nutation << endl;
   output << "NUTATION_PER = " << myCCSDSAPMSpinStabilized->nutationPeriod << endl;
   output << "NUTATION_PHASE = " << myCCSDSAPMSpinStabilized->nutationPhase << endl;

   return output;
}