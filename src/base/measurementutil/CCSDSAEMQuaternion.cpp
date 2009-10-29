#include "CCSDSAEMQuaternion.hpp"

//------------------------------------------------------------------------------
//  CCSDSAEMQuaternion()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::CCSDSAEMQuaternion() : CCSDSQuaternion()
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ) : CCSDSQuaternion(aemQ)
{
}

//---------------------------------------------------------------------------
//  CCSDSAEMQuaternion& operator=(const CCSDSAEMQuaternion &aemQ)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <AEMQ> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMQuaternion& CCSDSAEMQuaternion::operator=(const CCSDSAEMQuaternion &aemQ)
{
   if (&aemQ == this)
      return *this;

   CCSDSQuaternion::operator=(aemQ);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAEMQuaternion()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::~CCSDSAEMQuaternion()
{
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output, const CCSDSAEMQuaternion *myAEMQuaternion)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAEMQuaternion>    CCSDS quaternion data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output, const CCSDSAEMQuaternion *myAEMQuaternion)
{
   using namespace std;

    switch (myAEMQuaternion->attitudeType)
    {
        case CCSDSObType::CCSDS_QUATERNION_ID:
        {
            if (myAEMQuaternion->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << myAEMQuaternion->qC
                       << myAEMQuaternion->q1
                       << myAEMQuaternion->q2
                       << myAEMQuaternion->q3 << endl;

                return output;
            }
            else if (myAEMQuaternion->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << myAEMQuaternion->q1
                       << myAEMQuaternion->q2
                       << myAEMQuaternion->q3
                       << myAEMQuaternion->qC << endl;

                return output;
            }
            else
                return output;
        }

        break;

        case CCSDSObType::CCSDS_QUATERNION_DERIVATIVE_ID:
        {
            if (myAEMQuaternion->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << myAEMQuaternion->qC
                       << myAEMQuaternion->q1
                       << myAEMQuaternion->q2
                       << myAEMQuaternion->q3
                       << myAEMQuaternion->qCDot
                       << myAEMQuaternion->q1Dot
                       << myAEMQuaternion->q2Dot
                       << myAEMQuaternion->q3Dot << endl;

                return output;
            }
            else if (myAEMQuaternion->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << myAEMQuaternion->q1
                       << myAEMQuaternion->q2
                       << myAEMQuaternion->q3
                       << myAEMQuaternion->qC
                       << myAEMQuaternion->q1Dot
                       << myAEMQuaternion->q2Dot
                       << myAEMQuaternion->q3Dot
                       << myAEMQuaternion->qCDot << endl;

                return output;
            }
            else
                return output;
        }

        break;

        case CCSDSObType::CCSDS_QUATERNION_RATE_ID:
        {
            if (myAEMQuaternion->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_FIRST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << myAEMQuaternion->qC
                       << myAEMQuaternion->q1
                       << myAEMQuaternion->q2
                       << myAEMQuaternion->q3
                       << myAEMQuaternion->xRate
                       << myAEMQuaternion->yRate
                       << myAEMQuaternion->zRate << endl;

                return output;
            }
            else if (myAEMQuaternion->quaternionType == CCSDSQuaternion::CCSDS_QUATERNION_LAST_ID)
            {
                output << myAEMQuaternion->timeTag
                       << myAEMQuaternion->q1
                       << myAEMQuaternion->q2
                       << myAEMQuaternion->q3
                       << myAEMQuaternion->qC
                       << myAEMQuaternion->xRate
                       << myAEMQuaternion->yRate
                       << myAEMQuaternion->zRate << endl;

                return output;
            }
            else
                return output;
        }

        break;

        default:

            break;
    }

    return output;
}