#include "CCSDSAPMQuaternion.hpp"

//------------------------------------------------------------------------------
//  CCSDSAPMQuaternion()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMQuaternion::CCSDSAPMQuaternion() : CCSDSQuaternion()
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMQuaternion(const CCSDSAPMQuaternion &apmQ)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMQuaternion::CCSDSAPMQuaternion(const CCSDSAPMQuaternion &apmQ) : CCSDSQuaternion(apmQ)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMQuaternion& operator=(const CCSDSAPMQuaternion &apmQ)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apmQ> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMQuaternion& CCSDSAPMQuaternion::operator=(const CCSDSAPMQuaternion &apmQ)
{
   if (&apmQ == this)
      return *this;

   CCSDSQuaternion::operator=(apmQ);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMQuaternion()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMQuaternion::~CCSDSAPMQuaternion()
{
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAPMQuaternion *myAPMQuaternion)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAPMQuaternion>    CCSDS quaternion data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAPMQuaternion *myAPMQuaternion)
{
    using namespace std;

    if (!myAPMQuaternion->Validate()) return output;

    unsigned int i;
    for (i = 0; i < myAPMQuaternion->comments.size(); i++ )
    {
        output << "COMMENT " << myAPMQuaternion->comments[i] << endl;
    }
    if (i > 0) output << endl;

    output << "EPOCH = " << myAPMQuaternion->timeTag << endl;
    output << "Q_FRAME_A = " << myAPMQuaternion->frameA << endl;
    output << "Q_FRAME_B = " << myAPMQuaternion->frameB << endl;
    output << "Q_DIR = " << myAPMQuaternion->GetAttitudeDirText(myAPMQuaternion->direction) << endl;

    switch (myAPMQuaternion->attitudeType)
    {
        case CCSDSData::CCSDS_QUATERNION_ID:
        {
            output << "Q1 = " << myAPMQuaternion->q1 << endl;
            output << "Q2 = " << myAPMQuaternion->q2 << endl;
            output << "Q3 = " << myAPMQuaternion->q3 << endl;
            output << "QC = " << myAPMQuaternion->qC << endl;
            return output;
        }

        break;

        case CCSDSData::CCSDS_QUATERNION_DERIVATIVE_ID:
        {
            output << "Q1 = " << myAPMQuaternion->q1 << endl;
            output << "Q2 = " << myAPMQuaternion->q2 << endl;
            output << "Q3 = " << myAPMQuaternion->q3 << endl;
            output << "QC = " << myAPMQuaternion->qC << endl;
            output << "Q1_DOT = " << myAPMQuaternion->q1Dot << endl;
            output << "Q2_DOT = " << myAPMQuaternion->q2Dot << endl;
            output << "Q3_DOT = " << myAPMQuaternion->q3Dot << endl;
            output << "QC_DOT = " << myAPMQuaternion->qCDot << endl;
            return output;
        }

        break;

        case CCSDSData::CCSDS_QUATERNION_RATE_ID:
        {
            output << "Q1 = " << myAPMQuaternion->q1 << endl;
            output << "Q2 = " << myAPMQuaternion->q2 << endl;
            output << "Q3 = " << myAPMQuaternion->q3 << endl;
            output << "QC = " << myAPMQuaternion->qC << endl;
            output << "X_RATE = " << myAPMQuaternion->xRate << endl;
            output << "Y_RATE = " << myAPMQuaternion->yRate << endl;
            output << "Z_RATE = " << myAPMQuaternion->zRate << endl;
            return output;
        }

        break;

        default:

            break;
    }

   output << endl;

   return output;
}