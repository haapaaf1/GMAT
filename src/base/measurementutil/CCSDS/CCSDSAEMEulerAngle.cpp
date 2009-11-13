#include "CCSDSAEMEulerAngle.hpp"

//------------------------------------------------------------------------------
//  CCSDSAEMEulerAngle()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMEulerAngle::CCSDSAEMEulerAngle() : CCSDSEulerAngle()
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMEulerAngle(const CCSDSAEMEulerAngle &aemEA)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMEulerAngle::CCSDSAEMEulerAngle(const CCSDSAEMEulerAngle &aemEA) : CCSDSEulerAngle(aemEA)
{
}

//---------------------------------------------------------------------------
//  CCSDSAEMEulerAngle& operator=(const CCSDSAEMEulerAngle &aemEA)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <aemEA> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAEMEulerAngle& CCSDSAEMEulerAngle::operator=(const CCSDSAEMEulerAngle &aemEA)
{
   if (&aemEA == this)
      return *this;

   CCSDSEulerAngle::operator=(aemEA);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAEMEulerAngle()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMEulerAngle::~CCSDSAEMEulerAngle()
{
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAEMEulerAngle *myAEMEulerAngle)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAEMEulerAngle>    CCSDS Euler angle data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAEMEulerAngle *myAEMEulerAngle)
{
    using namespace std;

    for (unsigned int i = 0; i < myAEMEulerAngle->comments.size(); i++)
    {
        output << "COMMENT " << myAEMEulerAngle->comments[i] << endl;
    }

    switch (myAEMEulerAngle->eulerAngleType)
    {

        case CCSDSObType::CCSDS_EULER_ANGLE_ID:
        {
            output << myAEMEulerAngle->timeTag
                   << " " << myAEMEulerAngle->xAngle
                   << " " << myAEMEulerAngle->yAngle
                   << " " << myAEMEulerAngle->zAngle << endl;

            return output;
        }

        break;

        case CCSDSObType::CCSDS_EULER_ANGLE_RATE_ID:
        {
            output << myAEMEulerAngle->timeTag
                   << " " << myAEMEulerAngle->xAngle
                   << " " << myAEMEulerAngle->yAngle
                   << " " << myAEMEulerAngle->zAngle
                   << " " << myAEMEulerAngle->xRate
                   << " " << myAEMEulerAngle->yRate
                   << " " << myAEMEulerAngle->zRate << endl;

            return output;
        }

        break;

        default:

            break;
    }

   return output;
}