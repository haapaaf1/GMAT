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

   output << "EULER_FRAME_A = " << myAEMEulerAngle->frameA << endl;
   output << "EULER_FRAME_B = " << myAEMEulerAngle->frameB << endl;
   output << "EULER_DIR = " << myAEMEulerAngle->direction << endl;
   output << "EULER_ROT_SEQ = " << myAEMEulerAngle->rotationSequence << endl;
   output << "RATE_FRAME = " << myAEMEulerAngle->rateFrame << endl;
   output << "X_ANGLE = " << myAEMEulerAngle->xAngle << endl;
   output << "Y_ANGLE = " << myAEMEulerAngle->yAngle << endl;
   output << "Z_ANGLE = " << myAEMEulerAngle->zAngle << endl;
   output << "X_RATE = " << myAEMEulerAngle->xRate << endl;
   output << "Y_RATE = " << myAEMEulerAngle->yRate << endl;
   output << "Z_RATE = " << myAEMEulerAngle->zRate << endl;

   return output;
}