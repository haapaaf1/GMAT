#include "CCSDSAPMEulerAngle.hpp"

//------------------------------------------------------------------------------
//  CCSDSAPMEulerAngle()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMEulerAngle::CCSDSAPMEulerAngle() : CCSDSEulerAngle()
{
}

//------------------------------------------------------------------------------
//  CCSDSAPMEulerAngle(const CCSDSAPMEulerAngle &apmEA)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMEulerAngle::CCSDSAPMEulerAngle(const CCSDSAPMEulerAngle &apmEA) : CCSDSEulerAngle(apmEA)
{
}

//---------------------------------------------------------------------------
//  CCSDSAPMEulerAngle& operator=(const CCSDSAPMEulerAngle &apmEA)
//---------------------------------------------------------------------------
/**
 * Assignment operator for ObType structures.
 *
 * @param <apmEA> The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CCSDSAPMEulerAngle& CCSDSAPMEulerAngle::operator=(const CCSDSAPMEulerAngle &apmEA)
{
   if (&apmEA == this)
      return *this;

   CCSDSEulerAngle::operator=(apmEA);

   return *this;
}

//------------------------------------------------------------------------------
//  ~CCSDSAPMEulerAngle()
//------------------------------------------------------------------------------
/**
 * Destructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAPMEulerAngle::~CCSDSAPMEulerAngle()
{
}

//------------------------------------------------------------------------------
// std::ostream& operator<< (std::ostream &output,
//                           const CCSDSAPMEulerAngle *myAPMEulerAngle)
//------------------------------------------------------------------------------
/**
 * Formats CCCSDSObType data and sends to output stream.
 *
 * @param  <output>  Output stream
 * @param  <myAPMEulerAngle>    CCSDS Euler angle data to write out
 *
 * @return  Output stream
 */
//------------------------------------------------------------------------------
std::ostream& operator<< (std::ostream &output,
                          const CCSDSAPMEulerAngle *myAPMEulerAngle)
{
   using namespace std;

   unsigned int i;
   for (i = 0; i < myAPMEulerAngle->comments.size(); i++ )
   {
       output << "COMMENT " << myAPMEulerAngle->comments[i] << endl;
   }
   if (i > 0) output << endl;

   output << "EULER_FRAME_A = " << myAPMEulerAngle->frameA << endl;
   output << "EULER_FRAME_B = " << myAPMEulerAngle->frameB << endl;
   output << "EULER_DIR = " << myAPMEulerAngle->direction << endl;
   output << "EULER_ROT_SEQ = " << myAPMEulerAngle->rotationSequence << endl;
   output << "RATE_FRAME = " << myAPMEulerAngle->rateFrame << endl;
   output << "X_ANGLE = " << myAPMEulerAngle->xAngle << endl;
   output << "Y_ANGLE = " << myAPMEulerAngle->yAngle << endl;
   output << "Z_ANGLE = " << myAPMEulerAngle->zAngle << endl;
   output << "X_RATE = " << myAPMEulerAngle->xRate << endl;
   output << "Y_RATE = " << myAPMEulerAngle->yRate << endl;
   output << "Z_RATE = " << myAPMEulerAngle->zRate << endl;

   output << endl;

   return output;
}