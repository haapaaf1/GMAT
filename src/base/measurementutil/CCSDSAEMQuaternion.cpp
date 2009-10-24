#include "CCSDSAEMQuaternion.hpp"

//------------------------------------------------------------------------------
//  CCSDSAEMQuaternion()
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::CCSDSAEMQuaternion() : CCSDSQuaternion(),
{
}

//------------------------------------------------------------------------------
//  CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ)
//------------------------------------------------------------------------------
/**
 * Constructor for the obtype class
 */
//------------------------------------------------------------------------------
CCSDSAEMQuaternion::CCSDSAEMQuaternion(const CCSDSAEMQuaternion &aemQ) : CCSDSQuaternion(aemQ),
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

   output << "Quaternion Type = " << myAEMQuaternion->quaternionType << endl;
   output << "Q_FRAME_A = " << myAEMQuaternion->frameA << endl;
   output << "Q_FRAME_B = " << myAEMQuaternion->frameB << endl;
   output << "Q_DIR = " << myAEMQuaternion->direction << endl;
   output << "Q1 = " << myAEMQuaternion->q1 << endl;
   output << "Q2 = " << myAEMQuaternion->q2 << endl;
   output << "Q3 = " << myAEMQuaternion->q3 << endl;
   output << "QC = " << myAEMQuaternion->qC << endl;
   output << "Q1_DOT = " << myAEMQuaternion->q1Dot << endl;
   output << "Q2_DOT = " << myAEMQuaternion->q2Dot << endl;
   output << "Q3_DOT = " << myAEMQuaternion->q3Dot << endl;
   output << "QC_DOT = " << myAEMQuaternion->qCDot << endl;

   return output;
}