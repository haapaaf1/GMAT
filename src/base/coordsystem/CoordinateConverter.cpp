//$Id$
//------------------------------------------------------------------------------
//                                  CoordinateConverter
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under 
// MOMS Task order 124.
//
// Author: Wendy C. Shoan
// Created: 2004/12/27
//
/**
 * Implementation of the CoordinateConverter class.
 *
 */
//------------------------------------------------------------------------------

#include "gmatdefs.hpp"
#include "GmatBase.hpp"
#include "CoordinateConverter.hpp"
#include "SpacePoint.hpp"
#include "A1Mjd.hpp"
#include "CoordinateSystem.hpp"
#include "CoordinateSystemException.hpp"
#include "Rvector.hpp"
#include "TimeTypes.hpp"
#include "ICRFAxes.hpp"
#include "MessageInterface.hpp"

//#define DEBUG_FIRST_CALL
//#define DEBUG_TO_FROM
//#define DEBUG_BASE_SYSTEM
//#define DEBUG_ROT_DOT_MATRIX

#ifdef DEBUG_TO_FROM
   #include "MessageInterface.hpp"
#endif

#ifdef DEBUG_FIRST_CALL 
   #include "MessageInterface.hpp"
   static bool firstCallFired = false;
#endif

//---------------------------------
// static data
//---------------------------------
// none at this time

//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------

//---------------------------------------------------------------------------
//  CoordinateConverter();
//---------------------------------------------------------------------------
/**
 * Constructs CoordinateConverter objects.
 * (default constructor).
 */
//---------------------------------------------------------------------------
CoordinateConverter::CoordinateConverter() //:
//j2000Body         (NULL),
//j2000BodyName     ("Earth")
{
}

//---------------------------------------------------------------------------
//  CoordinateConverter(const CoordinateConverter &coordCvt);
//---------------------------------------------------------------------------
/**
 * Constructs CoordinateConverter classes, by copying 
 * the input instance (copy constructor).
 *
 * @param coordCvt  CoordinateConverter instance to copy to create "this" 
 *                  instance.
 */
//---------------------------------------------------------------------------
CoordinateConverter::CoordinateConverter(const CoordinateConverter &coordCvt) //:
//j2000Body     (coordCvt.j2000Body),
//j2000BodyName (coordCvt.j2000BodyName)
{
}

//---------------------------------------------------------------------------
//  CoordinateConverter& operator=(const CoordinateConverter &coordCvt)
//---------------------------------------------------------------------------
/**
 * Assignment operator for CoordinateConverter objects.
 *
 * @param coordCvt The original that is being copied.
 *
 * @return Reference to this object
 */
//---------------------------------------------------------------------------
const CoordinateConverter& CoordinateConverter::operator=(
                           const CoordinateConverter &coordCvt)
{
   if (&coordCvt == this)
      return *this;
   //j2000Body     = coordCvt.j2000Body;
   //j2000BodyName = coordCvt.j2000BodyName;
   
   return *this;
}
//---------------------------------------------------------------------------
//  ~CoordinateConverter()
//---------------------------------------------------------------------------
/**
 * Destructor.
 */
//---------------------------------------------------------------------------
CoordinateConverter::~CoordinateConverter()
{
   // nothing to do ... hm ... hm ... hm ... hm ... la-de-da-de-da
   
}

//------------------------------------------------------------------------------
//  void  Initialize()
//------------------------------------------------------------------------------
/**
 * This method initializes the CoordinateConverter class.
 *
 */
//------------------------------------------------------------------------------
void CoordinateConverter::Initialize()
{
   //if (!j2000Body)
   //   throw CoordinateSystemException(
   //         "j2000Body has not been defined for CoordinateConverter object");
   //toData      = toMJ2000RotMatrix.GetDataVector();
   //fromData    = fromMJ2000Matrix.GetDataVector();
   //internalState.SetSize(6);
   //toMJ2000RotMatrix = Rmatrix33();
   //fromMJ2000Matrix  = Rmatrix33();
   //toData      = toMJ2000RotMatrix.GetDataVector();
   //fromData    = fromMJ2000Matrix.GetDataVector();
   #ifdef DEBUG_FIRST_CALL
      firstCallFired = false;
   #endif
}

//------------------------------------------------------------------------------
//  void  SetJ2000BodyName(const std::string &toName)
//------------------------------------------------------------------------------
/**
 * This method sets the j2000 body name for the CoordinateConverter object.
 *
 * @param toName name to which to set the j2000 body name.
 */
//------------------------------------------------------------------------------
//void CoordinateConverter::SetJ2000BodyName(const std::string &toName)
//{
//   j2000BodyName = toName;
//}

//------------------------------------------------------------------------------
//  std::string  GetJ2000BodyName() const
//------------------------------------------------------------------------------
/**
 * This method returns the j2000 body name for the CoordinateConverter object.
 *
 * @return j2000 body name.
 */
//------------------------------------------------------------------------------
//std::string CoordinateConverter::GetJ2000BodyName() const
//{
//   if (j2000Body) return j2000Body->GetName();
//   else           return j2000BodyName;
//}

//------------------------------------------------------------------------------
//  void  SetJ2000Body(SpacePoint *toBody)
//------------------------------------------------------------------------------
/**
 * This method sets the j2000 body for the CoordinateConverter object.
 *
 * @param toBody body pointer to which to set the j2000 body parameter.
 */
//------------------------------------------------------------------------------
//void CoordinateConverter::SetJ2000Body(SpacePoint *toBody)
//{
//   j2000Body = toBody;
//}

//------------------------------------------------------------------------------
//  SpacePoint*  GetJ2000Body() const
//------------------------------------------------------------------------------
/**
 * This method returns the j2000 body pointer for the CoordinateConverter 
 * object.
 *
 * @return j2000 body pointer.
 */
//------------------------------------------------------------------------------
//SpacePoint* CoordinateConverter::GetJ2000Body()
//{
//   return j2000Body;
//}


//------------------------------------------------------------------------------
//  bool  Convert(const A1Mjd &epoch, const Rvector &inState,
//                CoordinateSystem *inCoord, Rvector &outState,
//                CoordinateSystem *outCoord, 
//                bool forceComputation, bool omitTranslation) 
//------------------------------------------------------------------------------
/**
 * This method converts the inState vector from coordIn CoordinateSystem
 * to the coordOut CoordinateSystem, at the input epoch, and returns the
 * result in outState.
 *
 * @param epoch    time for which to do the conversion.
 * @param inState  input State (in inCoord system).
 * @param inCoord  pointer to the input CoordinateSystem.
 * @param outState resulting vector, in the outCoord system.
 * @param outCoord pointer to the output CoordinateSystem.
 * @param omitTranslation omit the translation whether coincident or not
 *                        (default is false)
 *
 * @return true if successful; false otherwise.
 */
//------------------------------------------------------------------------------
bool CoordinateConverter::Convert(const A1Mjd &epoch, const Rvector &inState,
                          CoordinateSystem *inCoord, Rvector &outState,
                          CoordinateSystem *outCoord, 
                          bool forceComputation, bool omitTranslation)
{
   const Real *in = inState.GetDataVector();
   Real *out = new Real[outState.GetSize()];

   #ifdef DEBUG_TO_FROM
   MessageInterface::ShowMessage("in: %f %f %f %f %f %f %f\n", in[0], in[1], in[2], in[3], in[4], in[5]);
   MessageInterface::ShowMessage("out: %f %f %f %f %f %f %f\n", out[0], out[1], out[2], out[3], out[4], out[5]);
   #endif
   
   if (Convert(epoch, in, inCoord, out, outCoord, forceComputation, omitTranslation))
   {
      outState.Set(out);
      delete [] out;
      return true;
   }
   
   delete [] out;
   return false;

}


//------------------------------------------------------------------------------
// bool Convert(const A1Mjd &epoch, const Real *inState,
//              CoordinateSystem *inCoord, Real *outState,
//              CoordinateSystem *outCoord, bool forceComputation = false,
//              bool omitTranslation = false)
//------------------------------------------------------------------------------
bool CoordinateConverter::Convert(const A1Mjd &epoch, const Real *inState,
                          CoordinateSystem *inCoord, Real *outState,
                          CoordinateSystem *outCoord, 
                          bool forceComputation, bool omitTranslation)
{
   if ((!inCoord) || (!outCoord))										// made changes by TUAN NGUYEN
      throw CoordinateSystemException(									// made changes by TUAN NGUYEN
         "Undefined coordinate system - conversion not performed.");	// made changes by TUAN NGUYEN

//   if (epoch.Get() < 10.0)
//   {
//      Real *epochCatcher = NULL;
//      (*epochCatcher) = inState[0];
//   }
   
   #ifdef DEBUG_TO_FROM
      MessageInterface::ShowMessage
         ("In Convert, inCoord is %s(%p) and outCoord is %s(%p)\n",
         (inCoord->GetName()).c_str(), inCoord, (outCoord->GetName()).c_str(),
          outCoord);
      MessageInterface::ShowMessage
         ("   forceComputation=%d, omitTranslation=%d\n", forceComputation,
          omitTranslation);
   #endif
   if (inCoord->GetName() == outCoord->GetName())
   {
      // assuming state is size 6 here!!!
      for (Integer i=0;i<6;i++) outState[i] = inState[i];
      lastRotMatrix.Set(1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0);
      return true;
   }
   
   #ifdef DEBUG_FIRST_CALL
      if ((firstCallFired == false) || (epoch.Get() == GmatTimeConstants::MJD_OF_J2000))
      {
         MessageInterface::ShowMessage(
            "Coordinate conversion check:\n   %s --> %s\n", 
            inCoord->GetName().c_str(), outCoord->GetName().c_str());
         MessageInterface::ShowMessage(
            "   Epoch: %.12lf\n", epoch.Get());
         MessageInterface::ShowMessage(
            "   input State    = [%.10lf %.10lf %.10lf %.16lf %.16lf %.16lf]\n",
            inState[0], inState[1], inState[2], inState[3], inState[4], 
            inState[5]);
      }
   #endif

//   if ((!inCoord) || (!outCoord))										// made changes by TUAN NGUYEN
//      throw CoordinateSystemException(								// made changes by TUAN NGUYEN
//         "Undefined coordinate system - conversion not performed.");	// made changes by TUAN NGUYEN
   
   #ifdef DEBUG_TO_FROM
   MessageInterface::ShowMessage
      ("   inCoord->GetOrigin=%s(%p), outCoord->GetOrigin=%s(%p)\n",
       inCoord->GetOrigin()->GetName().c_str(), inCoord->GetOrigin(),
       outCoord->GetOrigin()->GetName().c_str(), outCoord->GetOrigin());
   #endif
   
   // call coordinate system methods to convert - allow exceptions to
   // percolate up (to be caught at a higher level)
   bool sameOrigin = (inCoord->GetOrigin() == outCoord->GetOrigin() ? 
                      true : false);
   
   bool coincident = sameOrigin || omitTranslation;
   Real intState[6];
   
   #ifdef DEBUG_TO_FROM
   MessageInterface::ShowMessage
      ("   sameOrigin=%d, omitTranslation=%d, coincident=%d\n",
       sameOrigin, omitTranslation, coincident);
   #endif
   
   inCoord->ToBaseSystem(epoch, inState, intState, coincident, forceComputation);
   #ifdef DEBUG_TO_FROM
      MessageInterface::ShowMessage
         ("In Convert, sameOrigin is %s,and coincident is %s\n",
         (sameOrigin? "TRUE" : "FALSE"),
         (coincident? "TRUE" : "FALSE"));
      MessageInterface::ShowMessage("inState = %18.10f   %18.10f   %18.10f\n",
            inState[0], inState[1], inState[2]);
      MessageInterface::ShowMessage("          %18.10f   %18.10f   %18.10f\n",
            inState[3], inState[4], inState[5]);
      MessageInterface::ShowMessage("intState = %18.10f   %18.10f   %18.10f\n",
            intState[0], intState[1], intState[2]);
      MessageInterface::ShowMessage("          %18.10f   %18.10f   %18.10f\n",
            intState[3], intState[4], intState[5]);
   #endif

   std::string inBaseName  = inCoord->GetBaseSystem();
   std::string outBaseName = outCoord->GetBaseSystem();
   #ifdef DEBUG_BASE_SYSTEM
      MessageInterface::ShowMessage("inBase system  = %s\n", inBaseName.c_str());
      MessageInterface::ShowMessage("outBase system = %s\n", outBaseName.c_str());
   #endif
   Real baseState[6];

   if (inBaseName != outBaseName)
   {
      ConvertFromBaseToBase(epoch, inBaseName, outBaseName, intState, baseState); // need Real* version of this method			// made changes by TUAN NGUYEN
      MessageInterface::ShowMessage("Conversion from base system %s to base system %s = this should not happen yet\n",
            inBaseName.c_str(), outBaseName.c_str());
   }
   else
   {
      for (unsigned int ii = 0; ii < 6; ii++)
         baseState[ii] = intState[ii];
   }
   #ifdef DEBUG_BASE_SYSTEM
      MessageInterface::ShowMessage("baseState = %18.10f   %18.10f   %18.10f\n",
            baseState[0], baseState[1], baseState[2]);
      MessageInterface::ShowMessage("          %18.10f   %18.10f   %18.10f\n",
            baseState[3], baseState[4], baseState[5]);
      MessageInterface::ShowMessage(" ... about to call outCoord->FromBaseSystem ... outCoord is %sNULL\n",
            (outCoord? "NOT " : ""));
      MessageInterface::ShowMessage("epoch = %le, coincident = %s, forceComputation = %s\n", epoch.Get(),
            (coincident? "true" : "false"), (forceComputation? "true" : "false"));
   #endif

   outCoord->FromBaseSystem(epoch, baseState, outState, coincident, forceComputation);
   #ifdef DEBUG_TO_FROM
      MessageInterface::ShowMessage("outState = %18.10f   %18.10f   %18.10f\n",
            outState[0], outState[1], outState[2]);
      MessageInterface::ShowMessage("          %18.10f   %18.10f   %18.10f\n",
            outState[3], outState[4], outState[5]);
   #endif
   
   // last rotation matrix
   // R1 * R2T
   Real R1[9];
   Real R2[9];
   Real lrm[3][3];
   Integer p3;

   inCoord->GetLastRotationMatrix(R1);
   outCoord->GetLastRotationMatrix(R2);
   Real  R2T[9] = {R2[0], R2[3], R2[6],
                   R2[1], R2[4], R2[7],
                   R2[2], R2[5], R2[8]};
   
   for (Integer p = 0; p < 3; ++p)
   {
      p3 = 3*p;
      for (Integer q = 0; q < 3; ++q)
      {
         lrm[p][q] = R2T[p3]   * R1[q]   +
                     R2T[p3+1] * R1[q+3] +
                     R2T[p3+2] * R1[q+6];
      }
   }
   lastRotMatrix.Set(lrm[0][0],lrm[0][1],lrm[0][2],
                     lrm[1][0],lrm[1][1],lrm[1][2],
                     lrm[2][0],lrm[2][1],lrm[2][2]);
 

   // last rotation Dot matrix
   // R1*R2Dot + R1dot*R2
   Real R1dot[9];
   Real R2dot[9];

   inCoord->GetLastRotationDotMatrix(R1dot);
   outCoord->GetLastRotationDotMatrix(R2dot);

   #ifdef DEBUG_ROT_DOT_MATRIX
      MessageInterface::ShowMessage("R1dot = %le %le %le %le %le %le %le %le %le\n",
            R1dot[0],R1dot[1],R1dot[2],R1dot[3],R1dot[4],R1dot[5],R1dot[6],R1dot[7],R1dot[8]);
      MessageInterface::ShowMessage("R2dot = %le %le %le %le %le %le %le %le %le\n",
            R2dot[0],R2dot[1],R2dot[2],R2dot[3],R2dot[4],R2dot[5],
            R2dot[6],R2dot[7],R2dot[8]);
   #endif
   Real  R2dotTR1[3][3];
   Real  R2TR1dot[3][3];
   Real  R2dotT[9] = {R2dot[0], R2dot[3], R2dot[6],
                      R2dot[1], R2dot[4], R2dot[7],
                      R2dot[2], R2dot[5], R2dot[8]};
   for (Integer p = 0; p < 3; ++p)
   {
      p3 = 3*p;
      for (Integer q = 0; q < 3; ++q)
      {
         R2dotTR1[p][q] = R2dotT[p3]   * R1[q]   +
                          R2dotT[p3+1] * R1[q+3] +
                          R2dotT[p3+2] * R1[q+6];
      }
   }
   for (Integer p = 0; p < 3; ++p)
   {
      p3 = 3*p;
      for (Integer q = 0; q < 3; ++q)
      {
         R2TR1dot[p][q] = R2T[p3]   * R1dot[q]   +
                          R2T[p3+1] * R1dot[q+3] +
                          R2T[p3+2] * R1dot[q+6];
      }
   }
   lastRotDotMatrix.Set(R2dotTR1[0][0] + R2TR1dot[0][0],   R2dotTR1[0][1] + R2TR1dot[0][1],   R2dotTR1[0][2] + R2TR1dot[0][2],
                        R2dotTR1[1][0] + R2TR1dot[1][0],   R2dotTR1[1][1] + R2TR1dot[1][1],   R2dotTR1[1][2] + R2TR1dot[1][2],
                        R2dotTR1[2][0] + R2TR1dot[2][0],   R2dotTR1[2][1] + R2TR1dot[2][1],   R2dotTR1[2][2] + R2TR1dot[2][2]);

   #ifdef DEBUG_FIRST_CALL
      if ((firstCallFired == false) || (epoch.Get() == GmatTimeConstants::MJD_OF_J2000))
      {
         MessageInterface::ShowMessage(
            "   internal State = [%.10lf %.10lf %.10lf %.16lf %.16lf %.16lf]\n",
            intState[0], intState[1], intState[2], intState[3], 
            intState[4], intState[5]);
         MessageInterface::ShowMessage(
            "   output State   = [%.10lf %.10lf %.10lf %.16lf %.16lf %.16lf]\n",
            outState[0], outState[1], outState[2], outState[3], outState[4], 
            outState[5]);
         firstCallFired = true;
      }
   #endif
   
   return true;
}


//------------------------------------------------------------------------------
// Rmatrix33 CoordinateConverter::GetLastRotationMatrix() const
//------------------------------------------------------------------------------
Rmatrix33 CoordinateConverter::GetLastRotationMatrix() const
{
   return lastRotMatrix;
}

//------------------------------------------------------------------------------
// Rmatrix33 CoordinateConverter::GetLastRotationDotMatrix() const
//------------------------------------------------------------------------------
Rmatrix33 CoordinateConverter::GetLastRotationDotMatrix() const
{
   return lastRotDotMatrix;
}


//------------------------------------------------------------------------------
// bool CoordinateConverter::ConvertFromBaseToBase(const A1Mjd &epoch,		// made changes by TUAN NGUYEN
//          const std::string &inBase,  const std::string &outBase,
//          const Rvector &inBaseState, Rvector &outBaseState)
//------------------------------------------------------------------------------
bool CoordinateConverter::ConvertFromBaseToBase(const A1Mjd &epoch,		// made changes by TUAN NGUYEN
		const std::string &inBase,  const std::string &outBase,
		const Rvector &inBaseState, Rvector &outBaseState)
{
   // if the base types are the same, just set the output state to the input state
   if (inBase == outBase)
   {
      if (inBaseState.GetSize() != outBaseState.GetSize())
      {
         std::string errmsg = "Cannot convert from coordinate system base type ";
         errmsg += inBase + " to base type ";
         errmsg += outBase + " - state vectors passed in are of differing sizes.\n";
         throw CoordinateSystemException(errmsg);
      }
      for (Integer ii = 0; ii < inBaseState.GetSize(); ii++)
      {
         outBaseState[ii] = inBaseState[ii];
      }
      return true;
   }
   const Real *in  = inBaseState.GetDataVector();
   Real       *out = new Real[outBaseState.GetSize()];

   if (ConvertFromBaseToBase(epoch, inBase, outBase, in, out))		// made changes by TUAN NGUYEN
   {
      outBaseState.Set(out);
      delete [] out;
      return true;
   }

   delete [] out;
   return false;
}


bool CoordinateConverter::ConvertFromBaseToBase(const A1Mjd &epoch,		// made changes by TUAN NGUYEN
		const std::string &inBase,  const std::string &outBase,
        const Real *inBaseState,    Real *outBaseState)
{
   // if the base types are the same, just set the output state to the input state
   if (inBase == outBase)
   {
      for (Integer i=0;i<6;i++) outBaseState[i] = inBaseState[i];
      return true;
   }

   // Use the matrix from ICRF to FK5
   Real iToF[9];  // @todo - this must be computed or set- specs TBD ********************** TBD *********************
   ICRFAxes* icrf = new ICRFAxes();
   icrf->Initialize();
   icrf->GetRotationMatrix(epoch, true);
   icrf->GetLastRotationMatrix(&iToF[0]);

   delete icrf;

//   iToF[0] = 1.0;
//   iToF[1] = 0.0;
//   iToF[2] = 0.0;
//   iToF[3] = 0.0;
//   iToF[4] = 1.0;
//   iToF[5] = 0.0;
//   iToF[6] = 0.0;
//   iToF[7] = 0.0;
//   iToF[8] = 1.0;

   if ((inBase == "ICRF") && (outBase == "FK5"))
   {
      outBaseState[0] = iToF[0]*inBaseState[0] + iToF[1]*inBaseState[1] + iToF[2]*inBaseState[2];
      outBaseState[1] = iToF[3]*inBaseState[0] + iToF[4]*inBaseState[1] + iToF[5]*inBaseState[2];
      outBaseState[2] = iToF[6]*inBaseState[0] + iToF[7]*inBaseState[1] + iToF[8]*inBaseState[2];

      outBaseState[3] = iToF[0]*inBaseState[3] + iToF[1]*inBaseState[4] + iToF[2]*inBaseState[5];
      outBaseState[4] = iToF[3]*inBaseState[3] + iToF[4]*inBaseState[4] + iToF[5]*inBaseState[5];
      outBaseState[5] = iToF[6]*inBaseState[3] + iToF[7]*inBaseState[4] + iToF[8]*inBaseState[5];
   }
   else if ((inBase == "FK5") && (outBase == "ICRF"))
   {
      // Transpose the conversion matrix and multiply
      outBaseState[0] = iToF[0]*inBaseState[0] + iToF[3]*inBaseState[1] + iToF[6]*inBaseState[2];
      outBaseState[1] = iToF[1]*inBaseState[0] + iToF[4]*inBaseState[1] + iToF[7]*inBaseState[2];
      outBaseState[2] = iToF[2]*inBaseState[0] + iToF[5]*inBaseState[1] + iToF[8]*inBaseState[2];

      outBaseState[3] = iToF[0]*inBaseState[3] + iToF[3]*inBaseState[4] + iToF[6]*inBaseState[5];
      outBaseState[4] = iToF[1]*inBaseState[3] + iToF[4]*inBaseState[4] + iToF[7]*inBaseState[5];
      outBaseState[5] = iToF[2]*inBaseState[3] + iToF[5]*inBaseState[4] + iToF[8]*inBaseState[5];
   }
   else
   {
      std::string errmsg = "Cannot convert from coordinate system base type ";
      errmsg += inBase + " to base type ";
      errmsg += outBase + " - unknown base type.\n";
      throw CoordinateSystemException(errmsg);
   }

   return true;
}

