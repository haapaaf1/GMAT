//$Header$
//------------------------------------------------------------------------------
//                                  Linear
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Matthew Weippert, Chu-chi Li, E. Corderman
// Created: 1995/07/21 for GSS project
// Modified: 2003/09/16 Linda Jun - See Linear.hpp
//
/**
 * Defines Linear Algebra conversion, Linear I/O, and Linear Math operations.
 */
//------------------------------------------------------------------------------
#include <fstream>
#include <iostream>
#include <iomanip>
#include "RealTypes.hpp"
#include "RealUtilities.hpp" // for PI, TWO_PI, Acos(), Atan()
#include "Rvector.hpp"
#include "Rvector3.hpp"
#include "Rmatrix.hpp"
#include "Linear.hpp"

using namespace GmatRealUtil;
using namespace GmatMathUtil;

//---------------------------------
//  pubic
//---------------------------------

//------------------------------------------------------------------------------
//  GmatRealUtil::RaCodec GmatRealUtil::CartesianToRaCodec(const Rvector3 &r)
//------------------------------------------------------------------------------
/**
 * @exception throws RealUtilitiesExceptions::ArgumentError if all three
 *            Cartesian coordinates to be converted are 0
 */
//------------------------------------------------------------------------------
GmatRealUtil::RaCodec GmatRealUtil::CartesianToRaCodec(const Rvector3 &r)
{
    RaCodec s;
    if ( (r[0] == 0.0) && (r[1] == 0.0) ) 
    {
        if (r[2] == 0.0) 
        {
            throw RealUtilitiesExceptions::ArgumentError();
        } 
        else if( r[2]<0.0 ) 
        {
            s.radiusD = -r[2];
            s.coDeclinationD = GmatMathUtil::PI;
            s.rightAscensionD = 0.0;
        } 
        else if( r[2]>0.0) 
        {
            s.radiusD = r[2];
            s.coDeclinationD = 0.0;
            s.rightAscensionD =0.0;
        }
    } 
    else
    {
        s.radiusD = r.GetMagnitude();
        s.coDeclinationD = ACos(r[2] / s.radiusD);
        s.rightAscensionD = ATan(r[1],r[0]);
    }
    return s;
}

//------------------------------------------------------------------------------
//  GmatRealUtil::RaDec GmatRealUtil::CartesianToRaDec(const Rvector3 &r)
//------------------------------------------------------------------------------
/**
 * @exception throws RealUtilitiesExceptions::ArgumentError if all three
 *            Cartesian coordinates to be converted are 0
 */
//------------------------------------------------------------------------------
GmatRealUtil::RaDec GmatRealUtil::CartesianToRaDec(const Rvector3 &r)
{
    RaDec RD;

    if ( r[0] == 0.0 && r[1] == 0.0) 
    {
        if(r[2] == 0.0) 
        {
            throw RealUtilitiesExceptions::ArgumentError();
        } 
        else if(r[2]<0.0) 
        {
            RD.radiusD = -r[2];
            RD.rightAscensionD = 0.0;
            RD.declinationD = -GmatMathUtil::PI_OVER_TWO;
        } 
        else if(r[2]>0.0) 
        {
            RD.radiusD = r[2];
            RD.rightAscensionD = 0.0;
            RD.declinationD  = GmatMathUtil::PI_OVER_TWO;
        }
    } 
    else 
    {
        RD.radiusD = r.GetMagnitude();
        RD.rightAscensionD = ATan(r[1],r[0]);
        RD.declinationD = ASin(r[2]/RD.radiusD);
    }
    return RD;
}

//------------------------------------------------------------------------------
//  Rvector3 GmatRealUtil::RaCodecToCartesian(const RaCodec &r)
//------------------------------------------------------------------------------
Rvector3 GmatRealUtil::RaCodecToCartesian(const RaCodec &r)
{
    Rvector3 v;
    v[0] = r.radiusD * Sin(r.coDeclinationD) * Cos(r.rightAscensionD);
    v[1] = r.radiusD * Sin(r.coDeclinationD) * Sin(r.rightAscensionD);
    v[2] = r.radiusD * Cos(r.coDeclinationD);
    return v;
}

//------------------------------------------------------------------------------
//  GmatRealUtil::RaDec GmatRealUtil::RaCodecToRaDec(const RaCodec &r)
//------------------------------------------------------------------------------
GmatRealUtil::RaDec GmatRealUtil::RaCodecToRaDec(const RaCodec &r)
{
    RaDec rD;
    rD.radiusD = r.radiusD;
    rD.rightAscensionD = r.rightAscensionD;
    rD.declinationD = GmatMathUtil::PI_OVER_TWO - r.coDeclinationD;
    return rD;
}

//------------------------------------------------------------------------------
//  Rvector3 GmatRealUtil::RaDecToCartesian(const RaDec &r)
//------------------------------------------------------------------------------
Rvector3 GmatRealUtil::RaDecToCartesian(const RaDec &r)
{
    Rvector3 v;
    v[0] = r.radiusD * Cos(r.rightAscensionD) * Cos(r.declinationD);
    v[1] = r.radiusD * Sin(r.rightAscensionD) * Cos(r.declinationD);
    v[2] = r.radiusD * Sin(r.declinationD);
    return v;
}

//------------------------------------------------------------------------------
//  GmatRealUtil::RaCodec GmatRealUtil::RaDecToRaCodec(const RaDec &r)
//------------------------------------------------------------------------------
GmatRealUtil::RaCodec GmatRealUtil::RaDecToRaCodec(const RaDec &r)
{
    RaCodec s;
    s.radiusD = r.radiusD;
    s.rightAscensionD = r.rightAscensionD;
    s.coDeclinationD = GmatMathUtil::PI_OVER_TWO - r.declinationD;
    return s;
}

//------------------------------------------------------------------------------
//  Real GmatRealUtil::Min(const Rvector &numbers)
//------------------------------------------------------------------------------
Real GmatRealUtil::Min(const Rvector &numbers)
{
    int i;
    int end = numbers.GetSize();
    Real smallest = numbers[0];
    
    for ( i = 1; i<end;i++) 
    {
        if (numbers[i]<smallest) 
        {
            smallest = numbers[i];
        }
    }
    return smallest;
}

//------------------------------------------------------------------------------
//  Real GmatRealUtil::Max(const Rvector &numbers)
//------------------------------------------------------------------------------
Real GmatRealUtil::Max(const Rvector &numbers)
{
    int i;
    int end = numbers.GetSize();
    Real biggest = numbers[0];

    for ( i = 1; i<end; i++) 
    {
        if (numbers[i]>biggest) 
        {
            biggest = numbers[i];
        }
    }
    return biggest;
}

struct LinearIODefaults 
{
    static const int  spacing = 4;
    static const bool horizontal = false;
    static const bool binaryIn = false;
    static const bool binaryOut = false;
};

//  const int  LinearIODefaults::spacing    = 4;
//  const bool LinearIODefaults::horizontal = false;
//  const bool LinearIODefaults::binaryIn   = false;
//  const bool LinearIODefaults::binaryOut  = false;

static int  spacing    = 4;     //determines number of spaces in between
                                //  each element
static bool horizontal = false; //print horizontally if true. Default is false
static bool binaryIn   = false; //read in binary if true. Default is false
static bool binaryOut  = false; //print in binary if true. Default is false

//------------------------------------------------------------------------------
//  std::ostream& SetSpacing(std::ostream& outFile, int i) 
//------------------------------------------------------------------------------
/**
 * Modifies the variable spacing
 *
 * @param <std::ostream&> output stream
 * @param <i> the value to set spacing with
 * @return output stream
 */
//------------------------------------------------------------------------------
//  std::ostream& SetSpacing(std::ostream &outFile, int i)
//  {
//      spacing = i;
//      return outFile;
//  }

//------------------------------------------------------------------------------
//  OMANIP(int) SetSpacing(int i) 
//------------------------------------------------------------------------------
//  std::OMANIP(int)(int)
//  SetSpacing(int i) 
//  {
//      return OMANIP(int)(&SetSpacing,i);
//  }

//------------------------------------------------------------------------------
//  std::ostream &SetHorizontal(std::ostream &outFile, int h) 
//------------------------------------------------------------------------------
/**
 * Modifies the variable indicating horizontal or vertical output
 *
 * @param <std::ostream&> output stream
 * @param <h> the value to set horizontal/vertical flag
 * @return output stream
 */
//------------------------------------------------------------------------------
//  std::ostream& SetHorizontal(std::ostream &outFile, bool h) 
//  {
//      horizontal = h;
//      return outFile;
//  }

//------------------------------------------------------------------------------
//  OMANIP(int) SetHorizontal(bool h)
//------------------------------------------------------------------------------
//  OMANIP(int)
//  SetHorizontal(bool h)
//  {
//      return OMANIP(int)(&SetHorizontal,(int)h);
//  }

//------------------------------------------------------------------------------
//  std::istream& SetBinaryIn(std::istream &inFile, int b) 
//
//  Description: modifies the variable binaryIn, setting input to binary or
//               non-binary
//
//  Arguments:
//  <std::istream&>   <-  input stream
//  <b>          <-  the value to set input binary/non-binary flag with
//
//  Returns: <std::istream&> input stream
// 
//------------------------------------------------------------------------------
//  std::istream& SetBinaryIn(std::istream &inFile, int b) 
//  {
//      binaryIn = (bool) b;
//      return inFile;
//  }

//------------------------------------------------------------------------------
//  IMANIP(int) SetBinaryIn(bool b)
//------------------------------------------------------------------------------
//  IMANIP(int)
//  SetBinaryIn(bool b)
//  {
//      return IMANIP(int)(&SetBinaryIn,(int)b);
//  }

//------------------------------------------------------------------------------
//  <"private">
//  std::ostream& SetBinaryOut(std::ostream &outFile, int b) 
//
//  Description: modifies the variable binaryOut, setting output to binary or
//               non-binary
//
//  Arguments:
//  <std::ostream&>   <-  output stream
//  <b>          <-  the value to set output binary/non-binary flag with
//
//  Returns: <std::ostream&>  output stream
//------------------------------------------------------------------------------
//  std::ostream& SetBinaryOut(std::ostream &outFile, int b) 
//  {
//      binaryOut = (bool) b;
//      return outFile;
//  }

//------------------------------------------------------------------------------
//  OMANIP(int) SetBinaryOut(bool b)
//------------------------------------------------------------------------------
//  OMANIP(int)
//  SetBinaryOut(bool b)
//  {
//      return OMANIP(int)(&SetBinaryOut,(int)b);
//  }

//------------------------------------------------------------------------------
//  int GmatRealUtil::GetSpacing() 
//------------------------------------------------------------------------------
int GmatRealUtil::GetSpacing()
{
    return spacing;
}

//------------------------------------------------------------------------------
// bool GmatRealUtil::IsHorizontal() 
//------------------------------------------------------------------------------
bool GmatRealUtil::IsHorizontal() 
{
    return horizontal;
}

//------------------------------------------------------------------------------
//  bool GmatRealUtil::IsBinaryIn() 
//------------------------------------------------------------------------------
bool GmatRealUtil::IsBinaryIn() 
{
    return binaryIn;
}

//------------------------------------------------------------------------------
//  bool GmatRealUtil::IsBinaryOut() 
//------------------------------------------------------------------------------
bool GmatRealUtil::IsBinaryOut() 
{
    return binaryOut;
}

//------------------------------------------------------------------------------
//  std::istream& GmatRealUtil::operator>> (std::istream &input, Rvector &a) 
//------------------------------------------------------------------------------
std::istream& GmatRealUtil::operator>> (std::istream &input, Rvector &a) 
{
    int size = a.GetSize();
    int i;
   
    if (binaryIn)
    {
        for (i=0; i<size; i++)  
        {
            input.read((char*)&a[i], sizeof(Real));
        }
    }
    else
    {
        for (i=0; i<size; i++)  
        {
            input>>a[i];
        }
    }
    binaryIn = LinearIODefaults::binaryIn;
    return input;
}

//------------------------------------------------------------------------------
//  std::ostream& GmatRealUtil::operator<< (std::ostream &output, const Rvector &a) 
//------------------------------------------------------------------------------
/**
 * @note Resets format to default.
 */
//------------------------------------------------------------------------------
std::ostream& GmatRealUtil::operator<< (std::ostream &output, const Rvector &a) 
{
    int size = a.GetSize();
    int i;
   
    if (binaryOut)
    {
        for (i=0; i<size; i++)  
        {
            output.write((char*)&a[i], sizeof(Real));
        }
    }
    else
    {
        if (horizontal) 
        {
            char *spaces = new char[spacing + 1];
            for (i=0; i<spacing; i++) 
            {
                spaces[i] = ' ';
            }
            spaces[spacing] = '\0';
            for (i=0; i<size; i++) 
            {
                output << a[i] << spaces;
            }
            output << std::endl;
        } 
        else 
        {
            for (i=0; i<size; i++) 
            {
                output << a[i] << std::endl;
            }
        }
    }

    spacing = LinearIODefaults::spacing;
    horizontal = LinearIODefaults::horizontal;
    binaryOut = LinearIODefaults::binaryOut;
    return output;
}

//------------------------------------------------------------------------------
//  std::istream& GmatRealUtil::operator>> (std::istream &input, Rmatrix &a) 
//------------------------------------------------------------------------------
std::istream& GmatRealUtil::operator>> (std::istream &input, Rmatrix &a) 
{
    int row = a.GetNumRows();
    int column = a.GetNumColumns();
    int i,j;

    if (binaryIn)
    {
        for (i=0; i<row; i++) 
        {
            for (j=0; j<column; j++) 
            {
                input.read((char*)&a(i,j), sizeof(Real));
            }
        }
    }
    else
    {
        for (i=0; i<row; i++) 
        {
            for (j=0; j<column; j++) 
            {
                input >> a(i,j);
            }
        }
    }
    binaryIn = LinearIODefaults::binaryIn;
    return input;
}

//------------------------------------------------------------------------------
//  std::ostream& GmatRealUtil::operator<< (std::ostream &output, const Rmatrix &a) 
//------------------------------------------------------------------------------
/**
 * @note Resets format to default.
 */
//------------------------------------------------------------------------------
std::ostream& GmatRealUtil::operator<< (std::ostream &output, const Rmatrix &a) 
{
    int row = a.GetNumRows();
    int column = a.GetNumColumns();
    int i,j;
    char *spaces = new char[spacing + 1];

    if (binaryOut)
    {
        for (i=0; i<row; i++) 
        {
            for (j=0; j<column; j++) 
            {
                output.write((char*)&a(i,j), sizeof(Real));
            }
        }
    }
    else
    {
        for(i=0; i<spacing; i++) 
        {
            spaces[i] = ' ';
        }
        spaces[spacing] = '\0';
      
        if (horizontal) 
        {
            for (i=0; i<row; i++) 
            {
                for (j=0; j<column; j++) 
                {
                    output << a.GetElement(i,j) << spaces;
                }
            }
        }   
        else 
        {
            for (i=0; i<row; i++) 
            {
                for (j=0; j<column; j++) 
                {
                    output << a.GetElement(i,j) << spaces;
                }
                output << std::endl;
            }
        }
    }
    spacing = LinearIODefaults::spacing;
    horizontal = LinearIODefaults::horizontal;
    binaryOut = LinearIODefaults::binaryOut;
    return output;
}
