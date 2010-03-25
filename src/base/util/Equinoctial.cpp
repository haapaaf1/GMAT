
//$Id:
//------------------------------------------------------------------------------
//                         Equinoctial
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Daniel Hunter/GSFC; Updates to match updated math specs: Wendy Shoan/GSFC
// Created: 2006.06.23; 2010.03.15
//
/**
 * Implementation for the Equinoctial class.
 */
//------------------------------------------------------------------------------

#include "Equinoctial.hpp"
#include "UtilityException.hpp"
#include "RealTypes.hpp"
#include "RealUtilities.hpp"
#include "CoordUtil.hpp"
#include "UtilityException.hpp"
#include "MessageInterface.hpp"

using namespace GmatMathUtil;

#define DEBUG_EQUINOCTIAL

//------------------------------------------------------------------------------
// static data
//------------------------------------------------------------------------------
const std::string Equinoctial::DATA_DESCRIPTIONS[NUM_DATA] =
{
	"SemiMajor",
	"Projection of eccentricity onto y_ep axis",
	"Projection of eccentricity onto x_ep axis",
	"Projection of N onto y_ep axis",
	"Projection of N onto x_ep axis",
	"Mean Longitude"
};

const Real Equinoctial::EQ_TOLERANCE = 1.0e-10;


//------------------------------------------------------------------------------
// public methods
//------------------------------------------------------------------------------
Equinoctial::Equinoctial() :
   sma(0.0),
   h(0.0),
   k(0.0),
   p(0.0),
   q(0.0),
   lambda(0.0)
{
}

Equinoctial::Equinoctial(const Rvector6& state) :
   sma(state[0]),
   h(state[1]),
   k(state[2]),
   p(state[3]),
   q(state[4]),
   lambda(state[5])
{
}

Equinoctial::Equinoctial(const Real& a, const Real& pEY, const Real& pEX, const Real& pNY, const Real& pNX, const Real& ml) :
	sma(a),
	h(pEY),
	k(pEX),
	p(pNY),
	q(pNX),
	lambda(ml)
{
}

Equinoctial::Equinoctial(const Equinoctial &eq) :
	sma(eq.sma),
	h(eq.h),
	k(eq.k),
	p(eq.p),
	q(eq.q),
	lambda(eq.lambda)
{
}

Equinoctial& Equinoctial::operator=(const Equinoctial &eq)
{
	if (this != &eq)
	{
	   sma    = eq.sma;
	   h      = eq.h;
	   k      = eq.k;
	   p      = eq.p;
	   q      = eq.q;
	   lambda = eq.lambda;
	}
	return *this;
}
	  
	
Equinoctial::~Equinoctial()
{
}

//  Friend functions
std::ostream& operator<<(std::ostream& output, const Equinoctial& eq)
{
    Rvector v(6, eq.sma, eq.h, eq.k, eq.p, eq.q, eq.lambda);

    output << v << std::endl;

    return output;
}

std::istream& operator>>(std::istream& input, Equinoctial& eq) {
    input >> eq.sma >> eq.h
    	  >> eq.k >> eq.p
    	  >> eq.q >> eq.lambda;

    return input;
}

Rvector6 CartesianToEquinoctial(const Rvector6& cartesian, const Real& mu)
{
   Real sma, h, k, p, q, lambda; // equinoctial elements

	Rvector3 pos(cartesian[0], cartesian[1], cartesian[2]);
	Rvector3 vel(cartesian[3], cartesian[4], cartesian[5]);
	Real r = pos.GetMagnitude();
	Real v = vel.GetMagnitude();

	Rvector3 eVec = ( ((v*v - mu/r) * pos) - ((pos * vel) * vel) ) / mu;
	Real e = eVec.GetMagnitude();

	// Check for a near parabolic orbit
	if (Abs(1.0 - e) < 1.0e-7)
	{
      #ifdef DEBUG_EQUINOCTIAL
         MessageInterface::ShowMessage("Equinoctial ... failing check for parabolic orbit  ... e = %12.10f\n",
               e);
      #endif
	   std::string errmsg =
	         "Conversion to equinoctial elements cannot be completed.  Orbit is nearly parabolic.\n";
	   throw UtilityException(errmsg);
	}

	Real xi  = (v * v / 2.0) - (mu / r);
	sma      = - mu / (2.0 * xi);

	// Check to see if the conic section is nearly singular
	if (Abs(sma * (1.0 - e)) < .001)
	{
      #ifdef DEBUG_EQUINOCTIAL
         MessageInterface::ShowMessage(
               "Equinoctial ... failing check for singular conic section ... e = %12.10f, sma = %12,10f\n",
               e, sma);
      #endif
	   std::string errmsg =
	         "Conversion to equinoctial elements cannot be completed,  The conic section is nearly singular.\n";
	   throw UtilityException(errmsg);
	}

   Rvector3 am = Cross(pos, vel).GetUnitVector();
	
	Integer j = 1;
	if (am[2] <= 0.0) j = -1;   // retrograde orbit
	
	// Define equinoctial coordinate system
	Rvector3 f;
	f[0]      =   1.0 - ((am[0] * am[0]) / (1.0 + Pow(am[2], j)));
	f[1]      = - (am[0] * am[1]) / (1.0 + Pow(am[2], j));
	f[2]      = - Pow(am[0], j);
	f         = f.GetUnitVector();

	Rvector3 g = Cross(am,f).GetUnitVector();

	h = eVec * g;
	k = eVec * f;
	p = am[0] / (1.0 + Pow(am[2], j));
	q = - am[1] / (1.0 + Pow(am[2], j));
	
	// Calculate mean longitude
	// First, calculate true longitude
	Real X1      = pos * f;
	Real Y1      = pos * g;
	Real tmpSqrt = Sqrt(1.0 - (h * h) - (k * k));
	Real beta    = 1.0 / (1.0 + tmpSqrt);
	Real cosF    = k + ((1.0 - k*k*beta) * X1 - (h * k * beta * Y1)) /
	               (sma * tmpSqrt);
	Real sinF    = h + ((1.0 - h * h * beta) * Y1 - (h * k * beta * X1)) /
	               (sma * tmpSqrt);
	Real F       = ATan2(sinF, cosF);
	// limit F to a positive value
	while (F < 0.0) F += TWO_PI;
	lambda       = (F + (h * cosF) - (k * sinF)) * DEG_PER_RAD;

	return Rvector6(sma, h, k, p, q, lambda);
}

Rvector6 EquinoctialToCartesian(const Rvector6& equinoctial, const Real& mu)
{
   Real sma    = equinoctial[0];   // semi major axis
   Real h      = equinoctial[1];   // projection of eccentricity vector onto y
   Real k      = equinoctial[2];   // projection of eccentricity vector onto x
   Real p      = equinoctial[3];   // projection of N onto y
   Real q      = equinoctial[4];   // projection of N onto x
   Real lambda = equinoctial[5]*RAD_PER_DEG;   // mean longitude
	     
   // Use mean longitude to find true longitude
	Real prevF;
	Real fF;
	Real fPrimeF;
	Real F = lambda;		 // first guess is mean longitude
	do {
		prevF   = F;
		fF      = F + h*Cos(F) - k*Sin(F) - lambda;
		fPrimeF = 1.0 - h*Sin(F) - k*Cos(F);
		F       = prevF - (fF/fPrimeF);
   } while (Abs(F-prevF) >= 1.0e-10);
//   } while (Abs(F-prevF) >= EQ_TOLERANCE);

	// Adjust true longitude to be between 0 and two-pi
   while (F < 0) F += TWO_PI;

   Real tmpSqrt = Sqrt(1.0 - (h * h) - (k * k));
   Real beta    = 1.0 / (1.0 + tmpSqrt);

//	Real beta = 1/(1 + Sqrt(1 - pEY*pEY - pEX*pEX));  // eq 4.36

//	Real n = Sqrt(mu/(sm*sm*sm));   // eq 4.37
//	Real r = sm*(1 - pEX*Cos(trueLong) - pEY*Sin(trueLong));  // eq 4.38
   Real n    = Sqrt(mu/(sma * sma * sma));
   Real cosF = Cos(F);
   Real sinF = Sin(F);
   Real r    = sma * (1.0 - (k * cosF) - (h * sinF));

   // Calculate the cartesian components expressed in the equinoctial coordinate system

   Real X1    = sma * (((1.0 - (h * h * beta)) * cosF) + (h * k * beta * sinF) - k);
   Real Y1    = sma * (((1.0 - (k * k * beta)) * sinF) + (h * k * beta * cosF) - h);
   Real X1Dot = ((n * sma * sma) / r) * ((h * k * beta * cosF) -
                (1.0 - (h * h * beta)) * sinF);
   Real Y1Dot = ((n * sma * sma) / r) * ((1.0 - (k * k * beta)) * cosF -
                (h * k * beta * sinF));

//	// eqns 4.39 - 4.42
//	Real x1  = sm*((1 - pEY*pEY*beta)*Cos(trueLong) + pEY*pEX*beta*Sin(trueLong) - pEX),
//	     y1  = sm*((1 - pEX*pEX*beta)*Sin(trueLong) + pEY*pEX*beta*Cos(trueLong) - pEY),
//	     _x1 = ((n*sm*sm)/r)*(pEY*pEX*beta*Cos(trueLong) - (1 - pEY*pEY*beta)*Sin(trueLong)),
//	     _y1 = ((n*sm*sm)/r)*((1 - pEX*pEX*beta)*Cos(trueLong) - pEY*pEX*beta*Sin(trueLong));

	// assumption in conversion from equinoctial to cartesian
	Integer j = 1;  // ******** how to determine if retrograde?  *****
	   
	// Compute Q matrix
	Rmatrix33 Q(1.0 - (p * p) + (q * q),   2.0 * p * q * j,                2.0 * p,
	            2.0 * p * q,               (1.0 + (p * p) - (q * q)) * j, -2.0 * q,
	           -2.0 * p * j,               2.0 * q,                       (1.0 - (p * p) - (q * q)) * j);
//	Rmatrix33 Q = SetQ(pNY, pNX, j);   // eq 4.46

	Rmatrix33 Q2 = (1.0 / (1.0 + (p * p) + (q * q))) * Q;
	Rvector3  f(Q2(0,0), Q2(1,0), Q2(2,0));
	Rvector3  g(Q2(0,1), Q2(1,1), Q2(2,1));
	f = f.GetUnitVector();
	g = g.GetUnitVector();

	Rvector3 pos = (X1 * f) + (Y1 * g);
	Rvector3 vel = (X1Dot * f) + (Y1Dot * g);

//	// eq 4.45
//	Rmatrix33 _Q = (1/(1+pNY*pNY+pNX*pNX))*Q;
//	Rvector3 fVec = _Q * Rvector3(1,0,0);
//	Rvector3 gVec = _Q * Rvector3(0,1,0);

//	Rvector3 pos = x1*fVec + y1*gVec;    // eq 4.43
//	Rvector3 vel = _x1*fVec + _y1*gVec;  // eq 4.44
	
//	Rvector6 cart( pos.Get(0),
//	               pos.Get(1),
//	               pos.Get(2),
//	               vel.Get(0),
//	               vel.Get(1),
//	               vel.Get(2) );
//
//	return cart;
	return Rvector6(pos, vel);
}

// public methods
Rvector6 Equinoctial::GetState()
{
	return Rvector6(sma, h, k, p, q, lambda);
}

void Equinoctial::SetState(const Rvector6& state)
{
   sma    = state[0];
   h      = state[1];
   k      = state[2];
   p      = state[3];
   q      = state[4];
   lambda = state[5];
}

Integer Equinoctial::GetNumData() const
{
	return NUM_DATA;
}

const std::string* Equinoctial::GetDataDescriptions() const
{
	return DATA_DESCRIPTIONS;
}

std::string* Equinoctial::ToValueStrings()
{
   std::stringstream ss("");

   ss << sma;
   stringValues[0] = ss.str();
   
   ss.str("");
   ss << h;
   stringValues[1] = ss.str();
   
   ss.str("");
   ss << k;
   stringValues[2] = ss.str();
   
   ss.str("");
   ss << p;
   stringValues[3] = ss.str();

   ss.str("");
   ss << q;
   stringValues[4] = ss.str();

   ss.str("");
   ss << lambda;
   stringValues[5] = ss.str();
   
   return stringValues;
}

//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------
// none

//Rmatrix33 Equinoctial::SetQ(Real pp, Real qq, Real jj) {
//	// eqn 4.46
//	return Rmatrix33( 1-pp*pp+qq*qq, 	   2*pp*qq*jj,			 2*pp,
//						  2*pp*qq, (1+pp*pp-qq*qq)*jj,			-2*qq,
//						 -2*pp*jj,		   2*qq, (1-pp*pp-qq*qq)*jj );
//}
