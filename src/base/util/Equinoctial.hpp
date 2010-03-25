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
 * Definition for the Equinoctial class.
 */
//------------------------------------------------------------------------------

#ifndef Equinoctial_hpp
#define Equinoctial_hpp

#include <iostream>
#include <sstream>
#include "gmatdefs.hpp"
#include "Rmatrix33.hpp"
#include "Rvector6.hpp"

class Equinoctial
{
public:
   Equinoctial(); 
   Equinoctial(const Rvector6& state);
   Equinoctial(const Real& a, const Real& pEY, const Real& pEX, const Real& pNY, const Real& pNX, const Real& ml);
   Equinoctial(const Equinoctial &eq);
   Equinoctial& operator=(const Equinoctial &m);
   virtual ~Equinoctial();

   //  Friend functions
   friend std::ostream& operator<<(std::ostream& output, const Equinoctial& eq);
   friend std::istream& operator>>(std::istream& input, Equinoctial& eq);

   friend Rvector6 CartesianToEquinoctial(const Rvector6& cartesian, const Real& mu);
   friend Rvector6 EquinoctialToCartesian(const Rvector6& equinoctial, const Real& mu);

   // public methods
   Rvector6 GetState();
   void SetState(const Rvector6& state);

   Integer GetNumData() const;
   const std::string* GetDataDescriptions() const;
   std::string* ToValueStrings();

protected:

   Real sma;      // semimajor axis
   Real h;        // projection of the eccentricity vector onto the yep axis
   Real k;        // projection of the eccentricity vector onto the xep axis
   Real p;        // projection of N onto the yep axis;
   Real q;        // projection of N onto the xep axis
   Real lambda;   // mean longitude, in degrees

   static const Integer      NUM_DATA = 6;
   static const std::string  DATA_DESCRIPTIONS[NUM_DATA];
   static const Real         EQ_TOLERANCE;

   std::string  stringValues[NUM_DATA];

//   Rmatrix33 SetQ(Real pp, Real qq, Real jj);
};



#endif /*Equinoctial_hpp*/
