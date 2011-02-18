//$Id$
//------------------------------------------------------------------------------
//                              JacchiaRobertsAtmosphere
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Waka A. Waktola
// Created: 2004/05/11
//
/**
 * The Jacchia-Roberts atmosphere.
 */
//------------------------------------------------------------------------------

#ifndef JacchiaRobertsAtmosphere_hpp
#define JacchiaRobertsAtmosphere_hpp

#include "AtmosphereModel.hpp"
#include "CelestialBody.hpp"
#include "A1Mjd.hpp"
#include "TimeTypes.hpp"


class GMAT_API JacchiaRobertsAtmosphere : public AtmosphereModel
{
public:
   JacchiaRobertsAtmosphere(const std::string &name = "");
   virtual ~JacchiaRobertsAtmosphere();
   JacchiaRobertsAtmosphere(const JacchiaRobertsAtmosphere& jr);
   JacchiaRobertsAtmosphere& operator=(const JacchiaRobertsAtmosphere& jr);
    
   // inherited from GmatBase
   virtual GmatBase* Clone() const;
    
   bool Density(Real *position, Real *density, Real epoch = GmatTimeUtil::MJD_OF_J2000,
                Integer count = 1);
   virtual void SetCentralBody(CelestialBody *cb);

   Real  JacchiaRoberts(Real height, Real space_craft[3], Real sun[3],
                      Real a1_time, bool new_file);
   Real  exotherm(Real space_craft[3], Real sun[3], GEOPARMS *geo,
                       Real height, Real sun_dec, Real geo_lat);
   Real  rho_100(Real height, Real temperature);
   Real  rho_125(Real height, Real temperature);
   Real  rho_cor(Real height, Real a1_time, Real geo_lat,
                      GEOPARMS *geo);
   Real  rho_high(Real height, Real temperature, Real t_500,
                       Real sun_dec, Real geo_lat);
   void  roots(Real a[], Integer na, Real croots[][2], Integer irl);

   void deflate_polynomial(Real c[], Integer n, Real root, Real c_new[]);
   
   Real get_geodetic_height(Real R_eq, Real r[3], Real f);
   
   Real length_of(Real v[3]);
   Real dot_product(Real a[3] , Real b[3]);
   
   void GetEarth();                             
                                                                                                 
private:
   CelestialBody *earth;
   
   ///  Auxiliary temperature related quantities
   Real root1;
   Real root2;
   Real x_root;
   Real y_root;

   ///  Some intermediate temperature results used by density computations
   Real t_infinity;
   Real tx;
   Real sum;
   Real cbPolarRadius;
   Real cbPolarSquared;

   /// low altitude density in g/cm**2
   const Real rho_zero;
   /// Temperature in degrees kelvin at height of 90km
   const Real tzero;
   /// earth gravitational constant m/sec**2
   const Real g_zero;
   /// gas constant (joules/(degK-mole))
   const Real gas_con;
   /// Avogadro's number
   const Real avogadro;
};

#endif // JacchiaRobertsAtmosphere_hpp
