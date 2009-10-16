//$Id$
//------------------------------------------------------------------------------
//                            File: HardwareParameters
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. 
//
// Author: Linda Jun
// Created: 2009.03.20
//
/**
 * Declares Hardware related parameter classes.
 *    FuelTank: FuelMass, Pressure, Temperature, FuelVolume, FuelDensity
 *    Thruster: DutyCycle, ThrustScaleFactor, GravitationalAccel
 */
//------------------------------------------------------------------------------
#ifndef HardwareParameters_hpp
#define HardwareParameters_hpp

#include "gmatdefs.hpp"
#include "HardwareReal.hpp"

class GMAT_API FuelMass : public HardwareReal
{
public:
   
   FuelMass(const std::string &name = "", GmatBase *obj = NULL);
   FuelMass(const FuelMass &copy);
   FuelMass& operator=(const FuelMass &right);
   virtual ~FuelMass();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

class GMAT_API Pressure : public HardwareReal
{
public:

   Pressure(const std::string &name = "", GmatBase *obj = NULL);
   Pressure(const Pressure &copy);
   Pressure& operator=(const Pressure &right);
   virtual ~Pressure();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

class GMAT_API Temperature : public HardwareReal
{
public:

   Temperature(const std::string &name = "", GmatBase *obj = NULL);
   Temperature(const Temperature &copy);
   Temperature& operator=(const Temperature &right);
   virtual ~Temperature();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

class GMAT_API Volume : public HardwareReal
{
public:

   Volume(const std::string &name = "", GmatBase *obj = NULL);
   Volume(const Volume &copy);
   Volume& operator=(const Volume &right);
   virtual ~Volume();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

class GMAT_API FuelDensity : public HardwareReal
{
public:

   FuelDensity(const std::string &name = "", GmatBase *obj = NULL);
   FuelDensity(const FuelDensity &copy);
   FuelDensity& operator=(const FuelDensity &right);
   virtual ~FuelDensity();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

class GMAT_API DutyCycle : public HardwareReal
{
public:

   DutyCycle(const std::string &name = "", GmatBase *obj = NULL);
   DutyCycle(const DutyCycle &copy);
   DutyCycle& operator=(const DutyCycle &right);
   virtual ~DutyCycle();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

class GMAT_API ThrustScaleFactor : public HardwareReal
{
public:

   ThrustScaleFactor(const std::string &name = "", GmatBase *obj = NULL);
   ThrustScaleFactor(const ThrustScaleFactor &copy);
   ThrustScaleFactor& operator=(const ThrustScaleFactor &right);
   virtual ~ThrustScaleFactor();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

class GMAT_API GravitationalAccel : public HardwareReal
{
public:

   GravitationalAccel(const std::string &name = "", GmatBase *obj = NULL);
   GravitationalAccel(const GravitationalAccel &copy);
   GravitationalAccel& operator=(const GravitationalAccel &right);
   virtual ~GravitationalAccel();
   
   // methods inherited from Parameter
   virtual bool Evaluate();
   
   // methods inherited from GmatBase
   virtual GmatBase* Clone(void) const;
   
protected:

};

#endif //HardwareParameters_hpp
