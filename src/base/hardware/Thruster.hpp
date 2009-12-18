//$Id$
//------------------------------------------------------------------------------
//                               Thruster
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2004/11/08
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under MOMS Task
// Order 124.
//
/**
 * Class definition for the thrusters.
 */
//------------------------------------------------------------------------------


#ifndef THRUSTER_HPP
#define THRUSTER_HPP

#include "FuelTank.hpp"
#include "Hardware.hpp"

// Declare forward reference since Thruster owns FuelTank
class FuelTank;

/**
 * Thruster model used for finite maneuvers
 */
class GMAT_API Thruster : public Hardware
{
public:
   
   static const Integer COEFFICIENT_COUNT = 16;
   
   Thruster(std::string nomme);
   virtual ~Thruster();
   Thruster(const Thruster& th);
   Thruster&            operator=(const Thruster& th);
   
   // required method for all subclasses
   virtual GmatBase*    Clone() const;
   virtual void         Copy(const GmatBase* inst);
   
   // Parameter access methods - overridden from GmatBase
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;
   virtual bool         IsParameterReadOnly(const Integer id) const;
   
   virtual Real         GetRealParameter(const Integer id) const;
   virtual Real         SetRealParameter(const Integer id,
                                         const Real value);
   virtual bool         SetStringParameter(const Integer id, 
                                           const std::string &value);
   virtual bool         GetBooleanParameter(const Integer id) const;
   virtual bool         SetBooleanParameter(const Integer id,
                                            const bool value);
   
   virtual const StringArray&
                        GetStringArrayParameter(const Integer id) const; 
   
   // Ref. object access methods - overridden from GmatBase
   virtual bool         RenameRefObject(const Gmat::ObjectType type,
                                        const std::string &oldName,
                                        const std::string &newName);
   virtual bool         SetRefObject(GmatBase *obj, 
                                     const Gmat::ObjectType type,
                                     const std::string &name = "");
   virtual ObjectArray& GetRefObjectArray(const Gmat::ObjectType type);
   virtual ObjectArray& GetRefObjectArray(const std::string& typeString);
   
   
   virtual bool         TakeAction(const std::string &action,  
                                   const std::string &actionData = "");

   virtual bool         Initialize();

   Real                 CalculateMassFlow();
   
protected:
   /// Finite burn instances access thruster data directly
   friend class FiniteBurn;
   
   /// Acceleration due to gravity, used to specify Isp in seconds
   Real                       gravityAccel;   
   /// Thrust duty cycle for this thruster
   Real                       dutyCycle;
   /// Thrust scale factor for this thruster
   Real                       thrustScaleFactor;
   /// Current tank pressure
   Real                       pressure;
   /// Current tank temperature divided by reference temperature
   Real                       temperatureRatio;
   /// Most recently calculated thrust
   Real                       thrust;
   /// Most recently calculated specific impulse
   Real                       impulse;
   /// Most recently calculated mass flow rate
   Real                       mDot;
   /// Array of thrust coefficients
   Real                       cCoefficients[COEFFICIENT_COUNT];
   /// Array of specific impulse coefficients
   Real                       kCoefficients[COEFFICIENT_COUNT];
   /// Decrement mass flag
   bool                       decrementMass;
   /// Flag used to turn thruster on or off
   bool                       thrusterFiring;
   /// Flag used for constant thrust and Isp
   bool                       constantExpressions;
   /// Flag used for thrust and Isp that only use the first 3 coefficients
   bool                       simpleExpressions;
   /// Flag used to determine if the configuration needs updating
   bool                       initialized;
   /// Tank names
   StringArray                tankNames;
   /// The tanks
   std::vector<FuelTank *>    tanks;
   /// Temporary buffer used to get ref objects
   ObjectArray                tempArray;
   
   /// C-coefficient units
   static  StringArray        cCoefUnits;
   /// K-coefficient units
   static  StringArray        kCoefUnits;
   
   /// Published parameters for thrusters
   enum
   {
      THRUSTER_FIRING = HardwareParamCount, 
      DUTY_CYCLE,
      THRUST_SCALE_FACTOR,
      DECREMENT_MASS,
      TANK,
      GRAVITATIONAL_ACCELERATION,
      C1,    C2,    C3,    C4,    C5,    C6,    C7,    C8,    
      C9,   C10,   C11,   C12,   C13,   C14,   C15,   C16, 
      K1,    K2,    K3,    K4,    K5,    K6,    K7,    K8,    
      K9,   K10,   K11,   K12,   K13,   K14,   K15,   K16,
      C_UNITS,
      K_UNITS,
      ThrusterParamCount
   };
   
   /// Thruster parameter labels
   static const std::string
                        PARAMETER_TEXT[ThrusterParamCount - HardwareParamCount];
   /// Thruster parameter types
   static const Gmat::ParameterType
                        PARAMETER_TYPE[ThrusterParamCount - HardwareParamCount];
   
   bool                 CalculateThrustAndIsp();
     
};

#endif // THRUSTER_HPP
