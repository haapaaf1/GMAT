//$Header$
//------------------------------------------------------------------------------
//                                 Maneuver
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Darrel J. Conway
// Created: 2003/09/23
//
/**
 * Definition for the Maneuver command class
 */
//------------------------------------------------------------------------------


#ifndef Maneuver_hpp
#define Maneuver_hpp

#include "Command.hpp"
#include "Burn.hpp"
#include "Spacecraft.hpp"


/**
 * This class implements the Maneuver command.  Maneuvers are performed using
 * pre-configured Burn objects.  A typical segment of script that implements
 * a maneuver for a spacecraft named Sat1 looks like this:
 *
 *     Create ImpulsiveBurn burn;
 *     burn.Type = VNB;
 *     burn.Element1 = 0.125;         % km/s
 *     ...
 *     Maneuver burn(Sat1);
 */
class Maneuver : public Command
{
public:
    Maneuver(void);
    virtual ~Maneuver(void);
    
    Maneuver(const Maneuver& m);
    Maneuver&               operator=(const Maneuver& m);
    
    // Methods used for configuration
    virtual bool            SetObject(const std::string &name,
                                      const Gmat::ObjectType type,
                                      const std::string &associate = "",
                                      const Gmat::ObjectType associateType =
                                      Gmat::UNKNOWN_OBJECT);
    
    virtual bool            SetObject(GmatBase *obj,
                                      const Gmat::ObjectType type);
    
    // Parameter accessor methods -- overridden from GmatBase
    virtual std::string     GetParameterText(const Integer id) const;
    virtual Integer         GetParameterID(const std::string &str) const;
    virtual Gmat::ParameterType
                            GetParameterType(const Integer id) const;
    virtual std::string     GetParameterTypeString(const Integer id) const;
    
    virtual Integer         GetIntegerParameter(const Integer id) const;
    virtual Integer         SetIntegerParameter(const Integer id,
                                            const Integer value);
    virtual bool            GetBooleanParameter(const Integer id) const;
    virtual bool            SetBooleanParameter(const Integer id,
                                                const bool value);
    
    // Methods used to run the command
    virtual void            InterpretAction(void);
    
    virtual bool            Initialize(void);
    virtual bool            Execute(void);
    
protected:
    /// The burn object used for the maneuver
    Burn                    *burn;
    /// The name of the spacecraft that gets maneuvered
    std::string             satName;
    /// The spacecraft
    Spacecraft              *sat;
    
    // Parameter IDs 
    /// ID for the burn object
    Integer                 burnID;
    /// ID for the spacecraft name
    Integer                 satNameID;
};

#endif // Maneuver_hpp
