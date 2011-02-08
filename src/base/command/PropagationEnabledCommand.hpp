//$Id$
//------------------------------------------------------------------------------
//                       PropagationEnabledCommand
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/07/20
//
/**
 * Definition of the PropagationEnabledCommand base class
 */
//------------------------------------------------------------------------------


#ifndef PropagationEnabledCommand_hpp
#define PropagationEnabledCommand_hpp

#include "GmatCommand.hpp"
#include "PropSetup.hpp"
#include "Propagator.hpp"

#include "Spacecraft.hpp"
#include "Formation.hpp"

/// A convenient typedef used in this code
typedef std::vector<SpaceObject*> PropObjectArray;


#define TIME_ROUNDOFF 1.0e-6
#define DEFAULT_STOP_TOLERANCE 1.0e-7


/**
 * PropagationEnabledCommand is a base class used for commands that perform
 * propagation.  It provides the methods and interfaces needed perform basic
 * time-based propagation.  It does not provide interfaces for more complicated
 * stopping conditions; derived classes provide those interfaces.
 */
class GMAT_API PropagationEnabledCommand : public GmatCommand
{
public:
   PropagationEnabledCommand(const std::string &typeStr);
   // Abstract to force a derived class to instantiate
   virtual ~PropagationEnabledCommand() = 0;
   PropagationEnabledCommand(const PropagationEnabledCommand& pec);
   PropagationEnabledCommand& operator=(const PropagationEnabledCommand& pec);

   virtual bool         Initialize();

protected:
   // todo: Merge the propagator objects in the Propagate command into this code

   /// Names of the PropSetups used by this command, as set in a derived class
   StringArray                   propagatorNames;
   /// The PropSetup used by this command, as set in a derived class
   std::vector<PropSetup*>       propagators;
   /// Flag used to indicate that the PropSetups were built in a derived class
   bool                          overridePropInit;
   /// The objects that are propagated; one StringArray per PropSetup
   std::vector<StringArray>      propObjectNames;
   /// The objects that are propagated; one PropObjectArray per PropSetup
   std::vector<PropObjectArray*> propObjects;

   /// Flag indicating that the command has been executed once, so that some
   /// pieces of initialization can be skipped
   bool hasFired;
   /// Flag indicating the command is currently executing; used for reentrance
   bool inProgress;

   /// The size of the propagation state vector
   Integer dim;

   /// ID for the spacecraft epoch parameter
   Integer                      epochID;
   /// Starting epoch for the propagation
   std::vector<Real>            baseEpoch;

   /// Time elapsed during this Propagate
   RealArray                    elapsedTime;
   /// Start epoch for the step
   RealArray                    currEpoch;
   /// The Propagator
   std::vector<Propagator*>     p;
   /// The ForceModel
   std::vector<ODEModel*>       fm;
   /// The Propagation State Managers
   std::vector<PropagationStateManager*>  psm;

   /// The Mean-of-J2000 propagation state vector data
   Real                         *j2kState;
   /// Data sent to the Publisher
   Real                         *pubdata;

   /// Stopping condition evaluation requires propagation; the satBuffer and
   /// formBuffer let us restore the Spacecraft and Formations to the state
   /// needed for the last step
   std::vector<Spacecraft *>    satBuffer;
   std::vector<Formation *>     formBuffer;


   bool                 PrepareToPropagate();
   bool                 AssemblePropagators();
   bool                 Step(Real dt);

   virtual void         AddToBuffer(SpaceObject *so);
   virtual void         EmptyBuffer();
   virtual void         BufferSatelliteStates(bool fillingBuffer = true);

   virtual void         SetPropagationProperties(PropagationStateManager *psm);
};

#endif /* PropagationEnabledCommand_hpp */