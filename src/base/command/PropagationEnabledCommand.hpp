//$Id$
//------------------------------------------------------------------------------
//                         ClassName
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2009/ /
//
/**
 * File description here.
 */
//------------------------------------------------------------------------------


#ifndef PropagationEnabledCommand_hpp
#define PropagationEnabledCommand_hpp

#include "GmatCommand.hpp"
#include "PropSetup.hpp"
#include "Propagator.hpp"

typedef std::vector<SpaceObject*> PropObjectArray;


class PropagationEnabledCommand : public GmatCommand
{
public:
   PropagationEnabledCommand(const std::string &typeStr);
   // Abstract to force a derived class to instantiate
   virtual ~PropagationEnabledCommand() = 0;
   PropagationEnabledCommand(const PropagationEnabledCommand& pec);
   PropagationEnabledCommand& operator=(const PropagationEnabledCommand& pec);

   virtual bool         Initialize();

protected:
   /// The PropSetup used by this command, as set in a derived class
   std::vector<PropSetup*>       propagators;
   /// The objects that are propagated; one StringArray per PropSetup
   std::vector<StringArray>      propObjectNames;
   /// The objects that are propagated; one PropObjectArray per PropSetup
   std::vector<PropObjectArray*> propObjects;

   bool hasFired;
   bool inProgress;

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

   Real                         *j2kState;
   /// Data sent to the Publisher
   Real                    *pubdata;


   bool                 PrepareToPropagate();
   bool                 AssemblePropagators();
   bool                 Step(Real dt);
};

#endif /* PropagationEnabledCommand_hpp */
