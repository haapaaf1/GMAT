/*
 * RunEstimator.cpp
 *
 *  Created on: Oct 21, 2008
 *      Author: djc
 */

#include "RunEstimator.hpp"
#include "MessageInterface.hpp"
#include "CommandException.hpp"
#include "Estimator.hpp"

#define DEBUG_STATE_MACHINE
//#define DEBUG_COMMAND_CALLS


//---------------------------------
//  static data
//---------------------------------
const std::string
RunEstimator::PARAMETER_TEXT[RunEstimatorParamCount - GmatCommandParamCount] =
{
   "EstimatorName",
};

const Gmat::ParameterType
RunEstimator::PARAMETER_TYPE[RunEstimatorParamCount - GmatCommandParamCount] =
{
   Gmat::STRING_TYPE,   // "EstimatorName"
};




RunEstimator::RunEstimator() :
   GmatCommand   		("RunEstimator"),
   estimatorName     (""),
   est               (NULL),
   baseEpoch         (0.0)
{
}

RunEstimator::~RunEstimator()
{
}

RunEstimator::RunEstimator(const RunEstimator &ld) :
	GmatCommand			(ld),
   estimatorName     (ld.estimatorName),
   est               (NULL),
   baseEpoch         (ld.baseEpoch)
{
}

RunEstimator& RunEstimator::operator=(const RunEstimator &ld)
{
	if (&ld != this)
	{
	   estimatorName = ld.estimatorName;
	   est = NULL;
	   baseEpoch = ld.baseEpoch;
	}

	return *this;
}

//------------------------------------------------------------------------------
//  GmatBase* Clone() const
//------------------------------------------------------------------------------
/**
 * This method returns a clone of the Save.
 *
 * @return clone of the Save.
 */
//------------------------------------------------------------------------------
GmatBase* RunEstimator::Clone() const
{
   return (new RunEstimator(*this));
}

//------------------------------------------------------------------------------
// bool InterpretAction()
//------------------------------------------------------------------------------
/**
 * This code should not be required.  However, the current version of the
 * Interpreter base class has broken the designed and previously implemented
 * feature of allowing simple commands to be configured without adding internal
 * parsing.  That feature needs to be restored to the system.
 */
bool RunEstimator::InterpretAction()
{
   // Sample string:  "RunEstimator myEstimator"

   Integer loc = generatingString.find("RunEstimator", 0) + 12;
   const char *str = generatingString.c_str();
   while (str[loc] == ' ')
      ++loc;
   Integer subEnd, commentStart;
   if ((commentStart = (Integer) generatingString.find_first_of("%", 0)) !=
       (Integer)generatingString.npos)
      subEnd = commentStart;
   else
      subEnd = (Integer) generatingString.size();
   std::string str1 = generatingString.substr(loc, subEnd-loc);

   #ifdef DEBUG_PARSING
      MessageInterface::ShowMessage("In InterpretAction, str1 = \n");
      MessageInterface::ShowMessage("   '%s'\n", str1.c_str());
   #endif

   // this command, for compatability with MATLAB, should not have
   // parentheses (except to indicate array elements), brackets, or braces
   if (!GmatStringUtil::HasNoBrackets(str, false))
   {
      std::string msg =
         "The RunEstimator command is not allowed to contain brackets, braces, "
         "or parentheses";
      throw CommandException(msg);
   }

   estimatorName = str1;
   return true;
}

//------------------------------------------------------------------------------
// Generic methods overridden from GmatBase:
//    virtual std::string  GetParameterText(const Integer id) const;
//    virtual Integer      GetParameterID(const std::string &str) const;
//    virtual Gmat::ParameterType
//                     GetParameterType(const Integer id) const;
//    virtual std::string  GetParameterTypeString(const Integer id) const;
//
//    virtual bool         SetStringParameter(const Integer id,
//                                        const std::string &value);
//    virtual bool         SetStringParameter(const std::string &label,
//                                        const std::string &value);
//    virtual std::string  GetStringParameter(const Integer id) const;
//    virtual std::string  GetStringParameter(const std::string &label) const;
//------------------------------------------------------------------------------
/**
 * The following methods are not needed since I'm doing internal parsing.  They
 * were coded before I found that the generic parsing is broken.
 */
std::string RunEstimator::GetParameterText(const Integer id) const
{
   if (id >= GmatCommandParamCount && id < RunEstimatorParamCount)
      return PARAMETER_TEXT[id - GmatCommandParamCount];
   else
      return GmatCommand::GetParameterText(id);
}

Integer RunEstimator::GetParameterID(const std::string &str) const
{
   for (int i=GmatCommandParamCount; i<RunEstimatorParamCount; i++)
   {
      if (str == PARAMETER_TEXT[i - GmatCommandParamCount])
         return i;
   }

   return GmatCommand::GetParameterID(str);
}

Gmat::ParameterType RunEstimator::GetParameterType(const Integer id) const
{
   if (id >= GmatCommandParamCount && id < RunEstimatorParamCount)
      return PARAMETER_TYPE[id - GmatCommandParamCount];
   else
      return GmatCommand::GetParameterType(id);
}

std::string RunEstimator::GetParameterTypeString(const Integer id) const
{
   if (id >= GmatCommandParamCount && id < RunEstimatorParamCount)
      return GmatBase::PARAM_TYPE_STRING[GetParameterType(id)];
   else
      return GmatCommand::GetParameterTypeString(id);
}

bool RunEstimator::SetStringParameter(const Integer id,
                                      const std::string &value)
{
   if (id == ESTIMATOR)
   {
      estimatorName = value;
      return true;
   }

   return GmatCommand::SetStringParameter(id, value);
}

bool RunEstimator::SetStringParameter(const std::string &label,
                                      const std::string &value)
{
   return SetStringParameter(GetParameterID(label), value);
}

std::string RunEstimator::GetStringParameter(const Integer id) const
{
   if (id == ESTIMATOR)
   {
      return estimatorName;
   }

   return GmatCommand::GetStringParameter(id);
}

std::string RunEstimator::GetStringParameter(const std::string &label) const
{
   return GetStringParameter(GetParameterID(label));
}

//------------------------------------------------------------------------------
// Methods used to run the command
//------------------------------------------------------------------------------


bool RunEstimator::Initialize()
{
   #ifdef DEBUG_COMMAND_CALLS
      MessageInterface::ShowMessage(
            "The RunEstimator command is initializing...");
   #endif

   bool retval = GmatCommand::Initialize();
   est = (Estimator*)(*objectMap)[estimatorName];
   if (est == NULL)
      est = (Estimator*)((*globalObjectMap)[estimatorName]);

   if (est == NULL)
      throw CommandException("Unable to find the estimator named \"" +
            estimatorName + "\"");

   // Set reference objects used by the estimator
   // 1. Set the propagator
   std::string objname = est->GetStringParameter("Propagator");

   MessageInterface::ShowMessage("Looking for propagator %s\n", objname.c_str());

   GmatBase *obj = (*objectMap)[objname];
   if (obj == NULL)
      obj = (*globalObjectMap)[objname];
   if (obj != NULL)
   {
      if (obj->IsOfType(Gmat::PROP_SETUP))
      {
         propagator = (PropSetup*)obj;
         est->SetRefObject(obj, obj->GetType(), obj->GetName());
      }
      else
         throw CommandException("The object named \"" +
               objname + "\" is not a propagator setup.");
   }
   else
      throw CommandException("There is no PropSetup named \"" +
            objname + "\"");

   // 2. Set the participants
   StringArray objlist = est->GetStringArrayParameter(est->GetParameterID("Participants"));

   MessageInterface::ShowMessage("Participant list has %d members:\n",
         objlist.size());


   for (StringArray::iterator i = objlist.begin(); i != objlist.end(); ++i)
   {
      obj = NULL;
      std::string objName = (*i);
      obj = (*objectMap)[objName];

      if (obj == NULL)
         obj = (*globalObjectMap)[objName];

      if (obj == NULL)
         throw CommandException("Cannot locate object named " + (objName) +
               " referenced by Estimator " + est->GetName());

      if (obj->GetName() != objName)
         obj = (*globalObjectMap)[objName];

      if (obj->GetName() != objName)
         throw CommandException("Still cannot locate object named " + objName +
               " referenced by Estimator " + est->GetName());

      MessageInterface::ShowMessage("   Adding participant %s of type %s, typeID %d\n",
            obj->GetName().c_str(), obj->GetTypeName().c_str(), obj->GetType());

      if (obj != NULL)
      {
//         if (obj->IsOfType(Gmat::SPACE_POINT))
         {
            participants.push_back(obj);
            est->SetRefObject(obj, obj->GetType(), obj->GetName());
         }
//         else
//            throw CommandException("The object named \"" +
//                  (*i) + "\" is not a valid participant.");
      }
      else
         throw CommandException("There is no participant named \"" +
               (*i) + "\"");

   }

   // 3. Set the measurement models



   // Request initial epoch for the measurements
   baseEpoch = GetStartEpoch();


   // Assemble the propagator
   if (retval)
      retval = AssemblePropagator();

   #ifdef DEBUG_COMMAND_CALLS
      MessageInterface::ShowMessage("Initialization complete.\n");
   #endif

   est->Initialize();

   return retval;
}



Real RunEstimator::GetStartEpoch()
{
   Real epoch = 0.0;
   for (ObjectArray::iterator i = participants.begin(); i != participants.end();
        ++i)
      if ((*i)->IsOfType(Gmat::SPACEOBJECT))
      {
         if (epoch == 0.0)
            epoch = ((SpaceObject*)(*i))->GetEpoch();
         else
            if (epoch != ((SpaceObject*)(*i))->GetEpoch())
               throw CommandException(
                     "Epochs are not synchronized at the start of estimation "
                     "for the command \n" + GetGeneratingString());
      }

   return epoch;
}


//---------------------------------------------------------------------------
//  bool Execute()
//---------------------------------------------------------------------------
/**
 * The method that is fired to perform the GmatCommand.
 *
 * Derived classes implement this method to perform their actions on
 * GMAT objects.
 *
 * @return true if the GmatCommand runs to completion, false if an error
 *         occurs.
 */
//---------------------------------------------------------------------------
bool RunEstimator::Execute()
{
   MessageInterface::ShowMessage("The \"%s\" command is running...\n",
         GetTypeName().c_str());

   // Drive through the state machine
   Solver::SolverState state = est->GetState();
   while (state != Solver::FINISHED)
   {
      switch (state)
      {
         case Solver::INITIALIZING:
            break;

         case Solver::PROPAGATING:
            Propagate();
            break;

         case Solver::CALCULATING:
            Calculate();
            break;

         case Solver::ESTIMATING:
            Estimate();
            break;

         case Solver::CHECKINGRUN:
            CheckConvergence();
            break;

         case Solver::FINISHED:
            Finalize();
            break;

         default:
            break;
      }

      state = est->AdvanceState();
   }

   return true;
}

bool RunEstimator::Propagate()
{
#ifdef DEBUG_STATE_MACHINE
   MessageInterface::ShowMessage(
         "Stepping the participants to the requested epoch.\n");
#endif
   Real dt = GetTimestep();
   Step(dt);

   return true;
}

bool RunEstimator::Calculate()
{
#ifdef DEBUG_STATE_MACHINE
   MessageInterface::ShowMessage("Command side accumulation stuff\n");
#endif

   return true;
}

bool RunEstimator::Estimate()
{
#ifdef DEBUG_STATE_MACHINE
   MessageInterface::ShowMessage("Command side estimation stuff\n");
#endif

   return true;
}

bool RunEstimator::CheckConvergence()
{
#ifdef DEBUG_STATE_MACHINE
   MessageInterface::ShowMessage("Command side convergence checking stuff\n");
#endif

   return true;
}

bool RunEstimator::Finalize()
{
#ifdef DEBUG_STATE_MACHINE
   MessageInterface::ShowMessage("Command side finalization stuff\n");
#endif

   return true;
}


void RunEstimator::RunComplete()
{
#ifdef DEBUG_STATE_MACHINE
   MessageInterface::ShowMessage(
         "The RunEstimator command has finished running!\n");
#endif
}

//------------------------------------------------------------------------------
// const std::string& GetGeneratingString(Gmat::WriteMode mode,
//                                        const std::string &prefix,
//                                        const std::string &useName)
//------------------------------------------------------------------------------
const std::string& RunEstimator::GetGeneratingString(Gmat::WriteMode mode,
                                             const std::string &prefix,
                                             const std::string &useName)
{
   generatingString = prefix + "RunEstimator " + estimatorName + ";";
   return GmatCommand::GetGeneratingString(mode, prefix, useName);
}



//------------------------------------------------------------------------------
// protected methods
//------------------------------------------------------------------------------
bool RunEstimator::AssemblePropagator()
{
   if (propagator == NULL)
      return false;

   for (ObjectArray::iterator i = participants.begin(); i != participants.end(); ++i)
   {
      // For now we only prop spacecraft
      if ((*i)->IsOfType(Gmat::SPACECRAFT))
         propagator->GetForceModel()->AddSpaceObject((SpaceObject*)(*i));
   }

   return propagator->Initialize();
}

Real RunEstimator::GetTimestep()
{
   // TODO: Call the estimator for this data
   // one minute steps to test flow
   return 60.0;
}

bool RunEstimator::Step(Real dt)
{
   bool retval = true;
   ForceModel* fm = propagator->GetForceModel();
   Real baseEpoch = 21545.0;
   if (dt != 0.0)
   {
      retval = propagator->GetPropagator()->Step(dt);
      fm->UpdateSpaceObject(dt/86400.0);

      // orbit related parameters use spacecraft for data
      Real elapsedTime = fm->GetTime();
      Real currEpoch = baseEpoch + elapsedTime /
            GmatTimeUtil::SECS_PER_DAY;

      // Update spacecraft epoch, without argument the spacecraft epoch
      // won't get updated for consecutive Propagate command
      fm->UpdateSpaceObject(currEpoch);
      baseEpoch = currEpoch;
   }
   return retval;
}
