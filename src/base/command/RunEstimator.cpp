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
   est               (NULL)
{
}

RunEstimator::~RunEstimator()
{
}

RunEstimator::RunEstimator(const RunEstimator &ld) :
	GmatCommand			(ld),
   estimatorName     (ld.estimatorName),
   est               (NULL)
{
}

RunEstimator& RunEstimator::operator=(const RunEstimator &ld)
{
	if (&ld != this)
	{
	   estimatorName = ld.estimatorName;
	   est = NULL;
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
   MessageInterface::ShowMessage("The \"%s\" command is initializing...\n",
         GetGeneratingString().c_str());
   bool retval = GmatCommand::Initialize();

   est = (Estimator*)(*objectMap)[estimatorName];
   if (est == NULL)
      est = (Estimator*)((*globalObjectMap)[estimatorName]);

   if (est == NULL)
      throw CommandException("Unable to find the estimator named \"" +
            estimatorName + "\"");

   MessageInterface::ShowMessage("The \"%s\" command %s initialization.\n",
         GetGeneratingString().c_str(), (retval ? "passed" : "failed"));

   return retval;
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
         GetGeneratingString().c_str());
   return true;
}


void RunEstimator::RunComplete()
{
   MessageInterface::ShowMessage("The \"%s\" command has finished running!\n",
         GetGeneratingString().c_str());
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
