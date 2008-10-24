/*
 * RunEstimator.hpp
 *
 *  Created on: Oct 21, 2008
 *      Author: djc
 */

#ifndef RunEstimator_HPP_
#define RunEstimator_HPP_

#include "GmatCommand.hpp"

class Estimator;     // Forward reference

/**
 * The RunEstimator command, the inverse of the Save command.  I hope!
 */
class RunEstimator : public GmatCommand
{
public:
	RunEstimator();
	virtual ~RunEstimator();
	RunEstimator(const RunEstimator &ld);
	RunEstimator& operator=(const RunEstimator &ld);

	virtual GmatBase* 	Clone() const;

	virtual bool         InterpretAction();
   virtual bool         Initialize();
	virtual bool         Execute();
   virtual void         RunComplete();

   // Setup object refs -- inherited from GmatBase
   // Parameter accessors
   virtual std::string  GetParameterText(const Integer id) const;
   virtual Integer      GetParameterID(const std::string &str) const;
   virtual Gmat::ParameterType
                        GetParameterType(const Integer id) const;
   virtual std::string  GetParameterTypeString(const Integer id) const;

   virtual bool         SetStringParameter(const Integer id,
                                           const std::string &value);
   virtual bool         SetStringParameter(const std::string &label,
                                           const std::string &value);
   virtual std::string  GetStringParameter(const Integer id) const;
   virtual std::string  GetStringParameter(const std::string &label) const;
//   virtual bool         RenameRefObject(const Gmat::ObjectType type,
//                                        const std::string &oldName,
//                                        const std::string &newName);

   virtual const std::string&
                        GetGeneratingString(
                           Gmat::WriteMode mode = Gmat::SCRIPTING,
                           const std::string &prefix = "",
                           const std::string &useName = "");

protected:
   std::string          estimatorName;
   Estimator            *est;

   // Parameter IDs
   enum
   {
      ESTIMATOR = GmatCommandParamCount,
      RunEstimatorParamCount
   };

   static const std::string
      PARAMETER_TEXT[RunEstimatorParamCount - GmatCommandParamCount];
   static const Gmat::ParameterType
      PARAMETER_TYPE[RunEstimatorParamCount - GmatCommandParamCount];

};

#endif /* RunEstimator_HPP_ */
