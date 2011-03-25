/*
 * Load.hpp
 *
 *  Created on: Oct 21, 2008
 *      Author: djc
 */

#ifndef LOAD_HPP_
#define LOAD_HPP_

#include "GmatCommand.hpp"


/**
 * The Load command, the inverse of the Save command.  I hope!
 */
class Load : public GmatCommand
{
public:
	Load();
	virtual ~Load();
	Load(const Load &ld);
	Load& operator=(const Load &ld);

	virtual GmatBase* 	Clone() const;
	virtual bool         Execute();

protected:
   std::string          fileName;
   StringArray          objectList;

};

#endif /* LOAD_HPP_ */
