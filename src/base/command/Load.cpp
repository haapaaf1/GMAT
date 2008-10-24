/*
 * Load.cpp
 *
 *  Created on: Oct 21, 2008
 *      Author: djc
 */

#include "Load.hpp"

Load::Load() :
   GmatCommand   		("Load"),
   fileName          ("")
{
	// TODO Auto-generated constructor stub

}

Load::~Load()
{
	// TODO Auto-generated destructor stub
}

Load::Load(const Load &ld) :
	GmatCommand			(ld),
   fileName          (ld.fileName)
{
}

Load& Load::operator=(const Load &ld)
{
	if (&ld != this)
	{
	   fileName = ld.fileName;
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
GmatBase* Load::Clone() const
{
   return (new Load(*this));
}


//---------------------------------------------------------------------------
//  bool GmatCommand::Execute()
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
bool Load::Execute()
{

}
