//$Id$
//------------------------------------------------------------------------------
//                                MarkPoint
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2009/10/06
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
/**
 * Class implementation for the MarkPoint command
 */
//------------------------------------------------------------------------------


#ifndef MarkPoint_hpp
#define MarkPoint_hpp

#include "GmatCommand.hpp"
#include "TsPlot.hpp"


/**
 * Command used to restore plotting on an XY plot during a run.
 */
class MarkPoint : public GmatCommand
{
public:
	MarkPoint();
	virtual          ~MarkPoint();
   MarkPoint(const MarkPoint &c);
   MarkPoint&        operator=(const MarkPoint &c);
   
   virtual GmatBase* Clone() const;
   
   virtual const ObjectTypeArray&
                     GetRefObjectTypeArray();
   virtual const StringArray&
                     GetRefObjectNameArray(const Gmat::ObjectType type);

   bool              InterpretAction();
   bool              Initialize();
   bool              Execute();

protected:
   StringArray          plotNameList;   
   std::vector<TsPlot*> thePlotList;
};

#endif /* MarkPoint_hpp */
