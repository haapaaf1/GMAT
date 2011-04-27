//$Id$
//------------------------------------------------------------------------------
//                                PenDown
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// Copyright (c) 2002-2011 United States Government as represented by the
// Administrator of The National Aeronautics and Space Administration.
// All Other Rights Reserved.
//
// Author: Darrel J. Conway
// Created: 2004/02/26
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
/**
 * Class implementation for the PenDown command
 */
//------------------------------------------------------------------------------


#ifndef PenDown_hpp
#define PenDown_hpp

#include "GmatCommand.hpp"
#include "XyPlot.hpp"


/**
 * Command used to restore plotting on an XY plot during a run; see also the 
 * PenUp command.
 */
class GMAT_API PenDown : public GmatCommand
{
public:
        PenDown();
        virtual          ~PenDown();
   PenDown(const PenDown &c);
   PenDown&        operator=(const PenDown &c);
   
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
   std::vector<XyPlot*> thePlotList;
};

#endif /* PenDown_hpp */
