//$Header$
//------------------------------------------------------------------------------
//                                  NoOp
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool.
//
// Author: Darrel J. Conway
// Created: 2003/10/24
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
/**
 * Null operator for the command sequence -- typically used as a place holder
 */
//------------------------------------------------------------------------------



#ifndef NoOp_hpp
#define NoOp_hpp

#include "GmatCommand.hpp" // inheriting class's header file

/**
 * Default command used to initialize the command sequence lists in the 
 * Moderator
 */
class GMAT_API NoOp : public GmatCommand
{
public:
   NoOp();
   virtual ~NoOp();
   NoOp(const NoOp& noop);
   NoOp&                   operator=(const NoOp &noop);

   bool                    Execute(void);

   // inherited from GmatBase
   virtual GmatBase* Clone(void) const;
};


#endif // NoOp_hpp