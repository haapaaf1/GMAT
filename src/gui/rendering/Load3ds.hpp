//------------------------------------------------------------------------------
//                              Load3ds
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// ** Legal **
//
// Author: Phillip Silvia, Jr.
// Created: 2009/06/24
/**
 * Loads a .3ds file and stores the information in a ModelObject
 */
//------------------------------------------------------------------------------

#ifndef _LOAD_3DS_H
#define _LOAD_3DS_H

#include "ModelObject.hpp"

// Constants
#define LOAD3DS_DEBUG 0

// Functions
extern char Load3DS(ModelObject *p_object, const wxString &p_filename);

#endif
