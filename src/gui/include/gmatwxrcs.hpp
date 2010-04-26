//$Header$
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2003/08/08
// Copyright: (c) 2003 NASA/GSFC. All rights reserved.
//
/**
 * This header file contains wxWindows resources.
 */
//------------------------------------------------------------------------------
#ifndef gamtwxrcs_h
#define gamtwxrcs_h

// the application icon (under Windows and OS/2 it is in resources)
#if defined(__WXGTK__) || defined(__WXMOTIF__) || defined(__WXMAC__) || defined(__WXMGL__) || defined(__WXX11__)
    #include "mondrian.xpm"
#endif

#endif // gamtwxrcs_h