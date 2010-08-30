//$Id: gmatOpenGLSupport.hpp 5162 2008-01-18 15:56:52Z djcinsb $
//------------------------------------------------------------------------------
//                              gmatOpenGLSupport
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// ** Legal **
//
// Author: Linda Jun
// Created: 2003/11/25
// Refactored here for visualization purposes
/**
* Declares gmatOpenGLSupport for opengl plot.
*/
//------------------------------------------------------------------------------
#ifndef GmatOpenGLSupport_hpp
#define GmatOpenGLSupport_hpp

// windows specific functions
void InitGL();
bool SetPixelFormatDescriptor();
void SetDefaultGLFont();
void ScreenShotSave(char* ImagePath);
#endif