//$Id$
//------------------------------------------------------------------------------
//                              DynamicLibrary
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/04/18
//
/**
 * Definition for library code loaded at run time.
 * 
 * This is prototype code.
 */
//------------------------------------------------------------------------------


#ifndef DYNAMICLIBRARY_HPP_
#define DYNAMICLIBRARY_HPP_


#include "gmatdefs.hpp"


class DynamicLibrary
{
public:
   DynamicLibrary(const std::string &name, const std::string &path = "./");
   virtual ~DynamicLibrary();
   DynamicLibrary(const DynamicLibrary& dlib);
   DynamicLibrary& operator=(const DynamicLibrary& dlib);

   bool                 LoadDynamicLibrary();
   void                 (*GetFunction(const std::string &funName))();
   
protected:
   std::string          libName;
   std::string          libPath;
   void *               libHandle;
};

#endif /*DYNAMICLIBRARY_HPP_*/
