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
 */
//------------------------------------------------------------------------------


#ifndef DYNAMICLIBRARY_HPP_
#define DYNAMICLIBRARY_HPP_


#include "gmatdefs.hpp"
#include "Factory.hpp"
#include "TriggerManager.hpp"

/**
 * The DynamicLibrary class defines the interfaces that are needed to build a
 * GMAT plug-in library.  GMAT plugins usually include one or more classes 
 * derived from GmatBase, one or more factories that create instances of these 
 * classes, and three C-style functions that are accessed to import the Factory 
 * into GMAT.  The functions required in the plugin library are defined as 
 * follows:
 * 
 *    Integer     GetFactoryCount();
 *    Factory*    GetFactoryPointer(Integer index);
 *    void        SetMessageReceiver(MessageReceiver* mr);
 * 
 * The use of the first two functions matches the calls defined for this class.
 * The MessageReceiver is set using the SetMessageReceiver function when the 
 * library is loaded into memory.
 *
 * Plugin libraries may also optionally create and use TriggerManagers.  A
 * TriggerManager is an engine-level component that exists at the scope of the
 * Sandbox.  Each unique TriggerManager is a singleton in the Sandbox, and is
 * used to trigger specific Mission Control Sequence actions during a run.  The
 * prototypical TriggerManager option is event location for shadow entry and
 * exit epoch calculations during a run.
 *
 * Plugin libraries that include TriggerManagers need to implement the following
 * two functions:
 *
 *    Integer           GetTriggerManagerCount();
 *    TriggerManager*   GetTriggerManager(Integer index);
 *
 * If these functions are not implemented, the library will load but no
 * TriggerManager will be loaded.  In other words, if your code does not need a
 * TriggerManager, there is no need to implement these functions.
 */
class DynamicLibrary
{
public:
   DynamicLibrary(const std::string &name, const std::string &path = "./");
   virtual ~DynamicLibrary();
   DynamicLibrary(const DynamicLibrary& dlib);
   DynamicLibrary& operator=(const DynamicLibrary& dlib);

   bool                 LoadDynamicLibrary();
   void                 (*GetFunction(const std::string &funName))();

   Integer              GetFactoryCount();
   Factory*             GetGmatFactory(Integer index = 0);
   Integer              GetTriggerManagerCount();
   TriggerManager*      GetTriggerManager(Integer index = 0);
   
protected:
   std::string          libName;
   std::string          libPath;
   void *               libHandle;
};

#endif /*DYNAMICLIBRARY_HPP_*/
