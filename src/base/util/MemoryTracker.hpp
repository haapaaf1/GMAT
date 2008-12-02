//$Id$
//------------------------------------------------------------------------------
//                                 MemoryTracker
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Linda Jun
// Created: 2008/11/21
//
/**
 * Declares MemoryTracker class which tracks memory usage. It is a singleton class -
 * only one instance of this class can be created.
 */
//------------------------------------------------------------------------------
#ifndef MemoryTracker_hpp
#define MemoryTracker_hpp

#include "gmatdefs.hpp"

class MemoryTracker
{
public:
   
   static MemoryTracker* Instance();
   
   void           SetShowTrace(bool show);
   void           Add(void *addr, const std::string &objName,
                      const std::string &funName, const std::string &note = "");
   void           Remove(void *addr, const std::string &objName,
                      const std::string &funName, const std::string &note = "");
   StringArray&   GetTracks();
   StringArray&   FindLeaks();

private:
   
   static MemoryTracker *instance;
   
   struct TrackType
   {
      std::string preface;
      void *address;
      std::string objectName;
      std::string functionName;
      std::string remark;
      TrackType(const std::string &pref, void* addr, const std::string &objName,
                const std::string &funName, const std::string &note)
         {
            preface = pref;
            address = addr;
            objectName = objName;
            functionName = funName;
            remark = note;
         };
   };
   
   std::vector<TrackType> memoryTracks;
   StringArray allTracks;
   bool showTrace;
   
   MemoryTracker();
   ~MemoryTracker();
   
};

#endif // MemoryTracker_hpp

