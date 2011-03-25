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
 * Implements MemoryTracker class which tracks memory usage. It is a singleton class -
 * only one instance of this class can be created.
 */
//------------------------------------------------------------------------------

#include "MemoryTracker.hpp"
#include "MessageInterface.hpp"

//--------------------------------------
//  initialize static variables
//--------------------------------------
MemoryTracker* MemoryTracker::instance = NULL;

//------------------------------------------------------------------------------
// MemoryTracker* Instance()
//------------------------------------------------------------------------------
MemoryTracker* MemoryTracker::Instance()
{
   if (instance == NULL)
      instance = new MemoryTracker;
   
   return instance;
}


//------------------------------------------------------------------------------
// void SetShowTrace(bool show)
//------------------------------------------------------------------------------
void MemoryTracker::SetShowTrace(bool show)
{
   showTrace = show;
}


//------------------------------------------------------------------------------
// void Add(void *addr, const std::string &objName, const std::string &funName,
//          const std::string &note)
//------------------------------------------------------------------------------
void MemoryTracker::Add(void *addr, const std::string &objName,
                        const std::string &funName, const std::string &note)
{
   if (showTrace)
   {
      MessageInterface::ShowMessage
         ("+++ Creating <%p> %-20s in %s  %s\n", addr, objName.c_str(),
          funName.c_str(), note.c_str());
   }
   
   TrackType track("+++", addr, objName, funName, note);
   memoryTracks.push_back(track);
}


//------------------------------------------------------------------------------
// void Remove(void *addr, const std::string &objName, const std::string &funName,
//             const std::string &note)
//------------------------------------------------------------------------------
void MemoryTracker::Remove(void *addr, const std::string &objName,
                           const std::string &funName, const std::string &note)
{
   if (showTrace)
   {
      MessageInterface::ShowMessage
         ("--- Deleting <%p> %-20s in %s %s\n", addr, objName.c_str(),
          funName.c_str(), note.c_str());
   }
   
   bool trackFound = false;
   std::vector<TrackType>::iterator track = memoryTracks.begin();
   while (track != memoryTracks.end())
   {
      if ((*track).address == addr)
      {
         memoryTracks.erase(track);
         trackFound = true;
         break;
      }
      
      ++track;
   }
   
   if (!trackFound)
   {
      TrackType track("---", addr, objName, funName, note);
      memoryTracks.push_back(track);
   }
   
}


//------------------------------------------------------------------------------
// StringArray& GetTracks()
//------------------------------------------------------------------------------
StringArray& MemoryTracker::GetTracks()
{
   allTracks.clear();
   std::vector<TrackType>::iterator track = memoryTracks.begin();
   static char text[100];
   while (track != memoryTracks.end())
   {
      sprintf(text, "%s <%p> %-20s %-s  %s", ((*track).preface).c_str(), (*track).address,
              ((*track).objectName).c_str(), ((*track).functionName).c_str(),
              ((*track).remark).c_str());
      allTracks.push_back(text);
      ++track;
   }
   
   return allTracks;
}


//------------------------------------------------------------------------------
//  MemoryTracker()
//------------------------------------------------------------------------------
MemoryTracker::MemoryTracker()
{
   showTrace = false;
}

//------------------------------------------------------------------------------
//  ~MemoryTracker()
//------------------------------------------------------------------------------
MemoryTracker::~MemoryTracker()
{
   memoryTracks.clear();
}
