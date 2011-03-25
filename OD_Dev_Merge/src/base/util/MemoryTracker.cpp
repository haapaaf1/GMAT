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
// void SetScript(const std::string &script)
//------------------------------------------------------------------------------
void MemoryTracker::SetScript(const std::string &script)
{
   scriptFile = script;
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
//          const std::string &note, void *from)
//------------------------------------------------------------------------------
void MemoryTracker::Add(void *addr, const std::string &objName,
                        const std::string &funName, const std::string &note,
                        void *from)
{
   if (showTrace)
   {
      if (from == NULL)
      {
         MessageInterface::ShowMessage
            ("+++ Creating <%p> %-20s in %s  %s\n", addr, objName.c_str(),
             funName.c_str(), note.c_str());
      }
      else
      {
         MessageInterface::ShowMessage
            ("+++ Creating <%p> %-20s in %s  %s from <%p>\n", addr, objName.c_str(),
             funName.c_str(), note.c_str(), from);
      }
   }
   
   TrackType track("+++", addr, objName, funName, note, scriptFile);
   memoryTracks.push_back(track);
}


//------------------------------------------------------------------------------
// void Remove(void *addr, const std::string &objName, const std::string &funName,
//             const std::string &note, void *from)
//------------------------------------------------------------------------------
void MemoryTracker::Remove(void *addr, const std::string &objName,
                           const std::string &funName, const std::string &note,
                           void *from)
{
   if (showTrace)
   {
      if (from == NULL)
      {
         MessageInterface::ShowMessage
            ("--- Deleting <%p> %-20s in %s %s\n", addr, objName.c_str(),
             funName.c_str(), note.c_str());
      }
      else
      {
         MessageInterface::ShowMessage
            ("--- Deleting <%p> %-20s in %s %s from <%p>\n", addr, objName.c_str(),
             funName.c_str(), note.c_str(), from);
      }
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
      TrackType track("---", addr, objName, funName, note, scriptFile);
      memoryTracks.push_back(track);
   }
   
}


//------------------------------------------------------------------------------
// StringArray& GetTracks(bool clearTracks, bool writeScriptName)
//------------------------------------------------------------------------------
StringArray& MemoryTracker::GetTracks(bool clearTracks, bool writeScriptName)
{
   allTracks.clear();
   std::vector<TrackType>::iterator track = memoryTracks.begin();
   static char text[400];
   Integer trackCount = 0;

   #ifdef DEBUG_GET_TRACKS
   MessageInterface::ShowMessage
      ("===> MemoryTracker::GetTracks() There are %d memory tracks\n",
       memoryTracks.size());
   #endif
   
   while (track != memoryTracks.end() && trackCount < 1000)
   {
      trackCount++;
      std::string script = "";
      if (writeScriptName)
         script = (*track).scriptName;
      sprintf(text, "%s <%p> %-20s %-s  %s %s", ((*track).preface).c_str(), (*track).address,
              ((*track).objectName).c_str(), ((*track).functionName).c_str(),
              ((*track).remark).c_str(), script.c_str());
      allTracks.push_back(text);
      ++track;
   }
   
   if (clearTracks)
      memoryTracks.clear();
   
   #ifdef DEBUG_GET_TRACKS
   MessageInterface::ShowMessage
      ("===> MemoryTracker::GetTracks() returning %d memory tracks\n", allTracks.size());
   #endif
   
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
