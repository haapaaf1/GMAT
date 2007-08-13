//$Header$
//------------------------------------------------------------------------------
//                                  Publisher
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: LaMont Ruley
// Created: 2003/10/21
/**
 * Definition for the Publisher class.
 */
//------------------------------------------------------------------------------

#ifndef Publisher_hpp
#define Publisher_hpp

#include "Subscriber.hpp"
#include <list>
#include <vector>


class GMAT_API Publisher
{
public:
   static Publisher*    Instance();

   bool Subscribe(Subscriber *s);
   bool Unsubscribe(Subscriber *s);
   bool UnsubscribeAll();
   
   bool Publish(Integer id, Real *data, Integer count);
   bool Publish(Integer id, char *data, Integer count = 0);
   bool Publish(Integer id, Integer *data, Integer count);

   bool FlushBuffers();
   bool NotifyEndOfRun();
   
   // Interface methods used to identify the data sent to the publisher and
   // subscribers
   Integer              RegisterPublishedData(const StringArray& owners, 
                                              const StringArray& elements);
   const StringArray&   GetStringArrayParameter(const std::string& type);
   
   // Interfaces used to update the state of the running system
   void                 SetRunState(const Gmat::RunState state);
   
//private:
   inline Gmat::RunState GetRunState();

private:
   /// The singleton
   static Publisher        *instance;
   /// List of the subscribers
   std::list<Subscriber*>  subs;
   /// Index used to identify number of registered data providers
   Integer                 providerCount;
   /// ID for the current data provider
   Integer                 currentProvider;
   /// Arrays used to track objects for published data
   std::vector<StringArray> objectMap;
   /// Arrays used to track elements for published data
   std::vector<StringArray> elementMap;
   /// State of the system (used to track data for display or suppression)
   Gmat::RunState           runState;

   void                 UpdateProviderID(Integer newId);
   // default constructor
   Publisher();
   // assignment operator
   Publisher& operator=(const Publisher &right);
   // destructor
   virtual ~Publisher();
};



inline Gmat::RunState Publisher::GetRunState()
{
   return runState; 
}

#endif // Publisher_hpp
