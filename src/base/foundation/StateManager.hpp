//$Id$
//------------------------------------------------------------------------------
//                                  StateManager
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool.
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG06CA54C
//
// Author: Darrel J. Conway, Thinking Systems, Inc.
// Created: 2008/12/15
//
/**
 * Definition of the StateManager base class.  This is the class for state 
 * managers used in GMAT's propagators and solvers.
 */
//------------------------------------------------------------------------------

#ifndef StateManager_hpp
#define StateManager_hpp

#include "GmatState.hpp"
#include "gmatdefs.hpp"
#include <map>
#include <vector>

struct ListItem
{
   std::string objectName;
   std::string elementName;
   GmatBase*   object;
   // Gmat::StateElementId
   Integer     elementID;
   Integer     subelement;
   Integer     parameterID;
   Gmat::ParameterType
               parameterType;
   Integer     rowIndex;      // Used for vectors and arrays
   Integer     rowLength;     // Used for vectors and arrays
   Integer     colIndex;      // Used for arrays
   Integer     length;
};


/**
 * The state manager base class.
 */
class StateManager
{
public:
	StateManager(Integer size = 0);
	virtual ~StateManager();
   StateManager(const StateManager& sm);
   StateManager& operator=(const StateManager& sm);
   
   // Abstract methods
   virtual bool SetObject(GmatBase* theObject) = 0;
   virtual bool SetProperty(std::string propName) = 0;
   virtual bool SetProperty(std::string propName, Integer index) = 0;
   virtual bool SetProperty(std::string propName, GmatBase *forObject) = 0;
   virtual bool BuildState() = 0;
   virtual bool MapObjectsToVector() = 0;
   virtual bool MapVectorToObjects() = 0;
   
   virtual Integer GetCount(Gmat::StateElementId elementType = 
                               Gmat::UNKNOWN_STATE);

   virtual bool UpdateState();
   virtual GmatState* GetState();
   virtual Integer GetStateSize();
   
   virtual bool GetStateObjects(ObjectArray& pObjects, 
         Gmat::ObjectType type = Gmat::UNKNOWN_OBJECT);
   
   virtual const StringArray& GetObjectList(std::string ofType = "");
   virtual const std::vector<ListItem*>* GetStateMap();

protected:
   /// Size of the managed state vector
   Integer                    stateSize;
   GmatState                  state;
   
   ObjectArray                objects;
   StringArray                objectNames;

   std::vector<Integer>       epochIDs;
   std::map<GmatBase*, StringArray*>  elements;
   GmatBase*                  current;

   std::vector<ListItem*>     stateMap;
};

#endif /*StateManager_hpp*/
