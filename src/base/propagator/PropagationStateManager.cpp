//$Id$
//------------------------------------------------------------------------------
//                           PropagationStateManager
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
 * Implementation of the PropagationStateManager base class.  This is the class
 * for state managers used in GMAT's propagation subsystem.
 */
//------------------------------------------------------------------------------

#include "PropagationStateManager.hpp"
#include "GmatBase.hpp"

#include "MessageInterface.hpp"
#include "PropagatorException.hpp"

#include "Rvector.hpp"
#include "Rmatrix.hpp"

#define DEBUG_STATE_CONSTRUCTION
//#define DUMP_STATE
//#define DEBUG_OBJECT_UPDATES


PropagationStateManager::PropagationStateManager(Integer size) :
   StateManager         (size)
{
}


PropagationStateManager::~PropagationStateManager()
{
//   for (StringArray::iterator i = elements.begin(); i != elements.end(); ++i)
//      delete (i.second());
}


PropagationStateManager::
         PropagationStateManager(const PropagationStateManager& psm) :
   StateManager         (psm)
{
}


PropagationStateManager& 
         PropagationStateManager::operator=(const PropagationStateManager& psm)
{
   if (this != &psm)
   {
      StateManager::operator=(psm);
   }
   
   return *this;
}


bool PropagationStateManager::SetObject(GmatBase* theObject)
{
   #ifdef DEBUG_STATE_CONSTRUCTION
      MessageInterface::ShowMessage("Setting object %s\n", 
            theObject->GetName().c_str());
   #endif
      
   // Be sure object is not already in the list 
   if (find(objects.begin(), objects.end(), theObject) != objects.end())
      return false;     // Could throw here, but that would stop everything

   // todo: validate that theObject can be propagated 

   objects.push_back(theObject);
   if (theObject->IsOfType(Gmat::FORMATION))
   {
      Integer id = theObject->GetParameterID("A1Epoch");
      epochIDs.push_back(id);
   }
   else
   {
      Integer id = theObject->GetParameterID("Epoch");
      if (theObject->GetParameterType(id) != Gmat::REAL_TYPE)
         id = theObject->GetParameterID("A1Epoch");
      epochIDs.push_back(id);
   }
   
   current = theObject;
   StringArray *objectProps = new StringArray;
   elements[current] = objectProps;
   *objectProps = current->GetDefaultPropItems();
   
   #ifdef DEBUG_STATE_CONSTRUCTION
      MessageInterface::ShowMessage("Object set; current points to %s\n", 
         current->GetName().c_str());
   #endif

   return true;
}


bool PropagationStateManager::SetProperty(std::string propName)
{
   #ifdef DEBUG_STATE_CONSTRUCTION
      MessageInterface::ShowMessage("Entered SetProperty(%s); current = %ld\n",
            propName.c_str(), current);
   #endif
   if (current) 
   {
      // Validate that the property can be propagated
      if (current->SetPropItem(propName) == Gmat::UNKNOWN_STATE)
         throw PropagatorException(propName 
               + " is not a known propagation parameter on " 
               + current->GetName());
      elements[current]->push_back(propName);
      
      #ifdef DEBUG_STATE_CONSTRUCTION
         MessageInterface::ShowMessage("Current property List:\n");
            for (StringArray::iterator i = elements[current]->begin(); 
                  i != elements[current]->end(); ++i)
               MessageInterface::ShowMessage("   %s\n", i->c_str());
      #endif
            
      return true;
   }
   
   return false;   
}


bool PropagationStateManager::BuildState()
{
   #ifdef DEBUG_STATE_CONSTRUCTION
      MessageInterface::ShowMessage("Entered BuildState()\n");
   #endif
   
   // Determine the size of the propagation state vector
   stateSize = SortVector();
   
   std::map<std::string,Integer> associateMap;
   // Build the associate map
   std::string name;
   for (Integer index = 0; index < stateSize; ++index)
   {
      name = stateMap[index]->objectName;
      if (associateMap.find(name) == associateMap.end())
         associateMap[name] = index;
   }   
   
   state.SetSize(stateSize);
   for (Integer index = 0; index < stateSize; ++index)
   {
      name = stateMap[index]->objectName;
      std::stringstream sel("");
      sel << stateMap[index]->subelement;
      state.SetElementProperties(index, stateMap[index]->elementID, 
            name + "." + stateMap[index]->elementName + "." + sel.str(), 
            associateMap[name]);
   }
   
   #ifdef DEBUG_STATE_CONSTRUCTION
      MessageInterface::ShowMessage(
            "Propagation state vector has %d elements:\n", stateSize);
      StringArray props = state.GetElementDescriptions();
      for (Integer index = 0; index < stateSize; ++index)
         MessageInterface::ShowMessage("   %d:  %s\n", index, 
               props[index].c_str());
   #endif
   
   #ifdef DUMP_STATE
      MapObjectsToVector();
      for (Integer i = 0; i < stateSize; ++i)
         MessageInterface::ShowMessage("State[%02d] = %.12lf, %s %d\n", i, state[i], 
               "RefState start =", state.GetAssociateIndex(i));
   #endif   
      
   return true;
}


bool PropagationStateManager::MapObjectsToVector()
{
   for (Integer index = 0; index < stateSize; ++index)
   {
      switch (stateMap[index]->parameterType)
      {
         case Gmat::REAL_TYPE:
            state[index] = 
               stateMap[index]->object->GetRealParameter(
                     stateMap[index]->parameterID);
            break;
            
         case Gmat::RMATRIX_TYPE:
            state[index] = 
               stateMap[index]->object->GetRealParameter(
                     stateMap[index]->parameterID, stateMap[index]->rowIndex,
                     stateMap[index]->colIndex);
            break;

         default:
            std::stringstream sel("");
            sel << stateMap[index]->subelement;
            std::string label = stateMap[index]->objectName + "." + 
                  stateMap[index]->elementName + "." + sel.str();
            MessageInterface::ShowMessage(
                  "%s not set; Element type not handled\n",label.c_str());
      }
   }
   
   // Manage epoch
   GmatEpoch theEpoch = 0.0;
   for (UnsignedInt i = 0; i < objects.size(); ++i)
   {
      if (i == 0)
         theEpoch = objects[i]->GetRealParameter(epochIDs[i]);
      else
         if (theEpoch != objects[i]->GetRealParameter(epochIDs[i]))
            // should throw here
            MessageInterface::ShowMessage("Epoch mismatch\n");
   }
   state.SetEpoch(theEpoch);
   
   #ifdef DEBUG_OBJECT_UPDATES
      MessageInterface::ShowMessage(
            "After mapping objects to vector, contents are\n"
            "   Epoch = %.12lf\n", state.GetEpoch());
      for (Integer index = 0; index < stateSize; ++index)
      {
         std::stringstream msg("");
         msg << stateMap[index]->subelement;
         std::string lbl = stateMap[index]->objectName + "." + 
            stateMap[index]->elementName + "." + msg.str() + " = ";
         MessageInterface::ShowMessage("   %d: %s%.12lf\n", index, lbl.c_str(), 
               state[index]);
      }
   #endif

   return true;
}

bool PropagationStateManager::MapVectorToObjects()
{
   #ifdef DEBUG_OBJECT_UPDATES
      MessageInterface::ShowMessage("Mapping vector to objects\n"
            "   Epoch = %.12lf\n", state.GetEpoch());
   #endif

   for (Integer index = 0; index < stateSize; ++index)
   {
      #ifdef DEBUG_OBJECT_UPDATES
         std::stringstream msg("");
         msg << stateMap[index]->subelement;
         std::string lbl = stateMap[index]->objectName + "." + 
            stateMap[index]->elementName + "." + msg.str() + " = ";
         MessageInterface::ShowMessage("   %d: %s%.12lf\n", index, lbl.c_str(), 
               state[index]);
      #endif

      switch (stateMap[index]->parameterType)
      {
         case Gmat::REAL_TYPE:
            stateMap[index]->object->SetRealParameter(
                     stateMap[index]->parameterID, state[index]);
            break;
            
         case Gmat::RMATRIX_TYPE:
            stateMap[index]->object->SetRealParameter(
                     stateMap[index]->parameterID, state[index], 
                     stateMap[index]->rowIndex, stateMap[index]->colIndex);
            break;

         default:
            std::stringstream sel("");
            sel << stateMap[index]->subelement;
            std::string label = stateMap[index]->objectName + "." + 
                  stateMap[index]->elementName + "." + sel.str();
            MessageInterface::ShowMessage(
                  "%s not set; Element type not handled\n",label.c_str());
      }
   }

   GmatEpoch theEpoch = state.GetEpoch();
   for (UnsignedInt i = 0; i < objects.size(); ++i)
      objects[i]->SetRealParameter(epochIDs[i], theEpoch);

   return true;
}


const std::vector<ListItem*>* PropagationStateManager::GetStateMap()
{
   return &stateMap;
}


Integer PropagationStateManager::SortVector()
{
   StringArray *propList;
   std::vector<Integer> order;
   std::vector<Gmat::StateElementId> idList;
   ObjectArray owners;
   StringArray property;
   std::vector<Integer>::iterator oLoc;

   Gmat::StateElementId id;
   Integer size, loc = 0, val;
   stateSize = 0;
   
   // First build a list of the property IDs and objects, measuring state size 
   // at the same time
   for (std::map<GmatBase*, StringArray*>::iterator i = elements.begin(); 
         i != elements.end(); ++i)
   {
      current  = i->first;
      propList = i->second;
      
      for (StringArray::iterator j = propList->begin(); 
            j != propList->end(); ++j)
      {
         id = (Gmat::StateElementId)current->SetPropItem(*j);
         if (id == Gmat::UNKNOWN_STATE)
            throw PropagatorException("Unknown state element: " + (*j));
         size = current->GetPropItemSize(id);
         stateSize += size;
         for (Integer k = 0; k < size; ++k)
         {
            idList.push_back(id);
            owners.push_back(current);
            property.push_back(*j);

            // Put this item in the ordering list
            oLoc = order.begin();
            while (oLoc != order.end())
            {
               val = idList[*oLoc];
               if (id < val)
               {
                  #ifdef DEBUG_STATE_CONSTRUCTION
                     MessageInterface::ShowMessage("Inserting; id = %d, z = %d,"
                           "  loc = %d\n", id, (*oLoc), loc);
                  #endif
                     
                  order.insert(oLoc, loc);
                  break;
               }
               ++oLoc;
            }
            if (oLoc == order.end())
               order.push_back(loc);
            
            ++loc;
         }
      }
   }
   
   ListItem *newItem;
   val = 0;
   
   for (Integer i = 0; i < stateSize; ++i)
   {
      #ifdef DEBUG_STATE_CONSTRUCTION
         MessageInterface::ShowMessage("%d <- %d: %d %s.%s gives ", i, order[i], 
               idList[order[i]], owners[order[i]]->GetName().c_str(), 
               property[order[i]].c_str());
      #endif
         
      newItem = new ListItem;
      newItem->objectName  = owners[order[i]]->GetName();
      newItem->elementName = property[order[i]];
      newItem->object      = owners[order[i]];
      newItem->elementID   = idList[order[i]];
      newItem->subelement  = ++val;
      newItem->parameterID = 
            owners[order[i]]->GetParameterID(property[order[i]]);
      newItem->parameterType = 
            owners[order[i]]->GetParameterType(newItem->parameterID);
      
      if (newItem->parameterType == Gmat::REAL_TYPE)
         newItem->parameterID += val - 1;

      #ifdef DEBUG_STATE_CONSTRUCTION
         MessageInterface::ShowMessage("[%s, %s, %d, %d, %d, %d]\n", 
               newItem->objectName.c_str(),
               newItem->elementName.c_str(),
               newItem->elementID,
               newItem->subelement,
               newItem->parameterID,
               newItem->parameterType); 
      #endif

      if (newItem->parameterType == Gmat::RVECTOR_TYPE)
      {
         const Rmatrix mat = 
            owners[order[i]]->GetRmatrixParameter(property[order[i]]);
         newItem->rowLength = mat.GetNumColumns();
         newItem->rowIndex = val-1; 
      }

      if (newItem->parameterType == Gmat::RMATRIX_TYPE)
      {
         const Rmatrix mat = 
            owners[order[i]]->GetRmatrixParameter(property[order[i]]);
         newItem->rowLength = mat.GetNumColumns();
         newItem->colIndex = (val-1) % newItem->rowLength;
         newItem->rowIndex = (Integer)((val - 1) / newItem->rowLength);
         
         #ifdef DEBUG_STATE_CONSTRUCTION
            MessageInterface::ShowMessage(
                  "RowLen = %d, %d -> row %2d  col %2d\n", newItem->rowLength, 
                  val, newItem->rowIndex, newItem->colIndex); 
         #endif
      }
      
      newItem->length      = owners[order[i]]->GetPropItemSize(idList[order[i]]);
      
      if (val == newItem->length)
         val = 0;
      
      stateMap.push_back(newItem);
   }

   #ifdef DEBUG_STATE_CONSTRUCTION
      MessageInterface::ShowMessage("State map contents:\n");
      for (std::vector<ListItem*>::iterator i = stateMap.begin(); 
            i != stateMap.end(); ++i)
         MessageInterface::ShowMessage("   %s %s %d %d of %d, id = %d\n", 
               (*i)->objectName.c_str(), (*i)->elementName.c_str(),
               (*i)->elementID, (*i)->subelement, (*i)->length, 
               (*i)->parameterID); 
   #endif
   
   return stateSize;
}
