//$Id$
//------------------------------------------------------------------------------
//                              ForPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number S-67573-G
//
// Author: Allison Greene
// Created: 2004/05/17
//
/**
 * Declares the For loop setup window.
 */
//------------------------------------------------------------------------------
#ifndef ForPanel_hpp
#define ForPanel_hpp

// gui includes
#include "gmatwxdefs.hpp"
#include "GmatAppData.hpp"
#include "GmatPanel.hpp"
#include "Parameter.hpp"
#include "MessageInterface.hpp"

// base includes
#include "gmatdefs.hpp"
#include "GmatCommand.hpp"
#include "For.hpp"

class ForPanel : public GmatPanel
{
public:
   // constructors
   ForPanel(wxWindow *parent, GmatCommand *cmd);
   ~ForPanel();  

private:

   enum GridColumn
   {
      INDEX_SEL_COL = 0,
      INDEX_COL,
      START_SEL_COL,
      START_COL,
      INCR_SEL_COL,
      INCR_COL,
      END_SEL_COL,
      END_COL,
      MAX_COL,
   };
   
   wxString mIndexString;
   wxString mStartString;
   wxString mEndString;
   wxString mIncrString;
   wxGrid *conditionGrid;
   
   bool mIndexIsSet;
   bool mIndexIsParam;
   bool mStartIsParam;
   bool mEndIsParam;
   bool mIncrIsParam;
   
   Real mStartValue;
   Real mIncrValue;
   Real mEndValue;  
   
   wxArrayString mObjectTypeList;
   For *theForCommand;
   Parameter* mIndexParam;  
   
   // methods inherited from GmatPanel
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   
   // data handling methods
   void GetNewValue(Integer row, Integer col);
   
   // event handling methods
   void OnCellLeftClick(wxGridEvent& event);
   void OnCellRightClick(wxGridEvent& event);
   void OnCellValueChange(wxGridEvent& event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_GRID = 99000
   };
};

#endif // ForPanel_hpp
