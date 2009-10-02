//$Id$
//------------------------------------------------------------------------------
//                              ThrusterCoefficientDialog
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Waka Waktola
// Created: 2005/01/13
//
/**
 * Declares ThrusterCoefficientDialog class. This class shows dialog window where
 * thruster coefficients can be modified.
 * 
 */
//------------------------------------------------------------------------------
#ifndef ThrusterCoefficientDialog_hpp
#define ThrusterCoefficientDialog_hpp

#include "gmatwxdefs.hpp"
#include "GmatDialog.hpp"

class ThrusterCoefficientDialog : public GmatDialog
{
public:
   ThrusterCoefficientDialog(wxWindow *parent, wxWindowID id, 
                             const wxString &title, GmatBase *obj,
                             const wxString &type);
   
private:
   
   GmatBase    *theObject;
   Integer     coefCount;
   StringArray coefNames;
   RealArray   coefValues;
   
   wxString coefType;
   
   wxGrid *coefGrid;
   
   // methods inherited from GmatDialog
   virtual void Create();
   virtual void LoadData();
   virtual void SaveData();
   virtual void ResetData();
   
   // event handling   
   DECLARE_EVENT_TABLE();
   
   // IDs for the controls and the menu commands
   enum
   {     
      ID_GRID = 30300,
   };
};

#endif
