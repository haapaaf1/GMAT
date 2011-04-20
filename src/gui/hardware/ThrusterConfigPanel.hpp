//$Id$
//------------------------------------------------------------------------------
//                            ThrusterConfigPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
//
// **Legal**
//
// Developed jointly by NASA/GSFC and Thinking Systems, Inc. under contract
// number NNG04CC06P.
//
//
// Author: Waka Waktola
// Created: 2005/01/06
/**
 * This class contains information needed to setup users spacecraft thruster 
 * parameters.
 */
//------------------------------------------------------------------------------
#ifndef ThrusterConfigPanel_hpp
#define ThrusterConfigPanel_hpp

#include "BurnThrusterPanel.hpp"

class ThrusterConfigPanel: public BurnThrusterPanel
{
public:
   ThrusterConfigPanel(wxWindow *parent, const wxString &name);
   ~ThrusterConfigPanel();
   
protected:
   
   virtual void LoadData();
   virtual void SaveData();
   
private:
};
#endif

