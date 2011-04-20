//$Id$
//------------------------------------------------------------------------------
//                              ImpulsiveBurnSetupPanel
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Author: LaMont Ruley
// Created: 2004/02/04
//
/**
 * This class contains the Impulsive Burn Setup window.
 */
//------------------------------------------------------------------------------
#ifndef ImpulsiveBurnSetupPanel_hpp
#define ImpulsiveBurnSetupPanel_hpp

#include "BurnThrusterPanel.hpp"

class ImpulsiveBurnSetupPanel : public BurnThrusterPanel
{
public:
   // constructors
   ImpulsiveBurnSetupPanel(wxWindow *parent, const wxString &name);
   ~ImpulsiveBurnSetupPanel();
    
protected:
   
   virtual void LoadData();
   virtual void SaveData();
   
private:
};
#endif // ImpulsiveBurnSetupPanel_hpp
