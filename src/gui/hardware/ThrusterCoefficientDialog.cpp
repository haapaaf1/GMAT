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
 * Implements ThrusterCoefficientDialog class. This class shows dialog window where
 * thruster coefficients can be modified.
 * 
 */
//------------------------------------------------------------------------------

#include "ThrusterCoefficientDialog.hpp"
#include "StringUtil.hpp"
#include "MessageInterface.hpp"
#include <wx/variant.h>

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------
BEGIN_EVENT_TABLE(ThrusterCoefficientDialog, GmatDialog)
   EVT_GRID_CELL_CHANGE(ThrusterCoefficientDialog::OnCellValueChange)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// ThrusterCoefficientDialog(wxWindow *parent, wxWindowID id, 
//                           const wxString &title, GmatBase *obj,
//                           const std::string &type)
//------------------------------------------------------------------------------
ThrusterCoefficientDialog::
ThrusterCoefficientDialog(wxWindow *parent, wxWindowID id, 
                          const wxString &title, GmatBase *obj,
                          const wxString &type)
   : GmatDialog(parent, id, title, obj, wxDefaultPosition, wxDefaultSize)
{
   coefType = type;
   theObject = obj;
   
   coefName.clear();
   coefValue.clear();
   
   if (obj != NULL)
   {
      Create();
      ShowData();
   }
}

//------------------------------------------------------------------------------
// void Create()
//------------------------------------------------------------------------------
void ThrusterCoefficientDialog::Create()
{
   coefGrid =
      new wxGrid( this, ID_GRID, wxDefaultPosition, wxDefaultSize, wxWANTS_CHARS);

   int coefCount = Thruster::COEFFICIENT_COUNT;
   
   coefGrid->EnableDragGridSize(false);
   coefGrid->EnableDragColSize(false);
   coefGrid->CreateGrid(coefCount, 3);
   coefGrid->SetRowLabelSize(0);
   coefGrid->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTRE);
   
   coefGrid->SetColLabelValue(0, _T("Coefficient"));
   coefGrid->SetColSize(0, 70);
   coefGrid->SetColLabelValue(1, _T("Value"));
   coefGrid->SetColSize(1, 135);
   coefGrid->SetColLabelValue(2, _T("Unit"));
   coefGrid->SetColSize(2, 80);
   
   // The first and third columns are read only
   for (int i=0; i<coefCount; i++)
   {
      coefGrid->SetReadOnly(i, 0);
      coefGrid->SetReadOnly(i, 2);
   }
   
   // wxSizers   
   theMiddleSizer->Add(coefGrid, 0, wxALIGN_CENTRE|wxGROW|wxALL, 3);
}

//------------------------------------------------------------------------------
// virtual void LoadData()
//------------------------------------------------------------------------------
void ThrusterCoefficientDialog::LoadData()
{
   Integer paramID = 0;
   int coefCount = Thruster::COEFFICIENT_COUNT;
   
   if (coefType == "C")
   {
      paramID = theObject->GetParameterID("C1");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C1");
      
      paramID = theObject->GetParameterID("C2");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C2");
      
      paramID = theObject->GetParameterID("C3");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C3");
      
      paramID = theObject->GetParameterID("C4");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C4");
      
      paramID = theObject->GetParameterID("C5");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C5");
      
      paramID = theObject->GetParameterID("C6");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C6");
      
      paramID = theObject->GetParameterID("C7");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C7");
      
      paramID = theObject->GetParameterID("C8");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C8");
      
      paramID = theObject->GetParameterID("C9");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C9");
      
      paramID = theObject->GetParameterID("C10");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C10");
      
      paramID = theObject->GetParameterID("C11");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C11");
      
      paramID = theObject->GetParameterID("C12");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C12");
      
      paramID = theObject->GetParameterID("C13");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C13");
      
      paramID = theObject->GetParameterID("C14");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C14");
   
      paramID = theObject->GetParameterID("C15");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C15");
      
      paramID = theObject->GetParameterID("C16");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("C16");

      paramID = theObject->GetParameterID("C_UNITS");
      StringArray coefUnits = theObject->GetStringArrayParameter(paramID);
      
      for (Integer i = 0; i < coefCount; i++)
      {
         coefGrid->SetCellValue(i, 0, coefName[i].c_str());
         coefGrid->SetCellValue(i, 1, wxVariant(coefValue[i]));
         coefGrid->SetCellValue(i, 2, coefUnits[i].c_str());
      }
   }
   else if (coefType == "K")
   {
      paramID = theObject->GetParameterID("K1");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K1");
      
      paramID = theObject->GetParameterID("K2");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K2");
      
      paramID = theObject->GetParameterID("K3");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K3");
      
      paramID = theObject->GetParameterID("K4");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K4");
      
      paramID = theObject->GetParameterID("K5");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K5");
      
      paramID = theObject->GetParameterID("K6");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K6");
      
      paramID = theObject->GetParameterID("K7");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K7");
      
      paramID = theObject->GetParameterID("K8");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K8");
      
      paramID = theObject->GetParameterID("K9");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K9");
      
      paramID = theObject->GetParameterID("K10");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K10");
      
      paramID = theObject->GetParameterID("K11");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K11");
      
      paramID = theObject->GetParameterID("K12");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K12");
      
      paramID = theObject->GetParameterID("K13");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K13");
      
      paramID = theObject->GetParameterID("K14");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K14");
      
      paramID = theObject->GetParameterID("K15");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K15");
      
      paramID = theObject->GetParameterID("K16");
      coefValue.push_back(theObject->GetRealParameter(paramID));
      coefName.push_back("K16");
      
      paramID = theObject->GetParameterID("K_UNITS");
      StringArray coefUnits = theObject->GetStringArrayParameter(paramID);
      
      for (Integer i = 0; i < coefCount; i++)
      {
         coefGrid->SetCellValue(i, 0, coefName[i].c_str());
         coefGrid->SetCellValue(i, 1, wxVariant(coefValue[i]));
         coefGrid->SetCellValue(i, 2, coefUnits[i].c_str());
      }
   }
}

//------------------------------------------------------------------------------
// virtual void SaveData()
//------------------------------------------------------------------------------
void ThrusterCoefficientDialog::SaveData()
{
   Integer paramID = 0;
   
   if (coefType == "C")
   {
      paramID = theObject->GetParameterID("C1");
      theObject->SetRealParameter(paramID, coefValue[0]);
      
      paramID = theObject->GetParameterID("C2");
      theObject->SetRealParameter(paramID, coefValue[1]);
      
      paramID = theObject->GetParameterID("C3");
      theObject->SetRealParameter(paramID, coefValue[2]);
      
      paramID = theObject->GetParameterID("C4");
      theObject->SetRealParameter(paramID, coefValue[3]);
      
      paramID = theObject->GetParameterID("C5");
      theObject->SetRealParameter(paramID, coefValue[4]);
      
      paramID = theObject->GetParameterID("C6");
      theObject->SetRealParameter(paramID, coefValue[5]);
      
      paramID = theObject->GetParameterID("C7");
      theObject->SetRealParameter(paramID, coefValue[6]);
      
      paramID = theObject->GetParameterID("C8");
      theObject->SetRealParameter(paramID, coefValue[7]);
      
      paramID = theObject->GetParameterID("C9");
      theObject->SetRealParameter(paramID, coefValue[8]);
      
      paramID = theObject->GetParameterID("C10");
      theObject->SetRealParameter(paramID, coefValue[9]);
      
      paramID = theObject->GetParameterID("C11");
      theObject->SetRealParameter(paramID, coefValue[10]);
      
      paramID = theObject->GetParameterID("C12");
      theObject->SetRealParameter(paramID, coefValue[11]);
      
      paramID = theObject->GetParameterID("C13");
      theObject->SetRealParameter(paramID, coefValue[12]);
      
      paramID = theObject->GetParameterID("C14");
      theObject->SetRealParameter(paramID, coefValue[13]);
      
      paramID = theObject->GetParameterID("C15");
      theObject->SetRealParameter(paramID, coefValue[14]);
      
      paramID = theObject->GetParameterID("C16");
      theObject->SetRealParameter(paramID, coefValue[15]);
   }
   else if (coefType == "K")
   {
      paramID = theObject->GetParameterID("K1");
      theObject->SetRealParameter(paramID, coefValue[0]);
 
      paramID = theObject->GetParameterID("K2");
      theObject->SetRealParameter(paramID, coefValue[1]);
      
      paramID = theObject->GetParameterID("K3");
      theObject->SetRealParameter(paramID, coefValue[2]);
      
      paramID = theObject->GetParameterID("K4");
      theObject->SetRealParameter(paramID, coefValue[3]);
      
      paramID = theObject->GetParameterID("K5");
      theObject->SetRealParameter(paramID, coefValue[4]);
      
      paramID = theObject->GetParameterID("K6");
      theObject->SetRealParameter(paramID, coefValue[5]);
      
      paramID = theObject->GetParameterID("K7");
      theObject->SetRealParameter(paramID, coefValue[6]);
      
      paramID = theObject->GetParameterID("K8");
      theObject->SetRealParameter(paramID, coefValue[7]);
      
      paramID = theObject->GetParameterID("K9");
      theObject->SetRealParameter(paramID, coefValue[8]);
      
      paramID = theObject->GetParameterID("K10");
      theObject->SetRealParameter(paramID, coefValue[9]);
      
      paramID = theObject->GetParameterID("K11");
      theObject->SetRealParameter(paramID, coefValue[10]);
      
      paramID = theObject->GetParameterID("K12");
      theObject->SetRealParameter(paramID, coefValue[11]);
      
      paramID = theObject->GetParameterID("K13");
      theObject->SetRealParameter(paramID, coefValue[12]);
      
      paramID = theObject->GetParameterID("K14");
      theObject->SetRealParameter(paramID, coefValue[13]);
      
      paramID = theObject->GetParameterID("K15");
      theObject->SetRealParameter(paramID, coefValue[14]);
      
      paramID = theObject->GetParameterID("K16");
      theObject->SetRealParameter(paramID, coefValue[15]);
   }        
}  

//------------------------------------------------------------------------------
// virtual void ResetData()
//------------------------------------------------------------------------------
void ThrusterCoefficientDialog::ResetData()
{
}     

//------------------------------------------------------------------------------
// virtual void OnCellValueChange()
//------------------------------------------------------------------------------
void ThrusterCoefficientDialog::OnCellValueChange(wxGridEvent &event)
{
    int row = event.GetRow();
    int col = event.GetCol();
    
    if (col == 1)
    {
       Real rvalue;
       std::string fieldName = coefName[row].c_str();
       std::string inputString(coefGrid->GetCellValue(row, col).c_str());
       if (CheckReal(rvalue, inputString, fieldName, "Real Number"))
       {
          coefValue[row] = rvalue;
          EnableUpdate(true);
          mDataUpdated = true;
       }
    }    
}
