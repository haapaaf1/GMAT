//$Header: 
//------------------------------------------------------------------------------
//                              IfPanel
//------------------------------------------------------------------------------
// GMAT: Goddard Mission Analysis Tool
//
// Author: Allison Greene
// Created: 2004/05/17
/**
 * This class contains the Conditional Statement Setup window.
 */
//------------------------------------------------------------------------------

#include "IfPanel.hpp"
#include "ParameterSelectDialog.hpp"
#include "gmatdefs.hpp"
#include "GmatAppData.hpp"

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(IfPanel, GmatPanel)
    EVT_GRID_CELL_LEFT_DCLICK(IfPanel::OnCellDoubleLeftClick)
    EVT_GRID_CELL_CHANGE(IfPanel::OnCellValueChange)
END_EVENT_TABLE()

//------------------------------------------------------------------------------
// IfPanel()
//------------------------------------------------------------------------------
/**
 * A constructor.
 */
//------------------------------------------------------------------------------
IfPanel::IfPanel(wxWindow *parent, GmatCommand *cmd) : GmatPanel(parent)
{
   theIfCommand = (If *)cmd;
   
   mNumberOfConditions = 0;
   mNumberOfLogicalOps = 0;
   mLhsList.clear();
   mOpStrings.clear();
   mRhsList.clear();
   mLogicalOpStrings.clear();
   
   Create();
   Show();
}

IfPanel::~IfPanel()
{
}

//-------------------------------
// private methods
//-------------------------------
void IfPanel::Create()
{
    Setup(this);    
}

void IfPanel::Setup( wxWindow *parent)
{
    wxBoxSizer *item0 = new wxBoxSizer( wxVERTICAL );

    conditionGrid = new wxGrid( parent, ID_GRID, wxDefaultPosition, 
                    wxSize(454,238), wxWANTS_CHARS );
    conditionGrid->CreateGrid( 10, 4, wxGrid::wxGridSelectCells );
    conditionGrid->SetRowLabelSize(0);
    conditionGrid->SetDefaultCellAlignment(wxALIGN_CENTRE, wxALIGN_CENTRE);
    conditionGrid->EnableEditing(false);
        
    conditionGrid->SetColLabelValue(0, _T(""));
    conditionGrid->SetColSize(0, 60);
    conditionGrid->SetColLabelValue(1, _T("LHS"));
    conditionGrid->SetColSize(1, 165);
    conditionGrid->SetColLabelValue(2, _T("Condition"));
    conditionGrid->SetColSize(2, 60);
    conditionGrid->SetColLabelValue(3, _T("RHS"));
    conditionGrid->SetColSize(3, 165);
    conditionGrid->SetCellValue(0, COMMAND_COL, "If");
        
    item0->Add( conditionGrid, 0, wxALIGN_CENTER|wxALL, 5 );

    theMiddleSizer->Add(item0, 0, wxGROW, 5);
}

//------------------------------------------------------------------------------
// void LoadData()
//------------------------------------------------------------------------------
void IfPanel::LoadData()
{
    if (theIfCommand != NULL)
    {
       Integer paramId;
       paramId = theIfCommand->GetParameterID("NumberOfConditions");
       mNumberOfConditions = theIfCommand->GetIntegerParameter(paramId);
       paramId = theIfCommand->GetParameterID("NumberOfLogicalOperators");
       mNumberOfLogicalOps = theIfCommand->GetIntegerParameter(paramId);       
       paramId = theIfCommand->GetParameterID("LeftHandStrings");
       mLhsList = theIfCommand->GetStringArrayParameter(paramId);

       paramId = theIfCommand->GetParameterID("OperatorStrings");
       mOpStrings = theIfCommand->GetStringArrayParameter(paramId);
       paramId = theIfCommand->GetParameterID("RightHandStrings");
       mRhsList = theIfCommand->GetStringArrayParameter(paramId);
       paramId = theIfCommand->GetParameterID("LogicalOperators");
       mLogicalOpStrings = theIfCommand->GetStringArrayParameter(paramId);
       
       conditionGrid->SetCellValue(0, LHS_COL, mLhsList[0].c_str()); 
       conditionGrid->SetCellValue(0, COND_COL, mOpStrings[0].c_str()); 
       conditionGrid->SetCellValue(0, RHS_COL, mRhsList[0].c_str());   
    }        
}

//------------------------------------------------------------------------------
// void SaveData()
//------------------------------------------------------------------------------
void IfPanel::SaveData()
{
       Integer paramId;
       paramId = theIfCommand->GetParameterID("NumberOfConditions");
       theIfCommand->SetIntegerParameter(paramId, mNumberOfConditions);
       paramId = theIfCommand->GetParameterID("NumberOfLogicalOperators");
       theIfCommand->SetIntegerParameter(paramId, mNumberOfLogicalOps);       
}

//------------------------------------------------------------------------------
// void OnCellDoubleLeftClick(wxGridEvent& event)
//------------------------------------------------------------------------------
void IfPanel::OnCellDoubleLeftClick(wxGridEvent& event)
{
   int row = event.GetRow();
   int col = event.GetCol();
   
   if (col == 1)
   {
      conditionGrid->EnableEditing(false);
      
      // show dialog to select parameter
      ParameterSelectDialog paramDlg(this);
      paramDlg.ShowModal();

      if (paramDlg.IsParamSelected())
      {
         wxString newParamName = paramDlg.GetParamName();
         conditionGrid->SetCellValue(row, col, newParamName);
         theApplyButton->Enable(true);
      }
   }
   else if (col == 2)
   {
      conditionGrid->EnableEditing(false);
      
      wxString strArray[] =
      {
         wxT("="),
         wxT(">"),
         wxT("<"),
         wxT(">="),
         wxT("<="),
         wxT("!=")
      };
         
      wxSingleChoiceDialog dialog(this, _T("Condition: \n"),
                                        _T("IfConditionDialog"), 6, strArray);
      dialog.SetSelection(0);

      if (dialog.ShowModal() == wxID_OK)
      {
         if (dialog.GetStringSelection() != conditionGrid->GetCellValue(row, col))
         {
            conditionGrid->SetCellValue(row, col, dialog.GetStringSelection());
            theApplyButton->Enable(true);
         }
      }   
   }
   else if (col == 3)
   {
      conditionGrid->EnableEditing(true);
   }       
}

//------------------------------------------------------------------------------
// OnCellValueChange(wxGridEvent& event)
//------------------------------------------------------------------------------
void IfPanel::OnCellValueChange(wxGridEvent& event)
{
    theApplyButton->Enable(true);
    conditionGrid->EnableEditing(false);    
}
