//$Id$
//------------------------------------------------------------------------------
//                              ParameterCreateDialog
//------------------------------------------------------------------------------
// GMAT: General Mission Analysis Tool
//
// Author: Linda Jun
// Created: 2004/02/25
//
/**
 * Implements ParameterCreateDialog class. This class shows dialog window where a
 * user parameter can be created.
 * 
 */
//------------------------------------------------------------------------------

#include "ParameterCreateDialog.hpp"
#include "ParameterSelectDialog.hpp"
#include "GmatStaticBoxSizer.hpp"
#include "RgbColor.hpp"
#include "ParameterInfo.hpp"            // for GetDepObjectType()
#include "StringUtil.hpp"               // for GmatStringUtil::
#include "MessageInterface.hpp"
#include "StringTokenizer.hpp"
#include "Array.hpp"
#include "gmatdefs.hpp"
#include "ArraySetupDialog.hpp"
#include "bitmaps/NewMission.xpm"

#include <wx/tglbtn.h>
#include <wx/notebook.h>
#include <wx/config.h>

//#define DEBUG_PARAM_CREATE_DIALOG 1
//#define DEBUG_PARAM_CREATE_VAR 1
//#define DEBUG_PARAM_CREATE_SAVE 1

//------------------------------------------------------------------------------
// event tables and other macros for wxWindows
//------------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ParameterCreateDialog, GmatDialog)
   EVT_BUTTON(ID_CREATE_BUTTON, ParameterCreateDialog::OnCreateButton)
   EVT_BUTTON(ID_SELECT_BUTTON, ParameterCreateDialog::OnSelectButtonClick)
   EVT_BUTTON(ID_EDITARRAY_BUTTON, ParameterCreateDialog::OnEditArrayButtonClick)
   EVT_BUTTON(ID_CLEAR_VAR_BUTTON, ParameterCreateDialog::OnClearButtonClick)
   EVT_BUTTON(ID_CLEAR_ARR_BUTTON, ParameterCreateDialog::OnClearButtonClick)
   EVT_BUTTON(ID_CLEAR_STR_BUTTON, ParameterCreateDialog::OnClearButtonClick)
   EVT_TEXT(ID_VARTEXTCTRL, ParameterCreateDialog::OnVarTextUpdate)
   EVT_TEXT(ID_ARYTEXTCTRL, ParameterCreateDialog::OnAryTextUpdate)
   EVT_TEXT(ID_STRTEXTCTRL, ParameterCreateDialog::OnStrTextUpdate)
   EVT_NOTEBOOK_PAGE_CHANGED(ID_NOTEBOOK, ParameterCreateDialog::OnPageChanged)
   EVT_LISTBOX(ID_LISTBOX, ParameterCreateDialog::OnListboxClick)
END_EVENT_TABLE()
   
//------------------------------------------------------------------------------
// ParameterCreateDialog(wxWindow *parent, int paramType)
//------------------------------------------------------------------------------
/*
 * @param paramType 1 = Variable, 2 = Array, 3 = String
 */
//------------------------------------------------------------------------------
ParameterCreateDialog::ParameterCreateDialog(wxWindow *parent, e_parameter_type paramType)
   : GmatDialog(parent, -1, wxString(_T("ParameterCreateDialog")))
{
   mParamType = paramType;
   mCurrParam = NULL;
   mParamNames.Clear();
   mIsParamCreated = false;
   mSelectVarStrings.Add("Spacecraft");
   mSelectVarStrings.Add("ImpulsiveBurn");
   
   //mColor.Set(0, 0, 0); // initialize to black
   
   Create(); 
   SetParameterType( paramType );
   ShowData();
}


//------------------------------------------------------------------------------
// ParameterCreateDialog(wxWindow *parent, string param_name)
//------------------------------------------------------------------------------
/*
  */
//------------------------------------------------------------------------------
ParameterCreateDialog::ParameterCreateDialog(wxWindow *parent, const wxString param_name)
   : GmatDialog(parent, -1, wxString(_T("ParameterCreateDialog")))
{
   mObjectName = param_name.c_str();
   mCurrParam = (Parameter*)theGuiInterpreter->GetConfiguredObject(mObjectName);
   if (!mCurrParam)
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "Cannot find the parameter object named " + mObjectName);
   }
   else
   {
      mParamNames.Clear();
      mIsParamCreated = false;
      mSelectVarStrings.Add("Spacecraft");
      mSelectVarStrings.Add("ImpulsiveBurn");
      //mColor.Set(0, 0, 0); // initialize to black
      
      Create(); 
      std::string s = mCurrParam->GetTypeName();
      if (s == "String")
         mParamType = STRING;
      else if (s == "Array")
         mParamType = ARRAY;
      else
         mParamType = VARIABLE;
      SetParameterType( mParamType );
      ShowData();
   }
}


//------------------------------------------------------------------------------
// ~ParameterCreateDialog()
//------------------------------------------------------------------------------
ParameterCreateDialog::~ParameterCreateDialog()
{
   mSelectVarStrings.Clear();   
}


//------------------------------------------------------------------------------
// virtual void Create()
//------------------------------------------------------------------------------
void ParameterCreateDialog::Create()
{
   #if DEBUG_PARAM_CREATE_DIALOG
   MessageInterface::ShowMessage("ParameterCreateDialog::Create() entered\n");
   #endif
   
   #if __WXMAC__
   int buttonWidth = 40;
   #else
   int buttonWidth = 25;
   #endif

   int bsize = 2;
   std::string CreateLabel = "="GUI_ACCEL_KEY">";
   wxBitmap clearBitmap = wxBitmap(NewMission_xpm);
   
   // get the config object
   wxConfigBase *pConfig = wxConfigBase::Get();
   // SetPath() understands ".."
   pConfig->SetPath(wxT("/Parameter"));

   notebook = new wxNotebook(this, ID_NOTEBOOK);
   wxPanel *varPanel = new wxPanel(notebook);
   wxPanel *arrPanel = new wxPanel(notebook);
   wxPanel *strPanel = new wxPanel(notebook);
   //wxStaticText
   wxStaticText *varNameStaticText =
      new wxStaticText(varPanel, ID_TEXT, wxT("Variable "GUI_ACCEL_KEY"Name"),
                        wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *expStaticText =
      new wxStaticText(varPanel, ID_TEXT, wxT("Variable "GUI_ACCEL_KEY"Value"),
                       wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *varEqualSignStaticText =
      new wxStaticText(varPanel, ID_TEXT, wxT("="),
                       wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *arrNameStaticText =
      new wxStaticText(arrPanel, ID_TEXT, wxT("Array "GUI_ACCEL_KEY"Name"),
                        wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *arr1RowStaticText =
      new wxStaticText(arrPanel, ID_TEXT, wxT(GUI_ACCEL_KEY"Row"),
                        wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *arr1ColStaticText =
      new wxStaticText(arrPanel, ID_TEXT, wxT(GUI_ACCEL_KEY"Column"),
                        wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *arrEqualSignStaticText =
      new wxStaticText(arrPanel, ID_TEXT, wxT("="),
                       wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *arrTimesStaticText =
      new wxStaticText(arrPanel, ID_TEXT, wxT(" X"),
                       wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *stringNameLabel =
      new wxStaticText(strPanel, ID_TEXT, wxT("String "GUI_ACCEL_KEY"Name"),
                        wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *stringEqualSignStaticText =
      new wxStaticText(strPanel, ID_TEXT, wxT("="),
                       wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *stringValueLabel =
      new wxStaticText(strPanel, ID_TEXT, wxT("String "GUI_ACCEL_KEY"Value"),
                        wxDefaultPosition, wxDefaultSize, 0);
   wxStaticText *configStringLabel =
      new wxStaticText(strPanel, ID_TEXT, wxT(GUI_ACCEL_KEY"Strings"),
                        wxDefaultPosition, wxDefaultSize, 0);
   
   // wxTextCtrl
   mVarClearButton =
      new wxBitmapButton(varPanel, ID_CLEAR_VAR_BUTTON, clearBitmap, wxDefaultPosition,
                         wxSize(buttonWidth, 20));
   mVarClearButton->SetToolTip(pConfig->Read(_T("ClearVariableHint"), "Clear Variable Fields"));
   
   mVarNameTextCtrl = new wxTextCtrl(varPanel, ID_VARTEXTCTRL, wxT(""),
                                     wxDefaultPosition, wxSize(130,20), 0);
   mVarNameTextCtrl->SetToolTip(pConfig->Read(_T("VariableNameHint")));

   mExprTextCtrl = new wxTextCtrl(varPanel, ID_VARTEXTCTRL, wxT(""),
                                  wxDefaultPosition, wxSize(280,20), 0);
   mExprTextCtrl->SetToolTip(pConfig->Read(_T("VariableValueHint")));
   
   mArrClearButton =
      new wxBitmapButton(arrPanel, ID_CLEAR_ARR_BUTTON, clearBitmap, wxDefaultPosition,
                         wxSize(buttonWidth, 20));
   mArrClearButton->SetToolTip(pConfig->Read(_T("ClearArrayHint"), "Clear Array Fields"));
   
   mArrNameTextCtrl = new wxTextCtrl(arrPanel, ID_ARYTEXTCTRL, wxT(""),
                                     wxDefaultPosition, wxSize(102,20), 0);
   mArrNameTextCtrl->SetToolTip(pConfig->Read(_T("ArrayNameHint")));
   mArrRowTextCtrl = new wxTextCtrl(arrPanel, ID_ARYTEXTCTRL, wxT(""),
                                    wxDefaultPosition, wxSize(50,20), 0, 
                                    wxTextValidator(wxGMAT_FILTER_NUMERIC));
   mArrRowTextCtrl->SetToolTip(pConfig->Read(_T("ArrayRowValueHint")));
   mArrColTextCtrl = new wxTextCtrl(arrPanel, ID_ARYTEXTCTRL, wxT(""),
                                    wxDefaultPosition, wxSize(50,20), 0,
                                    wxTextValidator(wxGMAT_FILTER_NUMERIC));
   mArrColTextCtrl->SetToolTip(pConfig->Read(_T("ArrayColumnValueHint")));

   mStrClearButton =
      new wxBitmapButton(strPanel, ID_CLEAR_STR_BUTTON, clearBitmap, wxDefaultPosition,
                         wxSize(buttonWidth, 20));
   mStrClearButton->SetToolTip(pConfig->Read(_T("ClearStringHint"), "Clear String Fields"));
   
   mStringNameTextCtrl = new wxTextCtrl(strPanel, ID_STRTEXTCTRL, wxT(""),
                                        wxDefaultPosition, wxSize(80,20), 0);
   mStringNameTextCtrl->SetToolTip(pConfig->Read(_T("StringNameHint")));
   mStringValueTextCtrl = new wxTextCtrl(strPanel, ID_STRTEXTCTRL, wxT(""),
                                     wxDefaultPosition, wxSize(110,20), 0);
   mStringValueTextCtrl->SetToolTip(pConfig->Read(_T("StringValueHint")));
   
   // wxButton
   mCreateVariableButton = new wxButton(varPanel, ID_CREATE_BUTTON, wxT(CreateLabel.c_str()),
                                        wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   mCreateVariableButton->SetToolTip(pConfig->Read(_T("CreateVariableHint")));
   mCreateVariableButton->Disable();
   mSelectButton = new wxButton(varPanel, ID_SELECT_BUTTON, wxT("Select"),
                                     wxDefaultPosition, wxDefaultSize, 0);
   mSelectButton->SetToolTip(pConfig->Read(_T("SelectHint")));

   mCreateArrayButton = new wxButton(arrPanel, ID_CREATE_BUTTON, wxT(CreateLabel.c_str()),
                                     wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   mCreateArrayButton->SetToolTip(pConfig->Read(_T("CreateArrayHint")));
   mCreateArrayButton->Disable();
   mEditArrayButton = new wxButton(arrPanel, ID_EDITARRAY_BUTTON, wxT("Edit"),
                                     wxDefaultPosition, wxDefaultSize, 0);
   mEditArrayButton->Disable();
   mEditArrayButton->SetToolTip(pConfig->Read(_T("EditArrayHint")));

   mCreateStringButton = new wxButton(strPanel, ID_CREATE_BUTTON, wxT(CreateLabel.c_str()),
                                      wxDefaultPosition, wxDefaultSize, wxBU_EXACTFIT);
   mCreateStringButton->SetToolTip(pConfig->Read(_T("CreateStringHint")));
   mCreateStringButton->Disable();
   
   //wxArrayString
   wxArrayString emptyArray;
   
   // wxListBox
   mUserVarListBox =
      theGuiManager->GetUserVariableListBox(varPanel, ID_LISTBOX, wxSize(170, 125), "");
   mUserVarListBox->SetToolTip(pConfig->Read(_T("VariableListHint")));
   mUserArrayListBox =
      theGuiManager->GetUserArrayListBox(arrPanel, ID_LISTBOX, wxSize(170, 125), "");
   mUserArrayListBox->SetToolTip(pConfig->Read(_T("ArrayListHint")));
   mUserStringListBox =
      theGuiManager->GetUserStringListBox(strPanel, ID_LISTBOX, wxSize(170, 125), "");
   mUserStringListBox->SetToolTip(pConfig->Read(_T("StringListHint")));
          
   // wxSizers
   wxBoxSizer *pageBoxSizer = new wxBoxSizer(wxVERTICAL);
   mDetailsBoxSizer = new wxBoxSizer(wxHORIZONTAL);   
   
   wxFlexGridSizer *top1FlexGridSizer = new wxFlexGridSizer(5, 0, 0);
   wxFlexGridSizer *objPropertyFlexGridSizer = new wxFlexGridSizer(4, 0, 0);
   wxFlexGridSizer *arr1FlexGridSizer = new wxFlexGridSizer(7, 0, 0);
   wxFlexGridSizer *stringFlexGridSizer = new wxFlexGridSizer(6, 0, 0);


   GmatStaticBoxSizer *variableStaticBoxSizer =
      new GmatStaticBoxSizer(wxHORIZONTAL, varPanel);
   
   GmatStaticBoxSizer *arrayStaticBoxSizer =
      new GmatStaticBoxSizer(wxHORIZONTAL, arrPanel);
   
   GmatStaticBoxSizer *stringStaticBoxSizer =
      new GmatStaticBoxSizer(wxVERTICAL, strPanel);
   
   // Add to wx*Sizers
   //-------------------------------------------------------
   // for Variable
   //-------------------------------------------------------
   top1FlexGridSizer->Add(0, 0, 0, bsize);
   top1FlexGridSizer->Add(varNameStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   top1FlexGridSizer->Add(0, 0, 0, bsize);
   top1FlexGridSizer->Add(expStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   top1FlexGridSizer->Add(0, 0, 0, bsize);
   
   top1FlexGridSizer->Add(mVarClearButton, 0, wxALIGN_CENTER|wxALL, bsize);
   top1FlexGridSizer->Add(mVarNameTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   top1FlexGridSizer->Add(varEqualSignStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   top1FlexGridSizer->Add(mExprTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   top1FlexGridSizer->Add(mCreateVariableButton, 0, wxALIGN_CENTRE|wxALL, bsize);
   
   top1FlexGridSizer->Add(0, 0, wxALIGN_CENTER|wxALL, bsize);
   top1FlexGridSizer->Add(0, 0, wxALIGN_CENTRE|wxALL, bsize);
   top1FlexGridSizer->Add(0, 0, wxALIGN_CENTRE|wxALL, bsize);
   top1FlexGridSizer->Add(mSelectButton, 0, wxALIGN_LEFT|wxALL, bsize);
   top1FlexGridSizer->Add(0, 0, wxALIGN_CENTRE|wxALL, bsize);
   objPropertyFlexGridSizer->Add(mUserVarListBox, 0, wxALIGN_CENTER|wxALL, bsize);
   
   variableStaticBoxSizer->Add(top1FlexGridSizer, 0, wxALIGN_TOP|wxALL, bsize);
   variableStaticBoxSizer->Add(objPropertyFlexGridSizer, 0, wxALIGN_TOP|wxALL, bsize);
   
   stringFlexGridSizer->Add(0, 0, 0, bsize);
   stringFlexGridSizer->Add(stringNameLabel, 0, wxALIGN_CENTER|wxALL, bsize);
   stringFlexGridSizer->Add(0, 0, 0, bsize);
   stringFlexGridSizer->Add(stringValueLabel, 1, wxALIGN_CENTER|wxALL, bsize);
   stringFlexGridSizer->Add(0, 0, 0, bsize);
   stringFlexGridSizer->Add(configStringLabel, 0, wxALIGN_CENTER|wxALL, bsize);
   
   stringFlexGridSizer->Add(mStrClearButton, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, bsize);
   stringFlexGridSizer->Add(mStringNameTextCtrl, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, bsize);
   stringFlexGridSizer->Add(stringEqualSignStaticText, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, bsize);
   stringFlexGridSizer->Add(mStringValueTextCtrl, 1, wxALIGN_CENTER_HORIZONTAL|wxALL, bsize);
   stringFlexGridSizer->Add(mCreateStringButton, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, bsize);
   stringFlexGridSizer->Add(mUserStringListBox, 0, wxALIGN_CENTER|wxALL, bsize);
   
   stringStaticBoxSizer->Add(stringFlexGridSizer, 0, wxALIGN_TOP|wxALL, bsize);
   
   //-------------------------------------------------------
   // for Array Creation
   //-------------------------------------------------------
   // 1st row
   arr1FlexGridSizer->Add(0, 0, 0, bsize);
   arr1FlexGridSizer->Add(arrNameStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(0, 0, 0, bsize);
   arr1FlexGridSizer->Add(arr1RowStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(0, 0, 0, bsize);
   arr1FlexGridSizer->Add(arr1ColStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(0, 0, 0, bsize);
   //arr1FlexGridSizer->Add(configArrStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   
   // 2nd row
   arr1FlexGridSizer->Add(mArrClearButton, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(mArrNameTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(arrEqualSignStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(mArrRowTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(arrTimesStaticText, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(mArrColTextCtrl, 0, wxALIGN_CENTER|wxALL, bsize);
   arr1FlexGridSizer->Add(mCreateArrayButton, 0, wxALIGN_CENTER|wxALL, bsize);
   //arr1FlexGridSizer->Add(mUserArrayListBox, 0, wxALIGN_CENTER|wxALL, bsize);

   arr1FlexGridSizer->Add(0, 0, 0, bsize);
   arr1FlexGridSizer->Add(mEditArrayButton, 0, wxALIGN_LEFT, bsize);

   arrayStaticBoxSizer->Add(arr1FlexGridSizer, 0, wxALIGN_TOP|wxALL, bsize);
   arrayStaticBoxSizer->Add(mUserArrayListBox, 0, wxALIGN_TOP|wxALL, bsize);
   
   varPanel->SetSizer(variableStaticBoxSizer);
   arrPanel->SetSizer(arrayStaticBoxSizer);
   strPanel->SetSizer(stringStaticBoxSizer);

   //-------------------------------------------------------
   // add to parent sizer
   //-------------------------------------------------------
   notebook->AddPage(varPanel, GUI_ACCEL_KEY"Variable", true);
   notebook->AddPage(arrPanel, GUI_ACCEL_KEY"Array", false);
   notebook->AddPage(strPanel, GUI_ACCEL_KEY"String", false);
   
   theMiddleSizer->Add(notebook, 0, wxALIGN_LEFT|wxGROW, 0);
   
   theCancelButton->SetLabel("Done");
   theOkButton->Hide();
   #if DEBUG_PARAM_CREATE_DIALOG
   MessageInterface::ShowMessage("ParameterCreateDialog::Create() exiting\n");
   #endif
   
}


//------------------------------------------------------------------------------
// virtual void LoadData()
//------------------------------------------------------------------------------
void ParameterCreateDialog::LoadData()
{
   #if DEBUG_PARAM_CREATE_DIALOG
   MessageInterface::ShowMessage("ParameterCreateDialog::LoadData() entering\n");
   #endif
   
   wxString str;
   int mNumRows;
   int mNumCols;

   if (mObjectName != "")
   {
      mCurrParam =
         (Parameter*)theGuiInterpreter->GetConfiguredObject(mObjectName.c_str());
      
      // Set the pointer for the "Show Script" button
      mObject = mCurrParam;
      if (mCurrParam == NULL)
         return;

      switch (mParamType)
      {
         case VARIABLE:
            mVarNameTextCtrl->SetValue(mObjectName.c_str());
            mExprTextCtrl->SetValue(mCurrParam->GetStringParameter("Expression").c_str());
            mCreateVariableButton->Disable();
            mUserVarListBox->SetStringSelection(mObjectName.c_str());
            break;
         case ARRAY:
            mArrNameTextCtrl->SetValue(mObjectName.c_str());
            mNumRows = mCurrParam->GetIntegerParameter("NumRows");
            mNumCols = mCurrParam->GetIntegerParameter("NumCols");
            str << mNumRows;
            mArrRowTextCtrl->SetValue(str);
            str = "";
            str << mNumCols;
            mArrColTextCtrl->SetValue(str);
            mCreateArrayButton->Disable();
            mEditArrayButton->Enable(mCurrParam != NULL);
            mUserArrayListBox->SetStringSelection(mObjectName.c_str());
            break;
         case STRING:
            mStringNameTextCtrl->SetValue(mObjectName.c_str());
            mStringValueTextCtrl->SetValue(mCurrParam->GetStringParameter("Expression").c_str());
            mCreateStringButton->Disable();
            mUserStringListBox->SetStringSelection(mObjectName.c_str());
            break;
      }
   }
   
   #if DEBUG_PARAM_CREATE_DIALOG
   MessageInterface::ShowMessage("ParameterCreateDialog::LoadData() exiting\n");
   #endif   
}


//------------------------------------------------------------------------------
// virtual void SaveData()
//------------------------------------------------------------------------------
void ParameterCreateDialog::SaveData()
{
   std::string s;
   Integer mNumCols;
   Integer mNumRows;

   EnableUpdate( mCreateVariableButton->IsEnabled() || 
                 mCreateArrayButton->IsEnabled() || 
                 mCreateStringButton->IsEnabled() );
   
   canClose = true;
   
   #if DEBUG_PARAM_CREATE_SAVE
   MessageInterface::ShowMessage
      ("ParameterCreateDialog::SaveData() canClose=%d, mCreateVariable=%d, "
       "mCreateString=%d, mCreateArray=%d\n", canClose, mCreateVariable,
       mCreateString, mCreateArray);
   #endif
   
   switch (mParamType)
   {
      case VARIABLE:
         if ((mCurrParam == NULL) || (mObjectName.c_str() != mVarNameTextCtrl->GetValue()))
         {         
            CreateVariable();
         }
         else
         {
            std::string expr = mExprTextCtrl->GetValue().c_str();
            Real rval;
            CheckReal(rval, expr, "Expression", "Real Number");
            mCurrParam->SetStringParameter("Expression", expr);
            ResetControls();
         }
         break;
      case ARRAY:
         if ((mCurrParam == NULL) || (mObjectName.c_str() != mArrNameTextCtrl->GetValue()))
         {         
            CreateArray();
         }
         else
         {
            s = mArrColTextCtrl->GetValue().c_str();
            CheckInteger(mNumCols, s, "Columns", "Integer Number >= 1", false, true, true, false);
            s = mArrRowTextCtrl->GetValue().c_str();
            CheckInteger(mNumRows, s, "Rows", "Integer Number >= 1", false, true, true, false);
            ((Array *) mCurrParam)->SetSize(mNumRows, mNumCols);
            ResetControls();
         }
         break;
      case STRING:
         if ((mCurrParam == NULL) || (mObjectName.c_str() != mStringNameTextCtrl->GetValue()))
         {         
            CreateString();
         }
         else
         {
            std::string expr = mStringValueTextCtrl->GetValue().c_str();
            mCurrParam->SetStringParameter("Expression", expr);
            ResetControls();
         }
         break;
   }
     
}


//------------------------------------------------------------------------------
// virtual void ResetData()
//------------------------------------------------------------------------------
void ParameterCreateDialog::ResetData()
{
   mIsParamCreated = false;
}


//------------------------------------------------------------------------------
// virtual void ResetControls()
//------------------------------------------------------------------------------
void ParameterCreateDialog::ResetControls()
{
   switch (mParamType)
   {
      case VARIABLE:
         mCreateVariableButton->Disable();
         mExprTextCtrl->SetValue("");
         mVarNameTextCtrl->SetValue("");
         break;
      case ARRAY:
         mCreateArrayButton->Disable();
         mArrNameTextCtrl->SetValue("");
         mArrRowTextCtrl->SetValue("");
         mArrColTextCtrl->SetValue("");
         mEditArrayButton->Disable();
         break;
      case STRING:
         mCreateStringButton->Disable();
         mStringNameTextCtrl->SetValue("");
         mStringValueTextCtrl->SetValue("");
         break;
   }
}


//---------------------------------
// event handling
//---------------------------------

//------------------------------------------------------------------------------
// void OnVarTextUpdate(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnVarTextUpdate(wxCommandEvent& event)
{
   mCreateVariableButton->Disable();

   if (mVarNameTextCtrl->GetValue().Trim() != "" &&
       mVarNameTextCtrl->GetValue().Trim() != " " &&
       mExprTextCtrl->GetValue().Trim() != "")
   {
      mCreateVariableButton->Enable();
      EnableUpdate(true);
   }
   
}


//------------------------------------------------------------------------------
// void OnAryTextUpdate(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnAryTextUpdate(wxCommandEvent& event)
{
   mCreateArrayButton->Disable();

   if (mArrNameTextCtrl->GetValue().Trim() != "" &&
       mArrRowTextCtrl->GetValue().Trim() != "" &&
       mArrColTextCtrl->GetValue().Trim() != "")
   {
      mCreateArrayButton->Enable();
      EnableUpdate(true);
   }

}


//------------------------------------------------------------------------------
// void OnStrTextUpdate(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnStrTextUpdate(wxCommandEvent& event)
{
   mCreateStringButton->Disable();
   
   if (mStringNameTextCtrl->GetValue().Trim() != "" )
   {
      mCreateStringButton->Enable();
      EnableUpdate(true);
   }
   
}


//------------------------------------------------------------------------------
// void OnCreateButton(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnCreateButton(wxCommandEvent& event)
{    
   SaveData();
   switch (mParamType)
   {
      case VARIABLE:
         mVarNameTextCtrl->SetFocus();
         break;
      case ARRAY:
         mArrNameTextCtrl->SetFocus();
         break;
      case STRING:
         mStringNameTextCtrl->SetFocus();
         break;
   }
}

//------------------------------------------------------------------------------
// void SetParameterType(e_parameter_type param_type)
//------------------------------------------------------------------------------
void ParameterCreateDialog::SetParameterType( e_parameter_type param_type )
{
   mParamType = param_type;

   notebook->SetSelection( (size_t) mParamType );
}

//------------------------------------------------------------------------------
// void OnPageChanged(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnPageChanged(wxNotebookEvent& event)
{    
   SetParameterType( (e_parameter_type) event.GetSelection() );
   switch (mParamType)
   {
      case VARIABLE:
         mVarNameTextCtrl->SetFocus();
         break;
      case ARRAY:
         mArrNameTextCtrl->SetFocus();
         break;
      case STRING:
         mStringNameTextCtrl->SetFocus();
         break;
   }
}


//------------------------------------------------------------------------------
// void OnClearButtonClick(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnClearButtonClick(wxCommandEvent& event)
{    
   switch (mParamType)
   {
      case VARIABLE:
         mVarNameTextCtrl->Clear();
         mExprTextCtrl->Clear();
         break;
      case ARRAY:
         mArrNameTextCtrl->Clear();
         mArrRowTextCtrl->Clear();
         mArrColTextCtrl->Clear();
         break;
      case STRING:
         mStringNameTextCtrl->Clear();
         mStringValueTextCtrl->Clear();
         break;
   }
}


//------------------------------------------------------------------------------
// void OnEditArrayButtonClick(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnEditArrayButtonClick(wxCommandEvent& event)
{    
   ArraySetupDialog paramDlg(this, mArrNameTextCtrl->GetValue());
   paramDlg.ShowModal();
}


//------------------------------------------------------------------------------
// void OnSelectButtonClick(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnSelectButtonClick(wxCommandEvent& event)
{    

      ParameterSelectDialog paramDlg(this, mSelectVarStrings,
         GuiItemManager::SHOW_PLOTTABLE, false, true);
      
      paramDlg.SetParamNameArray(mSelectVarStrings);
      paramDlg.ShowModal();
      
      if (paramDlg.HasSelectionChanged())
      {
         wxArrayString selectVarStrings = paramDlg.GetParamNameArray();
         if (selectVarStrings.Count() > 0)
         {
            mExprTextCtrl->Clear();
            for (unsigned int i=0; i<selectVarStrings.Count(); i++)
               mExprTextCtrl->AppendText(selectVarStrings[i]);
         }
         else // no selections
         {
            mExprTextCtrl->Clear();
         }
      }
}


//------------------------------------------------------------------------------
// void OnListboxClick(wxCommandEvent& event)
//------------------------------------------------------------------------------
void ParameterCreateDialog::OnListboxClick(wxCommandEvent& event)
{    
   mObjectName = event.GetString();
   LoadData();
}


//------------------------------------------------------------------------------
// Parameter* CreateParameter(const wxString &paramName)
//------------------------------------------------------------------------------
/*
 * @return newly created parameter pointer if it does not exist,
 *         return existing parameter pointer otherwise
 */
//------------------------------------------------------------------------------
Parameter* ParameterCreateDialog::CreateParameter(const wxString &name)
{
   #if DEBUG_PARAM_CREATE_DIALOG
   MessageInterface::ShowMessage
      ("ParameterCreateDialog::CreateParameter() name:%s\n", name.c_str());
   #endif
   
   std::string paramName(name.c_str());
   //std::string ownerName(mObjectListBox->GetStringSelection().c_str());
   //std::string propName(mPropertyListBox->GetStringSelection().c_str());
   //std::string depObjName = "";

   //if (mCentralBodyComboBox->IsShown())
   //{
   //   depObjName = std::string(mCentralBodyComboBox->GetStringSelection().c_str());
   //}
   //else if (mCoordSysComboBox->IsShown())
   //{
   //   depObjName = std::string(mCoordSysComboBox->GetStringSelection().c_str());
   //}
   
   Parameter *param = theGuiInterpreter->GetParameter(paramName);
   
   // create a parameter if it does not exist
   if (param == NULL)
   {
      //param = theGuiInterpreter->CreateParameter(propName, paramName);
      //param->SetRefObjectName(Gmat::SPACECRAFT, ownerName);
      
      //if (depObjName != "")
      //   param->SetStringParameter("DepObject", depObjName);
   }
   
   #if DEBUG_PARAM_CREATE_DIALOG
   MessageInterface::ShowMessage("ParameterCreateDialog::CreateParameter() exiting\n");
   #endif
   
   return param;
}


//------------------------------------------------------------------------------
// void CreateVariable()
//------------------------------------------------------------------------------
/*
 * This method creates a variable after going through validation.
 */
//------------------------------------------------------------------------------
void ParameterCreateDialog::CreateVariable()
{
   wxString wxvarName = mVarNameTextCtrl->GetValue().Trim();
   std::string varName = std::string(wxvarName.c_str());
   wxString wxvarExpr = mExprTextCtrl->GetValue().Trim();
   std::string varExpr = std::string(wxvarExpr.c_str());
   Real realNum;
   bool isRealNumber = true;
   
   #if DEBUG_PARAM_CREATE_VAR
   MessageInterface::ShowMessage
      ("ParameterCreateDialog::CreateVariable() varName = "  + varName +
       " varExpr = " + varExpr + "\n");
   #endif
   
   
   // check if it has blank variable name or expression
   if (varName == "" || varExpr == "")
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "Variable or expression cannot be blank");
      canClose = false;
      return;
   }
   
   // check if it has valid variable name
   if (!GmatStringUtil::IsValidName(varName))
   {
      MessageInterface::PopupMessage
         (Gmat::ERROR_, "Invalid variable name: \"%s.\" Variable name must "
          "start with an alphabet letter", varName.c_str());
      canClose = false;
      return;
   }
   
   // check if rhs is a number
   if (!GmatStringUtil::ToReal(varExpr, realNum))
      isRealNumber = false;
   
   Parameter *param = NULL;
   
   // check if variable name already exist
   if (theGuiInterpreter->GetParameter(varName) != NULL)
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "The variable: %s cannot be created. It already exists.",
          varName.c_str());
      canClose = false;
      return;
   }

   try
   {
      // create a variable if rhs is a number
      if (isRealNumber)
      {
         param = theGuiInterpreter->CreateParameter("Variable", varName);  
         param->SetStringParameter("Expression", varExpr);
      }
      else
      {
         // Parse the Parameter
         //StringTokenizer st(varExpr, "()*/+-^ ");
         // tokenize nothing, we want no expressions, 04/2010 TGG
         StringTokenizer st(varExpr, "");
         StringArray tokens = st.GetAllTokens();
         StringArray paramArray;
         
         // Check if unexisting varibles used in expression
         for (unsigned int i=0; i<tokens.size(); i++)
         {
            #if DEBUG_PARAM_CREATE_VAR
            MessageInterface::ShowMessage("   token:<%s> \n", tokens[i].c_str());
            #endif
            
            if (!GmatStringUtil::ToReal(tokens[i], realNum))
            {
               // create system parameter if it is NULL
               if (theGuiInterpreter->GetParameter(tokens[i]) == NULL)
               {
                  // check if it is system parameter
                  std::string type, owner, depObj;
                  GmatStringUtil::ParseParameter(tokens[i], type, owner, depObj);
                  if (theGuiInterpreter->IsParameter(type))
                  {
                     #if DEBUG_PARAM_CREATE_VAR
                     MessageInterface::ShowMessage
                        ("type:%s is a system parameter\n", type.c_str());
                     #endif
                  
                     Parameter *sysParam = 
                        theGuiInterpreter->CreateParameter(type, tokens[i]);
                  
                     // set ref. object name
                     sysParam->SetRefObjectName(sysParam->GetOwnerType(), owner);
                  
                     // set dependent object name
                     if (depObj != "")
                        sysParam->SetStringParameter("DepObject", depObj);
                  
                  }
                  else
                  {
                     MessageInterface::PopupMessage
                        (Gmat::WARNING_, "The variable \"%s\" does not exist. "
                         "It must be created first.", tokens[i].c_str());
                     canClose = false;
                     return;
                  }
               }
            
               // create a variable
               param = theGuiInterpreter->CreateParameter("Variable", varName);
               param->SetStringParameter("Expression", varExpr);
            
               // set parameter names used in expression
               param->SetRefObjectName(Gmat::PARAMETER, tokens[i]);
            
            }
         }      
      }
   
   
      #if DEBUG_PARAM_CREATE_VAR
      MessageInterface::ShowMessage
         ("ParameterCreateDialog::CreateVariable() The variable \"%s\" added\n",
          varName.c_str());
      #endif
      
      mParamNames.Add(varName.c_str());
      mIsParamCreated = true;
      theGuiManager->UpdateParameter();
      
      GmatAppData::Instance()->GetResourceTree()->UpdateVariable();
      mUserVarListBox->Append(varName.c_str());
      
      for (unsigned int i=0; i<mUserVarListBox->GetCount(); i++)
      {
         if (mUserVarListBox->GetString(i).IsSameAs(varName.c_str()))
         {
            mUserVarListBox->SetSelection(i);
            break;
         }
      }
      
      // reset values 
      ResetControls();
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
}


//------------------------------------------------------------------------------
// void CreateString()
//------------------------------------------------------------------------------
void ParameterCreateDialog::CreateString()
{
   wxString wxstrName = mStringNameTextCtrl->GetValue().Trim();
   std::string strName = std::string(wxstrName);
   std::string strValue = std::string(mStringValueTextCtrl->GetValue().c_str());

   try
   {
      // if new user string to create
      if (theGuiInterpreter->GetParameter(strName) == NULL)
      {
         Parameter *param;
      
         param = theGuiInterpreter->CreateParameter("String", strName);
         param->SetStringParameter("Expression", strValue);
      
         mParamNames.Add(strName.c_str());
         mIsParamCreated = true;
         theGuiManager->UpdateParameter();
      
         GmatAppData::Instance()->GetResourceTree()->UpdateVariable();
         mUserStringListBox->Append(strName.c_str());
      
         for (unsigned int i=0; i<mUserStringListBox->GetCount(); i++)
         {
            if (mUserStringListBox->GetString(i).IsSameAs(strName.c_str()))
            {
               mUserStringListBox->SetSelection(i);
               break;
            }
         }

         EnableUpdate(true);
      }
      else
      {
         MessageInterface::PopupMessage
            (Gmat::WARNING_, "ParameterCreateDialog::CreateString()\nThe string: %s"
             " cannot be created. It already exists.", strName.c_str());
      }

      ResetControls();
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
   
}


//------------------------------------------------------------------------------
// void CreateArray()
//------------------------------------------------------------------------------
void ParameterCreateDialog::CreateArray()
{
   std::string s;
   long row, col;
   Integer mNumCols, mNumRows;

   s = mArrColTextCtrl->GetValue().c_str();
   CheckInteger(mNumCols, s, "Columns", "Integer Number >= 1", false, true, true, false);
   s = mArrRowTextCtrl->GetValue().c_str();
   CheckInteger(mNumRows, s, "Rows", "Integer Number >= 1", false, true, true, false);
   
   if (!(mArrRowTextCtrl->GetValue().ToLong(&row)) ||
       !(mArrColTextCtrl->GetValue().ToLong(&col)))
   {
      wxLogError(wxT("Row or Column is not a number"));
      wxLog::FlushActive();
      canClose = false;
      return;
   }

   // Check for maximum array size of 1000x1000
   if (row > 1000 || col > 1000)
   {
      MessageInterface::PopupMessage
         (Gmat::WARNING_, "The array size %d x %d is too big. The maximum "
          "allowed size is 1000 x 1000", row, col);
      canClose = false;
      return;
   }
   
   try
   {
      wxString wxarrName = mArrNameTextCtrl->GetValue().Trim();
      std::string arrName = std::string(wxarrName.c_str());
      
      // if new user array to create
      if (theGuiInterpreter->GetParameter(arrName) == NULL)
      {
         Parameter *param;
         
         param = theGuiInterpreter->CreateParameter("Array", arrName);
         param->SetIntegerParameter("NumRows", row);
         param->SetIntegerParameter("NumCols", col);
         
         mParamNames.Add(arrName.c_str());
         mIsParamCreated = true;
         theGuiManager->UpdateParameter();
         
         GmatAppData::Instance()->GetResourceTree()->UpdateVariable();
         mUserArrayListBox->Append(arrName.c_str());
         
         for (unsigned int i=0; i<mUserArrayListBox->GetCount(); i++)
         {
            if (mUserArrayListBox->GetString(i).IsSameAs(arrName.c_str()))
            {
               mUserArrayListBox->SetSelection(i);
               break;
            }
         }
         
         EnableUpdate(true);
      }
      else
      {
         MessageInterface::PopupMessage
            (Gmat::WARNING_, "ParameterCreateDialog::CreateArray()\nThe array: %s"
             " cannot be created. It already exists.", arrName.c_str());
      }
      ResetControls();      
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
   }
}

