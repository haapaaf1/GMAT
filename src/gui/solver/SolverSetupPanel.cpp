#include "SolverSetupPanel.hpp"
#include "MessageInterface.hpp"

const wxString SolverSetupPanel::TF_SCHEMES[2] = 
   {
      wxT("false"),
      wxT("true")
   };

BEGIN_EVENT_TABLE(SolverSetupPanel, GmatPanel)
   EVT_COMBOBOX(ID_COMBOBOX, SolverSetupPanel::OnComboBoxChange)
   EVT_TEXT(ID_TEXTCTRL, SolverSetupPanel::OnTextChange)
//   EVT_CHECKBOX(ID_CHECKBOX, SQPSetupPanel::OnCheckboxChange)
END_EVENT_TABLE()


SolverSetupPanel::SolverSetupPanel(wxWindow *parent, const wxString &name):
   GmatPanel        (parent, name)
{
   theSolver =
      (Solver*)theGuiInterpreter->GetConfiguredObject(name.c_str());

   isTextModified = true;
   
   if (theSolver != NULL)
   {
      Create();
      Show();
   }
   else
   {
      // show error message
   }
}

SolverSetupPanel::~SolverSetupPanel()
{
}

void SolverSetupPanel::Create()
{
   Setup(this);
}

void SolverSetupPanel::LoadData()
{
   // load data from the core engine
   try
   {
      mObject = theSolver;
      
      std::string label;
      Integer propertyCount = theSolver->GetParameterCount();
      
      for (Integer i = 0; i < propertyCount; ++i)
      {
         if (mObject->IsParameterReadOnly(i) == false)
         {
            label = mObject->GetParameterText(i);
            LoadControl(label);
         }
      }
   }
   catch (BaseException &e)
   {
      MessageInterface::ShowMessage
         ("SolverSetupPanel:LoadData() error occurred!\n%s\n",
          e.GetFullMessage().c_str());
   }
   
   // explicitly disable apply button
   // it is turned on in each of the panels
   EnableUpdate(false);
}


void SolverSetupPanel::SaveData()
{
   canClose = true;
   
   try
   {
      for (std::map<std::string, Integer>::iterator i = controlMap.begin();
            i != controlMap.end(); ++i)
      {
         SaveControl(i->first);
         if (!canClose)
            return;
      }
   }
   catch (BaseException &e)
   {
      MessageInterface::PopupMessage(Gmat::ERROR_, e.GetFullMessage());
      canClose = false;
      return;
   }
}

void SolverSetupPanel::Setup(wxWindow *parent)
{
   if (theSolver == NULL)
      return;
   
   Integer propertyCount = theSolver->GetParameterCount();
   Integer j = 0;
   
   for (Integer i = 0; i < propertyCount; ++i)
   {
      if (theSolver->IsParameterReadOnly(i) == false)
      {
         propertyDescriptors.push_back(new wxStaticText(parent, ID_TEXT, 
               wxT(theSolver->GetParameterText(i).c_str())));
         
         controlMap[theSolver->GetParameterText(i)] = j++;

         wxControl* control = BuildControl(parent, i);
         propertyControls.push_back(control);
      }
   }

   wxFlexGridSizer *fGSMain = new wxFlexGridSizer(2);
   wxGridSizer *gSSpecs = new wxGridSizer(2);
   Integer border = 3;

   std::vector<wxStaticText*>::iterator item;

   for(item = propertyDescriptors.begin(), j = 0; 
       item != propertyDescriptors.end(); ++item, ++j) 
   {
      gSSpecs->Add(*item, 0, wxALL|wxALIGN_RIGHT, border);
      gSSpecs->Add(propertyControls[j], 0, wxALL|wxALIGN_LEFT, border);
   }

   fGSMain->Add(gSSpecs, 0, wxALL|wxALIGN_RIGHT, border*5);
   theMiddleSizer->Add(fGSMain, 0, wxALL|wxALIGN_CENTER, 5);
}

wxControl *SolverSetupPanel::BuildControl(wxWindow *parent, Integer index)
{
   wxControl *control = NULL;
   
   Gmat::ParameterType type = theSolver->GetParameterType(index);
   
   switch (type)
   {
      case Gmat::BOOLEAN_TYPE:
         {
            wxComboBox *cbControl = new wxComboBox(parent, ID_COMBOBOX, "true", 
                  wxDefaultPosition, wxDefaultSize, 2, TF_SCHEMES, 
                  wxCB_READONLY);
                           
            control = cbControl;
         }
         break;
         
      case Gmat::STRING_TYPE:
      default:
         control = new wxTextCtrl(parent, ID_TEXTCTRL, 
                     wxT(""), wxDefaultPosition, wxSize(100,-1));
         break;
   }
   
   return control;
}

void SolverSetupPanel::LoadControl(const std::string &label)
{
   Integer index = theSolver->GetParameterID(label);
   Gmat::ParameterType type = theSolver->GetParameterType(index);

   wxControl *theControl = propertyControls[controlMap[label]];
   wxString valueString;
   
   switch (type)
   {
      case Gmat::BOOLEAN_TYPE:
         if (theSolver->GetBooleanParameter(theSolver->GetParameterID(label)))
            ((wxComboBox*)(theControl))->SetValue(wxT("true"));
         else
            ((wxComboBox*)(theControl))->SetValue(wxT("false"));
         break;
      
      case Gmat::REAL_TYPE:
         {
            Real val = theSolver->GetRealParameter(
                  theSolver->GetParameterID(label));
            valueString << val;
            ((wxTextCtrl*)theControl)->ChangeValue(valueString);
         }
         break;
         
      case Gmat::INTEGER_TYPE:
         {
            Integer val = theSolver->GetIntegerParameter(
                  theSolver->GetParameterID(label));
            valueString << val;
            ((wxTextCtrl*)theControl)->ChangeValue(valueString);
         }
         break;
      
      case Gmat::STRING_TYPE:
         valueString = wxT(theSolver->GetStringParameter(label).c_str());
         ((wxTextCtrl*)theControl)->ChangeValue(valueString);
         break;

      default:
         break;
   }
}


void SolverSetupPanel::SaveControl(const std::string &label)
{
   Integer index = theSolver->GetParameterID(label);
   Gmat::ParameterType type = theSolver->GetParameterType(index);

   wxControl *theControl = propertyControls[controlMap[label]];
   std::string valueString;
   
   switch (type)
   {
      case Gmat::BOOLEAN_TYPE:
         {
            bool val = true;
            if (((wxComboBox*)theControl)->GetValue() == "false")
               val = false;
            theSolver->SetBooleanParameter(index, val);
         }
         break;
      
      case Gmat::REAL_TYPE:
         {
            Real val; 
            valueString = ((wxTextCtrl*)theControl)->GetValue();
            CheckReal(val, valueString, label, "Real Number");
            if (!canClose)
               return;
            theSolver->SetRealParameter(index, val);
         }
         break;
         
      case Gmat::INTEGER_TYPE:
         {
            Integer val;
            valueString = ((wxTextCtrl*)theControl)->GetValue();
            CheckInteger(val, valueString, label, "Integer");
            if (!canClose)
               return;
            theSolver->SetIntegerParameter(index, val);
         }
         break;
      
      case Gmat::STRING_TYPE:
         valueString = ((wxTextCtrl*)theControl)->GetValue();
         theSolver->SetStringParameter(index, valueString.c_str());
         break;

      default:
         break;
   }
}

//------------------------------------------------------------------------------
// void OnComboBoxChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
/**
 * @note Activates the Apply button when text is changed
 */
//------------------------------------------------------------------------------
void SolverSetupPanel::OnComboBoxChange(wxCommandEvent& event)
{
   if (theApplyButton != NULL)
      EnableUpdate(true);
}


//------------------------------------------------------------------------------
// void OnTextChange(wxCommandEvent& event)
//------------------------------------------------------------------------------
/**
 * @note Activates the Apply button when text is changed
 */
//------------------------------------------------------------------------------
void SolverSetupPanel::OnTextChange(wxCommandEvent& event)
{
   isTextModified = true;
   EnableUpdate(true);
}

