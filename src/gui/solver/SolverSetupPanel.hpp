#ifndef SolverSetupPanel_hpp
#define SolverSetupPanel_hpp

#include "GmatPanel.hpp"
#include "Solver.hpp"

class SolverSetupPanel : public GmatPanel
{
public:
	SolverSetupPanel(wxWindow *parent, const wxString &name);
	virtual ~SolverSetupPanel();
	
protected:
   Solver               *theSolver;
   bool                 isTextModified;
   
   virtual void         Create();
   virtual void         LoadData();
   virtual void         SaveData();
   void                 Setup(wxWindow *parent);
   wxControl *          BuildControl(wxWindow *parent, Integer index);
   void                 LoadControl(const std::string &label);
   void                 SaveControl(const std::string &label);
   
   // Text control event method
   void OnTextUpdate(wxCommandEvent& event);
   
   // any class wishing to process wxWindows events must use this macro
   DECLARE_EVENT_TABLE();
   void OnComboBoxChange(wxCommandEvent& event);
   void OnTextChange(wxCommandEvent& event);

   std::vector<wxStaticText*>       propertyDescriptors;
   std::vector<wxControl*>          propertyControls;
   std::map<std::string, Integer>   controlMap;

   enum
   {
      ID_TEXT = 55000,
      ID_TEXTCTRL,
      ID_COMBOBOX
   };
   
   static const wxString TF_SCHEMES[2];

};

#endif /* SolverSetupPanel_hpp */
