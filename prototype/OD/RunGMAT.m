
global ObjectStore jd_ref;

%==========================================================================
%==========================================================================
%------  Intialize the Solar System
%==========================================================================
%==========================================================================

SolarSystem = SolarSystem();
SolarSystem.Initialize;

%==========================================================================
%==========================================================================
%------  Intialize the sandbox and all objects in the sandbox
%==========================================================================
%==========================================================================

%---- Hard code initializing objects into Object Store.  Eventually this
%     should be done in an automated way.
SandBox.AddObject(ODSat,'ODSat');
SandBox.AddObject(BLS,'BLS');
SandBox.AddObject(Canberra,'Canberra');
SandBox.AddObject(CanberraData,'CanberraData');
SandBox.AddObject(ODProp,'ODProp');

SandBox.Initialize();

%==========================================================================
%==========================================================================
%------  Intialize the commands
%==========================================================================
%==========================================================================

RunEst = RunEstimator();
RunEst.Initialize(BLS);
RunEst.Execute();

       

