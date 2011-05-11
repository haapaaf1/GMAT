% loadlibrary test
% This tests the C GMAT shared library interface and builds
% (re-builds) the library .m wrapper and any thunk files required on this
% platform.
% 
% Important notes: MATLAB startup
% LINUX: UNTESTED!!!
% When invoking MATLAB, add to the LD_LIBRARY_PATH (unix) the full
% libOdtbxGmatInterface/bin path when invoking MATLAB.  This allows MATLAB
% to find the shared libraries found in the /bin directory.
%
% MAC: UNTESTED!!!
% Same as above but the env variable name is DYLD_LIBRARY_PATH.
%
% Windows: Path process untested
% (on 32-bit Windows 7) I didn't have to set any paths.  MATLAB was able to
% find the .dlls once they were on the MATLAB (internal) path.
%
% TBD: Be sure to add this directory and the location of the compiled shared 
% library to the MATLAB (internal) path as well!

if (libisloaded('libOdtbxGmatInterface'))
    fprintf(1,'Unloading libOdtbxGmatInterface...\n');
    unloadlibrary('libOdtbxGmatInterface');
end

% from the libOdtbxGmatInterface directory
[notfound, warnings] = loadlibrary('libOdtbxGmatInterface',...
    [pwd filesep 'C_wrapper' filesep 'src' filesep 'CGmatInterface.hpp'],...
    'mfilename', 'gmatCwrapper');

% Later, you can load the library with no header via:
%[notfound, warnings] = loadlibrary('libOdtbxGmatInterface', @gmatCwrapper);

ready = libisloaded('libOdtbxGmatInterface');
if ~ready
    return;
else
    fprintf(1,'Successfully loaded libOdtbxGmatInterface...\n');
end

clear ready;

libfunctions libOdtbxGmatInterface

fprintf(1,'Calling libOdtbxGmatInterface getLastMessage():\n');
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

fprintf(1,'Calling libOdtbxGmatInterface CStartGmat():\n');
ode = calllib('libOdtbxGmatInterface','StartGmat');
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

%scriptName = 'default.script';
scriptName = 'defaultOdtbx.script';
retval = calllib('libOdtbxGmatInterface','LoadScript',scriptName);
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

disp('Created the following objects:');
count = calllib('libOdtbxGmatInterface','CountObjects');
for i = 0 : count-1
    name = calllib('libOdtbxGmatInterface','GetObjectName',i);
    fprintf(1,'   %s\n',name);
end;
   
retval = calllib('libOdtbxGmatInterface','RunScript');
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

ode = calllib('libOdtbxGmatInterface','GetRunSummary');
disp('Run Summary:');
disp('************');
disp(ode);
disp('************');

retval = calllib('libOdtbxGmatInterface','FindOdeModel','');
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);



% whos
% 
% epoch = 21545.0;
% state = [7000.0; 1000.0; 0.0; 1.0; 7.0; 0.5]; % typical matlab column vector
% statesize = int32(6);
% fprintf(1,'The epoch: %f\n',epoch);
% fprintf(1,'The state: \n\t[%f\n\t%f\n \t%f\n \t%f\n\t%f\n\t%f]\n',state);
% statep = libpointer('doublePtr', state);
% 
% fprintf(1,'Calling libOdtbxGmatInterface CSetODEState:\n');
% retval = calllib('libOdtbxGmatInterface','CSetODEState',ode,epoch,statep,statesize);
% if retval ~= 0
%     fprintf(1,'Failue in libOdtbxGmatInterface CSetODEState():\n');
%     errmsg = calllib('libOdtbxGmatInterface','getLastError');
%     fprintf(1,'The error message was: %s\n',errmsg);
%     
%     % clean up memory:
%     calllib('libOdtbxGmatInterface','deleteCOdeModel',ode);
%     clear ode;
%     return;
% else
%     fprintf(1,'(No error.)\n');
% end
% whos
% 
% fprintf(1,'Calling libOdtbxGmatInterface CInitODEs:\n');
% retval = calllib('libOdtbxGmatInterface','CInitODEs',ode);
% if retval ~= 0
%     fprintf(1,'Failue in libOdtbxGmatInterface CInitODEs():\n');
%     errmsg = calllib('libOdtbxGmatInterface','getLastError');
%     fprintf(1,'The error message was: %s\n',errmsg);
%     
%     % clean up memory:
%     calllib('libOdtbxGmatInterface','deleteCOdeModel',ode);
%     clear ode;
%     return;
% else
%     fprintf(1,'(No error.)\n');
% end
% whos
% 
% fprintf(1,'Calling libOdtbxGmatInterface CGetDerivatives:\n');
% dim = 0;  % Note, this value won't change
% dimptr = libpointer('int32Ptr', dim);
% derivsptr = calllib('libOdtbxGmatInterface','CGetDerivatives',ode, 0.0, 1, dimptr);
% dimval = get(dimptr,'value');
% if dimval == 0
%     fprintf(1,'Failue in libOdtbxGmatInterface CGetDerivatives() dimval:\n');
%     errmsg = calllib('libOdtbxGmatInterface','getLastError');
%     fprintf(1,'The error message was: %s\n',errmsg);
%     
%     % clean up memory:
%     calllib('libOdtbxGmatInterface','deleteCOdeModel',ode);
%     clear ode;
%     return;
% else
%     fprintf(1,'(No error.)\n');
% end
% setdatatype(derivsptr, 'doublePtr', dimval, 1);
% derivsdata = get(derivsptr,'Value');
% 
% fprintf(1,'The epoch: %f\n',epoch);
% fprintf(1,'The state derivs: \n\t[%f\n\t%f\n \t%f\n \t%f\n\t%f\n\t%f]\n',derivsdata(1:6));
% fprintf(1,'The A matrix: \n');
% deriv = (reshape(derivsdata(7:end),6,6))';
% fprintf(1,'%e\t%e\t%e\t%e\t%e\t%e\n',deriv(1,1:6));
% fprintf(1,'%e\t%e\t%e\t%e\t%e\t%e\n',deriv(2,1:6));
% fprintf(1,'%e\t%e\t%e\t%e\t%e\t%e\n',deriv(3,1:6));
% fprintf(1,'%e\t%e\t%e\t%e\t%e\t%e\n',deriv(4,1:6));
% fprintf(1,'%e\t%e\t%e\t%e\t%e\t%e\n',deriv(5,1:6));
% fprintf(1,'%e\t%e\t%e\t%e\t%e\t%e\n',deriv(6,1:6));
% 
% % now try to clean up the ode:
% calllib('libOdtbxGmatInterface','deleteCOdeModel',ode);
% clear ode;
% 
% % clear any memory held by matlab
% clear all;


% now unload the library
fprintf(1,'Unloading libOdtbxGmatInterface...\n');
unloadlibrary('libOdtbxGmatInterface');
libisloaded('libOdtbxGmatInterface');
