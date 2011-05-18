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

[notfound, warnings] = loadlibrary('libOdtbxGmatInterface', @interfacewrapper);

ready = libisloaded('libOdtbxGmatInterface');
if ~ready
    return;
else
    fprintf(1,'Successfully loaded libOdtbxGmatInterface...\n');
end

clear ready;

% 1. Start the GMAT engine
ode = calllib('libOdtbxGmatInterface','StartGmat');
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

% 2. Load and populate a configuration
scriptName = 'defaultOdtbx.script';
retval = calllib('libOdtbxGmatInterface','LoadScript',scriptName);
retval = calllib('libOdtbxGmatInterface','RunScript');

% Show the run status
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

% % Optional: See that it all ran
% ode = calllib('libOdtbxGmatInterface','GetRunSummary');
% disp('Run Summary:');
% disp('************');
% disp(ode);
% disp('************');

% 3. Set the ODE model to the one named in the script (currently uses the
%    first model found; will use named models later on)
retval = calllib('libOdtbxGmatInterface','FindOdeModel','');
if retval ~= 0
    retval = calllib('libOdtbxGmatInterface','FindOdeModel','');
end
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

% 4. Get info about the state from the ODE model
dimension = calllib('libOdtbxGmatInterface','GetStateSize');
fprintf(1,'The ODE model state has %d elements:\n',dimension);

% retval = calllib('libOdtbxGmatInterface','GetStateDescription');
% ode = calllib('libOdtbxGmatInterface','getLastMessage');
% disp(ode);

% 5.a) Pass in state data and...
epoch = 21545.0;
state = [7000.0; 1000.0; 0.0; 1.0; 7.0; 0.5]; % typical matlab column vector
%state = [1000.0; 7000.0; 0.0; 7.0; 0.5; 0.9]; % typical matlab column vector
statesize = int32(6);
fprintf(1,'Input state:\n');
fprintf(1,'   Epoch: %f\n',epoch);
fprintf(1,'   State: \n      [%f\n      %f\n      %f\n      %f\n      %f\n      %f]\n', state);
statep = libpointer('doublePtr', state);
 
retval = calllib('libOdtbxGmatInterface','SetState',epoch,statep,statesize);

% gmatState = calllib('libOdtbxGmatInterface','GetState');
% setdatatype(gmatState, 'doublePtr', dimension, 1);
% stateData = get(gmatState,'Value');
% 
% fprintf(1,'The GMAT state is:\n');
% for i = 1 : dimension
%     fprintf(1, '   %d: %f\n', i, stateData(i));
% end
% 
% fprintf(1,'The State is:\n   [');
% for i = 1 : 6
%     fprintf(1, ' %e ', stateData(i));
% end
% fprintf(1, ']\n');
% fprintf(1,'The A-Matrix data (currently bogus) is:\n   [');
% for i = 6 : 6 : 36
%     for j = 1 : 6
%        fprintf(1, ' %e ', stateData(i+j));
%     end
%     if i ~= 36
%        fprintf(1, ';\n     ');
%     end
% end
% fprintf(1, ']\n');
 
% 5.b) ...get back derivative data
dim = 0;  % Note, this value won't change
dimptr = libpointer('int32Ptr', dim);
derivsptr = calllib('libOdtbxGmatInterface','GetDerivatives', 0.0, 1, dimptr);
dimval = get(dimptr,'value');
if dimval == 0
    fprintf(1,'Failure in libOdtbxGmatInterface GetDerivatives() dimval:\n');
    errmsg = calllib('libOdtbxGmatInterface','getLastMessage');
    fprintf(1,'The error message was: %s\n',errmsg);
    
    return;
end
setdatatype(derivsptr, 'doublePtr', dimval, 1);
derivsdata = get(derivsptr,'Value');

fprintf(1,'\nThe State derivative is:\n   [');
for i = 1 : 6
    fprintf(1, ' %e ', derivsdata(i));
end
fprintf(1, ']\n');
fprintf(1,'The A-Matrix is:\n   [');
for i = 6 : 6 : 36
    for j = 1 : 6
       fprintf(1, ' %e ', derivsdata(i+j));
    end
    if i ~= 36
       fprintf(1, ';\n     ');
    end
end
fprintf(1, ']\n');

% Try the all in one call
derivsptr = calllib('libOdtbxGmatInterface','GetDerivativesForState',epoch,statep,statesize,0.0,1,dimptr);
dimval = get(dimptr,'value');
if dimval == 0
    fprintf(1,'Failure in libOdtbxGmatInterface GetDerivativesForState() dimval:\n');
    errmsg = calllib('libOdtbxGmatInterface','getLastMessage');
    fprintf(1,'The error message was: %s\n',errmsg);
    
    return;
end
setdatatype(derivsptr, 'doublePtr', dimval, 1);
derivsdata = get(derivsptr,'Value');

fprintf(1,'\nThe State derivative is:\n   [');
for i = 1 : 6
    fprintf(1, ' %e ', derivsdata(i));
end
fprintf(1, ']\n');
fprintf(1,'The A-Matrix is:\n   [');
for i = 6 : 6 : 36
    for j = 1 : 6
       fprintf(1, ' %e ', derivsdata(i+j));
    end
    if i ~= 36
       fprintf(1, ';\n     ');
    end
end
fprintf(1, ']\n');


% And now with a different state
epoch = 21575.0;
state = [21000.0; 6000.0; 420.0; 0.8; 2.8; 0.00365];
fprintf(1,'\nNew input state:\n');
fprintf(1,'   Epoch: %f\n',epoch);
fprintf(1,'   State: \n      [%f\n      %f\n      %f\n      %f\n      %f\n      %f]\n', state);
statep = libpointer('doublePtr', state);
derivsptr = calllib('libOdtbxGmatInterface','GetDerivativesForState',epoch,statep,statesize,0.0,1,dimptr);
dimval = get(dimptr,'value');
if dimval == 0
    fprintf(1,'Failure in libOdtbxGmatInterface GetDerivativesForState() dimval:\n');
    errmsg = calllib('libOdtbxGmatInterface','getLastMessage');
    fprintf(1,'The error message was: %s\n',errmsg);
    return;
end
setdatatype(derivsptr, 'doublePtr', dimval, 1);
derivsdata = get(derivsptr,'Value');

fprintf(1,'\nThe State derivative is:\n   [');
for i = 1 : 6
    fprintf(1, ' %e ', derivsdata(i));
end
fprintf(1, ']\n');
fprintf(1,'The A-Matrix is:\n   [');
for i = 6 : 6 : 36
    for j = 1 : 6
       fprintf(1, ' %e ', derivsdata(i+j));
    end
    if i ~= 36
       fprintf(1, ';\n     ');
    end
end
fprintf(1, ']\n');


% now unload the library
fprintf(1,'Unloading libOdtbxGmatInterface...\n');
unloadlibrary('libOdtbxGmatInterface');
if libisloaded('libOdtbxGmatInterface')
   fprintf(1,'Unloading failed\n');
else
   fprintf(1,'Unloaded\n');
end    
