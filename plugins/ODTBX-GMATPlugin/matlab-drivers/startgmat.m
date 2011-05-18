function [retval] = startgmat(filename);

% STARTGMAT GMAT engine initialization routine
%  [retval] = startgmat() Loads the GMAT interface and core library, 
%  initializes the objects in GMAT using the default script
%  (defaultOdtbx.script), populates GMAT's sandbox with the configuration
%  described in the script, runs the script, forcing GMAT to establish all
%  object-to-object interconnections, and then locates the first ODE model 
%  in the Mission Control Sequence for use by ODTBX.
%
%  [retval] = startgmat(filename) Loads the GMAT interface and core 
%  library, initializes the objects in GMAT using the specified script, 
%  populates GMAT's sandbox with the configuration described in the script, 
%  runs the script, forcing GMAT to establish all object-to-object 
%  interconnections, and then locates the first ODE model in the Mission 
%  Control Sequence for use by ODTBX.
% 
%
%   INPUTS
%   VARIABLE        SIZE    	DESCRIPTION (Optional/Default)
%   filename        (1x1)       (Optional) GMAT script used to configure
%                               and populate the GMAT sandbox.
%
%   OUTPUTS
%   retval          (1x1)       Integer return code indicating the status
%                               of the GMAT startup process.
%
%   keyword: GMAT Integrators Forces 
%
%
% (This file is part of GMAT, The General Mission Analysis Tool, and is
%  distributed under the NASA Open Source Agreement.  See file source for
%  more details.)

% GMAT: General Mission Analysis Tool
%
% Copyright (c) 2002-2011 United States Government as represented by the
% Administrator of The National Aeronautics and Space Administration.
% All Other Rights Reserved.
%
% Developed jointly by NASA/GSFC and Thinking Systems, Inc. under NASA
% Prime Contract NNG10CP02C, Task Order 28.
%
% Author: Darrel J. Conway, Thinking Systems, Inc.
% Created: 2011/05/17

% Prepare configuration script options
if (nargin >= 1)
    if filename == ''
        filename = 'defaultOdtbx.script';
    end
else
    filename = 'defaultOdtbx.script';
end

% Load the library
if ~(libisloaded('libOdtbxGmatInterface'))
    [notfound, warnings] = loadlibrary('libOdtbxGmatInterface', ...
        @interfacewrapper);
    
    if (~isempty(notfound)) || (~libisloaded('libOdtbxGmatInterface'))
       error('Unable to load the libOdtbxGmatInterface shared library.  Warnings were: %s\n', warnings);
    end
end

% Start the GMAT engine
retval = calllib('libOdtbxGmatInterface','StartGmat');
if retval ~= 0
    error('GMAT failed to start.  This may be caused by missing or misconfigured files; check the gmat_startup_file.\n');
end
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

% Load and populate a configuration
retval = calllib('libOdtbxGmatInterface', 'LoadScript', filename);
if retval ~= 0
    error('The configuration script "%s" failed to load\n', filename); 
end
retval = calllib('libOdtbxGmatInterface','RunScript');
if retval ~= 0
    error('The configuration script "%s" failed to run\n', filename); 
end

% Show the run status
ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

% Set the ODE model to the one named in the script (currently uses the
% first model found; will use named models later on)
retval = calllib('libOdtbxGmatInterface','FindOdeModel','');
if retval ~= 0
    retval = calllib('libOdtbxGmatInterface','FindOdeModel','');
end
if retval ~= 0
    error('Unable to locate an ODE model for use by ODTBX.  Please check the configuration script "%s"\n', filename);
end

ode = calllib('libOdtbxGmatInterface','getLastMessage');
disp(ode);

end %function
