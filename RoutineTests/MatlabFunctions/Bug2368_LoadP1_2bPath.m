%   Modification History
%   ---------------------------------------------------------------------------------
%   05/12/2010 - E.Dove:  Created

% Determine the main directory that the Phase 1/2b code resides in
P1_2bRootDir = fileparts(fileparts(mfilename('fullpath')));

% Add paths to the current matlab session that are needed for this phase
addpath(genpath([P1_2bRootDir,'\FormDesign']));
addpath([P1_2bRootDir,'\MonteCarlo']);
