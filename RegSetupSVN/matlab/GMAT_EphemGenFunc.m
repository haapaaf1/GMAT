function [Ephem] = GMAT_EphemGenFunc(X0, Times, Spacecraft, ForceModel, Propagator, tempfile, reportFlag)

% -------------------------------------------------------------------------
% ---------------------------  Initializations  ---------------------------
% -------------------------------------------------------------------------
format long g
if nargin < 6
    disp('Warning:  Not all inputs were defined in GMATProp.m')
    Ephem = [];
    return
end

global FullArray 
FullArray = {};
ClearGMAT

% -------------------------------------------------------------------------
% ----------------------  Create Spacecraft -------------------------------
% -------------------------------------------------------------------------
Create Spacecraft Sat;
Spacecraft.X = X0(1,1);
Spacecraft.Y = X0(2,1); 
Spacecraft.Z = X0(3,1); 
Spacecraft.VX = X0(4,1); 
Spacecraft.VY = X0(5,1); 
Spacecraft.VZ = X0(6,1); 
SatString = Struct2Str('Sat',Spacecraft); FullArray = {};
%  Send Spacecraft
for i = 1:size(SatString,1)
    CallGMAT('GMAT', SatString{i});
end

% -------------------------------------------------------------------------
% ----------------------  Create Force Model ------------------------------
% -------------------------------------------------------------------------
Create ForceModel Forces;
if ~isempty(ForceModel)
    FMString  = Struct2Str('Forces',ForceModel); FullArray = {};
    for i = 1:size(FMString,1)
        CallGMAT('GMAT', FMString{i});
    end
end

% -------------------------------------------------------------------------
% ----------------------  Create Integrator -------------------------------
% -------------------------------------------------------------------------
Create Propagator Prop
if ~isempty(ForceModel)
   GMAT Prop.FM = Forces;
end
if ~isempty(Propagator)
    PropString  = Struct2Str('Prop',Propagator); FullArray = {};
    for i = 1:size(PropString,1)
        CallGMAT('GMAT', PropString{i});
    end
end

% -------------------------------------------------------------------------
% ----------------------  Create the Report -------------------------------
% -------------------------------------------------------------------------
%  Create the reportfile
Create ReportFile TempData
FilenameStr = ['TempData.Filename = ' tempfile]; 
CallGMAT('GMAT', FilenameStr);
GMAT TempData.WriteHeaders = Off;
if reportFlag == 1
    reportOutput = 'GMAT TempData.Add = ''{Sat.UTCGregorian Sat.X Sat.Y Sat.Z Sat.VX Sat.VY Sat.VZ}''';
    eval(reportOutput);
end;

% -------------------------------------------------------------------------
% -------------------  Create Variables\Arrays ----------------------------
% -------------------------------------------------------------------------
Create Variable DaysToProp
Create Variable propLoop
eval(['Create Array TimesToProp[',num2str(max(size(Times))),',1]']); % Create an Array in GMAT with the same size as the Times array
for loop = 1:max(size(Times))
    eval(['GMAT TimesToProp(',num2str(loop),',1) = ',num2str(Times{loop},16)]);
end

% Report initial state if intermediate prop steps are not outputted
if reportFlag == 0
    reportOutput = 'Report TempData Sat.UTCGregorian Sat.X Sat.Y Sat.Z Sat.VX Sat.VY Sat.VZ';
    eval(reportOutput);
end

% *****Start For Loop*****
eval(['For propLoop = 2:',num2str(max(size(Times)))]);

% -------------------------------------------------------------------------
% --------------  Calculate ElapsedDays for Propagation -------------------
% -------------------------------------------------------------------------

    % Evaluate equation
    GMAT DaysToProp = TimesToProp(propLoop,1) - Sat.UTCModJulian;

% -------------------------------------------------------------------------
% ----------------------  Perform the Propagation -------------------------
% -------------------------------------------------------------------------
    If DaysToProp >= 0;
       propStr = 'Propagate Prop(Sat) {Sat.ElapsedDays = DaysToProp, StopTolerance = 1e-010};';
       CallGMAT('GMAT', propStr);
    Else;
       propStr = 'Propagate BackProp Prop(Sat) {Sat.ElapsedDays = DaysToProp, StopTolerance = 1e-010};';
       CallGMAT('GMAT', propStr);
    EndIf;
    
    if reportFlag == 0
        reportOutput = 'Report TempData Sat.UTCGregorian Sat.X Sat.Y Sat.Z Sat.VX Sat.VY Sat.VZ';
        eval(reportOutput);
    end

% *****End For Loop*****
EndFor;

BuildRunGMAT

% -------------------------------------------------------------------------
% ---------------------- Extract the Ephemeris Data -----------------------
% -------------------------------------------------------------------------

if isempty(strfind(lower(reportOutput),'gregorian')) == 0; % Gregorian DateFormat used
    fid1 = fopen(tempfile);
    loadEphem = textscan(fid1,'%s','delimiter','\n');
    fclose(fid1);

    ephemRowSize = size(loadEphem{1,1},1);
    ephemColSize = size(loadEphem{1,1}{1,1},2);
    space        = ' ';
    colon        = ':';
    period       = '.';
    slash        = '/';
    for ephemLoop = 1:ephemRowSize
        dd      = loadEphem{1,1}{ephemLoop,1}(1:2);
        mmm     = loadEphem{1,1}{ephemLoop,1}(4:6);
        yyyy    = loadEphem{1,1}{ephemLoop,1}(8:11);
        HH      = loadEphem{1,1}{ephemLoop,1}(13:14);
        MM      = loadEphem{1,1}{ephemLoop,1}(16:17);
        SS      = loadEphem{1,1}{ephemLoop,1}(19:20);
        SSS     = loadEphem{1,1}{ephemLoop,1}(22:24);
        ephemDate{ephemLoop,1} = [dd,space,mmm,space,yyyy,space,HH,colon,MM,colon,SS,period,SSS]; % Arrange DateFormat. All String cell arrays.
        ephemCartState(ephemLoop,:) = str2num(loadEphem{1,1}{ephemLoop,1}(25:ephemColSize)); % [X Y Z VX VY VZ] state. Numeric cell arrays.
        Ephem = [ephemDate,num2cell(ephemCartState)];
    end
else % ModJulian DateFormat used
    loadEphem = textread(tempfile);
    loadEphem(:,8) = [];
    Ephem = num2cell(loadEphem);
end    