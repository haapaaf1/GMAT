% STK_Repropagate Connects to STK propagates satellites, generates reports, and outputs run times.
% The STK_Repropagate script was designed to reduce the time it took
% to generate STK report files, after modifications to the STK
% scenario were made, and obtain more accurate STK run times. Through
% STK's connect module Matlab connect with STK and propagates
% satellites, generates reports, and outputs run times.
%
%      File folders used
%   [Main CVS Repository Directory]/GMAT_RegSetup/input/AcceptTest
%   [Main CVS Repository Directory]/GMAT_RegSetup/output/CompareResults/Performance
%   [Main CVS Repository Directory]/RefSoftFiles/STKData
%   [Main CVS Repository Directory]/GMAT_RegSetup/output/AcceptTest/Good_reports/STK
%
%   INPUTS 
%   STK scenario files that follow the GMAT Acceptance Test Plan naming convention
%
%   OUTPUTS
%   Matlab .mat file with the propagation time of each satellite.
%   STK report files
%
%   REFERENCES
%
%   REVISION HISTORY
%   Author      Date            Comment
%               (MM/DD/YYYY)
%   E.Dove      12/07/2005      Original
%   E.Dove      9/19/2007      Last Modified
%   $Id: STK_Repropagate.m,v 1.7 2007/12/28 17:24:49 edove Exp $

rerunScript = 1;
while size(rerunScript,1) ~= 0
    
    clc
    clear all
    close all
    rerunScript = 1;
    format long
    tempDir = mfilename('fullpath');
    if ispc
        temp = findstr(tempDir,'\');
        compareDir = tempDir(1:temp(size(temp,2))-1);
    else
        temp = findstr(tempDir,'/');
        compareDir = tempDir(1:temp(size(temp,2))-1);
    end
    cd(compareDir);
    cd('..');
    cd('..');
    mainDir = cd;
    cd('..');
    cvsRootDir = cd;
    AcceptTestDir = [cvsRootDir,'/GMATDocuments/AcceptTest'];    

    addpath(genpath([mainDir,'/matlab']));
    inputDir = [mainDir,'/input'];
    outputTestDir = [mainDir,'/output/AcceptTest'];
    cd(compareDir);
    
    % STK needs either all \ or / to load a scenario
    findSlash = findstr(cvsRootDir,'\');
    if isempty(findSlash)
    else
        for loop = 1:size(findSlash,2)
            cvsRootDir(findSlash(loop)) = '/';
        end
    end
    
    % Displaying welcome message
    disp('Welcome to the STK re-propagation and report generation program.');
    disp('Please make sure STK is open before running this program');
    disp('Press ENTER to continue.');
    disp(' ');
    pause

    % Prompt user on the type of reports to output
    Propagator = 'Propagator';
    CS = 'Coordinate System Parameters';
    Cb = 'Central Body Parameters';
    Performance = 'Performance';
    reportList = {Propagator,CS,Cb,Performance}.'; % Always add new options to the end of the list
    
    disp(' ');
    disp('Select the type of report(s) to generate after re-propagation');
    disp(['1. ', Propagator]);
    disp(['2. ', CS]);
    disp(['3. ', Cb]);
    disp(['4. ',Performance]);
    reportMenu = size(reportList,1);
    
    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    reportChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(reportChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                reportChoice = input('Choice: ');
                disp(' ');
            end
        end
        if reportChoice ~= [1:reportMenu]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            reportChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end
    
    % Initializing variables
    STKDir = [cvsRootDir,'/RefSoftFiles/STKData'];
    STKreportDir = [mainDir,'/output/AcceptTest/Good_reports/STK'];
    DataDir = [mainDir,'/output/AcceptTest/CompareResults/Performance'];
    folderCount = 0;
    temp = dir(STKDir);
    loopFolder = size(temp,1);
    matchFound        = 0;
    matchFoundNSG     = 0;
    matchFoundDrag    = 0;
    matchFoundSRP     = 0;
    matchFoundAllPlan = 0;
    reportFlag = 1;
    cbOnlyNames      = {'Earth_0_0_0','Venus_0_0_0','Neptune_0_0_0','Saturn_0_0_0','Jupiter_0_0_0', 'Mercury_0_0_0','Pluto_0_0_0','Mars_0_0_0','Luna_0_0_0','Uranus_0_0_0','Sun_0_0_0'}.';
    jrOnlyNames      = {'Earth_0_JR_0'}.';
    jgm2OnlyNames    = {'Earth_JGM2_0_0'}.';
    srpOnlyNames     = {'Earth_0_0_SRP','Venus_0_0_SRP','Neptune_0_0_SRP','Saturn_0_0_SRP','Jupiter_0_0_SRP', 'Mercury_0_0_SRP','Pluto_0_0_SRP','Mars_0_0_SRP','Luna_0_0_SRP','Uranus_0_0_SRP','Sun_0_0_SRP'}.';
    allPlanOnlyNames = {'AllPlanets_0_0_0'}.';
    
    % Search for folders that have scenario files. Folders to ignore are coded into the if statements.
    for loop = 1:loopFolder
        if strcmp(temp(loop).name, 'Astrogator') | strcmp(temp(loop).name, 'Config') | strcmp(temp(loop).name, 'Prefs')

        elseif (isempty(strfind(temp(loop).name,'.')) == 0) | (isempty(strfind(temp(loop).name,'_Template')) == 0)

        else
            folderCount = folderCount + 1;
            validFolder{folderCount,1} = temp(loop).name;
        end
    end
    
    % Display menu of scenarios to choose from
    disp('Select a STK scenario');
    folderMenu = size(validFolder,1); % Total number of options user can choose from
    for folderLoop = 1:folderMenu
        disp([num2str(folderLoop), '. ', validFolder{folderLoop}]);
    end
    
    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    folderChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(folderChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                folderChoice = input('Choice: ');
                disp(' ');
            end
        end
        if folderChoice ~= [1:folderMenu]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            folderChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end

    folder = validFolder{folderChoice};

    % Check to see if the selected folder contains a STK scenario file
    temp = dir([STKDir,'/',folder,'/*.sc']);
    if isempty(temp.name)
        disp('Display error message');
    elseif size(temp,1) == 1
        scenario = temp(1).name;
    elseif size(temp,1) < 1
        disp('Display error message');
    else
        disp('Display error message');
    end

    
    % **** BRAINSTORM IDEA - This could be the moment where remove ephem and store all s/c settings, 
    % convert to variable step if Performance was selected 
    % Read in all sa files into nx1 string cell array
    % Read in post ephemeris data section of the sa file
    if reportChoice == 4
        temp = dir([STKDir,'/',folder,'/*.sa']);
        saLoopEnd = size(temp,1);
        endrow = 212;
        for saLoop = 1:saLoopEnd;
            sizeSA = size(temp(saLoop).name,2);
            %fid = fopen([STKDir,'/',folder,'/',temp(saLoop).name]);
            %eval(['sa',temp(saLoop).name(1:sizeSA-3),' = textscan(fid,''%s'',212,''delimiter'', ''\n'');']);
            eval(['sa',temp(saLoop).name(1:sizeSA-3),' = caseread2line([STKDir,''/'',folder,''/'',temp(saLoop).name],endrow);']);
            %fclose(fid);
            %findEphem = strmatch('BEGIN Ephemeris',eval(['sa',temp(saLoop).name(1:sizeSA-3),'{1,1}']));
            findEphem = strmatch('BEGIN Ephemeris',eval(['sa',temp(saLoop).name(1:sizeSA-3)]));
            %eval(['sa',temp(saLoop).name(1:sizeSA-3),'{1,1} = [sa',temp(saLoop).name(1:sizeSA-3),'{1,1}(1:',num2str(findEphem-1),')];']);
            eval(['sa',temp(saLoop).name(1:sizeSA-3),' = [sa',temp(saLoop).name(1:sizeSA-3),'(1:',num2str(findEphem-1),',:)];']);

            STKverLoc = strmatch('stk.v.',eval(['sa',temp(saLoop).name(1:sizeSA-3)]));
            if isempty(STKverLoc) == 0;
                if STKverLoc(1) == 1
                    %STKver = eval(['sa',temp(saLoop).name(1:sizeSA-3),'{1,1}{1,1}(7:9)']);
                    STKver = eval(['sa',temp(saLoop).name(1:sizeSA-3),'(1,7:9)']);
                    try
                        %fid = fopen([STKDir,'/saSuffix',STKver(1),'.txt']);
                        %saSuffix = textscan(fid,'%s','delimiter','\n');
                        saSuffix = caseread([STKDir,'/saSuffix',STKver,'.txt']);
                        %fclose(fid);
                    catch;
                        break;
                    end;
                else
                    break
                end;
            end;

            % Append suffix .sa file
            %eval(['sa',temp(saLoop).name(1:sizeSA-3),'New{1,1} = [sa',temp(saLoop).name(1:sizeSA-3),'{1,1};saSuffix{1,1}];']); % store as cell char array
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'New = cellstr(strvcat(sa',temp(saLoop).name(1:sizeSA-3),',saSuffix));']); % store as cell char array
            %eval(['sa',temp(saLoop).name(1:sizeSA-3),'Save = strvcat(sa',temp(saLoop).name(1:sizeSA-3),'New{1,1}{:,1});']); % store as char array
            %strvcat(saAllPlanets_0_0_0{1,1}{:,1})

            sizeNewSa = size(eval(['sa',temp(saLoop).name(1:sizeSA-3),'New']),1);

            % Change step size and method to the perfered values
            StepControlMethod = 'RelativeError';
            findTemp          = strmatch('    StepControlMethod',eval(['sa',temp(saLoop).name(1:sizeSA-3),'New']));
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'New{',num2str(findTemp(1)),',1} = [''    StepControlMethod   ',StepControlMethod,'''];']);

            ErrorTolerance    = '1.00000000000000e-013';
            findTemp          = strmatch('    ErrorTolerance',eval(['sa',temp(saLoop).name(1:sizeSA-3),'New']));
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'New{',num2str(findTemp(1)),',1} = [''    ErrorTolerance   ',ErrorTolerance,'''];']);

            TimeStep          = '60.000000';
            findTemp          = strmatch('    TimeStep',eval(['sa',temp(saLoop).name(1:sizeSA-3),'New']));
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'New{',num2str(findTemp(1)),',1} = [''    TimeStep   ',TimeStep,'''];']);

            MinStepSize       = '0.000010';
            findTemp          = strmatch('    MinStepSize',eval(['sa',temp(saLoop).name(1:sizeSA-3),'New']));
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'New{',num2str(findTemp(1)),',1} = [''    MinStepSize   ',MinStepSize,'''];']);

            MaxStepSize       = '300.000000';
            findTemp          = strmatch('    MaxStepSize',eval(['sa',temp(saLoop).name(1:sizeSA-3),'New']));
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'New{',num2str(findTemp(1)),',1} = [''    MaxStepSize   ',MaxStepSize,'''];']);

            ReportFixed       = 'No';
            findTemp          = strmatch('    ReportOnFixedStep',eval(['sa',temp(saLoop).name(1:sizeSA-3),'New']));
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'New{',num2str(findTemp(1)),',1} = [''    ReportOnFixedStep   ',ReportFixed,'''];']);            
            
            % Backup old sa file and save new sa file as original name
            movefile([STKDir,'/',folder,'/',temp(saLoop).name(1:sizeSA-3),'.sa'],[STKDir,'/',folder,'/',temp(saLoop).name(1:sizeSA-3),'.saBk']);
            eval(['sa',temp(saLoop).name(1:sizeSA-3),'Save = char(sa',temp(saLoop).name(1:sizeSA-3),'New);']); % store as char array
            %casewrite(eval(['sa',temp(saLoop).name(1:sizeSA-3),'Save']),[STKDir,'/',folder,'/',temp(saLoop).name]);

            % removes trailing blanks
            % char(cellstr(saAllPlanets_0_0_0Save(15,:)))

            fid = fopen([STKDir,'/',folder,'/',temp(saLoop).name],'wt');
            for saInnerLoop = 1:sizeNewSa;
                fprintf(fid,'%s',eval(['char(cellstr(sa',temp(saLoop).name(1:sizeSA-3),'Save(saInnerLoop,:)));']));
                if saInnerLoop ~= sizeNewSa;
                    fprintf(fid,'\n');
                end;
            end;
            fclose(fid);
        end;
    end;
    % Connect to STK using the Connect feature. Must be set-up before running this script
    try,
    warning off
    stkInit;
    conid = stkOpen;
    warning on
    catch
        disp(' ');
        disp('STK is not open or there is a problem connecting to it.');
        disp('Try opening or restarting STK and then rerun this script.');
        disp('If that doesn''t work reset matlab and try again');
        return
    end
    
    % Check to see if an STK scenario is open. Close the current scenario if it is.
    command = 'CheckScenario /';
    isSTKopen =  stkExec(conid,command);
    if isSTKopen == '1'
        command = 'Unload / */';
        stkExec(conid,command);
    end    
    
    % Open the selected STK scenario
    disp('Please wait. Loading STK scenario and scanning for satellite objects.');
    command = ['Load / Scenario "',STKDir,'/',folder,'/',scenario,'"'];
    stkExec(conid,command);

    scenario1 = scenario(1:(size(scenario,2)-size('.sc',2)));

    % Scan for satellite objects within the scenario and store the names in a cell
    command = ['ShowNames Scenario/',scenario1,' Class Satellite'];
    allSats = stkExec(conid,command);

    findSats1 = findstr(allSats,'Satellite/');
    findSats2 = findstr(allSats,' ');
    numSats   = size(findSats1,2);
    numCbs    = size(cbOnlyNames,1);
    numDrags  = size(jrOnlyNames,1);
    numNSGs   = size(jgm2OnlyNames,1);
    numSRPs   = size(srpOnlyNames,1);
    numAllPlan= size(allPlanOnlyNames,1);

    for loop = 1:numSats
%         if numSats == 1
%             allSatsCell{loop,1} = allSats;
        if numSats ~= 1 & loop ~=numSats
            allSatsCell{loop,1} = allSats((findSats1(loop)+size('Satellite/',2)):findSats2(loop+1)-1);
        elseif numSats == 1 | (numSats ~= 1 & loop ==numSats)
            allSatsCell{loop,1} = strcat(allSats((findSats1(loop)+size('Satellite/',2)):size(allSats,2)));
        else
            disp('Display error message');
        end
    end

    % Get the start and stop time for the scenario. Satellites can not have different start and stop 
    % times from the scenario
    command = ['GetTimePeriod Scenario/',scenario1];
    timePeriod = stkExec(conid,command);
    timePeriodStart = timePeriod(2:findstr(timePeriod,',')-2);
    timePeriodStop = timePeriod(findstr(timePeriod,',')+3:size(timePeriod,2)-1);
    
    % Determine which STK report to use
    switch lower(scenario1)
        case {'deepspace'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Sun MJ2000Ec Position Velocity';
                outputInterval = '86400';
            elseif reportChoice == 2
                reportFlag = 0;
            elseif reportChoice == 3
                reportFlag = 0;
            end
        case {'deepspaceeeq'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '86400';
            elseif reportChoice == 2
                reportFlag = 0;
            elseif reportChoice == 3
                reportFlag = 0;
            end
        case {'esl2'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '43200';
            elseif reportChoice == 2
                reportFlag = 0;
            elseif reportChoice == 3
                reportFlag = 0;
            end
        case {'eml2'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '2400';
            elseif reportChoice == 2
                reportFlag = 0;
            elseif reportChoice == 3
                reportFlag = 0;
            end
        case {'geo'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '600';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters Fixed','GMAT CSParameters MJ2000Ec','GMAT CSParameters MJ2000Eq','GMAT CSParameters MODEc','GMAT CSParameters MODEq','GMAT CSParameters MOEEc','GMAT CSParameters MOEEq','GMAT CSParameters TODEc','GMAT CSParameters TODEq','GMAT CSParameters TOEEc','GMAT CSParameters TOEEq', 'GMAT CSParameters Earth GSM'  'GMAT CSParameters Earth GSE'}.';
                fileEnding = {'2Body_EarthFixed','2Body_EarthMJ2000Ec','2Body_EarthMJ2000Eq','2Body_EarthMODEc','2Body_EarthMODEq','2Body_EarthMOEEc','2Body_EarthMOEEq','2Body_EarthTODEc','2Body_EarthTODEq','2Body_EarthTOEEc','2Body_EarthTOEEq','2Body_EarthGSM','2Body_EarthGSE'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'gps'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '120';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters Fixed','GMAT CSParameters MJ2000Ec','GMAT CSParameters MJ2000Eq','GMAT CSParameters MODEc','GMAT CSParameters MODEq','GMAT CSParameters MOEEc','GMAT CSParameters MOEEq','GMAT CSParameters TODEc','GMAT CSParameters TODEq','GMAT CSParameters TOEEc','GMAT CSParameters TOEEq' 'GMAT CSParameters Earth GSM'  'GMAT CSParameters Earth GSE'}.';
                fileEnding = {'2Body_EarthFixed','2Body_EarthMJ2000Ec','2Body_EarthMJ2000Eq','2Body_EarthMODEc','2Body_EarthMODEq','2Body_EarthMOEEc','2Body_EarthMOEEq','2Body_EarthTODEc','2Body_EarthTODEq','2Body_EarthTOEEc','2Body_EarthTOEEq','2Body_EarthGSM','2Body_EarthGSE'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'hyperbolic'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '60';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters Fixed','GMAT CSParameters MJ2000Ec','GMAT CSParameters MJ2000Eq','GMAT CSParameters MODEc','GMAT CSParameters MODEq','GMAT CSParameters MOEEc','GMAT CSParameters MOEEq','GMAT CSParameters TODEc','GMAT CSParameters TODEq','GMAT CSParameters TOEEc','GMAT CSParameters TOEEq' 'GMAT CSParameters Earth GSM'  'GMAT CSParameters Earth GSE'}.';
                fileEnding = {'2Body_EarthFixed','2Body_EarthMJ2000Ec','2Body_EarthMJ2000Eq','2Body_EarthMODEc','2Body_EarthMODEq','2Body_EarthMOEEc','2Body_EarthMOEEq','2Body_EarthTODEc','2Body_EarthTODEq','2Body_EarthTOEEc','2Body_EarthTOEEq','2Body_EarthGSM','2Body_EarthGSE'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'iss'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '60';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters Fixed','GMAT CSParameters MJ2000Ec','GMAT CSParameters MJ2000Eq','GMAT CSParameters MODEc','GMAT CSParameters MODEq','GMAT CSParameters MOEEc','GMAT CSParameters MOEEq','GMAT CSParameters TODEc','GMAT CSParameters TODEq','GMAT CSParameters TOEEc','GMAT CSParameters TOEEq' 'GMAT CSParameters Earth GSM'  'GMAT CSParameters Earth GSE'}.';
                fileEnding = {'2Body_EarthFixed','2Body_EarthMJ2000Ec','2Body_EarthMJ2000Eq','2Body_EarthMODEc','2Body_EarthMODEq','2Body_EarthMOEEc','2Body_EarthMOEEq','2Body_EarthTODEc','2Body_EarthTODEq','2Body_EarthTOEEc','2Body_EarthTOEEq','2Body_EarthGSM','2Body_EarthGSE'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'jupiter1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Jupiter MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters JupiterFixed','GMAT CSParameters Jupiter MJ2000Ec','GMAT CSParameters Jupiter MJ2000Eq'}.';
                fileEnding = {'2Body_JupiterFixed','2Body_JupiterMJ2000Ec','2Body_JupiterMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Jupiter');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'mars1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Mars MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters MarsFixed','GMAT CSParameters Mars MJ2000Ec','GMAT CSParameters Mars MJ2000Eq'}.';
                fileEnding = {'2Body_MarsFixed','2Body_MarsMJ2000Ec','2Body_MarsMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Mars');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'mercury1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Mercury MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters MercuryFixed','GMAT CSParameters Mercury MJ2000Ec','GMAT CSParameters Mercury MJ2000Eq'}.';
                fileEnding = {'2Body_MercuryFixed','2Body_MercuryMJ2000Ec','2Body_MercuryMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Mercury');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'molniya'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters Fixed','GMAT CSParameters MJ2000Ec','GMAT CSParameters MJ2000Eq','GMAT CSParameters MODEc','GMAT CSParameters MODEq','GMAT CSParameters MOEEc','GMAT CSParameters MOEEq','GMAT CSParameters TODEc','GMAT CSParameters TODEq','GMAT CSParameters TOEEc','GMAT CSParameters TOEEq' 'GMAT CSParameters Earth GSM'  'GMAT CSParameters Earth GSE'}.';
                fileEnding = {'2Body_EarthFixed','2Body_EarthMJ2000Ec','2Body_EarthMJ2000Eq','2Body_EarthMODEc','2Body_EarthMODEq','2Body_EarthMOEEc','2Body_EarthMOEEq','2Body_EarthTODEc','2Body_EarthTODEq','2Body_EarthTOEEc','2Body_EarthTOEEq','2Body_EarthGSM','2Body_EarthGSE'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'moon'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Moon MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters MoonFixed','GMAT CSParameters Moon MJ2000Ec','GMAT CSParameters Moon MJ2000Eq'}.';
                fileEnding = {'2Body_MoonFixed','2Body_MoonMJ2000Ec','2Body_MoonMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Moon');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'neptune1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Neptune MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters NeptuneFixed','GMAT CSParameters Neptune MJ2000Ec','GMAT CSParameters Neptune MJ2000Eq'}.';
                fileEnding = {'2Body_NeptuneFixed','2Body_NeptuneMJ2000Ec','2Body_NeptuneMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Neptune');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'performancetests'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = 'none';
            elseif reportChoice == 2
                reportFlag = 0;
            elseif reportChoice == 3
                reportFlag = 0;
            end
        case {'pluto1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Pluto MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters PlutoFixed','GMAT CSParameters Pluto MJ2000Ec','GMAT CSParameters Pluto MJ2000Eq'}.';
                fileEnding = {'2Body_PlutoFixed','2Body_PlutoMJ2000Ec','2Body_PlutoMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Pluto');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'saturn1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Saturn MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters SaturnFixed','GMAT CSParameters Saturn MJ2000Ec','GMAT CSParameters Saturn MJ2000Eq'}.';
                fileEnding = {'2Body_SaturnFixed','2Body_SaturnMJ2000Ec','2Body_SaturnMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Saturn');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'sunsync'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Earth MJ2000 Position Velocity';
                outputInterval = '60';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters Fixed','GMAT CSParameters MJ2000Ec','GMAT CSParameters MJ2000Eq','GMAT CSParameters MODEc','GMAT CSParameters MODEq','GMAT CSParameters MOEEc','GMAT CSParameters MOEEq','GMAT CSParameters TODEc','GMAT CSParameters TODEq','GMAT CSParameters TOEEc','GMAT CSParameters TOEEq' 'GMAT CSParameters Earth GSM'  'GMAT CSParameters Earth GSE'}.';
                fileEnding = {'2Body_EarthFixed','2Body_EarthMJ2000Ec','2Body_EarthMJ2000Eq','2Body_EarthMODEc','2Body_EarthMODEq','2Body_EarthMOEEc','2Body_EarthMOEEq','2Body_EarthTODEc','2Body_EarthTODEq','2Body_EarthTOEEc','2Body_EarthTOEEq','2Body_EarthGSM','2Body_EarthGSE'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'uranus1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Uranus MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters UranusFixed','GMAT CSParameters Uranus MJ2000Ec','GMAT CSParameters Uranus MJ2000Eq'}.';
                fileEnding = {'2Body_UranusFixed','2Body_UranusMJ2000Ec','2Body_UranusMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Uranus');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        case {'venus1'}
            if (reportChoice == 1) | (reportChoice == 4)
                reportStyle = 'Venus MJ2000 Position Velocity';
                outputInterval = '300';
            elseif reportChoice == 2
                reportStyle = {'GMAT CSParameters VenusFixed','GMAT CSParameters Venus MJ2000Ec','GMAT CSParameters Venus MJ2000Eq'}.';
                fileEnding = {'2Body_VenusFixed','2Body_VenusMJ2000Ec','2Body_VenusMJ2000Eq'}.';
                outputInterval = '600';
            elseif reportChoice == 3
                reportStyle = ('GMAT CbParameters Venus');
                reportStyle1 = ('Greenwich Hour Angle');
                fileEnding = ('2Body_MHA');
                outputInterval = '600';
            end
        otherwise
            outputInterval = 'none';
            reportFlag = 0;
    end

    disp(' ');
    disp('Re-propagating satellites and generating report files. Some scenarios do not generate reports.');
    disp('The Matlab .mat file, containing run times, is located in the following directory:');
    disp(DataDir);
    disp(' ');
    if reportChoice == 1
        for loop = 1:numSats
            tic
            command = ['Propagate Scenario/',scenario1,'/Satellite/',allSatsCell{loop,1},' "',timePeriodStart,'" "',timePeriodStop,'"'];
            stkExec(conid,command);
            timeRun(loop,1) = toc;

            disp(['Satellite ',allSatsCell{loop,1},' repropagation complete. Run time: ', num2str(timeRun(loop,1)),' second(s).']);

            if strmatch(outputInterval,'none') | reportFlag == 0
            else
                command = ['Report Scenario/',scenario1,'/Satellite/',allSatsCell{loop,1},' SaveAs "',reportStyle,'" "',STKreportDir,'/STK_',scenario1,'_',allSatsCell{loop,1},'.report" "',timePeriodStart,'" "',timePeriodStop,'" ',outputInterval];
                stkExec(conid,command);
            end
        end

        % Propagate satellite, save report, and save propagation time
        timeList=cat(2,allSatsCell,num2cell(timeRun));
        savefile = [date,'_STK_',scenario1,'_Time4All.mat'];
        warning off
        save([DataDir,'/',savefile],'timeList','-v6');
        warning on
        disp('');
        disp(['Total time to run all cases: ', num2str(sum(timeRun)/60),' minutes']);
           
    elseif reportChoice == 2
        % Select the 2-body case of the the scenario
        for outerloop = 1:numCbs
            for innerloop = 1:numSats
                if isempty(strmatch(cbOnlyNames{outerloop},allSatsCell{innerloop},'exact')) == 0 
                    sat2Prop = allSatsCell{innerloop}; % Selects the two-body satellite to propagate and generate reports from
                    matchFound = matchFound + 1;
                end
            end
        end
        if matchFound == 0 | reportFlag == 0
            disp('');
            disp('The type of report you wanted to generate cannot be created without a two-body satellite');
            disp('(i.e. Earth_0_0_0 or Mars_0_0_0) in the STK scenario you selected');
            disp('or report generation was disabled for this scenario');
            
            command = 'Unload / */';
            stkExec(conid,command);
            
            stkClose(conid);
            disp(' ');
            disp('Press any alpha-numeric key and then ENTER to rerun this script or');
            disp('Press ENTER to exit.');
            rerunScript = input('');
            continue
        end

        % Propagate satellite, save report, and save propagation time
        tic
        command = ['Propagate Scenario/',scenario1,'/Satellite/',sat2Prop,' "',timePeriodStart,'" "',timePeriodStop,'"'];
        stkExec(conid,command);
        timeRun = toc;
        disp(['Satellite ',sat2Prop,' repropagation complete. Run time: ', num2str(timeRun),' second(s).']);
        
        for loop = 1:size(reportStyle,1)
            command = ['Report Scenario/',scenario1,'/Satellite/',sat2Prop,' SaveAs "',reportStyle{loop},'" "',STKreportDir,'/CSParams_STK_',scenario1,'_',fileEnding{loop},'.report" "',timePeriodStart,'" "',timePeriodStop,'" ',outputInterval];
            stkExec(conid,command);
        end
    elseif reportChoice == 3
        % Select the 2-body case of the the scenario
        for outerloop = 1:numCbs
            for innerloop = 1:numSats
                if isempty(strmatch(cbOnlyNames{outerloop},allSatsCell{innerloop},'exact')) == 0 
                    sat2Prop = allSatsCell{innerloop}; % Selects the two-body satellite to propagate and generate reports from
                    matchFound = matchFound + 1;
                end
            end
        end
        if matchFound == 0 | reportFlag == 0
            disp('');
            disp('The type of report you wanted to generate cannot be created without a two-body satellite');
            disp('(i.e. Earth_0_0_0 or Mars_0_0_0) in the STK scenario you selected');
            disp('or report generation was disabled for this scenario');
            
            command = 'Unload / */';
            stkExec(conid,command);
            
            stkClose(conid);
            disp(' ');
            disp('Press any alpha-numeric key and then ENTER to rerun this script or');
            disp('Press ENTER to exit.');
            rerunScript = input('');
            continue
        end
        
        % Propagate satellite, save report, and save propagation time
        tic
        command = ['Propagate Scenario/',scenario1,'/Satellite/',sat2Prop,' "',timePeriodStart,'" "',timePeriodStop,'"'];
        stkExec(conid,command);
        timeRun = toc;
        disp(['Satellite ',sat2Prop,' repropagation complete. Run time: ', num2str(timeRun),' second(s).']);
        
        command = ['Report Scenario/',scenario1,'/Satellite/',sat2Prop,' SaveAs "',reportStyle,'" "',STKreportDir,'/CbParams_STK_',scenario1,'_',fileEnding(1:5),'.report" "',timePeriodStart,'" "',timePeriodStop,'" ',outputInterval];
        stkExec(conid,command);
        
        command = ['Report Scenario/',scenario1,' SaveAs "',reportStyle1,'" "',STKreportDir,'/Cb_STK_',scenario1,'_',fileEnding,'.report" "',timePeriodStart,'" "',timePeriodStop,'" ',outputInterval];
        stkExec(conid,command);
    elseif reportChoice == 4
        timeRun = [];
        allCasesCell = {};
        % Select the 2-body, nsg, drag, or SRP case of the the scenario
        for outerloop = 1:numCbs
            for innerloop = 1:numSats
                if isempty(strmatch(cbOnlyNames{outerloop},allSatsCell{innerloop},'exact')) == 0 
                    sat2Prop = allSatsCell{innerloop}; % Selects the two-body satellite to propagate and generate reports from
                    matchFound = matchFound + 1;
                end
            end
        end
        for outerloop = 1:numNSGs
            for innerloop = 1:numSats
                if isempty(strmatch(jgm2OnlyNames{outerloop},allSatsCell{innerloop},'exact')) == 0 
                    sat2PropNSG = allSatsCell{innerloop}; % Selects the NSG only satellite to propagate and generate reports from
                    matchFoundNSG = matchFoundNSG + 1;
                end
            end
        end
        for outerloop = 1:numDrags
            for innerloop = 1:numSats
                if isempty(strmatch(jrOnlyNames{outerloop},allSatsCell{innerloop},'exact')) == 0 
                    sat2PropDrag = allSatsCell{innerloop}; % Selects the drag only satellite to propagate and generate reports from
                    matchFoundDrag = matchFoundDrag + 1;
                end
            end
        end
        for outerloop = 1:numSRPs
            for innerloop = 1:numSats
                if isempty(strmatch(srpOnlyNames{outerloop},allSatsCell{innerloop},'exact')) == 0 
                    sat2PropSRP = allSatsCell{innerloop}; % Selects the two-body satellite to propagate and generate reports from
                    matchFoundSRP = matchFoundSRP + 1;
                end
            end
        end
        for outerloop = 1:numAllPlan
            for innerloop = 1:numSats
                if isempty(strmatch(allPlanOnlyNames{outerloop},allSatsCell{innerloop},'exact')) == 0 
                    sat2PropAllPlan = allSatsCell{innerloop}; % Selects the two-body satellite to propagate and generate reports from
                    matchFoundAllPlan = matchFoundAllPlan + 1;
                end
            end
        end

        matchFoundTotal = matchFound + matchFoundNSG + matchFoundDrag + matchFoundSRP + matchFoundAllPlan;
    
        if matchFoundTotal == 0 | reportFlag == 0
            disp('');
            disp('The type of report you wanted to generate cannot be created.');
            disp('or report generation was disabled for this scenario');
            
            command = 'Unload / */';
            stkExec(conid,command);
            
            stkClose(conid);
            disp(' ');
            disp('Press any alpha-numeric key and then ENTER to rerun this script or');
            disp('Press ENTER to exit.');
            rerunScript = input('');
            continue
        end
        
        command =  'SetUnits / EPOCHDAY';
        stkExec(conid,command);
        perfLoopEnd = 5;
        for performanceLoop = 1:perfLoopEnd
            if exist('sat2Prop')
                % ==================== Point Mass Case ===================
                % Propagate satellite, save report, and save propagation time
                command = ['GetReport Scenario/',scenario1,'/Satellite/',sat2Prop,' "',reportStyle,'" "0.0" "0.0" 60'];
                STKdata = stkExec(conid,command);
                InitPMG_PV = str2num(STKdata(2,:));
                command = ['GetPropName Scenario/',scenario1,'/Satellite/',sat2Prop,' Active'];
                STKsatprop = stkExec(conid,command);
                if strmatch(lower(STKsatprop),'astrogator','exact')
                    %                 command = ['ASTROGATOR */Satellite/',sat2Prop,' SETVALUE MainSEQUENCE.SegmentList.Propagate.StoppingConditions.Duration.TripValue 15 Day'];
                else
                    command = ['SetState Scenario/',scenario1,'/Satellite/',sat2Prop,' Cartesian ',STKsatprop,' "0.0" "30.0" 60 J2000 "0" ',num2str(InitPMG_PV(1,2:7)*1000)];
                    stkExec(conid,command);
                end

                if matchFound ~= 0
                    tic
                    command = ['Propagate Scenario/',scenario1,'/Satellite/',sat2Prop,' "0.0" "30.0"'];
                    stkExec(conid,command);
                    timeRun(size(timeRun,1)+1,1) = toc;
                    disp(['Satellite ',sat2Prop,' repropagation complete w/o Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);

                    command = ['Report Scenario/',scenario1,'/Satellite/',sat2Prop,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'_1PM_output','.report"'];
                    stkExec(conid,command);
                    timeRun(size(timeRun,1)+1,1) = toc;

                    command = ['Report Scenario/',scenario1,'/Satellite/',sat2Prop,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'PM_1PM_output','.report"'];
                    stkExec(conid,command);

                    disp(['Satellite ',sat2Prop,' repropagation complete w/ Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);
                    timeRun(size(timeRun,1)+1,1) = timeRun(size(timeRun,1)-1,1);
                    timeRun(size(timeRun,1)+1,1) = timeRun(size(timeRun,1)-1,1);

                    allCasesCell = [allCasesCell; {['Performance_STK_',scenario1,'_1PM_noOutput.m'];['Performance_STK_',scenario1,'_1PM_output.m'];['Performance_STK_',scenario1,'PM_1PM_noOutput.m'];['Performance_STK_',scenario1,'PM_1PM_output.m']}];

                    % Save amount of integrator step sizes
                    % Report style must have ephem step size checked and the propagator must report
                    % out in integrator step size
                    fid = fopen([STKreportDir,'/Performance_STK_',scenario1,'_1PM_output','.report']);
                    currentLine = fgetl(fid);
                    countLine = 1;
                    while 1
                        currentLine = fgetl(fid);
                        if currentLine == -1
                            break
                        else
                            countLine = countLine + 1;
                        end
                    end
                    countLine = countLine - 6; % Subtract amount of lines that make up header
                    intStep(size(timeRun,1) - 3,1) = countLine;
                    intStep(size(timeRun,1) - 2,1) = countLine;
                    intStep(size(timeRun,1) - 1,1) = countLine;
                    intStep(size(timeRun,1),1) = countLine;
                    fclose(fid);
                end
                % ========================================================
            end
            if exist('sat2PropAllPlan')
                % ==================== AllPlanets Case ===================
                command = ['GetReport Scenario/',scenario1,'/Satellite/',sat2PropAllPlan,' "',reportStyle,'" "0.0" "0.0" 60'];
                STKdata = stkExec(conid,command);
                InitPMG_PV = str2num(STKdata(2,:));
                command = ['GetPropName Scenario/',scenario1,'/Satellite/',sat2PropAllPlan,' Active'];
                STKsatprop = stkExec(conid,command);
                if strmatch(lower(STKsatprop),'astrogator','exact')
                else
                    command = ['SetState Scenario/',scenario1,'/Satellite/',sat2PropAllPlan,' Cartesian ',STKsatprop,' "0.0" "30.0" 60 J2000 "0" ',num2str(InitPMG_PV(1,2:7)*1000)];
                    stkExec(conid,command);
                end

                if matchFoundAllPlan ~= 0
                    tic
                    command = ['Propagate Scenario/',scenario1,'/Satellite/',sat2PropAllPlan,' "0.0" "30.0"'];
                    stkExec(conid,command);
                    timeRun(size(timeRun,1)+1,1) = toc;
                    disp(['Satellite ',sat2PropAllPlan,' repropagation complete w/o Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);

                    command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropAllPlan,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'_AllPlanets_output','.report"'];
                    stkExec(conid,command);
                    timeRun(size(timeRun,1)+1,1) = toc;

                    command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropAllPlan,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'PM_AllPlanets_output','.report"'];
                    stkExec(conid,command);

                    disp(['Satellite ',sat2PropAllPlan,' repropagation complete w/ Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);
                    timeRun(size(timeRun,1)+1,1) = timeRun(size(timeRun,1)-1,1);
                    timeRun(size(timeRun,1)+1,1) = timeRun(size(timeRun,1)-1,1);

                    allCasesCell = [allCasesCell; {['Performance_STK_',scenario1,'_AllPlanets_noOutput.m'];['Performance_STK_',scenario1,'_AllPlanets_output.m'];['Performance_STK_',scenario1,'PM_AllPlanets_noOutput.m'];['Performance_STK_',scenario1,'PM_AllPlanets_output.m']}];

                    % Save amount of integrator step sizes
                    % Report style must have ephem step size checked and the propagator must report
                    % out in integrator step size
                    fid = fopen([STKreportDir,'/Performance_STK_',scenario1,'_AllPlanets_output','.report']);
                    currentLine = fgetl(fid);
                    countLine = 1;
                    while 1
                        currentLine = fgetl(fid);
                        if currentLine == -1
                            break
                        else
                            countLine = countLine + 1;
                        end
                    end
                    countLine = countLine - 6; % Subtract amount of lines that make up header
                    intStep(size(timeRun,1) - 3,1) = countLine;
                    intStep(size(timeRun,1) - 2,1) = countLine;
                    intStep(size(timeRun,1) - 1,1) = countLine;
                    intStep(size(timeRun,1),1) = countLine;
                    fclose(fid);
                end
                % ========================================================
            end
            if strmatch(lower(STKsatprop),'astrogator','exact')
            else
                if exist('sat2PropNSG')
                    % ======================= NSG Case =======================
                    command = ['GetReport Scenario/',scenario1,'/Satellite/',sat2PropNSG,' "',reportStyle,'" "0.0" "0.0" 60'];
                    STKdata = stkExec(conid,command);
                    InitPMG_PV = str2num(STKdata(2,:));
                    command = ['GetPropName Scenario/',scenario1,'/Satellite/',sat2PropNSG,' Active'];
                    STKsatprop = stkExec(conid,command);
                    command = ['SetState Scenario/',scenario1,'/Satellite/',sat2PropNSG,' Cartesian ',STKsatprop,' "0.0" "30.0" 60 J2000 "0" ',num2str(InitPMG_PV(1,2:7)*1000)];
                    stkExec(conid,command);

                    if matchFoundNSG ~= 0
                        tic
                        command = ['Propagate Scenario/',scenario1,'/Satellite/',sat2PropNSG,' "0.0" "30.0"'];
                        stkExec(conid,command);
                        timeRun(size(timeRun,1)+1,1) = toc;
                        disp(['Satellite ',sat2PropNSG,' repropagation complete w/o Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);

                        command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropNSG,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'_NSG_output','.report"'];
                        stkExec(conid,command);
                        timeRun(size(timeRun,1)+1,1) = toc;

                        command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropNSG,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'PM_NSG_output','.report"'];
                        stkExec(conid,command);

                        disp(['Satellite ',sat2PropNSG,' repropagation complete w/ Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);

                        allCasesCell = [allCasesCell; {['Performance_STK_',scenario1,'_NSG_noOutput.m'];['Performance_STK_',scenario1,'_NSG_output.m']}];

                        % Save amount of integrator step sizes
                        % Report style must have ephem step size checked and the propagator must report
                        % out in integrator step size
                        fid = fopen([STKreportDir,'/Performance_STK_',scenario1,'_NSG_output','.report']);
                        currentLine = fgetl(fid);
                        countLine = 1;
                        while 1
                            currentLine = fgetl(fid);
                            if currentLine == -1
                                break
                            else
                                countLine = countLine + 1;
                            end
                        end
                        countLine = countLine - 6; % Subtract amount of lines that make up header
                        intStep(size(timeRun,1) - 1,1) = countLine;
                        intStep(size(timeRun,1),1) = countLine;
                        fclose(fid);
                    end
                    % ========================================================
                end
                if exist('sat2PropDrag')
                    % ======================= Drag Case ======================
                    command = ['GetReport Scenario/',scenario1,'/Satellite/',sat2PropDrag,' "',reportStyle,'" "0.0" "0.0" 60'];
                    STKdata = stkExec(conid,command);
                    InitPMG_PV = str2num(STKdata(2,:));
                    command = ['GetPropName Scenario/',scenario1,'/Satellite/',sat2PropDrag,' Active'];
                    STKsatprop = stkExec(conid,command);
                    command = ['SetState Scenario/',scenario1,'/Satellite/',sat2PropDrag,' Cartesian ',STKsatprop,' "0.0" "30.0" 60 J2000 "0" ',num2str(InitPMG_PV(1,2:7)*1000)];
                    stkExec(conid,command);

                    if matchFoundDrag ~= 0
                        tic
                        command = ['Propagate Scenario/',scenario1,'/Satellite/',sat2PropDrag,' "0.0" "30.0"'];
                        stkExec(conid,command);
                        timeRun(size(timeRun,1)+1,1) = toc;
                        disp(['Satellite ',sat2PropDrag,' repropagation complete w/o Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);

                        command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropDrag,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'_Drag_output','.report"'];
                        stkExec(conid,command);
                        timeRun(size(timeRun,1)+1,1) = toc;

                        command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropDrag,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'PM_Drag_output','.report"'];
                        stkExec(conid,command);

                        disp(['Satellite ',sat2PropDrag,' repropagation complete w/ Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);

                        allCasesCell = [allCasesCell; {['Performance_STK_',scenario1,'_Drag_noOutput.m'];['Performance_STK_',scenario1,'_Drag_output.m']}];
                        
                        % Save amount of integrator step sizes
                        % Report style must have ephem step size checked and the propagator must report
                        % out in integrator step size
                        fid = fopen([STKreportDir,'/Performance_STK_',scenario1,'_Drag_output','.report']);
                        currentLine = fgetl(fid);
                        countLine = 1;
                        while 1
                            currentLine = fgetl(fid);
                            if currentLine == -1
                                break
                            else
                                countLine = countLine + 1;
                            end
                        end
                        countLine = countLine - 6; % Subtract amount of lines that make up header
                        intStep(size(timeRun,1) - 1,1) = countLine;
                        intStep(size(timeRun,1),1) = countLine;
                        fclose(fid);
                    end
                    % ========================================================
                end
                if exist('sat2PropSRP')
                    % ======================== SRP Case ======================
                    command = ['GetReport Scenario/',scenario1,'/Satellite/',sat2PropSRP,' "',reportStyle,'" "0.0" "0.0" 60'];
                    STKdata = stkExec(conid,command);
                    InitPMG_PV = str2num(STKdata(2,:));
                    command = ['GetPropName Scenario/',scenario1,'/Satellite/',sat2PropSRP,' Active'];
                    STKsatprop = stkExec(conid,command);
                    command = ['SetState Scenario/',scenario1,'/Satellite/',sat2PropSRP,' Cartesian ',STKsatprop,' "0.0" "30.0" 60 J2000 "0" ',num2str(InitPMG_PV(1,2:7)*1000)];
                    stkExec(conid,command);

                    if matchFoundSRP ~= 0
                        tic
                        command = ['Propagate Scenario/',scenario1,'/Satellite/',sat2PropSRP,' "0.0" "30.0"'];
                        stkExec(conid,command);
                        timeRun(size(timeRun,1)+1,1) = toc;
                        disp(['Satellite ',sat2PropSRP,' repropagation complete w/o Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);

                        command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropSRP,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'_SRP_output','.report"'];
                        stkExec(conid,command);
                        timeRun(size(timeRun,1)+1,1) = toc;

                        command = ['Report Scenario/',scenario1,'/Satellite/',sat2PropSRP,' SaveAs "',reportStyle,'" "',STKreportDir,'/Performance_STK_',scenario1,'PM_SRP_output','.report"'];
                        stkExec(conid,command);

                        disp(['Satellite ',sat2PropSRP,' repropagation complete w/ Report. Run time: ', num2str(timeRun(size(timeRun,1),1)),' second(s).']);
                        timeRun(size(timeRun,1)+1,1) = timeRun(size(timeRun,1)-1,1);
                        timeRun(size(timeRun,1)+1,1) = timeRun(size(timeRun,1)-1,1);

                        allCasesCell = [allCasesCell; {['Performance_STK_',scenario1,'_SRP_noOutput.m'];['Performance_STK_',scenario1,'_SRP_output.m'];['Performance_STK_',scenario1,'PM_SRP_noOutput.m'];['Performance_STK_',scenario1,'PM_SRP_output.m']}];

                        % Save amount of integrator step sizes
                        % Report style must have ephem step size checked and the propagator must report
                        % out in integrator step size
                        fid = fopen([STKreportDir,'/Performance_STK_',scenario1,'_SRP_output','.report']);
                        currentLine = fgetl(fid);
                        countLine = 1;
                        while 1
                            currentLine = fgetl(fid);
                            if currentLine == -1
                                break
                            else
                                countLine = countLine + 1;
                            end
                        end
                        countLine = countLine - 6; % Subtract amount of lines that make up header
                        intStep(size(timeRun,1) - 3,1) = countLine;
                        intStep(size(timeRun,1) - 2,1) = countLine;
                        intStep(size(timeRun,1) - 1,1) = countLine;
                        intStep(size(timeRun,1),1) = countLine;
                        fclose(fid);
                    end
                    % ========================================================
                end
            end

            timeList=cat(2,allCasesCell,num2cell(timeRun/60), num2cell(intStep));
            timeHeader = {'Test Case','Time (minutes)', 'Integrator Steps'};
            savefile = [date,'_STK_',scenario1,'_Time4All_Performance_',num2str(performanceLoop),'.mat'];
            warning off
            save([DataDir,'/',savefile],'timeList','timeHeader','-v6');
            warning on
            disp(' ');
        end
        command =  'SetUnits / GREGUTC';
        stkExec(conid,command);
        
        % Replace sa files created for performance runs with original sa files
        for saLoop = 1:saLoopEnd;
            sizeSA = size(temp(saLoop).name,2);
            delete([STKDir,'/',folder,'/',temp(saLoop).name]);
            movefile([STKDir,'/',folder,'/',temp(saLoop).name(1:sizeSA-3),'.saBk'],[STKDir,'/',folder,'/',temp(saLoop).name(1:sizeSA-3),'.sa']);
        end;
    end
    
    command = 'Unload / */';
    stkExec(conid,command);

    stkClose(conid);
    
    cd(compareDir);
    
    disp(' ');
    disp('Press any alpha-numeric key and then ENTER to rerun this script or');
    disp('Press ENTER to exit.');
    rerunScript = input('');
end