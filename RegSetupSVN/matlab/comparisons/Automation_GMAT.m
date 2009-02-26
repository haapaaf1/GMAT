% GMAT Script Automation Program
% Created: 20-Jul-2005 by EDove
% Last Modified: 9-Aug-2005 by EDove
%
% See also COMPARISON_TOOL1_TOOL2_PV, BUILDRUN_SCRIPT_GMAT

clc
error = 1;

while error == 1
    disp('Do you want to run the GMAT Script Automation Program?');
    runScript = input('(Y/N): ','s');

    if (runScript == 'y') | (runScript == 'Y')
        break
    elseif (runScript == 'n') | (runScript == 'N')
        return
    else
        disp(' ');
        disp('You have entered an incorrect value. Please try again.');
        disp(' ');
    end
end

rerunScript = 1;
while size(rerunScript,1) ~= 0
    clc
    clear all
    rerunScript = 1;
    % ======================= Initialize Variables ==========================
    Earth       = 'Earth';
    EarthSun    = 'EarthSun';
    EarthLuna   = 'EarthLuna';
    EarthSunLuna= 'EarthSunLuna';
    AllPlanets  = 'AllPlanets';
    pmgList     = {Earth;EarthSun;EarthLuna;EarthSunLuna;AllPlanets};
    None        = '0';
    JGM2        = 'JGM2';
    JGM3        = 'JGM3';
    EGM96       = 'EGM96';
    nsgList     = {JGM2;JGM3;EGM96;None};
    JR          = 'JacchiaRoberts';
    MSISE90     = 'MSISE90';
    dragList    = {JR;MSISE90;None};
    SRP         = 'SRP';
    altforceList= {SRP;None};
    currPath    = 'C:\\GMAT_results\\GMAT_scripts\\';
    reptPath    = 'C:\\GMAT_results\\GMAT_reports\\';
    GMATBuild   = 'Aug 9 2005';

    disp('Welcome to the GMAT Script Automation Program. Press ENTER to continue.')
    pause

    % ====== Generate menu for user to choose from a preset trajectory ======
    % ================= and initial orbital parameters ======================
    % Display menu
    disp(' ');
    disp('Choose from the available trajectory models below.');
    disp('(Enter the number for the desired trajectory)');
    disp(' ');
    traj = dir('*.traj');
    trajMenu = size(traj,1); % Total number of options user can choose from
    for trajLoop = 1:trajMenu
        chars = findstr(traj(trajLoop).name,'.') - 1;
        disp([num2str(trajLoop), '. ', traj(trajLoop).name(1:chars)]);
    end

    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    trajChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(trajChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                trajChoice = input('Choice: ');
                disp(' ');
            end
        end
        if trajChoice ~= [1:trajMenu]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            trajChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end

    % Create variable of current choice
    charsFin = findstr(traj(trajChoice).name,'.') - 1;
    for loop = 1:trajMenu
        if trajChoice == loop
            ScriptFile = ['GMAT_',traj(trajChoice).name(1:charsFin)];
        end
    end
    SatName = traj(trajChoice).name(1:charsFin);

    % ======= Menu for the user to choose a Point Mass Gravity model ========
    % Display menu
    disp('Choose from the available Point Mass Gravity (pmg) models below.');
    disp(' ');
    pmgListSize = size(pmgList,1); % Total number of options user can choose from
    for loop = 1:pmgListSize
        disp([num2str(loop), '. ', pmgList{loop}]);
    end

    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    pmgChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(pmgChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                pmgChoice = input('Choice: ');
                disp(' ');
            end
        end
        if pmgChoice ~= [1:size(pmgList,1)]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            pmgChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end

    % Append current choice w/ previous choice and send to a variable
    for loop = 1:(size(pmgList,1))
        if pmgChoice == loop
            ScriptFile = [ScriptFile,'_',pmgList{pmgChoice}];
        end
    end

    % ========= Create the comma delimited point mass variable ===========
    % ===============   for insertion into GMAT script ===================
    if strcmp(pmgList(pmgChoice), 'AllPlanets') == 0
        pmgSplit    = isstrprop(pmgList{pmgChoice}, 'upper');
        pmgSplitLoc = find(pmgSplit==1);
        pmgSizeSplit= size(pmgSplit,2);
        pmgSizeUpper= size(pmgSplitLoc,2);
        if pmgSizeUpper  == 1
            pmgGMAT = [''];
        else
            for loop=1:pmgSizeUpper
                if loop==1
                elseif (loop == 2) & (pmgSizeUpper ~= 2)
                    pmgGMAT = [pmgList{pmgChoice}(pmgSplitLoc(loop):pmgSplitLoc(loop+1)-1)];
                elseif (loop == pmgSizeUpper) & (pmgSizeUpper ~= 2)
                    pmgGMAT = [pmgGMAT,', ',pmgList{pmgChoice}(pmgSplitLoc(loop):pmgSizeSplit)];
                elseif (loop == pmgSizeUpper) & (pmgSizeUpper == 2)
                    pmgGMAT = [pmgList{pmgChoice}(pmgSplitLoc(loop):pmgSizeSplit)];
                else
                    pmgGMAT = [pmgGMAT,', ',pmgList{pmgChoice}(pmgSplitLoc(loop):pmgSplitLoc(loop+1)-1)];
                end
            end
        end
    else
        pmgGMAT = ['Sun, Luna, Mercury, Venus, Mars, Jupiter, Saturn, Uranus, Neptune, Pluto'];
    end

    % ====== Menu for the user to choose a Non-Spherical Gravity model =======
    % Display menu
    disp('Choose from the available Non-Spherical Gravity (nsg) models below.');
    disp(' ');
    nsgListSize = size(nsgList,1); % Total number of options user can choose from
    for loop = 1:nsgListSize
        if loop ~= nsgListSize
            disp([num2str(loop), '. ', nsgList{loop}]);
        else
            disp([num2str(loop), '. ','None of the above']);
        end
    end

    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    nsgChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(nsgChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                nsgChoice = input('Choice: ');
                disp(' ');
            end
        end
        if nsgChoice ~= [1:size(nsgList,1)]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            nsgChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end

    % Append current choice w/ previous choices and send to a variable
    for loop = 1:(size(nsgList,1))
        if nsgChoice == loop
            ScriptFile = [ScriptFile,'_',nsgList{nsgChoice}];
        end
    end

    % ============= Menu for the user to choose a Drag model ================
    % Display menu
    disp('Choose from the available Drag models below.');
    disp(' ');
    dragListSize = size(dragList,1); % Total number of options user can choose from
    for loop = 1:dragListSize
        if loop ~= dragListSize
            disp([num2str(loop), '. ', dragList{loop}]);
        else
            disp([num2str(loop), '. ','None of the above']);
        end
    end

    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    dragChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(dragChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                dragChoice = input('Choice: ');
                disp(' ');
            end
        end
        if dragChoice ~= [1:size(dragList,1)]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            dragChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end

    % Append current choice w/ previous choices and send to a variable
    for loop = 1:(size(dragList,1))
        if dragChoice == loop && dragChoice == 1
            ScriptFile = [ScriptFile,'_JR'];
        elseif dragChoice == loop && dragChoice ~= 1
            ScriptFile = [ScriptFile,'_',dragList{dragChoice}];
        end
    end

    % ======== Menu for the user to choose any other Force models ===========
    % Display menu
    disp('Choose from the available alternate Force models below.');
    disp(' ');
    altforceListSize = size(altforceList,1); % Total number of options user can choose from
    for loop = 1:altforceListSize
        if loop ~= altforceListSize
            disp([num2str(loop), '. ', altforceList{loop}]);
        else
            disp([num2str(loop), '. ','None of the above']);
        end
    end

    % User inputs value. Program warns the user of any incorrectly entered value
    error = 1;
    altforceChoice = input('Choice: ');
    disp(' ');
    while error == 1
        error = 2;
        while error == 2
            if size(altforceChoice,1) ~= 0
                break
            else
                error = 2;
                disp('You have entered an incorrect value. Please try again.');
                altforceChoice = input('Choice: ');
                disp(' ');
            end
        end
        if altforceChoice ~= [1:size(altforceList,1)]
            error = 1;
            disp('You have entered an incorrect value. Please try again.');
            altforceChoice = input('Choice: ');
            disp(' ');
        else
            break
        end
    end

    % Append current choice w/ previous choices and send to a variable
    for loop = 1:(size(altforceList,1))
        if altforceChoice == loop
            ScriptFile = [ScriptFile,'_',altforceList{altforceChoice}];
        end
    end
    ScriptFileAll = [ScriptFile,'.m'];

    intStep = input('Enter the integrator''s fixed step size(seconds): ');
    outStep = input('Enter the report output step size(seconds): ');
    error = 1;
    while error == 1
        error = 2;
        while error == 2
            if size(intStep,1) == 0 || size(outStep,1) ==0
                error = 2;
                disp(' ');
                disp('You have entered an incorrect value. Please try again.');
                intStep = input('Enter the integrator''s fixed step size(seconds): ');
                outStep = input('Enter the report output step size(seconds): ');
            else
                break
            end
        end
        if mod(outStep,intStep) ~= 0
            error = 1;
            disp(' ');
            disp('The report output step size can not be generated');
            disp('with the given integrator step size');
            disp('Please enter the values again');
            intStep = input('Enter the integrator''s fixed step size(seconds): ');
            outStep = input('Enter the report output step size(seconds): ');
        else
            break
        end
    end
    error = 1;
    while error == 1
        timeProp= input('How many days would you like to propagate this orbit?: ');
        if size(timeProp,1) == 0
            error = 1;
            disp(' ');
            disp('You have entered an incorrect value. Please try again.');
        else
            break
        end
    end

    % ================  Create the users's GMAT script ================
    % ~~Create GMAT's Spacecraft section from preset conditions

    % Scan trajectory file and save to structure variable
    fid = fopen(traj(trajChoice).name);
    txtArray  = textscan(fid, '%s');
    fclose(fid);
    txtArraySize = size(txtArray{1},1);
    fields = {'A'};
    txtStruct = cell2struct(txtArray,fields,size(txtArray{1},1));

    % Reformat structure file so one line of GMAT code is a string
    line = 1;
    txtByLine(1) = {''};
    for loop=1:txtArraySize
        findColon = findstr(char(txtStruct.A(loop)),';');
        TorF      = size(findColon,1);
        txtByLine{line,1} = [txtByLine{line,1},' ',char(txtStruct.A(loop))];
        if TorF == 1
            line = line + 1;
            txtByLine(line,1) = {''};
        end
    end
    txtByLineSize = size(txtByLine,1);

    % ~~Create file and add header comment section
    fid = fopen(ScriptFileAll,'w');
    fprintf(fid,'%% MATLAB Autogeneration GMAT Script File\n');
    fprintf(fid,['%% Used MATLAB script that was last modified ' date ' by EDove\n\n']);

    for loop=1:txtByLineSize
        fprintf(fid,[txtByLine{loop},'\n']);
    end

    % ~~Create GMAT's Forcemodel Section
    if nsgChoice ~= size(nsgList,1)
        fprintf(fid,['Create ForceModel ',Earth,char(nsgList(nsgChoice)),';','\n']);
        ForceModel = [Earth,char(nsgList(nsgChoice))];
    else
        fprintf(fid,['Create ForceModel ',Earth,'2Body',';','\n']);
        ForceModel = [Earth,'2Body'];
    end
    fprintf(fid,['GMAT ',ForceModel,'.PrimaryBodies = {',Earth,'};','\n']);
    if dragChoice ~= size(dragList,1)
        fprintf(fid,['GMAT ',ForceModel,'.Drag = ',char(dragList(dragChoice)),';','\n']);
        fprintf(fid,['GMAT ',ForceModel,'.Drag.F107 = 150;\n']);
        fprintf(fid,['GMAT ',ForceModel,'.Drag.F107A = 150;\n']);
        fprintf(fid,['GMAT ',ForceModel,'.Drag.MagneticIndex = 3;\n']);
    else
        fprintf(fid,['GMAT ',ForceModel,'.Drag = ','None',';','\n']);
    end
    if altforceChoice == 1
        fprintf(fid,['GMAT ',ForceModel,'.SRP = ','On',';','\n']);
        fprintf(fid,['GMAT ',ForceModel,'.SRP.Flux_Pressure = 4.53443218374393e-006;\n']);
    else
        fprintf(fid,['GMAT ',ForceModel,'.SRP = ','Off',';','\n']);
    end
    if nsgChoice ~= size(nsgList,1)
        fprintf(fid,['GMAT ',ForceModel,'.Gravity.',Earth,'.Model = .\\files\\gravity\\earth\\',char(nsgList(nsgChoice)),'.grv;','\n']);
        fprintf(fid,['GMAT ',ForceModel,'.Gravity.',Earth,'.Degree = 20;','\n']);
        fprintf(fid,['GMAT ',ForceModel,'.Gravity.',Earth,'.Order = 20;','\n']);
    else
        fprintf(fid,['GMAT ',ForceModel,'.Gravity.',Earth,'.Model = ','JGM2',';','\n']);
        fprintf(fid,['GMAT ',ForceModel,'.Gravity.',Earth,'.Degree = 0;','\n']);
        fprintf(fid,['GMAT ',ForceModel,'.Gravity.',Earth,'.Order = 0;','\n']);
    end
    fprintf(fid,['GMAT ',ForceModel ,'.PointMasses   = {',pmgGMAT,'};\n\n']);

    % ~~Create GMAT's Propagator Section
    Prop = 'RKV89';
    fprintf(fid,['Create Propagator ',Prop,';\n']);
    fprintf(fid,['GMAT ',Prop,'.FM = ',ForceModel,';\n']);
    fprintf(fid,['GMAT ',Prop,'.Type = RungeKutta89;\n']);
    fprintf(fid,['GMAT ',Prop,'.StepSize = ',num2str(intStep),';\n']);
    fprintf(fid,['GMAT ',Prop,'.Accuracy = 9.9999999999999994e-012;\n']);
    fprintf(fid,['GMAT ',Prop,'.MinStep = ',num2str(intStep),';\n']);
    fprintf(fid,['GMAT ',Prop,'.MaxStep = ',num2str(intStep),';\n']);
    fprintf(fid,['GMAT ',Prop,'.MaxStepAttempts = 50;\n\n']);

    % ~~Create GMAT's Report Section
    ReportName = [SatName,'_Report'];
    fprintf(fid,['Create ReportFile ', ReportName,'\n']);
    fprintf(fid,['GMAT ',ReportName,'.Filename =  ',reptPath,ScriptFile,'.report;\n']);
    fprintf(fid,['GMAT ',ReportName,'.Precision = 12;\n']);
    fprintf(fid,['GMAT ',ReportName,'.WriteHeaders = Off;\n']);
    fprintf(fid,['GMAT ',ReportName,'.ColumnWidth = 20;\n\n']);

    fprintf(fid,'%%-----------Begin Propagation and Report Generation--------\n');
    fprintf(fid,'%% Propagate based on preset propagation parameters\n');
    fprintf(fid,'%% and current stop conditions.\n\n');

    fprintf(fid,'%% Output Report file data for each propagation set in the FOR loop\n');
    fprintf(fid,['Report ',ReportName,' ',SatName,'.CurrA1MJD ',SatName,'.X ',SatName,'.Y ',SatName,'.Z ',SatName,'.VX ',SatName,'.VY ',SatName,'.VZ;\n']);
    fprintf(fid,['For OutputStepSize = 1:',num2str(timeProp*60*60*24/outStep),';\n']);
    fprintf(fid,['\tPropagate   ',Prop,'(',SatName,', {',SatName,'.ElapsedSecs = ',num2str(outStep-intStep),'});\n']);
    fprintf(fid,['\tReport      ',ReportName,' ',SatName,'.ElapsedSecs ',SatName,'.X ',SatName,'.Y ',SatName,'.Z ',SatName,'.VX ',SatName,'.VY ',SatName,'.VZ;\n']);
    fprintf(fid,'EndFor ;\n');

    fclose(fid);

    % ==== Display the GMAT filename used for the users furure reference ====
    disp(' ');
    disp('The following GMAT file has been generated:');
    disp(ScriptFileAll);
    disp(' ');

    disp('Press any key and then ENTER to create another GMAT script or');
    disp('Press ENTER to exit.');
    rerunScript = input('');
end

