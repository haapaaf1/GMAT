function [] = saveLaTex_3(maxDiffs2XL,mainDir,Tool1Folder,Tool2Folder,scriptDir,Tool2Dir,saveDir)

texTool1Folder = Tool1Folder;
texTool2Folder = Tool2Folder;
if isempty(strfind(lower(Tool1Folder),'macgmat')) & isempty(strfind(lower(Tool1Folder),'linuxgmat')) & (isempty(strfind(lower(Tool1Folder),'gmat')) == 0);
    texTool1Folder = [Tool1Folder(1:strfind(lower(Tool1Folder),'gmat')-1),'WinGMAT'];
end;
if isempty(strfind(lower(Tool2Folder),'macgmat')) & isempty(strfind(lower(Tool2Folder),'linuxgmat')) & (isempty(strfind(lower(Tool2Folder),'gmat')) == 0);
    texTool2Folder = [Tool2Folder(1:strfind(lower(Tool2Folder),'gmat')-1),'WinGMAT'];
end;

% ======== Format data for exporting into a LaTeX document =========
Data{1}      = {};
Data{2}      = {};
Data{3}      = {};
Data{4}      = {};
Data{5}      = {};
RowLabels{1} = {};
for testCase = 1:(size(maxDiffs2XL,1)-1);
    for loop = 1:5
        % ==== Convert underscores to dashes for the current case name, since Latex can't hande underscores
        underLoc    = findstr(char(maxDiffs2XL(testCase+1,1)),'_');

        if size(underLoc,2) == 1; % Check to see if only one underscore appears in case name
            currCase2 = [maxDiffs2XL{testCase+1,1}(1,1:underLoc(1)-1),'-',maxDiffs2XL{testCase+1,1}(1,underLoc(1)+1:size(maxDiffs2XL{testCase+1,1},2))];
        elseif size(underLoc,2) == 0; % Check to see if no underscores appears in case name
            currCase2 = char(maxDiffs2XL(testCase+1,1));
        else; % Check to see if more than one underscore appears in case name
            currCase2 = [maxDiffs2XL{testCase+1,1}(1,1:underLoc(1)-1),'-'];
            for innerLoop = 1:size(underLoc,2)
                if innerLoop == size(underLoc,2)
                    currCase2 = [currCase2,maxDiffs2XL{testCase+1,1}(1,underLoc(innerLoop)+1:size(maxDiffs2XL{testCase+1,1},2))];
                else
                    currCase2 = [currCase2,maxDiffs2XL{testCase+1,1}(1,underLoc(innerLoop)+1:underLoc(innerLoop+1)-1),'-'];
                end
            end
        end
        % ================================================================
        digits      = 10;
        RowLabels{testCase,1} = currCase2;

        switch loop
            case 1
                ColumnLabels{1,loop} = {'Test Case' 'Altitude (m)' 'Eccentricity' 'M. Anomaly (deg)' 'M. Motion (rad/sec)' 'Period (sec)'}.';
                Data{loop}    = [Data{loop};[maxDiffs2XL(testCase+1,2),maxDiffs2XL(testCase+1,5),maxDiffs2XL(testCase+1,9),maxDiffs2XL(testCase+1,10),maxDiffs2XL(testCase+1,11)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Central Body Dependent Parameter Differences (1)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CbParams-1.tex'];
            case 2
                ColumnLabels{1,loop} = {'Test Case' 'Semi-major Axis (m)' 'True Anomaly (deg)' 'Semilatus Rectum(m)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,15),maxDiffs2XL(testCase+1,16),maxDiffs2XL(testCase+1,17)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Central Body Dependent Parameter Differences (2)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CbParams-2.tex'];
            case 3
                ColumnLabels{1,loop} = {'Test Case' 'Apoapsis Rad. (m)' 'Periapsis Rad. (m)' 'Apo. Vel. ($m/sec$)' 'Per. Vel. ($m/sec$)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,12),maxDiffs2XL(testCase+1,13),maxDiffs2XL(testCase+1,18),maxDiffs2XL(testCase+1,19)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Central Body Dependent Parameter Differences (3)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CbParams-3.tex'];
            case 4
                ColumnLabels{1,loop} = {'Test Case' 'C3-Energy ($m^2/sec^2$)' 'Latitude (deg)' 'Longitude (deg)' 'MHA (deg)' 'LST (deg)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,4),maxDiffs2XL(testCase+1,6),maxDiffs2XL(testCase+1,7),maxDiffs2XL(testCase+1,20),maxDiffs2XL(testCase+1,21)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Central Body Dependent Parameter Differences (4)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CbParams-4.tex'];
            case 5
                ColumnLabels{1,loop} = {'Test Case' 'Beta Angle (deg)' '(RxV)-Mag ($m^2/sec$)' 'R-Mag (m)'}.';
                Data{loop} = [Data{loop};[maxDiffs2XL(testCase+1,3),maxDiffs2XL(testCase+1,8),maxDiffs2XL(testCase+1,14)]];
                Caption{loop} = [texTool1Folder,'/',texTool2Folder,' Central Body Dependent Parameter Differences (5)'];
                saveFile{loop}= [Tool1Folder,'-',Tool2Folder,'-CbParams-5.tex'];
        end
    end
end
for texLoop = 1:5
    Label       = ['Table: ',texTool1Folder,'-',texTool2Folder,' CB Parameters Set ',num2str(texLoop)];
    if texLoop == 1
        digits = 6;
    end
    cd(scriptDir);
    BasicLatexTable(Data{texLoop}, ColumnLabels{1,texLoop}, RowLabels, Caption{texLoop}, Label, digits, saveFile{texLoop}, saveDir)
    cd(Tool2Dir);
end
% =================================================================