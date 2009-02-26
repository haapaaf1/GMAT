function [Tool1Folder,Tool2Folder] = dispToolMenu_2

% ############### Display the menu for Tool1 #######################
disp(' ');
disp('Choose the first tool for comparison from the menu below:');
availTooltemp1 = dir('./Good_reports/*Exact');
availTooltemp2 = dir('*GMAT*_reports');
availTool = struct('name', {availTooltemp1(:).name, availTooltemp2(:).name}');
toolMenu = size(availTool,1); % Total number of options user can choose from
for toolLoop = 1:toolMenu
    if strcmp(availTool(toolLoop).name,'Exact');
        disp([num2str(toolLoop), '. ', availTool(toolLoop).name]);
    else;
        chars(toolLoop) = findstr(availTool(toolLoop).name,'_report') - 1;
        disp([num2str(toolLoop), '. ', availTool(toolLoop).name(1:chars(toolLoop))]);
    end;
end

% User inputs value. Program warns the user of any incorrectly entered value
error = 1;
toolChoice = input('Choice: ');
disp(' ');
while error == 1
    error = 2;
    while error == 2
        if size(toolChoice,1) ~= 0
            break
        else
            error = 2;
            disp('You have entered an incorrect value. Please try again.');
            toolChoice = input('Choice: ');
            disp(' ');
        end
    end
    if toolChoice ~= [1:toolMenu]
        error = 1;
        disp('You have entered an incorrect value. Please try again.');
        toolChoice = input('Choice: ');
        disp(' ');
    else
        break
    end
end

if strcmp(availTool(toolChoice).name,'Exact');
    Tool1Folder = availTool(toolChoice).name;
else;
    Tool1Folder = availTool(toolChoice).name(1:chars(toolChoice));
end;

% ############### Display the menu for Tool2 #######################
availTool2 = {};
count = 1;
for loop = 1:toolMenu
    if toolChoice ~= loop
        if strcmp(availTool(loop).name,'Exact');
            availTool2{count,1} = availTool(loop).name;
        else;
            availTool2{count,1} = availTool(loop).name(1:chars(loop));
        end;
        count = count + 1;
    end
end

disp('Choose the second tool for comparison from the menu below:');
toolMenu2 = size(availTool2,1); % Total number of options user can choose from
for toolLoop = 1:toolMenu2
    disp([num2str(toolLoop), '. ', char(availTool2(toolLoop))]);
end

% User inputs value. Program warns the user of any incorrectly entered value
error = 1;
toolChoice = input('Choice: ');
disp(' ');
while error == 1
    error = 2;
    while error == 2
        if size(toolChoice,1) ~= 0
            break
        else
            error = 2;
            disp('You have entered an incorrect value. Please try again.');
            toolChoice = input('Choice: ');
            disp(' ');
        end
    end
    if toolChoice ~= [1:toolMenu2]
        error = 1;
        disp('You have entered an incorrect value. Please try again.');
        toolChoice = input('Choice: ');
        disp(' ');
    else
        break
    end
end
Tool2Folder = char(availTool2(toolChoice));