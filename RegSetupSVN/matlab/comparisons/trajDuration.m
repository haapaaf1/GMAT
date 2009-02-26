function [duration] = trajDuration(currCase)

% ==== Trajectory Duration (in days) ====
durationSwitch = 0;
switch durationSwitch
    case {isempty(findstr(currCase,'GEO'))}
        duration = 7;
    case {isempty(findstr(currCase,'ISS'))}
        duration = 1;
    case {isempty(findstr(currCase,'SunSync'))}
        duration = 1;
    case {isempty(findstr(currCase,'GPS'))}
        duration = 2;
    case {isempty(findstr(currCase,'Molniya'))}
        duration = 3;
    case {isempty(findstr(currCase,'Moon'))}
        duration = 3;
    case {isempty(findstr(currCase,'Pluto'))}
        duration = 3;
    case {isempty(findstr(currCase,'Venus'))}
        duration = 3;
    case {isempty(findstr(currCase,'Mars'))}
        duration = 3;
    case {isempty(findstr(currCase,'Mercury'))}
        duration = 3;
    case {isempty(findstr(currCase,'DeepSpace'))}
        duration = 365;
    case {isempty(findstr(currCase,'ESL2'))}
        duration = 180;
    case {isempty(findstr(currCase,'EML2'))}
        duration = 14;
    case {isempty(findstr(currCase,'Saturn'))}
        duration = 3;
    case {isempty(findstr(currCase,'Neptune'))}
        duration = 3;
    case {isempty(findstr(currCase,'Jupiter'))}
        duration = 3;
    case {isempty(findstr(currCase,'Uranus'))}
        duration = 3;
    otherwise
        duration = 0;
end