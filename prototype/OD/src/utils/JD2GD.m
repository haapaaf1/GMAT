function [YearOrDateStr,Month,Day,Hour,Minute,Second] = JD2GD(JD)
 

%% Initializations
        % Jan Feb Mar Apr May Jun Jul Aug Sep Oct Nov Dec
LMonth = [31  28  31  30   31  30  31  31  31  30  30  31];

%% Determine the year
T_1900  = (JD - 2415019.5)/365;
Year    = 1900 + fix(T_1900);
LeapYrs = fix((Year - 1900 -1)*0.25);

%%  Determine day of year
Days    = (JD - 2415019.5) - ((Year-1900)*365.0+LeapYrs);
if Days < 1.0
    Year = Year - 1;
    LeapYrs = fix((Year - 1900 -1)*0.25);
    Days    = (JD - 2415019.5) - ((Year-1900)*365.0+LearYrs);
end
if mod(Year,4) == 0
    LMonth(2) = 29;
end
DayofYear = fix(Days);
 
%% Determine the month of year
Month  = 1;
while sum(LMonth(1:Month)) + 1 <= DayofYear
    Month = Month + 1;
end
daySum = sum(LMonth(1:Month-1));

%% Determine day and time of day
Day = DayofYear - daySum;
tau = (Days - DayofYear)*24;
Hour = fix(tau);
Minute  = fix((tau -Hour)*60);
Second = (tau - Hour - Minute/60)*3600;

%% Configure output based on number of output arguments
if nargout == 1
    monString = {'Jan' 'Feb' 'Mar' 'Apr' 'May' 'Jun' 'Jul' 'Aug' 'Sep' 'Oct' 'Nov' 'Dec'};
    intSeconds = floor(Second);
    if intSeconds <= 9
        if intSeconds == 0
            secString = '00';
        else
            secString = ['0' num2str(intSeconds)];
        end
    else
        secString = num2str(intSeconds);
    end
    fracOfSecond = num2str(round((Second - intSeconds)*1000));
    YearOrDateStr = [num2str(Day)  ' ' monString{Month} ' ' num2str(Year) ' ' num2str(Hour) ':' num2str(Minute) ':' secString ':' fracOfSecond];
else
    YearOrDateStr = Year;
end
