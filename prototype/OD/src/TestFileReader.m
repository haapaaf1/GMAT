%clear classes;
clc;
jDate = 2453158.00037077
fileReader = WeatherFileReader;
fileReader.Initialize()
[apVector,F107,F107A] = fileReader.GetApAndF107Data(jDate)



% jd1 = 2450932.919;  %  29 Apr 1998 10:03:21.600
% DataString = JD2GD(jd1)
% jd2 = 2451787.808;  %  31 Aug 2000 07:23:31.200
% DataString =  JD2GD(jd2)
% jd3 = 2459223.764;  %  09 Jan 2021 06:20:09.600
% DataString =  JD2GD(jd3)
