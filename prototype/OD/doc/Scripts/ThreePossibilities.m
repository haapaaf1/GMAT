% Script Example 1
%--------------------------------------------------------------------------
%-----------  Define the measurements between MySat and Maui Ground Station
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
% Original version; minor tweak
%--------------------------------------------------------------------------
Create MeasurementGroup MauiData;
MauiData.DataFile    = MauiData.B3
MauiData.FileFormat  = B3 %  Is this needed?
MauiData.AddModel    = {‘Range’,MyGS.Sensor1,MySat,’Bias’,.1,’NoiseSigma’,...
                        .01,’TimeConstant’,7200};
MauiData.AddModel    = {‘AveragedDoppler’,MyGS,MySat,’InitialBias’,.1,...
                        ’NoiseSigma’,.01,’TimeConstant’,7200};


%--------------------------------------------------------------------------
% Named model version
%--------------------------------------------------------------------------
Create MeasurementModel MySatMauiRange
MySatMauiRange.Type = Range;
MySatMauiRange.Anchor = MyGS.Sensor1;      % This is the end node where the 
                                           % measurement data was collected
MySatMauiRange.Participants = { MySat };   % Anything else measured
MySatMauiRange.Bias         = 0.1;
MySatMauiRange.NoiseSigma   = 0.01;
MySatMauiRange.TimeConstant = 7200;

Create MeasurementModel MySatMauiDoppler
MySatMauiDoppler.Type         = AveragedDoppler;
MySatMauiDoppler.Anchor       = MyGS;
MySatMauiDoppler.Participants = { MySat };
MySatMauiDoppler.Bias         = 0.075;
MySatMauiDoppler.NoiseSigma   = 0.02;
MySatMauiDoppler.TimeConstant = 7200;

Create MeasurementGroup MauiData;
MauiData.DataFile    = MauiData.B3
MauiData.FileFormat  = B3;
MauiData.AddModel    = {MySatMauiRange, MySatMauiDoppler};


%--------------------------------------------------------------------------
% Named model and stream manager version
%--------------------------------------------------------------------------
Create B3Datafile MauiDataStream
MauiDataStream.Filename = MauiData.B3

Create MeasurementModel MySatMauiRange
MySatMauiRange.Type   = Range;
MySatMauiRange.Anchor = MyGS.Sensor1;      % This is the end node where the 
                                           % measurement data was collected
MySatMauiRange.Participants = { MySat };   % Anything else measured
MySatMauiRange.Bias         = 0.1;
MySatMauiRange.NoiseSigma   = 0.01;
MySatMauiRange.TimeConstant = 7200;

Create MeasurementModel MySatMauiDoppler
MySatMauiDoppler.Type         = AveragedDoppler;
MySatMauiDoppler.Anchor       = MyGS;
MySatMauiDoppler.Participants = { MySat };
MySatMauiDoppler.Bias         = 0.075;
MySatMauiDoppler.NoiseSigma   = 0.02;
MySatMauiDoppler.TimeConstant = 7200;

% Stream specifiers can be part of either the models or, when models are
% collected together, the resulting groups.  Here is the group version:
Create MeasurementGroup MauiData;
MauiData.DataStream  = MauiDataStream;
MauiData.AddModel    = {MySatMauiRange, MySatMauiDoppler};
