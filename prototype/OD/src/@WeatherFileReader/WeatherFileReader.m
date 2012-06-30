classdef WeatherFileReader < handle
    
    properties (SetAccess = 'protected')
        weatherData
    end
    
    properties (SetAccess = 'private')
        numRows
    end
    
    methods
       
        %% Initialize the file reader
        function this = Initialize(this)
            LoadWeatherData(this);
        end
        
        %% Compute Ap and F10.7 values
        function [apVector,F107,F107A] = GetApAndF107Data(this,jdUTC)
            [apVector]   = GetApValues(this,jdUTC);
            [F107,F107A] = GetF107Values(this,jdUTC);
        end
        
        %%  Compute Ap values
        function [apVector] = GetApValues(this,jdUTC)
                     
            %  Determine year, month, day and compute row and col idx
            [year,month,day,hour] = JD2GD(jdUTC);
            rowIdx = this.GetRowIndex(year, month, day);
            colIdx = fix(hour/3) + 15 ;
            
            %  Assemble Ap values required for MISISE model
%          This vector apVector 
%          consists of daily magnetic index (AP), 3 hour AP for current
%          time, 3 hour AP for 3 hours before current time, 3 hour AP for 6
%          hours before current time, 3 hour AP for 9 hours before current
%          time, average of eight 3 hour AP indices from 12 to 33 hours
%          prior to current time, and average of eight 3 hour AP indices
%          from 36 to 57 hours prior to current time.
            ap3HourValues = zeros(20,1);
            %  Get the average ap value for the day.
            apVector(1,1)   = this.weatherData(rowIdx,23);
            for ctr = 1:20 
                if colIdx == 14;
                    colIdx = 22;
                    rowIdx = rowIdx -1;
                end
                ap3HourValues(ctr,1) = this.weatherData(rowIdx,colIdx);
                colIdx = colIdx - 1;
            end
            %  Get 3 hour averages for 0 3 6 and 9 hours intervals.
            apVector(2:5,1) = ap3HourValues(1:4,1);
            %  Get long term averages.
            apVector(6,1)   = mean(ap3HourValues(5:12,1));
            apVector(7,1)   = mean(ap3HourValues(13:20,1));
            
        end
        
        %%  Get Observed F107 values
        function [F107,F107A] = GetF107Values(this,jdUTC)                    
            
            [year,month,day] = JD2GD(jdUTC);
            rowIdx = this.GetRowIndex(year, month, day);
            %  Get Observed F107 from the previous day
            F107   = this.weatherData(rowIdx-1,31);
            %  Get Observed F107 81 day average centered on current day
            F107A  = this.weatherData(rowIdx,32);
            
        end
        
        %%  Get row of weather file associated with current epoch
        function [rowIdx] = GetRowIndex(this, year, month, day)
              
            %  TODO:  Make search more efficient
            for rowIdx = 1:this.numRows
                if year == this.weatherData(rowIdx,1) &&...
                        month == this.weatherData(rowIdx,2)...
                        && day == this.weatherData(rowIdx,3);
                    return
                end
            end
            disp('Error Reading Space Weather File:  Epoch not on file.');
            rowIdx = [];
            
        end
        
        %%  Load the weather data file
        function LoadWeatherData(this)
                 WeatherDataFile;
                 this.weatherData = theWeatherData;
                 this.numRows = size(this.weatherData,1);             
        end
        
    end
end