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
            colIdx = fix(hour/3) + 15;
            
            %  Assemble Ap values required for MISISE model
            ap3HourValues = zeros(20,1);
            apVector(1,1)   = this.weatherData(rowIdx,23);
            for ctr = 1:20 
                if colIdx == 14;
                    colIdx = 22;
                    rowIdx = rowIdx -1;
                end
                ap3HourValues(ctr,1) = this.weatherData(rowIdx,colIdx);
                colIdx = colIdx - 1;
            end
            apVector(2:5,1) = ap3HourValues(1:4,1);
            apVector(6,1)   = mean(ap3HourValues(5:12,1));
            apVector(7,1)   = mean(ap3HourValues(13:20,1));
            
        end
        
        %%  Get Observed F107 values
        function [F107,F107A] = GetF107Values(this,jdUTC)                    
            
            [year,month,day] = JD2GD(jdUTC);
            rowIdx = this.GetRowIndex(year, month, day);
             %F107   = this.weatherData(rowIdx,31);
             %F107A  = this.weatherData(rowIdx,32);
             F107   = this.weatherData(rowIdx,27);
             F107A  = this.weatherData(rowIdx,29);
            
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