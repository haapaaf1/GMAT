classdef MeasurementManager < handle
    
    %  The MeasurementManager manages the construction of the measurements,
    %  their partials, and the estimation STM, given the configured
    %  measurement objects define by the user.  The Measurement Manager
    %  supports multiple measurement objects for a single solver, and
    %  multiple data types for each measurement object.
    %
    %  - Maintain information on which measurements are functions of which
    %    solver for and consider
    %  - Construct total Htilde and STM matrices from submatrices
    %    associated with different measurements and states
    %
    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    
    %----- Set the public properties
    properties  (SetAccess = 'public')
        MeasurementHandles = {};
    end
    
    %----- Set the public properties
    properties  (SetAccess = 'private')
        measHandles  = {}; % handles for measurement objects attached by user.
        Epochs       = []; % v  
        Obs          = [];
        Htilde       = [];
        numMeas      = 0   % number of measurement objects attached by user
        numObs       = 0;  % number of total observations (vector obs counted as 1)
        numDataTypes = []; 
        numStates    = 0;
        paramIdsVector;
        lengthObjects = 0;
        chunkSizeVector;
    end
    
    %----------------------------------------------------------------------
    %  Define the methods
    %----------------------------------------------------------------------
    methods
        
        %----- The constructor
        function MeasManager = MeasurementManager(MeasManager)
            
        end
        
        %----- The initialize method
        function MeasManager = Initialize(MeasManager,Sandbox,Estimator)
            
            %==============================================================
            %  -- Get pointers to all measurements
            %  -- Get participants handles for each measurement data type
            %  -- Initialize each measurement. (...read file etc)
            %  -- Sort all measurements according to epoch
            %  -- Assemble array of data types for each measurement
            %  -- Determine number of data types for each meas. object
            %  -- Initialize the partial derivative maps
            %==============================================================
            
            %----- Extract data just to make code shorter
            ESM                 = Estimator.ESM;
            Meas                = Estimator.Measurements;
            MeasManager.numMeas = size(Estimator.Measurements,2);
            MeasManager.numStates = Estimator.ESM.numStates;
            MeasManager.paramIdsVector = ESM.paramIdsVector;
            MeasManager.lengthObjects = size(Estimator.ESM.ObjectsVector,2);
            MeasManager.chunkSizeVector = ESM.chunkSizeVector;
            hcount = 0;
            for i = 1:MeasManager.numMeas
                
                %  Initialize the ith measurement
                MeasManager.measHandles{i}  = Sandbox.GetHandle(Estimator.Measurements{i});
                MeasManager.numDataTypes(i) = size(MeasManager.measHandles{i}.AddDataType,2);
                MeasManager.measHandles{i}.Initialize(Sandbox);
                MeasManager.measHandles{i}.SetThisPointer;
                MeasManager.measHandles{i}.SetObjectDependencies(ESM.ObjectsVector);
                
                %----- Add meas times, handles, and other data to total
                %      data arrays
                currMeas = MeasManager.measHandles{i};
                numMeas = size(currMeas.Epochs,1);
                MeasManager.Epochs  = [MeasManager.Epochs; currMeas.Epochs];
                MeasManager.Obs     = [MeasManager.Obs;    currMeas.Obs];
                for k = 1:numMeas
                    hcount = hcount + 1;
                    MeasManager.MeasurementHandles{hcount} =  currMeas;
                end
                
            end
            
            %----- Sort the measurements
            [MeasManager.Epochs, sortIndeces] = sort(MeasManager.Epochs);
            MeasManager.Obs                   = MeasManager.Obs(sortIndeces,:);
            MeasManager.MeasurementHandles    = MeasManager.MeasurementHandles(sortIndeces);
            MeasManager.numObs                = size(MeasManager.Obs,1);
                                 
        end
        
                %----- The initialize method
        function MeasManager = InitializeforSim(MeasManager,Sandbox,Simulator)
            
            %==============================================================
            %  -- Get pointers to all measurements
            %  -- Get participants handles for each measurement data type
            %  -- Initialize each measurement for simulation 
            %  -- Assemble array of data types for each measurement
            %  -- Determine number of data types for each meas. object
            %==============================================================
            
            %----- Extract data just to make code shorter
            Meas                = Simulator.Measurements;
            MeasManager.numMeas = size(Simulator.Measurements,2);
            hcount = 0;
            for i = 1:MeasManager.numMeas
                
                %  Initialize the ith measurement
                MeasManager.measHandles{i}  = Sandbox.GetHandle(Simulator.Measurements{i});
                MeasManager.numDataTypes(i) = size(MeasManager.measHandles{i}.AddDataType,2);
                MeasManager.measHandles{i}.InitializeforSimulation(Sandbox);
                               
            end
            
            %----- Sort the measurements
            [MeasManager.Epochs, sortIndeces] = sort(MeasManager.Epochs);
            MeasManager.Obs                   = MeasManager.Obs(sortIndeces);
            MeasManager.numObs                = size(MeasManager.Obs,1);
                                 
        end
              
        function [y, Htilde, isFeasible, Htildec, W, Phi] = GetMeasurement(measManager,index)
            
            % -- Call the measurement model
            Htilde = zeros(measManager.numStates);
            isFeasible = 1;
            [y] = measManager.MeasurementHandles{index}.Evaluate;
            sizeY = size(y,1);
            Htilde = zeros(sizeY,measManager.numStates);
            runningTotal = 1;
            for i = 1:measManager.lengthObjects
                participantId = measManager.MeasurementHandles{index}.ObjectDependencies(i);
                stateId       = measManager.paramIdsVector{i};
                chunkSize     = measManager.chunkSizeVector{i};
                if participantId
                    dydx = measManager.MeasurementHandles{index}.GetPartial(participantId,stateId);
                    Htilde(1:sizeY,runningTotal:runningTotal+chunkSize-1) = dydx; 
                end
                runningTotal = runningTotal + chunkSize;
            end
            
        end

    end

end