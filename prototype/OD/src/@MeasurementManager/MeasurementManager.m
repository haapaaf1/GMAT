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
        measHandles  = {};
        Epochs       = [];
        Obs          = [];
        dataTypeIndeces = [];
        Htilde       = [];
        Htildemap    = [];
        numMeas      = 0;
        numObs       = 0;
        numDataTypes = [];
        numStates    = 0;
        partialsMap  = {};
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
            hcount = 0;
            for i = 1:MeasManager.numMeas
                
                %  Initialize the ith measurement
                MeasManager.measHandles{i}  = Sandbox.GetHandle(Estimator.Measurements{i});
                MeasManager.numDataTypes(i) = size(MeasManager.measHandles{i}.AddDataType,2);
                MeasManager.measHandles{i}.Initialize(Sandbox);
                
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
                
                MeasManager.dataTypeIndeces = [MeasManager.dataTypeIndeces currMeas.dataTypeIndeces];
                
                %  Loop over participants for the current measurement and
                %  add state Ids to its partials map.               
                MeasManager.partialsMap{i} = Map();
                numParticipants    = size(currMeas.Participants{i},2);
                for j = 1:size(Estimator.ESM.ParamIds,2)
                    %  loop over data types
                    numStates = size(Estimator.ESM.ParamIds{j},1);
                    for k = 1:numStates
                        stateSize = Estimator.ESM.subStateSizes{j}(k);
                        paramId   = Estimator.ESM.ParamIds{j}(k);
                        MeasManager.partialsMap{i}.Add(zeros(stateSize,1),paramId);
                    end
                end

            end
            
            %----- Sort the measurements
            [MeasManager.Epochs, sortIndeces] = sort(MeasManager.Epochs);
            MeasManager.Obs                   = MeasManager.Obs(sortIndeces);
            MeasManager.MeasurementHandles    = MeasManager.MeasurementHandles(sortIndeces);
            MeasManager.dataTypeIndeces       = MeasManager.dataTypeIndeces(sortIndeces);
            MeasManager.numObs                = size(MeasManager.Obs,1);
                                 
        end
              
        function [y, Htilde, isFeasible, Htildec, W, Phi] = GetMeasurement(measManager,index)
            
            % -- Call the measurement model
            dataType   = measManager.dataTypeIndeces(index);
            Htilde = zeros(measManager.numStates);
            [y, Htilde, isFeasible] = measManager.MeasurementHandles{index}.Evaluate(dataType,measManager.MeasurementHandles{index}.Participants{dataType});
            
        end

    end

end