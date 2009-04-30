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
                
%             load(GSMeas.Filename);
%             %----- KLUDGE WHILE REWORKING THIS COMPONENT
%             GSMeas.Measurements.Obs    = MeasData{1}.Obs;
%             GSMeas.Measurements.Epochs = MeasData{1}.Epochs;
            
            %----- Loop over all data types to get handles for participants
            %      and to add data for each type to the allData structure.
%             for i = 1:MeasManager.measHandles{i}.numDataTypes
%                
%                 %----- Concatenate Types, Epochs, Obs
%                 numcurrObs            = size(MeasData{i}.Obs,1);
%                 low                   = totalnumObs;
%                 high                  = totalnumObs+numcurrObs-1;
%                 DataTypes(low:high,1) = ones(numcurrObs,1)*MeasData{i}.DataType;
%                 Epochs(low:high,1)    = MeasData{i}.Epochs;
%                 Obs(low:high,:)       = MeasData{i}.Obs;
%                 typeIndex(low:high,1) = ones(numcurrObs,1)*i;
%                 
%             end
%                 
                
                %  Loop over participants for the current measurement and
                %  add state Ids to its partials map.               
%                 MeasManager.partialsMap{i} = Map();
%                 numParticipants    = size(currMeas.Participants{i},2);
%                 for j = 1:size(Estimator.ESM.ParamIds,2)
%                     %  loop over data types
%                     numStates = size(Estimator.ESM.ParamIds{j},1);
%                     for k = 1:numStates
%                         stateSize = Estimator.ESM.subStateSizes{j}(k);
%                         paramId   = Estimator.ESM.ParamIds{j}(k);
%                         MeasManager.partialsMap{i}.Add(zeros(stateSize,1),paramId);
%                     end
%                 end

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
            [y, Htilde] = measManager.MeasurementHandles{index}.Evaluate();
            
        end

    end

end