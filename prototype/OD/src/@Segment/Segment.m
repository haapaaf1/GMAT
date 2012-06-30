classdef Segment < handle
    
    %% Protected properties
    properties  (SetAccess = 'protected')
        initialTime 
        finalTime 
        isInitialTimeVariable   
        isFinalTimeVariable 
        isMassVariable
        isCoastArc 
        numStates
        numControls
        numStaticParameters
        numQuadraturePoints
        numCollocationPoints
        decisionVector
        numDecisionVariables;
    end
    
    %% Private properties
    properties  (SetAccess = 'private')
        quadraturePoints
        quadratureWeights
        timeVector
        interpolator;
    end
    
    methods
        
        %% Constructor, initialization, etc.
        function this = Segment(~)
            this.interpolator = LagrangeInterpolator;
        end
        
        %  Initialize the quadrature points, weights etc...
        function Initialize(this)
            [quadPoints,quadWeights]=rpmLgrnodes(this.numCollocationPoints-1);
            this.quadraturePoints =  quadPoints;
            this.quadratureWeights = quadWeights;
            this.timeVector    = [this.quadraturePoints; 1]*...
                (this.finalTime - this.initialTime)/2 ...
                + (this.finalTime + this.initialTime)/2;
            this.interpolator.setIndependentVariables(this.quadraturePoints + 1);
        end
        
        %% Functions for setting simple properties
        function SetInitialTime(obj,Time)
            obj.initialTime = Time;
        end
        
        function SetFinalTime(obj,Time)
            obj.finalTime = Time;
        end
        
        function SetisInitialTimeVariable(this,flag)
            this.isInitialTimeVariable = flag;        
        end
        
        function SetisFinalTimeVariable(this,flag)
            this.isFinalTimeVariable = flag;        
        end
        
        function SetisMassVariable(this,flag)
            this.isMassVariable = flag;        
        end
        
        function SetNumCollocationPoints(this,N)
            this.numCollocationPoints = N;
            this.numQuadraturePoints = N + 1;
        end
        
        function SetisCoastArc(this,flag)
           this.isCoastArc = flag; 
        end
        
        %%  Extract state matrix "X" from optimization variable vector
        function stateMatrix = GetStateMatrix(this,optVariables)            
            numStateParameters = this.numQuadraturePoints*this.numStates;
            stateMatrix = reshape(optVariables(1:numStateParameters,1),...
                          this.numStates,this.numQuadraturePoints)';            
        end
        
        %%  Construct the "state" portion of the decision vector
        function decVector = SetdecVector_State(this,stateMatrix)          
            [rows,cols] = size(stateMatrix);
            this.numStates = cols;
            if rows ~= this.numQuadraturePoints
                disp(['Error in Segment.SetdecVectorState: the number ' ...
                    'of rows in the stateMatrix must be the same as' ...
                    'the number of quadrature points']);
                return
            end
            decVector(1:rows*cols,1) = reshape(stateMatrix',rows*cols,1); 

        end
        
        %  Get the state assocatiated with ith Point
        function state = GetState(obj,vec,I)
            start = (I - 1)*obj.numStates + 1;
            state = vec(start:start+obj.numStates,1);
        end
        
        
        %%  Get the dynamics constraints
        function [c,J] = GetDynamicsConstraints(obj,vec)
        
            numConstraints = obj.numStates*obj.numCollocationPoints
            c = zeros(numConstraints,1);
            % TODO:  Fix how number of decision variables is computed
            numDecisionVariables = numConstraints;
            J = zeros(numConstraints,numDecisionVariables);
            stateMatrix = obj.GetStateMatrix(vec);
            diffMatrix = obj.interpolator.GetDiffMatrix();
            for pointIdx = 1:obj.numCollocationPoints
                currentState =  obj.GetState(vec,pointIdx);
                currentTime  =  obj.timeVector(pointIdx);
                sum = zeros(1,obj.numStates)
                for newIdx = 1:obj.numCollocationPoints
                    sum = sum + diffMatrix(pointIdx,:)*obj.GetState(vec,newIdx);
                end
                [Xdot,dXdotdX,dXdotdu,dXdotdt] = psDynamics(currentTime,currentState,[]);
                
            end
            
        end
        
    end
    
end
