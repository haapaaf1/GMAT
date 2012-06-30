classdef SegmentManager < handle
  
    properties  (SetAccess = 'protected')        
        isInitialTimeVariable   
        isFinalTimeVariable 
        isMassVariable
        numStates
        numControls
        numStaticParameters
        numQuadraturePoints
        numCollocationPoints
    end
    
    methods
        
        function this = SegmentManager(~)
            this.isInitialTimeVariable = 0;
            this.isFinalTimeVariable = 0;
            this.isMassVariable = 0;
            this.numStates = 6;
            this.numControls = 0;
            this.numStaticParameters = 0;
            this.numQuadraturePoints = 12;
            this.numCollocationPoints = this.numQuadraturePoints - 1;
        end
        
        function SetnumQuadraturePoints(this,N)
            
            [x,w,P]=rpmLgrnodes(N)
            
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
        
        %%  Extract state matrix "X" from optimization variable vector
        function stateMatrix = GetStateMatrix(this,optVariables) 
           
            numStateParameters = this.numQuadraturePoints*this.numStates;
            stateMatrix = resize(optVariables(1:numStateParameters,1),...
                          this.numStates,this.numQuadraturePoints)';
            
        end
    end
    
end