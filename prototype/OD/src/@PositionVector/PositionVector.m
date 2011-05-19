classdef PositionVector < OrbitVector

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------

    %-----  Set the public data
    properties  (SetAccess = 'protected')
         
    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------
    
    methods
          
        %----- Compute the the numeric values of the vector component   
        function vector = computeVector(this)
           
            this.computeCartesianData();
            this.vectorComponents = this.positionVector;
            this.scaleVector();
            vector = this.vectorComponents;
            
        end
        
                function obj = Initialize(obj,Sandbox)
            
                end
        
    end % method
    
end % classdef
