classdef VectorAndPrincipalPlane < CoordinateSystem
    
    

    %%  Define the object properties
    properties  (SetAccess = 'protected')
         PrimaryVector
         SecondaryVector
    end

    %%  Define the object methods
    methods
          
        function setPrimaryVector(this,Vector)          
            this.PrimaryVector = Vector;           
        end
        
        function setSecondaryVector(this,Vector)           
            this.PrimaryVector = Vector;            
        end
        
        function R = ToMJ2000(this)
            x = 1;
            this.PrimaryVector.computeVector()
        end
    end % method
    
end % classdef