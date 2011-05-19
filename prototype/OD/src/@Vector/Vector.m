classdef Vector < handle

    %----------------------------------------------------------------------
    %  Define the object properties
    %----------------------------------------------------------------------
    
    %-----  Set the public data
    properties  (SetAccess = 'protected')
        ReferenceSystem = 'EarthMJ2000Eq'     
        PrimaryBody     = ''
        SecondaryBody   = ''
        graphicsOrigin  = ''
        mu              = 398600.4415;
        scaleMethod     = 'ScaleFactor'
        scaleFactor     = 1;
        lengthValue     = 1;
        vectorComponents
    end

    %----------------------------------------------------------------------
    %  Define the object's methods
    %----------------------------------------------------------------------

    methods 

        %-----  The constructor
        function this = Vector(~)
        end 
        
        %----- Set the primary body
        function setPrimaryBody(this,primaryBody)
           this.PrimaryBody = primaryBody;
        end       
        
        %----- Set the secondary body
        function setSecondaryBody(this,secondaryBody)
           this.SecondaryBody = secondaryBody;
        end  
        
        %----- Set the scaleFactor
        function setScaleFactor(this,scaleFactor)
           this.scaleFactor = scaleFactor;
        end   
        
        %----- Set the scaleMethod
        function setScaleMethod(this,scaleMethod)
           this.scaleMethod = scaleMethod;
        end         
        
        %----- Set the lengthValue
        function setLengthValue(this,lengthValue)
           this.lengthValue = lengthValue;
        end        
        
        %----- Set Graphics Origin
        function setGraphicsOrigin(this,graphicsOrigin)
            this.graphicsOrigin = graphicsOrigin;
        end
        
        %----- Scale the vector
        function scaleVector(this)
            
            if strcmp(this.scaleMethod, 'None');
                return
            end
            
            if strcmp(this.scaleMethod,'ScaleFactor')
                this.vectorComponents = this.vectorComponents*this.scaleFactor;
            end
            
            if strcmp(this.scaleMethod,'FixedLength')
                this.vectorComponents = this.vectorComponents/norm(this.vectorComponents)...
                    * this.lengthValue;
            end
            
        end
                   
    end % method

end % classdef
