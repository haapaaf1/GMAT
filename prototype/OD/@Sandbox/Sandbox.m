classdef Sandbox < handle

    % Set the public properties
    properties  (SetAccess = 'public')
        numObj        = 0;
        ObjectNames   = {};
        ObjectHandles = {};
        SolarSystem
    end

    % Set the methods
    methods

        %---- Add objects to sandbox
        function obj = AddObject(obj,obj2,name)
            obj.numObj = obj.numObj + 1;
            obj.ObjectNames{obj.numObj} = name;
            obj.ObjectHandles{obj.numObj} = obj2;
        end % AddObject

        %---- Initialize all objects in the sandbox
        function Initialize(Sandbox)

            for i = 1:Sandbox.numObj
                obj = Sandbox.ObjectHandles{i};
                obj = obj.Initialize;
            end
            
        end % Initialize

    end

end