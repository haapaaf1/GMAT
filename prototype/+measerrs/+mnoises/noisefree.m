classdef noisefree < measerrs.mnoises.mnoise
    % Class that handles the perfect measurement case.
    methods
        function nf = noisefree(~)
            nf = nf@measerrs.mnoises.mnoise(0);
        end % function
    end % methods
end % classdef