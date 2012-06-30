classdef pnoise < handle & hgsetget
    % Base class for process noise subpackage of bias subpackage.
    properties (SetAccess = protected, GetAccess = protected)
        processNoiseIntegral;
        integrationTime;
    end % properties (SetAccess = protected, GetAccess = protected)
    methods(Abstract)
        Qd = processNoiseCovariance(obj,dt)
    end % methods(Abstract)
    methods
        function pn = pnoise(~)
            pn.processNoiseIntegral = 0;
            pn.integrationTime = 0;
        end % function
        function wd = processNoiseRealization(pn,dt)
            if dt == pn.integrationTime
                Qd = pn.processNoiseIntegral;
            else
                Qd = processNoiseCovariance(pn,dt);
            end
            wd = measerrs.covsmpl(Qd);
        end % function
    end % methods
end % classdef