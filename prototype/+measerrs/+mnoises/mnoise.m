classdef mnoise < handle & hgsetget
    % Base Class for Measurement Noises Subpackage.
    properties
        covariance;
        realization;
    end % properties
    methods
        function n = mnoise(R)
            n.covariance = R;
        end % function
        function v = get.realization(n)
            v = measerrs.covsmpl(n.covariance);
        end % function
    end % methods
end % classdef