classdef rrunpn < measerrs.biases.pnoises.rwalkpn
    % Class for process noise for random run biases.
    methods
        function rr = rrunpn(Q)
            rr = rr@measerrs.biases.pnoises.rwalkpn(Q);
        end % function
        function Qd = processNoiseCovariance(rr,dt)
            Qd(2,2) = ...
                processNoiseCovariance@measerrs.biases.pnoises.rwalkpn(rr,dt);
            Qd(1,1:2) = [dt^3/3 dt^2/2];
            Qd(1:2,1) = Qd(1,1:2)';
            rr.processNoiseIntegral = Qd;
            rr.integrationTime = dt;
        end % function
    end % methods
end % classdef