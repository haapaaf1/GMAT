classdef rzoompn < measerrs.biases.pnoises.rrunpn
    % Class for process noise for random zoom biases.
    methods
        function rz = rzoompn(Q)
            rz = rz@measerrs.biases.pnoises.rrunpn(Q);
        end % function
        function Qd = processNoiseCovariance(rz,dt)
            Qd(2:3,2:3) = ...
                processNoiseCovariance@measerrs.biases.pnoises.rrunpn(rz,dt);
            Qd(1,1:3) = [dt^5/20 dt^4/8 dt^3/6];
            Qd(1:3,1) = Qd(1,1:3)';
            rz.processNoiseIntegral = Qd;
            rz.integrationTime = dt;
        end % function
    end % methods
end % classdef