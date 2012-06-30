classdef vibstm < measerrs.biases.stms.stm
    % Class for biases with vibratory STM's.
    properties
        naturalFrequency;
        dampingRatio;
    end % properties
    methods
        function vs = vibstm(w_n,zeta)
            vs.naturalFrequency = w_n;
            vs.dampingRatio = zeta;
        end % function
        function Phi = stateTransitionMatrix(vs,dt)
            z = vs.dampingRatio;
            wn = vs.naturalFrequency;
            if z == 1
                Phi = exp(-wn*dt)*[(1+wn*dt) dt; -wn^2*dt (1-wn*dt)];
            else
                wd = wn*sqrt(1-z^2);
                if z < 1
                    cwt = cos(wd*dt);
                    swt = sin(wd*dt);
                else
                    cwt = cosh(wd*dt);
                    swt = sinh(wd*dt);
                end
                Phi = exp(-z*wn*dt)/wd*[(wd*cwt + z*wn*swt) swt; ...
                    -wn^2*swt (wd*cwt - z*wn*swt)];
            end
        end % function
    end % methods
end % classdef