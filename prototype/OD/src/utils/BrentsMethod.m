function [root] = BrentsMethod(func,a,b,tol,maxiter)

sa = a; sb = b;
fa = feval(func,sa);
fb = feval(func,sb);
c  = sa;              % line 10
fc = fa;
e = sb - sa;
d = e;
if abs(fc) <= abs(fb) % line 20
    sa = sb;
    sb = c;
    c = sa;
    fa = fb;
    fb = fc;
    fc = fa;
end
ftol = 2*eps*abs(sb) + tol; % line 30
m    = 0.5*(c - sb);

while abs(m) >= ftol && fb ~= 0.0
    xxx = 1
    if abs(e) <= ftol && abs(fa) < abs(fb)
        e = m;
        d = e;
    else
        s = fb/fa;  % line 40
        if sa == c
            p = 2.0*m*s;
            q = 1.0 - s;
        else
            q = fa/fc;  % line 50
            r = fb/fc;
            p = s*(2.0*m*q*(q-r) - (sb - sa)*(r - 1.0));
            q = (q - 1.0)*(r - 1.0)*(s - 1.0);
        end
        if (p >= 0.0) %line 60
            q = -q;
        else
            p = -p;  % line 70
        end
        s = e;  % line 80
        e = d;
        if (2*p < 3.0*m*q-abs(ftol*q)) || ( p <= abs(0.5*s*q) )
            d = p/q;
        else
            e = m; % line 90
            d = e;
        end
    end

    sa = sb;  % line 100
    fa = fb;
    if abs(d) >= ftol
        sb = sb + d;
    else
        if m >= 0.0   % line 110
            sb = sb + tol;
        else
            sb = sb - ftol;  % line 120
        end
    end
    fb = feval(func,sb);  % line 130
    %
    if fb ~= 0.0 && fc ~= 0.0
        c  = sa;
        fc = fa;
        e = sb - sa;
        d = e;
        if abs(fc) >= abs(fb)
            sa = sb;
            sb = c;
            c = sa;
            fa = fb;
            fb = fc;
            fc = fa;
        end
    end
    ftol = 2*eps*abs(sb) + tol; % line 30
    m    = 0.5*(c - sb);
end

root = sb;



