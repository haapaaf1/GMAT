function varargout = GMATFile(filename,varargin);

fid = fopen(filename,'w');

for i = 1:nargin -1
    cellfun(@(X) fprintf(fid,...
        [regexprep(X,{'%','\\'},{'%%','\\\\'}),'\n']),varargin{i});
end

varargout{1} = fclose(fid);

end