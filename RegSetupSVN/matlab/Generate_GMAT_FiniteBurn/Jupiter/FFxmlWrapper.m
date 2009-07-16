function FFxmlWrapper(filetoopen,FFfile)

Cell2Write = {
    '<?xml version="1.0" encoding="utf-8" standalone="yes"?>';
'<FreeFlyerProjectFile xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">';
'  <ProjectInformation>';
'    <Author></Author>';
'    <Source></Source>';
'    <FreeFlyerVersion>6.5.1.8215</FreeFlyerVersion>';
'    <RevisionNumber></RevisionNumber>';
'    <DateCreated>17:37 05/26/2009</DateCreated>';
'    <LastModified>17:48 05/26/2009</LastModified>';
'    <Title></Title>';
'    <Subject></Subject>';
'    <Category></Category>';
'    <Keywords></Keywords>';
'    <Comments></Comments>';
'    <Notes></Notes>';
'  </ProjectInformation>';
'  <ProjectPreferences>';
'    <General>';
'      <LockProjectFromEditing>false</LockProjectFromEditing>';
'    </General>';
'    <OnOpen>';
'      <ShowControlSequence>true</ShowControlSequence>';
'      <ShowEmptyEnvironment>false</ShowEmptyEnvironment>';
'      <ShowNotes>false</ShowNotes>';
'      <CreateBackup>false</CreateBackup>';
'      <BackupLocation></BackupLocation>';
'    </OnOpen>';
'    <OnRun>';
'      <SaveMissionPlan>false</SaveMissionPlan>';
'    </OnRun>';
'  </ProjectPreferences>';
'  <ProjectDataFiles>';
'    <EarthOrientationFile useDefault="true"></EarthOrientationFile>';
'    <ElectronDensityCoefficientFile useDefault="true"></ElectronDensityCoefficientFile>';
'    <GeopotentialFile useDefault="true"></GeopotentialFile>';
'    <HarrisPriesterDragFile useDefault="true"></HarrisPriesterDragFile>';
'    <JacchiaAtmosphereModelFile useDefault="true"></JacchiaAtmosphereModelFile>';
'    <LeapSecondFile useDefault="true"></LeapSecondFile>';
'    <MagneticFieldFile useDefault="true"></MagneticFieldFile>';
'    <PlanetaryFile useDefault="true"></PlanetaryFile>';
'    <StationGeodeticsFile useDefault="true"></StationGeodeticsFile>';
'  </ProjectDataFiles>';
'  <ProjectObjects />';
'  <ProjectMissionSequence>';
'    <ProjectExternals />';
'    <ProjectCommands>';
'      <FreeForm enabled="true">';
'        <Label>FreeForm</Label>';
['        <Line>Include ''',filetoopen,''';</Line>'];
'      </FreeForm>';
'    </ProjectCommands>';
'  </ProjectMissionSequence>';
'  <ProjectScript>';
['    <Line>Include ''',filetoopen,''';</Line>'];
'  </ProjectScript>';
'  <ProjectNotes></ProjectNotes>';
'</FreeFlyerProjectFile>'};


fid = fopen(FFfile,'w');

    cellfun(@(X) fprintf(fid,...
        [regexprep(X,{'%','\\'},{'%%','\\\\'}),'\n']),Cell2Write);

fclose(fid);

end