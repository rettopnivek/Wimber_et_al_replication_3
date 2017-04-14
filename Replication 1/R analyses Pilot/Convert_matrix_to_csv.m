
% Convert matrix to .csv file
output = { 'Cue,numImage,numComp,Base,CatT,CatC'; 'Data' }; % Create a cell string array
% Row 1: Index for the word cue
% Row 2: Image number for targets
% Row 3: Image number for competitors
% Row 4: Conditions, where Target/Competitor = 1, Baseline  = 2
% Row 5: Category for targets, where Faces = 1, Objects = 2, Scenes = 3
% Row 6: Category for competitors, where Faces = 1, Objects = 2, Scenes = 3

for c = 1:36
    tmp = [];
for r = 1:6
    if r < 6
        tmp = [ tmp, num2str(pairings(r,c)), ',' ];
    else
        tmp = [ tmp, num2str(pairings(r,c)) ];
    end;
end;
output{c+1} = tmp;
end;

% Write to a file
fileID = fopen('S1_stimulus_order.csv','w');
for ln = 1:length(output)
    fprintf( fileID, [ char( output{ln} ), char(10) ] );
end;
fclose(fileID);
