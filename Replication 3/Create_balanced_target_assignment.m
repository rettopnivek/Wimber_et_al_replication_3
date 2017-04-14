%----------------------------------------------------%
% Create balanced assignments to Targets/Competitors %
%----------------------------------------------------%

rng shuffle; % Random seed for rng

% Maximum number of potential subjects
N = 96;
% Number images per category (i.e. Faces, Objects, Scenes)
nCat = 48;
% Split each category into two halves
halfCat = nCat/2;
% Total number of images
I = nCat*3;
% Number of targets/competitors
nAssoc = I/2;
% Number of baseline trials
baselineTrials = nAssoc/4;
% Index for each image
% The assignment scheme assumes that images are arranged as:
% Faces (1:48), Objects (1:48), Scenes (1:48)
Images = 1:I;
% Index indicating category that each image belongs to, where...
% 1 = Faces, 2 = Objects, and 3 = Scenes
Category = [ ones( 1, nCat ), ones( 1, nCat )*2, ones( 1, nCat )*3 ];

% For debugging purposes
all_Target_sel = zeros( I, N );

% Empty matrix to store all assignments across subjects
all_pairings = [ ];

% Loop over total number of potential subjects
for n = 1:N
    
    % Initialize index for target images
    Targets = zeros( 1, I/2 );
    
    if mod( n, 2 ) == 0
        % For even-numbered subjects, flip-flop the assignment of
        % targets and competitors. This ensures that as long the number of
        % subjects is even and sequential, the assignment to targets and
        % competitors will be balanced
        Targets = Images( Target_sel == 0 );
    else
        % Loop through each category, randomly assigning half of the images
        % to the target condition
        for i = 1:3
            tmp = Images( Category == i ); % Select images for one category
            tmp = tmp( randperm( nCat ) ); % Randomly shuffle images
            sel = ( 1 + halfCat*(i-1) ):( halfCat + halfCat*(i-1) );
            Targets( sel ) = tmp( 1:halfCat ); % Assign half to be targets
        end
    end
    
    % Assign remaining images to competitor condition
    Competitors = Images;
    Competitors( Targets ) = [];
    
    % Create index for flipping with even subjects to create a balanced
    % assignment
    Target_sel = zeros( 1, I );
    Target_sel( Targets ) = 1;
    
    % For debugging purposes
    all_Target_sel( :, n ) = Target_sel;
    
    % Clean up workspace
    clear( 'tmp', 'sel', 'i' );
    
    % The targets need to be matched to competitors in a different category
    % To balance it so that the 2 different categories are matched to the
    % remaining category equally, select either the first or last half of
    % each of the two opposing categories' competitors and randomly match
    % them to the remaining category's targets
    
    % Index to reorder competitors and match them to the targets
    % 1:24 (Faces), 25:48 (Objects), 49:72 (Scenes)
    Matching = zeros( 1, I/2 );
    ind = 1:(I/2);
    % Number of images in the first/last half of the competitor category
    quartCat = halfCat/2;
    % Randomly shuffle indices on whether to use 1st/2nd half
    FL = [ randperm( 2 ); randperm( 2 ) + 2; randperm( 2 ) + 4 ];
    % Which rows/columns to use from FL during the loop
    v = [ 2 1 3 1; ...
        1 1 3 2; ...
        2 2 1 2 ];
    % Loop through each target category
    for i = 1:3
        % Select 1st/2nd half of 1st opposing category
        fl_1 = FL(v(i,1),v(i,2));
        % Select 1st/2nd half of 2nd opposing category
        fl_2 = FL(v(i,3),v(i,4));
        % Indices to pull 1st/2nd halves
        sel_1 = ( 1 + quartCat*( fl_1 - 1 ) ):( quartCat + quartCat*( fl_1 - 1 ) );
        sel_2 = ( 1 + quartCat*( fl_2 - 1 ) ):( quartCat + quartCat*( fl_2 - 1 ) );
        tmp = [ ind( sel_1 ) ind(sel_2 ) ];
        % Index for final vector
        sel_3 = ( 1 + halfCat*(i - 1) ):( halfCat + halfCat*(i - 1) );
        % Randomize order of images
        Matching( sel_3 ) = tmp( randperm( halfCat ) );
    end
    
    % Clean up workspace
    clear( 'fl_1', 'fl_2', 'sel_1', 'sel_2', 'sel_3', 'tmp', 'ind', ...
        'FL' );
    
    % Select categories for targets
    targetCategory = Category( Targets );
    % Select categories for competitors
    compCategory = Category( Competitors );
    % Create an index for assignment to selective retrieval or the baseline
    % condition
    Baseline = [ ones( 1, nAssoc - baselineTrials ) ...
        ones( 1, baselineTrials )*2 ];
    % Create a subject index
    subject = ones( 1, nAssoc )*n;
    
    % Combine all the indices into a single matrix for easy referencing
    pairings = [ randperm( nAssoc ); Targets; Competitors( Matching ); ...
        Baseline( randperm( nAssoc ) ); targetCategory; ...
        compCategory( Matching ); subject ];
    % Row 1: Index for the word cue
    % Row 2: Image number for targets
    % Row 3: Image number for competitors
    % Row 4: Conditions, where Target/Competitor = 1, Baseline  = 2
    % Row 5: Categories for targets, where Faces = 1, Objects = 2, Scenes = 3
    % Row 6: Categories for competitors, where Faces = 1, Objects = 2, Scenes = 3
    % Row 7: Subject index
    
    % Store all assignments across subjects into single matrix
    all_pairings = [ all_pairings pairings ];
    
end

% Check that assignments are balanced for even numbers of subjects
check = sum( all_Target_sel(:, 1:N ), 2 );

% Save the results as a matrix to be passed into the experiment
save('Image_cue_assignment.mat','all_pairings');