% Script to produce the text files used by Presentation
% Generates stimulus pairings such that each F/P/O equally often serves as
% target & competitor

function create_balance(whichRP,NRP)
% clear all

% ---- change according to randomization ----
%studySet = 'X';         % which version of the similar lures will be studied
%studySet = 'Y';        % which version of the similar lures will be studied

switch whichRP
    case 'A'
        setAB_RP = 1:18; setAC_RP = 19:36; 
        setAB_C = 1:6;   setAC_C = 7:12;  
    case 'B'
        setAB_RP = 19:36; setAC_RP = 1:18;
        setAB_C = 7:12;   setAC_C = 1:6;  
end           
% ---------------

rootDir = 'I:/fMRI/RIF_FOP';
types = {'faces' 'places' 'objects'};
typeIDs = {'F' 'P' 'O'};
ST = {'FP' 'FO' 'PF' 'PO' 'OF' 'OP'};
pairST = {'faces' 'places'; 'faces' 'objects'; 'places' 'faces'; 'places' 'objects'; 'objects' 'faces'; 'objects' 'places'};
posRP = {(1:9) (1:9); (10:18) (1:9); (1:9) (1:9); (10:18) (10:18); (1:9) (10:18); (10:18) (10:18)};
posC = {(1:3) (1:3); (4:6) (1:3); (1:3) (1:3); (4:6) (4:6); (1:3) (4:6); (4:6) (4:6)};
RPtag = {'RP' 'C'};
setTag = {'AB' 'AC'};

fid1 = fopen('picsAB_AC_X.txt','w');
fprintf(fid1, 'picID\t fileAB\t fileAC\t ;\n');
fid2 = fopen('picsAB_AC_Y.txt','w');
fprintf(fid2, 'picID\t fileAB\t fileAC\t ;\n');
fid3 = fopen('labels.txt','w');
fid4 = fopen('wordsRand.txt','w');
fid5 = fopen('labelStems.txt','w');
fid6 = fopen('picsAll_X.txt','w');
fprintf(fid6, 'picID\t file\t ;\n');
fid7 = fopen('picsAll_Y.txt','w');
fprintf(fid7, 'picID\t file\t ;\n');
fid8 = fopen('labelsAll.txt','w');

% Generate random sequence of words
words = textread('wordsConcrete.txt','%s');
ord = randperm(length(words));
words = words(ord);
for i = 1:length(words)
    fprintf(fid4, [words{i,1} '\n']); 
end
fclose(fid4);

% Open text file containing first version of each stimulus
targetPics = fullfile(rootDir, 'Programme', 'picsX.xls');

[RP_ind,allPics] = xlsread(targetPics);
RP_ind = RP_ind(:,NRP);

for i = 1:size(allPics,1)
    fprintf(fid6, ['"' allPics{i,2} 'A"' '\t' strcat('"', allPics{i,1}, '"') '\t' ';' '\n']);
    repName = strrep(allPics{i,1},'1.bmp','2.bmp');
    fprintf(fid7, ['"' allPics{i,2} 'B"' '\t' strcat('"', repName, '"') '\t' ';' '\n']);
    findCap = regexp(allPics{i,1}, '[A-Z]');
    findDot = regexp(allPics{i,1}, '.bmp');
    findDot = findDot-2;

    if size(findCap,2) == 4
        label = [allPics{i,1}(findCap(1):findCap(2)-1) ' ' allPics{i,1}(findCap(2):findCap(3)-1) ' ' allPics{i,1}(findCap(3):findCap(4)-1) ' ' allPics{i,1}(findCap(4):findDot)];
    elseif size(findCap,2) == 3
        label = [allPics{i,1}(findCap(1):findCap(2)-1) ' ' allPics{i,1}(findCap(2):findCap(3)-1) ' ' allPics{i,1}(findCap(3):findDot)];           
    elseif size(findCap,2) == 2
        label = [allPics{i,1}(findCap(1):findCap(2)-1) ' ' allPics{i,1}(findCap(2):findDot)];     
    else
        label = allPics{i,1}(findCap(1):findDot);
    end
    fprintf(fid8, [label '\n']);
end
fclose(fid6);
fclose(fid7);
fclose(fid8);

% Random assignment of one of the 2 versions as target/lure
ordXY = [repmat(1,1,size(allPics,1)) repmat(2,1,size(allPics,1)) ];
randXY = randperm(length(ordXY));
ordXY = ordXY(randXY);

for i = 1:size(allPics,1)
    allPics{i,1} = strrep(allPics{i,1}, '1', mat2str(ordXY(1,i)));
end
% Find different types
RP = find(RP_ind == 1);
C = find(RP_ind == 0);

% divide items into (shuffled) sets AB and AC, and RP and C items
for t = 1:length(types)
    eval(sprintf('%s_RP = allPics(intersect(strmatch(''%s'',allPics(:,2)), RP),:);', types{t}, typeIDs{t}));
    eval(sprintf('%s_C = allPics(intersect(strmatch(''%s'',allPics(:,2)), C),:);', types{t}, typeIDs{t}));
    for j = 1:length(RPtag)
        for s = 1:length(setTag)
            eval(sprintf('%s_%s_%s = %s_%s(set%s_%s,:);', types{t}, RPtag{j}, setTag{s}, types{t}, RPtag{j}, setTag{s}, RPtag{j}));
            eval(sprintf('ord = randperm(size(%s_%s_%s,1))'';', types{t}, RPtag{j}, setTag{s}));
            eval(sprintf('%s_%s_%s = %s_%s_%s(ord,:);', types{t}, RPtag{j}, setTag{s}, types{t}, RPtag{j}, setTag{s}));
        end
    end
end

% Concatenate into list of pairs
allConcat = cell(size(allPics,1)/2,5);
put = 1;
for p = 1:size(pairST,1)
    for j = 1:length(RPtag)
        eval(sprintf('actPairs = [%s_%s_AB(pos%s{p,1}'',:) %s_%s_AC(pos%s{p,2}'',:)];', pairST{p,1}, RPtag{j},RPtag{j},pairST{p,2},RPtag{j},RPtag{j}));
        
        % Check whether first letters are different in each pairing
        ckV = repmat(0,size(actPairs,1),1);
        ck = 0;
        while ck == 0
            for f = 1:size(actPairs,1)
                letterAB = actPairs{f,1}(1,1);
                letterAC = actPairs{f,3}(1,1);
                if strmatch(letterAB, letterAC,'exact') == 1
                    ckV(f) = 1;
                else
                    ckV(f) = 0;
                end
            end
            if ~isempty(find(ckV,1))
               ord1 = randperm(size(actPairs,1));
               actPairs(:,1:2) = actPairs(ord1,1:2);
               ck = 0;
            else
                ck = 1;
            end
        end
        
        % Concatenate
        allConcat(put:(put+size(actPairs,1)-1),1:4) = actPairs;
        allConcat(put:(put+size(actPairs,1)-1),5) = RPtag(j);
        put = put + size(actPairs,1);
    end
end

% Reorder before writing into text file
reOrd = [(1:3) 10 (4:6) 11 (7:9) 12]';
origOrd = (1:12)';
finalPairsX = cell(size(allPics,1)/2,5);
for st = 1:length(ST)
    finalPairsX(origOrd,:) = allConcat(reOrd,:);
    origOrd = origOrd + 12;
    reOrd = reOrd + 12;
end

% select counterparts of the X pairs for Y 
finalPairsY_1 = strrep(finalPairsX, '1.bmp','3.bmp');
finalPairsY_2 = strrep(finalPairsY_1, '2.bmp','4.bmp');
finalPairsY_3 = strrep(finalPairsY_2, '4.bmp','1.bmp');
finalPairsY = strrep(finalPairsY_3, '3.bmp','2.bmp');
clear finalPairsY_*

for i = 1:size(finalPairsX,1)
    fprintf(fid1, ['"' finalPairsX{i,2} '_' finalPairsX{i,4} '_' finalPairsX{i,5} '"' '\t']);
    fprintf(fid1, [strcat('"', finalPairsX{i,1}, '"') '\t']);
    fprintf(fid1, [strcat('"', finalPairsX{i,3}, '"') '\t' ';' '\n']);
    fprintf(fid2, ['"' finalPairsY{i,2} '_' finalPairsY{i,4} '_' finalPairsY{i,5} '"' '\t']);
    fprintf(fid2, [strcat('"', finalPairsY{i,1}, '"') '\t']);
    fprintf(fid2, [strcat('"', finalPairsY{i,3}, '"') '\t' ';' '\n']);
end

j = 1;
for p = 1:2
    for i = 1:size(finalPairsX,1)
            findCap = regexp(finalPairsX{i,j}, '[A-Z]');
            findDot = regexp(finalPairsX{i,j}, '.bmp');
            findDot = findDot-2;

            if size(findCap,2) == 4
                label = [finalPairsX{i,j}(findCap(1):findCap(2)-1) ' ' finalPairsX{i,j}(findCap(2):findCap(3)-1) ' ' finalPairsX{i,j}(findCap(3):findCap(4)-1) ' ' finalPairsX{i,j}(findCap(4):findDot)];
            elseif size(findCap,2) == 3
                label = [finalPairsX{i,j}(findCap(1):findCap(2)-1) ' ' finalPairsX{i,j}(findCap(2):findCap(3)-1) ' ' finalPairsX{i,j}(findCap(3):findDot)];           
            elseif size(findCap,2) == 2
                label = [finalPairsX{i,j}(findCap(1):findCap(2)-1) ' ' finalPairsX{i,j}(findCap(2):findDot)];     
            else
                label = finalPairsX{i,j}(findCap(1):findDot);
            end
            fprintf(fid3, [label '\n']);
            fprintf(fid5, [label(1:1) '\n']);
    end
    j = j + 2;
end
    
status = fclose('all')

