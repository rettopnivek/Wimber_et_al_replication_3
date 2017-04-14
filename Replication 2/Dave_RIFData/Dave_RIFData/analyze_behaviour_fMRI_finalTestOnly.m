% Script to analyse final forced-choice test only

clear all
subs        = 1:24;  
rootDir     = '/imaging/mw02/fMRI/RIF_FOP/Programme/fMRI/';
cd(rootDir)

itemTypes   = {'RP_minus' 'C_minus' 'RP_plus' 'C_plus'};
picTypes    = {'Face','Place','Object'};
rpTypes     = {'target' 'comp' 'error' 'dk' 'miss'};

output      = zeros(length(subs), 32);
output2     = zeros(length(subs), 12, 'double');

for s = subs
    
    % ----- Final Discrimination Test -----
    eval(sprintf('file = ''./Sub%d/VP%d_fMRI-PictureDiscrimination_fMRI.log'';', s, s));
    [type, code, times] = textread(file, '%*s %*s %s %s %d %*s %*s %*s %*s %*s %*s %*s %*s', 'headerlines', 5, 'whitespace', '\t');
    
    % find items & RTs
    item        = find(cellfun('isempty', regexp(code, '\w*_', 'match')) == 0);
    RP          = find(cellfun('isempty', regexp(code, '\w*_RP', 'match')) == 0);
    C           = find(cellfun('isempty', regexp(code, '\w*_C', 'match')) == 0);

    RT_table    = cell(size(item,1),7);
    
    for i = 1:size(item,1)
        line    = item(i);
        toFind  = code{line}(1,1:end-2);
        sideTag = code{line}(1,end-1:end);
        
        RT_table{i,7} = sideTag;
                
        RT_table{i,1} = toFind;
        RT_table{i,2} = times(line);     % onset of item
        ck = strmatch('Response', type(line+1));
        if ck == 1
            RT_table(i,3) = code(line+1);
            RT_table{i,4} = times(line+1);
            RT_table{i,5} = RT_table{i,4} - RT_table{i,2};
        elseif strmatch('Response', type(line+2)) == 1
            RT_table(i,3) = code(line+2);
            RT_table{i,4} = times(line+2);
            RT_table{i,5} = RT_table{i,4} - RT_table{i,2};
        elseif strmatch('3', code(line+1)) == 1
            RT_table(i,3) = code(line+1);
            RT_table{i,4} = times(line+1);
            RT_table{i,5} = RT_table{i,4} - RT_table{i,2};
        elseif strmatch('3', code(line+2)) == 1
            RT_table(i,3) = code(line+2);
            RT_table{i,4} = times(line+2);
            RT_table{i,5} = RT_table{i,4} - RT_table{i,2};
        else
            RT_table{i,3} = ''; %NaN;
            RT_table{i,4} = []; %NaN;
            RT_table{i,5} = []; %NaN
        end
        
    end

    % ----- Trim RTs -----
    RT_mean = mean(cell2mat(RT_table(:,5)));
    RT_sd = std(cell2mat(RT_table(:,5)));
    cutoff(1) = RT_mean - (2*RT_sd); 
    cutoff(2) = RT_mean + (2*RT_sd); 

    for i = 1:size(RT_table,1)
        if isempty(RT_table{i,5})
            RT_table{i,6} = [];
        elseif RT_table{i,5} < cutoff(1) || RT_table{i,5} > cutoff(2)
            RT_table{i,6} = [];
        else
            RT_table{i,6} = RT_table{i,5};
        end
    end

    % ----- Go through RT table & calculate RTs for different item types -----

    leftAll     = strmatch('_L', RT_table(:,7));
    rightAll    = strmatch('_R', RT_table(:,7));
    leftResp    = union(strmatch('11',RT_table(:,3),'exact'), strmatch('111',RT_table(:,3),'exact'));
    rightResp   = union(strmatch('22',RT_table(:,3),'exact'), strmatch('222',RT_table(:,3),'exact'));
    corrLeft    = intersect(leftAll, leftResp);
    corrRight   = intersect(rightAll, rightResp);
    corrAll     = union(corrLeft, corrRight);
    
    RP_minus    = find(cellfun('isempty', regexp(RT_table(1:72,1), '\w*_RP', 'match')) == 0); % AC associations
    C_minus     = find(cellfun('isempty', regexp(RT_table(1:72,1), '\w*_C', 'match')) == 0); % AC associations
    RP_plus     = find(cellfun('isempty', regexp(RT_table(73:end,1), '\w*_RP', 'match')) == 0) + 72; % AB associations
    C_plus      = find(cellfun('isempty', regexp(RT_table(73:end,1), '\w*_C', 'match')) == 0) + 72; % AB associations
    
    % During final test, AC tested before AB, so in first half first code
    % is target, in second half second code is target on screen
    Face_T_AB   = find(cellfun('isempty', regexp(RT_table(73:end,1), 'F\w*_\w*_', 'match')) == 0) + 72;
    Face_T_AC   = find(cellfun('isempty', regexp(RT_table(1:72,1), '\w*_F', 'match')) == 0);    
    Face_T      = union(Face_T_AB, Face_T_AC); 
    Place_T_AB  = find(cellfun('isempty', regexp(RT_table(73:end,1), 'P\w*_\w*_', 'match')) == 0) + 72; 
    Place_T_AC  = find(cellfun('isempty', regexp(RT_table(1:72,1), '\w*_P', 'match')) == 0);    
    Place_T     = union(Place_T_AB, Place_T_AC); 
    Object_T_AB = find(cellfun('isempty', regexp(RT_table(73:end,1), 'O\w*_\w*_', 'match')) == 0) + 72; 
    Object_T_AC = find(cellfun('isempty', regexp(RT_table(1:72,1), '\w*_O', 'match')) == 0);    
    Object_T    = union(Object_T_AB, Object_T_AC);

    for it = 1:length(itemTypes)
        eval(sprintf('%s_corr = intersect(%s,corrAll);', itemTypes{it}, itemTypes{it}));
        eval(sprintf('RTs(s,it) = mean(cell2mat(RT_table(%s,5)))/10;', itemTypes{it}));
        eval(sprintf('RTs(s,it+4) = mean(cell2mat(RT_table(%s,6)))/10;', itemTypes{it}));
        eval(sprintf('RTs(s,it+8) = mean(cell2mat(RT_table(%s_corr,6)))/10;', itemTypes{it}));
        eval(sprintf('correct(s,it) = length(%s_corr)/length(%s);', itemTypes{it}, itemTypes{it}));
    end
    
    
    % Percent correct split according to picture type
    cs = 0;
    for p = 1:length(picTypes)
        eval(sprintf('target = %s_T_AC;', picTypes{p}));
        output2(s,1+cs) = (length(intersect(RP_minus_corr,target)))/(length(target)*3/4); % how many face_T, RP_minus trials?
        output2(s,2+cs) = (length(intersect(C_minus_corr,target)))/(length(target)*1/4);
        eval(sprintf('target = %s_T_AB;', picTypes{p}));
        output2(s,3+cs) = (length(intersect(RP_plus_corr,target)))/(length(target)*3/4);
        output2(s,4+cs) = (length(intersect(C_plus_corr,target)))/(length(target)*1/4);
        cs = cs + 4;
    end   
    
end
output = horzcat(correct,RTs);
    
