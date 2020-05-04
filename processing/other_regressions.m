clear all; clc;

mazes = {'I', 'L', 'Z', 'U'};
for m = mazes
    
    % load aggregated (across runs and mazes) behavior 
    data_path = '/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/';
    fname = 'behavior';
    behavior = load([data_path fname '.mat']);
    behavior = behavior.data;
    behavior.Participant = str2double(behavior.Participant);
    load([data_path 'regression_' m{1} '.mat']); % res
    
    for p = {'Mean_Ratings', 'SOD', 'Gender', 'RFP'} %, 'IPQ_Presence', 'PTSOT', 'vgame'}
        % build design matrix
        predictor_names = {'participant'; p{1}};
        design = table(behavior.Participant, behavior.(p{1}), 'VariableNames', predictor_names);
        % specify model
        model = ['map_point ~ ' p{1}]; 
        % select map measure and run regression of model
        map_measure = squeeze(res.head_loc.all_maps_maze(:,1,:,:)); % head location metric sampled every second        
        res.regression.(p{1}) = regress_map_mobi3d(map_measure, design, 1, model);
    end
    
    % save res (grand mean) and regression_maps (effect)
    save([data_path 'regression_' m{1}], 'res');
    clear res;
end
