%% general settings

clear all; clc;

% load study params
params;

% processing settings
settings.resolution_stepsize = .1;
settings.gauss_kernel_size = 1.5; %see setttings imgaussfilt
settings.streaming_pc_name = '';
settings.streamname = 'Rigid';

% select maze and run
mazes = ['I', 'L', 'Z', 'U'];
settings.runs = 1:3;

for m = mazes
    
    if strcmp(m, 'L')
        subjects(subjects==26) = []; % remove subject 26 for maze L as the subject is missing some data here
    end
    
    settings.maze = m;

    %% measure to analyze

    % example 1: histogram of head location which produced a map where the
    % participants spent most of their time in the maze
    % example 2: map of head velocity at x,y -> how fast were participants
    % moving where in the maze
    % both examples can be run together as both xy and vel are returned for
    % head rigid body with below settings
    settings_head_loc = settings;
    settings_head_loc.rb = 1; % head rb index in imt
    settings_head_loc.metric = 'vel'; % no metric as we want a normal histogram of x,y location
    settings_head_loc.rotation = 0; % no rotation data but translation
    settings_head_loc.cdata = 0; % do not overwrite frequency count with some other cdata
    settings_head_loc.resample = 250; % take a sample every second (250Hz sampling rate)
    settings_head_loc.do_zscore = 0; % do not zscore the frequency count

    % 3 wall touch location: where was the wall touched the most
    settings_touch = settings;
    settings_touch.rb = 2; % head rb index in imt
    settings_touch.metric = 'itd_head'; % inter touch distance head, but also returns xy of touch
    settings_touch.rotation = 0; % no rotation data but translation
    settings_touch.cdata = 1; % cdata only considers discrete events not continouos metrics
    settings_touch.resample = 0; % does not matter
    settings_touch.do_zscore = 0; % do not zscore

    % 4: jerk of hand at x,y -> how much were they thrusting
    settings_thrust = settings;
    settings_thrust.rb = 2; % hand rb index in imt
    settings_thrust.metric = 'jerk'; % Jerk mocap channel
    settings_thrust.rotation = 0;
    settings_thrust.cdata = 0;
    settings_thrust.resample = 250; % take a sample every second
    settings_thrust.do_zscore = 0;

    %% other example settings

    % % example 3: map of head rotation velocity at x,y -> how fast were participants
    % % rotating where in the maze
    % rb = 1; % head rb index in imt
    % metric = 'vel'; % velocity
    % rotation = 1; % no rotation data but translation
    % cdata = 0; % cdata only considers discrete events not continouos metrics
    % resample = 10; % take every 10 sample with 250Hz sampling rate
    % do_zscore = 1; % do not zscore the velocities

    %% compute

    disp('making maps... ')

    for subject = subjects

        % load mocap data
        mocap_file = [study_folder raw_EEGLAB_data_folder num2str(subject) '/' merged_mocap_filename];
        mocap = pop_loadset(mocap_file );

        % convert mocap head ori to degree
        mocap.data(4,:) = rad2deg(mocap.data(4,:));

        % load events
        events_path = [study_folder single_subject_analysis_folder single_subject_analysis_folder_ICA num2str(subject) '/events.mat'];
        load(events_path); % struct is called events

        % run analysis for specific settings and save to results struct

        % analysis head location/duration (1st dim), velocity (2nd dim)
        [~, average_map_maze, ~, average_lims_maze] = run_maze_analysis( settings_head_loc, events, mocap);
        res.head_loc.all_maps_maze(subject,:,:,:) = average_map_maze;
        res.head_loc.all_lims_maze(subject,:,:) = average_lims_maze;

        % analysis wall touch location
        [~, average_map_maze, ~, average_lims_maze] = run_maze_analysis( settings_touch, events, mocap);
        res.touch.all_maps_maze(subject,:,:,:) = average_map_maze;
        res.touch.all_lims_maze(subject,:,:) = average_lims_maze;

        % analysis hand jerk
        [~, average_map_maze, ~, average_lims_maze] = run_maze_analysis( settings_thrust, events, mocap);
        res.hand_jerk.all_maps_maze(subject,:,:,:) = average_map_maze;
        res.hand_jerk.all_lims_maze(subject,:,:) = average_lims_maze;

    end

    % remove missing participants
    fieldnames = {'head_loc', 'touch', 'hand_jerk'};
    for f = fieldnames
        res.(f{1}).all_maps_maze([2,24,27],:,:,:) = [];
        res.(f{1}).all_lims_maze([2,24,27],:,:) = [];
    end

    disp('done')

    %% Statistical parametric maps

    % load aggregated (across runs and mazes) behavior 
    data_path = '/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/';
    fname = 'behavior';
    behavior = load([data_path fname '.mat']);
    behavior = behavior.data;
    behavior.Participant = str2double(behavior.Participant);

    % build design matrix
    predictor_names = {'participant'; 'IPQ_Presence'};
    design = table(behavior.Participant, behavior.IPQ_Presence, 'VariableNames', predictor_names);

    % specify model in R terminology'
    model = 'map_point ~ IPQ_Presence';

    % select map measure and run regression of model
    % head location
    map_measure = squeeze(res.head_loc.all_maps_maze(:,1,:,:));
    regression_maps.head_loc = regress_map_mobi3d(map_measure, design, 1, model);
    % head velocity
    map_measure = squeeze(res.head_loc.all_maps_maze(:,2,:,:));
    regression_maps.head_vel = regress_map_mobi3d(map_measure, design, 1, model);
    % touch location
    map_measure = squeeze(res.touch.all_maps_maze(:,1,:,:));
    regression_maps.touch = regress_map_mobi3d(map_measure, design, 1, model);
    % hand jerk
    map_measure = squeeze(res.hand_jerk.all_maps_maze(:,1,:,:));
    regression_maps.hand_jerk = regress_map_mobi3d(map_measure, design, 1, model);

    % save res (grand mean) and regression_maps (effect)
    out = struct('means', res, 'effects', regression_maps);
    save([data_path model '_' m], 'out');
    clear out res regression_maps;

end