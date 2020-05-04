%% plot grand average mean duration per maze and p-masked effect of presence

% select maze and run
mazes = {'I', 'L', 'Z', 'U'};
measures = {'head_loc', 'touch', 'hand_jerk'};
data_path = '/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/';
model = 'map_point ~ IPQ_Presence';

c = 1; % counter

% p-mask settings
alpha = .05;
mcc = 0;
filter = 10; % number of minimum size of significant pixel cluster

% select measure
% measure = measures{1};
effects = [0, 1];

% plot settings
margins = [.13 .06]; % tight subplot margins

% marker of start points with >
add_head_marker = 0;
start_points = [18,35; 39,15; 33,49; 48,20] - 4;
sz = 80;

% image size
bg_image_size_x = 62;
bg_image_size_y = 122;

for measure = measures
    measure = measure{1};
    
    for effect = effects
    
        % new figure with subplots
        f=figure('Renderer', 'painters', 'Position', [10 10 1000 500]);
        c=1;
        
        % get overall plot limits
        count = 1;
        for m = mazes
            m = m{1};
            
            fname = [data_path model '_' m];
            load(fname);
            load(['/Users/lukasgehrke/Documents/MATLAB/toolboxes/mobi-3D-tools/' m '_mask']); % maze mask

            if effect % plot effect
                m_ef = out.effects.(measure);
                plot_map = squeeze(m_ef.estimate(1,:,:));
                plot_lim = max(abs(plot_map(:))) * [-1 1];
            else % plot means
                m_mean = out.means.(measure);
                plot_lim = squeeze(mean(m_mean.all_lims_maze(:,1,:),1))';
            end
            
            lims(count,:) = plot_lim;
            count = count + 1;
        end
        
        if effect
            plot_lim = (max(abs(lims(:)))/3) * [-1 1];
        else
            plot_lim = (max(abs(lims(:)))/2) * [0 1];
        end
        clear lims % delete limits for next maze
        
        % now plot
        for m = mazes
            
            m = m{1};
            
            fname = [data_path model '_' m];
            load(fname);
            load(['/Users/lukasgehrke/Documents/MATLAB/toolboxes/mobi-3D-tools/' m '_mask']); % maze mask

            if effect % plot effect
                m_ef = out.effects.(measure);
                % compute pmask
                p_mask = make_pmask(squeeze(out.effects.(measure).p(2,:,:)), alpha, mcc, filter, mask);
                plot_map = squeeze(m_ef.estimate(1,:,:));
            else % plot means
                m_mean = out.means.(measure);
                % mean acoss subjects
                plot_map = squeeze(mean(m_mean.all_maps_maze(:,1,:,:),1)); % first dim here is head location, 2nd is velocity
            end

            % make plot
            subplot_tight(2, size(mazes,2)/2, c, margins);

            % fit plot_map to center 
            bg_image = zeros(bg_image_size_x, bg_image_size_y);
            start_x = ((size(bg_image,1) - size(plot_map,1))/2);
            start_y = ((size(bg_image,2) - size(plot_map,2))/2);
            
            % smooth gaussian and remove outside maze pixel
            if effect
                % maze mask
                maze_mask = plot_map(:);
                maze_mask(maze_mask~=0) = 1;
                plot_map = imgaussfilt(plot_map,1);
                plot_map_long = plot_map(:);
                plot_map_long(logical(~maze_mask)) = 0;
                plot_map = reshape(plot_map_long, size(plot_map,1), size(plot_map,2));
            end


            % plot map
            bg_image(start_x:(start_x-1+size(plot_map,1)), ...
                start_y:(start_y-1+size(plot_map,2))) = plot_map;
            
            bg_image(bg_image==0) = mean(plot_lim); % remove color of outside maze
            
            imagesc(bg_image, plot_lim);
            hold on;

            % plot maze boundaries
            bg_image(start_x:(start_x-1+size(plot_map,1)), ...
                start_y:(start_y-1+size(plot_map,2))) = mask;
            contour(bg_image, 1, 'linecolor', 'black', 'LineWidth', 1); % maze mask
            if effect
                % plot p_mask
                bg_image(start_x:(start_x-1+size(plot_map,1)), ...
                start_y:(start_y-1+size(plot_map,2))) = p_mask;
                contour(bg_image, 1, 'linecolor', 'black', 'LineWidth', .5); % p mask
            end

            % add start point marker
            if add_head_marker
        %     [img, map, alphachannel] = imread('/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/figures/eeg_head.png');
        %     scale = .05;
        %     rot_deg = -90;
        %     img = imresize(img, scale);
        %     alphachannel = imresize(alphachannel, scale);
        %     img = imrotate(img, rot_deg);
        %     alphachannel = imrotate(alphachannel, rot_deg);
        %     image(start_points(c,1)-10, start_points(c,2)-8, img, 'AlphaData', alphachannel);    
                scatter(start_points(c,1), start_points(c,2), sz,...
                    '>',...
                    'MarkerEdgeColor',[0 .5 .5],...
                    'MarkerFaceColor',[0 .7 .7],...
                    'LineWidth',1.5)
            end

            % ticklabels
            xl = str2double(string(xticklabels))/10;
            xticklabels(xl(1:end-1))
            yl = str2double(string(yticklabels))/10;
            yticklabels(yl(1:end-1))
            ax = gca;
            ax.FontSize = 28; 

            % format plot
            title(m, 'FontSize', 24, 'FontWeight', 'normal', 'Margin', 3);

            if strcmp(m, 'U')
                xlabel('meter', 'FontSize', 28);
                ylabel('meter', 'FontSize', 28);
                cbarax = cbar;

                if effect
                    ylabel('effect presence', 'FontSize', 28);
                else
                    %cbarax.YLim = [0 cbarax.YLim(2)]; % optional, better leave diverging cbar -> comment out line
                    
                    if strcmp(measure, 'head_loc')
                        l = 'time (a.u.)';
                    elseif strcmp(measure, 'touch')
                        l = 'wall touches (a.u.)';
                    elseif strcmp(measure, 'hand_jerk')
                        l = 'hand jerk (a.u.)';
                    end
                    ylabel(l, 'FontSize', 28);
                end
                ax = gca;
                ax.FontSize = 28; 

            end
            c=c+1;
        end

        f = tightfig(f);
        
        % save plot
        fname = [data_path(1:end-9), 'figures/' measure];
        if effect
            fname = [fname '_effect'];
        end
        saveas(f, [fname '.eps'], 'epsc')
        close(f)        
    end
end

%% plot vgame

% select maze and run
mazes = {'I', 'L', 'Z', 'U'};
measures = {'head_loc', 'touch', 'hand_jerk'};
data_path = '/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/';
predictor = 'PTSOT';
model = ['map_point ~ ' predictor];

c = 1; % counter

% p-mask settings
alpha = .05;
mcc = 0;
filter = 10; % number of minimum size of significant pixel cluster

% select measure
effect = 1;

% plot settings
margins = [.13 .06]; % tight subplot margins

% marker of start points with >
add_head_marker = 0;
start_points = [18,35; 39,15; 33,49; 48,20] - 4;
sz = 80;

% image size
bg_image_size_x = 62;
bg_image_size_y = 122;

for measure = measures
    measure = measure{1};

    % new figure with subplots
    f=figure('Renderer', 'painters', 'Position', [10 10 1000 500]);
    c=1;

    % get overall plot limits
    count = 1;
    for m = mazes
        m = m{1};

        fname = [data_path model '_' m];
        load(fname);
        load(['/Users/lukasgehrke/Documents/MATLAB/toolboxes/mobi-3D-tools/' m '_mask']); % maze mask

        if effect % plot effect
            m_ef = out.regression_maps.(measure);
            plot_map = squeeze(m_ef.estimate(1,:,:));
            plot_lim = max(abs(plot_map(:))) * [-1 1];
        else % plot means
            %m_mean = out.means.(measure);
            %plot_lim = squeeze(mean(m_mean.all_lims_maze(:,1,:),1))';
        end

        lims(count,:) = plot_lim;
        count = count + 1;
    end

    if effect
        plot_lim = (max(abs(lims(:)))/3) * [-1 1];
    else
        %plot_lim = (max(abs(lims(:)))/2) * [0 1];
    end
    clear lims % delete limits for next maze

    % now plot
    for m = mazes

        m = m{1};

        fname = [data_path model '_' m];
        load(fname);
        load(['/Users/lukasgehrke/Documents/MATLAB/toolboxes/mobi-3D-tools/' m '_mask']); % maze mask

        if effect % plot effect
            m_ef = out.regression_maps.(measure);
            % compute pmask
            p_mask = make_pmask(squeeze(out.regression_maps.(measure).p(2,:,:)), alpha, mcc, filter, mask);
            plot_map = squeeze(m_ef.estimate(1,:,:));
        else % plot means
            %m_mean = out.means.(measure);
            % mean acoss subjects
            %plot_map = squeeze(mean(m_mean.all_maps_maze(:,1,:,:),1)); % first dim here is head location, 2nd is velocity
        end

        % make plot
        subplot_tight(2, size(mazes,2)/2, c, margins);

        % fit plot_map to center 
        bg_image = zeros(bg_image_size_x, bg_image_size_y);
        start_x = ((size(bg_image,1) - size(plot_map,1))/2);
        start_y = ((size(bg_image,2) - size(plot_map,2))/2);

        % smooth gaussian and remove outside maze pixel
        if effect
            % maze mask
            maze_mask = plot_map(:);
            maze_mask(maze_mask~=0) = 1;
            plot_map = imgaussfilt(plot_map,1);
            plot_map_long = plot_map(:);
            plot_map_long(logical(~maze_mask)) = 0;
            plot_map = reshape(plot_map_long, size(plot_map,1), size(plot_map,2));
        end


        % plot map
        bg_image(start_x:(start_x-1+size(plot_map,1)), ...
            start_y:(start_y-1+size(plot_map,2))) = plot_map;

        bg_image(bg_image==0) = mean(plot_lim); % remove color of outside maze

        imagesc(bg_image, plot_lim);
        hold on;

        % plot maze boundaries
        bg_image(start_x:(start_x-1+size(plot_map,1)), ...
            start_y:(start_y-1+size(plot_map,2))) = mask;
        contour(bg_image, 1, 'linecolor', 'black', 'LineWidth', 1); % maze mask
        if effect
            % plot p_mask
            bg_image(start_x:(start_x-1+size(plot_map,1)), ...
            start_y:(start_y-1+size(plot_map,2))) = p_mask;
            contour(bg_image, 1, 'linecolor', 'black', 'LineWidth', .5); % p mask
        end

        % add start point marker
        if add_head_marker
    %     [img, map, alphachannel] = imread('/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/figures/eeg_head.png');
    %     scale = .05;
    %     rot_deg = -90;
    %     img = imresize(img, scale);
    %     alphachannel = imresize(alphachannel, scale);
    %     img = imrotate(img, rot_deg);
    %     alphachannel = imrotate(alphachannel, rot_deg);
    %     image(start_points(c,1)-10, start_points(c,2)-8, img, 'AlphaData', alphachannel);    
            scatter(start_points(c,1), start_points(c,2), sz,...
                '>',...
                'MarkerEdgeColor',[0 .5 .5],...
                'MarkerFaceColor',[0 .7 .7],...
                'LineWidth',1.5)
        end

        % ticklabels
        xl = str2double(string(xticklabels))/10;
        xticklabels(xl(1:end-1))
        yl = str2double(string(yticklabels))/10;
        yticklabels(yl(1:end-1))
        ax = gca;
        ax.FontSize = 28; 

        % format plot
        title(m, 'FontSize', 24, 'FontWeight', 'normal', 'Margin', 3);

        if strcmp(m, 'U')
            xlabel('meter', 'FontSize', 28);
            ylabel('meter', 'FontSize', 28);
            cbarax = cbar;

            if effect
                ylabel('effect presence', 'FontSize', 28);
            else
                %cbarax.YLim = [0 cbarax.YLim(2)]; % optional, better leave diverging cbar -> comment out line

                if strcmp(measure, 'head_loc')
                    l = 'time (a.u.)';
                elseif strcmp(measure, 'touch')
                    l = 'wall touches (a.u.)';
                elseif strcmp(measure, 'hand_jerk')
                    l = 'hand jerk (a.u.)';
                end
                ylabel(l, 'FontSize', 28);
            end
            ax = gca;
            ax.FontSize = 28; 

        end
        c=c+1;
    end

    f = tightfig(f);

    % save plot
    fname = [data_path(1:end-9), 'figures/' measure];
    if effect
        fname = [fname '_effect_' predictor];
    end
    saveas(f, [fname '.eps'], 'epsc')
    close(f)        
end

%% plot tests

% figure;
% for i = 1:29
%     
%     subplot(10,3,i);
%     imagesc(squeeze(out.means.touch.all_maps_maze(i,1,:,:)));cbar;
% end

% grand mean

for i = {'I', 'L', 'Z', 'U'}
    figure;
    sgtitle(i{1})
    load(['/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/map_point ~ vgame_' ...
        i{1} '.mat']);
    subplot(1,3,1)
    imagesc(squeezemean(out.means.touch.all_maps_maze(:,1,:,:),1));title('touch');cbar;
    subplot(1,3,2)
    imagesc(squeezemean(out.means.head_loc.all_maps_maze(:,1,:,:),1));title('headloc');cbar;
    subplot(1,3,3)
    imagesc(squeezemean(out.means.hand_jerk.all_maps_maze(:,1,:,:),1));title('hand_jerk');cbar;
end

%% betas plot test

for i = {'L'} %{'I', 'L', 'Z', 'U'}
    figure;
    sgtitle(i{1})
    load(['/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/map_point ~ vgame_' ...
        i{1} '.mat']);
    subplot(1,3,1)
    imagesc(squeeze(out.effects.touch.estimate(1,:,:)),[-.01 .01]);title('touch');cbar;
    subplot(1,3,2)
    imagesc(squeeze(out.effects.head_loc.estimate(1,:,:)),[-.01 .01]);title('headloc');cbar;
    subplot(1,3,3)
    imagesc(squeeze(out.effects.hand_jerk.estimate(1,:,:)),[-.01 .01]);title('hand_jerk');cbar;
end

%% betas plot test 2

for pred = {'SOD', 'Gender', 'RFP', 'IPQ_Presence', 'PTSOT', 'vgame'}
    figure;
    c = 1;
    sgtitle(pred{1});
    
    for maze = {'I', 'L', 'Z', 'U'}
        load(['/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/regression_' maze{1} '.mat']);
        betas = squeeze(res.regression.(pred{1}).estimate(2,:,:));
        lims = .05 * [-1, 1]; %max(abs(betas(:)))/2 * [-1 1];
        subplot(1,4,c);
        c = c+1;
        imagesc(betas,lims);title(pred{1});cbar;
    end
end

%% UIST methods: single subject and grand average maps

% get data
maze = 'U';
measure = 'IPQ_Presence';
load(['/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/regression_' maze '.mat']);
%betas = squeeze(res.head_loc.all_maps_maze(14,1,:,:)); % single subject
betas = squeezemean(res.head_loc.all_maps_maze(:,1,:,:),1); % grand mean
%limsbetas = betas;
%limsbetas(limsbetas==0)=NaN;
lims = [.05 .2]; % min(limsbetas(:))*5 max(betas(:))/1.5


% smooth
load(['/Users/lukasgehrke/Documents/MATLAB/toolboxes/mobi-3D-tools/' maze '_mask.mat']);
%betas = imgaussfilt(betas,1.5);
plot_map_long = betas(:);
plot_map_long(logical(~mask)) = 0;
betas = reshape(plot_map_long, size(betas,1), size(betas,2));

figure('Renderer', 'painters', 'Position', [10 10 1000 950]); % i:200, l:650, z:575, u: 950
imagesc(betas,lims);%title(measure);

% set size so all mazes are scaled equally
xlim([0,180]);
ylim([-90,90]);

% add masking line around maze
hold on
contour(mask, 1, 'linecolor', 'black', 'LineWidth', 4);

% format plot
xl = str2double(string(xticklabels))/10;
xticklabels(xl(1:end-1))
yl = str2double(string(yticklabels))/10;
yticklabels(yl(1:end-1))
%xlabel('meter');
%ylabel('meter')
ax = gca;
ax.FontSize = 36; 
set(gca,'linewidth',2);

% add colorbar
cbar;
%ylabel('betas presence');
ax = gca;
ax.FontSize = 36;
set(gca,'linewidth',2);

%% UIST figure 2

% get data
maze = 'I';
measure = 'IPQ_Presence';
load(['/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/regression_' maze '.mat']);
betas = squeeze(res.regression.(measure).estimate(2,:,:));
lims = .03 * [-1, 1]; %max(abs(betas(:)))/2 * [-1 1];

% smooth
maze_mask = betas(:);
maze_mask(maze_mask~=0) = 1;
betas = imgaussfilt(betas,1);
plot_map_long = betas(:);
plot_map_long(logical(~maze_mask)) = 0;
betas = reshape(plot_map_long, size(betas,1), size(betas,2));

figure('Renderer', 'painters', 'Position', [10 10 1000 950]); % i:200, l:, z:575, u: 950
imagesc(betas,lims);title(measure);

% set size so all mazes are scaled equally
xlim([0,180]);
ylim([-90,90]);

% add line around maze
hold on
load(['/Users/lukasgehrke/Documents/MATLAB/toolboxes/mobi-3D-tools/' maze '_mask.mat']);
contour(mask, 1, 'linecolor', 'black', 'LineWidth', 4);

% add sig mask
alpha = .05;
p = squeeze(res.regression.(measure).p(2,:,:));
p(p>alpha) = NaN;
p(p<alpha) = 1;
p(isnan(p)) = 0;
p = bwareaopen(p,2); % remove small significant patches
contour(p, 1, 'linecolor', 'black', 'LineWidth', 4, 'LineStyle', '-.'); % maze mask

% format plot
% ticklabels
xl = str2double(string(xticklabels))/10;
xticklabels(xl(1:end-1))
yl = str2double(string(yticklabels))/10;
yticklabels(yl(1:end-1))
%xlabel('meter');
%ylabel('meter')
ax = gca;
ax.FontSize = 36; 
set(gca,'linewidth',2);

% add colorbar
cbar;
%ylabel('betas presence');
ax = gca;
ax.FontSize = 36;
set(gca,'linewidth',2);

%% UIST r^2 map

% get data
maze = 'Z';
measure = 'IPQ_Presence';
load(['/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/regression_' maze '.mat']);
betas = res.regression.(measure).adjusted_r_squared;
lims = max(abs(betas(:)))/2 * [0 1];

% smooth
maze_mask = betas(:);
maze_mask(maze_mask~=0) = 1;
betas = imgaussfilt(betas,1);
plot_map_long = betas(:);
plot_map_long(logical(~maze_mask)) = 0;
betas = reshape(plot_map_long, size(betas,1), size(betas,2));

figure('Renderer', 'painters', 'Position', [10 10 1000 575]); % i:200, l:, z:575, u: 950
imagesc(betas,lims);title(measure);

% add line around maze
hold on
load(['/Users/lukasgehrke/Documents/MATLAB/toolboxes/mobi-3D-tools/' maze '_mask.mat']);
contour(mask, 1, 'linecolor', 'black', 'LineWidth', 4);

% format plot
% ticklabels
xl = str2double(string(xticklabels))/10;
xticklabels(xl(1:end-1))
yl = str2double(string(yticklabels))/10;
yticklabels(yl(1:end-1))
%xlabel('meter');
%ylabel('meter')
ax = gca;
ax.FontSize = 36; 
set(gca,'linewidth',2);

% add colorbar
cbar;
%ylabel('betas presence');
ax = gca;
ax.FontSize = 36;
set(gca,'linewidth',2);

%% UIST extract stats

% get data
maze = 'U';
measure = 'IPQ_Presence';
load(['/Users/lukasgehrke/Documents/bpn_work/publications/2019-IEEE-VR-Spatial-exploration-behavior-in-large-scale-VR-predicts-subjective-spatial-presence/analyses/regression_' maze '.mat']);

% plot p to select significant pixel
alpha = .05;
p = squeeze(res.regression.(measure).p(2,:,:));
p(p>alpha) = NaN;
figure('Renderer', 'painters', 'Position', [1000 1000 1000 300]);
imagesc(p);cbar;

%% select pixel
% I: [5,40], [3, 18], [7, 83]
% L: [13,61]
% Z: [18,48], [30,38]
% U: [31, 35], [41, 37]

x = 41;
y = 37;

%% print stats at pixel
b = res.regression.(measure).estimate(2,x,y);
t = res.regression.(measure).t(2,x,y);
p = res.regression.(measure).p(2,x,y);
r2 = res.regression.(measure).adjusted_r_squared(x,y);

disp(['result at [' num2str(x) ', ' num2str(y) '] with beta: ' num2str(b) ' has t: ' num2str(t) ', p = ' num2str(p) ' and r^2 = ' num2str(r2)]);










