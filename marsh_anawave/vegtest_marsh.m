%=========================================================================
% AUTHOR:  Tarandeep S. Kalra Oct 18-2014  (tkalra@usgs.gov)
%
% USAGE:
% 
%
% DESCRIPTION:
%
% INPUT:  (all must have same dimensions)
%
% OUTPUT: 
%
% DISCLAIMER:
%  This software is provided "as is" without warranty of any kind.  
%
% REFERENCE: 
%=========================================================================
clear all ; close all ; clc ;

% Get the grid parameters from a solution of US East Coast
netcdf_load('veg_test_his.nc');
im=50; jm=21; 

save_dim=size(tke);
imax=save_dim(1); jmax=save_dim(2); kmax=save_dim(3);
ntime=save_dim(4);

igrid=5;
[z_new]=z_value(Vtransform,Vstretching,theta_s,theta_b,....
                         hc,kmax,igrid,h,zeta,ntime);

ntime_hours=ocean_time(end,1)/3600 ;

for t=1:ntime
%  for j=1:jmax 
% y_2d(j,k)=y_rho(im,j)/1000;
%   % z_2d(j,k)=z_new(im,j,1,t);
   hh_2d(t)=h(im,jm+1)+zeta(im,jm+1,t); 
   depth_1d(t)=1.3; 
  % end
end
figure(1) 
plot(1:ntime,hh_2d(1:ntime))
hold on
plot(1:ntime,depth_1d(1:ntime),'k--')

figure(2)
plot(Thrust_tonelli(im,jm,1:ntime),hh_2d(1:ntime))

% 
% for t=1:ntime
%     for k=1:kmax
%        z_1d(k)=z_new(im,jm,k,ntime);
%       Thrust_tonelli_2d=Thrust_tonelli(im,jm,ntime);
%     end
% end 
% plot(1:kmax,z_1d)
%zeta_1d(:)=zeta(im,jm+1,:);
%h_1d(:)=h(im,jm+1);
%hh=h_1d+zeta_1d;

%Tonelli(:)=Thrust_tonelli(im,jm,:);

%thrust_mask(:)=mask_thrust(im,jm+1,:);
%%Hwave_1d(:)=Hwave(im,jm+1,:);
%sand_mass_2d(:,:)=sandmass_01(im,:,:); 

%figure(1)
%set(gca,'fontWeight','bold','lineWidth',14)

%plot(ocean_time(:)/3600,hh_1d(jm,:)); 
%hold on 
%ylabel('zeta+h')
%xlabel('time in hours')
%legend('original marsh','adjacent cell')
%print('-dpdf',a'-r600','sandmass_01')
