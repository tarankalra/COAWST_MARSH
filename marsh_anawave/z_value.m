%function [z_new]=z_value2(his_file)
function [z_new]=z_value(Vtransform,Vstretching,theta_s,theta_b,....
			 hc,N,igrid,h,zeta,ntime)
 
% zeta=ncread(his_file,'zeta'); 
% h=ncread(his_file,'h') ; 
% ocean_time_his=ncread(his_file,'ocean_time');
% total_time=size(ocean_time_his);  
% 
% ntime= total_time(1); 
% Vtransform=1; Vstretching =1; theta_s=1.0; theta_b=1.0;
% hc=0.0; N=200; igrid=5 ;
% 
 
 report=0; 

for t=1:ntime 
  z_new(:,:,:,t)=set_depth(Vtransform, Vstretching, ...
                       theta_s, theta_b, hc, N, ...
                       igrid, h, squeeze(zeta(:,:,t)), report);
end


 
 
