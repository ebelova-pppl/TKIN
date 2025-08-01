;note: requires colorbar.pro
; changes from original wdata.pro 09/99

;#####################################################################
PRO wdata_zscatter,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Makes scatter plot in circular cross-section

widget_control,event.top,get_uvalue=info,/No_Copy
widget_control,info.convertID,get_uvalue=particle,/No_Copy
if n_tags(particle,/length) eq 0 then begin
	Message,'No particle data loaded.',/continue
	widgeT_control,event.top,set_uvalue=info,/No_COpy
	return
endif
widget_control,/hourglass
deltaz=info.dz/100.0*particle.zlen
minz=-0.5*deltaz
maxz=0.5*deltaz
test=indgen(particle.np)
number=n_elements(where((test mod info.nth) eq 0))
z1=fltarr(number)
x1=fltarr(number)
y1=fltarr(number)
w1=fltarr(number)
for i=0L,number-1 do begin
	z1[i]=particle.z[info.nth*i]
	x1[i]=particle.x[info.nth*i]
	y1[i]=particle.y[info.nth*i]
	w1[i]=particle.w[info.nth*i]
endfor
total=n_elements(where((z1 GE minz) AND (z1 LE maxz)))
x=fltarr(total)
y=fltarr(total)
w=fltarr(total)
index=0
for i=0L,number-1 do begin
	if ((z1[i] GE minz) AND (z1[i] LE maxz)) then begin
		x[index]=x1[i]
		y[index]=y1[i]
		w[index]=w1[i]
		index=index+1
	endif
endfor
if info.retain eq 0 then begin
	loadCT,38,ncolors=!D.n_colors
	info.ncolors=!D.n_colors
	info.bottom=0
endif
wset,info.wid
dc=info.ncolors-info.bottom-1
d=max(w)-min(w)
wscale=(w-min(w))*dc/d+info.bottom
titlestring='Particles in '+string(minz,format='(F5.2)')+' < z <' $
            +string(maxz,format='(F5.2)')
Plot,x[0:0],y[0:0],psym=3,symsize=5.0,xtitle='X',ytitle='Y' $
        ,/xstyle,/ystyle $
	,title=titlestring $
	,xrange=[particle.x0-0.5*particle.xlen $
                 ,particle.x0+0.5*particle.xlen] $
	,yrange=[particle.y0-0.5*particle.ylen $
                 ,particle.y0+0.5*particle.ylen] $
	,/NoData,position=[0.15,0.25,0.95,0.95],/isotropic
for i=0,total-1 do begin
	oplot,x[i:i],y[i:i],psym=3,symsize=5.0,color=wscale[i]
endfor
Colorbar,bottom=info.bottom,max=max(w),minimum=min(w) $
	,ncolors=info.ncolors,position=[0.08,0.03,0.95,0.11] $
	,/top,format='(F7.2)'
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',/copy
	file='zscatter.ps'
	device,filename=file,xsize=6.5,ysize=9.0,/inches $
                ,xoffset=1.00 $
		,yoffset=1.00,/color,bits_per_pixel=8
	Plot,x[0:0],y[0:0],psym=3,symsize=5.0,xtitle='X',ytitle='Y' $
                ,/xstyle,/ystyle $
		,title=titlestring $
		,xrange=[particle.x0-0.5*particle.xlen $
                         ,particle.x0+0.5*particle.xlen] $
		,yrange=[particle.y0-0.5*particle.ylen $
                         ,particle.y0+0.5*particle.ylen] $
		,/NoData,position=[0.15,0.25,0.95,0.95],/isotropic
	for i=0,total-1 do oplot,x[i:i],y[i:i],psym=3,symsize=5.0 $
                      ,color=wscale[i]
	Colorbar,bottom=info.bottom,max=max(w),minimum=min(w) $
		,ncolors=info.ncolors,position=[0.08,0.03,0.95,0.11] $
		,/top,format='(F7.2)'
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850,no PS output to file' $
                        ,/continue
	endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
                     ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
                     ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.convertID,set_uvalue=particle,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_phiscatter,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Scatter plot of particle data in rz plane

widget_control,event.top,get_uvalue=info,/No_Copy
widget_control,info.convertID,get_uvalue=particle,/No_Copy
if n_tags(particle,/length) eq 0 then begin
	Message,'No particle data loaded.',/continue
	widgeT_control,event.top,set_uvalue=info,/No_COpy
	return
endif
widget_control,/hourglass
test=indgen(particle.np)
number=n_elements(where((test mod info.nth) eq 0))
z1=fltarr(number)
x1=fltarr(number)
y1=fltarr(number)
w1=fltarr(number)
for i=0L,number-1 do begin
	z1[i]=particle.z[info.nth*i]
	x1[i]=particle.x[info.nth*i]
	y1[i]=particle.y[info.nth*i]
	w1[i]=particle.w[info.nth*i]
endfor
r1=fltarr(number)
theta1=fltarr(number)
for i=0L,number-1 do begin
	ytemp=y1[i]-particle.y0
	xtemp=x1[i]-particle.x0
	r1[i]=sqrt(xtemp^2+ytemp^2)
	if ((ytemp eq 0) AND (xtemp eq 0)) then ytemp=0.00001
	theta1[i]=atan(ytemp,xtemp)*180/!PI
	if theta1[i] LT 0 then theta1[i]=theta1[i]+360
endfor
phimin=info.phi[2]-0.5*info.phi[3]
phimax=info.phi[2]+0.5*info.phi[3]
if phimin LT 0 then begin
	for i=0L,number-1 do begin
		if ((360-theta1[i]) LE -1*phimin) then theta1[i]=theta1[i]-360
	endfor
endif
if phimax GE 360 then begin
	for i=0L,number-1 do begin
		if (theta1[i] LE (phimax-360)) then theta1[i]=theta1[i]+360
	endfor
endif
total=n_elements(where((theta1 LE phimax) AND (theta1 GE phimin)))
r=fltarr(total)
z=fltarr(total)
w=fltarr(total)
theta=fltarr(total)
index=0
for i=0L,number-1 do begin
	if ((theta1[i] LE phimax) AND (theta1[i] GE phimin)) then begin
		r[index]=r1[i]
		theta[index]=theta1[i]
		z[index]=z1[i]
		w[index]=w1[i]
		if theta[index] LT 0 then theta[index]=theta[index]+360
		if theta[index] GE 360 then theta[index]=theta[index]-360
		index=index+1
	endif
endfor
if info.project eq 1 then begin
	for i=0L,total-1 do begin
		difference=abs(theta[i]-info.phi[2])*!PI/180
		r[i]=r[i]*cos(difference)
	endfor
endif
if info.retain eq 0 then begin
	loadCT,38,ncolors=!D.n_colors
	info.ncolors=!D.n_colors
	info.bottom=0
endif
wset,info.wid
dc=info.ncolors-info.bottom
d=max(w)-min(w)
wscale=(w-min(w))*dc/d+info.bottom
titlestring='Particles in '+string(phimin,format='(F6.1)')+' < phi < '+string(phimax,format='(F6.1)')
myxrange=[-0.5*particle.zlen,0.5*particle.zlen]
myyrange=[0,0.5*particle.ylen]
Plot,z[0:0],r[0:0],psym=3,symsize=3.0,xtitle='Z',ytitle='R',/xstyle,/ystyle $
	,title=titlestring $
	,xrange=myxrange,yrange=myyrange $
	,position=[0.15,0.25,0.95,0.95],/NoData
for i=0,total-1 do oplot,z[i:i],r[i:i],psym=3,symsize=5.0,color=wscale[i]
Colorbar,bottom=info.bottom,max=max(w),minimum=min(w) $
	,ncolors=info.ncolors,position=[0.08,0.03,0.95,0.11] $
	,/top,format='(F7.2)'
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',/copy
	file='phiscatter.ps'
	device,filename=file,xsize=6.5,ysize=6.0,/inches,xoffset=1.00 $
		,yoffset=1.00,/color,bits_per_pixel=8
	Plot,z[0:0],r[0:0],psym=3,symsize=3.0,xtitle='Z',ytitle='R',/xstyle,/ystyle $
		,title=titlestring $
		,xrange=myxrange,yrange=myyrange $
		,/NoData,position=[0.15,0.25,0.95,0.95]
	for i=0,total-1 do oplot,z[i:i],r[i:i],psym=3,symsize=5.0,color=wscale[i]
	Colorbar,bottom=info.bottom,max=max(w),minimum=min(w) $
		,ncolors=info.ncolors,position=[0.08,0.03,0.95,0.11] $
		,/top,format='(F7.2)'
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850,no PS output to file' $
                ,/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
                     ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
                     ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.convertID,set_uvalue=particle,/No_COpy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_project,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Projection method for scatter plots

widget_control,event.top,get_uvalue=info,/NO_COpy
info.project=event.index
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_nth,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Changes number of particles to use in scatter plot

widget_control,event.top,get_uvalue=info,/No_Copy
info.nth=event.value
widget_control,event.top,set_uvalue=info,/no_Copy
END

;#####################################################################
PRO wdata_dz,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****Determines width of z slice for scatter plot

widget_control,event.top,get_uvalue=info,/NO_Copy
info.dz=event.value
widget_control,event.top,set_uvalue=info,/NO_Copy
END

;#####################################################################
PRO wdata_resize,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****Resizes draw window***

widget_control,event.top,get_uvalue=info,/NO_copy
if event.ID eq info.xID then widget_control,info.drawID,draw_xsize=event.value
if event.ID eq info.yID then widget_control,info.drawID,draw_ysize=event.value
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_retain,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****decides whether or not to load new color table

widget_control,event.top,get_uvalue=info,/NO_copy
info.retain=event.index
widget_control,event.top,set_uvalue=info,/NO_copy
END

;#####################################################################
PRO wdata_device,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****decides whether to use FRC set of plots or Others

widget_control,event.top,get_uvalue=info,/NO_copy
info.device=event.index
widget_control,event.top,set_uvalue=info,/NO_copy
END

;#####################################################################
PRO wdata_psi,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***does script PSI

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
	message,'No equilibrium data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.n_colors
	info.ncolors=!D.n_colors
	info.bottom=0
endif
wset,info.wid
shell=-min(equ.psi)*.10
edge=min(equ.psi)*.01
contour,equ.psi,equ.z,equ.r,levels=[edge,0.,shell],/xstyle,/ystyle $
	,xrange=[0,max(equ.z)],yrange=[min(equ.r),max(equ.r)],/isotropic
; TMP save 2d psi data
print,''
print,'    Saving psi in fig_psi.dat for profiles_idl code'
print,''
file='fig_psi.dat'
openw,1,file
printf,1,equ.ni,equ.nj
printf,1,equ.z,equ.r
printf,1,equ.psi
close,1

thisdevice=!D.name
set_plot,'PS',copy=info.retain
if info.retain eq 0 then loadCT,0,ncolors=!D.n_colors
device,filename='psi0.ps',ysize=6,/inches,yoffset=2,/color
contour,equ.psi,equ.z,equ.r,levels=[0],/xstyle,/ystyle $
	,xrange=[0,max(equ.z)],yrange=[min(equ.r),max(equ.r)],/isotropic
device,/close_file
set_plot,thisdevice
message,'Graphic printed to file psi0.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_psi1,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Does script psi1

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
	message,'No equilibrium data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
wset,info.wid
!P.multi=[0,1,3]
!p.charsize=1.5
; zmax=equ.z[equ.ni-1]
zmax=max(equ.z)
zmin=min(equ.z)
rmax=max(equ.r)
rmin=min(equ.r)

iso=1.
if zmax/rmax gt 4 then begin
	zmin=0.
	iso=0
endif

if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.n_colors
	info.ncolors=!D.n_colors
	info.bottom=0
endif
contour,equ.psi,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Psi' $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],isotropic=iso
contour,equ.p,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='P' $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],isotropic=iso
!P.multi=[2,2,3]
k=equ.ni/2

maxp=max(equ.p)
index=where( equ.p eq maxp, count)
if count gt 0 then k=index(0)- floor(index(0)/equ.ni)*equ.ni

plot,equ.r,equ.p(k,*),xtitle='R',ytitle='P (z=0)',/xstyle $
	,xrange=[rmin,rmax]
;TMP p profile fit
psi0=min(equ.psi)
u1= 0.01*psi0
x=(equ.psi(k,*)-u1)/(psi0-u1)
p_fit=0.5*(1+tanh((x-0.73)/0.25)) + 0.1*x^1.5
p_fit= p_fit/max(p_fit)*max(equ.p(k,*))
oplot,equ.r,p_fit,linestyle=5

plot,equ.r,equ.psi(k,*),xtitle='R',ytitle='Psi (z=0)',/xstyle $
	,xrange=[rmin,rmax]
thisdevice=!D.name
set_plot,'PS',copy=info.retain
if info.retain eq 0 then loadCT,0,ncolors=!D.n_colors
device,filename='psi.ps',ysize=8,/inches,yoffset=2
!P.multi=[0,1,3]
contour,equ.psi,equ.z,equ.r,nlevels=20,/xstyle,/ystyle $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],title='Psi',isotropic=iso
contour,equ.p,equ.z,equ.r,nlevels=20,/xstyle,/ystyle $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],title='P',isotropic=iso
!P.multi=[2,2,3]
plot,equ.r,equ.p(k,*),xtitle='R',ytitle='P (z=0)',/xstyle $
	,xrange=[rmin,rmax]
plot,equ.r,equ.psi(k,*),xtitle='R',ytitle='Psi (z=0)',/xstyle $
	,xrange=[rmin,rmax]
device,/close_file
set_plot,thisdevice
!P.multi=0
!p.charsize=1.0
message,'Graphics printed to file psi.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_psi2,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Does script PSI2
; for FRC (info.device=0) these are velocity plots
; for smth else (info.device=1) these are density and v_a plots, using
;  equ.vel array for density

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
	message,'No equilibrium data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
wset,info.wid
zmax=max(equ.z)
zmin=min(equ.z)
rmax=max(equ.r)
rmin=min(equ.r)

if info.retain eq 0 then begin
;	loadCT,0,ncolors=!D.n_colors
;	info.bottom=0
;	info.ncolors=!D.n_colors
        loadCT,6,ncolors=!D.N_Colors
        loadCT,20,ncolors=20+1,bottom=0
        loadCT,33,ncolors=20,/bottom
        info.bottom=1
        info.ncolors=20
device,decomposed=0
endif

if info.device eq 0 then begin
  !P.multi=[0,1,3]

  equ.vel= (equ.vel < 40.)
  equ.vel= (equ.vel gt -40.)*equ.vel
  contour,equ.vel,equ.z,equ.r,nlevels=20,/xstyle,title='Vphi',/fill
  !P.multi=[0,1,3]
  psi0= min(equ.psi)
  plevels=[psi0*4/5,psi0*3/5,psi0*2/5,psi0/5,0.]
  contour,equ.psi,equ.z,equ.r,levels=plevels,/xstyle,/noerase
  k=fix(equ.ni/2)
  !P.multi=[4,2,3]
  plot,equ.r,equ.vel(k,*),xtitle='R',ytitle='Vphi (z=0)'
  jj= fix(0.45*equ.nj)
  plot,equ.z,equ.vel(*,jj),xtitle='Z',ytitle='Vphi'
  omega=fltarr(equ.ni,equ.nj)
  for j=0,fix(equ.nj)-1 do omega[*,j]=equ.vel[*,j]/equ.r[j]
  omega[*,0]=omega[*,1]-(omega[*,2]-omega[*,1])/(equ.r[2]-equ.r[1])
; omega= (omega < 1.) 
; omega= (omega gt -1.)*omega
;  plot,equ.r,omega,xtitle='R',ytitle='Omega (z=0)'
  !P.multi=[1,1,3]
; om_levels=[-2000,-1000,-100,-10,-1,1,10,100,1000,2000]
  om_levels=[-100,-10,-1,1,10,100]
  contour,omega,equ.z,equ.r,levels=om_levels,/xstyle,title='Omega',/fill
  print,'max(omega)=',max(omega),min(omega)
  !P.multi=[1,1,3]
  contour,equ.psi,equ.z,equ.r,levels=[0.],/xstyle,/noerase
;  plot,-equ.psi(k,*)/min(equ.psi),omega,xtitle='psi/|psi0|' $
;	,ytitle='omega (z=0)'
endif

if info.device eq 1 then begin
  !P.multi=[0,2,3]
  density=equ.vel
  k=equ.ni/2
  ;===== density plots
  contour,density,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='density' $
    ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
  plot,equ.r,density(k,*),xtitle='R',ytitle='density',xrange=[rmin,rmax] $
    ,/xstyle
;TMP rho profile fit
psi0=min(equ.psi)
u1= 0.01*psi0
x=(equ.psi(k,*)-u1)/(psi0-u1)
rho_fit=0.1+0.9*(exp(-1.5*(1-x)^1.5) - exp(-1.5))/(1-exp(-1.5))
oplot,equ.r,rho_fit,linestyle=5

  ;===== V_a plots (normalized)
  btot2=equ.bz^2 + equ.br^2 + equ.bphi^2
  index=where(density eq 0.,count)
  if count ne 0 then density(index)=.0001
  va=sqrt(btot2/density)
  bt=sqrt(btot2)
  contour,bt,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='B_tot' $
    ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
  plot,equ.r,bt(k,*),xtitle='R',ytitle='B_tot',xrange=[rmin,rmax] $
    ,/xstyle  
;print,'btot_min=',min(bt(k,*))
;TMP--------------------------------------------------
; contour,1/bt,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='B0/B' $
;    ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic,/noerase
; contour,equ.psi,equ.z,equ.r,/xsty,/ysty,xra=[zmin,zmax],yra=[rmin,rmax],/iso
; k=equ.ni/2
; plot,-equ.psi(k,*),1/bt(k,*),xtitle='B0/B',ytitle='psi',/xsty,/ysty
; TMP save 1d data for new 'omega_nstx.pro' plots.
 file='lambda-pphi.dat'
 psi0= min(reform(equ.psi(equ.ni/2,*)),jmin)
 r0= equ.r(jmin)
 openw,1,file
 printf,1,equ.nj
 printf,1,r0,psi0,-equ.psi(equ.ni/2,*),1/bt(equ.ni/2,*)
 close,1
;--------------------------
  index=where(va eq 0.,count)
  if count ne 0 then va(index)=.0001
  contour,1./va,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='1/V_a' $
    ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
  plot,equ.r,1./va(k,*),xtitle='R',ytitle='1/V_a',xrange=[rmin,rmax] $
    ,/xstyle
endif

thisdevice=!D.name
set_plot,'PS',copy=info.retain
if info.retain eq 0 then begin
;       loadCT,6,ncolors=!D.N_Colors
;       loadCT,20,ncolors=20+1,bottom=0
;       loadCT,33,ncolors=20,/bottom
;       info.bottom=1
;       info.ncolors=20
        loadCT,0,ncolors=!D.N_Colors
        info.bottom=0
        info.ncolors=!D.n_colors
;device,decomposed=0
endif
device,filename='psi2.ps',ysize=8,/inches,yoffset=2
if info.device eq 0 then begin
  !P.multi=[0,1,3]
  contour,equ.vel,equ.z,equ.r,nlevels=20,/xstyle,title='Vphi',/fill
  !P.multi=[0,1,3]
  contour,equ.psi,equ.z,equ.r,levels=plevels,/xstyle,/noerase
  !P.multi=[4,2,3]
  plot,equ.r,equ.vel(k,*),xtitle='R',ytitle='Vphi (z=0)'
  jj= fix(0.45*equ.nj)
  plot,equ.z,equ.vel(*,jj),xtitle='Z',ytitle='Vphi'

; !p.multi=[4,2,3]
; plot,equ.r,equ.vel(k,*),xtitle='R',ytitle='Vphi (z=0)'
; plot,equ.r,omega,xtitle='R',ytitle='Omega (z=0)'
  plot,-equ.psi(k,*)/min(equ.psi),omega,xtitle='psi/|psi0|' $
	,ytitle='omega (z=0)'
endif
if info.device eq 1 then begin
  !P.multi=[0,2,3]
  contour,density,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='density' $
    ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
  plot,equ.r,density(k,*),xtitle='R',ytitle='density',xrange=[rmin,rmax] $
    ,/xstyle  
  contour,bt,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='B_tot' $
    ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
  plot,equ.r,bt(k,*),xtitle='R',ytitle='B_tot',xrange=[rmin,rmax] $
    ,/xstyle
  contour,1./va,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='1/V_a' $
    ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
  plot,equ.r,1./va(k,*),xtitle='R',ytitle='1/V_a',xrange=[rmin,rmax] $
    ,/xstyle
endif
device,/close_file
set_plot,thisdevice
!p.multi=0
message,'Graphics printed to file psi2.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_psi3,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Does script PSI3***

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
	message,'No equilibrium data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
wset,info.wid
!P.multi=[0,1,3]
!p.charsize=1.5
; zmax=equ.z[equ.ni-1]
zmax=max(equ.z)
zmin=min(equ.z)
rmax=max(equ.r)
rmin=min(equ.r)

if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_Colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif

iso=0
xrange=[0,zmax]
if info.device eq 1 then begin
  iso=1
  xrange=[zmin,zmax]
endif

contour,equ.rho,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Rhoi' $
	,xrange=xrange,isotropic=iso
contour,equ.ji,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Ji' $
	,xrange=xrange,isotropic=iso
!P.multi=[2,2,3]
k=equ.ni/2
plot,equ.r,equ.rho(k,*),xtitle='R',ytitle='RHOI',/xstyle
psi0=min(equ.psi)
u1= 0.01*psi0
;TMP rhob profile fit
rhob_fit=((equ.psi(k,*)-u1)/(psi0-u1))^4*max(equ.rho)
oplot,equ.r,rhob_fit,linestyle=5
plot,equ.r,equ.ji(k,*),xtitle='R',ytitle='Ji (z=0)',/xstyle

; Save data for fig1b
;====================
;lambda_i= 5.62; shot 204707 t=0.440
;file='fig1b.dat'
;openw,1,file
;for i=0,equ.nj-1 do printf,1, equ.r[i]*lambda_i/100., equ.rho(k,i),rhob_fit[i]
;close,1
;==================

thisdevice=!D.name
set_plot,'PS',copy=info.retain
device,filename='psi3.ps',ysize=8,/inches,yoffset=2
!P.multi=[0,1,3]
if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors
contour,equ.rho,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Rhoi' $
	,xrange=xrange,isotropic=iso
contour,equ.ji,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Ji' $
	,xrange=xrange,isotropic=iso
!P.multi=[2,2,3]
plot,equ.r,equ.rho(k,*),xtitle='R',ytitle='RHOI (z=0)',/xstyle
oplot,equ.r,rhob_fit,linestyle=5
plot,equ.r,equ.ji(k,*),xtitle='R',ytitle='Ji (z=0)',/xstyle
device,/close_file
set_plot,thisdevice
!P.multi=0
!p.charsize=1.0
Message,'Graphics printed to file psi3.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_jpol,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Plots poloidal fast ion current***

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
	message,'No equilibrium data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
wset,info.wid
if info.device ne 1 then return

!P.multi=[0,2,3]
!p.charsize=1.5
; zmax=equ.z[equ.ni-1]
zmax=max(equ.z)
zmin=min(equ.z)
rmax=max(equ.r)
rmin=min(equ.r)

if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_Colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif

iso=0
xrange=[0,zmax]
if info.device eq 1 then begin
  iso=1
  xrange=[zmin,zmax]
endif

maxjr=max(equ.jir)
index=where(equ.jir eq maxjr, count)
; help,index
; print,index,count
jj=equ.nj/2
; if count eq 1 then jj=reform(index)/equ.ni
if count ge 1 then jj=index(0)/equ.ni
print,'max_jr=',maxjr,'  min_jr=',min(equ.jir), jj

k=equ.ni/2
contour,equ.jiz,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Jiz' $
	,xrange=xrange,isotropic=iso
plot,equ.r,equ.jiz(k,*),xtitle='R',ytitle='Jiz',/xstyle
contour,equ.jir,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Jir' $
	,xrange=xrange,isotropic=iso
plot,equ.z,equ.jir(*,jj),xtitle='Z',ytitle='Jir(z)',/xstyle
contour,equ.g,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='G' $
        ,xrange=xrange,isotropic=iso
plot,equ.r,equ.g(k,*),xtitle='R',ytitle='G(z=0)',/xstyle


thisdevice=!D.name
set_plot,'PS',copy=info.retain
device,filename='jpol.ps',ysize=8,/inches,yoffset=2
!P.multi=[0,2,3]
if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors

contour,equ.jiz,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Jiz' $
        ,xrange=xrange,isotropic=iso
plot,equ.r,equ.jiz(k,*),xtitle='R',ytitle='Jiz',/xstyle
contour,equ.jir,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='Jir' $
        ,xrange=xrange,isotropic=iso
plot,equ.z,equ.jir(*,jj),xtitle='Z',ytitle='Jir(z)',/xstyle
contour,equ.g,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='G' $
        ,xrange=xrange,isotropic=iso
plot,equ.r,equ.g(k,*),xtitle='R',ytitle='G(z=0)',/xstyle

device,/close_file
set_plot,thisdevice
!P.multi=0
!p.charsize=1.0
Message,'Graphics printed to file jpol.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_psi4,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Does script psi4

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
	message,'No equilibrium data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
wset,info.wid
!P.multi=[0,1,3]
!p.charsize=2.0
; zmax=equ.z[equ.ni-1]
zmax=max(equ.z)
zmin=min(equ.z)
rmax=max(equ.r)
rmin=min(equ.r)
iso=1.
if zmax/rmax gt 4 then begin
        zmin=0.
        iso=0
endif
curt=equ.cur+equ.ji
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_Colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
contour,curt,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='J_phi' $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],isotropic=iso
contour,equ.cur,equ.z,equ.r,nlevels=20,/xstyle,/ystyle $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],title='Jb_phi',isotropic=iso
!P.multi=[2,2,3]
k=equ.ni/2
;k=equ.ni/4
plot,equ.r,curt(k,*),xtitle='R',ytitle='J_phi',xrange=[rmin,rmax] $
  ,/xstyle
oplot,equ.r,equ.cur(k,*),linestyle=2
plot,equ.r,equ.ji(k,*),xtitle='R',ytitle='Ji_phi',xrange=[rmin,rmax] $
  ,/xstyle

thisdevice=!D.name
set_plot,'PS',copy=info.retain
device,filename='psi4.ps',ysize=8,/inches,yoffset=2
!p.multi=[0,1,3]
if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors
contour,curt,equ.z,equ.r,nlevels=20,/xstyle,/ystyle $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],title='J_phi',isotropic=iso
contour,equ.cur,equ.z,equ.r,nlevels=20,/xstyle,/ystyle $
	,xrange=[zmin,zmax],yrange=[rmin,rmax],title='Jb_phi',isotropic=iso
!P.multi=[2,2,3]
plot,equ.r,curt(k,*),xtitle='R',ytitle='J_phi',xrange=[rmin,rmax] $
  ,/xstyle
oplot,equ.r,equ.cur(k,*),linestyle=2
plot,equ.r,equ.ji(k,*),xtitle='R',ytitle='Ji_phi',xrange=[rmin,rmax] $
  ,/xstyle

device,/close_file
set_plot,thisdevice
!P.multi=0
!p.charsize=1.0
message,'Graphics printed to psi4.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
FUNCTION qvalue,z,r,psi,bz,br,bphi,psiv
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****Calculates qv for psi=psiv, to be used in psi5.

ni=n_elements(z)
nj=n_elements(r)

if psiv gt 0.0 then RETURN, -1.0
contour,psi,z,r,/path_data_coords,path_info=info,path_xy=xy $
       ,levels=[psiv]

infoi=n_elements(info.n) - 1
x=reform( xy(0,info(infoi).offset + indgen(info(infoi).n)) )
y=reform( xy(1,info(infoi).offset + indgen(info(infoi).n)) )

;------check if contour is closed
if min(x) LE min(z) OR max(x) GE max(z) then RETURN, -1.0
if min(y) LE min(r) OR max(y) GE max(r) then RETURN, -1.0


;------Calculate s_i

ns=info(infoi).n + 1
s=fltarr(ns)
s(0)=0.0
for i=1,ns-2 do s(i)=s(i-1) + sqrt( (x(i)-x(i-1))^2+(y(i)-y(i-1))^2 )
s(ns-1)= s(ns-2) + sqrt( (x(0)-x(ns-2))^2+(y(0)-y(ns-2))^2 )

;------Calculate f_i

dz=z(1)-z(0)
dr=r(1)-r(0)
ix=(x-z(0))/dz
iy=(y-r(0))/dr
f=fltarr(ni,nj)
r2d=fltarr(ni,nj)
z2d=fltarr(ni,nj)
for j=0,nj-1 do z2d(*,j)=z
for i=0,ni-1 do r2d(i,*)=r
bp=sqrt( bz^2 + br^2 )
f=bphi/(r2d*bp)
fi=fltarr(ns)
fii=interpolate(f,ix,iy,/cubic)
fi=[fii,fii(0)]

;-------Calculate integral Int(f ds)

qv=int_tabulated(s,fi)/6.2831853

RETURN, qv
END

;######################################################################
FUNCTION q_profile,psi
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****Calculates q(psi) for given q profile, to be used in psi5.

psi0= min(psi)
u1= 0.01
u1= u1*psi0
x= (psi-u1)/(psi0-u1)

;qfit= 2.36 + 9.34*exp( -(x^0.75)/0.45 )*(1-x)
;qfit= 0.82 + 10.5*exp( -(x^0.7)/0.3 )*(1-x)
qfit= 1.2 + 16.5*exp( -(x^0.6)/0.3 )*(1-x)^4

RETURN, qfit
END

;#####################################################################
PRO wdata_psi5,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Does script psi5 (toroidal m.field and poloidal current plots).

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
	message,'No equilibrium data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
wset,info.wid
!P.multi=[0,2,3]
!p.charsize=1.5
zmax=max(equ.z)
zmin=min(equ.z)
rmax=max(equ.r)
rmin=min(equ.r)
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_Colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
; Bz,Br vector plot
nib=fix(equ.ni/20.)
njb=fix(equ.nj/14.)
nii= equ.ni-(equ.ni mod nib)
njj= equ.nj-(equ.nj mod njb)
z2=equ.z(0:nii-1) 
r2=equ.r(0:njj-1) 
z1=rebin(z2,nii/nib)
r1=rebin(r2,njj/njb)
bz2=equ.bz(0:nii-1,0:njj-1)
br2=equ.br(0:nii-1,0:njj-1)
vz=rebin(bz2,nii/nib,njj/njb)
vr=rebin(br2,nii/nib,njj/njb)
velovect,vz,vr,z1,r1,xtitle='Z',ytitle='R',title='Bz, Br' $
   ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic

if max(abs(equ.bphi)) eq 0.0 then begin
	message,'B_phi=0.',/continue
        widget_control,event.top,set_uvalue=info,/NO_COpy
        return
endif

; Jz,Jr vector plot
jz2=equ.curz(0:nii-1,0:njj-1)
jr2=equ.curr(0:nii-1,0:njj-1)
jz=rebin(jz2,nii/nib,njj/njb)
jr=rebin(jr2,nii/nib,njj/njb)
velovect,jz,jr,z1,r1,xtitle='Z',ytitle='R',title='Jz, Jr' $
   ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic

; B_phi contour
contour,equ.bphi,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='B_phi' $
  ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic,/fill
; J_phi contour
; contour,equ.cur,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='J_phi' $
;  ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic

k=equ.ni/2
maxb=max(equ.bphi)
index=where( equ.bphi eq maxb, count)
;if count gt 0 then k=index(0)- floor(index(0)/equ.ni)*equ.ni

; Calculate q.

if info.device eq 1 then begin
  qv= fltarr(njj/njb)
  psi2=reform(equ.psi(k,0:njj-1))
  psiv=rebin(psi2,njj/njb)
  for jj=0,njj/njb-1 do  $
   qv[jj]= qvalue(equ.z,equ.r,equ.psi,equ.bz,equ.br,equ.bphi,psiv[jj]) 
; q(psi) plot
 psi0= 0.99*min(equ.psi)
 nq= 30
 psia= psi0 - findgen(nq)/(nq-1)*psi0
 qva= fltarr(nq)
 for i=0,nq-1 do $
  qva[i]= qvalue(equ.z,equ.r,equ.psi,equ.bz,equ.br,equ.bphi,psia[i])
 qprof= q_profile(psia)
 qva= abs(qva)

 plot,psia(where(qva gt 0.)),qva(where(qva gt 0.)),xtitle='psi' $
    ,ytitle='q',xrange=[psi0,0.],/xstyle,yrange=[0.,12.],/ystyle
 oplot,psia,qprof,linestyle=5
endif

; Save data for fig1c
;====================
;file='fig1c.dat'
;openw,1,file
;for i=0,nq-1 do printf,1, psia[i],qprof[i],psia[i],qva[i]
;close,1
;==================

; B_phi(R) plot.
plot,equ.r,equ.bphi(k,*),xtitle='R',ytitle='B_phi',xrange=[rmin,rmax] $
  ,/xstyle
oplot,equ.r,equ.bphi(k,0)*equ.r(0)/equ.r,linestyle=5
print, 'B_phi>0 at i=',k

; q(R) plot
if info.device eq 1 then begin
 ind=where(psiv le 0. and abs(qv) gt 0.,count)
 print,''
 print,'  z(k)=',equ.z(k)
 print,''
 if count gt 0 then begin
  plot,r1(ind),abs(qv(ind)) $
    ,xtitle='R',ytitle='q',xrange=[rmin,rmax],/xstyle $
    ,yrange=[0.,10.],/ystyle
 endif
endif

; Bz plot
if info.device eq 0  then begin
plot,equ.r,equ.bz(k,*),xtitle='R',ytitle='B_z',xrange=[rmin,rmax] $
  ,/xstyle
plot,equ.r,equ.curz(k,*),xtitle='R',ytitle='J_z',xrange=[rmin,rmax] $
  ,/xstyle
endif

thisdevice=!D.name
set_plot,'PS',copy=info.retain
device,filename='psi5.ps',ysize=8,/inches,yoffset=2
!p.multi=[0,2,3]
if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors
velovect,vz,vr,z1,r1,xtitle='Z',ytitle='R',title='Bz, Br' $
   ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
velovect,jz,jr,z1,r1,xtitle='Z',ytitle='R',title='Jz, Jr' $
   ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
contour,equ.bphi,equ.z,equ.r,nlevels=20,/xstyle,/ystyle,title='B_phi' $
   ,xrange=[zmin,zmax],yrange=[rmin,rmax],/isotropic
if info.device eq 1 then begin
plot,psia(where(qva gt 0.)),qva(where(qva gt 0.)),xtitle='psi' $
    ,ytitle='q',xrange=[psi0,0.],/xstyle,yrange=[0.,12.],/ystyle
oplot,psia,qprof,linestyle=5
endif
plot,equ.r,equ.bphi(k,*),xtitle='R',ytitle='B_phi',xrange=[rmin,rmax] $
  ,/xstyle
oplot,equ.r,equ.bphi(k,0)*equ.r(0)/equ.r,linestyle=5
if info.device eq 1 then begin
if count gt 0 then begin
 plot,r1(ind),abs(qv(ind)) $
    ,xtitle='R',ytitle='q',xrange=[rmin,rmax],/xstyle $
    ,yrange=[0.,10.],/ystyle
endif
endif
; Bz plot
if info.device eq 0  then begin
plot,equ.r,equ.bz(k,*),xtitle='R',ytitle='B_z',xrange=[rmin,rmax] $
  ,/xstyle
plot,equ.r,equ.curz(k,*),xtitle='R',ytitle='J_z',xrange=[rmin,rmax] $
  ,/xstyle
endif


device,/close_file
set_plot,thisdevice
!P.multi=0
!p.charsize=1.0
message,'Graphics printed to psi5.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_rhoi,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****Executes script RHOI

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.printID,get_uvalue=rh,/No_COpy
if n_tags(rh,/length) eq 0 then begin
	message,'No rhoi data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
        message,'No equilibrium data loaded.',/continue
        widget_control,event.top,set_uvalue=info,/NO_COpy
        return
endif
wset,info.wid
!P.multi=[0,1,3]
!p.charsize=1.5
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_Colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
zmax=rh.z[rh.ni-1]
zmin=rh.z[0]
iso=0
xrange=[0,zmax]
if info.device eq 1 then begin
  iso=1
  xrange=[zmin,zmax]
endif

contour,rh.rhoi,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Rhoi',isotropic=iso
contour,rh.ji,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Ji',isotropic=iso
!P.multi=[2,2,3]
k=rh.ni/2
plot,rh.r,rh.rhoi(k,*),xtitle='R',ytitle='RHOI',/xstyle
oplot,equ.r,equ.rho(k,*),linestyle=2
plot,rh.r,rh.ji(k,*),xtitle='R',ytitle='Ji (z=0)',/xstyle
oplot,equ.r,equ.ji(k,*),linestyle=2

thisdevice=!D.name
set_plot,'PS',copy=info.retain
if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors-1
device,filename='rhoi.ps',ysize=8,/inches,yoffset=2
!P.multi=[0,1,3]
contour,rh.rhoi,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Rhoi',isotropic=iso
contour,rh.ji,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Ji',isotropic=iso
!P.multi=[2,2,3]
plot,rh.r,rh.rhoi(k,*),xtitle='R',ytitle='RHOI (z=0)',/xstyle
plot,rh.r,rh.ji(k,*),xtitle='R',ytitle='Ji (z=0)',/xstyle

device,/close_file
set_plot,thisdevice
!P.multi=0
!p.charsize=1.0
message,'Graphics printed to rhoi.ps.',/continue
widget_control,info.printID,set_uvalue=rh,/No_Copy
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_ji,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****Executes script JI

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.printID,get_uvalue=rh,/No_COpy
if n_tags(rh,/length) eq 0 then begin
	message,'No rhoi data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_COpy
	return
endif
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
if n_tags(equ,/length) eq 0 then begin
        message,'No equilibrium data loaded.',/continue
        widget_control,event.top,set_uvalue=info,/NO_COpy
        return
endif
wset,info.wid
if info.device ne 1 then return

!P.multi=[0,1,3]
!p.charsize=1.5
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_Colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
zmax=rh.z[rh.ni-1]
zmin=rh.z[0]
iso=0
xrange=[0,zmax]
if info.device eq 1 then begin
  iso=1
  xrange=[zmin,zmax]
endif

maxjr=max(rh.jir)
index=where(rh.jir eq maxjr, count)
jj=rh.nj/2
if count ge 1 then jj=index(0)/rh.ni
print,'max_jr=',maxjr,'  min_jr=',min(rh.jir), jj

contour,rh.jiz,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Jiz',isotropic=iso,/fill
contour,rh.jir,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Jir',isotropic=iso,/fill
!P.multi=[2,2,3]
k=rh.ni/2
plot,rh.r,rh.jiz(k,*),xtit='R',ytit='Jiz',/xsty $
    ,yra=[min(equ.jiz(k,*)),max(equ.jiz(k,*))]
oplot,equ.r,equ.jiz(k,*),color=150
plot,rh.z,rh.jir(*,jj),xtit='Z',ytit='Jir(z)',/xsty $
    ,yra=[min(equ.jir(*,jj)),max(equ.jir(*,jj))]
oplot,equ.z,equ.jir(*,jj),color=150

thisdevice=!D.name
set_plot,'PS',copy=info.retain
if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors-1
device,filename='ji_zr.ps',ysize=8,/inches,yoffset=2
!P.multi=[0,1,3]
contour,rh.jiz,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Jiz',isotropic=iso
contour,rh.jir,rh.z,rh.r,nlevels=20,/xstyle,/ystyle,xrange=xrange $
	,title='Jir',isotropic=iso
!P.multi=[2,2,3]
plot,rh.r,rh.jiz(k,*),xtitle='R',ytitle='Jiz (z=0)',/xstyle
plot,rh.z,rh.jir(*,jj),xtitle='Z',ytitle='Jir(z)',/xstyle

device,/close_file
set_plot,thisdevice
!P.multi=0
!p.charsize=1.0
message,'Graphics printed to ji_zr.ps.',/continue
widget_control,info.printID,set_uvalue=rh,/No_Copy
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_f0,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***does script F0

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
;if n_tags(equ,/length) eq 0 then begin
;        message,'No equilibrium data loaded.',/continue
;        widget_control,event.top,set_uvalue=info,/NO_COpy
;        return
;endif
if info.retain eq 0 then begin
        loadCT,0,ncolors=!D.n_colors
        info.ncolors=!D.n_colors
        info.bottom=0
endif
wset,info.wid

print,''
print,'Reading particle data from ff.dat, psp.dat, en.dat'
file='ff.dat'         ; Read lambda and p_phi data
openr,1,file
readf,1,np
close,1
psp=fltarr(np)
lambda=fltarr(np)
pphi=fltarr(np)
en=fltarr(np)

openr,1,file
readf,1,np,lambda,pphi
close,1

file='en.dat'         ; Read energy data
openr,1,file
readf,1,np2,en
close,1

file='psp.dat'        ; Read psp
openr,1,file
readf,1,np3,psp
close,1

pmax= max(pphi)
pmin= min(pphi)
pmin= 20.
emax= max(en)
emin= min(en)

; Calculate f(v_pll/v,eps).

nx=20
ny=20
ax=findgen(nx)
ay=findgen(ny)
ff=fltarr(nx,ny)     ; note ff=0.

xmin=0
xmax=1.01
ymin=0.
ymax=emax
ax= xmin + (xmax-xmin)*ax/(nx-1.)
ay= ymin + (ymax-ymin)*ay/(ny-1.)
dax= (xmax-xmin)/(nx-1.)
day= (ymax-ymin)/(ny-1.)
print,''
;print,min(lambda),max(lambda)
for m=0L,np-1 do begin
;  if lambda[m] ge 1. then lambda[m]=1.
  if lambda[m] ge 1. then begin
    lambda[m]=1.
    psp[m]=0.
  endif
  axm=sqrt(1.00001-lambda[m])                    ; V_pll/V~ sqrt(1-lambda)
  if axm gt xmax then axm=xmax
  aym=en[m]
  i=fix( (axm-xmin)/dax + 0.5 )
  j=fix( (aym-ymin)/day + 0.5 )
  ff[i,j]= ff[i,j] + psp[m]
endfor
fmax= max(ff)
ff= ff/fmax
;resline=1./ax^2/emax
om=0.34
vvp=(1./om-1)/(1+1/8.6)
resline=0.5*(vvp)^2/ax^2/emax
;contour,ff,ax,ay/emax,/xsty,/ysty,nlev=10,xtitle='V_pll/V',ytitle='energy' $
contour,ff,ax,ay/emax,/xsty,/ysty,xtitle='V_pll/V',ytitle='energy' $
       ,lev=0.6*[1./20,1./10,1./4,1./2,1.] $
       ,xra=[xmin,xmax],yrange=[0,1.],title='',/fill
plot,ax,0.5*(1.38)^2/ax^2/emax,xra=[xmin,xmax],yrange=[0,1.],/xsty,/ysty,/noerase
oplot,ax,0.5*(1.4)^2/ax^2/emax
oplot,ax,0.5*(1.55)^2/ax^2/emax
;plot,ax,resline,xra=[xmin,xmax],yrange=[0,1.],/xsty,/ysty,/noerase
;

thisdevice=!D.name
set_plot,'PS',copy=info.retain
;if info.retain eq 0 then loadCT,0,ncolors=!D.n_colors
if info.retain eq 0 then begin
        loadCT,0,ncolors=!D.n_colors,bottom=1
        info.ncolors=!D.n_colors
        info.bottom=0
        loadCT,4,ncolors=!D.n_colors,bottom=1
;       info.bottom=!D.n_colors-1
        info.ncolors=!D.n_colors
;device,decomposed=0
endif
device,filename='f0.ps',xsize=3.0,ysize=4,/inches,yoffset=2,/color

contour,ff,ax,ay/emax,/xsty,/ysty,nlev=10,xtitle='V_pll/V',ytitle='energy' $
       ,xra=[xmin,xmax],yrange=[0,1.],title='',/fill
plot,ax,0.5*(1.38)^2/ax^2/emax,xra=[xmin,xmax],yra=[0,1.],/xsty,/ysty,/noerase
oplot,ax,0.5*(1.38)^2/ax^2/emax,color=300
oplot,ax,0.5*(1.4)^2/ax^2/emax,color=350
oplot,ax,0.5*(1.55)^2/ax^2/emax,color=400

device,/close_file
set_plot,thisdevice
message,'Graphic printed to file f0.ps.',/continue
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_draw2,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Draws distribution function integrated over one direction

widget_control,event.top,geT_uvalue=info,/No_Copy
ids=[info.loadnewID,info.load1ID,info.dataID]
for i=0,2 do begin
	for j=0,2 do begin
		if event.ID eq info.draw2ID[i,j] then begin
		  widget_control,ids[i],get_uvalue=df,/No_Copy
		  theid=ids[i]
		  thej=j
		  thei=i
		endif
	endfor
endfor
if n_tags(df,/length) eq 0 then begin
	message,'Please compute distribution function before displaying.',/continue
	widget_control,event.top,set_uvalue=info,/NO_Copy
	return
endif
wset,info.wid
!P.multi=[0,2,2,0,1]
CASE thej OF
	0 : BEGIN
	F2=fltarr(info.nbins[1],info.nbins[2])
	Faccurate2=fltarr(info.nbins[1],info.nbins[2])
	deltaF2=fltarr(info.nbins[1],info.nbins[2])
	for k=0,info.nbins[0]-1 do begin
		F2=F2+reform(df.F[k,*,*])*df.deltav0
		Faccurate2=Faccurate2+reform(df.Faccurate[k,*,*])*df.deltav0
		deltaF2=deltaF2+reform(df.deltaF[k,*,*])*df.deltav0
	endfor
	binv0=df.binv1
	binv1=df.binv2
	if thei eq 0 then begin
		xtitle='Vy'
		ytitle='Vz'
	endif
	if thei eq 1 then begin
		xtitle='Vr'
		ytitle='Vphi'
	endif
	if thei eq 2 then begin
		xtitle='Vn'
		ytitle='Vphi'
	endif
	END
	1 : BEGIN
	F2=fltarr(info.nbins[0],info.nbins[2])
	Faccurate2=fltarr(info.nbins[0],info.nbins[2])
	deltaF2=fltarr(info.nbins[0],info.nbins[2])
	for k=0,info.nbins[1]-1 do begin
		F2=F2+reform(df.F[*,k,*])*df.deltav1
		Faccurate2=Faccurate2+reform(df.Faccurate[*,k,*])*df.deltav1
		deltaF2=deltaF2+reform(df.deltaF[*,k,*])*df.deltav1
	endfor
	binv0=df.binv0
	binv1=df.binv2
	if thei eq 0 then begin
		xtitle='Vx'
		ytitle='Vz'
	endif
	if thei eq 1 then begin
		xtitle='Vz'
		ytitle='Vphi'
	endif
	if thei eq 2 then begin
		xtitle='Vp'
		ytitle='Vphi'
	endif
	END
	2 : BEGIN
	F2=fltarr(info.nbins[0],info.nbins[1])
	Faccurate2=fltarr(info.nbins[0],info.nbins[1])
	deltaF2=fltarr(info.nbins[0],info.nbins[1])
	for k=0,info.nbins[2]-1 do begin
		F2=F2+reform(df.F[*,*,k])*df.deltav2
		Faccurate2=Faccurate2+reform(df.Faccurate[*,*,k])*df.deltav2
		deltaF2=deltaF2+reform(df.deltaF[*,*,k])*df.deltav2
	endfor
	binv0=df.binv0
	binv1=df.binv1
	if thei eq 0 then begin
		xtitle='Vx'
		ytitle='Vy'
	endif
	if thei eq 1 then begin
		xtitle='Vz'
		ytitle='Vr'
	endif
	if thei eq 2 then begin
		xtitle='Vp'
		ytitle='Vn'
	endif
	END
ENDCASE
if info.retain eq 0 then begin
	loadCT,33,Ncolors=20,/Bottom
	info.ncolors=20
	info.bottom=1
endif
a=max(Faccurate2)
b=min(Faccurate2)
step=(a-b)/19.0
levels=IndGen(20)*step+b
if levels[1] eq levels[0] then begin
	Message,'Cannot create contour plot.',/continue
	!P.multi=0
	!X.margin[0]=10.0
	widget_control,theid,set_uvalue=df,/No_Copy
	widget_control,event.top,set_uvalue=info,/No_Copy	
	return	
endif
a=max(deltaF2)
b=min(deltaF2)
step2=(a-b)/19.0
levels2=IndGen(20)*step2+b
if levels[1] eq levels[0] then begin
	Message,'Cannot create contour plot.',/continue
	!P.multi=0
	!X.margin[0]=10.0
	widget_control,theid,set_uvalue=df,/No_Copy
	widget_control,event.top,set_uvalue=info,/No_Copy	
	return	
endif
myxrange=[min(binv0)/info.small,max(binv0)/info.small]
myyrange=[min(binv1)/info.small,max(binv1)/info.small]
!X.margin[0]=6.0
; Decomposed color off
device, Get_Decomposed=thisDecomposed
device, decomposed=0
contour,F2,binv0,binv1,xtitle=xtitle,ytitle=ytitle,/xstyle,/ystyle $
	,/fill,levels=levels,C_Colors=IndGen(20)+2 $
	,title='F',/isotropic,/xticklen,/yticklen,xrange=myxrange $
	,yrange=myyrange,charsize=0.8
contour,Faccurate2,binv0,binv1,xtitle=xtitle,ytitle=ytitle,/xstyle $
	,/ystyle,/fill,levels=levels,/isotropic $
	,C_Colors=IndGen(20)+1,title='F0 + delta F',/xticklen $
	,/yticklen,xrange=myxrange,yrange=myyrange,charsize=0.8
contour,deltaF2,binv0,binv1,xtitle=xtitle,ytitle=ytitle,/xstyle,/ystyle $
	,/fill,levels=levels2,C_colors=IndGen(20)+2 $
	,title='delta F',/isotropic,/xticklen,/yticklen,xrange=myxrange $
	,yrange=myyrange,charsize=0.8
!P.multi=0
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',/copy
	if thei eq 0 then choice='xyz'
	if thei eq 1 then choice='zrphi'
	if thei eq 2 then choice='pnphi'
	file='df'+choice+'.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color
	!P.Multi=[0,2,2,0,1]
	contour,F2,binv0,binv1,xtitle=xtitle,ytitle=ytitle,/xstyle,/ystyle $
		,/fill,levels=levels,C_Colors=IndGen(20)+2 $
		,title='F',/isotropic,/xticklen,/yticklen,xrange=myxrange $
		,yrange=myyrange,charsize=1.5
	contour,Faccurate2,binv0,binv1,xtitle=xtitle,ytitle=ytitle,/xstyle $
		,/ystyle,/fill,levels=levels,/isotropic $
		,C_Colors=IndGen(20)+1,title='F0 + delta F',/xticklen $
		,/yticklen,xrange=myxrange,yrange=myyrange,charsize=1.5
	contour,deltaF2,binv0,binv1,xtitle=xtitle,ytitle=ytitle,/xstyle,/ystyle $
		,/fill,levels=levels2,C_colors=IndGen(20)+2 $
		,title='delta F',/isotropic,/xticklen,/yticklen,xrange=myxrange $
		,yrange=myyrange,charsize=1.5
	!P.Multi=0
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
!X.margin[0]=10.0
widget_control,theid,set_uvalue=df,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_draw1,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Draws distribution function integrated over two directions

widget_control,event.top,get_uvalue=info,/No_Copy
ids=[info.loadnewID,info.load1ID,info.dataID]
wset,info.wid
for i=0,2 do begin
	for j=0,2 do begin
		if event.ID eq info.draw1ID[i,j] then begin
		   widget_control,ids[i],get_uvalue=df,/No_Copy
		   TheID=ids[i]
		   TheI=i	
		   TheJ=j
		endif
	endfor
endfor
if n_tags(df,/length) eq 0 then begin
	Message,'Please compute the distribution function before display.',/continue
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif
CASE Thej OF
	0: BEGIN
	F1=fltarr(info.nbins[0])
	Faccurate1=fltarr(info.nbins[0])
	deltaF1=fltarr(info.nbins[0])
	for i=0,info.nbins[1]-1 do begin
		for j=0,info.nbins[2]-1 do begin
			F1=F1+reform(df.F[*,i,j])*df.deltav1*df.deltav2
			Faccurate1=Faccurate1+reform(df.Faccurate[*,i,j])*df.deltav1*df.deltav2
			deltaF1=deltaF1+reform(df.deltaF[*,i,j])*df.deltav1*df.deltav2
		endfor
	endfor
	binv=df.binv0
	if thei eq 0 then myxtitle='Vx'
	if thei eq 1 then myxtitle='Vz'
	if thei eq 2 then myxtitle='Vp'
	END
	1: BEGIN
	F1=fltarr(info.nbins[1])
	Faccurate1=fltarr(info.nbins[1])
	deltaF1=fltarr(info.nbins[1])
	for i=0,info.nbins[0]-1 do begin
		for j=0,info.nbins[2]-1 do begin
			F1=F1+reform(df.F[i,*,j])*df.deltav0*df.deltav2
			Faccurate1=Faccurate1+reform(df.Faccurate[i,*,j])*df.deltav0*df.deltav2
			deltaF1=deltaF1+reform(df.deltaF[i,*,j])*df.deltav0*df.deltav2
		endfor
	endfor
	binv=df.binv1
	if thei eq 0 then myxtitle='Vy'
	if thei eq 1 then myxtitle='Vr'
	if thei eq 2 then myxtitle='Vn'
	END
	2: BEGIN
	F1=fltarr(info.nbins[2])
	Faccurate1=fltarr(info.nbins[2])
	deltaF1=fltarr(info.nbins[2])
	for i=0,info.nbins[0]-1 do begin
		for j=0,info.nbins[1]-1 do begin
			F1=F1+reform(df.F[i,j,*])*df.deltav1*df.deltav0
			Faccurate1=Faccurate1+reform(df.Faccurate[i,j,*])*df.deltav1*df.deltav0
			deltaF1=deltaF1+reform(df.deltaF[i,j,*])*df.deltav1*df.deltav0
		endfor
	endfor
	binv=df.binv2
	if thei eq 0 then myxtitle='Vz'
	if ((thei eq 1) OR (thei eq 2)) then myxtitle='Vphi'
	END
ENDCASE
!P.Multi=[0,1,3,0,1]
myrange=[min(binv)/info.small,max(binv)/info.small]
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.n_colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
plot,binv,F1,xtitle=myxtitle,title='F',/ystyle,/xstyle,/xticklen $
	,xrange=myrange,charsize=1.5
plot,binv,Faccurate1,xtitle=myxtitle,title='F0 (known) + deltaF' $
	,/ystyle,/xstyle,/xticklen,xrange=myrange,charsize=1.5
plot,binv,deltaF1,xtitle=myxtitle,title='delta F',/xstyle,/ystyle $
	,/xticklen,xrange=myrange,charsize=1.5
!P.Multi=0
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',/copy
	if thei eq 0 then choice='xyz'
	if thei eq 1 then choice='zrphi'
	if thei eq 2 then choice='pnphi'
	file='df'+choice+'.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color
	!P.Multi=[0,1,3,0,1]
	plot,binv,F1,xtitle=myxtitle,title='F',/ystyle,/xstyle,/xticklen $
		,xrange=myrange,charsize=2.0
	plot,binv,Faccurate1,xtitle=myxtitle,title='F0 (known) + deltaF' $
		,/ystyle,/xstyle,/xticklen,xrange=myrange,charsize=2.0
	plot,binv,deltaF1,xtitle=myxtitle,title='delta F',/xstyle,/ystyle $
		,/xticklen,xrange=myrange,charsize=2.0
	!P.Multi=0
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
widget_control,TheID,set_uvalue=df,/No_COpy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_printnext,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Sets to print next image generated**

widget_control,event.top,get_uvalue=info,/No_Copy
if event.id eq info.printfileID then info.printnext=1
if event.id eq info.printsID then info.printnext=2
if event.id eq info.printID then info.printnext=3
widget_control,event.top,set_uvalue=info,/No_COpy
END

;#####################################################################
PRO wdata_vectcont,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;**Draws contour plots of z,r, or phi components in circular 
; cross-section

widget_control,event.top,get_uvalue=info,/No_Copy
widget_control,info.fileID,get_uvalue=vector,/No_Copy
if n_tags(vector,/length) eq 0 then begin
	message,'No vector data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif
widget_control,/hourglass
ni=vector.ni
nj=vector.nj
nk=vector.nk
zero=complexarr(nk)
for k=0,nk-1 do zero(k)=0.
k=info.nmode
CASE info.component OF
	0 : BEGIN
	array1=vector.vz
	choice='Z'
	END
	1 : BEGIN
	array1=vector.vr
	choice='R'
	END
	2 : BEGIN
	array1=vector.vs
	choice='Phi'
	END
	3 : BEGIN
        array1=vector.vz
	array2=vector.vr
	array3=vector.vs
	choice='Parallel'
        END
	4 : BEGIN
        array1=vector.vz
        array2=vector.vr
        array3=vector.vs
	choice='Total'
	END
ENDCASE
array=fltarr(ni,nj,nk)
if k ge 0 then begin
  for i=0,ni-1 do begin
  for j=0,nj-1 do begin
	tmp=reform(array1(i,j,*))
	tmp1=FFT(tmp)
	zero(k)=tmp1(k)
	if k gt 0 then zero(nk-k)=tmp1(nk-k)
	tmp=FFT(zero,1)
	array(i,j,*)=float(tmp)
  endfor
  endfor
  if info.component ge 3 then begin
      array1=array
      for i=0,ni-1 do begin
      for j=0,nj-1 do begin
        tmp=reform(array2(i,j,*))
        tmp1=FFT(tmp)
        zero(k)=tmp1(k)
	if k gt 0 then zero(nk-k)=tmp1(nk-k)
        tmp=FFT(zero,1)
        array(i,j,*)=float(tmp)
      endfor
      endfor
      array2=array
      for i=0,ni-1 do begin
      for j=0,nj-1 do begin
        tmp=reform(array3(i,j,*))
        tmp1=FFT(tmp)
        zero(k)=tmp1(k)
	if k gt 0 then zero(nk-k)=tmp1(nk-k)
        tmp=FFT(zero,1)
        array(i,j,*)=float(tmp)
      endfor
      endfor
      array3=array
      if info.component eq 3 then begin
         array=array1*vector.vz + array2*vector.vr + array3*vector.vs
         array1=sqrt(vector.vz^2 + vector.vr^2 + vector.vs^2)
         array=array/array1
      endif
      if info.component eq 4 then $
         array= sqrt(array1^2 + array2^2 + array3^2)
  endif
endif else begin
  array=array1
  if info.component ge 3 then $
     array= sqrt(array1^2 + array2^2 + array3^2)
endelse

zvalue=info.z[1]/100.0*(max(vector.zpol)-min(vector.zpol))+min(vector.zpol)
interval=(max(vector.zpol)-min(vector.zpol))/(ni-1)
index1=fix((zvalue-min(vector.zpol))/interval)
index2=ceil((zvalue-min(vector.zpol))/interval)
if index2 EQ ni then index2=index2-1
slice1=reform(array(index1,*,*))
slice2=reform(array(index2,*,*))
slice=slice1*(vector.zpol(index2)-zvalue)/interval+ $
	slice2*(zvalue-vector.zpol(index1))/interval
tslice=transpose(slice)
wset,info.wid
if info.scale[2] eq 0 then begin
	maximum=max(array)
	minimum=min(array)
endif else begin
	maximum=max(tslice)
	minimum=min(tslice)
endelse
step=(maximum-minimum)/(info.ncont[3]-1)
levels=IndGen(info.ncont[3])*step+minimum
if levels[1] eq levels[0] then begin
	Message,'Cannot create contour plot.',/continue
	widget_control,info.fileID,set_uvalue=vector,/No_Copy
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif
if info.retain eq 0 then begin
	loadCT,6,ncolors=!D.N_Colors
	loadCT,20,ncolors=info.ncont[3]+1,bottom=0
	loadCT,33,ncolors=info.ncont[3],bottom=1
	info.bottom=1
	info.ncolors=info.ncont[3]
endif
string=string(vector.type,' (',choice,') at z=',zvalue)
device, decomposed=0
vectorsp= vector.spol+.001
polar_contour,tslice,vectorsp,vector.rpol,/xstyle,/ystyle,/fill $
	,/isotropic,title=string,Levels=levels $
	,C_Colors=Indgen(info.ncont[3])+1,position=[0.15,0.25,0.95,0.95] $
	,xtitle='X',ytitle='Y'
; for min(r)>0
minr=min(vector.rpol)
if minr gt 0.01*max(vector.rpol) then begin
  ri=findgen(20)/19.*minr*1.0
  fi=fltarr(nk,20)
  for k=0,nk-1 do fi(k,*)=ri
  polar_contour,fi,vectorsp,ri,levels=[minr],/isotropic,/fill,c_colors=0 $
               ,/overplot
endif

Colorbar,bottom=1,max=maximum,minimum=minimum $
	,ncolors=info.ncont[3],position=[0.08,0.03,0.95,0.11] $
        ,/top,format='(E10.2)'
;	,/top,format='(F7.4)'
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file=choice+'contour.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then begin
		loadCT,6,ncolors=!D.N_Colors
		loadCT,20,ncolors=info.ncont[3]+1,bottom=0
		loadCT,33,ncolors=info.ncont[3],/bottom
	endif
	!P.color=!P.background
	polar_contour,tslice,vectorsp,vector.rpol,/xstyle,/ystyle,/fill $
		,/isotropic,title=string,Levels=levels $
		,C_Colors=IndGen(info.ncont[3])+1,position=[0.15,0.25,0.95,0.95] $
		,xtitle='X',ytitle='Y' 
        if minr gt 0.01*max(vector.rpol) then $
           polar_contour,fi,vectorsp,ri,levels=[minr],/isotropic,/fill $
                        ,c_colors=0,/overplot
	Colorbar,/bottom,max=maximum,minimum=minimum $
		,ncolors=info.ncont[3],position=[0.08,0.03,0.95,0.11] $
                ,/top,format='(E10.2)'
;		,/top,format='(F7.4)' 
	widget_control,info.timeID,get_value=ctime
	xyouts,10,1,ctime,/device,charsize=1.2,color=!P.color
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.fileID,set_uvalue=vector,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_phicont,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****Contour plot of phi component in selected RZ plane

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.fileID,Get_Uvalue=vector,/No_COpy
if n_tags(vector,/length) eq 0 then begin
	message,'No vector data loaded.',/continue
	widget_control,event.top,Set_Uvalue=info,/NO_Copy
	return
endif
widget_control,/hourglass
nk=vector.nk
ni=vector.ni
nj=vector.nj

;== temporary changes=================================
; vector.vs=vector.vr
; print,'Int(dJ_i*dE*)d^3x=',total(vector.vs)/(ni*nj*nk)
;=====================================================

zero=complexarr(nk)
for k=0,nk-1 do zero(k)=0.
vs1=fltarr(ni,nj,nk)
k=info.nmode
if k ge 0 then begin
  for i=0,ni-1 do begin
  for j=0,nj-1 do begin
	tmp=reform(vector.vs(i,j,*),nk)
	tmp1=FFT(tmp)
	zero(k)=tmp1(k)
	if k gt 0 then zero(nk-k)=tmp1(nk-k)
	tmp=FFT(zero,1)
	vs1(i,j,*)=float(tmp)
  endfor
  endfor
endif else begin
  vs1=vector.vs
endelse

nib=1
njb=1
nii=ni-(ni mod nib)
njj=nj-(nj mod njb)
z2=vector.zpol(0:nii-1)
r2=vector.rpol(0:njj-1)

nphi=round(info.phi[1]*nk/360)
nphi2=(nphi+nk/2) mod nk
z=rebin(z2,nii/nib)
r=rebin(r2,njj/njb)
vs=reform(vs1(*,*,nphi),ni,nj)
vs=rebin(vs(0:nii-1,0:njj-1),nii/nib,njj/njb)
string=string(vector.type,'_phi',', n=',info.nmode,', phi=' $
	,vector.spol(nphi)*180/!pi,format='(A,A, A,i2, A,f5.1)')
maxz=max(z)
maxr=max(r)
maxvs=max(vs)
minvs=min(vs)
svmax=string(maxvs,format='(e10.2)')
svmin=string(minvs,format='(e10.2)')
strvmax='Vmax = '+svmax+';     Vmin= '+svmin
wset,info.wid
if info.retain eq 0 then begin
	loadCT,6,ncolors=!D.n_colors
;	info.bottom=0
;	info.ncolors=!D.n_colors
        loadCT,20,ncolors=info.ncont[3]+1,bottom=0
        loadCT,0,ncolors=info.ncont[3],bottom=1
        info.bottom=1
        info.ncolors=info.ncont[2]
endif
rchar=0.8
if (maxz/maxr) gt 4 then begin
  if info.isotropic[1] eq 0 then begin
    !p.multi=[0,1,3]
    rchar=1.5
  endif
endif
device,decomposed=0
; contour,vs,z,r,/xstyle,/ystyle,title=string $
;	,nlevels=info.ncont[2]/2 $
;	,isotropic=info.isotropic[1] $
;       ,min_value=0.,c_linestyle=0 $
;	,xtitle='Z',ytitle='R',charsize=rchar
; contour,vs,z,r,/xstyle,/ystyle,nlevels=info.ncont[2]/2 $
;       ,isotropic=info.isotropic[1] $
;       ,max_value=0.,c_linestyle=1,charsize=rchar,/noerase
contour,vs,z,r,/xstyle,/ystyle,title=string $
       ,nlevels=info.ncont[2],isotropic=info.isotropic[1] $
	,C_Colors=Indgen(info.ncont[2])+1 $
	,position=[0.15,0.45,0.95,0.75] $
 	,xtitle='Z',ytitle='R',charsize=rchar,/fill
;       ,xtitle='Z',ytitle='R',charsize=rchar
Colorbar,bottom=1,max=max(vs),minimum=min(vs) $
        ,ncolors=info.ncont[2],position=[0.08,0.05,0.95,0.13] $
        ,/top,format='(E10.2)',charsize=rchar
xyouts,10,1,strvmax,/device,charsize=1. 
!p.multi=0
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='phicontour.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
        if info.retain eq 0 then begin
         loadCT,6,ncolors=!D.N_Colors
         loadCT,20,ncolors=info.ncont[2]+1,bottom=0
         loadCT,4,ncolors=info.ncont[2],bottom=1
;        loadCT,0,ncolors=info.ncont[2],bottom=1
        endif
	!P.color=!P.background
;        info.bottom=1
;        info.ncolors=info.ncont[2]

        if (maxz/maxr) gt 4 then begin
          if info.isotropic[1] eq 0 then !p.multi=[0,1,3]
        endif
	contour,vs,z,r,/xstyle,/ystyle,title=string $
		,nlevels=info.ncont[2] $
		,isotropic=info.isotropic[1] $
                ,C_Colors=Indgen(info.ncont[2])+1 $
		,position=[0.15,0.45,0.95,0.75] $
 		,xtitle='Z',ytitle='R' ,charsize=rchar,/fill
;               ,xra=[0,max(z)] 
;		,xtitle='Z',ytitle='R' ,charsize=rchar
        Colorbar,bottom=1,max=max(vs),minimum=min(vs) $
        ,ncolors=info.ncont[2] $
        ,position=[0.08,0.05,0.95,0.13] $
        ,/top,format='(E10.2)',charsize=rchar
	widget_control,info.timeID,get_value=ctime
	totalstring=ctime + ', ' + strvmax
	xyouts,10,1,totalstring,/device,charsize=1.2
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
!p.multi=0
widget_control,info.fileID,set_uvalue=vector,/No_COpy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_zrvect,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Draws vector plot of r and z components at given angle

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.fileID,Get_Uvalue=vector,/NO_Copy
if n_tags(vector,/length) eq 0 then begin
	Message,'No vector data loaded.',/continue
	widget_control,event.top,Set_Uvalue=info,/No_COpy
	return
endif
widget_control,/hourglass
nk=vector.nk
ni=vector.ni
nj=vector.nj
zero=complexarr(nk)
for k=0,nk-1 do zero(k)=0.
k=info.nmode
vz1=fltarr(ni,nj,nk)
vr1=fltarr(ni,nj,nk)
if k ge 0 then begin
  for i=0,ni-1 do begin
  for j=0,nj-1 do begin
	tmp=reform(vector.vz(i,j,*),nk)
	tmp1=FFT(tmp)
	zero(k)=tmp1(k)
	if k gt 0 then zero(nk-k)=tmp1(nk-k)
	tmp=FFT(zero,1)
	vz1(i,j,*)=float(tmp)

	tmp=reform(vector.vr(i,j,*),nk)
	tmp1=FFT(tmp)
	zero(k)=tmp1(k)
	if k gt 0 then zero(nk-k)=tmp1(nk-k)
	tmp=FFT(zero,1)
	vr1(i,j,*)=float(tmp)
  endfor
  endfor
endif else begin
  vz1=vector.vz
  vr1=vector.vr
endelse

; for FRC (info.device=0) this is 20 x 14 grid.
nib=fix(ni/40.)
njb=fix(nj/25.)
if info.device eq 0 then begin
  nib=fix(ni/21.)
  njb=fix(nj/14.)
endif
nii=ni-(ni mod nib)
njj=nj-(nj mod njb)
z=rebin(vector.zpol(0:nii-1),nii/nib)
r=rebin(vector.rpol(0:njj-1),njj/njb)
nphi=round(info.phi[1]*nk/360)
vz=reform(vz1(*,*,nphi))
vr=reform(vr1(*,*,nphi))
vz=rebin(vz(0:nii-1,0:njj-1),nii/nib,njj/njb)
vr=rebin(vr(0:nii-1,0:njj-1),nii/nib,njj/njb)
maxz=max(z)
maxr=max(r)
vamp=sqrt(vz*vz+vr*vr)
vmax=max(vamp)
vmin=min(vamp)
svmax=string(vmax,format='(e10.2)')
svmin=string(vmin,format='(e10.2)')
strvmax='Vmax = '+svmax+';     Vmin= '+svmin
wset,info.wid
string=string(vector.type,'(z, r)',', n=',info.nmode,', phi=' $
	,vector.spol(nphi)*180/!pi,format='(A,A, A,i2, A,f5.1)')
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
rchar=0.8
if (maxz/maxr) gt 4 then begin
  if info.isotropic[1] eq 0 then begin
    !p.multi=[0,1,3]
    rchar=1.5
  endif
endif
velovect,vz,vr,z,r,xtitle='Z',ytitle='R',title=string $
	,/xstyle,/ystyle,length=1.0,isotropic=info.isotropic[1] $
        ,charsize=rchar
xyouts,10,1,strvmax,charsize=1.,/device
!p.multi=0
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='zrvect.ps'
	device,filename=file,xsize=4.5,ysize=4.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors
        if (maxz/maxr) gt 4 then begin
          if info.isotropic[1] eq 0 then !p.multi=[0,1,3]
        endif
	velovect,vz,vr,z,r,xtitle='Z',ytitle='R',title=string $
		,/xstyle,/ystyle,length=1.0 $
                ,isotropic=info.isotropic[1],charsize=rchar
	widget_control,info.timeID,get_value=ctime
	totalstring=ctime+', '+strvmax
	xyouts,10,1,totalstring,/device,charsize=1.2,color=0
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
!p.multi=0
widget_control,info.fileID,set_uvalue=vector,/NO_COpy
widget_control,event.top,Set_Uvalue=info,/No_COpy
END

;#####################################################################
PRO wdata_load1,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***This module loads 1 frame of a movie data file*****

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
filename=DIALOG_PICKFILE(filter='*.dat')
if strlen(filename) eq 0 then begin
	message,'You must choose a file to open.',/continue
	if n_tags(scalar,/length) GT 0 then $
		widget_control,info.quitID,set_Uvalue=scalar,/No_Copy
	widget_control,event.top,set_uvalue=info,/No_Copy
	return	
endif
widget_control,/hourglass
openr,lun,filename,/Get_Lun
readf,lun,ni,nj,nk,nl
close,lun
free_lun,lun

zpol=fltarr(ni)
rpol=fltarr(nj)
spol=fltarr(nk)
t=fltarr(nl)
ppol=fltarr(ni,nj,nk,nl)
psave=fltarr(ni,nj,nk,nl)
openr,lun,filename,/Get_Lun
readf,lun,ni,nj,nk,nl,zpol,rpol,spol,t,ppol
close,lun
Free_Lun,lun

baseID=event.top
if NOT finite(info.framebaseID) then begin
        framebaseID=widget_base(baseID,/column,/frame)
	info.frameID=cw_field(framebaseID,/column $
		,value=0,/integer,title='Frame to Load:')
endif
string1='The frame field below the graphics window indicates the current'
string2='value of Frame to Load.  To load a different one, insert your choice'
string3='in place of the value currently displayed, and select the Load'
string4='1 Frame of Movie button again.  Value entered should'
string4=string('range from 0 to',nl-1)
;junk=DIALOG_MESSAGE([string1,string2,string3,string4] $
;	,title='Note:')
widget_control,info.frameID,Get_Value=framen
print,'framen=',framen
if ((framen LT 0) OR (framen GE nl)) then begin
	message,'Inappropriate selection.',/continue
	if n_tags(scalar,/length) GT 0 then $
		widget_control,info.quitID,set_Uvalue=scalar,/No_Copy
	widget_control,event.top,set_Uvalue=info
	return
endif
p=reform(ppol(*,*,*,framen))
time=t(framen)
ctime=string('t=',time)
scalar1={ni:ni, $
	nj:nj, $
	nk:nk, $
	ctime:ctime, $
	zpol:zpol, $
	rpol:rpol, $
	spol:spol, $
	ppol:p, $
        psave:psave,  $
	num:!Values.F_NaN, $
	type:'Pressure'}	
if FINITE(info.isovalueID) then begin
	junkstring=string('Isosurface at ',info.iso*max(p) $
		,format='(A,F5.3)')
	widget_control,info.isovalueID,set_value=junkstring
endif
if info.mode eq 0 then widget_control,info.timeID,set_value=ctime
widget_control,info.quitID,set_uvalue=scalar1,/No_Copy
widget_control,event.top,set_Uvalue=infonew,/No_Copy
END

;#####################################################################
PRO wdata_loadnew,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*******This module loads a new data file********

widget_control,event.top,Get_Uvalue=info,/No_Copy
filename=DIALOG_PICKFILE(filter='*.dat')
if strlen(filename) eq 0 then begin
	message,'You must choose a file to open.',/continue
	widget_control,event.top,Set_Uvalue=info,/No_Copy
	return
endif
Widget_Control,/hourglass
modestring=''
openr,lun,filename,/Get_Lun
readf,lun,modestring
close,lun
Free_Lun,lun

CASE modestring OF
	' scalar' : BEGIN
	widget_control,info.quitID,get_uvalue=trash,/No_Copy
	ctime=''
	type=''
	type0=''
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,type
	readf,lun,ctime
	readf,lun,ni,nj,nk
	close,lun
	Free_Lun,lun
	zpol=fltarr(ni)
	rpol=fltarr(nj)
	spol=fltarr(nk)
	ppol=fltarr(ni,nj,nk)
	psave=fltarr(ni,nj,nk)
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,type
	readf,lun,ctime
	readf,lun,ni,nj,nk,zpol,rpol,spol,ppol
	type0=type
	psave=ppol
	close,lun
	Free_Lun,lun	
	scalar={type:type, $
		type0:type0, $
		ctime:ctime, $
		ni:ni, $
		nj:nj, $
		nk:nk, $
		zpol:zpol, $
		rpol:rpol, $
		spol:spol, $
		ppol:ppol, $
		psave:psave,     $
		num:!Values.F_NaN}
	if finite(info.isovalueID) then begin
		junkstring=string('Isosurface at ',info.iso*max(ppol) $
			,format='(A,F5.3)') 
		widget_control,info.isovalueID,set_value=junkstring
	endif
	if info.mode eq 0 then widget_control,info.timeID,set_value=ctime
	widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
	END
	' vector' : BEGIN
	widget_control,info.fileID,get_uvalue=junk,/No_Copy
	type=''
	type0=''
	ctime=''
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,type
	readf,lun,ctime
	readf,lun,ni,nj,nk
	close,lun
	Free_Lun,lun
	zpol=fltarr(ni)
	rpol=fltarr(nj)
	spol=fltarr(nk)
	vz=fltarr(ni,nj,nk)
	vr=fltarr(ni,nj,nk)
	vs=fltarr(ni,nj,nk)
        vz0=fltarr(ni,nj,nk)
        vr0=fltarr(ni,nj,nk)
        vs0=fltarr(ni,nj,nk)
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,type
	readf,lun,ctime
	readf,lun,ni,nj,nk,zpol,rpol,spol,vz,vr,vs
	type0=type
	vz0=vz
	vr0=vr
	vs0=vs
	vector={type:type, $
		type0:type0, $
		ctime:ctime, $
		ni:ni, $
		nj:nj, $
		nk:nk, $
		zpol:zpol, $
		rpol:rpol, $
		spol:spol, $
		vz0:vz0, $
                vr0:vr0, $
                vs0:vs0, $
		vz:vz, $
		vr:vr, $
		vs:vs}
	if info.mode eq 1 then widget_control,info.timeID,set_value=ctime
	widget_control,info.fileID,set_uvalue=vector,/No_Copy
	END
	' particle' : BEGIN
	widget_control,info.convertID,get_uvalue=trash,/No_Copy
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,np
	close,lun
	Free_Lun,lun
	z=fltarr(np)
	y=fltarr(np)
	x=fltarr(np)
	vz=fltarr(np)
	vy=fltarr(np)
	vx=fltarr(np)
	p=fltarr(np)
	w=fltarr(np)
	openr,lun,filename,/get_lun
	readf,lun,modestring
	readf,lun,np,vth,x0,y0,xlen,ylen,zlen,z,y,x,vz,vy,vx,p,w
	particle={np:np, $
		vth:vth, $
		x0:x0, $
		y0:y0, $
		xlen:xlen, $
		ylen:ylen, $
		zlen:zlen, $
		z:z, $
		y:y, $
		x:x, $
		vz:vz, $
		vy:vy, $
		vx:vx, $
		p:p, $
		w:w}
	widget_control,info.convertID,set_uvalue=particle,/No_Copy
	END
	'magnetic' : BEGIN
	widget_control,info.colorID,get_uvalue=trash,/No_Copy
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,ni,nj,nk
	close,lun
	Free_Lun,lun
	zgrid=fltarr(ni)
	rgrid=fltarr(nj)
	sgrid=fltarr(nk)
	bzgrid=fltarr(ni,nj,nk)
	brgrid=fltarr(ni,nj,nk)
	bsgrid=fltarr(ni,nj,nk)
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,ni,nj,nk,zgrid,rgrid,sgrid,bzgrid,brgrid,bsgrid
	close,lun
	Free_Lun,lun

	ninew=fix(ni*2/3)
	njnew=fix(nj*2/3)
	zgrid=congrid(zgrid,ninew,/interp)
	rgrid=congrid(rgrid,njnew,/interp)
	bzgrid=congrid(bzgrid,ninew,njnew,nk)
	brgrid=congrid(brgrid,ninew,njnew,nk)
	bsgrid=congrid(bsgrid,ninew,njnew,nk)
	num=njnew*2-1
	mr=max(rgrid)
	interval=mr/(njnew-1)
	x=fltarr(num)
	y=fltarr(num)
	bz=fltarr(num,num,ninew)
	br=fltarr(num,num,ninew)
	bs=fltarr(num,num,ninew)
	for i=0,num-1 do begin
		x[i]=-1*mr+i*interval
		y[i]=-1*mr+i*interval
	endfor
	x=x+mr
	y=y+mr
	myspacing=[interval,interval]
	mybounds=[-1*mr,-1*mr,mr,mr]
	for i=0,ninew-1 do begin
		bzgridi=reform(bzgrid(i,*,*))
		bzi=polar_surface(bzgridi,rgrid,sgrid,/grid $
			,spacing=myspacing,bounds=mybounds)
		bz(*,*,i)=reform(bzi,num,num,1)
		brgridi=reform(brgrid(i,*,*))
		bri=polar_surface(brgridi,rgrid,sgrid,/grid $
			,spacing=myspacing,bounds=mybounds)
		br(*,*,i)=reform(bri,num,num,1)
		bsgridi=reform(bsgrid(i,*,*))
		bsi=polar_surface(bsgridi,rgrid,sgrid,/grid $
			,spacing=myspacing,bounds=mybounds)
		bs(*,*,i)=reform(bsi,num,num,1)
	endfor	
	magnetic={num:num, $
		ninew:ninew, $
		x:x, $
		y:y, $
		z:zgrid, $
		bz:bz, $
		br:br, $
		bs:bs, $
		interval:interval}
	widget_control,info.colorID,set_uvalue=magnetic,/No_Copy
	END
	' equilibrium' : BEGIN
	widget_control,info.ctableID,get_uvalue=trash,/No_Copy
	openr,lun,filename,/Get_lun
	readf,lun,modestring
	readf,lun,ni,nj
	close,lun
	free_lun,lun
	z=fltarr(ni)
	r=fltarr(nj)
	psi=fltarr(ni,nj)
	p=fltarr(ni,nj)
	bz=fltarr(ni,nj)
	br=fltarr(ni,nj)
        bphi=fltarr(ni,nj)
	rho=fltarr(ni,nj)
	ji=fltarr(ni,nj)
	vel=fltarr(ni,nj)
        curz=fltarr(ni,nj)
        curr=fltarr(ni,nj)
	cur=fltarr(ni,nj)
        jiz=fltarr(ni,nj)
        jir=fltarr(ni,nj)
        g=fltarr(ni,nj)
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,ni,nj,z,r,psi,p,bz,br,bphi,rho,ji,vel,curz,curr,cur
	close,lun
	free_lun,lun
        if info.device eq 1 then begin
            openr,lun,'ji.dat',/Get_Lun
            readf,lun,ni1,nj1
            close,lun
            free_lun,lun
            if ni1 ne ni or nj1 ne nj then begin
                print,"wrong ji.dat file"
                stop
            endif
            openr,lun,'ji.dat',/Get_Lun
            readf,lun,ni1,nj1,rho,jiz,jir,ji,g
            close,lun
            free_lun,lun
        endif
	equilibrium={ni:ni, $
			nj:nj, $
			z:z, $
			r:r, $
			psi:psi, $
			p:p, $
			bz:bz, $
			br:br, $
                        bphi:bphi, $
			rho:rho, $
			ji:ji, $
			vel:vel, $
                        curz:curz, $
                        curr:curr, $
			cur:cur, $
                        jiz:jiz, $
                        jir:jir, $
                        g:g }
	widget_control,info.ctableID,set_uvalue=equilibrium,/No_COpy
	END
	' rhoi' : BEGIN
	widget_control,info.printID,get_uvalue=trash,/No_Copy
	openr,lun,filename,/Get_Lun
	readf,lun,modestring
	readf,lun,ni,nj
	close,lun
	Free_lun,lun
	z=fltarr(ni)
	r=fltarr(nj)
	rhoi=fltarr(ni,nj)
	ji=fltarr(ni,nj)
        jiz=fltarr(ni,nj)
        jir=fltarr(ni,nj)
	openr,lun,filename,/Get_lun
	readf,lun,modestring
	readf,lun,ni,nj,z,r,rhoi,ji
        if info.device eq 1 then $
                            readf,lun,jiz,jir
	close,lun
	Free_Lun,lun
	rhoi={ 	ni:ni, $
		nj:nj, $
		z:z, $
		r:r, $
		rhoi:rhoi, $
		ji:ji, $
                jiz:jiz, $
                jir:jir }
	widget_control,info.printID,set_uvalue=rhoi,/No_Copy
	END
	ELSE : BEGIN
	string1='Error: the selected file is not in the correct'
	string2='format.  Please try a different file, or edit'
	string3='this one and try again.'
	junk=DIALOG_MESSAGE([string1,string2,string3])
	widget_control,event.top,set_uvalue=info,/No_Copy
	return	
	END
ENDCASE
message,'New data loaded.',/continue
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_isotropic,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*******This module changes the display type

widget_control,event.top,Get_Uvalue=info,/no_copy
info.isotropic[where(info.isotropicID eq event.id)]=event.index
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_deltas,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*******This module changes Scalar to delta_Scalar and back 
;********(ie subrtracts n=0 part).

widget_control,event.top,Get_Uvalue=info,/no_copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
if n_tags(scalar,/length) eq 0 then begin
        message,'No scalar data loaded.',/continue
	widget_control,info.deltasID,set_droplist_select=info.deltas
        widget_control,event.top,Set_Uvalue=info,/No_COpy
        return
endif

info.deltas=event.index
if info.deltas eq 0 then begin
	scalar.ppol= scalar.psave
	scalar.type= scalar.type0
endif else begin
	p0= total(scalar.psave,3)/scalar.nk
endelse

if info.deltas eq 1  then begin
	for k=0,scalar.nk-1 do $
          scalar.ppol(*,*,k)= scalar.psave(*,*,k) - p0
	scalar.type=string('delta_',scalar.type0)
endif

if info.deltas eq 2  then begin
        for k=0,scalar.nk-1 do $
          scalar.ppol(*,*,k)= p0
        scalar.type=string(scalar.type0,'_n=0')
endif

if info.deltas eq 3  then begin
        for k=0,scalar.nk-1 do $
          scalar.ppol(*,*,k)= scalar.psave(*,*,k) - p0
        a2= total((scalar.ppol)^2,3)/scalar.nk
        for k=0,scalar.nk-1 do $
          scalar.ppol(*,*,k)= sqrt(2*a2)
        scalar.type=string('|delta_',scalar.type0,'|')
endif

widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_v2s,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*******This module changes Vector component type to Scalar type, so
;*******it can be plotted using scalar menus.

widget_control,event.top,Get_Uvalue=info,/no_copy
widget_control,info.fileID,Get_Uvalue=vector,/No_Copy
if n_tags(vector,/length) eq 0 then begin
        message,'No vector data loaded.',/continue
        widget_control,info.v2sID,set_droplist_select=info.v2s
        widget_control,event.top,Set_Uvalue=info,/No_COpy
        return
endif

info.v2s=event.index

if info.v2s gt 0 then begin
  widget_control,info.quitID,get_uvalue=scalar,/NO_Copy
  CASE info.v2s OF
        1 : BEGIN
        array=vector.vz
        choice='V_z'
        END
        2 : BEGIN
        array=vector.vr
        choice='V_r'
        END
        3 : BEGIN
        array=vector.vs
        choice='V_phi'
        END
	4 : BEGIN
	array=sqrt( vector.vz^2 + vector.vr^2 + vector.vs^2 + 1e-16)
	choice='V_tot'
	END
	5 : BEGIN
	array=vector.vz
	choice='psi'
	END
	6 : BEGIN
	array=vector.vs
	choice='V_pll'
	END
        7 : BEGIN
        array=vector.vs
        choice='|V_perp|'
        END
  ENDCASE
  nk=vector.nk
  ni=vector.ni
  nj=vector.nj
  zero=complexarr(nk)
  for k=0,nk-1 do zero(k)=0.
  vs1=fltarr(ni,nj,nk)
  vs2=fltarr(ni,nj,nk)
  k=info.nmode
  if k ge 0 then begin
    for i=0,ni-1 do begin
    for j=0,nj-1 do begin
        tmp=reform(array(i,j,*),nk)
        tmp1=FFT(tmp)
        zero(k)=tmp1(k)
	if k gt 0 then zero(nk-k)=tmp1(nk-k)
        tmp=FFT(zero,1)
        vs1(i,j,*)=float(tmp)
    endfor
    endfor
  endif else begin
    vs1=array
  endelse
;; For option 'v_tot' do FFT for each component.
  zero(*)=0.
  if k ge 0 then begin
    if info.v2s eq 4 then begin
      vs2(*,*,*)= 0.
      for nf=1,3 do begin
        if(nf eq 1) then array= vector.vz
        if(nf eq 2) then array= vector.vr
        if(nf eq 3) then array= vector.vs
          for i=0,ni-1 do begin
          for j=0,nj-1 do begin
            tmp=reform(array(i,j,*),nk)
            tmp1=FFT(tmp)
            zero(k)=tmp1(k)
	    if k gt 0 then zero(nk-k)=tmp1(nk-k)
            tmp=FFT(zero,1)
            vs1(i,j,*)=float(tmp)
          endfor
          endfor
          vs2= vs2 + vs1^2
      endfor
      vs1= sqrt(vs2 + 1e-18)
    endif
  endif
;; For option 'psi' calculate poloidal flux by integrating (-r*B_z) in r.
; Assumes axisymmetric field.
  if info.v2s eq 5 then begin
    for i=0,ni-1 do begin
      k=3
      tmp=-vector.rpol(*)*reform(vs1(i,*,k),nj)
      array(i,0,k)=0.
      for j=1,nj-1 do begin
        array(i,j,k)=int_tabulated(vector.rpol(0:j),tmp(0:j))
      endfor
    endfor
    for k=0,nk-1 do begin
    vs1(*,*,k)=array(*,*,3)
    endfor
  endif
;; For option 'V_pll'  calculate parallel projection to V_n=0.
  zero(*)=0.
  if info.v2s eq 6 or info.v2s eq 7 then begin
     b01=fltarr(ni,nj,nk,3)
     vs2(*,*,*)= 0.
     for nf=1,3 do begin
       if(nf eq 1) then array= vector.vz
       if(nf eq 2) then array= vector.vr
       if(nf eq 3) then array= vector.vs
          for i=0,ni-1 do begin
          for j=0,nj-1 do begin
            tmp=reform(array(i,j,*),nk)
            tmp1=FFT(tmp)
            zero(0)=tmp1(0)
            tmp=FFT(zero,1)
            vs1(i,j,*)=float(tmp)
          endfor
          endfor
          vs2= vs2 + vs1^2
          b01(*,*,*,nf-1)=vs1
     endfor
     vs1= sqrt(vs2 + 1e-18)
     for nf=0,2 do b01(*,*,*,nf)= b01(*,*,*,nf)/vs1      ;b0^=B0/B0
     bamp= vs1
     ;
     k=info.nmode
     zero(*)=0.
     if k ge 0 then begin
       deltav=fltarr(ni,nj,nk,3)
       for nf=1,3 do begin
         if(nf eq 1) then array= vector.vz
         if(nf eq 2) then array= vector.vr
         if(nf eq 3) then array= vector.vs
         for i=0,ni-1 do begin
         for j=0,nj-1 do begin
            tmp=reform(array(i,j,*),nk)
            tmp1=FFT(tmp)
            zero(k)=tmp1(k)
	    if k gt 0 then zero(nk-k)=tmp1(nk-k)
            tmp=FFT(zero,1)
            vs1(i,j,*)=float(tmp)
         endfor
         endfor
         deltav(*,*,*,nf-1)=vs1
       endfor
       for i=0,ni-1 do begin
         for j=0,nj-1 do begin
           tmp1= deltav(i,j,*,0)*b01(i,j,*,0) $
                +deltav(i,j,*,1)*b01(i,j,*,1) $
                +deltav(i,j,*,2)*b01(i,j,*,2)
           vs1(i,j,*)= reform(tmp1,nk)
         endfor
       endfor
     endif else begin
      vs1= (vector.vz-b01(*,*,*,0)*bamp)*b01(*,*,*,0) $
         + (vector.vr-b01(*,*,*,1)*bamp)*b01(*,*,*,1) $
         + (vector.vs-b01(*,*,*,2)*bamp)*b01(*,*,*,2)
     endelse
     if info.v2s eq 7 then begin          ; Calculate |dB_perp|
       vpll2= vs1^2
       zero(*)=0.
       if k ge 0 then begin
        vs2(*,*,*)= 0.
        for nf=1,3 do begin
          if(nf eq 1) then array= vector.vz
          if(nf eq 2) then array= vector.vr
          if(nf eq 3) then array= vector.vs
            for i=0,ni-1 do begin
            for j=0,nj-1 do begin
              tmp=reform(array(i,j,*),nk)
              tmp1=FFT(tmp)
              zero(k)=tmp1(k)
              if k gt 0 then zero(nk-k)=tmp1(nk-k)
              tmp=FFT(zero,1)
              vs1(i,j,*)=float(tmp)
            endfor
            endfor
            vs2= vs2 + vs1^2
        endfor
        vs1= sqrt(abs(vs2 - vpll2))
       endif
     endif

  endif
;=======================================
; Define scalar structure.
	scalar={type:choice, $
                type0:choice, $
                ctime:vector.ctime, $
                ni:vector.ni, $
                nj:vector.nj, $
                nk:vector.nk, $
                zpol:vector.zpol, $
                rpol:vector.rpol, $
                spol:vector.spol, $
                ppol:vs1, $
                psave:array,     $
                num:!Values.F_NaN}
   if info.mode eq 0 then widget_control,info.timeID,set_value=ctime
   message,'New scalar data loaded.',/continue
   widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
endif
widget_control,info.fileID,set_uvalue=vector,/NO_Copy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_rzcontour,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;******This module displays the rz-contour plots*******

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
;widget_control,event.top,get_uvalue=info,/No_COpy
if n_tags(scalar,/length) eq 0 then begin
	message,'No scalar data loaded.',/continue
	widget_control,event.top,Set_Uvalue=info,/No_COpy
	return
endif
widget_control,info.ctableID,get_uvalue=equ,/No_COpy
;if n_tags(equ,/length) eq 0 then begin
;        message,'No equilibrium data loaded.',/continue
;        widget_control,event.top,set_uvalue=info,/NO_COpy
;        return
;endif

angler=info.phi[0]*!pi/180
find1=round(angler/(2*!pi/scalar.nk))
;== change scalar.ppol temporary to n=0 ===========================
;	p0= total(scalar.ppol,3)/scalar.nk
;        for k=0,scalar.nk-1 do $
;          scalar.ppol(*,*,k)= p0
;	print,'Int(dJ_i*dE*)d^3x=',total(p0)/(scalar.ni*scalar.nj)
;==========================================================
pslice1=reform(scalar.ppol(*,*,find1),scalar.ni,scalar.nj)
find2=(find1+scalar.nk/2) mod scalar.nk
pslice2=reverse(reform(scalar.ppol(*,*,find2),scalar.ni,scalar.nj),2)
p=fltarr(scalar.ni,2*scalar.nj-1)
p(*,0:scalar.nj-1)=pslice2
p(*,scalar.nj:scalar.nj*2-2)=pslice1(*,1:scalar.nj-1)
vals=fltarr(scalar.nj*2-1)
vals(0:scalar.nj-1)=-1*reverse(scalar.rpol)
vals(scalar.nj:scalar.nj*2-2)=scalar.rpol(1:scalar.nj-1)
if info.scale[0] eq 0 then begin
	maximum=max(scalar.ppol)
	minimum=min(scalar.ppol)
endif else begin
	maximum=max(p)
	minimum=min(p)
endelse
step=(maximum-minimum)/(info.ncont[1]-1)
levels=IndGen(info.ncont[1])*step+minimum
if levels(1) eq levels(0) then begin
	message,'Cannot produce contour plot at this angle',/continue
	widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif
string=string(scalar.type,' at phi=',scalar.spol(find1)*180/!pi)
Wset,info.wid

if info.retain eq 0 then begin
	loadCT,6,ncolors=!D.N_Colors
	loadCT,20,ncolors=info.ncont[1]+1,bottom=0
	loadCT,33,ncolors=info.ncont[1],/bottom
	info.bottom=1
	info.ncolors=info.ncont[1]
endif

device,decomposed=0
yrange=[-max(scalar.rpol),max(scalar.rpol)]
if min(scalar.rpol) gt 0.0 then yrange=[min(scalar.rpol),max(scalar.rpol)]
; rescale
ninew=round(scalar.ni/2)
zbin=congrid(scalar.zpol,ninew,/interp)
pnew=congrid(p,ninew,2*scalar.nj-1)
p=pnew

if info.device eq 0 then begin
  contour,p,zbin,vals,/xstyle,/ystyle,title=string,/fill $
 	,Levels=levels,C_Colors=IndGen(info.ncont[1])+1 $
 	,isotropic=info.isotropic[0],position=[0.15,0.25,0.95,0.95] $
 	,xtitle='Z',ytitle='R',yrange=yrange 
 Colorbar,bottom=1,max=maximum,minimum=minimum $
        ,ncolors=info.ncont[1],position=[0.08,0.03,0.95,0.11] $
         ,/top,format='(E10.2)'
endif
if info.device eq 1 then begin
; transpose p and psi
  psl=fltarr(scalar.nj,scalar.ni)
  for i=0,scalar.ni-1 do  psl(*,i)= pslice1(i,*)
  contour,psl,scalar.rpol,scalar.zpol,/xstyle,/ystyle,title=string,/fill $
        ,Levels=levels,C_Colors=IndGen(info.ncont[1])+1 $
        ,isotropic=info.isotropic[0],position=[0.15,0.15,0.55,0.85] $
        ,xtitle='R',ytitle='Z'

  psi=fltarr(equ.nj,equ.ni)
  for i=0,equ.ni-1 do  psi(*,i)= equ.psi(i,*)
  ind=where(abs(equ.z) gt 30.,count)
  psi(*,ind)=1.
  contour,psi,equ.r,equ.z,/xstyle,/ystyle,title='',levels=[0.05*min(psi)] $
       ,isotropic=info.isotropic[0],position=[0.15,0.15,0.55,0.85] $
       ,xtitle='',ytitle='',c_colors=100,/noerase

  Colorbar,/vert, max=maximum,minimum=minimum $
         ,ncolors=info.ncont[1],position=[0.56,0.15,0.59,0.85] $
         ,/right,format='(E9.2)'
endif

; TMP save 2d data
;file='scalar_2d.dat'
;openw,1,file
;printf,1,scalar.ni,scalar.nj
;printf,1,scalar.zpol,scalar.rpol
;printf,1,pslice1
;close,1

;TMP plot psi contours
;ind= where(levels le 0)
;contour,p,zbin,vals,/xstyle,/ystyle,title=string,Levels=levels(ind) $
;	,isotropic=info.isotropic[0],position=[0.15,0.25,0.95,0.95] $	
;	,xtitle='Z',ytitle='R',yrange=yrange

print,'Max/Min=',max(p),min(p)
;print,levels
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='rzcont.ps'
	device,filename=file,xsize=3.5,ysize=4.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then begin
		loadCT,6,ncolors=!D.N_Colors
		loadCT,20,ncolors=info.ncont[1]+1,bottom=0
		loadCT,33,ncolors=info.ncont[1],/bottom
	endif
	!P.color=!P.background
        rchar=0.8
        if info.device eq 0 then begin
          contour,p,zbin,vals,/xstyle,/ystyle,title=string,/fill $
            ,Levels=levels,C_Colors=IndGen(info.ncont[1])+1 $
            ,isotropic=info.isotropic[0],position=[0.15,0.25,0.95,0.95] $
            ,xtitle='Z',ytitle='R',yrange=yrangea,charsize=rchar
          Colorbar,bottom=1,max=maximum,minimum=minimum $
            ,ncolors=info.ncont[1],position=[0.08,0.03,0.95,0.11] $
            ,/top,format='(E10.2)',charsize=rchar
        endif
        if info.device eq 1 then begin
          contour,psl,scalar.rpol,scalar.zpol,/xstyle,/ystyle,title=string,/fill $
            ,Levels=levels,C_Colors=IndGen(info.ncont[1])+1 $
            ,isotropic=info.isotropic[0],position=[0.15,0.15,0.55,0.85] $
            ,xtitle='R',ytitle='Z',charsize=rchar

          contour,psi,equ.r,equ.z,/xstyle,/ystyle,title='',levels=[0.05*min(psi)] $
            ,isotropic=info.isotropic[0],position=[0.15,0.15,0.55,0.85] $
            ,xtitle='',ytitle='',c_colors=100,charsize=rchar,/noerase

          Colorbar,/vert, max=maximum,minimum=minimum $
            ,ncolors=info.ncont[1],position=[0.56,0.15,0.59,0.85] $
            ,/right,format='(E9.2)',charsize=rchar
        endif
	widget_control,info.timeID,get_value=ctime
	xyouts,10,1,ctime,/device,charsize=1.2,color=!P.color
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
widget_control,info.ctableID,set_uvalue=equ,/No_Copy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_rz1dslice,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;******This module displays 1D Z (R) slice plots*******

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
if n_tags(scalar,/length) eq 0 then begin
        message,'No scalar data loaded.',/continue
        widget_control,event.top,Set_Uvalue=info,/No_COpy
        return
endif
angler=info.phi[0]*!pi/180
value=info.rzvalue[0]/100.
find1=round(angler/(2*!pi/scalar.nk))
pslice=reform(scalar.ppol(*,*,find1),scalar.ni,scalar.nj)
pmax=max(pslice)
pmin=min(pslice)

; Do Z-slice or R-slice.

if info.rzslice[0] eq 0 then begin
    zvalue=value*(max(scalar.zpol)-min(scalar.zpol))+min(scalar.zpol)
    zinterval=(max(scalar.zpol)-min(scalar.zpol))/(scalar.ni-1)
    index=(zvalue-min(scalar.zpol))/zinterval
    p=reform(pslice(index,*),scalar.nj)
    xp=scalar.rpol
    nxp=scalar.nj
    xtitle='R'
    title=string(scalar.type,'(R) at Z=',scalar.zpol(index) $
      ,'  phi=',scalar.spol(find1)*180/!pi,format='(A,A,F7.2,A,F6.2)')
endif else begin
    rvalue=value*(max(scalar.rpol)-min(scalar.rpol))+min(scalar.rpol)
    rinterval=(max(scalar.rpol)-min(scalar.rpol))/(scalar.nj-1)
    index=(rvalue-min(scalar.rpol))/rinterval
    p=reform(pslice(*,index),scalar.ni)
    xp=scalar.zpol
    nxp=scalar.ni
    xtitle='Z'
    title=string(scalar.type,'(Z) at R=',scalar.rpol(index) $
       ,'  phi=',scalar.spol(find1)*180/!pi,format='(A,A,F7.2,A,F6.2)')
endelse

xrange=[min(xp),max(xp)]
yrange=[pmin,pmax]

plot,xp,p,xrange=xrange,/xstyle,yrange=yrange,/ystyle $
        ,xtitle=xtitle,ytitle=scalar.type,title=title

; TMP save 1d data
;file='scalar_1d.dat'
;openw,1,file
;printf,1,nxp
;printf,1,xp,p
;close,1


if info.printnext GT 0 then begin
        thisdevice=!D.Name
        set_plot,'PS',copy=info.retain
        file='1dslice.ps'
        device,filename=file,xsize=6.5,ysize=6.5,/inches,xoffset=0.75 $
                ,yoffset=2.0,/color
	if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors
        plot,xp,p,xrange=xrange,/xstyle,yrange=yrange,/ystyle $
               ,xtitle=xtitle,ytitle=scalar.type,title=title 
        widget_control,info.timeID,get_value=ctime
        xyouts,10,1,ctime,/device,charsize=1.2
        device,/close_file
        set_plot,thisdevice
        if info.printnext eq 2 then begin
                spawn,string('lpr -Pl230-t850 ',file)
                spawn,string('rm ',file)
                message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
        info.printnext=0
       ;TMP print 1D data 
        print,'x data'
        print,xp
        print,'1D scalar data'
        print,p
endif

widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_ccontour1,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;******This module displays circular polar contours********

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.quitID,get_uvalue=scalar,/No_Copy
if n_tags(scalar,/length) eq 0 then begin
	message,'No scalar data loaded.',/continue
	widget_control,event.top,Set_Uvalue=info,/No_Copy
	return
endif
zvalue=(info.z[0])*(max(scalar.zpol)-min(scalar.zpol))/100+min(scalar.zpol)
zinterval=(max(scalar.zpol)-min(scalar.zpol))/(scalar.ni-1)
index=(zvalue-min(scalar.zpol))/zinterval
if NOT info.cinterp then begin
	index=round(index)
	slice=reform(scalar.ppol(index,*,*))
	string=string(scalar.type,' contour at z=',scalar.zpol(index))
endif else begin
	slice1=reform(scalar.ppol(floor(index),*,*))
	slice2=reform(scalar.ppol(ceil(index),*,*))
	slice=slice1*(scalar.zpol(ceil(index))-zvalue)/zinterval + $
		slice2*(zvalue-scalar.zpol(floor(index)))/zinterval	
	string=string(scalar.type,' contour at z=',zvalue)
endelse
if NOT info.scale[1] then begin
	maximum=max(scalar.ppol)
	minimum=min(scalar.ppol)
endif else begin
	maximum=max(slice)
	minimum=min(slice)
endelse
step=(maximum-minimum)/(info.ncont[0]-1)
levels=IndGen(info.ncont[0])*step+minimum
if levels(1) eq levels (0) then begin
	message,'Cannot create contour plot at this Z',/continue
	widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
	widget_control,event.top,Set_Uvalue=info,/No_Copy
	return
endif
Wset,info.wid
if info.retain eq 0 then begin
	loadCT,6,ncolors=!D.N_COlors
	loadCT,20,ncolors=info.ncont[0]+1,bottom=0
	loadCT,33,ncolors=info.ncont[0],/bottom
	info.bottom=1
	info.ncolors=info.ncont[0]
endif
device,decomposed=0
polar_contour,transpose(slice),scalar.spol,scalar.rpol,/xstyle,/ystyle $
	,title=string,/fill,/isotropic,Levels=levels $
	,C_Colors=IndGen(info.ncont[0])+1,position=[0.15,0.25,0.95,0.95] $
	,xtitle='X',ytitle='Y'
; for min(r)>0
minr=min(scalar.rpol)
if minr gt 0.01*max(scalar.rpol) then begin
  ri=findgen(20)/19.*minr*1.0
  fi=fltarr(scalar.nk,20)
  for k=0,scalar.nk-1 do fi(k,*)=ri
  polar_contour,fi,scalar.spol,ri,levels=[minr],/isotropic,/fill,c_colors=0 $
             ,/overplot
endif

Colorbar,bottom=1,max=maximum,minimum=minimum $
	,ncolors=info.ncont[0],position=[0.08,0.03,0.95,0.11] $
        ,/top,format='(E10.2)'
;	,/top,format='(F7.4)' 
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='polarcont.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then begin
		loadCT,6,ncolors=!D.N_COlors
		loadCT,20,ncolors=info.ncont[0]+1,bottom=0
		loadCT,33,ncolors=info.ncont[0],/bottom
	endif
	!P.color=!P.background
	polar_contour,transpose(slice),scalar.spol,scalar.rpol,/xstyle,/ystyle $
		,title=string,/fill,/isotropic,Levels=levels $
		,C_Colors=IndGen(info.ncont[0])+1,position=[0.15,0.25,0.95,0.95] $
		,xtitle='X',ytitle='Y'
        if minr gt 0.01*max(scalar.rpol) then $
           polar_contour,fi,scalar.spol,ri,levels=[minr],/isotropic,/fill $
                     ,c_colors=0,/overplot

	Colorbar,bottom=1,max=maximum,minimum=minimum $
		,ncolors=info.ncont[0],position=[0.08,0.03,0.95,0.11] $
                ,/top,format='(E10.2)'
;		,/top,format='(F7.4)' 
	widget_control,info.timeID,get_value=ctime
	xyouts,10,1,ctime,/device,charsize=1.2,color=!P.color
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.quitID,set_uvalue=scalar,/No_Copy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_ccontour2,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;******This module displays circular Cartesian contours*****

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
if n_tags(scalar,/length) eq 0 then begin
	message,'No scalar data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/NO_Copy
	return
endif
if NOT finite(scalar.num) then begin
	Message,'You must convert to Cartesian coordinates first.',/continue
	widget_control,info.quitID,set_uvalue=scalar,/No_COpy
	widget_control,event.top,Set_Uvalue=info,/No_Copy
	Return
endif
zvalue=(info.z[0])*(max(scalar.z)-min(scalar.z))/100+min(scalar.z)
zinterval=(max(scalar.z)-min(scalar.z))/(scalar.ninew-1)
index=(zvalue-min(scalar.z))/zinterval
if NOT info.cinterp then begin
	index=round(index)
	slice=reform(scalar.pxyz(*,*,index))
	string=string(scalar.type,' contour at z=',scalar.z(index))
endif else begin
	slice1=reform(scalar.pxyz(*,*,floor(index)))
	slice2=reform(scalar.pxyz(*,*,ceil(index)))
	slice=slice1*(scalar.z(ceil(index))-zvalue)/zinterval + $
		slice2*(zvalue-scalar.z(floor(index)))/zinterval	
	string=string(scalar.type, ' contour at z=',zvalue)
endelse
if NOT info.scale[1] then begin
	maximum=max(scalar.pxyz)
	minimum=min(scalar.pxyz)
endif else begin
	maximum=max(slice)
	minimum=min(slice)
endelse
step=(maximum-minimum)/(info.ncont[0]-1)
levels=IndGen(info.ncont[0])*step+minimum
if info.retain eq 0 then begin
	loadCT,6,ncolors=!D.N_COlors
	loadCT,20,ncolors=info.ncont[0]+1,bottom=0
	loadCT,33,ncolors=info.ncont[0],/bottom
	info.bottom=1
	info.ncolors=info.ncont[0]
endif
if levels(1) eq levels (0) then begin
	Message,'Cannot produce contour plot at this Z',/continue	
	widget_control,info.quitID,Set_Uvalue=scalar,/No_Copy
	widget_control,event.top,Set_Uvalue=info,/No_Copy
	return
endif
Wset,info.wid
device,decomposed=0
contour,slice,scalar.x,scalar.y,/xstyle,/ystyle,title=string $
	,/fill,/isotropic,Levels=levels $
	,C_Colors=IndGen(info.ncont[0])+1,position=[0.15,0.25,0.95,0.95] $
	,xtitle='X',ytitle='Y' 
Colorbar,bottom=1,max=maximum,minimum=minimum $
	,ncolors=info.ncont[0],position=[0.08,0.03,0.95,0.11] $
        ,/top,format='(E10.2)'
;	,/top,format='(F7.4)'
if  info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='polarcont1.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then begin
		loadCT,6,ncolors=!D.N_COlors
		loadCT,20,ncolors=info.ncont[0]+1,bottom=0
		loadCT,33,ncolors=info.ncont[0],/bottom
	endif
	!P.color=!P.background
	contour,slice,scalar.x,scalar.y,/xstyle,/ystyle,title=string $
		,/fill,/isotropic,Levels=levels $
	,C_Colors=IndGen(info.ncont[0])+1,position=[0.15,0.25,0.95,0.95] $
		,xtitle='X',ytitle='Y' 
	Colorbar,bottom=1,max=maximum,minimum=minimum $
		,ncolors=info.ncont[0],position=[0.08,0.03,0.95,0.11] $
                ,/top,format='(E10.2)'
;		,/top,format='(F7.4)'
	widget_control,info.timeID,get_value=ctime
	xyouts,10,1,ctime,/device,charsize=1.2,color=!P.color
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.quitID,Set_Uvalue=scalar,/No_Copy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_cinterp,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;******This module determines whether nearest plane or interp is used

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.cinterp=event.index
widget_Control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_z,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****This module determines which circular contour to display

widget_control,event.top,Get_Uvalue=info,/No_copy
info.z[where(info.zID eq event.id)]=event.value
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_ctable, event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****This program calls XLoadCT to change color table****

widget_control,event.top,get_uvalue=info
xloadCT,group=event.id,ncolors=info.ncolors,bottom=info.bottom
END

;#####################################################################
PRO wdata_phi,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****This program selects which angle rz slice for scalar and vector
; modes

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.phi[where(info.phiID eq event.id)]=event.value
widget_Control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_scale,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****This program chooses which data to scale colors to

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.scale[where(info.scaleID eq event.id)] = event.index
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_rzslice,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****This program chooses which 1D slice (Z or R) to make:

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.rzslice[where(info.rzsliceID eq event.id)] = event.index
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_rzvalue,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****This program selects at which value of Z(R) to make 1D slice: 

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.rzvalue[where(info.rzvalueID eq event.id)]=event.value
widget_Control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_component,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***This program chooses the component of the vector data to contour

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.component=event.index
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_histo1,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****This program displays histogram of cylindrical data******

widget_control,event.top,Get_UValue=info,/no_copy
widget_control,info.quitID,Get_Uvalue=scalar,/NO_Copy
if n_tags(scalar,/length) eq 0 then begin
	message,'No scalar data loaded.',/continue
	widget_control,event.top,Set_Uvalue=info,/No_Copy
	return
endif
histo=histogram(scalar.ppol,binsize=0.01*(max(scalar.ppol)-min(scalar.ppol)))
junk=where(histo eq 0,count)
if count GT 0 then histo(junk) = 1
interval=(max(scalar.ppol)-min(scalar.ppol))/100.0
x=IndGen(101)*interval+min(scalar.ppol)
Wset,info.wid
if info.retain eq 0 then begin
	loadcT,0,ncolors=!D.N_COlors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
plot,x,histo,/xstyle,/ystyle,/ylog $
	,title=string('Histogram of Cylindrical ',scalar.type,' Data') $
	,xtitle=scalar.type
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='histzrphi.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors
	plot,x,histo,/xstyle,/ystyle,/ylog $
		,title=string('Histogram of Cylindrical ',scalar.type,' Data') $
		,xtitle=scalar.type
	widget_control,info.timeID,get_value=ctime
	xyouts,10,1,ctime,/device,charsize=1.2,color=0
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
!Y.type=0
widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
widget_control,event.top,Set_Uvalue=info,/no_copy
END

;#####################################################################
PRO wdata_histo2,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****This program displays histogram of Cartesian data

widget_control,event.top,Get_Uvalue=info,/no_copy
widget_control,info.quitID,Get_Uvalue=scalar,/no_copy
if n_tags(scalar,/length) eq 0 then begin
	message,'No scalar data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif
if NOT finite(scalar.num) then begin
	message,'You must convert to Cartesian coordinates first.',/continue
	widget_control,info.quitID,set_uvalue=scalar,/No_Copy
	widget_control,event.top,Set_Uvalue=info,/no_copy
	return
endif
histo=histogram(scalar.pxyz,binsize=0.01*(max(scalar.pxyz)-min(scalar.pxyz)))
junk=where(histo eq 0,count)
if count GT 0 then histo(junk)=1
interval=(max(scalar.pxyz)-min(scalar.pxyz))/100.0
x=IndGen(101)*interval+min(scalar.pxyz)
Wset,info.wid
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.N_COlors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
titlestring=string('Histogram of Cartesian ',scalar.type,' Data')
plot,x,histo,/xstyle,/ystyle,/ylog,xtitle=scalar.type,title=titlestring
if  info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='histxyz.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then loadCT,0,ncolors=!D.N_COlors
	plot,x,histo,/xstyle,/ystyle,/ylog,xtitle=scalar.type,title=titlestring
	widget_control,info.timeID,get_value=ctime
	xyouts,10,1,ctime,/device,charsize=1.2,color=0
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.quitID,Set_Uvalue=scalar,/no_copy
widget_control,event.top,Set_Uvalue=info,/no_copy
!Y.type=0
END

;#####################################################################
PRO wdata_cleanup,baseID
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****This program cleans up when the GUI is exiting

widget_control,baseID,Get_UValue=info,/No_Copy
If N_elements(info) eq 0 then Heap_GC 
END

;#####################################################################
PRO wdata_quit,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****This module deletes the GUI

!P.charsize=1.
LoadCT,0
widget_control,event.top,/destroy
END

;#####################################################################
PRO wdata_slider,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****This module chooses the pressure at the isosurface

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
info.iso=event.value/100.0
if ((n_tags(scalar,/length) ne 0) AND (finite(info.isovalueID))) then begin
	junkstring=string('Isosurface at ',info.iso*max(scalar.ppol) $
		,format='(A,F5.3)')
	widget_control,info.isovalueID,set_value=junkstring
	widget_control,info.quitID,set_uvalue=scalar,/No_Copy
endif
widget_Control,event.top,Set_Uvalue=info,/No_Copy
END
;#####################################################################
PRO wdata_getstat,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****This module calculates data statistics: max, min, avg.
Widget_Control,/hourglass
Widget_Control,event.top,Get_Uvalue=info,/no_copy
widget_control,info.quitID,get_uvalue=scalar,/NO_Copy
if n_tags(scalar,/length) eq 0 then begin
        message,'No scalar data loaded.',/continue
        widget_control,event.top,Set_Uvalue=info,/No_Copy
        return
endif

array=scalar.ppol
max=max(array)
min=min(array)
avg=total(array)/(scalar.ni*scalar.nj*scalar.nk)
print,'Max=',max
print,'Min=',min
print,'Avg=',avg

Message,'Data statistics is completed.',/continue
widget_control,info.quitID,set_uvalue=scalar,/NO_Copy
widget_control,event.top,set_uvalue=info,/no_copy
END

;#####################################################################
PRO wdata_convert,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****This module converts cylindrical to Cartesian (and rebins)

Widget_Control,/hourglass
Widget_Control,event.top,Get_Uvalue=info,/no_copy
widget_control,info.quitID,get_uvalue=scalar,/NO_Copy
if n_tags(scalar,/length) eq 0 then begin
	message,'No scalar data loaded.',/continue
	widget_control,event.top,Set_Uvalue=info,/No_Copy
	return
endif
;if finite(scalar.num) then begin
;	message,'This data is already converted.',/continue
;	widget_control,info.quitID,set_uvalue=scalar,/no_copy
;	widget_control,event.top,set_uvalue=info,/No_COpy
;	return
;endif
ninew=round(scalar.ni*2/3)
njnew=round(scalar.nj*2/3)
r=congrid(scalar.rpol,njnew,/interp)
z=congrid(scalar.zpol,ninew,/interp)
ppol=congrid(scalar.ppol,ninew,njnew,scalar.nk,/interp)
num=njnew*2-1
mr=max(r)
interval=mr/(n_elements(r)-1)
x=fltarr(num)
y=fltarr(num)
for q=0,num-1 do begin
	x(q)=-1*mr+q*interval
	y(q)=-1*mr+q*interval
endfor
p=fltarr(num,num,ninew)
myspacing=[interval,interval]
mybounds=[-1*mr,-1*mr,mr,mr]
for q=0,ninew-1 do begin
	ppolq=reform(ppol(q,*,*),njnew,scalar.nk)
	pq=polar_surface(ppolq,r,scalar.spol,/grid,spacing=myspacing $
		,bounds=mybounds)
	p(*,*,q)=reform(pq,num,num,1)
endfor
scalar1= {ni:scalar.ni, $
	nj:scalar.nj, $
	nk:scalar.nk, $
	ctime:scalar.ctime, $
	zpol:scalar.zpol, $
	rpol:scalar.rpol, $
	spol:scalar.spol, $
	ppol:scalar.ppol, $
	psave:scalar.psave, $
	type:scalar.type, $
	type0:scalar.type0, $
	num:num, $
	ninew:ninew, $
	z:z, $
	x:x, $
	y:y, $
	pxyz:p}
Message,'Conversion to Cartesian coordinates completed.',/continue
Widget_Control,info.quitID,Set_Uvalue=scalar1,/No_Copy
widget_control,event.top,set_uvalue=info,/no_copy
END

;#####################################################################
PRO wdata_display,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****This program displays isosurfaces******

widget_control,event.top,Get_Uvalue=info,/no_copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
if n_tags(scalar,/length) eq 0 then begin
	message,'No scalar data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif
if NOT finite(scalar.num) then begin
	message,'You must convert to Cartesian before displaying isosurfaces.',/continue
	widget_control,info.quitID,set_uvalue=scalar,/No_COpy
	widget_control,event.top,Set_Uvalue=info,/No_copy
	RETURN
endif
widget_control,/hourglass
WSet,info.wid
Shade_Volume,scalar.pxyz,info.iso*max(scalar.pxyz),vertices,polygons,/low
if n_elements(polygons) eq 0 then begin
	message,'There is no isosurface at this value of pressure.',/continue
	widget_control,info.quitID,set_uvalue=scalar,/No_Copy
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif

xmin= -fix(0.3*scalar.nj)
xmax= fix(3*scalar.nj)
ymin= 0
ymax= xmax
zmin= fix(7./150*scalar.ni)
zmax= fix(62./150*scalar.ni)
aspect=(max(scalar.zpol)-min(scalar.zpol))/(2*max(scalar.rpol))
if aspect LE 4.0 then begin
  xmin= fix(0.1*scalar.nj)
  xmax= fix(1.25*scalar.nj)
  ymin= xmin
  ymax= xmax
  zmin= fix(7./150*scalar.ni)
  zmax= fix(66./150*scalar.ni)
endif
;;;; for tokamak
if min(scalar.rpol) gt 0.0 then begin
  xmin= fix(0.15*scalar.nj)
  xmax= fix(0.9*scalar.nj)
  ymin= xmin
  ymax= xmax
  zmin= fix(10./80*scalar.ni) 
  zmax= fix(100./80*scalar.ni)
endif
;;;; for nstx
if info.device eq 1 then begin
  xmin= fix(0.1*scalar.nj)
  xmax= fix(1.2*scalar.nj)
  ymin= xmin
  ymax= xmax
  zmin= fix(9./100*scalar.ni) 
  zmax= fix(45./100*scalar.ni)
endif
Scale3,XRange=[xmin,xmax],YRange=[ymin,ymax],ZRange=[zmin,zmax]

isosurface=PolyShade(vertices,polygons,/T3D)
;isosurface1=isosurface
; isosurface1(where(isosurface1 EQ 0)) = !D.N_colors-1
; if info.retain eq 0 then begin
;	loadCT,1,NColors=!D.N_Colors
;	info.bottom=0
;	info.ncolors=!D.n_colors
; endif else topcolor=!D.N_colors-1
;;;;;;;;
ncolors_iso=60
bottom_iso=40
topcolor= bottom_iso+ncolors_iso-1
isosurface1=bytscl(isosurface,top=ncolors_iso-1) + byte(bottom_iso)
if info.retain eq 0 then begin
        loadCT,1,ncolors=ncolors_iso,bottom=bottom_iso
        info.bottom=bottom_iso
        info.ncolors=ncolors_iso
endif
isosurface1(where(isosurface1 EQ bottom_iso)) = topcolor

TV,isosurface1
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='isosurface.ps'
	device,filename=file,xsize=7.5,ysize=7.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color,bits_per_pixel=8
	if info.retain eq 0 then loadCT,1,ncolors=info.ncolors $
                                         ,bottom=info.bottom

        Scale3,XRange=[xmin,xmax],YRange=[ymin,ymax],ZRange=[zmin,zmax]

	isosurface2=bytscl(isosurface,top=info.ncolors-1) + byte(info.bottom)
	isosurface2(where(isosurface2 eq info.bottom))=topcolor
	TV,isosurface2

; isosurface1=isosurface
; isosurface1(where(isosurface1 EQ 0))=!D.N_colors-1
; loadCT,1,NColors=!D.N_Colors-1
; TV,isosurface1

	widget_control,info.timeID,get_value=ctime
	xyouts,10,1,ctime,/device,charsize=1.2,color=0
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
		spawn,string('lpr -Pl230-t850 ',file)
		spawn,string('rm ',file)
		message,'Image sent to l230-t850, no PS output to file.',/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
 ,/continue
          endif else $
            message,string('Print to file completed,filename=',file) $
 ,/continue
        endelse
	info.printnext=0
endif
widget_control,info.quitID,Set_Uvalue=scalar,/No_Copy
widget_control,event.top,set_Uvalue=info,/no_copy
END

;#####################################################################
PRO wdata_slicer,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***THis program calls IDL interactive Slicer (scalar mode)

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,info.quitID,Get_Uvalue=scalar,/No_Copy
if n_tags(scalar,/length) eq 0 then begin
        message,'No scalar data loaded.',/continue
        widget_control,event.top,Set_Uvalue=info,/No_COpy
        return
endif
if NOT finite(scalar.num) then begin
        message,'You must convert to Cartesian before displaying isosurfaces.'$
               ,/continue
        widget_control,info.quitID,set_uvalue=scalar,/No_COpy
        widget_control,event.top,Set_Uvalue=info,/No_copy
        RETURN
endif

;TVLCT,r,g,b,/get
; common volume_data,a
; a= scalar.pxyz
; slicer
hdata=ptr_new(scalar.pxyz,/no_copy)
slicer3,hdata,/modal
;TVLCT,r,g,b

widget_control,info.quitID,Set_Uvalue=scalar,/No_Copy
widget_control,event.top,set_Uvalue=info,/no_copy
END
;#####################################################################
PRO wdata_small,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***THis program chooses the display reduction (particle mode)

widget_control,event.top,get_uvalue=info,/No_COpy
info.small=event.value
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_ncont,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***This program selects the number of contours (scalar/vector modes)

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.ncont(where(info.ncontID eq event.id))=event.value
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_polarvect,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***draws vector plot in z slice (r, phi components)

widget_control,event.top,get_uvalue=info,/No_COpy
widget_control,info.fileID,get_uvalue=vector,/No_Copy
if n_tags(vector,/length) eq 0 then begin
	message,'No vector data loaded.',/continue
	widget_control,event.top,set_uvalue=info,/No_Copy
	return
endif
widgeT_control,/hourglass
zero=complexarr(vector.nk)
for k=0,vector.nk-1 do zero[k]=0
vr1=fltarr(vector.ni,vector.nj,vector.nk)
vs1=fltarr(vector.ni,vector.nj,vector.nk)

k=info.nmode
nk=vector.nk
if k ge 0 then begin
  for i=0,vector.ni-1 do begin
	for j=0,vector.nj-1 do begin
		tmp=reform(vector.vr(i,j,*),vector.nk)
		tmp2=FFT(tmp)
                zero(k)=tmp2(k)
		if k gt 0 then zero(nk-k)=tmp2(nk-k)
		tmp=FFT(zero,1)	
		vr1(i,j,*)=float(tmp)

		tmp=reform(vector.vs(i,j,*),vector.nk)
		tmp2=FFT(tmp)
                zero(k)=tmp2(k)
		if k gt 0 then zero(nk-k)=tmp2(nk-k)
		tmp=FFT(zero,1)
		vs1(i,j,*)=float(tmp)		
	endfor
  endfor
endif else begin
  vr1=vector.vr
  vs1=vector.vs
endelse

zvalue=info.z[1]/100.0*(max(vector.zpol)-min(vector.zpol)) $
       +min(vector.zpol)
inter=(max(vector.zpol)-min(vector.zpol))/(vector.ni-1)
zindex=round((zvalue-min(vector.zpol))/inter)	
ztruevalue=vector.zpol[zindex]

nib=fix(vector.ni/20.)
njb=fix(vector.nj/14.)
nii=vector.ni-(vector.ni mod nib)
njj=vector.nj-(vector.nj mod njb)
z=rebin(vector.zpol(0:nii-1),nii/nib)
r=rebin(vector.rpol(0:njj-1),njj/njb)
vrslice=reform(vr1(zindex,0:njj-1,*),njj,vector.nk)
vsslice=reform(vs1(zindex,0:njj-1,*),njj,vector.nk)
vrslice=rebin(vrslice,njj/njb,vector.nk)
vsslice=rebin(vsslice,njj/njb,vector.nk)

interval=max(r)/(n_elements(r)-1)
myspacing=[interval,interval]
mybounds=[-1*max(r),-1*max(r),max(r),max(r)]

;get the x and y velocity components
vx=fltarr(njj/njb,vector.nk)
vy=fltarr(njj/njb,vector.nk)
for p=0,njj/njb-1 do begin
  for m=0,vector.nk-1 do begin
    vx[p,m]= vrslice[p,m]*cos(vector.spol[m]) $
            -vsslice[p,m]*sin(vector.spol[m])
    vy[p,m]= vrslice[p,m]*sin(vector.spol[m]) $
            +vsslice[p,m]*cos(vector.spol[m])
  endfor
endfor

; Map Vx and Vy from regular r,phi grid into regular x,y grid.

vectors1= vector.spol+.001
vxxy=polar_surface(vx,r,vectors1,/grid,spacing=myspacing $
                  ,bounds=mybounds,/quintic)
vyxy=polar_surface(vy,r,vectors1,/grid,spacing=myspacing $
                  ,bounds=mybounds,/quintic)
num=n_elements(r)*2-1
x=fltarr(num)
y=fltarr(num)
for p=0,num-1 do begin
	x[p]=-1*max(r)+p*interval
	y[p]=-1*max(r)+p*interval
endfor
for i=0,num-1 do begin
  jy=where(x(i)^2+y^2 lt (min(r))^2, count)
  if count ne 0 then begin
     vxxy(i,jy)= 0.0
     vyxy(i,jy)= 0.0
  endif
endfor

wset,info.wid
titlestring=string(vector.type,' (r, phi) at z=',ztruevalue)
vamp=sqrt(vxxy*vxxy+vyxy*vyxy)
vmax=max(vamp)
vmin=min(vamp)
svmax=string(vmax,format='(e10.2)')
svmin=string(vmin,format='(e10.2)')
strvmax='Vmax = '+svmax+';     Vmin= '+svmin
if info.retain eq 0 then begin
	loadCT,0,ncolors=!D.n_colors
	info.bottom=0
	info.ncolors=!D.n_colors
endif
velovect,vxxy,vyxy,x,y,xtitle='X',ytitle='Y',title=titlestring $
        ,/isotropic
xyouts,10,1,strvmax,/device,charsize=1.
if info.printnext GT 0 then begin
	thisdevice=!D.Name
	set_plot,'PS',copy=info.retain
	file='rphivect.ps'
	device,filename=file,xsize=4.5,ysize=4.5,/inches,xoffset=0.75 $
		,yoffset=2.0,/color
	if info.retain eq 0 then loadCT,0,ncolors=!D.N_Colors
	velovect,vxxy,vyxy,x,y,xtitle='X',ytitle='Y',title=titlestring $
		,/isotropic
        widget_control,info.timeID,get_value=ctime
	totalstring=ctime+', '+strvmax
	xyouts,10,1,totalstring,/device,charsize=1.
	device,/close_file
	set_plot,thisdevice
	if info.printnext eq 2 then begin
          spawn,string('lpr -Pl230-t850 ',file)
	  spawn,string('rm ',file)
	  message,'Image sent to l230-t850, no PS output to file.' $
                 ,/continue
        endif else begin
          if info.printnext eq 3 then begin
                spawn,string('lpr ',file)
                spawn,string('rm ',file)
                message,'Image sent to lpr, no PS output to file.' $
,/continue
          endif else $
              message,string('Print to file completed,filename=',file) $
,/continue
        endelse
	info.printnext=0
endif
widget_control,info.fileID,set_uvalue=vector,/no_copy
widget_control,event.top,set_uvalue=info,/NO_Copy
END

;#####################################################################
PRO wdata_curlv,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*******This module changes Vector to curl(Vector) and back.

widget_control,event.top,Get_Uvalue=info,/no_copy
widget_control,info.fileID,Get_Uvalue=vector,/No_Copy
if n_tags(vector,/length) eq 0 then begin
        message,'No vector data loaded.',/continue
        widget_control,info.curlvID,set_droplist_select=info.curlv
        widget_control,event.top,Set_Uvalue=info,/No_COpy
        return
endif
; widget_control,/hourglass

info.curlv=event.index
if info.curlv eq 0 then begin
	vector.type= vector.type0
	vector.vz= vector.vz0
	vector.vr= vector.vr0
	vector.vs= vector.vs0
endif else if info.curlv eq 1 then begin
        vector.type=string('curl_',vector.type0)
	dz= vector.zpol(1)-vector.zpol(0)
	dr= vector.rpol(1)-vector.rpol(0)
	ds= vector.spol(1)-vector.spol(0)
	ni= vector.ni
	nj= vector.nj
	nk= vector.nk
; curl(V)_z
	for j=1,nj-2 do vector.vz(*,j,*)= ( vector.rpol(j+1) $
   *vector.vs0(*,j+1,*) - vector.rpol(j-1)*vector.vs0(*,j-1,*) )/(2.*dr)
        for k=1,nk-2 do vector.vz(*,*,k)= vector.vz(*,*,k)  $
               - ( vector.vr0(*,*,k+1) - vector.vr0(*,*,k-1) )/(2.*ds)
	for j=1,nj-2 do vector.vz(*,j,*)=vector.vz(*,j,*)/vector.rpol(j)
; curl(V)_r
        for k=1,nk-2 do vector.vr(*,*,k)=  $
                 ( vector.vz0(*,*,k+1) - vector.vz0(*,*,k-1) )/(2.*ds)
        for j=1,nj-2 do vector.vr(*,j,*)=vector.vr(*,j,*)/vector.rpol(j)
        for i=1,ni-2 do vector.vr(i,*,*)= vector.vr(i,*,*)  $
               - ( vector.vs0(i+1,*,*) - vector.vs0(i-1,*,*) )/(2.*dz)
; curl(V)_phi
	for i=1,ni-2 do vector.vs(i,*,*)=  $
                 ( vector.vr0(i+1,*,*) - vector.vr0(i-1,*,*) )/(2.*dz)
	for j=1,nj-2 do vector.vs(*,j,*)= vector.vs(*,j,*)  $
               - ( vector.vz0(*,j+1,*) - vector.vz0(*,j-1,*) )/(2.*dr)
; fix boundaries (set=0)
        vector.vz(*,0,*)= 0.
        vector.vz(*,*,0)= vector.vz(*,*,nk-2)
        vector.vz(*,nj-1,*)= 0.
        vector.vz(*,*,nk-1)= vector.vz(*,*,1)
        vector.vr(0,*,*)= 0.
        vector.vr(*,*,0)= vector.vr(*,*,nk-2)
        vector.vr(ni-1,*,*)= 0.
        vector.vr(*,*,nk-1)= vector.vr(*,*,1)
        vector.vs(0,*,*)= 0.
        vector.vs(*,0,*)= 0.
        vector.vs(ni-1,*,*)= 0.
        vector.vs(*,nj-1,*)= 0.
endif 

; div(V).
if info.curlv eq 2 then begin
        vector.type=string('div(',vector.type0,')')
        dz= vector.zpol(1)-vector.zpol(0)
        dr= vector.rpol(1)-vector.rpol(0)
        ds= vector.spol(1)-vector.spol(0)
        ni= vector.ni
        nj= vector.nj
        nk= vector.nk
; 
        for j=1,nj-2 do vector.vz(*,j,*)= ( vector.rpol(j+1) $
   *vector.vr0(*,j+1,*) - vector.rpol(j-1)*vector.vr0(*,j-1,*) )/(2.*dr)
        for k=1,nk-2 do vector.vz(*,*,k)= vector.vz(*,*,k)  $
               + ( vector.vs0(*,*,k+1) - vector.vs0(*,*,k-1) )/(2.*ds)
        for j=1,nj-2 do vector.vz(*,j,*)=vector.vz(*,j,*)/vector.rpol(j)
        for i=1,ni-2 do vector.vz(i,*,*)= vector.vz(i,*,*)  $
               + ( vector.vz0(i+1,*,*) - vector.vz0(i-1,*,*) )/(2.*dz)
        vector.vz(0,*,*)= 0.
        vector.vz(*,0,*)= 0.
        vector.vz(*,*,0)= vector.vz(*,*,1)
        vector.vz(ni-1,*,*)= 0.
        vector.vz(*,nj-1,*)= 0.
        vector.vz(*,*,nk-1)= vector.vz(*,*,nk-2)
        vector.vr= vector.vz
        vector.vs= vector.vz
endif

widget_control,info.fileID,set_uvalue=vector,/NO_Copy
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_nmode,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;This program selects the mode of the vector data to plot

widget_control,event.top,Get_Uvalue=info,/No_Copy
info.nmode=event.value
widget_control,event.top,Set_Uvalue=info,/No_COpy
END

;#####################################################################
PRO wdata_area,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Changes the area of distribution function calculation***

widget_control,event.top,get_uvalue=info,/NO_COpy
info.area(where(info.setareaID eq event.ID))=event.value
widget_control,info.setareaID[0],set_slider_max=info.area[1]
widget_control,info.setareaID[1],set_slider_min=info.area[0]
widget_control,info.setareaID[2],set_slider_max=info.area[3]
widget_control,info.setareaID[3],set_slider_min=info.area[2]
widget_control,info.setareaID[4],set_slider_max=info.area[5]
widget_control,info.setareaID[5],set_slider_min=info.area[4]
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_nbins,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;*****Sets number of bins for distribution functions*****

widget_control,event.top,get_uvalue=info,/No_Copy
info.nbins[where(info.nbinsID eq event.ID)]=event.value
widget_control,event.top,set_uvalue=info,/NO_Copy
END

;#####################################################################
PRO wdata_compute,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Computes distribution functions***

widget_control,event.top,get_uvalue=info,/No_Copy
widget_control,info.convertID,get_uvalue=particle,/No_Copy
if n_tags(particle,/length) eq 0 then begin
	Message,'No particle data loaded.',/continue
	widgeT_control,event.top,set_uvalue=info,/No_COpy
	return
endif
if event.id eq info.computeID[2] then begin
	widget_control,info.colorID,get_uvalue=magnetic,/NO_Copy
	if n_tags(magnetic,/length) eq 0 then begin
		message,'No magnetic field data loaded.',/continue
		widget_control,info.convertID,set_uvalue=particle,/No_COpy
		widget_control,event.top,set_uvalue=info,/No_COpy
		return
	endif
	bzarray=magnetic.bz
	brarray=magnetic.br
	bsarray=magnetic.bs
endif
widget_control,/hourglass
xmi=particle.x0-0.5*particle.xlen
xma=particle.x0+0.5*particle.xlen
ymi=particle.y0-0.5*particle.ylen
yma=particle.y0+0.5*particle.ylen
zmi=min(particle.z)
zma=max(particle.z)
xminvalue=info.area[0]*(xma-xmi)/100.0+xmi
xmaxvalue=info.area[1]*(xma-xmi)/100.0+xmi
yminvalue=info.area[2]*(yma-ymi)/100.0+ymi
ymaxvalue=info.area[3]*(yma-ymi)/100.0+ymi
zminvalue=info.area[4]*(zma-zmi)/100.0+zmi
zmaxvalue=info.area[5]*(zma-zmi)/100.0+zmi
if event.id eq info.computeID[0] then begin
	v0=particle.vx
	v1=particle.vy
	v2=particle.vz
endif
if ((event.id eq info.computeID[1]) OR (event.id eq info.computeID[2])) then begin
	vr=fltarr(particle.np)
	vs=fltarr(particle.np)	
	r=fltarr(particle.np)
	if event.id eq info.computeID[2] then begin
		vp=fltarr(particle.np)
		vn=fltarr(particle.np)
	endif
	for m=0L,particle.np-1 do begin
		r[m]=sqrt((particle.x[m]-particle.x0)^2+ $
			(particle.y[m]-particle.y0)^2)
		if r[m] eq 0 then r[m]=0.00001
		vr[m]=particle.vx[m]*(particle.x[m]-particle.x0)/r[m] + $
			particle.vy[m]*(particle.y[m]-particle.y0)/r[m]
		vs[m]=-1*particle.vx[m]*(particle.y[m]-particle.y0)/r[m] + $
			particle.vy[m]*(particle.x[m]-particle.x0)/r[m]
		if event.id eq info.computeID[2] then begin
			xindex=particle.x[m]/magnetic.interval
			yindex=particle.y[m]/magnetic.interval
			zindex=(particle.z[m]-min(magnetic.z)) $
			   /(max(magnetic.z)-min(magnetic.z)) $
			   *(magnetic.ninew-1)
			bzm=interpolate(bzarray,xindex,yindex,zindex)
			brm=interpolate(brarray,xindex,yindex,zindex)
			bsm=interpolate(bsarray,xindex,yindex,zindex)
			bm=sqrt((bzm)^2+(brm)^2+(bsm)^2)
			if (bm NE 0) then begin
				vp[m]=(vr[m]*brm+vs[m]*bsm+particle.vz[m]*bzm)/bm
				rpart=vr[m]-(vp[m]*brm/bm)
				spart=vp[m]*bsm/bm
				zpart=particle.vz[m]-(vp[m]*bzm/bm)
				vn[m]=sqrt(rpart^2+spart^2+zpart^2)
				check=rpart*(-1*bzm/bm)+zpart*(brm/bm)
				if check LT 0.0 then vn[m]=-1*vn[m]
			endif else begin
				vp[m]=!Values.F_NaN	
			endelse
		endif
	endfor
	v2=vs				
	if event.id eq info.computeID[1] then begin
		v0=particle.vz
		v1=vr
	endif else begin
		v0=vp
		v1=vn
	endelse
endif
v0max=max(v0,/NaN)
v0min=min(v0,/NaN)
v1max=max(v1,/NaN)
v1min=min(v1,/NaN)
v2max=max(v2,/NaN)
v2min=min(v2,/NaN)
deltav0=(v0max-v0min)/(info.nbins[0]-1)
deltav1=(v1max-v1min)/(info.nbins[1]-1)
deltav2=(v2max-v2min)/(info.nbins[2]-1)
N_p=fltarr(info.nbins[0],info.nbins[1],info.nbins[2])
n_w=fltarr(info.nbins[0],info.nbins[1],info.nbins[2])
for m=0L,particle.np-1 do begin
	v0i=round((v0[m]-v0min)/deltav0)
	v1i=round((v1[m]-v1min)/deltav1)
	v2i=round((v2[m]-v2min)/deltav2)
	if ((particle.x[m] GE xminvalue) AND $
	    (particle.x[m] LE xmaxvalue) AND $
	    (particle.y[m] GE yminvalue) AND $
	    (particle.y[m] LE ymaxvalue) AND $
	    (particle.z[m] GE zminvalue) AND $
	    (particle.z[m] LE zmaxvalue) AND $
	    (finite(v0[m]))) then begin
		N_p(v0i,v1i,v2i)=N_p(v0i,v1i,v2i)+particle.p[m]
		n_w(v0i,v1i,v2i)=n_w(v0i,v1i,v2i)+particle.w[m]
	endif
endfor
F=N_p/max(N_p)
deltaF=n_w/max(N_p)
binv0=fltarr(info.nbins[0])
binv1=fltarr(info.nbins[1])
binv2=fltarr(info.nbins[2])
for i=0,info.nbins[0]-1 do binv0[i]=v0min+i*deltav0
for i=0,info.nbins[1]-1 do binv1[i]=v1min+i*deltav1
for i=0,info.nbins[2]-1 do binv2[i]=v2min+i*deltav2
binv=fltarr(info.nbins[0],info.nbins[1],info.nbins[2])
for i=0,info.nbins[0]-1 do begin
	for j=0,info.nbins[1]-1 do begin
		for k=0,info.nbins[2]-1 do binv(i,j,k)= $
			sqrt((binv0[i])^2+(binv1[j])^2+(binv2[k])^2)
	endfor
endfor
F0=fltarr(info.nbins[0],info.nbins[1],info.nbins[2])
for i=0,info.nbins[0]-1 do begin
	for j=0,info.nbins[1]-1 do begin
		for k=0,info.nbins[2]-1 do F0[i,j,k]= $
			exp(-1*(binv[i,j,k]/particle.vth)^2)
	endfor
endfor
F0=F0/max(F0)
Faccurate=F0+deltaF
df={binv0:binv0, $
    binv1:binv1, $
    binv2:binv2, $
    Faccurate:Faccurate, $
    F:F, $
    deltaF:deltaF, $
    nv0:info.nbins[0], $
    nv1:info.nbins[1], $
    nv2:info.nbins[2], $
    deltav0:deltav0, $
    deltav1:deltav1, $
    deltav2:deltav2}
if event.id eq info.computeID[0] then $
	widget_control,info.loadnewID,set_uvalue=df,/No_Copy
if event.id eq info.computeID[1] then $
	widget_control,info.load1ID,set_uvalue=df,/No_Copy
if event.id eq info.computeID[2] then begin
	widget_control,info.dataID,set_uvalue=df,/No_Copy
	widget_control,info.colorID,set_uvalue=magnetic,/NO_Copy
endif
widget_control,info.convertID,set_uvalue=particle,/No_Copy
widget_control,event.top,set_uvalue=info,/No_Copy
END

;#####################################################################
PRO wdata_mode,event
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;****Loads the mode for analysis ****

widget_control,event.top,Get_Uvalue=info,/No_Copy
widget_control,event.top,update=0
x=!Values.F_NaN
CASE event.index OF
	0 : BEGIN  ;scalar mode
	if info.mode eq 0 then begin
		widget_control,event.top,set_uvalue=info,/No_Copy
		return
	endif
	info.mode=0
	widget_control,info.timeID,/map
	widget_control,info.quitID,get_uvalue=scalar,/NO_COpy
	widget_control,info.drawID,draw_xsize=640,draw_ysize=580
	widget_control,info.xID,set_value=640
	widget_control,info.yID,set_value=680
	if n_tags(scalar,/length) EQ 0 then $
		widget_control,info.timeID,set_value='No scalar data loaded.' else begin $
	    widget_control,info.timeID,set_value=scalar.ctime
	    widget_control,info.quitID,set_uvalue=scalar,/No_Copy
	endelse
	IF finite(info.zmodeID) then begin
		widget_control,info.zmodeID,/destroy
		widget_control,info.rzmodeID,/destroy
		widget_control,info.nmodeID,/destroy
		widget_control,info.curlvmodeID,/destroy
		widget_control,info.v2smodeID,/destroy
		info.ncontID[2:3]=x
		info.phiID[1]=x
		info.zID[1]=x
		info.isotropicID[1]=x
		info.zmodeID = x
		info.rzmodeID= x
		info.v2smodeID= x
	endif
	IF finite(info.smallID) then begin
		widget_control,info.smallID,/destroy
		for i=0,2 do widget_control,info.dfID[i],/destroy
		widget_control,info.areaID,/destroy
		info.smallID=x
		widget_control,info.nbinsareaID,/destroy
		info.setareaID=[x,x,x,x,x,x]
	endif
	if FINITE(info.nthID) then begin
		widget_control,info.nthID,/destroy
		widget_control,info.zsliceID,/destroy
		widget_control,info.phisliceID,/destroy
		info.nthID=x
		info.phiID[2]=x
		info.phiID[3]=x
		info.projectID=x
	endif
	IF finite(info.eqbaseID) then begin
		widget_control,info.eqbaseID,/destroy
		widget_control,info.eqlabelID,/destroy
		info.eqlabelID=x
		info.eqbaseID=x
	endif
	baseID=event.top
;	info.histomodeID=Widget_Base(baseID,title='Histogram',column=1 $
;		,/frame,/base_align_center)
        info.deltasmodeID=Widget_Base(baseID,title='Delta s',column=1 $
                ,/base_align_center)
        info.v2smodeID=Widget_Base(baseID,title='V to S' $
                ,/base_align_center)
	info.rzcontmodeID=Widget_Base(baseID,title='R-Z Contour Display' $
		,column=1,/frame,/base_align_center)
        info.rzslicemodeID=Widget_Base(baseID,title='R(Z) Slice' $
                ,column=1,/frame,/base_align_center)
	info.isomodeID=Widget_Base(baseID,title='Isosurface Display' $
		,column=1,/frame,/base_align_center)
	info.ccontmodeID=Widget_Base(baseID $
		,title='Circular Contour Display' $
		,column=1,/frame,/base_align_center)	
; S or delta_S
        info.deltasID=widget_droplist(info.deltasmodeID $
                ,event_pro='wdata_deltas' $
                ,title='Plot S or Delta_S:' $
                ,value=['S','Delta_S','S0','|dS|'])
        widget_control,info.deltasID,set_droplist_select=info.deltas
; Vector to Scalar
        info.v2sID=widget_droplist(info.v2smodeID $
                ,event_pro='wdata_v2s' $
                ,title='Vector to Scalar:' $
                ,value=['none','V_z','V_r','V_phi','V_tot','flux','V_pll','|V_perp|'])
        widget_control,info.v2sID,set_droplist_select=info.v2s
; isosurfaces
	isolabelID=Widget_Label(info.isomodeID,/align_center,Value='ISOSURFACES')
	sliderID=widget_slider(info.isomodeID,xsize=150 $
		,title='Isosurface (% max)',/Scroll $
		,Value=info.iso*100,event_pro='wdata_slider')
	displayID=Widget_Button(info.isomodeID,value='Display Isosurface' $
		,Event_Pro='wdata_display')
	widget_control,info.quitID,get_uvalue=scalar,/NO_COpy
	if n_tags(scalar,/length) GT 0 then begin
		junkstring=string('Isosurface at ', $
			info.iso*max(scalar.ppol),format='(A,F5.3)')
		widget_control,info.quitID,set_uvalue=scalar,/No_COpy
	endif else junkstring='No scalar data loaded.'
	info.isovalueID=Widget_Text(info.isomodeID,value=junkstring)
        slicerID=Widget_Button(info.isomodeID,value='IDL Slicer' $
                ,Event_Pro='wdata_slicer')
; rz contours
	rzcontlabelID=Widget_Label(info.rzcontmodeID,/align_Center $
		,Value='R-Z CONTOUR PLOTS')
	info.phiID[0]=widget_slider(info.rzcontmodeID,maximum=360,scroll=5 $
		,value=info.phi[0] $
		,title='Angle',event_pro='wdata_phi',xsize=150)
	info.ncontID[1]=widget_slider(info.rzcontmodeID,maximum=30,scroll=1 $
                ,minimum=6 $
		,value=info.ncont[1],title='Number of RZ Contours' $
		,event_pro='wdata_ncont')
	isochooseID=widget_base(info.rzcontmodeID,/column,/frame $
		,title='Display Type',/base_align_center)
	junkID=widget_label(isochooseID $
		,value='Display Type:')
	info.isotropicID[0]=widget_list(isochooseID $
		,event_pro='wdata_isotropic' $
		,value=['Full Graphics Window','Maintain Aspect Ratio'] $
		,ysize=2)
	widget_control,info.isotropicID[0],set_list_select=info.isotropic[0]
	info.scaleID[0]=widget_droplist(info.rzcontmodeID $
		,event_pro='wdata_scale' $
		,title='Data scaling:' $
		,value=['All data','This slice only'])
	widget_control,info.scaleID[0],set_droplist_select=info.scale[0]
	rzcontourID=widget_button(info.rzcontmodeID,value='Show RZ Contour' $
		,Event_Pro='wdata_rzcontour')
;;;
        rzslicelabelID=Widget_Label(info.rzslicemodeID,/align_Center $
                ,Value='')
        info.rzsliceID[0]=widget_droplist(info.rzslicemodeID $
                ,event_pro='wdata_rzslice' $
                ,title='Choose Z or R slice:' $
                ,value=['Z','R'])
        widget_control,info.rzsliceID[0],set_droplist_select=info.rzslice[0]
        info.rzvalueID[0]=widget_slider(info.rzslicemodeID,maximum=100 $
                ,scroll=1,xsize=150 $
                ,value=info.rzvalue[0],title='Z or R value (%)' $
                ,event_pro='wdata_rzvalue')
        rzsliceID=widget_button(info.rzslicemodeID,value='Show Z (R) slice' $
                ,Event_Pro='wdata_rz1dslice')

;	histolabelID=WIdget_Label(info.histomodeID $
;		,value='HISTOGRAMS',/align_center)
;	histo1ID=widget_button(info.histomodeID $
;		,value='Histogram Cylindrical Data' $
;		,Event_Pro='wdata_histo1',/align_center)
;	histo2ID=widget_button(info.histomodeID $
;		,value='Histogram Cartesian Data' $
;		,Event_Pro='wdata_histo2',/align_center)
 
	ccontlabelID=Widget_label(info.ccontmodeID,/align_center $
		,value='CIRCULAR CONTOUR PLOTS')
	info.zID[0]=widget_slider(info.ccontmodeID,scroll=5 $
		,value=info.z[0],title='Z value (%)',event_pro='wdata_z')
	info.ncontID[0]=widget_slider(info.ccontmodeID,maximum=30,scroll=1,minimum=6 $
		,value=info.ncont[0],title='Number of Contours' $
		,event_pro='wdata_ncont')
	cinterpID=widget_droplist(info.ccontmodeID,event_pro='wdata_cinterp' $
		,title='Get the data:' $
		,value=['Nearest plane','Interpolation'])
	widget_control,cinterpID,set_droplist_select=info.cinterp
	info.scaleID[1]=widget_droplist(info.ccontmodeID $
		,event_pro='wdata_scale' $
		,title='Scaling:' $
		,value=['All data','This slice only'])
	widget_control,info.scaleID[1],set_droplist_select=info.scale[1]
	ccontour1ID=widget_button(info.ccontmodeID,value='Show Contour (Polar)' $
		,Event_Pro='wdata_ccontour1')
	ccontour2ID=widget_button(info.ccontmodeID,value='Show Contour (XY)' $
		,Event_Pro='wdata_ccontour2')
	END
	1 : BEGIN
	if info.mode eq 1 then begin
		widget_control,event.top,set_uvalue=info,/No_Copy
		return
	endif
	info.mode=1
	widget_control,info.timeID,/map
	widget_control,info.fileID,get_uvalue=vector,/no_copy
	widget_control,info.drawID,draw_xsize=640,draw_ysize=580
	widget_control,info.xID,set_value=640
	widget_control,info.yID,set_value=680
	if n_tags(vector,/length) eq 0 then $
	  widget_control,info.timeID,set_value='No vector data loaded.' else begin
		widget_control,info.timeID,set_value=vector.ctime
		widget_control,info.fileID,set_uvalue=vector,/no_copy
	endelse
	if finite(info.ccontmodeID) then begin
		widget_control,info.ccontmodeID,/destroy
		widget_control,info.isomodeID,/destroy
		widget_control,info.deltasmodeID,/destroy
		widget_control,info.v2smodeID,/destroy
		widget_control,info.rzcontmodeID,/destroy
		widget_control,info.rzslicemodeID,/destroy
;		widget_control,info.histomodeID,/destroy
		info.ncontID[0:1]=x
		info.phiID[0]=x
		info.zID[0]=x
		info.scaleID=[x,x]
		info.rzsliceID[0]=x
		info.rzvalueID[0]=x
                info.deltasID=x
		info.isotropicID[0]=x
		info.ccontmodeID=x
		info.isovalueID=x
		info.v2smodeID=x
	endif
	IF finite(info.smallID) then begin
		widget_control,info.smallID,/destroy
		for i=0,2 do widget_control,info.dfID[i],/destroy
		widget_control,info.areaID,/destroy
		info.setareaID=[x,x,x,x,x]
		info.smallID=x
		widget_control,info.nbinsareaID,/destroy
	endif
	if FINITE(info.nthID) then begin
		widget_control,info.nthID,/destroy
		widget_control,info.zsliceID,/destroy
		widget_control,info.phisliceID,/destroy
		info.nthID=x
		info.phiID[2]=x	
		info.phiID[3]=x
		info.projectID=x
	endif
	if finite(info.eqbaseID) then begin
		widget_control,info.eqbaseID,/destroy
		widget_control,info.eqlabelID,/destroy
		info.eqlabelID=x
		info.eqbaseID=x
	endif
	baseID=event.top
        info.v2smodeID=Widget_Base(baseID,title='V to S' $
                ,/base_align_center)
	info.rzmodeID=widget_base(baseID,title='RZ Mode',/frame,/column $
		,/base_align_center)
	info.nmodeID=widget_slider(baseID,title='Mode to Plot (-1 for total V)' $
                ,minimum=-1,maximum=12 $
		,scroll=1,value=info.nmode,event_pro='wdata_nmode')
;
	info.zmodeID=widget_base(baseID,title='Z Mode',/frame,/column $
		,/base_align_center)
        info.curlvmodeID=Widget_Base(baseID,title='curl_V',column=1 $
                ,/base_align_center)
; V or curl_V 
        info.curlvID=widget_droplist(info.curlvmodeID $
                ,event_pro='wdata_curlv' $
                ,title='Plot V, curl(V) or div(V):' $
                ,value=['V','curl(V)','div(V)'])
        widget_control,info.curlvID,set_droplist_select=info.curlv
; Vector to Scalar
        info.v2sID=widget_droplist(info.v2smodeID $
                ,event_pro='wdata_v2s' $
                ,title='Vector to Scalar:' $
                ,value=['none','V_z','V_r','V_phi','V_tot','psi','V_pll','|V_perp|'])
        widget_control,info.v2sID,set_droplist_select=info.v2s
; vector rz plots
	rzlabelID=widget_label(info.rzmodeID,/align_center,value='RZ Plane')
	info.phiID[1]=widget_slider(info.rzmodeID,maximum=360,scroll=5 $
		,value=info.phi[1] $
		,title='Angle:',event_pro='wdata_phi',xsize=150)	
	info.ncontID[2]=Widget_slider(info.rzmodeID,maximum=30,minimum=6 $
		,value=info.ncont[2],title='Number of Contours' $
		,event_pro='wdata_ncont',xsize=150)
	phicontID=widget_button(info.rzmodeID $
		,value='Draw Contour Plot of Phi-component' $
		,event_Pro='wdata_phicont')
	zrvectID=widget_button(info.rzmodeID $
		,value='Draw Vector Plot of Z and R components' $
		,event_pro='wdata_zrvect')
	isochoose2ID=widget_base(info.rzmodeID,/column,/frame $
		,title='Display Type')
	list2labelID=widget_label(isochoose2ID,/align_center $
		,value='Display Options:')
	info.isotropicID[1]=widget_list(isochoose2ID $
		,event_pro='wdata_isotropic' $
		,value=['Full Graphics Window','Maintain Aspect Ratio'] $
		,ysize=2,/align_center)
	widget_control,info.isotropicID[1],set_list_select=info.isotropic[1]
	zlabelID=widget_label(info.zmodeID,/align_center $
		,value='Z Slices (Circular)')
	info.zID[1]=widget_slider(info.zmodeID,scroll=5,value=info.z[1] $
		,title='Z Value (% of Total)',event_pro='wdata_z')
	info.ncontID[3]=widget_slider(info.zmodeID,maximum=30,minimum=6,/scroll $
		,value=info.ncont[3],title='Number of Contours' $
		,event_pro='wdata_ncont',xsize=150)
	componentID=widget_droplist(info.zmodeID,event_pro='wdata_component' $
		,title='Pick Component for Contouring:' $
		,value=['Z','R','Phi','Parallel','Total'])
	widget_control,componentID,set_droplist_select=info.component
	info.scaleID[2]=widget_droplist(info.zmodeID $
		,event_pro='wdata_scale' $
		,title='Contour scaling:' $
		,value=['All data','This slice only'])
	widget_control,info.scaleID[2],set_droplist_select=info.scale[2]
	vectcontID=widget_button(info.zmodeID $
		,value='Draw Contour Plot' $
		,event_pro='wdata_vectcont')
	polarvectID=widget_button(info.zmodeID $
		,value='Vector Plot of r and phi components' $
		,event_pro='wdata_polarvect')
	END
	2 : BEGIN ;particle mode
	if info.mode eq 2 then begin
		widget_control,event.top,set_uvalue=info,/No_Copy
		return
	endif
	info.mode=2
	widget_control,info.timeID,map=0
	widget_control,info.drawID,draw_xsize=580,draw_ysize=580
	widget_control,info.xID,set_value=580
	widget_control,info.yID,set_value=580
	if finite(info.ccontmodeID) then begin
		widget_control,info.ccontmodeID,/destroy
		widget_control,info.isomodeID,/destroy
		widget_control,info.deltasmodeID,/destroy
		widget_control,info.rzcontmodeID,/destroy
		widget_control,info.rzslicemodeID,/destroy
;		widget_control,info.histomodeID,/destroy
		info.ncontID[0:1]=x
		info.phiID[0]=x
		info.zID[0]=x
		info.isovalueID=x
		info.scaleID=[x,x]
		info.rzsliceID[0]=x
		info.rzvalueID[0]=x
		info.deltasID=x
		info.isotropicID[0]=x
		info.ccontmodeID=x
	endif
	IF finite(info.zmodeID) then begin
		widget_control,info.zmodeID,/destroy
		widget_control,info.rzmodeID,/destroy
		widget_control,info.nmodeID,/destroy
		widget_control,info.curlvmodeID,/destroy
                widget_control,info.v2smodeID,/destroy
		info.ncontID[2:3]=x
		info.phiID[1]=x
		info.zID[1]=x
		info.isotropicID[1]=x
		info.zmodeID=x
	endif
	if FINITE(info.nthID) then begin
		widget_control,info.nthID,/destroy
		widget_control,info.zsliceID,/destroy
		widget_control,info.phisliceID,/destroy
		info.nthID=x
		info.phiID[2]=x
		info.phiID[3]=x
		info.projectID=x
	endif
	IF finite(info.eqbaseID) then begin
		widget_control,info.eqbaseID,/destroy
		widget_control,info.eqlabelID,/destroy
		info.eqlabelID=x
		info.eqbaseID=x
	endif
	info.nbinsareaID=widget_base(event.top,/column,/frame)
	nbinslabelID=widget_label(info.nbinsareaID $
		,value='Number of bins')
	info.smallID=widget_slider(event.top,event_pro='wdata_small' $
		,maximum=10,minimum=1,value=info.small,/scroll $
		,title='Reduce Display:')
	for i=0,2 do info.nbinsID[i]=widget_slider(info.nbinsareaID $
		,event_pro='wdata_nbins',/scroll $
		,maximum=80,minimum=8,value=info.nbins[i] $
		,title=string('Number of Bins (Component',i+1,')'))
	info.areaID=widget_base(event.top,/frame,/column,/base_align_center)
	arealabelID=widget_label(info.areaID,value='Area of Integration')
	strings=['Minimum X (%):','Maximum X (%):','Minimum Y (%):' $
		,'Maximum Y (%):','Minimum Z (%):','Maximum Z (%):']
	for i=0,5 do $
		info.setareaID[i]=widget_slider(info.areaID $
		,event_pro='wdata_area' $
		,scroll=5,title=strings[i],value=info.area[i])
	strings2=['XYZ','ZRPhi','PNPhi']
	strings3=[['X','Z','P'],['Y','R','N'],['Z','Phi','Phi']]
	for i=0,2 do begin
		info.dfID[i]=widget_base(event.top,/frame $
			,/column)
		info.dflabelID[i]=widget_label(info.dfID[i] $
			,/align_center $
			,value=string(strings2[i],' Distribution Function'))
		info.computeID[i]=widget_button(info.dfID[i] $
			,event_pro='wdata_compute' $
			,value=string('Compute ',strings2[i],' Distribution Function'))	
	for j=0,2 do begin
		info.draw1ID[i,j]=widget_button(info.dfID[i] $
			,event_pro='wdata_draw1'$
			,value=string('Display Component ',strings3[i,j]))
		indices=intarr(2)
		if j eq 0 then indices=[1,2]
		if j eq 1 then indices=[0,2]
		if j eq 2 then indices=[0,1]
		info.draw2ID[i,j]=widget_button(info.dfID[i] $
			,event_pro='wdata_draw2' $
			,value=string('Display Components ',strings3[i,indices[0]],' and ',strings3[i,indices[1]]))
	endfor
	endfor
	END
	3 : BEGIN ;scatter mode
	if info.mode eq 3 then begin
		widget_control,event.top,set_uvalue=info,/No_Copy
		return
	endif
	widget_control,info.timeID,map=0
	widget_control,info.drawID,draw_xsize=580,draw_ysize=580
	widget_control,info.xID,set_value=580
	widget_control,info.yID,set_value=580
	info.mode=3
	if FINITE(info.ccontmodeID) then begin
		widget_control,info.ccontmodeID,/destroy
		widget_control,info.isomodeID,/destroy
		widget_control,info.deltasmodeID,/destroy
		widget_control,info.rzcontmodeID,/destroy
		widget_control,info.rzslicemodeID,/destroy
;		widget_control,info.histomodeID,/destroy
		info.ncontID[0:1]=x
		info.phiID[0]=x
		info.zID[0]=x
		info.scaleID=[x,x]
		info.rzsliceID[0]=x
		info.rzvalueID[0]=x
		info.deltasID=x
		info.isotropicID[0]=x
		info.ccontmodeID=x
		info.isovalueID=x
	endif
	if FINITE(info.zmodeID) then begin
		widget_control,info.zmodeID,/destroy
		widget_control,info.rzmodeID,/destroy
		widget_control,info.nmodeID,/destroy
		widget_control,info.curlvmodeID,/destroy
                widget_control,info.v2smodeID,/destroy
		info.ncontID[2:3]=x
		info.phiID[1]=x
		info.zID[1]=x
		info.isotropicID[1]=x
		info.zmodeID=x
	endif
	if FINITE(info.smallID) then begin
		widget_control,info.smallID,/destroy
		for i=0,2 do widget_control,info.dfID[i],/destroy
		widget_control,info.areaID,/destroy
		widgeT_control,info.nbinsareaID,/destroy	
		info.smallID=x
		info.setareaID=[x,x,x,x,x]
	endif
	IF finite(info.eqbaseID) then begin
		widget_control,info.eqbaseID,/destroy
		widget_control,info.eqlabelID,/destroy
		info.eqlabelID=x
		info.eqbaseID=x
	endif
	info.nthID=widget_slider(event.top,event_pro='wdata_nth' $
		,maximum=20,/minimum,/scroll,title='Every nth particle:' $
		,value=info.nth)
	info.zsliceID=widget_base(event.top,/frame,/column $
		,/base_align_center)
	info.phisliceID=widget_base(event.top,/frame,/column $
		,/base_align_center)
	zlabelID=widget_label(info.zsliceID,value='Z=0 slice')
	dzID=widget_slider(info.zsliceID,event_pro='wdata_dz' $
		,maximum=100,/minimum,/scroll,title='dz (% Z length)' $
		,value=info.dz)
	zscatterID=widget_button(info.zsliceID,event_pro='wdata_zscatter' $
		,value='Produce scatter plot')
	philabelID=widget_label(info.phisliceID,value='Phi slice')
	info.phiID[2]=widget_slider(info.phisliceID,event_pro='wdata_phi' $
		,maximum=360,minimum=0,scroll=5,title='Phi' $
		,value=info.phi[2])
	info.phiID[3]=widget_slider(info.phisliceID,event_pro='wdata_phi' $
		,maximum=180,minimum=0,scroll=3,title='Delta phi' $
		,value=info.phi[3])
	projectID=widget_base(info.phisliceID,/column,/frame $
		,title='Interpolation Method:',/base_align_center)
	projlabelID=widget_label(projectID,/align_center $
		,value='Projection Method:')
	info.projectID=widget_list(projectID $
		,event_pro='wdata_project' $
		,value=['No projection','Angular projection'] $
		,ysize=2)
	widget_control,info.projectID,set_list_select=info.project
	phiscatterID=widget_button(info.phisliceID $
		,event_pro='wdata_phiscatter',value='Produce scatter plot')
	END
	4 : BEGIN ;equilibrium mode
	if info.mode eq 4 then begin
		widget_control,event.top,set_uvalue=info,/No_COpy
		return
	endif
	widget_control,info.timeID,map=0
	widget_control,info.drawID,draw_xsize=640,draw_ysize=580
	widget_control,info.xID,set_value=640
	widget_control,info.yID,set_value=680
	info.mode=4
	if FINITE(info.ccontmodeID) then begin
		widget_control,info.ccontmodeID,/destroy
		widget_control,info.isomodeID,/destroy
		widget_control,info.deltasmodeID,/destroy
		widget_control,info.rzcontmodeID,/destroy
		widget_control,info.rzslicemodeID,/destroy
;		widget_control,info.histomodeID,/destroy
                widget_control,info.v2smodeID,/destroy
		info.ncontID[0:1]=x
		info.phiID[0]=x
		info.zID[0]=x
		info.scaleID=[x,x]
                info.rzsliceID[0]=x
                info.rzvalueID[0]=x
		info.deltasID=x
		info.isotropicID[0]=x
		info.ccontmodeID=x
		info.isovalueID=x
	endif
	if FINITE(info.zmodeID) then begin
		widget_control,info.zmodeID,/destroy
		widget_control,info.rzmodeID,/destroy
		widget_control,info.nmodeID,/destroy
		widget_control,info.curlvmodeID,/destroy
                widget_control,info.v2smodeID,/destroy
		info.ncontID[2:3]=x
		info.phiID[1]=x
		info.zID[1]=x
		info.isotropicID[1]=x
		info.zmodeID=x
	endif
	if FINITE(info.smallID) then begin
		widget_control,info.smallID,/destroy
		for i=0,2 do widget_control,info.dfID[i],/destroy
		widget_control,info.areaID,/destroy
		widgeT_control,info.nbinsareaID,/destroy	
		info.smallID=x
		info.setareaID=[x,x,x,x,x]
	endif
	if FINITE(info.nthID) then begin
		widget_control,info.nthID,/destroy
		widget_control,info.zsliceID,/destroy
		widget_control,info.phisliceID,/destroy
		info.nthID=x
		info.phiID[2]=x
		info.phiID[3]=x
		info.projectID=x
	endif
	info.eqlabelID=widget_label(event.top,value='Analyze Equilibrium Data:')
	info.eqbaseID=widget_base(event.top,/column,/base_align_center)
	psiID=widget_button(info.eqbaseID,event_pro='wdata_psi' $
		,value='PSI')
	psi1ID=widget_button(info.eqbaseID,event_pro='wdata_psi1' $
		,value='PSI1')
	psi2ID=widget_button(info.eqbaseID,event_pro='wdata_psi2' $
		,value='PSI2')
	psi3ID=widget_button(info.eqbaseID,event_pro='wdata_psi3' $
		,value='PSI3')
        jpolID=widget_button(info.eqbaseID,event_pro='wdata_jpol' $
                ,value='Jpol')
	psi4ID=widget_button(info.eqbaseID,event_pro='wdata_psi4' $
		,value='PSI4')
        psi5ID=widget_button(info.eqbaseID,event_pro='wdata_psi5' $
                ,value='PSI5')
	rhoiID=widget_button(info.eqbaseID,event_pro='wdata_rhoi' $
		,value='RHOI')
        jiID=widget_button(info.eqbaseID,event_pro='wdata_ji' $
                ,value='JI_zr')
	f0ID=widget_button(info.eqbaseID,event_pro='wdata_f0' $
		,value='F0(lambda,eps)')
	END
ENDCASE
widget_control,event.top,/update
screen=get_screen_size()
widget_control,event.top,tlb_get_size=tlbsize
rest=tlbsize-[640,680]
widget_control,info.xID,set_slider_max=screen[0]-rest[0]
widget_control,info.yID,set_slider_max=screen[1]-rest[1]
widget_control,event.top,Set_Uvalue=info,/No_Copy
END

;#####################################################################
;#####################################################################
PRO gui1
;ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
;***Main module creates GUI

!P.charsize=1.

baseID=Widget_Base(title='Displaying Simulation Results' $
	,MBar=menubaseID,column=3)
fileID=Widget_Button(menubaseID,Value='File',/Menu)
printfileID=Widget_Button(fileID,Value='Print Next Graphic to File' $
	,Event_Pro='wdata_printnext')
printsID=Widget_Button(fileID,Value='Print Next Graphic to l230-t850' $
	,event_pro='wdata_printnext')
printID=Widget_Button(fileID,Value='Print Next Graphic to PRINTER' $
        ,event_pro='wdata_printnext')
quitID=Widget_Button(fileID,value='Quit',Event_Pro='wdata_quit')
dataID=Widget_Button(menubaseID,Value='Data',/Menu)
loadnewID=Widget_Button(dataID,value='Load New Data Set' $
	,Event_Pro='wdata_loadnew')
load1ID=Widget_Button(dataID,value='Load 1 Time-Slice of 4d Array' $
	,Event_Pro='wdata_load1')
convertID=Widget_Button(dataID,value='Convert to Cartesian' $
	,Event_Pro='wdata_convert')
getstatID=Widget_Button(dataID,value='Get Statistics' $
        ,Event_Pro='wdata_getstat')
colorID=Widget_Button(menubaseID,value='Colors',/Menu)
ctableID=Widget_Button(colorID,value='Change Colors' $
	,Event_Pro='wdata_ctable')
leftID=widget_base(baseID,/column)
drawID=Widget_Draw(leftID,Xsize=380,YSize=380)
dummyID=widget_base(leftID,column=2)
dummy2ID=widget_base(dummyID)
timeID=widget_label(dummy2ID,/frame,/dynamic_resize $
	,value='No data loaded yet.')
retainID=widget_list(dummyID,ysize=2,event_pro='wdata_retain' $
	,value=['Load New Color Table','Retain Color Table'])
deviceID=widget_list(dummyID,ysize=2,event_pro='wdata_device' $
        ,value=['FRC','Others'])
xID=widget_slider(dummyID,event_pro='wdata_resize',scroll=5 $
	,title='X Size',value=380,maximum=400,minimum=100)
yID=widget_slider(dummyID,event_pro='wdata_resize',scroll=5 $
	,title='Y Size',value=380,maximum=400,minimum=100)
modeID=widget_list(baseID,event_pro='wdata_mode' $
	,value=['Scalar','Vector','Particle','Scatter','Equilibrium'] $
	,ysize=4)
widget_control,baseID,/realize
widget_control,drawID,Get_Value=wid
widget_control,retainID,set_list_select=0
widget_control,deviceID,set_list_select=0
widget_control,baseID,tlb_get_size=array
rest=array-[380,380]
screen=get_screen_size()
widget_control,xID,set_slider_max=screen[0]-rest[0]
widget_control,yID,set_slider_max=screen[1]-rest[1]
WSet,wid
x=!Values.F_NaN
info= { wid:wid, $
	fileID:fileID, $
	quitID:quitID, $
	convertID:convertID, $
	getstatID:getstatID, $
	colorID:colorID, $
	loadnewID:loadnewID, $
	load1ID:load1ID, $
	dataID:dataID, $
	ctableID:ctableID, $
	timeID:timeID, $
	isovalueID:x, $
	ncontID:[x,x,x,x], $
	ncont:[20,20,20,20], $
	z:[50,50], $
	zID:[x,x], $
	cinterp:0, $
	isotropic:[1,1], $
	isotropicID:[x,x],$
	scale:[0,0,0], $
	scaleID:[x,x,x], $
	rzslice:[0,0], $
        rzsliceID:[x,x], $
        rzvalue:[50,50], $
        rzvalueID:[x,x], $
        deltas:0, $
	deltasID:x, $
	mode:x, $
	phi:[0,0,0,10], $
	phiID:[x,x,x,x], $
	nmode:1, $
	curlv:0, $
	curlvID:x, $
	curlvmodeID:x, $
	v2s:0, $
	v2sID:x, $
	v2smodeID:x, $
	iso:0.5, $
	component:0, $
        printsID:printsID, $
	printID:printID, $
	printfileID:printfileID, $
	small:1, $
	printnext:0, $
	isomodeID:x, $
	deltasmodeID:x, $
	rzcontmodeID:x, $
	rzslicemodeID:x, $
	ccontmodeID:x, $
	histomodeID:x, $
	framebaseID:x, $
	frameID:x, $
	rzmodeID:x, $
	zmodeID:x, $
	nmodeID:x, $
	setareaID:[x,x,x,x,x,x], $
	area:[0,100,0,100,0,100], $
	areaID:x, $
	smallID:x, $
	dfID:[x,x,x],$
	dflabelID:[x,x,x], $
	computeID:[x,x,x], $
	draw1ID:[[x,x,x],[x,x,x],[x,x,x]], $
	draw2ID:[[x,x,x],[x,x,x],[x,x,x]], $
	eqlabelID:x, $
	eqbaseID:x, $
	nbinsareaID:x, $
	nbins:[25,25,25], $
	nbinsID:[x,x,x], $
	ncolors:!D.n_colors, $
	bottom:0, $
	retain:0, $
	device:0, $
	drawID:drawID, $
	xID:xID, $
	yID:yID, $
	nth:5, $
	nthID:x, $
	zsliceID:x, $
	phisliceID:x, $
	dz:5, $
	projectID:x, $
	project:1}
Widget_Control,baseID,Set_Uvalue=info,/No_Copy
Xmanager,'gui',baseID,/No_Block $
	,cleanup='wdata_cleanup'

END
