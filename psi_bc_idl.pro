; idl code to read and plot boundary flux data from Luca 
; Saves normalized flux in psi_bc.dat to be read into tkin_read.p

pro psi_bc_idl

filename='Luca_RZ_out.dat'

nz=50
nr=50
nt=2*nr+2*nz-3

array=fltarr(3,nt)

r=fltarr(nr)
z=fltarr(nz)
psi=fltarr(nz,nr)

openr,lun,filename,/get_lun
readf,lun,array
close,lun
free_lun,lun

; Check the box size in input file.
print,''
print,'RZ_out.dat: rmin,rmax(m)=',min(array(0,*)),max(array(0,*))
print,'RZ_out.dat: zmin,zmax(m)=',min(array(1,*)),max(array(1,*))
print,''

; Change sign of psi and normalization to make consistent with HYM.
;ul= 0.0763           ;m         for 203262-220
;ub= 0.587            ;T
;ul= 0.0562           ;m         for 204707-440
;ub= 0.546            ;T
 ul= 0.0482           ;m         for 141711-470
 ub= 0.4545           ;T
upsi= ub*ul^2       ;Wb

rt= array(0,*)/ul
zt= array(1,*)/ul
psit= -array(2,*)/upsi   ; sign!!! 

; TMP re-scale for 1st try ~ I_tot.
;psit= psit*1.22

print,'  unit_psi=',upsi
print,'  max/min(psi)=',max(psit),min(psit)

r=rt((nr+nz-2):(2*nr+nz-3))
z=zt((nr-1):(nr+nz-2))

print,'  rmin,rmax=',min(r),max(r)
print,'  zmin,zmax=',min(z),max(z)

indz=nz-1-indgen(nz)
indr=nr-1-indgen(nr)
psi(*,*)=0.
psi(0,indr)=psit(0:(nr-1))
psi(nz-1,*)=psit((nr+nz-2):(2*nr+nz-3))
l1=psi(0,0)
l2=psi(0,nr-1)
r1=psi(nz-1,0)
r2=psi(nz-1,nr-1)

psi(*,0)=psit((nr-1):(nr+nz-2))
psi(indz,nr-1)=psit((2*nr+nz-3):(nt-1))
psii=psi

;==== Make symmetric in z.===============
tmp= 0.5*( psi(0,*)+psi(nz-1,*) )
psi(0,*)=tmp
psi(nz-1,*)=tmp
for i=0,nz/2-1 do begin
  r1= 0.5*( psi(i,0)    + psi(nz-1-i,0) )
  r2= 0.5*( psi(i,nr-1) + psi(nz-1-i,nr-1) )
  psi(i,0)= r1
  psi(nz-1-i,0)=r1
  psi(i,nr-1)=r2
  psi(nz-1-i,nr-1)=r2
endfor
print,'making psi symmetric'
print,' max/min(psi)=',max(psi),min(psi)
print,'making psi>=0'
;psi= psi-min(psi)
print,' max/min(psi)=',max(psi),min(psi)

; Interpolate to my grid.
nid=120
njd=60
mynz= 2*(nid+4-1)+1            ;mynz=295
mynr= 2*(njd+4-1)+1            ;mynr=247
psi_low=interpol(reform(psi(*,0   )),mynz)
psi_hi =interpol(reform(psi(*,nr-1)),mynz)
psi_le =interpol(reform(psi(0,*   )),mynr)
psi_ri =interpol(reform(psi(nz-1,*)),mynr)
zi=interpol(z,mynz)
ri=interpol(r,mynr)

; Check
;plot,z,psi(*,nr-1)
;oplot,zi,psi_hi
;
;plot,z,psi(*,nr-1)
;oplot,z,psi(*,0)
;plot,r,psi(nz-1,*)
;oplot,r,psi(0,*)

; Smooth psi_low with square fit.
zc=max(z)
f0=psi_low(mynz/2)
f1=psi_low(0)
f2=psi_low(mynz-1)
a0=f0
a1=0.5*(f2-f1)/zc
a2=(-a0 + 0.5*(f1+f2))/zc^2
f= a0 + a1*zi + a2*zi^2
; turn off for larger R_i.
 psi_low= f                 ; Smoothed psi at r=ri

;=============================================
; Make few plots
!p.multi=[0,2,2]
!p.charsize=1.0
loadCT,4

plot,z,psii(*,nr-1),/xsty,yra=[min(psit),max(psit)],xtitle='Z',title='from FREE_FIX'
oplot,z,psii(*,0),color=200
plot,r,psii(nz-1,*),/xsty,xtitle='R',title='from FREE_FIX'
oplot,r,psii(0,*),color=200

plot,zi,psi_hi,/xsty,yra=[min(psit),max(psit)],title='modified for tkin_read.p'
oplot,zi,psi_low,color=200
plot,ri,psi_le,/xsty,title='modified for tkin_read.p'
oplot,ri,psi_ri,color=200

!p.multi=0
;=============================================

; Check corners.
print,'Check corners'
print,psi_low(0),psi_le(0)
print,psi_le(mynr-1),psi_hi(0)
print,psi_hi(mynz-1),psi_ri(mynr-1)
print,psi_ri(0),psi_low(mynz-1)

; Save to be read into tkin_read.p
filename='psi_bc.dat'
openw,lun,filename,/get_lun
printf,lun, mynz,mynr
printf,lun, zi(0),zi(mynz-1),ri(0),ri(mynr-1)
printf,lun, psi_low
printf,lun, psi_hi
printf,lun, psi_le
printf,lun, psi_ri
close,lun
free_lun,lun

end
