;; name: andy hoell
;; description: plots u.s. climate divisions

;; libraries

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
 
; contour levels

thresh = (/40,50,60/)
clevs = (/0,10,20,30,40,50,60,70/)
textbar = (/"0","10","20","30","40","50","60","70"/)
colors = (/0,2,3,4,5,6,7,8/)

; get climate division information

fname = "/mnt/c/Data/data/climatedivision/climate_division_info.txt"
stateid = stringtofloat(systemfunc("cut -c1-3 " + fname))
statena = systemfunc("cut -c5-6 " + fname)
numdiv = stringtointeger(systemfunc("cut -c9-11 " + fname))
delete(fname)

; make calcs

ndrought = new((/dimsizes(thresh),12,dimsizes(stateid),10/),float)

do it=0,dimsizes(thresh)-1

do is = 0,dimsizes(stateid)-1
    
  if ( stateid(is).lt.10 ) then
    strstateid = "0"+tostring(toint(stateid(is)))
  else
    strstateid = tostring(toint(stateid(is)))
  end if
        
  nc = numdiv(is)

  do ic = 1,nc
   
    if ( statena(is).eq."NE".and.ic.eq.4 ) then ; nebraska has no climate division 4
    
    
    else

      if ( ic.lt.10 ) then
        cdivid = "0"+tostring(ic)
      else
        cdivid = tostring(ic)
      end if
      
      in1 = asciiread("/mnt/c/Data/data/uw_lsm/vic/division_flash_"+thresh(it)+"/flash_"+strstateid+cdivid+".txt",-1,"integer")
      in2 = onedtond(in1,(/dimsizes(in1)/2,2/))
      mm = in2(:,0)
      mm = mm/100-mm/10000*100

      if ( all(in2.eq.-999) ) then
    
    
      else
  
        if (all(in2.eq.0) ) then
    
          ndrought(it,:,is,ic-1) = 0.0
    
        else
    
          ndrought(it,:,is,ic-1) = 0.0
      
          do im=0,dimsizes(mm)-1
            ndrought(it,toint(mm(im))-1,is,ic-1) = ndrought(it,toint(mm(im))-1,is,ic-1)+1.0
          end do
          
        end if
        
      end if
      
      delete(in1)
      delete(in2)
      delete(mm)
      
    end if
  
  end do
  
end do

end do

adrought = dim_sum_n(ndrought,1)


; make plots

bfile = addfile("/mnt/c/Data/data/climatedivision/climdiv_polygons.nc","r")

wks_type = "pdf" 
wks_type@wkOrientation = "landscape" 
wks = gsn_open_wks(wks_type,"cdiv_annual_flash")
;gsn_merge_colormaps(wks,"MPL_accent","precip_diff_12lev")
plot = new(3,graphic)
plot1 = plot
plot2 = plot
pp = new(10000,graphic)
 
cmap1 = (/(/255.0,255,255/), \
          (/0, 0, 0/), \
(/255,247,188/), \
(/254,227,145/), \
(/254,196,79/), \
(/254,153,41/), \
(/236,112,20/), \
(/204,76,2/), \
(/140,45,4/)/)        
               
cmap1 = cmap1 / 255.0
 
gsn_define_colormap(wks,cmap1) 
 
 
 
; map resources

res = True
pnlres = True

res@gsnFrame = False
res@gsnDraw = False
res@gsnMaximize = True  

res@mpLambertParallel1F = 33.0         ; two parallels
res@mpLambertParallel2F = 45.0
res@mpLambertMeridianF  = -95.0        ; central meridian
res@mpLimitMode         = "LatLon"
res@mpMinLatF           = 24.0         ; map area
res@mpMaxLatF           = 50.0         ; latitudes
res@mpMinLonF           = -120.0       ; and
res@mpMaxLonF           = -72.0        ; longitudes
  
res@mpDataBaseVersion     = "MediumRes"
res@mpFillOn              = True
res@mpPerimOn             = False
res@mpAreaMaskingOn       = True  
res@mpFillAreaSpecifiers  =(/"Water","Land"/) 
res@mpSpecifiedFillColors = (/0,0/) 
res@mpOutlineBoundarySets = "USStates"
res@mpMaskAreaSpecifiers  =(/"Conterminous US"/)
res@mpGridAndLimbOn       = False
  
; polygon resources

res_fill             = True
res_fill@gsEdgesOn   = False   
res_fill@tfPolyDrawOrder = "PreDraw"

; polyline resources

res_line             = True
res_line@gsEdgesOn   = False
res_line@gsLineThicknessF = 0.5
res_line@gsLineColor = "gray25"   
res_line@tfPolyDrawOrder = "PreDraw"

; increase viewport size
  
;res@vpWidthF  = 0.9
;res@vpHeightF = 0.9
;res@vpXF      = 0.05
;res@vpYF      = 0.90
  
res@tfPolyDrawOrder = "PostDraw"

res@tiMainFont = 22
res@tiMainFontHeightF = 0.022
res@tiMainOffsetYF = -0.005


ppcnt=0
do ip=0,2

res@tiMainString = thresh(ip)+" Percentile Decrease"
plot(ip) = gsn_map (wks,"LambertConformal",res)
 
do is = 0,dimsizes(stateid)-1

  nc = numdiv(is)

  cc = 1
  do ic = 1,nc
    
    if ( statena(is).eq."NE".and.ic.eq.4 .or. statena(is).eq."FL".and.ic.eq.7 ) then ; nebraska has no climate division 4
    
    else

      cdivid = tostring(cc) 

      varname = statena(is)+"_CD"+cdivid
      varin = bfile->$varname$

      x = varin@lon
      y = varin@lat
      
      pp(ppcnt) = gsn_add_polyline(wks,plot(ip),x,y,res_line)
      ppcnt=ppcnt+1

      pvalue = adrought(ip,is,ic-1)

      if ( pvalue.eq.clevs(0).or.pvalue.gt.clevs(dimsizes(clevs)-1) ) then 
      
        res_fill@gsFillColor = colors(0)
        
      else
      
        do il=1,dimsizes(clevs)-1
          if ( pvalue.gt.clevs(il-1).and.pvalue.le.clevs(il) )
            res_fill@gsFillColor = il+1
          end if
        end do
        
      end if

      pp(ppcnt) = gsn_add_polygon(wks,plot(ip),x,y,res_fill)
      ppcnt=ppcnt+1

      delete(res_fill@gsFillColor)
      delete(pvalue)
      delete(x)
      delete(y)

      cc=cc+1

    end if
    
  end do
end do

end do



labelres = True
labelres@gsEdgesOn   = True

txres = True
txres@txFontHeightF = 0.011
txres@txFont = 21


bx=0.26
lx=0.06
by=0.32
ly=0.0175

do i=0,dimsizes(clevs)-1
  xbox = (/bx,bx+lx,bx+lx,bx,bx/)
  ybox = (/by,by,by+ly,by+ly,by/)
  labelres@gsFillColor = colors(i)
  gsn_polygon_ndc(wks,xbox,ybox,labelres)
  gsn_text_ndc(wks,textbar(i),bx+lx,by-0.0125,txres)
  delete(ybox)
  delete(xbox)
  bx=bx+lx
end do

txres@txJust = "CenterCenter"
gsn_text_ndc(wks,"Number of Occurrences During 1916-2017",0.50,by+ly+0.0125,txres)

;pnlres@txString = targetmonthname+" Drought Onset"
;pnlres@txFontHeightF = 0.017
;pnlres@txFont = 22
pnlres@gsnPanelCenter = True

;pnlres@gsnPanelYWhiteSpacePercent = 5
;pnlres@gsnPanelXWhiteSpacePercent = 5

;drawNDCGrid (wks)

;pnlres@gsnPanelYF = (/0.05,0.52,0.05,0.52,0.05,0.52/)
;pnlres@gsnPanelXF = (/-1,0.18/)

gsn_panel(wks,plot,(/1,3/),pnlres)