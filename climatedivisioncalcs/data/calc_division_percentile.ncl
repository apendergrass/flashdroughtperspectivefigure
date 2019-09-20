; author: andy
; purpose: calculate climate division percentile

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "func_percentile1d_selfrelative.ncl"

; get climate division information

fname = "~/Desktop/data/climatedivision/climate_division_info.txt"
stateid = stringtofloat(systemfunc("cut -c1-3 " + fname))
statena = systemfunc("cut -c5-6 " + fname)
numdiv = stringtointeger(systemfunc("cut -c9-11 " + fname))
delete(fname)

; dummy dates

dum = yyyymmdd_time(2000,2000,"integer")
mmdd_dum = dum - dum/10000*10000

; calculations

do is = 0,dimsizes(stateid)-1
  
  print(statena(is))
  
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

      in1 = asciiread("/Users/ahoell/Desktop/data/uw_lsm/vic/division_raw/div.vic_soil."+strstateid+cdivid+".txt",-1,"float")
      in2 = onedtond(in1,(/dimsizes(in1)/4,4/))
      varin = in2(:,3)
      yyyy = in2(:,0)
      mm = in2(:,1)
      dd = in2(:,2)      
      mmdd = mm*100+dd

      pct = varin

      do it = 0,dimsizes(mmdd_dum)-1

        if ( mmdd_dum(it).eq.0229 ) then
  
          ii1 = ind(mmdd_dum(it).eq.mmdd)
          ii2 = ind(0228.eq.mmdd)
          ii = new((/dimsizes(ii1)+dimsizes(ii2)/),integer)
          ii(0:dimsizes(ii1)-1) = ii1
          ii(dimsizes(ii1):dimsizes(ii1)+dimsizes(ii2)-1) = ii2
          aa = varin(ii)
          bb = func_percentile1d_selfrelative(aa)
          pct(ii1) = bb(0:dimsizes(ii1)-1)
          delete(aa)
          delete(bb)
          delete(ii)
          delete(ii1)
          delete(ii2)
        
        else

          ii = ind(mmdd_dum(it).eq.mmdd)
          aa = varin(ii)
          pct(ii) = func_percentile1d_selfrelative(aa)
          delete(aa)
          delete(ii)
        
        end if
    
      end do
      
      varout = in2
      varout(:,3) = pct
      
      opt = True
      opt@fout = "division_percentile/sm_percentile_"+strstateid+cdivid+".txt"
      write_matrix(varout,"4f12.2",opt)
      
      delete(varout)
      delete(varin)
      delete(yyyy)
      delete(mm)
      delete(dd)
      delete(mmdd)
      delete(pct)
      delete(in1)
      delete(in2)
      delete(cdivid)
   
    end if
  
  end do

  delete(strstateid)

end do



