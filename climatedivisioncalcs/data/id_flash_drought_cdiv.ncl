; author:
; purpose:

load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/shea_util.ncl"
load "func_percentile1d_selfrelative.ncl"

; flash drought thresholds

changethresh = 60.0
botthresh = 20.0
topthresh = 30.0
window = 30
length = 30

; times to get

btime = 19160101
etime = 20171231

; get climate division information

fname = "~/Desktop/data/climatedivision/climate_division_info.txt"
stateid = stringtofloat(systemfunc("cut -c1-3 " + fname))
statena = systemfunc("cut -c5-6 " + fname)
numdiv = stringtointeger(systemfunc("cut -c9-11 " + fname))
delete(fname)

; make calculation

opt = True

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

      in1 = asciiread("/Users/ahoell/Desktop/data/uw_lsm/vic/division_percentile/sm_percentile_"+strstateid+cdivid+".txt",-1,"float")
      in2 = onedtond(in1,(/dimsizes(in1)/4,4/))
      varin = in2(:,3)
      yyyy = toint(in2(:,0))
      mm = toint(in2(:,1))
      dd = toint(in2(:,2))
      yyyymmdd = yyyy*10000+mm*100+dd
      nt = dimsizes(yyyymmdd)
 
      opt@fout = "division_flash_"+changethresh+"/flash_"+strstateid+cdivid+".txt"
 
      nn=0
      count=0
      do while ( nn.le.nt-1-window-length )
    
        ss1 = varin(nn)
        ss2 = varin(nn+window-1)
        ss3 = varin(nn+window-1:nn+window-1+length)
        sdiff = ss1-ss2
      
        if ( ss2.le.botthresh.and.sdiff.ge.changethresh.and.all(ss3.lt.topthresh) ) then
      
          nn=nn+window+length
          count=count+1
      
        else
      
          nn=nn+1
        
        end if
      
        delete(ss1)
        delete(ss2)
        delete(ss3)
        delete(sdiff)
    
      end do 
     
; part of code serves as the output

      if ( count.eq.0 ) then

        varout = new((/1,2/),integer)
        varout(0,:) = 0
        write_matrix(varout,"2I10",opt)
        delete(varout)    
    
      else

      varout = new((/count,2/),integer)

        nn=0
        count=0
        do while ( nn.le.nt-1-window-length )
    
          ss1 = varin(nn)
          ss2 = varin(nn+window-1)
          ss3 = varin(nn+window-1:nn+window-1+length)
          sdiff = ss1-ss2
      
          if ( ss2.le.botthresh.and.sdiff.ge.changethresh.and.all(ss3.lt.topthresh) ) then
      
            varout(count,0) = yyyymmdd(nn)
            varout(count,1) = yyyymmdd(nn+window-1)
            nn=nn+window+length
            count=count+1
      
          else
        
            nn=nn+1
      
          end if
        
          delete(ss1)
          delete(ss2)
          delete(ss3)
          delete(sdiff)
    
        end do
    
        write_matrix(varout,"2I10",opt)
        delete(varout) 
    
      end if
   
      delete(in1)
      delete(in2)
      delete(varin)
      delete(yyyy)
      delete(mm)
      delete(dd)
      delete(yyyymmdd)
      delete(cdivid)
   
    end if
  
  end do

  delete(strstateid)

end do











