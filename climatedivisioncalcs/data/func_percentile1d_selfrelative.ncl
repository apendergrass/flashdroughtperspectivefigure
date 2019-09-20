; calculates percentiles for 1d series
; percentile = (100 * rank) / (n+1)

undef("func_percentile1d_selfrelative")
function	func_percentile1d_selfrelative(varin1)

begin

;local varin1, varout, control, controlsort, controlrank, cc, controlranksort, target, targetrank,

; calculates the rank of the distribution to compare to 
        
control = varin1
controlsort = control
qsort(controlsort)
controlrank = new((/dimsizes(controlsort)/),float)
do i=0,dimsizes(controlrank)-1
  cc = ind(control(i).eq.controlsort)
  controlrank(i) = cc(0)+1
  delete(cc)
end do
      
varout = ( 100.0 * controlrank ) / ( dimsizes(controlrank)+1 )

delete(control)
delete(controlsort)
delete(controlrank)

return(varout)
		
end