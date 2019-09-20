       program gcon
c
       real data(5684)
       real pinc, tinc, t42lg, t42lt, seas(1916:2018,12,31,344)
       real out(12), grid(116,49)
       real xdat(116,49,9)
       real dgrid(344,100)
       integer lgrid(344,100), min, max
       integer state(344), div(344), vn(344)
       integer year(1916:2018,12)
       integer monthday(12)
c
c
       open(1,file='116x49.to.div.info')
       open(3,file='/projects/lsm/vic_soilliq3.dly.1916.2018_06')
c
       monthday(1) = 31
       monthday(2) = 28
       monthday(3) = 31
       monthday(4) = 30
       monthday(5) = 31
       monthday(6) = 30
       monthday(7) = 31
       monthday(8) = 31
       monthday(9) = 30
       monthday(10) = 31
       monthday(11) = 30
       monthday(12) = 31
c
       do ndv = 1, 344
        read(1, '(3i5)') state(ndv), div(ndv), n
        read(1, '(12i6)') (lgrid(ndv,i),i=1,n)
        read(1, '(12f6.1)') (dgrid(ndv,i),i=1,n)          
        vn(ndv) = n
       enddo
c
       do iy = 1916, 2018
        do im = 1, 12
         do id = 1, 31
          do idv = 1, 344
          seas(iy,im,id,idv) = -99.
          enddo
         enddo
        enddo
       enddo
c
       open(2,file='/projects/lsm/vic/lsm_vic.dly.191601_201806',
     +form='unformatted',
     +access='direct',recl=116*49*9*4)
c
       nrec = 0
       do iy = 1916, 2018
c
       mth = 12
       if(iy .eq. 2018) mth = 6
       do im = 1, mth
c
       if(mod(iy,4) .eq. 0) then
         if(mod(iy,100) .eq. 0.0) then
          monthday(2) = 28
          if(mod(iy,400) .eq. 0) monthday(2) = 29
         else
          monthday(2) = 29
         endif
        else
         monthday(2) = 28
        endif
c
        do k = 1, 12
         year(iy,k) = monthday(k)
        enddo
c
        mth = 12
        if(iy .eq. 2018) mth = 6
c
         do id = 1, monthday(im)
         nrec = nrec + 1
         read(2,rec=nrec) (((xdat(j,k,i),j=1,116),k=1,49),i=1,9)
         print*, iy, im, id, nrec
         do j = 1, 116
          do k = 1, 49
           grid(j,k) = xdat(j,k,8)
          enddo
         enddo
c
       n = 1
       do k = 1, 49
        do j = 1, 116
         data(n) = grid(j,k)
         n = n + 1
        enddo
       enddo
c
         do ndv = 1, 344
         xx = 0.0
         xxc = 0.0
         n = vn(ndv)
         do i = 1, n
          if(dgrid(ndv,i) .lt. 5.) dgrid(ndv,i) = 5.
          if(dgrid(ndv,i) .gt. 200.) goto 17
          if(data(lgrid(ndv,i)) .gt. -90.) then
          xx = xx + (data(lgrid(ndv,i)) * (1./(dgrid(ndv,i)**2.)))
          xxc = xxc + (1. / (dgrid(ndv,i)**2.))
          endif
 17       continue
         enddo
         if(xxc .gt.  0.0) then
          xx = xx / xxc
         else
          xx = -99.
         endif
         seas(iy,im,id,ndv) = xx
c
         enddo
c.. day
        enddo
c.. month
       enddo
c.. year
       enddo
c
       do ndv = 1, 344
        do iy = 1916, 2018
         mth = 12
         if(iy .eq. 2018) mth = 6
         do im = 1, mth
          n = year(iy,im)
       write(3,18) state(ndv),div(ndv),iy,im,n,(seas(iy,im,i,ndv),i=1,n)
 18    format(2i2,i5,2i3,31f7.1)
         enddo
        enddo
       enddo
c
       end
