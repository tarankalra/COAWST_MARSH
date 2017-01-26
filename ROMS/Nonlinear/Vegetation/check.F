             program hello
              implicit none 
              real :: FY 
              real :: cff, cff1 

              FY=-0.0
              cff=SIGN(1.0,FY)
              cff1=MAX(cff,0.0)

              print *,cff,cff1
             end program 
