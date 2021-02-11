#Changed for SAS Grid upgrade October 2019 S Dorsey
# Changed for X13 compliance in Feb 2014 by Conrad MacCormick
#Updated 30/10/13 to include prior adjustment factors
#Conrad MacCormick
# 15/10/2020 Steve White
# Added ao2020.3 to regression variables

series{
  start=1986.1
  period=4
  title='fnlf'
  file='/nas/corporate/snzdata/prd/seasadj/hlfs/d/fnlf.dat'
  save=(b1)
  format=datevalue
}

x11{
  mode=mult
  sigmalim=(1.8,2.8)
  save=(c17 d8 d9 d10 d11 d12 d13)
  print=(default f3 qstat)
}

transform{
  type=permanent
  file='/nas/corporate/snzdata/prd/seasadj/hlfs/f/fnlf.fac'
  mode=ratio
  function=log
  format=datevalue
}

arima{
  model=(0 1 1)(0 1 1)
}

estimate {}

regression{
  variables=(ao2020.2 ao2020.3)
  save=ao
}
