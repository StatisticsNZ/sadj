#Changed for SAS Grid upgrade October 2019 S Dorsey
# Changed for X13 compliance in Feb 2014 by Conrad MacCormick
# 15/10/2020 Steve White
# Added ao2020.3 to regression variables

series{
  file='/nas/corporate/snzdata/prd/seasadj/hlfs/d/munemp.dat'
  period=4
  title='Males Unemployed'
  name='MUNEMP'
  span=(1986.1,)
  save=(b1)
  format=datevalue
}

x11{
  sigmalim=(1.8 2.8)
  seasonalma=(msr)
  mode=mult
  print=(default f3 qstat)
  save=(c17 d8 d9 d10 d11 d12 d13)
}

transform{
  function=log
}

arima{
  model=(0 1 1)(0 1 1)
}

estimate {}

regression{
  variables=(ao2020.2 ao2020.3)
  save=ao
}
