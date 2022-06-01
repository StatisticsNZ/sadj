#  Comments get parsed correctly

series{ title= 'Example Quarterly Series'
        comptype=add
        period=4
        span=(1992.3,)
        file= 'exquarterly.dat'
        save=(b1)
        format='datevalue'
}

x11{ sigmalim=(1.8 , 2.8 )
     mode=mult
     print=( ftestb1  c17  d10  d11  d12  d8  ftestd8  replacsi  movseasrat  residualseasf
x11diag  qstat
)
     save=( c17  d8  d9  d10  d11  d12  d13 )
}

transform{function = log}
regression{ variables=( td1nolpyear ao2020.2)   save=ao}
arima{ model = (0 1 1)(0 1 1)4}
estimate{ }
forecast{ }
