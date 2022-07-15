potencia n e = if e == 0 then 1 else n * potencia n (e-1)

potencia' n e | e == 0 = 1
              | e > 0 = n * potencia' n (e-1)
