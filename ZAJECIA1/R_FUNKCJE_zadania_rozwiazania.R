#1
kostka= function(n){
    rzuty = sample(1:6,n, replace=TRUE)
    return(rzuty)
}

#2
 stworz_wektor= function(n){
    wektor=1:n
    return(wektor)
 }

#3
  pole_kola= function(r){
    pole= pi *r^2
    return(pole)
  }

#4
  przeciwprostokatna = function(a,b){
    c=sqrt(a^2 + b^2)
    return(c)
  }
  
#5
kalkulator = function(a, b, operacja) {
  if (operacja == "+") {
    wynik = a + b
  } else if (operacja == "-") {
    wynik = a - b
  } else if (operacja == "*") {
    wynik = a * b
  } else if (operacja == "/") {
    if (b != 0) {
      wynik = a / b
    } else {
      wynik = "Nie można dzielić przez zero!"
    }
  } else {
    wynik = "Nieznana operacja!"
  }
  return(wynik)
}

#6
przyznaj_nagrode = function() {
  rzut = sample(1:6, 1, replace = TRUE)
  if (rzut == 6) {
    nagroda = "Super bonus!"
  } else if (rzut == 4 || rzut == 5) {
    nagroda = "Nagroda standardowa"
  } else {
    nagroda = "Brak nagrody..."
  }
  return(nagroda)
}

#7
oblicz_podatek = function(dochod, sposob_rozliczenia) {
  if (sposob_rozliczenia == "liniowy") {
    podatek = dochod * 0.19
  } else if (sposob_rozliczenia == "ogolne") {
    if (dochod <= 85528) {
      podatek = dochod * 0.18 - 556
    } else {
      nadwyzka = dochod - 85528
      podatek = 14839 + nadwyzka * 0.32
    }
  } else {
    podatek = "Nieznany sposób rozliczenia!"
  }
  return(podatek)
}
