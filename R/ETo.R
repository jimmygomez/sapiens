#' FAO56 Penman-Monteith method
#'
#' @description Function use for calculate the reference evapotranspiration
#' @param data data frame with the metrerological information
#' @param day  date of the weather measure (yyy-mm-dd)
#' @param Tmin  Temperatura minima (ºC)
#' @param Tmax  Temperatura maxima (ºC)
#' @param RHmin Humedad relativa minima (%)
#' @param RHmax Humedad relativa maxima (%)
#' @param n     sun hours (h)
#' @param v     wind velocity (m.s-1)
#' @param lat   latitud en radianes decimales (numeric)
#' @param alt   altitud (msnm)
#' @param z     Heigth of data collect for metereological station (m)
#' @param alb    Albedo
#' @return evapotranspiration (ET) by days
#' @importFrom lubridate yday
#' @export


PenMon <- function(data, day,
               Tmin, Tmax,
               RHmin, RHmax,
               n, v,
               lat, alt,
               z , alb = 0.23){


  data <- as.data.frame(data)

  day <- as.Date(data[, day])
  Tmin <- data[, Tmin]
  Tmax <- data[, Tmax]
  RHmin <- data[, RHmin]
  RHmax <- data[, RHmax]
  n <- data[,n]
  v <- data[,v]
  lat <- data[,lat]
  alt <- data[,alt]
  z <- data[,z]


  # wind <> 2 m -------------------------------------------------------------

  # Ec. 47

  uz <- v*((4.87)/(log(67.8*z-5.42)))


  # Temperature media -------------------------------------------------------

  # Ec. 9

  Tm <- (Tmax + Tmin)/2

  # pressure ----------------------------------------------------------------

  # Ec. 7

  P <-  101.3*((293-0.0065*alt)/(293))


  # Constante psicrometrica -------------------------------------------------

  # Ec. 8

  kp <- (1.013*10^(-3)*P)/(2.45*0.622)


  # Pendiente de la curva de presion de saturacion de vapor -----------------

  # Ec. 13

  psv <- (4098*(0.6108*exp((17.27*Tm)/(Tm+237.3))))/(Tm+237.3)^2


  # Deficit de presion de vapor ---------------------------------------------

  # Ec. 11

  tx <- 0.6108*exp((17.27*Tmax)/(Tmax+237.3))
  tm <- 0.6108*exp((17.27*Tmin)/(Tmin+237.3))

  es <- (tx + tm)/2

  ea <- (tm*(RHmax/100)+tx*(RHmin/100))/2

  # Ec. 17

  dpv <- es - ea


  # Radiation ---------------------------------------------------------------


  J <- lubridate::yday(day)

  # Ec. 23

  dr <- 1 + 0.033 * cos((2*pi/365)*J)

  # Ec. 24

  ds <- 0.409 * sin(((2*pi)/(365))*J - 1.39)

  # Ec. 25

  ws <- acos(-tan(lat)*tan(ds))

  # Ec. 21

  Ra <- ((24*60)/pi)*0.082*dr*(ws*sin(lat)*sin(ds)+cos(lat)*cos(ds)*sin(ws))


  # Ec. 34

  N <- (24/pi)*ws

  # Ec. 35

  Rs <- (0.25 + 0.5*(n/N))*Ra

  # Ec. 37

  Rso <- (0.75+2*10^(-5)*alt)*Ra

  # Ec. 38

  Rns <- (1 - alb)*Rs

  # Ec. 39

  Rnl <-  ((4.903*10^(-9))*(((Tmax+273.16)^4 + (Tmin+273.16)^4)/2))*(0.34-0.14*sqrt(ea))*(1.35*(Rs/Rso)-0.35)

  # Ec. 40

  Rn <- Rns - Rnl

  # Ec. 42
  # Gday = 0

  a <- psv/(psv+kp*(1+0.34*uz))

  b <- kp/(psv+kp*(1+0.34*uz))

  c <- (900/(Tm + 273))*uz


  et <- 0.408*Rn*a + c*dpv*b


}
