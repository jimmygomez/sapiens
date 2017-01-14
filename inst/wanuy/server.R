# wanuy -------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(dplyr)
library(DT)
library(leaps)
library(scatterplot3d)
library(flashClust)
library(lubridate)


shinyServer(function(input, output) {

ft <- sapiens::getData(dir = "https://docs.google.com/spreadsheets/d/1XBezmc0S1T0EQj7zuZjTAOlPXosdDgTTotJK2jPAQAI/edit#gid=1020115876")


fert <- ft %>%
  dplyr::select(fertilizante, elemento, porcentage) %>%
  tidyr::spread(key = elemento, value = porcentage)

output$frt <- DT::renderDataTable(fert, server = F, filter = 'top', option = list(pageLength = 50, autoWidth = F))

# select row & colums: https://rstudio.github.io/DT/shiny.html

output$sfrt = renderPrint({
  s = input$frt_rows_selected
  if (length(s)) {
    cat(fert$fertilizante[s], sep = '\n')
  }
})


})


#--------------developer bucles------------------

#calculo de fertilizantes compuestos,

# ETAPA1, INGRESO DE EXIGENCIAS Y RESUMEN DEL LOS ARREGLOS
  Nitrogeno<-0;
  Phosphorus<-10;
  Potassium<-20;
  Ca<-10;
  Mg<-80;
  Zn<-0;
  B<-10;
  Cu<-0

  #EXIGENCIAS nota A:array ############
A<-c(N,P,K,Ca,Mg,Zn,B,Cu)

l_n<-6;   l_p<-2;   l_k<-1;   l_ca<-9;   l_mg<-10;   l_zn<-10;   l_b<-0;   l_cu<-0 # Ingresamos la ley.
ley1<-c(l_n,l_p,l_k,l_ca,l_mg,l_zn,l_b,l_cu)/100;comment(ley1)<-'ley1 : c(N,P,K,Ca,Mg,Zn,B,Cu) en % '

pc1<-length(A[A>0]);pc1

#PC1 :  1er punto de control
#if(pc1!=0){


A1<-c(N,P,K,Ca,Mg,Zn,B,Cu)    # A1 CON LAS EXIGENCIAS
comment(A1)<-'A1 : N,P,K,Ca,Mg,Zn,B,Cu'


A2<-order(A1, na.last = TRUE, decreasing = FALSE,
          method = c("shell", "radix"))               # A2 ubicacion elementos  ORDENADO CRECIENTEMENTE
comment(A2)<-'A2 : Ubicacion de los elementos ordenados'
#1) inicio    ORDENAMIENTO DE LOS ELEMENTOS#
for (i in A2) {
  A3<-A1[A2]
  comment(A3)<-'A3 : Elementos ordenados segun A2'
  A4<-A3[A3>0]                                          # A4 CON LAS EXIGENCIAS SIGNIFICATIVAS
  comment(A4)<-'A4 : Exigencias significativas'
  A5<-A3[A3<=0]                                         # A5 EXIGENCIAS NO SIGNIFICATIVAS
  comment(A5)<-'A5 : Exigencias NO signifcativas'
  nombre<-c("Nitrogeno","Fosforo","Potasio","Calcio","Magnesio","Zinc","Boro","Cobre")
  comment(nombre)<-'nombre : Nombre de los elementos seg?n orden A1'
  l1<-length(A1); comment(l1)<-'l1 : longitud de A1'
  l4<-length(A4); comment(l4)<-'l4 : longitud de A4'
  l5<-length(A5); comment(l5)<-'l5 : longitud de A5'
  l1
  n_<-nombre[A2];
  comment(n_)<-'n_ : Nombre de los elementos  ordenados de manera creciente segun A2'

  nombre_Ordenado<-0   #  declarar arreglo = 0, indica guardar n?meros
  comment(nombre_Ordenado)<-'nombre_Ordenado : nombre en orden A2 de las exigencias SIGNIFICATIVAS'
}                           # A3 elemento ORDENADO CRECIENTEMENTE
#1final   ORDENAMIENTO DE LOS ELEMENTOS#

#2) inicio   BUCLE QUE FINALIZA LOS CU?LCULOS O MUESTRA NOMBRE DE LOS ELEMENTOS SIGNIFICATIVOS DE MANERA CRECIENTE#
if((l1-l5)==0){
  print("C?lculo terminado")
}else{
  for (i in (l5+1):l1 ) {
    nombre_Ordenado[i-l5]<-c(nombre[A2[i]])
  }
}
comment(nombre_Ordenado)<-'Nombre de las exigenicas significativas en orden creciente'
#2final   BUCLE QUE FINALIZA LOS CU?LCULOS O MUESTRA NOMBRE DE LOS ELEMENTOS SIGNIFICATIVOS DE MANERA CRECIENTE#

paste("Por favor, 1ero cubrir las exigencias de ",nombre_Ordenado[1],"  ",A4[1],"kg, ingrese ley: ")

#3) inicio BUCLE PARA HALLAR EL      e
ubic<-NA # declarar NA porque solo se desea que ubique un numero natural
for(f in 1:l1){
  switch (nombre[f]==nombre_Ordenado[1],ubic[f]<-f)

  comment(ubic)<-'ubic : nuestra la ubicaci?n del 1er elemento a cubrir'
  nombre_ubic<-which(!is.na(ubic));comment(nombre_ubic)<-'nombre_ubic : muestra la posici?n 1er elemento a cubrir'
  e<-nombre_ubic;comment(e)<-'e : nombre_ubic  A1'

}
#3final del bucle para hallar     e


#4)  inicio  C?LCULO DE q y q1
for(s in 1:l1){
  q1<-NA
  q<-0
  if(A1[s]<0 | ley1[s]<0){
    if(A1[s]<0){
      q1<-c(paste("Corregir la EXIGENCIA del ",nombre[s]," este No puede ser negativo"))
      break
    }else{
      q1<-c(paste("Corregir la LEY del ", nombre[s]," este No puede ser negativo"))
      break
    }

  }
  if(A1[s]==0 & ley1[s]>0){
    q1<-c(paste("Contaminacion por ",nombre[s]))
  }
  if(A1[e]>0 & ley1[e]==0 ){
    q1<-c(paste("Cambie de fertilizante,  No cuenta con  ",nombre[e]," dentro de la ley"))
  }else{
    q<-A1[e]/ley1[e]; q1<-paste(q," Kg del fertilizante cubre las exigencias de ", nombre[e])
    comment(q)<-'A1[e]/ley1[e]'
  }
}
q1
#4 final  C?LCULO DE q y q1


#5) Bucle para saber si la nueva exigencia (A1) se hizo negativo
Q1<-ley1[-e]*q;Q1
comment(Q1)<- 'Q1 = ley1[-e] * q1'
p1<-A1[-e]-Q1; comment(p1)<-' p1 : A1[-e] -  Q1'

if(length(p1[p1<0])!=0){
  re<-c("contaminacion por el fertilizante empleado")
}else{
  re<-c("hasta aqu?, los c?lculos son los correctos")
}
re
#####################################################
n_n<-NA
nombrq<-nombre[-e];comment(nombrq)<-'nombreq : nombre[-e]'
for(i in 1:length(p1)){
  if(p1[i]==0){
    n_n[i]<-paste(nombrq[i],"cubierto.")
  }
  if (p1[i]<0){
    n_n[i]<-paste("Ocurre contaminaci?n por ", nombrq[i]," escoja otro fertilizante.")
  }
  if(p1[i]>0){
    n_n[i]<-paste("NO sobrepasa las exigencias de ", nombrq[i],".")

  }
}
n_n
comment(n_n)<-'n_n : Despu?s de hacer los c?lculos, verifica la situacion de las exigencias'
