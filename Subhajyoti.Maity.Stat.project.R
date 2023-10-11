tab1<-read.csv("C:\\Users\\subha\\Downloads\\Hobby_Data.csv")
tab1


library(tidyverse)


# Question 1 --------------------------------------------------------------
# How many numbers of students participated in Olympiad?

tab1$Olympiad_Participation
Olympiad_table<-table(tab1$Olympiad_Participation)
Olympiad_table




#Showing by pie chart

pie(Olympiad_table,col=c('violet','green'),labels=paste0(round(100*Olympiad_table/sum(Olympiad_table),2),"%"),clockwise =T,init.angle =90,radius=1,main='Olympiad Participation',sub='pie chart')
legend("topright",legend = c("No","Yes"), fill = c('violet','green'))
#frequency bar diagram

g=barplot(Olympiad_table,ylab='frequency',col=c('purple','blue'),border='yellow',ylim=c(0,1000),main='Olympiad_participation',sub='vertical bar diagram')
matrix(g)
##
barplot(Olympiad_table,ylim=c(0,1000),
        col=c('purple','blue'),border='yellow',ylab='Frequency',
        main='Olympiad_participation',sub='vertical bar diagram')
text(c( 0.7,1.9),
     as.numeric(Olympiad_table)+60,
     paste0(as.numeric(round(prop.table(Olympiad_table)*100,2)),"%"))


#Question2----------------------------------------------------------
#2)Find the number of students whose grasping power is high /low?
tab1$Grasp_pow
table_4<-table(tab1$Grasp_pow)
table_4
sum(table_4)

##showing by bar plot and pie chart

# Here we calculate percentage of students with respect to  Grasping power

##
pie(table_4,labels=paste0(round(100*table_4/sum(table_4)),"%"),
    col=c('green','blue','purple','red','orange','magenta'),
    main='Grasp_pow_percentage',sub='pie chart',radius=1,
    clockwise=T,init.angle = 0)
legend("topright",legend=c(1,2,3,4,5,6),fill=c('green','blue','purple','red','orange','magenta'))

##
k=barplot(table_4, ylim=c(0,800),
        ylab='Frequency', xlab="Grasping power",
        col=c('green','blue','purple','red','orange','magenta'),
        main='Grasping power',sub='Horizontal bar diagram')
matrix(k)
##
barplot(table_4, ylim=c(0,800),
        ylab='Frequency', xlab="Grasping power",
        col=c('green','blue','purple','red','orange','magenta'),
        main='Grasping power',sub='Vartical bar diagram')
text(c(0.7, 1.9, 3.1, 4.3, 5.5, 6.7), 
     as.numeric(table_4)+30, 
     paste0(as.numeric(round(prop.table(table_4)*100, 2)), '%'))





# Question 3--------------------------------------------------------------------
#which predicted hobby chosen maximum number of students?

tab26=tab1$Predicted.Hobby
tab26
Predicted_Hobby_table=table(tab26)
Predicted_Hobby_table


pie(Predicted_Hobby_table,col=c('green','blue','magenta'),
            labels=paste0(round(100*Predicted_Hobby_table/sum(Predicted_Hobby_table)),"%"),
               radius=0.9,main='Predicted Hobby Percentage',sub='Pie Chart')
legend("topright",legend=c("Academics","Arts","Sports"),fill=c('green','blue','magenta'))


#Question 4----------------------------------------------------------------------
#which kind of Favorites subject the students like most in academics?

tab1$Fav_sub
table_3<-table(tab1$Fav_sub)
table_3
sum(table_3)
#####showing by bar plot and pie chart
##
m=barplot(table_3,ylab='frequency',col=4:7,ylim=c(0,800),main='Favourite Subject',sub='vertical Bar Diagram')
matrix(m)
##
barplot(table_3,ylab='Frequency',col=c('blue','green','orange','red'),ylim=c(0,800),
        main='Favourite subject',sub='vertical Bar Diagram')
text(c(0.7, 1.9, 3.1, 4.3), 
     as.numeric(table_3)+30, 
     paste0(as.numeric(round(prop.table(table_3)*100, 2)), '%'))

##
pie(table_3,col=c('blue','green','orange','red'),labels=paste0(round(100*table_3/sum(table_3)),"%"),init.angle=0,radius=0.7,main='Favourite Subject Student Percentage',sub='Pie Chart')
legend("topright",legend=c("Any lang.","His/Geo","Mathematics","Science"),fill=c('blue','green','orange','red'))



#Question5----------------------------------------------------------------
# Favourite subject with both medals
##which number of students have favorite subject either Mat or Science or Any language or History/geography ,got medal from sport and art?
#find the number of student whose favorite subject is Mathematics occupied medals from both sport and art?
#find the number of student whose favorite subject is  Science occupied medals from both sport and art?
#find the number of student whose favorite subject is Any language occupied  medals from both sport and art?
#find the number of student whose favorite subject is History/Geography occupied  medals from both sport and art?

tab8=tab1[tab1$Fav_sub=='Mathematics'&tab1$Medals=='Yes'&tab1$Won_arts=='Yes',]  
tab8
Mat_Medals=length(tab8$Fav_sub)     ##showing the number of student whose favourite sub Mat. got medal from sport and art
Mat_Medals

tab9=tab1[tab1$Fav_sub=='Science'&tab1$Medals=='Yes'&tab1$Won_arts=='Yes',]
tab9
Science_Medals=length(tab9$Fav_sub)    ##showing the number of student whose favourite subject science got medal from sprt and art
Science_Medals

tab10=tab1[tab1$Fav_sub=='Any language'&tab1$Medals=='Yes'&tab1$Won_arts=='Yes',]
tab10
Any_language_Medals=length(tab10$Fav_sub)  ##showing the number of student whose favourite subject Any language got medal from sprt and art
Any_language_Medals

tab11=tab1[tab1$Fav_sub=='History/Geography'&tab1$Medals=='Yes'&tab1$Won_arts=='Yes',]
tab11
History_Geography_Medals=length(tab11$Fav_sub)   ##showing the number of student whose favourite subject history/geography got medal from sprt and art
History_Geography_Medals

z=c(Mat_Medals=length(tab8$Fav_sub),Science_Medals=length(tab9$Fav_sub),Any_language_Medals=length(tab10$Fav_sub),History_Geography_Medals=length(tab11$Fav_sub))
z

sum(z)
##
pie(z,col=c('purple','orange','blue','red'),labels=paste0(round(100*z/sum(z)),"%"),main='Favourite Subject With Both Medals Percentage',sub='pie chart',radius=0.7)
legend("topright",legend=c("Mathematics","Science","Any lang","His/Geo"),fill=c('purple','orange','blue','red'))
##
barplot(z,ylab='Frequency',col=c('purple','orange','blue','red'),ylim=c(0,100),border='yellow',main='Favourite Subject with both medals percentage',sub='Vertical Bar Diagram')
text(c(0.7, 1.9 ,3.1, 4.3),
     as.numeric(z)+10,
     paste0(as.numeric( round( prop.table(z)*100,2)),"%"))

#Question6--------------------------------------------------------------------
#####Predicted hobby by sport medal
###       find the number of students whose predicted hobby is Academics but he got the sport medal?
###       find the number of students whose predicted hobby is Art but he got the sport medal?
###       find the number of students whose predicted hobby is Sport and he got the sport medal?

tab20=tab1[tab1$Predicted.Hobby=='Academics'&tab1$Medals=='Yes',]
tab20$Predicted.Hobby
length(tab20$Predicted.Hobby)  ###the number of students whose predicted hobby is Academics but he/she got the sport medal

tab21=tab1[tab1$Predicted.Hobby=='Arts'&tab1$Medals=='Yes',]
tab21$Predicted.Hobby
length(tab21$Predicted.Hobby)  ###the number of students whose predicted hobby is Art but he/she got the sport medal

tab22=tab1[tab1$Predicted.Hobby=='Sports'&tab1$Medals=='Yes',]
tab22$Predicted.Hobby
length(tab22$Predicted.Hobby)  ###the number of students whose predicted hobby is Sport also he/she got the sport medal

m=c(P_H_Sprt_S_Medal=length(tab22$Predicted.Hobby),P_H_Art_S_Medal=length(tab21$Predicted.Hobby),P_H_Academic_S_Medal=length(tab20$Predicted.Hobby))
m

sum(m)

pie(m,col=c('purple','red','blue'),labels=paste0(round(100*m/sum(m)),"%"),radius=0.9,main='Predicted Hobby by Sports Medal Percentage',sub='pie chart')
legend("topright",legend=c("Sport","Art","Academics"),fill=c('purple','red','blue'))



#Question7------------------------------------------------------------------------
#predicted hobby by won arts
 
### find the number of students whose predicted hobby is Academics but he/she got prize in Arts?
### find the number of students whose predicted hobby is Art but he/she got prize in Arts?
###find the number of students whose predicted hobby is Sport and he/she got prize in Arts?

tab23=tab1[tab1$Predicted.Hobby=='Academics'&tab1$Won_arts=='Yes',]
tab23$Predicted.Hobby
length(tab23$Predicted.Hobby)

tab24=tab1[tab1$Predicted.Hobby=='Arts'&tab1$Won_arts=='Yes',]
tab24$Predicted.Hobby
length(tab24$Predicted.Hobby)

tab25=tab1[tab1$Predicted.Hobby=='Sports'&tab1$Won_arts=='Yes',]
tab25$Predicted.Hobby
length(tab25$Predicted.Hobby)

w=c(P_H_Art_Won_Arts=length(tab24$Predicted.Hobby),P_H_Sport_Won_Arts=length(tab25$Predicted.Hobby),P_H_Academic_Won_Arts=length(tab23$Predicted.Hobby))
w
sum(w)

##
barplot(w,ylab='Frequency',col=c('purple','blue','red'),ylim=c(0,600),border='yellow',main='Predicted hobby by won arts percentage',sub='vertical bar diagram')
text(c(0.7,1.9,3.2),
       as.numeric(w)+30,
     paste0(as.numeric(round(prop.table(w)*100,1)),"%"))


##
pie(w,col=c('green','magenta','blue'),labels=paste0(round(100*w/sum(w),2),"%"),radius=0.9,main='Predicted Hobby by Won Arts Percentage',sub='pie chart')
legend("topright",legend=c("Art","sport","Academic"),fill=c('green','magenta','blue'))

#Question8--------------------------------------------------------------------------
##predicted hobby by  getting both medals from sport and art
tab50=tab1[tab1$Medals=="Yes"&tab1$Won_arts=="Yes"&tab1$Predicted.Hobby=="Academics",]
tab50
tab50$Predicted.Hobby
l1=length(tab50$Predicted.Hobby)
l1

tab51=tab1[tab1$Medals=="Yes"&tab1$Won_arts=="Yes"&tab1$Predicted.Hobby=="Sports",]
tab51
tab51$Predicted.Hobby
l2=length(tab51$Predicted.Hobby)
l2

tab52=tab1[tab1$Medals=="Yes"&tab1$Won_arts=="Yes"&tab1$Predicted.Hobby=="Arts",]
tab52
tab52$Predicted.Hobby
l3=length(tab52$Predicted.Hobby)
l3
l=c(Academics=l1,Sports=l2,Arts=l3)
l
sum(l)
##
pie(l,col=c('red','magenta','orange'),labels=paste0(round(100*l/sum(l)),"%"),radius=0.9,main='Predicted Hobby By Won Both Medal',sub='Pie Chart')
legend("topright",legend=c("Academics","sports","Arts"),fill=c('red','magenta','orange'))



#Question9---------------------------------------------------------------------------------

##Find the number of students got scholarship or not?

tab1$Scholarship
table_2<-table(tab1$Scholarship)
table_2

sum(table_2)
######showing by pie chart and bar diagram

pie(table_2,col=c('orange','purple'),labels=paste0(round(100*table_2/sum(table_2)),"%"),clockwise =T,init.angle =90,radius=1,main='Scholarship Student',sub='pie chart')
legend("topright",legend=c("No","Yes"),fill=c('orange','purple'))
##
barplot(table_2,ylab='count',col=c('orange','purple'),ylim=c(0,1000),
        main='Scholarship Student',sub='vertical Bar Diagram')
text(c(0.7,1.9),
     as.numeric(table_2)+30,
     paste0(as.numeric(round(prop.table(table_2)*100,1)),"%"))



##Question10--------------------------------------------------------------------------
##Number of students percentage of getting medals or not from  sport and art

tab60=tab1[tab1$Medals=='Yes'&tab1$Won_arts=='Yes',]
tab60
p1=length(tab60$Medals)
p1

tab61=tab1[tab1$Medals=='Yes'&tab1$Won_arts=='No',]
tab61
p2=length(tab61$Medals)
p2

tab62=tab1[tab1$Medals=='Yes'&tab1$Won_arts=='Maybe',]
tab62
p3=length(tab62$Medals)
p3

tab63=tab1[tab1$Medals=='No'&tab1$Won_arts=='Yes',]
tab63
p4=length(tab63$Medals)
p4

tab64=tab1[tab1$Medals=='No'&tab1$Won_arts=='Maybe',]
tab64
p5=length(tab64$Medals)
p5

tab65=tab1[tab1$Medals=='No'& tab1$Won_arts=='No',]
tab65
p6=length(tab65$Medals)
p6

p=c(Yes_Yes=p1,Yes_No=p2,Yes_Maybe=p3,No_Yes=p4,No_Maybe=p5,No_No=p6)
p
sum(p)
##
pie(p,labels=paste0(round(100*p/sum(p)),"%"),col=c('green','blue','white','red','orange','magenta'),main='Students performance In Sport and Art Medals',sub='pie chart',radius=0.7,init.angle = 0)  
legend("topright",legend=c("Yes_Yes","Yes_No","Yes_Maybe","No_Yes","No_Maybe","No_No"),cex=0.8,fill=c('green','blue','purple','red','orange','magenta'))

##Question11-------------------------------------------------------------------

##how many number of students to precedence his/her career in sport?
tab1$Career_sprt
table_5=table(tab1$Career_sprt)
table_5
####showing by Bar Diagram 
barplot(table_5,ylab="Frequency",col=c("purple","orange"),ylim=c(0,1200),main="Sport Career",sub="Vertical Bar Diagram")
text(c(0.7,1.9),
     as.numeric(table_5)+40,
     paste0(as.numeric(round(prop.table(table_5)*100,1)),"%"))


##Question12--------------------------------------------------------------------
##percentage of Predicted hobby of the students who will precedence his career in sport 
tab1$Career_sprt
table(tab1$Career_sprt)

tab85=tab1[tab1$Career_sprt=='Yes'& tab1$Predicted.Hobby=='Academics',]
tab85
tab85$Career_sprt
c1=length(tab85$Career_sprt)
c1

tab86=tab1[tab1$Career_sprt=='Yes'& tab1$Predicted.Hobby=='Arts',]
tab86
tab86$Career_sprt
c2=length(tab86$Career_sprt)
c2

tab87=tab1[tab1$Career_sprt=='Yes'& tab1$Predicted.Hobby=='Sports',]
tab87
tab87$Career_sprt
c3=length(tab87$Career_sprt)
c3

c4=c(Cs_Academics=c1,Cs_Arts=c2,Cs_Sports=c3)
c4

barplot(c4,ylab="Frequency",col=c("purple","blue","green"),ylim=c(0,800),border="yellow",main="Sports Career And Predicted Hobby",sub="Vertical Bar Diagram")
text(c(0.7,1.9,3.1),
     as.numeric(c4)+40,
     paste0(as.numeric(round(prop.table(c4)*100,2)),"%"))

#Question13----------------------------------------------------------------------
##Out of total students how many students looks like fantasy art?

tab145=tab1$Fant_arts
tab145
table_145=table(tab145)
table_145

barplot(table_145,ylab="Frequency",ylim=c(0,1000),col=c("blue","green"),main="Fantasy Art",sub="Vertical Bar Diagram")
text(c(0.7,1.9),
     as.numeric(table_145)+30,
    paste0(as.numeric(round(prop.table(table_145)*100,1)),"%"))


##Question14 ------------------------------------------------------------------------

##how many student utilize maximum time and minimum time  behind sport?
tab1$Time_sprt.hours.in.a.day.
table_6=table(tab1$Time_sprt.hours.in.a.day.)
table_6

#####by using bar plot and pie chart
barplot(table_6,ylab='Frequency',col=c('violet','green','red','blue','purple','orange'),border='yellow',ylim=c(0,600),main='Sports Time',sub='Vertical Bar Diagram')
text(c(0.7,1.9,3.1,4.3,5.5,6.7),
     as.numeric(table_6)+30,
     paste0(as.numeric(round(prop.table(table_6)*100,1)),"%"))


##Question15  ---------------------------------------------------------------
##how many numbers of students spent maximum and minimum time behind art?

tab1$Time_art.hours.in.a.day.
table_7=table(tab1$Time_art.hours.in.a.day.)
table_7
####showing by bar plot
barplot(table_7,ylab='Frequency',col=c('violet','green','red','blue','purple','orange'),border='yellow',ylim=c(0,600),main='Arts Time',sub='Vertical Bar Diagram')
text(c(0.7,1.9,3.1,4.3,5.5,6.7),
     as.numeric(table_7)+30,
     paste0(as.numeric(round(prop.table(table_7)*100,1)),"%"))


##Question16------------------------------------------------------------------------------
###how many students like to go to school? 
tab1$School
School_table=table(tab1$School)
School_table
##
barplot(School_table,ylab="Frequency",ylim=c(0,1200),col=c("blue","green"),main="School Presense ",sub="Vertical Bar Diagram")
text(c(0.7,1.9),
     as.numeric(School_table)+30,
     paste0(as.numeric(round(prop.table(School_table)*100,1)),"%"))

###Question17 -------------------------------------------------------------------
##How many students have done the project work ?
tab1$Projects
Projects_table=table(tab1$Projects)
Projects_table
ps=sum(Projects_table)
ps
pie(Projects_table,col=c('red','green'),
    labels=paste0(round(100*Projects_table/sum(Projects_table),2),"%"),
    init.angle =180,radius=1,main='Project Work',sub='pie Chart')
legend("topright",legend=c("No","Yes"),fill=c('red','green'))

#Question18-------------------------------------------------------------------
## Find number of students percentage getting medal from Sports?
tab324=tab1$Medals
tab324
table_324=table(tab324)
table_324
per1=paste0(round(prop.table(table_324)*100,2),"%")
per1
##
cbind(table_324,per1)

#Question19--------------------------------------------------------------------------------
##Find the number of students won prize from Art?
tab325=tab1$Won_arts
tab325
table_325=table(tab325)
table_325
per2=paste0(round(prop.table(table_325)*100,2),"%")
per2
##
cbind(table_325,per2)

##Question20------------------------------------------------------------
##Find the number of students percentage expert in Academic,Sport and Art?
tab300=tab1[tab1$Olympiad_Participation=="Yes"&tab1$Scholarship=="Yes"&tab1$Medals=="Yes"&tab1$Won_arts=="Yes",]
tab300
e=length(tab300$Olympiad_Participation)
e
s=length(tab1$Olympiad_Participation)
s
per3=as.numeric(round(100*e/s,2))
per3




























