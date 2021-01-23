#######################################################################
#Demonstration of link prediction
#
#Modified by: Anand
#Modified date: 05/05/2020
######################################################################

rm(list = ls())
graphics.off()

library(igraph)
library(igraphdata)

#####################Generic Functions##################################
#####Function to give sequence of edges that will be deleted############
fnseqedgesdel<-function(gr,percent){
  ecnt<-ecount(gr)
  noedgdel<-round((ecnt*percent)/100)
  seqedgesdel<-sample(1:ecnt,noedgdel)
  return(seqedgesdel)
}

#####Function which returns the deleted edges list#####################
fnlistofedgesdel<-function(gr,seqedgedel){
  alledges<-E(gr)
  cnt=1
  for(i in seqedgedel){
    if(cnt==1){
      edgesdel<-alledges[i]
    }else{
      edgesdel<-append(edgesdel,alledges[i],after=length(edgesdel))  
    }
    cnt=cnt+1  
  }
  return(edgesdel)
}

######Function which shows deleted edges is part of predicted missing edges#######
ffinddeledgesinpred<-function(gr,listdeledges,predictobj){
  flag=0
  for(i in 1:length(listdeledges)){
    for(j in 1:nrow(predictobj$edges)){
      if(paste(vertex_attr(gr,name="name",index=predictobj$edges[j,1]),
               vertex_attr(gr,name="name",index=predictobj$edges[j,2]),
               sep="|")==as_ids(listdeledges[i])){
        flag=1
        cat("Edge = ",as_ids(listdeledges[i])," Probability=",predictobj$prob[j], "\n")
      }
    }
    
    if(flag==0){
      cat("Edge ",as_ids(listdeledges[i])," is not in predicted list", "\n")
    }
    flag=0
  }
}

######Function which adds top predicted edges#######
faddedgestograph<-function(gr,predictobj,listdeledges,num_edges){
  
  E(gr)$color<-"gray"
  lay<-layout_nicely(gr)
  flag=0
  for(i in 1:nrow(predictobj$edges[1:num_edges,])){
    for(j in 1:length(listdeledges)){
      if(paste(vertex_attr(gr,name="name",index=predictobj$edges[i,1]),
               vertex_attr(gr,name="name",index=predictobj$edges[i,2]),
               sep="|")==as_ids(listdeledges[j])){
        flag=1
        gr<-add_edges(gr,t(predictobj$edges[i, ]),color="green")
      }
    }
    if(flag==0){
      gr<-add_edges(gr,t(predictobj$edges[i, ]),color="red")
    }
    flag=0
  }
  return(gr)
}

##########################karate###############################
#####Data Set contains two clubs - A and H
#####Edges represent the social interactions between those group members
set.seed(12)

######Delete edges and predict#####################################################
#Problem 1-a#######################################################################
#Case 1 5% of edges to be deleted
data(karate)
?karate
plot(karate)

vcount(karate)
ecount(karate)

seqedges<-fnseqedgesdel(karate,5)
listedgesdel<-fnlistofedgesdel(karate,seqedges)
karate5<-delete.edges(karate,seqedges)

vcount(karate5)
ecount(karate5)
listedgesdel

plot.igraph(karate5)

#Fit a hierarchical random graph model
#MCMC - Markov chain Monte Carlo for sampling dendrograms which best fits the network
#Lets try using steps as 0 to see the convergence which is the to find best possible dendrogram
#If it takes time then we can take from sampling in steps
?fit_hrg

hcg<-fit_hrg(karate5)

#it stricly separated two club members - A and H 
plot_dendrogram(hcg)

##############Lets predict the missing edges ################
###this method uses first MCMC to find the best fit dendrogram#####
###after that it predicts the edges from the dendrogram############
###Sampling parameters were not mentioned so it converges to best fit####
?predict_edges

karatepred5<-predict_edges(karate5)

####It has generated possible combination of edges 487 and each edge with certain probability#############
karatepred5$edges
karatepred5$prob
plot_dendrogram(karatepred5$hrg)

plot(karatepred5$prob[1:50])
ffinddeledgesinpred(karate5,listedgesdel,karatepred5)

#############Based on the plot of probability of edges######################
#############Looking at Elbow adding 10 top predicted edges to the graph####
graphics.off()
karate5f1<-faddedgestograph(karate5,karatepred5,listedgesdel,10)
plot(karate5f1)

  
#Problem 1-b#######################################################################

#Case 1 5% of edges to be deleted
data(kite)
?kite
plot(kite)

vcount(kite)
ecount(kite)

seqedges<-fnseqedgesdel(kite,5)
listedgesdel<-fnlistofedgesdel(kite,seqedges)
kite5<-delete.edges(kite,seqedges)

vcount(kite5)
ecount(kite5)
listedgesdel

plot.igraph(kite5)

#Fit a hierarchical random graph model
#MCMC - Markov chain Monte Carlo for sampling dendrograms which best fits the network
#Lets try using steps as 0 to see the convergence which is the to find best possible dendrogram
#If it takes time then we can take from sampling in steps
?fit_hrg

hcg<-fit_hrg(kite5)

#it stricly separated two club members - A and H 
plot_dendrogram(hcg)

##############Lets predict the missing edges ################
###this method uses first MCMC to find the best fit dendrogram#####
###after that it predicts the edges from the dendrogram############
###Sampling parameters were not mentioned so it converges to best fit####
?predict_edges

kitepred5<-predict_edges(kite5)

####It has generated possible combination of edges 487 and each edge with certain probability#############
kitepred5$edges
kitepred5$prob
plot_dendrogram(kitepred5$hrg)

plot(kitepred5$prob[1:22])
ffinddeledgesinpred(kite5,listedgesdel,kitepred5)

#############Based on the plot of probability of edges######################
#############Looking at Elbow adding 10 top predicted edges to the graph####
graphics.off()
kite5f1<-faddedgestograph(kite5,kitepred5,listedgesdel,15)
plot(kite5f1)

######################################################################################
#Problem 1-C#######################################################################
#Case 1-a 15% of edges to be deleted
deletion_percent=15
data(karate)
?karate
plot(karate)

vcount(karate)
ecount(karate)

seqedges<-fnseqedgesdel(karate,deletion_percent)
listedgesdel<-fnlistofedgesdel(karate,seqedges)
karate5<-delete.edges(karate,seqedges)

vcount(karate5)
ecount(karate5)
listedgesdel

plot.igraph(karate5)

#Fit a hierarchical random graph model
#MCMC - Markov chain Monte Carlo for sampling dendrograms which best fits the network
#Lets try using steps as 0 to see the convergence which is the to find best possible dendrogram
#If it takes time then we can take from sampling in steps
?fit_hrg

hcg<-fit_hrg(karate5)

#it stricly separated two club members - A and H 
plot_dendrogram(hcg)

##############Lets predict the missing edges ################
###this method uses first MCMC to find the best fit dendrogram#####
###after that it predicts the edges from the dendrogram############
###Sampling parameters were not mentioned so it converges to best fit####
?predict_edges

karatepred5<-predict_edges(karate5)

####It has generated possible combination of edges 487 and each edge with certain probability#############
karatepred5$edges
karatepred5$prob
plot_dendrogram(karatepred5$hrg)

plot(karatepred5$prob[1:50])
ffinddeledgesinpred(karate5,listedgesdel,karatepred5)

#############Based on the plot of probability of edges######################
#############Looking at Elbow adding 10 top predicted edges to the graph####
graphics.off()
karate5f1<-faddedgestograph(karate5,karatepred5,listedgesdel,15)
plot(karate5f1)

##################################################################################
#Case 1-a 40% of edges to be deleted
deletion_percent=40
data(karate)
?karate
plot(karate)

vcount(karate)
ecount(karate)

seqedges<-fnseqedgesdel(karate,deletion_percent)
listedgesdel<-fnlistofedgesdel(karate,seqedges)
karate5<-delete.edges(karate,seqedges)

vcount(karate5)
ecount(karate5)
listedgesdel

plot.igraph(karate5)

#Fit a hierarchical random graph model
#MCMC - Markov chain Monte Carlo for sampling dendrograms which best fits the network
#Lets try using steps as 0 to see the convergence which is the to find best possible dendrogram
#If it takes time then we can take from sampling in steps
?fit_hrg

hcg<-fit_hrg(karate5)

#it stricly separated two club members - A and H 
plot_dendrogram(hcg)

##############Lets predict the missing edges ################
###this method uses first MCMC to find the best fit dendrogram#####
###after that it predicts the edges from the dendrogram############
###Sampling parameters were not mentioned so it converges to best fit####
?predict_edges

karatepred5<-predict_edges(karate5)

####It has generated possible combination of edges 487 and each edge with certain probability#############
karatepred5$edges
karatepred5$prob
plot_dendrogram(karatepred5$hrg)

plot(karatepred5$prob[1:50])
ffinddeledgesinpred(karate5,listedgesdel,karatepred5)

#############Based on the plot of probability of edges######################
#############Looking at Elbow adding 10 top predicted edges to the graph####
graphics.off()
karate5f1<-faddedgestograph(karate5,karatepred5,listedgesdel,10)
plot(karate5f1)

####################################################################################
#problem 1-c 
#Case 3- Kite 15%
###################################################################################
deletion_percent=15
data(kite)
?kite
plot(kite)

vcount(kite)
ecount(kite)

seqedges<-fnseqedgesdel(kite,deletion_percent)
listedgesdel<-fnlistofedgesdel(kite,seqedges)
kite5<-delete.edges(kite,seqedges)

vcount(kite5)
ecount(kite5)
listedgesdel

plot.igraph(kite5)

#Fit a hierarchical random graph model
#MCMC - Markov chain Monte Carlo for sampling dendrograms which best fits the network
#Lets try using steps as 0 to see the convergence which is the to find best possible dendrogram
#If it takes time then we can take from sampling in steps
?fit_hrg

hcg<-fit_hrg(kite5)

#it stricly separated two club members - A and H 
plot_dendrogram(hcg)

##############Lets predict the missing edges ################
###this method uses first MCMC to find the best fit dendrogram#####
###after that it predicts the edges from the dendrogram############
###Sampling parameters were not mentioned so it converges to best fit####
?predict_edges

kitepred5<-predict_edges(kite5)

####It has generated possible combination of edges 487 and each edge with certain probability#############
kitepred5$edges
kitepred5$prob
plot_dendrogram(kitepred5$hrg)

plot(kitepred5$prob[1:22])
ffinddeledgesinpred(kite5,listedgesdel,kitepred5)

#############Based on the plot of probability of edges######################
#############Looking at Elbow adding 10 top predicted edges to the graph####
graphics.off()
kite5f1<-faddedgestograph(kite5,kitepred5,listedgesdel,5)
plot(kite5f1)

##################################################################################
#Case 2 40% of edges to be deleted
#################################################################################
deletion_percent=40
data(kite)
?kite
plot(kite)

vcount(kite)
ecount(kite)

seqedges<-fnseqedgesdel(kite,deletion_percent)
listedgesdel<-fnlistofedgesdel(kite,seqedges)
kite5<-delete.edges(kite,seqedges)

vcount(kite5)
ecount(kite5)
listedgesdel

plot.igraph(kite5)

#Fit a hierarchical random graph model
#MCMC - Markov chain Monte Carlo for sampling dendrograms which best fits the network
#Lets try using steps as 0 to see the convergence which is the to find best possible dendrogram
#If it takes time then we can take from sampling in steps
?fit_hrg

hcg<-fit_hrg(kite5)

#it stricly separated two club members - A and H 
plot_dendrogram(hcg)

##############Lets predict the missing edges ################
###this method uses first MCMC to find the best fit dendrogram#####
###after that it predicts the edges from the dendrogram############
###Sampling parameters were not mentioned so it converges to best fit####
?predict_edges

kitepred5<-predict_edges(kite5)

####It has generated possible combination of edges 487 and each edge with certain probability#############
kitepred5$edges
kitepred5$prob
plot_dendrogram(kitepred5$hrg)

plot(kitepred5$prob[1:22])
ffinddeledgesinpred(kite5,listedgesdel,kitepred5)

#############Based on the plot of probability of edges######################
#############Looking at Elbow adding 10 top predicted edges to the graph####
graphics.off()
kite5f1<-faddedgestograph(kite5,kitepred5,listedgesdel,5)
plot(kite5f1)


################################The End##########################################


