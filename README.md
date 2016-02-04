# MrBayes

#### A MrBayes tutorial for tip-dating and node calibration on primates.

The tutorial is available [here](https://github.com/TGuillermeTeaching/MrBayes/blob/master/MrBayes_primates.pdf). You can download it along with the data [here](https://github.com/TGuillermeTeaching/MrBayes/archive/master.zip).


##
````
execute C:\Users\my_name\Desktop\TotalEvidence_Primates.nex
```` 


````r
#Extract the ages table from a tree
#Calculating the tips and the elements age
tree.age_table<-function(tree){
    ages<-dist.nodes(tree)[length(tree$tip.label)+1,]
    tip.names<-tree$tip.label
    if(is.null(tree$node.label)) {
        nod.names<-c((length(tree$tip.label)+1):length(dist.nodes(tree)[,1]))
    } else {
        nod.names<-tree$node.label
    }
    elements<-c(tip.names, nod.names)
    ages.table<-data.frame(ages=ages,elements=elements)
    return(ages.table)
}


#Scaling the ages from tree.age_table if scale != 1
tree.age_scale<-function(ages.table, scale){
    ages.table$ages<-ages.table$ages/max(ages.table$ages)
    ages.table$ages<-ages.table$ages*scale
    return(ages.table)
}


tree.age<-function(tree, age, order='past'){

#CALCULATE THE EDGES AGE

    if(age == 0) {
        ages.table<-tree.age_table(tree)
    } else {
        ages.table<-tree.age_scale(tree.age_table(tree), age)
    }

    #Type
    if(order == 'past'){
        tree.height<-max(ages.table$ages)
        ages.table$ages<-round(abs(ages.table$ages-tree.height), digit=3)
    } else {
        ages.table$ages<-round(ages.table$ages, digit=7)
    }

    #Output
    #ages.table<-round(ages.table[1,], digit=3)
    return(ages.table)
}
````


