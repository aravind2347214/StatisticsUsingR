library(ggplot2)
data<-mtcars
data
g1=ggplot(data,aes(mpg,disp))
g1+geom_point(aes(color=factor(cyl)))
g1+geom_text(aes(label=cyl),size = 3)

ggplot(data = mtcars,aes(x=mpg,y=hp,col=factor(cyl),size=factor(gear)))+geom_point()+labs(size="gear",col="cyl")

myscaterplot<-ggplot(mtcars,aes(x=wt,y=mpg,col=factor(cyl),size=wt))+geom_point()
myscaterplot + facet_grid(~cyl)

ggplot(mtcars,aes(x=mpg,fill=as.factor(cyl)))+geom_density(alpha=1)



ggplot(data = mtcars,
       mapping = aes(x=cyl,fill=as.factor(am)))
+geom_bar()


